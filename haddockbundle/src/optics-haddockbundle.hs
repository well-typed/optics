{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- cabal haddock -w ghc-8.10.7 --haddock-for-hackage optics optics-extra optics-th optics-core
module Main (main) where

import Cabal.Package     (readPackage)
import Control.Exception (throwIO)
import Control.Lens      (_Just, folded, to, (&), (^..))
import Data.Foldable     (foldl')
import Data.List         (intercalate, stripPrefix)
import Data.Map.Strict   (Map)
import Data.Maybe        (mapMaybe)
import Data.Set          (Set)
import Data.Text         (Text)
import Data.Traversable  (for)
import Data.Void         (absurd)
import System.FilePath   (takeDirectory, (-<.>), (</>))

import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Archive.Tar.Entry         as Tar
import qualified Codec.Compression.GZip          as GZip
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Encoding             as A
import qualified Data.Aeson.Types                as A
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Distribution.ModuleName         as C
import qualified Distribution.Package            as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Pretty             as C

import qualified Distribution.Types.GenericPackageDescription.Lens as L
import qualified Distribution.Types.Library.Lens                   as L

data PkgMap a = PkgMap
    { pkgOptics      :: a
    , pkgOpticsCore  :: a
    , pkgOpticsTH    :: a
    , pkgOpticsExtra :: a
    }
  deriving (Show, Functor, Foldable, Traversable)

packages :: PkgMap String
packages = PkgMap
    { pkgOptics      = "optics"
    , pkgOpticsCore  = "optics-core"
    , pkgOpticsTH    = "optics-th"
    , pkgOpticsExtra = "optics-extra"
    }

data Pkg = Pkg C.GenericPackageDescription (Map FilePath LBS.ByteString)
  deriving Show

newtype DocIndex = DocIndex (V.Vector DocIndexEntry)
  deriving (Show, Semigroup, Monoid, A.FromJSON, A.ToJSON)

data DocIndexEntry = DocIndexEntry
    { dieHtml   :: !Text
    , dieLink   :: !Text
    , dieModule :: !C.ModuleName
    , dieName   :: !Text
    }
  deriving Show

instance A.FromJSON DocIndexEntry where
    parseJSON = A.withObject "DocIndexEntry" $ \obj -> do
        dieHtml   <- obj A..: "display_html"
        dieLink   <- obj A..: "link"
        dieModule <- A.explicitParseField parseModuleName obj "module"
        dieName   <- obj A..: "name"

        return DocIndexEntry {..}
      where
        parseModuleName :: A.Value -> A.Parser C.ModuleName
        parseModuleName = A.withText "ModuleName" $ \t -> either fail return $ C.eitherParsec (T.unpack t)

instance A.ToJSON DocIndexEntry where
    toEncoding DocIndexEntry {..} = A.pairs $
        "name"  A..= dieName <>
        A.pair "module" (encodeModuleName dieModule) <>
        "link"  A..= dieLink <>
        "display_html" A..= dieHtml
      where
        encodeModuleName :: C.ModuleName -> A.Encoding
        encodeModuleName mn = A.toEncoding (C.prettyShow mn)

    toJSON = error "toJSON @DocIndexEntry"

main :: IO ()
main = do
    pkgs <- for packages $ \pkg -> do
        -- read package description
        gpd <- readPackage $ pkg </> pkg -<.> "cabal"
        let ver = C.packageVersion gpd

        -- construct the path to haddock tarball (we need a version)!
        let haddockPath = "dist-newstyle" </> (pkg ++ "-" ++ C.prettyShow ver ++ "-docs.tar.gz")

        contents <- LBS.readFile haddockPath
        let unzipped = GZip.decompress contents
        let entries  = Tar.read unzipped

        -- force entries and extract files.
        entries' <- traverseEntries throwIO entries
        let files = Tar.foldEntries insertFile Map.empty absurd entries'

        return $ Pkg gpd files

    let reexportedModules :: Set C.ModuleName
        reexportedModules = case pkgOptics pkgs of
            Pkg gpd _ -> Set.fromList $ gpd ^.. L.condLibrary . _Just . to (fst . C.ignoreConditions) . L.reexportedModules . folded . to C.moduleReexportName

    let docIndices :: PkgMap DocIndex
        docIndices = fmap readDocIndex pkgs

    let combinedIndex :: DocIndex
        combinedIndex = pkgOptics docIndices
            <> filterDocIndex reexportedModules (pkgOpticsCore docIndices)
            <> filterDocIndex reexportedModules (pkgOpticsExtra docIndices)
            <> filterDocIndex reexportedModules (pkgOpticsTH docIndices)

    let optics0 = pkgOptics pkgs
    let optics1 = addCombinedIndex combinedIndex optics0
    let optics2 = optics1
          & addFiles reexportedModules (pkgOpticsCore pkgs)
          & addFiles reexportedModules (pkgOpticsTH pkgs)
          & addFiles reexportedModules (pkgOpticsExtra pkgs)

    case optics2 of
        Pkg _ files ->
            LBS.writeFile "dist-newstyle/optics-bundled-docs.tar" $
            Tar.write $ makeEntries $ Map.toList files

traverseEntries :: Applicative f => (e -> f e') -> Tar.Entries e -> f (Tar.Entries e')
traverseEntries _ Tar.Done            = pure Tar.Done
traverseEntries f (Tar.Fail e)        = Tar.Fail <$> f e
traverseEntries f (Tar.Next entry es) = Tar.Next entry <$> traverseEntries f es

insertFile :: Tar.Entry -> Map FilePath LBS.ByteString -> Map FilePath LBS.ByteString
insertFile entry m = case Tar.entryContent entry of
    Tar.NormalFile contents _ -> Map.insert (Tar.entryPath entry) contents m
    _                         -> m

readDocIndex :: Pkg -> DocIndex
readDocIndex (Pkg gpd files)
    | Just lbs <- Map.lookup (docsDir gpd </> "doc-index.json") files
    , Just x  <- A.decode lbs
    = x

    | otherwise
    = DocIndex V.empty

docsDir :: C.GenericPackageDescription -> FilePath
docsDir gpd =
    C.prettyShow (C.packageName gpd) ++ "-" ++ C.prettyShow (C.packageVersion gpd) ++ "-docs"

filterDocIndex :: Set C.ModuleName -> DocIndex -> DocIndex
filterDocIndex modules (DocIndex entries) =
    DocIndex $ V.filter (\e -> Set.member (dieModule e) modules) entries

addCombinedIndex :: DocIndex -> Pkg -> Pkg
addCombinedIndex docIndex (Pkg gpd files) =
    Pkg gpd $ Map.insert (docsDir gpd </> "doc-index.json") (A.encode docIndex) files

makeEntries :: [(FilePath, LBS.ByteString)] -> [Tar.Entry]
makeEntries = go Set.empty where
    go _    []                 = []
    go dirs ((fp, lbs) : rest) = goDirs dirs (reverse (unfoldDirs (takeDirectory fp))) fp lbs rest

    goDirs dirs []     fp lbs rest = Tar.fileEntry (either error id $ Tar.toTarPath False fp) lbs : go dirs rest
    goDirs dirs (d:ds) fp lbs rest
        | Set.member d dirs
        = goDirs dirs ds fp lbs rest

        | otherwise
        = Tar.directoryEntry (either error id $ Tar.toTarPath True fp) : goDirs (Set.insert d dirs) ds fp lbs rest

unfoldDirs :: FilePath -> [FilePath]
unfoldDirs "." = []
unfoldDirs dir = dir : unfoldDirs (takeDirectory dir)

addFiles :: Set C.ModuleName -> Pkg -> Pkg -> Pkg
addFiles modules (Pkg sGpd sFiles) (Pkg tGpd tFiles) =
    Pkg tGpd (foldl' f tFiles modules <> sourceFiles)
  where
    sDocsDir = docsDir sGpd
    tDocsDir = docsDir tGpd

    -- if a module docs are in source package, add it to target (acc) package
    f :: Map FilePath LBS.ByteString -> C.ModuleName -> Map FilePath LBS.ByteString
    f acc mn = case Map.lookup (sDocsDir </> modulePath mn) sFiles of
        Nothing  -> acc
        Just lbs -> Map.insert (tDocsDir </> modulePath mn) lbs acc

    modulePath :: C.ModuleName -> FilePath
    modulePath mn = intercalate "-" (C.components mn) ++ ".html"

    sourceFiles :: Map FilePath LBS.ByteString
    sourceFiles = Map.fromList $ mapMaybe p $ Map.toList sFiles where
        p (fp, lbs) = do
            end <- stripPrefix (sDocsDir </> "src/") fp
            return (tDocsDir </> "src" </> end, lbs)
