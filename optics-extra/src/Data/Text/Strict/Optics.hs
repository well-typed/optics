{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module: Data.Text.Strict.Optics
-- Description: Optics for working with strict 'Strict.Text'.
--
-- This module provides 'Iso's for converting strict 'Strict.Text' to or from a
-- 'String' or 'Builder', and an 'IxTraversal' for traversing the individual
-- characters of a 'Strict.Text'.
--
-- If you need to work with both strict and lazy text, "Data.Text.Optics"
-- provides combinators that support both varieties using a typeclass.
--
module Data.Text.Strict.Optics
  ( packed
  , unpacked
  , builder
  , text
  , utf8
  , _Text
  , pattern Text
  ) where

import Data.ByteString (ByteString)
import Data.Text as Strict
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder

import Data.Profunctor.Indexed

import Optics.Core
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.IxTraversal
import Optics.Internal.Optic

-- | This isomorphism can be used to 'pack' (or 'unpack') strict 'Strict.Text'.
--
--
-- >>> "hello" ^. packed -- :: Text
-- "hello"
--
-- @
-- 'pack' x ≡ x 'Optics.Operators.^.' 'packed'
-- 'unpack' x ≡ x 'Optics.Operators.^.' 're' 'packed'
-- 'packed' ≡ 're' 'unpacked'
-- 'packed' ≡ 'iso' 'pack' 'unpack'
-- @
packed :: Iso' String Text
packed = iso pack unpack
{-# INLINE packed #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') strict 'Strict.Text'.
--
-- >>> Strict.pack "hello" ^. unpacked -- :: String
-- "hello"
--
-- This 'Iso' is provided for notational convenience rather than out of great
-- need, since
--
-- @
-- 'unpacked' ≡ 're' 'packed'
-- @
--
-- @
-- 'pack' x ≡ x 'Optics.Operators.^.' 're' 'unpacked'
-- 'unpack' x ≡ x 'Optics.Operators.^.' 'packed'
-- 'unpacked' ≡ 'iso' 'unpack' 'pack'
-- @
unpacked :: Iso' Text String
unpacked = Optic unpacked__
{-# INLINE unpacked #-}

-- | This is an alias for 'unpacked' that makes it more obvious how to use it
-- with 'Optics.Operators.#'
--
-- >> _Text # "hello" -- :: Text
-- "hello"
_Text :: Iso' Text String
_Text = unpacked
{-# INLINE _Text #-}

-- | Convert between strict 'Strict.Text' and 'Builder' .
--
-- @
-- 'fromText' x ≡ x 'Optics.Operators.^.' 'builder'
-- 'toStrict' ('toLazyText' x) ≡ x 'Optics.Operators.^.' 're' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso fromText (toStrict . toLazyText)
{-# INLINE builder #-}

-- | Traverse the individual characters in strict 'Strict.Text'.
--
-- >>> anyOf text (=='o') (Strict.pack "hello")
-- True
--
-- When the type is unambiguous, you can also use the more general 'each'.
--
-- @
-- 'text' ≡ 'unpacked' % 'traversed'
-- 'text' ≡ 'each'
-- @
--
-- Note that when just using this as a 'Setter', @'sets' 'Data.Text.map'@ can be
-- more efficient.
text :: IxTraversal' Int Text Char
text = Optic text__
{-# INLINE text #-}

-- | Encode\/Decode a strict 'Strict.Text' to\/from strict 'ByteString', via UTF-8.
--
-- >>> utf8 # Strict.pack "☃"
-- "\226\152\131"
utf8 :: Prism' ByteString Text
utf8 = prism' encodeUtf8 (preview _Right . decodeUtf8')
{-# INLINE utf8 #-}

pattern Text :: String -> Text
pattern Text a <- (view _Text -> a) where
  Text a = review _Text a

----------------------------------------
-- Internal implementations

-- | Internal implementation of 'unpacked'.
unpacked__ :: Profunctor p => Optic__ p i i Text Text String String
unpacked__ = dimap unpack pack
{-# INLINE unpacked__ #-}

-- | Internal implementation of 'text'.
text__ :: Traversing p => Optic__ p j (Int -> j) Text Text Char Char
text__ = unpacked__ . itraversed__
{-# INLINE [0] text__ #-}

{-# RULES

"strict text__ -> foldr"
  forall (o :: Forget r j Char Char). text__ o = foldring__ Strict.foldr (reForget o)
    :: Forget r (Int -> j) Text Text

"strict text__ -> ifoldr"
  forall (o :: IxForget r j Char Char). text__ o = ifoldring__ ifoldrStrict o
    :: IxForget r (Int -> j) Text Text

"strict text__ -> map"
  forall (o :: FunArrow j Char Char). text__ o = roam Strict.map (reFunArrow o)
    :: FunArrow (Int -> j) Text Text

"strict text__ -> imap"
  forall (o :: IxFunArrow j Char Char). text__ o = iroam imapStrict o
    :: IxFunArrow (Int -> j) Text Text

#-}

-- | Indexed fold for 'text__'.
ifoldrStrict :: (Int -> Char -> a -> a) -> a -> Text -> a
ifoldrStrict f z xs =
  Strict.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrStrict #-}

-- | Indexed setter for 'text__'.
imapStrict :: (Int -> Char -> Char) -> Text -> Text
imapStrict f = snd . Strict.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapStrict #-}
