{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Optics.Internal
  (
  -- * Traversals
    HasTypeVars(..)
  , typeVars      -- :: HasTypeVars t => Traversal' t Name
  , typeVarsKinded
  , substTypeVars -- :: HasTypeVars t => Map Name Name -> t -> t
  , SubstType(..)

  -- * Prisms
  , _FamilyI
  , _ClosedTypeFamilyD
  , _OpenTypeFamilyD
  , _ForallT

  -- * TyVarBndr compatibility
  , TyVarBndrSpec
  ) where

import Data.Map as Map hiding (map, toList)
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Data.Set as Set hiding (map, toList)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr

import Data.Set.Optics
import Optics.Core

-- | Has a 'Name'
class HasName t where
  -- | Extract (or modify) the 'Name' of something
  name :: Lens' t Name

instance HasName (TyVarBndr_ flag) where
  name = lensVL $ \f -> \case
#if MIN_VERSION_template_haskell(2,17,0)
    PlainTV n flag    -> (\n' -> PlainTV n' flag) <$> f n
    KindedTV n flag k -> (\n' -> KindedTV n' flag k ) <$> f n
#else
    PlainTV n    -> PlainTV <$> f n
    KindedTV n k -> (`KindedTV` k) <$> f n
#endif

-- | Provides for the extraction of free type variables, and alpha renaming.
class HasTypeVars t where
  -- | When performing substitution into this traversal you're not allowed to
  -- substitute in a name that is bound internally or you'll violate the
  -- 'Traversal' laws, when in doubt generate your names with 'newName'.
  typeVarsEx :: Set Name -> Traversal' t Name

instance HasTypeVars (TyVarBndr_ flag) where
  typeVarsEx s = traversalVL $ \f b ->
    if view name b `Set.member` s
    then pure b
    else traverseOf name f b

instance HasTypeVars Name where
  typeVarsEx s = traversalVL $ \f n ->
    if n `Set.member` s
    then pure n
    else f n

instance HasTypeVars Type where
  typeVarsEx s = traversalVL $ \f -> \case
    VarT n            -> VarT <$> traverseOf (typeVarsEx s) f n
    AppT l r          -> AppT <$> traverseOf (typeVarsEx s) f l
                              <*> traverseOf (typeVarsEx s) f r
    SigT t k          -> SigT <$> traverseOf (typeVarsEx s) f t
                              <*> traverseOf (typeVarsEx s) f k
    ForallT bs ctx ty -> let s' = s `Set.union` setOf typeVars bs
                         in ForallT bs <$> traverseOf (typeVarsEx s') f ctx
                                       <*> traverseOf (typeVarsEx s') f ty
    InfixT  t1 n t2   -> InfixT <$> traverseOf (typeVarsEx s) f t1
                                <*> pure n
                                <*> traverseOf (typeVarsEx s) f t2
    UInfixT t1 n t2   -> UInfixT <$> traverseOf (typeVarsEx s) f t1
                                 <*> pure n
                                 <*> traverseOf (typeVarsEx s) f t2
    ParensT t         -> ParensT <$> traverseOf (typeVarsEx s) f t
#if MIN_VERSION_template_haskell(2,15,0)
    AppKindT t k       -> AppKindT <$> traverseOf (typeVarsEx s) f t
                                   <*> traverseOf (typeVarsEx s) f k
    ImplicitParamT n t -> ImplicitParamT n <$> traverseOf (typeVarsEx s) f t
#endif
    t                 -> pure t

instance HasTypeVars t => HasTypeVars [t] where
  typeVarsEx s = traversed % typeVarsEx s


-- | Traverse /free/ type variables
typeVars :: HasTypeVars t => Traversal' t Name
typeVars = typeVarsEx mempty

-- | Traverse /free/ type variables paired with their kinds if applicable.
typeVarsKinded :: Fold Type Type
typeVarsKinded = foldVL $ go mempty
  where
    go s f = \case
      var@(VarT n)          -> if n `Set.member` s then pure () else f var
      var@(SigT (VarT n) _) -> if n `Set.member` s then pure () else f var

      AppT l r           -> go s f l *> go s f r
      SigT t k           -> go s f t *> go s f k
      ForallT bs ctx ty  -> let s' = s `Set.union` setOf typeVars bs
                            in traverse_ (go s' f) ctx *> go s' f ty
      InfixT  t1 _ t2    -> go s f t1 *> go s f t2
      UInfixT t1 _ t2    -> go s f t1 *> go s f t2
      ParensT t          -> go s f t
#if MIN_VERSION_template_haskell(2,15,0)
      AppKindT t k       -> go s f t *> go s f k
      ImplicitParamT _ t -> go s f t
#endif
      _                 -> pure ()

-- | Substitute using a map of names in for /free/ type variables
substTypeVars :: HasTypeVars t => Map Name Name -> t -> t
substTypeVars m = over typeVars $ \n -> fromMaybe n (n `Map.lookup` m)

-- | Provides substitution for types
class SubstType t where
  -- | Perform substitution for types
  substType :: Map Name Type -> t -> t

instance SubstType Type where
  substType m t@(VarT n)          = fromMaybe t (n `Map.lookup` m)
  substType m (ForallT bs ctx ty) = ForallT bs (substType m' ctx) (substType m' ty)
    where m' = foldrOf typeVars Map.delete m bs
  substType m (SigT t k)          = SigT (substType m t) (substType m k)
  substType m (AppT l r)          = AppT (substType m l) (substType m r)
  substType m (InfixT  t1 n t2)   = InfixT  (substType m t1) n (substType m t2)
  substType m (UInfixT t1 n t2)   = UInfixT (substType m t1) n (substType m t2)
  substType m (ParensT t)         = ParensT (substType m t)
#if MIN_VERSION_template_haskell(2,15,0)
  substType m (AppKindT t k)       = AppKindT (substType m t) (substType m k)
  substType m (ImplicitParamT n t) = ImplicitParamT n (substType m t)
#endif
  substType _ t                   = t

instance SubstType t => SubstType [t] where
  substType = map . substType


_FamilyI :: Prism' Info (Dec, [InstanceDec])
_FamilyI
  = prism' reviewer remitter
  where
      reviewer (x, y) = FamilyI x y
      remitter (FamilyI x y) = Just (x, y)
      remitter _ = Nothing

_ClosedTypeFamilyD :: Prism' Dec (TypeFamilyHead, [TySynEqn])
_ClosedTypeFamilyD
  = prism' reviewer remitter
  where
      reviewer (x, y) = ClosedTypeFamilyD x y
      remitter (ClosedTypeFamilyD x y) = Just (x, y)
      remitter _ = Nothing

_OpenTypeFamilyD :: Prism' Dec TypeFamilyHead
_OpenTypeFamilyD
  = prism' reviewer remitter
  where
      reviewer = OpenTypeFamilyD
      remitter (OpenTypeFamilyD x) = Just x
      remitter _ = Nothing

_ForallT :: Prism' Type ([TyVarBndrSpec], Cxt, Type)
_ForallT
  = prism' reviewer remitter
  where
      reviewer (x, y, z) = ForallT x y z
      remitter (ForallT x y z) = Just (x, y, z)
      remitter _ = Nothing
