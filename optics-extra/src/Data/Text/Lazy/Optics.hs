{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module: Data.Text.Lazy.Optics
-- Description: Optics for working with lazy 'Text.Text'.
--
-- This module provides 'Iso's for converting lazy 'Text.Text' to or from a
-- 'String' or 'Builder', and an 'IxTraversal' for traversing the individual
-- characters of a 'Text.Text'.
--
-- If you need to work with both strict and lazy text, "Data.Text.Optics"
-- provides combinators that support both varieties using a typeclass.
--
module Data.Text.Lazy.Optics
  ( packed
  , unpacked
  , _Text
  , text
  , builder
  , utf8
  , pattern Text
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy as Text
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding

import Optics.Core
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.IxTraversal
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Optics.Core

-- | This isomorphism can be used to 'pack' (or 'unpack') lazy 'Text.Text'.
--
-- >>> "hello"^.packed -- :: Text
-- "hello"
--
-- @
-- 'pack' x ≡ x 'Optics.Operators.^.' 'packed'
-- 'unpack' x ≡ x 'Optics.Operators.^.' 're' 'packed'
-- 'packed' ≡ 're' 'unpacked'
-- @
packed :: Iso' String Text
packed = iso Text.pack Text.unpack
{-# INLINE packed #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') lazy 'Text.Text'.
--
-- >>> "hello"^.unpacked -- :: String
-- "hello"
--
-- @
-- 'pack' x ≡ x 'Optics.Operators.^.' 're' 'unpacked'
-- 'unpack' x ≡ x 'Optics.Operators.^.' 'packed'
-- @
--
-- This 'Iso' is provided for notational convenience rather than out of great
-- need, since
--
-- @
-- 'unpacked' ≡ 're' 'packed'
-- @
unpacked :: Iso' Text String
unpacked = Optic unpacked__
{-# INLINE unpacked #-}

-- | This is an alias for 'unpacked' that makes it clearer how to use it with
-- @('Optics.Operators.#')@.
--
-- @
-- '_Text' = 're' 'packed'
-- @
--
-- >>> _Text # "hello" -- :: Text
-- "hello"
_Text :: Iso' Text String
_Text = re packed
{-# INLINE _Text #-}

-- | Convert between lazy 'Text.Text' and 'Builder' .
--
-- @
-- 'fromLazyText' x ≡ x 'Optics.Operators.^.' 'builder'
-- 'toLazyText' x ≡ x 'Optics.Operators.^.' 're' 'builder'
-- @
builder :: Iso' Text Builder
builder = iso fromLazyText toLazyText
{-# INLINE builder #-}

-- | Traverse the individual characters in a 'Text.Text'.
--
-- >>> anyOf text (=='c') "chello"
-- True
--
-- @
-- 'text' = 'unpacked' . 'traversed'
-- @
--
-- When the type is unambiguous, you can also use the more general 'each'.
--
-- @
-- 'text' ≡ 'each'
-- @
--
-- Note that when just using this as a 'Setter', @'sets' 'Data.Text.Lazy.map'@
-- can be more efficient.
text :: IxTraversal' Int Text Char
text = Optic text__
{-# INLINE text #-}

-- | Encode\/Decode a lazy 'Text.Text' to\/from lazy 'ByteString', via UTF-8.
--
-- Note: This function does not decode lazily, as it must consume the entire
-- input before deciding whether or not it fails.
--
-- >>> ByteString.unpack (utf8 # "☃")
-- [226,152,131]
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
unpacked__ = dimap Text.unpack Text.pack
{-# INLINE unpacked__ #-}

-- | Internal implementation of 'text'.
text__ :: Traversing p => Optic__ p j (Int -> j) Text Text Char Char
text__ = unpacked__ . itraversed__
{-# INLINE [0] text__ #-}

{-# RULES

"lazy text__ -> foldr"
  forall (o :: Forget r j Char Char). text__ o = foldring__ Text.foldr (reForget o)
    :: Forget r (Int -> j) Text Text

"lazy text__ -> ifoldr"
  forall (o :: IxForget r j Char Char). text__ o = ifoldring__ ifoldrLazy o
    :: IxForget r (Int -> j) Text Text

"lazy text__ -> map"
  forall (o :: FunArrow j Char Char). text__ o
                                    = roam Text.map (reFunArrow o)
    :: FunArrow (Int -> j) Text Text

"lazy text__ -> imap"
  forall (o :: IxFunArrow j Char Char). text__ o = iroam imapLazy o
    :: IxFunArrow (Int -> j) Text Text

#-}

-- | Indexed fold for 'text__'.
ifoldrLazy :: (Int -> Char -> a -> a) -> a -> Text -> a
ifoldrLazy f z xs =
  Text.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrLazy #-}

-- | Indexed setter for 'text__'.
imapLazy :: (Int -> Char -> Char) -> Text -> Text
imapLazy f = snd . Text.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapLazy #-}
