module Pretty where

import Prelude

import Data.Array as A
import Data.Const (Const)
import Data.Identity (Identity)
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe, maybe)
import Data.Newtype as N
import Data.String (Pattern(..), split) as S
import Data.String.CodeUnits (fromCharArray) as S
import Data.Tuple (Tuple(..))
import PrettyPrinter (Doc, char, fillSep, line, list, text, tupled, unsafeStringWithoutNewlines, vsep)

words :: forall ann. String -> Array (Doc ann)
words = map pretty <<< S.split (S.Pattern " ")

reflow :: forall ann. String -> Doc ann
reflow = fillSep <<< words

viaShow :: forall a ann. Show a => a -> Doc ann
viaShow = pretty <<< show

unsafeViaShow :: forall a ann. Show a => a -> Doc ann
unsafeViaShow = unsafeStringWithoutNewlines <<< show

prettyListDefault :: forall ann a. Pretty a => Array a -> Doc ann
prettyListDefault = list <<< map pretty

class Pretty a where
  pretty :: forall ann. a -> Doc ann
  prettyList :: forall ann. Array a -> Doc ann

instance prettyConst :: Pretty a => Pretty (Const a b) where
  pretty = pretty <<< N.unwrap
  prettyList xs = list $ map pretty xs

instance prettyIdentity :: Pretty a => Pretty (Identity a) where
  pretty = pretty <<< N.unwrap
  prettyList xs = list $ map pretty xs

instance prettyArray :: Pretty a => Pretty (Array a) where
  pretty = prettyListDefault
  prettyList xs = list $ map pretty xs 

instance prettyLList :: Pretty a => Pretty (L.List a) where
  pretty = prettyListDefault <<< L.toUnfoldable
  prettyList = prettyListDefault

instance prettyLazyList :: Pretty a => Pretty (LL.List a) where
  pretty = prettyListDefault <<< LL.toUnfoldable
  prettyList = prettyListDefault

instance prettyString :: Pretty String where 
  pretty = vsep <<< map unsafeStringWithoutNewlines <<< S.split (S.Pattern "\n")
  prettyList xs = list $ map pretty xs

instance prettyUnit :: Pretty Unit where
  pretty _ = text "()"
  prettyList xs = list $ map pretty xs

instance prettyBoolean :: Pretty Boolean where
  pretty b = text $ case b of
    true -> "true"
    false -> "false"
  prettyList xs = list $ map pretty xs

instance prettyChar :: Pretty Char where
  pretty '\n' = line
  pretty c = char c
  prettyList = pretty <<< S.fromCharArray

instance prettyInt :: Pretty Int where 
  pretty = unsafeViaShow
  prettyList xs = list $ map pretty xs

instance prettyNumber :: Pretty Number where 
  pretty = unsafeViaShow
  prettyList xs = list $ map pretty xs 

instance prettyTuple :: (Pretty a1, Pretty a2) => Pretty (Tuple a1 a2) where
  pretty (Tuple x1 x2) = tupled [pretty x1, pretty x2]
  prettyList xs = list $ map pretty xs

instance prettyMaybe :: Pretty a => Pretty (Maybe a) where
  pretty = maybe mempty pretty
  prettyList = prettyList <<< A.catMaybes

instance prettyVoid :: Pretty Void where 
  pretty = absurd
  prettyList xs = list $ map pretty xs