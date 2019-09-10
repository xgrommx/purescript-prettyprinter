module PrettyPrinter where

import Prelude hiding ((<$>))

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array as A
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (round, toNumber)
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..)) as S
import Data.String.CodeUnits (fromCharArray, length, singleton, uncons) as S
import Data.String.Common (null, split) as S
import Data.Traversable (class Traversable, sequenceDefault)
import Partial.Unsafe (unsafeCrashWith)

replicate :: Int -> Char -> String
replicate i c = S.fromCharArray $ A.replicate i c

----------------------------------------------------------------

newtype FittingPredicate ann = FittingPredicate (PageWidth -> Int -> Maybe Int -> SimpleDocStream ann -> Boolean)

----------------------------------------------------------------

data LayoutPipeline ann 
  = Nil
  | Cons Int (Doc ann) (LayoutPipeline ann)
  | UndoAnn (LayoutPipeline ann)

----------------------------------------------------------------

type LayoutOptions = { layoutPageWidth :: PageWidth }

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = { layoutPageWidth: AvailablePerLine 80 1.0 }

----------------------------------------------------------------

data FusionDepth = Shallow | Deep

derive instance genericFusionDepth :: Generic FusionDepth _

instance showFusionDepth :: Show FusionDepth where
  show p = genericShow p

instance eqFusionDepth :: Eq FusionDepth where
  eq p1 p2 = genericEq p1 p2

instance ordFusionDepth :: Ord FusionDepth where
  compare p1 p2 = genericCompare p1 p2

fuse :: forall ann. FusionDepth -> Doc ann -> Doc ann
fuse depth = go
  where
    go = case _ of
      Cat Empty x -> go x
      Cat x Empty -> go x
      Cat (Char c1) (Char c2) -> Text 2 (S.singleton c1 <> S.singleton c2)
      Cat (Text lt t) (Char c) -> Text (lt+1) (t <> S.singleton c)
      Cat (Char c) (Text lt t) -> Text (1+lt) (S.singleton c <> t)
      Cat (Text l1 t1) (Text l2 t2) -> Text (l1+l2) (t1 <> t2)

      Cat x@(Char _) (Cat y@(Char _) z) -> go (Cat (go (Cat x y)) z)
      Cat x@(Text _ _) (Cat y@(Char _) z) -> go (Cat (go (Cat x y)) z)
      Cat x@(Char _) (Cat y@(Text _ _) z) -> go (Cat (go (Cat x y)) z)
      Cat x@(Text _ _) (Cat y@(Text _ _) z) -> go (Cat (go (Cat x y)) z)

      Cat (Cat x y@(Char _)) z -> go (Cat x (go (Cat y z)))
      Cat (Cat x y@(Text _ _)) z -> go (Cat x (go (Cat y z)))

      Cat x y -> Cat (go x) (go y)

      Nest i (Nest j x) -> let fused = Nest (i+j) x in go fused
      Nest _ x@Empty -> x
      Nest _ x@(Text _ _) -> x
      Nest _ x@(Char _) -> x
      Nest 0 x -> go x
      Nest i x -> Nest i (go x)

      Annotated _ Empty -> Empty

      FlatAlt x1 x2 -> FlatAlt (go x1) (go x2)
      Union x1 x2 -> Union (go x1) (go x2)

      other | depth == Shallow -> other

      Column f -> Column (go <<< f)
      WithPageWidth f -> WithPageWidth (go <<< f)
      Nesting f -> Nesting (go <<< f)

      other -> other

----------------------------------------------------------------

data SimpleDocStream ann 
  = SFail
  | SEmpty
  | SChar Char (SimpleDocStream ann)
  | SText Int String (SimpleDocStream ann)
  | SLine Int (SimpleDocStream ann)
  | SAnnPush ann (SimpleDocStream ann)
  | SAnnPop (SimpleDocStream ann)

derive instance genericSimpleDocStream :: Generic (SimpleDocStream ann) _

instance showSimpleDocStream :: Show ann => Show (SimpleDocStream ann) where
  show p = genericShow p

instance eqSimpleDocStream :: Eq ann => Eq (SimpleDocStream ann) where
  eq p1 p2 = genericEq p1 p2

instance ordSimpleDocStream :: Ord ann => Ord (SimpleDocStream ann) where
  compare p1 p2 = genericCompare p1 p2

instance functorSimpleDocStream :: Functor SimpleDocStream where
  map = reAnnotateS

instance foldableSimpleDocStream :: Foldable SimpleDocStream where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = go
    where
      go = case _ of
        SFail -> mempty
        SEmpty -> mempty
        SChar _ rest -> go rest
        SText _ _ rest -> go rest
        SLine _ rest -> go rest
        SAnnPush ann rest -> f ann <> go rest
        SAnnPop rest -> go rest

instance traversableSimpleDocStream :: Traversable SimpleDocStream where
  sequence = sequenceDefault
  traverse f = go
    where
      go = case _ of
        SFail -> pure SFail
        SEmpty -> pure SEmpty
        SChar c rest -> map (SChar c) (go rest)
        SText l t rest -> map (SText l t) (go rest)
        SLine i rest -> map (SLine i) (go rest)
        SAnnPush ann rest -> lift2 SAnnPush  (f ann) (go rest)
        SAnnPop rest -> map SAnnPop (go rest)

unAnnotateS :: forall ann xxx. SimpleDocStream ann -> SimpleDocStream xxx
unAnnotateS = go
  where
    go s = case s of
      SFail -> SFail
      SEmpty -> SEmpty
      SChar c rest -> SChar c (go rest)
      SText l t rest -> SText l t (go rest)
      SLine l rest -> SLine l (go rest)
      SAnnPop rest -> go rest
      SAnnPush _ann rest -> go rest

reAnnotateS :: forall ann ann'. (ann -> ann') -> SimpleDocStream ann -> SimpleDocStream ann'
reAnnotateS re = go
  where
    go s = case s of
      SFail -> SFail
      SEmpty -> SEmpty
      SChar c rest -> SChar c (go rest)
      SText l t rest -> SText l t (go rest)
      SLine l rest -> SLine l (go rest)
      SAnnPop rest -> SAnnPop (go rest)
      SAnnPush ann rest -> SAnnPush (re ann) (go rest)

---------------------------------------------------------------

data PageWidth
  = AvailablePerLine Int Number
  | Unbounded

derive instance genericPageWidth :: Generic PageWidth _

instance showPageWidth :: Show PageWidth where
  show p = genericShow p

instance eqPageWidth :: Eq PageWidth where
  eq p1 p2 = genericEq p1 p2

instance ordPageWidth :: Ord PageWidth where
  compare p1 p2 = genericCompare p1 p2

------------------------------------------------------------------

data Doc ann 
  = Fail
  | Empty
  | Char Char
  | Text Int String
  | Line
  | FlatAlt (Doc ann) (Doc ann)
  | Cat (Doc ann) (Doc ann)
  | Nest Int (Doc ann)
  | Union (Doc ann) (Doc ann)
  | Column (Int -> Doc ann)
  | WithPageWidth (PageWidth -> Doc ann)
  | Nesting (Int -> Doc ann)
  | Annotated ann (Doc ann)

derive instance genericDoc :: Generic (Doc ann) _

instance showDoc :: Show (Doc ann) where
  show d = renderDoc d

instance semigroupDoc :: Semigroup (Doc ann) where
  append = Cat

instance monoidDoc :: Monoid (Doc ann) where
  mempty = Empty

instance functorDoc :: Functor Doc where
  map = reAnnotate

reAnnotate :: forall ann ann'. (ann -> ann') -> Doc ann -> Doc ann'
reAnnotate re = alterAnnotations (pure <<< re)

alterAnnotations :: forall ann ann'. (ann -> Array ann') -> Doc ann -> Doc ann'
alterAnnotations re = go
  where
    go = case _ of
      Fail -> Fail
      Empty -> Empty
      Char c -> Char c
      Text l t -> Text l t
      Line -> Line
      FlatAlt x y -> FlatAlt (go x) (go y)
      Cat x y -> Cat (go x) (go y)
      Nest i x -> Nest i (go x)
      Union x y -> Union (go x) (go y)
      Column f -> Column (go <<< f)
      WithPageWidth f -> WithPageWidth (go <<< f)
      Nesting f -> Nesting (go <<< f)
      Annotated ann x -> foldr Annotated (go x) (re ann)

----------------------------------------------------------------

text :: forall ann. String -> Doc ann
text "" = Empty
text s  = Text (S.length s) s

char :: forall ann. Char -> Doc ann
char = Char

string :: forall ann. String -> Doc ann
string = A.intercalate line <<< map text <<< S.split (S.Pattern "\n")

squotes :: forall ann. Doc ann -> Doc ann
squotes = enclose squote squote

dquotes :: forall ann. Doc ann -> Doc ann
dquotes = enclose dquote dquote

parens :: forall ann. Doc ann -> Doc ann
parens = enclose lparen rparen

angles :: forall ann. Doc ann -> Doc ann
angles = enclose langle rangle

brackets :: forall ann. Doc ann -> Doc ann
brackets = enclose lbracket rbracket

braces :: forall ann. Doc ann -> Doc ann
braces = enclose lbrace rbrace

squote :: forall ann. Doc ann
squote = char '\''

dquote :: forall ann. Doc ann
dquote = char '"'

lparen :: forall ann. Doc ann
lparen = char '('

rparen :: forall ann. Doc ann
rparen = char ')'

langle :: forall ann. Doc ann
langle = char '<'

rangle :: forall ann. Doc ann
rangle = char '>'

lbracket :: forall ann. Doc ann
lbracket = char '['

rbracket :: forall ann. Doc ann
rbracket = char ']'

lbrace :: forall ann. Doc ann
lbrace = char '{'

rbrace :: forall ann. Doc ann
rbrace = char '}'

semi :: forall ann. Doc ann
semi = char ';'

colon :: forall ann. Doc ann
colon = char ':'

comma :: forall ann. Doc ann
comma = char ','

space :: forall ann. Doc ann
space = char ' '

dot :: forall ann. Doc ann
dot = char '.'

slash :: forall ann. Doc ann
slash = char '/'

backslash :: forall ann. Doc ann
backslash = char '\\'

equals :: forall ann. Doc ann
equals = char '='

pipe :: forall ann. Doc ann
pipe = char '|'

----------------------------------------------------------------

emptyDoc :: forall ann. Doc ann
emptyDoc = Empty

nest :: forall ann. Int -> Doc ann -> Doc ann
nest 0 x = x
nest i x = Nest i x

line :: forall ann. Doc ann
line = FlatAlt Line space

line' :: forall ann. Doc ann
line' = FlatAlt Line mempty

softline :: forall ann. Doc ann
softline = group line

softline' :: forall ann. Doc ann
softline' = group line'

hardline :: forall ann. Doc ann
hardline = Line

group :: forall ann. Doc ann -> Doc ann
group x = case changesUponFlattening x of
  Nothing -> x
  Just x' -> Union x' x

changesUponFlattening :: forall ann. Doc ann -> Maybe (Doc ann)
changesUponFlattening = case _ of
    FlatAlt _ y -> Just (flatten y)
    Line -> Just Fail
    Union x _ -> changesUponFlattening x <|> Just x
    Nest i x -> map (Nest i) (changesUponFlattening x)
    Annotated ann x -> map (Annotated ann) (changesUponFlattening x)

    Column f -> Just (Column (flatten <<< f))
    Nesting f -> Just (Nesting (flatten <<< f))
    WithPageWidth f -> Just (WithPageWidth (flatten <<< f))

    Cat x y -> case changesUponFlattening x, changesUponFlattening y of
      Nothing, Nothing -> Nothing
      Just x', Nothing -> Just (Cat x' y )
      Nothing, Just y' -> Just (Cat x  y')
      Just x', Just y' -> Just (Cat x' y')

    Empty -> Nothing
    (Char _) -> Nothing
    (Text _ _) -> Nothing
    Fail   -> Nothing
  where
    flatten :: Doc ann -> Doc ann
    flatten = go where
      go = case _ of
        FlatAlt _ y -> go y
        Cat x y -> Cat (go x) (go y)
        Nest i x -> Nest i (go x)
        Line -> Fail
        Union x _ -> go x
        Column f -> Column (go <<< f)
        WithPageWidth f -> WithPageWidth (go <<< f)
        Nesting f -> Nesting (go <<< f)
        Annotated ann x -> Annotated ann (go x)

        x@Fail -> x
        x@Empty -> x
        x@(Char _) -> x
        x@(Text _ _) -> x

flatAlt :: forall ann. Doc ann -> Doc ann -> Doc ann
flatAlt = FlatAlt

align :: forall ann. Doc ann -> Doc ann
align d = column (\k -> nesting (\i -> nest (k - i) d))

hang :: forall ann. Int -> Doc ann -> Doc ann
hang i d = align (nest i d)

indent :: forall ann. Int -> Doc ann -> Doc ann
indent i d = hang i (spaces i <> d)

encloseSep :: forall ann. Doc ann -> Doc ann -> Doc ann -> Array (Doc ann) -> Doc ann
encloseSep l r s ds = case ds of
  []  -> l <> r
  [d] -> l <> d <> r
  _   -> cat (LL.toUnfoldable $ LL.zipWith (<>) (l LL.: LL.repeat s) (LL.fromFoldable ds)) <> r

list :: forall ann. Array (Doc ann) -> Doc ann
list = group <<< encloseSep (flatAlt (text "[ ") lbracket)
                            (flatAlt (text " ]") rbracket)
                            (text ", ")
-- list = group <<< encloseSep (flatAlt lbracket lbracket) (flatAlt rbracket rbracket) comma

tupled :: forall ann. Array (Doc ann) -> Doc ann
tupled = group <<< encloseSep (flatAlt lparen lparen) (flatAlt rparen rparen) comma

appendWithSpace :: forall ann. Doc ann -> Doc ann -> Doc ann
appendWithSpace x y = x <> space <> y
infixr 6 appendWithSpace as <+>

appendWithSoftline :: forall ann. Doc ann -> Doc ann -> Doc ann
appendWithSoftline x y = x <> softline <> y
infixr 5 appendWithSoftline as </>

appendWithSoftbreak :: forall ann. Doc ann -> Doc ann -> Doc ann
appendWithSoftbreak x y = x <> softline' <> y
infixr 5 appendWithSoftbreak as <//>

appendWithLine :: forall ann. Doc ann -> Doc ann -> Doc ann
appendWithLine x y = x <> line <> y
infixr 5 appendWithLine as <$>

appendWithLinebreak :: forall ann. Doc ann -> Doc ann -> Doc ann
appendWithLinebreak x y = x <> line' <> y
infixr 5 appendWithLinebreak as <$$>

hsep :: forall ann. Array (Doc ann) -> Doc ann
hsep = concatWith (<+>)

vsep :: forall ann. Array (Doc ann) -> Doc ann
vsep = concatWith (\x y -> x <> line <> y)

fillSep :: forall ann. Array (Doc ann) -> Doc ann
fillSep = concatWith (\x y -> x <> softline <> y)

sep :: forall ann. Array (Doc ann) -> Doc ann
sep = group <<< vsep

hcat :: forall ann. Array (Doc ann) -> Doc ann
hcat = concatWith (<>)

fillCat :: forall ann. Array (Doc ann) -> Doc ann
fillCat = concatWith (\x y -> x <> softline' <> y)

punctuate :: forall ann. Doc ann -> Array (Doc ann) -> Array (Doc ann)
punctuate p = go
  where
    go d = case A.uncons d of
      Nothing -> []
      Just r -> case A.uncons r.tail of
        Nothing -> [r.head]
        _ -> (r.head <> p) A.: go r.tail

column :: forall ann. (Int -> Doc ann) -> Doc ann
column = Column

nesting :: forall ann. (Int -> Doc ann) -> Doc ann
nesting = Nesting

width :: forall ann. Doc ann -> (Int -> Doc ann) -> Doc ann
width doc f = column (\colStart -> doc <> column (\colEnd -> f (colEnd - colStart)))

pageWidth :: forall ann. (PageWidth -> Doc ann) -> Doc ann
pageWidth = WithPageWidth

fill :: forall ann. Int -> Doc ann -> Doc ann
fill n doc = width doc (\w -> spaces (n - w))

fillBreak :: forall ann. Int -> Doc ann -> Doc ann
fillBreak f x = width x (\w -> if w > f then nest f line' else spaces (f - w))

unsafeStringWithoutNewlines :: forall ann. String -> Doc ann
unsafeStringWithoutNewlines s = case S.uncons s of
  Nothing -> Empty
  Just r
    | S.null r.tail -> char r.head
    | otherwise -> text s

spaces :: forall ann. Int -> Doc ann
spaces n = unsafeStringWithoutNewlines (replicate n ' ')

foldr1 :: forall a. Monoid a => (a -> a -> a) -> Array a -> a
foldr1 f = A.unsnoc >>> case _ of
  Nothing -> mempty
  Just { init, last } -> foldr f last init

concatWith :: forall ann. (Doc ann -> Doc ann -> Doc ann) -> Array (Doc ann) -> Doc ann
concatWith = foldr1

vcat :: forall ann. Array (Doc ann) -> Doc ann
vcat = concatWith (\x y -> x <> line' <> y)

cat :: forall ann. Array (Doc ann) -> Doc ann
cat = group <<< vcat

plural :: forall doc amount. Semiring amount => Eq amount => doc -> doc -> amount -> doc
plural o m n
  | n == one = o
  | otherwise = m

enclose :: forall ann. Doc ann -> Doc ann -> Doc ann -> Doc ann
enclose l r x = l <> x <> r  

surround :: forall ann. Doc ann -> Doc ann -> Doc ann -> Doc ann
surround x l r = l <> x <> r

annotate :: forall ann. ann -> Doc ann -> Doc ann
annotate = Annotated

unAnnotate :: forall ann xxx. Doc ann -> Doc xxx
unAnnotate = alterAnnotations (const [])

----------------------------------------------------------------

layoutWadlerLeijen :: forall ann. FittingPredicate ann -> LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutWadlerLeijen fittingPredicate lo doc = best 0 0 (Cons 0 doc Nil)
  where
    best :: Int -> Int -> LayoutPipeline ann -> SimpleDocStream ann
    best _ _ Nil = SEmpty
    best nl cc (UndoAnn ds) = SAnnPop (best nl cc ds)
    best nl cc (Cons i d ds) = case d of
      Fail -> SFail
      Empty -> best nl cc ds
      Char c -> let cc' = cc+1 in SChar c (best nl cc' ds)
      Text l t -> let cc' = cc+l in SText l t (best nl cc' ds)
      Line -> SLine i (best i i ds)
      FlatAlt x _ -> best nl cc (Cons i x ds)
      Cat x y -> best nl cc (Cons i x (Cons i y ds))
      Nest j x -> let ij = i+j in best nl cc (Cons ij x ds)
      Union x y -> let 
                      x' = best nl cc (Cons i x ds)
                      y' = best nl cc (Cons i y ds)
                   in 
                      selectNicer fittingPredicate nl cc x' y'
      Column f -> best nl cc (Cons i (f cc) ds)
      WithPageWidth f -> best nl cc (Cons i (f lo.layoutPageWidth) ds)
      Nesting f -> best nl cc (Cons i (f i) ds)
      Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))

    selectNicer :: FittingPredicate ann -> Int -> Int -> SimpleDocStream ann -> SimpleDocStream ann -> SimpleDocStream ann
    selectNicer (FittingPredicate fits) lineIndent currentColumn x y
      | fits lo.layoutPageWidth (min lineIndent currentColumn) (availableWidth lineIndent currentColumn) x = x 
      | otherwise = y
    ribbonWidth = case lo.layoutPageWidth of
        AvailablePerLine lineLength ribbonFraction -> (Just <<< max 0 <<< min lineLength <<< round) (toNumber lineLength * ribbonFraction)
        Unbounded -> Nothing
    availableWidth li cc = do
        columnsLeftInLine <- case lo.layoutPageWidth of
            AvailablePerLine cpl _ribbonFrac -> Just (cpl - cc)
            Unbounded -> Nothing
        columnsLeftInRibbon <- do
            li' <- Just li
            rw <- ribbonWidth
            cc' <- Just cc
            Just (li' + rw - cc')
        Just (min columnsLeftInLine columnsLeftInRibbon)

layoutPretty :: forall ann. LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutPretty = layoutWadlerLeijen
    (FittingPredicate (\_pWidth _minNestingLevel maxWidth sdoc -> case maxWidth of
        Nothing -> true
        Just w -> fits w sdoc ))
  where
    fits :: Int -> SimpleDocStream ann -> Boolean
    fits w _ | w < 0 = false
    fits _ SFail = false
    fits _ SEmpty = true
    fits w (SChar _ x) = fits (w - 1) x
    fits w (SText l _t x) = fits (w - l) x
    fits _ (SLine _ _) = true
    fits w (SAnnPush _ x) = fits w x
    fits w (SAnnPop x) = fits w x

layoutSmart :: forall ann. LayoutOptions -> Doc ann -> SimpleDocStream ann
layoutSmart = layoutWadlerLeijen
    (FittingPredicate (\pWidth minNestingLevel maxWidth sdoc -> case maxWidth of
        Nothing -> true
        Just w -> fits pWidth minNestingLevel w sdoc ))
  where
    fits :: PageWidth -> Int -> Int -> SimpleDocStream ann -> Boolean
    fits _ _ w _ | w < 0  = false
    fits _ _ _ SFail = false
    fits _ _ _ SEmpty = true
    fits pw m w (SChar _ x) = fits pw m (w - 1) x
    fits pw m w (SText l _t x) = fits pw m (w - l) x
    fits pw m _ (SLine i x)
      | m < i, AvailablePerLine cpl _ <- pw = fits pw m (cpl - i) x
      | otherwise = true
    fits pw m w (SAnnPush _ x) = fits pw m w x
    fits pw m w (SAnnPop x) = fits pw m w x

layoutCompact :: forall ann. Doc ann -> SimpleDocStream ann
layoutCompact doc = scan 0 (L.singleton doc)
  where
    scan _ L.Nil = SEmpty
    scan col (L.Cons d ds) = case d of
      Fail -> SFail
      Empty -> scan col ds
      Char c -> SChar c (scan (col+1) ds)
      Text l t -> let col' = col+l in SText l t (scan col' ds)
      FlatAlt x _ -> scan col (L.Cons x ds)
      Line -> SLine 0 (scan 0 ds)
      Cat x y -> scan col (L.Cons x (L.Cons y ds))
      Nest _ x -> scan col (L.Cons x ds)
      Union _ y -> scan col (L.Cons y ds)
      Column f -> scan col (L.Cons (f col) ds)
      WithPageWidth f -> scan col (L.Cons (f Unbounded) ds)
      Nesting f -> scan col (L.Cons (f 0) ds)
      Annotated _ x -> scan col (L.Cons x ds)

renderShowS :: forall ann. SimpleDocStream ann -> String
renderShowS = case _ of
  SFail -> unsafeCrashWith $ "SFail can not appear uncaught in a rendered SimpleDocStream"
  SEmpty -> ""
  SChar c x -> S.fromCharArray [c] <> renderShowS x
  SText _l t x -> t <> renderShowS x
  SLine i x -> "\n" <> (replicate i ' ') <> renderShowS x
  SAnnPush _ x -> renderShowS x
  SAnnPop x -> renderShowS x        

renderDoc :: forall ann. Doc ann -> String
renderDoc doc = renderShowS (layoutPretty defaultLayoutOptions doc)

renderDoc' :: forall ann. Number -> Int -> Doc ann -> String
renderDoc' p w doc = renderShowS (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = { layoutPageWidth: AvailablePerLine w p }

renderDoc'' :: forall ann. Int -> Doc ann -> String
renderDoc'' w doc = renderShowS (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = { layoutPageWidth: AvailablePerLine w 1.0 }     