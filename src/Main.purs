module Main where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable)
import Data.List.Lazy as LL
import Effect (Effect)
import Effect.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Pretty (class Pretty, pretty, reflow)
import PrettyPrinter (Doc, FusionDepth(..), PageWidth(..), align, brackets, column, comma, concatWith, dot, emptyDoc, encloseSep, flatAlt, fuse, group, hang, hardline, hsep, indent, layoutCompact, layoutPretty, layoutSmart, lbracket, line, line', list, nest, nesting, pageWidth, parens, pipe, rbracket, renderDoc'', renderShowS, replicate, sep, softline, softline', surround, text, tupled, vsep, width, (<+>))

prettyType :: forall ann f. Foldable f => f (Doc ann) -> Doc ann
prettyType = align <<< sep <<< LL.toUnfoldable <<< LL.zipWith (<+>) (text "::" LL.: LL.repeat (text "->")) <<< LL.fromFoldable

prettyDecl :: forall ann p f. Pretty p => Foldable f => p -> f (Doc ann) -> Doc ann
prettyDecl n tys = pretty n <+> prettyType tys

doc :: forall ann. Doc ann
doc = prettyDecl "example" (map text ["Int", "Bool", "Char", "IO ()"])

----------------------------------------------------------------------

open :: forall ann. Doc ann
open = flatAlt (text "") (text "{ ")

close :: forall ann. Doc ann  
close = flatAlt (text "") (text " }")

separator :: forall ann. Doc ann
separator = flatAlt (text "") (text "; ")

prettyDo :: forall ann. Array (Doc ann) -> Doc ann
prettyDo xs = group (text "do" <+> align (encloseSep open close separator xs))

statements :: forall ann. Array (Doc ann)
statements = map text ["name:_ <- getArgs", "let greet = \"Hello, \" <> name", "putStrLn greet"]

main :: Effect Unit
main = do
  log $ renderDoc'' 80 (prettyDo statements)
  log "-----------------------------------"
  log $ renderDoc'' 20 (prettyDo statements)
  log "-----------------------------------"
  log $ renderDoc'' 80 doc
  log "-----------------------------------"
  log $ renderDoc'' 20 doc
  log "--------------Examples-------------"
  logShow $ vsep [text "hello", parens emptyDoc, text "world"]
  log "-----------------------------------"
  logShow $ vsep [nest 4 (vsep (map text ["lorem", "ipsum", "dolor"])), text "sit", text "amet"]
  log "-----------------------------------"
  let doc0 = text "lorem ipsum" <> line <> text "dolor sit amet"
  logShow doc0
  logShow $ group doc0
  log "-----------------------------------"
  let doc1 = text "lorem ipsum" <> line' <> text "dolor sit amet"
  logShow doc1
  logShow $ group doc1
  log "-----------------------------------"
  let doc2 = text "lorem ipsum" <> softline <> text "dolor sit amet"
  log $ renderDoc'' 80 doc2
  log $ renderDoc'' 10 doc2
  log "-----------------------------------"
  let doc3 = text "ThisWord" <> softline' <> text "IsWayTooLong"
  log $ renderDoc'' 80 doc3
  log $ renderDoc'' 10 doc3
  log "-----------------------------------"
  let doc4 = text "lorem ipsum" <> hardline <> text "dolor sit amet"
  log $ renderDoc'' 1000 doc4
  logShow $ group doc4
  log "-----------------------------------"
  logShow $ text "lorem" <+> vsep (map text ["ipsum", "dolor"])
  logShow $ text "lorem" <+> align (vsep (map text ["ipsum", "dolor"]))
  log "-----------------------------------"
  let doc5 = reflow "Indenting these words with hang"
  log $ renderDoc'' 24 (text "prefix" <+> hang 4 doc5)
  log $ renderDoc'' 24 (text "prefix" <+> nest 4 doc5)
  log $ renderDoc'' 24 (text "prefix" <+> indent 4 doc5)
  log "-----------------------------------"
  let doc6 = text "list" <+> align (encloseSep lbracket rbracket comma (map pretty [1,20,300,4000]))
  log $ renderDoc'' 80 doc6
  log $ renderDoc'' 10 doc6
  log "-----------------------------------"
  let doc7 = list (map pretty [1,20,300,4000])
  log $ renderDoc'' 80 doc7
  log $ renderDoc'' 10 doc7
  log "-----------------------------------"
  let doc8 = tupled (map pretty [1,20,300,4000])
  log $ renderDoc'' 80 doc8
  log $ renderDoc'' 10 doc8
  log "-----------------------------------"
  logShow $ text "hello" <+> text "world"
  log "-----------------------------------"
  logShow $ concatWith (surround dot) (map text ["Data", "Text", "Prettyprint", "Doc"])
  log "-----------------------------------"
  -- let docs = LL.take 20 (LL.cycle (LL.fromFoldable (map text ["lorem", "ipsum", "dolor", "sit", "amet"])))
  -- log $ renderDoc'' 80 (text "Docs:" <+> fillSep (LL.toUnfoldable docs))
  -- log $ renderDoc'' 40 (text "Docs:" <+> fillSep (LL.toUnfoldable docs))
  log "-----------------------------------"
  let doc9 = text "prefix" <+> column (\l -> text "| <- column" <+> pretty l)
  logShow $ vsep (map (\n -> indent n doc9) [0,4,8])
  log "-----------------------------------"
  let doc10 = text "prefix" <+> nesting (\l -> brackets (text "Nested:" <+> pretty l))
  logShow $ vsep (map (\n -> indent n doc10) [0,4,8])
  log "-----------------------------------"
  let annotate doc = width (brackets doc) (\w -> text " <- width:" <+> pretty w)
  logShow $ align (vsep (map annotate [text "---", text "------", indent 3 (text "---"), vsep [text "---", indent 4 (text "---")]]))
  log "-----------------------------------"
  let 
    prettyPageWidth :: Partial => _
    prettyPageWidth (AvailablePerLine l r) = text "Width:" <+> pretty l <> text ", ribbon fraction:" <+> pretty r
  let doc11 = text "prefix" <+> pageWidth (brackets <<< (unsafePartial prettyPageWidth))
  log $ renderDoc'' 32 (vsep (map (\n -> indent n doc11) [0,4,8]))
  log "-----------------------------------"
  let oftenUsed = fuse Shallow (text "a" <> text "b" <> pretty 'c' <> text "d")
  logShow $ hsep (A.replicate 5 oftenUsed)
  log "-----------------------------------"
  let doc12 = hang 4 (vsep [text "lorem", text "ipsum", hang 4 (vsep [text "dolor", text "sit"])])
  logShow doc12
  log $ renderShowS (layoutCompact doc12)
  log "-----------------------------------"
  let fun x = hang 2 ((text "fun(") <> softline' <> x) <> (text ")")
  let doc13 = (fun <<< fun <<< fun <<< fun <<< fun) (align (list [text "abcdef", text "ghijklm"]))
  let hr = pipe <> pretty (replicate (26-2) '-') <> pipe
  let go layouter x = (renderShowS <<< layouter { layoutPageWidth: AvailablePerLine 26 1.0 }) (vsep [hr, x, hr])
  log $ go layoutPretty doc13
  log $ go layoutSmart doc13
  log "-----------------------------------"