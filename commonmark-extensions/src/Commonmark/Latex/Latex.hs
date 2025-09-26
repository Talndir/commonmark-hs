{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Commonmark.Latex.Latex where

import Commonmark
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory, takeBaseName)
import Data.Functor.Identity
import System.IO
import System.Exit
import Commonmark.Extensions.Wikilinks
import Commonmark.Extensions.Math
import Data.Char (isDigit)
import Commonmark.Blocks
import Text.Parsec
import Commonmark.TokParsers
import Control.Monad
import Data.Tree
import Data.Dynamic
import Text.Parsec.Pos
import qualified Control.Monad.State as S

latexSpec :: Monad m => SyntaxSpec m (Latex ()) (Latex ())
latexSpec = wikilinksSpec TitleAfterPipe <> mathSpec <> yamlSpec

{-
    LatexEnv n ps x = \begin{t}[ps] x \end{t}
    LatexCmd n ps x = \n[ps]{x}
    LatexInl start end x = start x end
    LatexTxt t = t
    LatexMth t = \[ t \]
    LatexSep = \n\n
-}

data Latex a
    = LatexEnv {-# UNPACK #-} !Text [Text] !(Latex a)
    | LatexCmd {-# UNPACK #-} !Text [Text] !(Latex a)
    | LatexInl !Text !Text !(Latex a)
    | LatexTxt !Text
    | LatexMth !Text
    | LatexSep
    | LatexCnc !(Latex a) !(Latex a)
    | LatexNul
    | LatexPar !(Latex a)

renderOpts :: [Text] -> Text
renderOpts [] = ""
renderOpts xs = "[" <> foldl1 (\r t -> r <> "," <> t) xs <> "]"

skipLastNewline :: Text -> Text
skipLastNewline t = case T.stripSuffix "\n" t of
    Just t' -> t'
    _ -> t

renderLatex' :: Latex a -> S.State Bool Text
renderLatex' (LatexEnv n ps x) = do
    S.put False
    return $
        "\\begin{" <> n <> "}" <> renderOpts ps <> "\n"
        <> skipLastNewline (renderLatex x)
        <> "\n\\end{" <> n <> "}"
renderLatex' (LatexCmd n ps x) = return $
    "\\" <> n <> renderOpts ps <> "{" <> renderLatex x <> "}"
renderLatex' (LatexInl start end x) = return $
    start <> renderLatex x <> end
renderLatex' (LatexTxt t) = return $
    skipLastNewline t
renderLatex' (LatexMth t) = do
    S.put False
    return $ "\\[\n" <> t <> "\n\\]"
renderLatex' LatexSep = return $
    "\n"
renderLatex' (LatexCnc x y) =
    liftA2 (<>) (renderLatex' x) (renderLatex' y)
renderLatex' LatexNul = return $
    ""
renderLatex' (LatexPar p) = do
    n <- S.get
    let ind = case n of
                False -> "\\noindent "
                True -> ""
    S.put True
    t <- renderLatex' p
    return $ ind <> t <> "\n\n"

renderLatex :: Latex a -> Text
renderLatex p = S.evalState (renderLatex' p) False

instance Show (Latex a) where
    show = T.unpack . renderLatex

instance Semigroup (Latex a) where
    LatexNul <> x = x
    x <> LatexNul = x
    (LatexTxt t1) <> (LatexTxt t2) = LatexTxt (t1 <> t2)
    --LatexSep <> LatexSep = LatexSep
    --LatexCnc x LatexSep <> LatexSep = LatexCnc x LatexSep
    --LatexSep <> LatexCnc LatexSep y = LatexCnc LatexSep y
    x <> y = LatexCnc x y

instance Monoid (Latex a) where
    mempty = LatexNul
    mappend = (<>)

sep :: Latex a
sep = LatexCnc LatexSep LatexSep

instance Rangeable (Latex a) where
    ranged _ = id

instance HasAttributes (Latex a) where
    addAttributes _ = id

instance IsInline (Latex a) where
    lineBreak = sep
    softBreak = LatexSep
    str = LatexTxt
    entity = LatexTxt
    escapedChar c = LatexTxt (T.pack $ "\\" ++ [c])
    emph t = LatexCmd "textit" [] t
    strong t = LatexCmd "textbf" [] t
    link target title desc = undefined
    image source title desc = undefined
    code t = let w = case T.stripPrefix "{" t of
                    Just k -> T.tail . snd . T.breakOn "}" $ k
                    Nothing -> t
        in LatexInl "|" "|" (LatexTxt w)
    rawInline fmt t = undefined

instance IsBlock (Latex a) (Latex a) where
    paragraph p = LatexPar p
    plain p = p
    thematicBreak = LatexNul
    blockQuote = undefined
    codeBlock info p = LatexEnv "spec" [] (LatexTxt p) <> sep
    heading level p = LatexCmd h [] p <> sep where
        h = case level of
            1 -> "section"
            2 -> "subsection"
            3 -> "subsubsection"
            5 -> "subparagraph"
            _ -> "paragraph"
    rawBlock fmt t = LatexTxt t
    referenceLinkDefinition label (dest, title) = undefined
    list typ spacing lst = LatexEnv ltyp [] contents <> sep where
        ltyp = case typ of
            BulletList _ -> "itemize"
            OrderedList _ _ _ -> "enumerate"
        contents = foldl1 LatexCnc (map (\t -> LatexCmd "item" [] t <> LatexSep) lst)

normalise :: String -> String
normalise = map f where
    f ' ' = '_'
    f x = x

normaliseT :: Text -> Text
normaliseT = T.pack . normalise . T.unpack

instance HasWikilinks (Latex a) where
    wikilink (T.stripPrefix "PhD/Thesis/" -> Just url) _ = LatexCmd "Cref" [] (LatexTxt (normaliseT url))
    wikilink _ p@(LatexTxt t)
        | T.null (T.filter (not . isDigit) (T.takeEnd 4 t)) = LatexCmd "cite" [] p
    wikilink _ p = p

instance HasMath (Latex a) where
    inlineMath t = LatexInl "$" "$" (LatexTxt t)
    displayMath t
        | T.isPrefixOf "\n\\begin" t = LatexEnv n [] (LatexTxt r)
        | otherwise = LatexMth (T.dropWhile f (T.dropWhileEnd f t))
        where
            k = T.drop 8 t
            n = T.takeWhile (/= '}') k
            m = T.length n
            r = T.dropEnd (7 + m) (T.drop (m + 2) k)
            f = (== '\n')


yamlSpec :: (Monad m, IsBlock il bl, IsInline il) => SyntaxSpec m il bl
yamlSpec = mempty
    { syntaxBlockSpecs = [ySpec]
    }
    where
        ySpec = BlockSpec
            { blockType             = "YAMLFrontmatter"
            , blockStart            = do
                pos <- getPosition
                let fileStart = initialPos (sourceName pos)
                guard $ pos == fileStart
                dashes <- many1 (symbol '-')
                guard $ length dashes == 3
                lookAhead $ void lineEnd <|> eof
                addNodeToStack $
                    Node (defBlockData ySpec) {
                        blockData = toDyn (),
                        blockStartPos = [pos]
                    } []
                return BlockStartMatch
            , blockCanContain       = const False
            , blockContainsLines    = True
            , blockParagraph        = False
            , blockContinue         = \node -> do
                pos <- getPosition
                try ( do
                        dashes <- many1 (symbol '-')
                        guard $ length dashes == 3
                        skipWhile (hasType Spaces)
                        lookAhead $ void lineEnd <|> eof
                        endOfBlock
                        return $! (pos, node)
                    ) <|> (do
                        return (pos, node)
                    )
            , blockConstructor      = \_ -> return $! plain (str "")
            , blockFinalize         = defaultFinalizer
            }

errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)

makeTitle :: String -> Latex a -> Latex a
makeTitle t p = heading 1 (LatexTxt (T.pack t)) <> p

compile :: String -> String -> IO ()
compile fname outname = do
    putStrLn fname
    toks <- tokenize fname <$> TIO.readFile fname
    let spec = latexSpec <> defaultSyntaxSpec
    createDirectoryIfMissing True (takeDirectory (normalise outname))
    let title = takeBaseName fname
    case runIdentity (parseCommonmarkWith spec toks) of
        Left e -> errExit e
        Right (r :: Latex ()) -> TIO.writeFile outname . renderLatex . makeTitle title $ r
    putStrLn $ "\t" ++ outname
    return ()

data Rose a = Leaf a | Branch a [Rose a]

flattenRose :: Monoid a => Rose a -> [a]
flattenRose (Branch n xs) = concatMap (\x -> map (n <>) (flattenRose x)) xs
flattenRose (Leaf x) = [x]

files :: Rose String
files = Branch "./" [
    Branch "Ruby/"
        [ Leaf "Introduction"
        , Leaf "Syntax and Semantics"
        , Leaf "Implementation"
        , Leaf "Type System"
        , Leaf "Compilation"
        , Leaf "Simulation"
        , Leaf "Reasoning and Verification"
        , Leaf "Evaluation" ]
    ]

inDir :: String
inDir = "/mnt/d/Omar/Documents/My Vault/PhD/Thesis/"

outDir :: String
outDir = "/home/omar/Projects/phd-thesis/src/chapters/"

compileAll :: IO ()
compileAll = do
    let fs = map (\n -> (inDir ++ n ++ ".md", outDir ++ normalise n ++ ".tex")) (flattenRose files)
    mapM_ (uncurry compile) fs 
