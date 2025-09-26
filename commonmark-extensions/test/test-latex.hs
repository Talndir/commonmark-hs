{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark
import           Commonmark.Latex.Latex
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (groupBy)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.IO             (hSetEncoding, utf8, openFile,
                                        IOMode(..))
import qualified Data.Text.Lazy        as TL
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec
import           Text.Parsec.Pos

readTextFile :: FilePath -> IO Text
readTextFile fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  T.hGetContents h

main :: IO ()
main = do
  tests <- mapM (uncurry getSpecTestTree)
             [ ("test/latex.md", latexSpec)
             ]
  defaultMain $ testGroup "Tests" tests

getSpecTestTree :: FilePath
                -> SyntaxSpec Identity (Latex ()) (Latex ())
                -> IO TestTree
getSpecTestTree fp syntaxspec = do
  spectests <- getSpecTests fp
  let spectestgroups = groupBy (\t1 t2 -> section t1 == section t2)
                          spectests
  let spectestsecs = [(section x, x:xs) | x:xs <- spectestgroups]
  let parser = runIdentity . parseCommonmarkWith
                   (syntaxspec <> defaultSyntaxSpec)
  return $ testGroup fp $
    map (\(secname, tests) ->
           testGroup (T.unpack secname) $
             map (toSpecTest parser) tests)
        spectestsecs

getSpecTests :: FilePath -> IO [SpecTest]
getSpecTests fp = do
  speclines <- zip [1..] . T.lines . T.replace "→" "\t"
                <$> readTextFile fp
  return $ either (error . show) id $ runParser
             (many (try (skipMany normalLine *> parseSpecTest))
                <* skipMany normalLine <* eof) ("",1) fp
                speclines

data SpecTest = SpecTest
     { section    :: Text
     , example    :: Int
     , markdown   :: Text
     , end_line   :: Int
     , start_line :: Int
     , latex       :: Text }
  deriving (Show)

toSpecTest :: ([Tok] -> Either ParseError (Latex ()))
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = T.unpack (section st) ++ " example " ++ show (example st) ++
                 " (" ++ show (start_line st) ++ "-" ++
                 show (end_line st) ++ ")"
          expected = normalizeLatex $ latex st
          actual = normalizeLatex . renderLatex .
                   fromRight mempty $
                     (parser (tokenize "" (markdown st))
                      :: Either ParseError (Latex ()))

normalizeLatex :: Text -> Text
normalizeLatex = id

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

--- parser for spec test cases

satisfyLine :: (Text -> Bool)
            -> Parsec [(Int, Text)] (Text, Int) Text
satisfyLine f = token showTok posFromTok testTok
  where
     showTok (_,t)       = T.unpack t
     posFromTok (pos,_)  = newPos "" pos 1
     testTok (_,t)       = if f t then Just t else Nothing

parseSpecTest :: Parsec [(Int, Text)] (Text, Int) SpecTest
parseSpecTest = do
  startpos <- getPosition
  () <$ satisfyLine (== "```````````````````````````````` example")
  markdownTxt <- T.unlines <$> manyTill (satisfyLine (const True))
                                 (satisfyLine (=="."))
  latexTxt <- T.unlines <$> manyTill (satisfyLine (const True))
              (satisfyLine (== "````````````````````````````````"))
  endline <- (\x -> x - 1) . sourceLine <$> getPosition
  (sectionName, exampleNumber) <- getState
  putState (sectionName, exampleNumber + 1)
  return SpecTest{
       section = sectionName
     , example = exampleNumber
     , markdown = markdownTxt
     , end_line = endline
     , start_line = sourceLine startpos
     , latex = latexTxt
   }

normalLine :: Parsec [(Int, Text)] (Text, Int) ()
normalLine = do
  t <- satisfyLine (/= "```````````````````````````````` example")
  when ("#" `T.isPrefixOf` t) $ updateState $ \(_secname, exampnum) ->
           (T.strip $ T.dropWhile (=='#') t, exampnum)
