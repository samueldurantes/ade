module Parser
  ( Expr(..)
  , Doc(..)
  , parseAll
  ) where

import Data.Void (Void)
import Data.Maybe (fromMaybe)

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum)

type Parser = Parsec Void String

newtype Doc = Doc String
  deriving (Show)

data Expr
  = Module String [Expr]
  | Identifier String
  | Function String Expr Doc
  | Arrow Expr Expr
  deriving (Show)

sc :: Parser ()
sc = L.space
  space1
  empty
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parseIdentifier :: Parser Expr
parseIdentifier = label "Identifier" $ lexeme $
  Identifier <$> some letterChar

parseTy :: Parser Expr
parseTy = do
  l <- parseIdentifier
  r <- optional $ do
    lexeme $ string "->"
    parseTy
  pure $ maybe l (Arrow l) r

parseDoc :: Parser Doc
parseDoc = label "Doc" $ lexeme $ do
  lexeme $ string "///"
  d <- lexeme $ takeWhileP Nothing (/= '\n')
  pure $ Doc d

-- f : a -> a
parseFunction :: Parser Expr
parseFunction = label "Function" $ lexeme $ do
  d <- lexeme $ optional $ parseDoc
  n <- lexeme $ some letterChar
  lexeme $ char ':'
  t <- lexeme $ parseTy
  pure $ Function n t (fromMaybe (Doc "") d)

parseModule :: Parser Expr
parseModule = label "Module" $ lexeme $ do
  lexeme $ string "module"
  n <- lexeme $ some letterChar
  e <- lexeme $ some parseFunction
  pure $ Module n e

parseAll :: String -> Either String Expr
parseAll input =
  let
    outputE = parse
      (between sc eof parseModule)
      ""
      input
  in

  case outputE of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output
