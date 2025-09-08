{-# LANGUAGE OverloadedStrings #-}
module Parser (
  readExpr,
  readExprFile
) where

import LispVal
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)

-- Lexer

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
  Tok.commentStart    = "{-"
, Tok.commentEnd      = "-}"
, Tok.commentLine     = "--"
, Tok.opStart         = Tok.opLetter style
, Tok.opLetter        = oneOf ":!#$%%&*+./<=>?@\\^|-~"
, Tok.identStart      = letter <|> oneOf "-+/*=|&><"
, Tok.identLetter     = digit <|> letter <|> oneOf "?+=|&-/"
, Tok.reservedOpNames = ["'", "\""]
, Tok.reservedNames   = ["Nil", "#t", "#f"]
}

Tok.TokenParser { Tok.parens       = m_parens
                , Tok.identifier   = m_identifier
                , Tok.reservedOp   = m_reservedOp
                , Tok.reserved     = m_reserved
                } = Tok.makeTokenParser style

-- Parser

parseAtom :: Parser LispVal
parseAtom = Atom . T.pack <$> m_identifier

parseText :: Parser LispVal
parseText = do
    m_reservedOp "\""
    p <- many1 $ noneOf "\""
    m_reservedOp "\""
    return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
    _ <- char '-'
    d <- many1 digit
    return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList = List <$> (parseExpr `sepBy` many1 (oneOf " \n\r"))

parseSExp :: Parser LispVal
parseSExp = List <$> m_parens (parseExpr `sepBy` many1 (oneOf " \n\r"))

parseQuote :: Parser LispVal
parseQuote = do
    m_reservedOp "\'"
    x <- parseExpr
    return $ List [Atom "quote", x]

parseReserved :: Parser LispVal
parseReserved =
        (m_reserved "Nil" >> return Nil)
    <|> (m_reserved "#t"  >> return (Bool True))
    <|> (m_reserved "#f"  >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseReserved
    <|> parseNumber
    <|> try parseNegNum
    <|> parseAtom
    <|> parseText
    <|> parseQuote
    <|> parseSExp

-- Reader

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"