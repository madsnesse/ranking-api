{-# LANGUAGE OverloadedStrings #-}

module EmailUtils (parseEmail) where 
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative (many, (<|>))

-- This module contains a parser for email addresses. It is used to validate that email addresses are correctly formatted.
emailParser :: Parser T.Text
emailParser = do 
    local <- firstParser
    _ <- char '@'
    domain <- secondParser
    return $ T.concat [local, "@", domain]

-- parse what is before the @
firstParser :: Parser T.Text
firstParser = do 
    first <- letter <|> digit <|> specialCharacter -- first character cant be .
    rest <- many $ letter <|> digit <|> specialCharacter <|> char '.'
    return $ T.pack (first:rest)

-- parse what is after the @
secondParser :: Parser T.Text
secondParser = do
    first <- domainParser
    dot <- char '.'
    rest <- domainParser
    return $ T.concat ([first, T.singleton dot, rest])

-- parse what is after the .
domainParser :: Parser T.Text
domainParser = do
    first <- letter <|> digit
    rest <- many $ letter <|> digit <|> char '-'
    return $ T.pack (first:rest)

specialCharacter :: Parser Char
specialCharacter = satisfy (`elem` ("!#$%&'*+/=?^_`{|}~-" :: String))


parseEmail :: String -> Either String T.Text
parseEmail = parseOnly emailParser . T.pack