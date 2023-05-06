{-# LANGUAGE OverloadedStrings #-}

module EmailUtils (parseEmail) where 
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative (many, (<|>))
import Data.Typeable (typeOf)

emailParser :: Parser T.Text
emailParser = do 
    local <- firstParser
    _ <- char '@'
    domain <- secondParser
    return $ T.concat [local, "@", domain]

firstParser :: Parser T.Text
firstParser = do 
    fst <- letter <|> digit <|> specialCharacter
    rest <- many $ letter <|> digit <|> specialCharacter <|> char '.'
    return $ T.pack (fst:rest)

secondParser :: Parser T.Text
secondParser = do
    fst <- domainParser
    dot <- char '.'
    rest <- domainParser
    return $ T.concat ([fst, T.singleton dot, rest])

domainParser :: Parser T.Text
domainParser = do
    fst <- letter <|> digit
    rest <- many $ letter <|> digit <|> char '-'
    return $ T.pack (fst:rest)

specialCharacter :: Parser Char
specialCharacter = satisfy (`elem` ("!#$%&'*+/=?^_`{|}~-" :: String))


parseEmail :: String -> Either String T.Text
parseEmail = parseOnly emailParser . T.pack