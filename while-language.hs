#! /usr/bin/env stack
-- stack script --resolver lts-13.17 --package parsec,text,containers

{-# LANGUAGE OverloadedStrings #-}

import           Text.Parsec
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Control.Monad (void, unless, forM_)
import           Data.Char (isSpace)
import           Data.List (sort)

type Var = String
type Env = Map Var Integer
type Parser a = Parsec Text Env a

grabState :: Parser a -> Parser Env
grabState p = p >> getState

var :: Parser String
var = many1 letter

natural :: Parser Integer
natural = read <$> many1 digit

num :: Parser Integer
num = natural <|> do {char '+'; natural}
              <|> do {char '-'; negate <$> natural}

safeDiv :: Integer -> Integer -> Integer
safeDiv _ 0 = 1
safeDiv x y = div x y

addOp :: Parser (Integer -> Integer -> Integer)
addOp = do {char '+'; pure (+)}
    <|> do {char '-'; pure (-)}

mulOp :: Parser (Integer -> Integer -> Integer)
mulOp = do {char '*'; pure (*)}
    <|> do {char '/'; pure safeDiv}

boolOp :: Parser (Bool -> Bool -> Bool)
boolOp = do {string "&&"; pure (&&)}
     <|> do {string "||";  pure (||)}

relOp :: Ord a => Parser (a -> a -> Bool)
relOp = do {char '<'; pure (<)}
    <|> do {char '>'; pure (>)}

-- |Arithmetic Expressions
aexpr :: Parser Integer
aexpr = aterm `chainl1` addOp

aterm :: Parser Integer
aterm = afact `chainl1` mulOp

afact :: Parser Integer
afact = num <|>
  (do
     varname <- var
     memo <- getState
     pure (memo M.! varname)) <|>
  (do
     char '('
     int <- aexpr
     char ')'
     pure int)

-- |Boolean Expressions
bexpr :: Parser Bool
bexpr = bterm `chainl1` boolOp

bterm :: Parser Bool
bterm =
  (do
    char '('
    b <- bexpr
    char ')'
    pure b) <|>
  (do
    string "true"
    pure True) <|>
  (do
    string "false"
    pure False) <|>
  (do
    int <- aexpr
    op <- relOp
    int' <- aexpr
    pure (op int int'))

skip :: Parser a -> Parser ()
skip p = do
    st <- getState
    p
    putState st

lookAhead' :: Parser a -> Parser a
lookAhead' p = do
    state <- getParserState
    x <- p
    s <- getState
    setParserState state
    putState s
    pure x

-- | While language commands
stmt :: Parser ()
stmt = (do
          try $ string "if"
          b <- bexpr
          string "then{"
          if b then do
            program
            string "}else{"
            skip program
            char '}'
            pure ()
          else do
            skip program
            string "}else{"
            program
            char '}'
            pure ()) <|>
       whileStmt <|>
       (do
          varname <- var
          string ":="
          int <- aexpr
          modifyState (M.insert varname int))

-- | While implemented for streams, uses lookAhead'
-- be careful with runtime exceptions
whileLoop :: Parser Bool -> Parser () -> Parser ()
whileLoop pb p = do
    b <- lookAhead pb
    if b then lookAhead' (pb *> p) *>  whileLoop pb p
         else skip $ pb *> p

whileStmt :: Parser ()
whileStmt = do
    string "while"
    whileLoop bexpr $ do
        string "do{"
        program
        char '}'
        pure ()

-- | A program is many statements
program :: Parser ()
program = void $ sepBy stmt $ char ';'

fixInput :: Text -> Text
fixInput t = let t' = T.filter (not . isSpace) t
            in T.replace "and" "&&" $ T.replace "or" "||" t'

main :: IO ()
main = do
    l <- fixInput <$> TIO.getContents
    let Right env = runParser (grabState program) M.empty "" l
    forM_ (sort (M.toList env)) $ \ (var,int) -> putStrLn $ var ++ " " ++ show int
