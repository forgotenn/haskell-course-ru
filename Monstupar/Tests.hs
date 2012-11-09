module Monstupar.Tests where

import Monstupar.Core
import Monstupar.Derived

--------------------------------------------------------------------------------
-- В помощь хозяйке

mustParse s p = case runParser p s of
    Left  _ -> False
    Right _ -> True

mustFail s = not . mustParse s

infixl 2 &.&
(&.&) p1 p2 x = p1 x && p2 x

--------------------------------------------------------------------------------
-- Тесты

-- Правильная скобочная последовательность
balPar = bp >> eof where
    bp = (do
          char '('
          bp
          char ')'
          bp) <|> ok

balParTest = mustParse ""
         &.& mustFail  "("
         &.& mustFail  ")"
         &.& mustParse "()"
         &.& mustParse "(())()(())()"
         &.& mustFail  "())()(())()"
         &.& mustFail  "(())()(()()"
         &.& mustFail  "())()(()()"
         $ balPar

-- Список натуральных чисел
-- тут следует использовать класс Read

checkDigit :: Char -> Bool
checkDigit a = '0' <= a && a <= '9' 

parseInt :: Monstupar Char Int
parseInt = do
    s' <- many1 (like checkDigit)
    return (read s')

natList :: Monstupar Char [Int]
natList = do
            p <- parse
            eof
            return p where
    parse = do
        n <- parseInt
        m <- many $ do
                        char ','
                        x <- parseInt
                        return x
        return (n:m)


natListTest = mustFail  ""
          &.& mustParse "0"
          &.& mustParse "0,1"
          &.& mustFail  "0,1,"
          &.& mustParse "10,20,12,3423,2342,234,2234,2342,22342,22232,17583,9573"
          &.& mustFail  "10,20,12,3423,2342,234,-2234,2342,22342,22232,17583,9573"
          &.& mustFail  "10,20,12,3423,0.,234,234,2342,22342,22232,17583,9573"
          $ natList

doBlockTest = mustParse "do {a; b <- c; do { alsdjflka;jakj; }; akjhad <- do { e; f; qq <- q; }; g;}" 
          &.& mustParse "a"
          &.& mustParse "do {a;}"
          &.& mustParse "do {do {a;}; a <- b;}"
          &.& mustFail "a <- b <- c"
          &.& mustFail "a;"
          &.& mustFail "do {};" 
          $ doBlock

spaces :: Monstupar Char ()
spaces = do
    many $ like (`elem` [' ', '\t', '\n'])
    return ()

checkLetter :: Char -> Bool
checkLetter a = ('a' <= a && a <= 'z') || ('A' <= a && a <= 'Z')
 
parseIdentificator :: Monstupar Char String
parseIdentificator = do
   s' <- many1 (like checkLetter)
   return s'

{-
parseSimpleAction :: Monstupar Char Action
parseSimpleAction = do
    spaces
    a <- parseIdentificator
    spaces
    char ';'
    return $ Action a

parseActionWithArrow :: Monstupar Char Action
parseActionWithArrow = do
    spaces
    a <- parseIdentificator
    spaces
    string "<-"
    spaces
    b <- parseIdentificator
    spaces
    char ';'
    return $ Assignment a b 
    
parseAction :: Monstupar Char Action
parseAction = do
    parseSimpleAction <|> parseActionWithArrow

parseDoBlock :: Monstupar Char Action
parseDoBlock = do
    spaces
    string "do"
    spaces    
    char '{'
    a <- many1 parseAction
    spaces
    string "};"
    return $ SimpleDoBlock a
 
parseDoBlockWithAssignment :: Monstupar Char Action
parseDoBlockWithAssignment = do
    spaces
    a <- parseIdentificator
    spaces
    string "<-"
    b <- parseDoBlock
    return $ DoBlockWithAssignment a b 

data Action = Assignment String String | Action String 
            | SimpleDoBlock [Action] | DoBlockWithAssignment String Action

doNotation :: Monstupar Char [Action]
doNotation = undefined
-}

data Action = Simple String | Composite [DoAction] deriving (Read, Show)
data DoAction = NoAssignment Action | Assignment String Action deriving (Read, Show)

parseAction :: Monstupar Char Action
parseAction = parseComposite <|> parseSimple

parseSimple :: Monstupar Char Action
parseSimple = do
    spaces
    a <- parseIdentificator
    return $ Simple a
     

parseComposite :: Monstupar Char Action
parseComposite = do
    spaces >> string "do" >> spaces >> char '{' >> spaces
    a <- many1 $ do
        spaces
        b <- parseDoAction
        spaces >> char ';'
        return b
    spaces >> char '}'
    return $ Composite a
    

parseDoAction :: Monstupar Char DoAction
parseDoAction = parseAssignment <|> parseNoAssignment 

parseNoAssignment :: Monstupar Char DoAction
parseNoAssignment = do
    spaces
    a <- parseAction
    return $ NoAssignment a
 
parseAssignment :: Monstupar Char DoAction
parseAssignment = do
    spaces
    a <- parseIdentificator
    spaces >> string "<-" >> spaces
    b <- parseAction
    return $ Assignment a b

doBlock :: Monstupar Char Action
doBlock = do
    a <- parseAction
    eof
    return a
