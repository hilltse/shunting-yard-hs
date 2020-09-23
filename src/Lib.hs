module Lib where
import qualified Data.Map as Map
import Data.Char (ord, isDigit)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Precedence of operation
newtype Prec = Prec Integer deriving (Eq, Ord)
-- A binary operation
newtype Op2 = Op2 (Integer -> Integer -> Integer)

-- Associativity of operation
data Assoc = LeftAssoc | RightAssoc deriving Eq
data BinOp = BinOp Op2 Prec Assoc

prec (BinOp _ p _) = p
assoc (BinOp _ _ a) = a

data Token = Literal Integer | Operator BinOp | LPar | RPar

-- Syntax tree
data Expr = Value Integer | Apply2 Op2 Expr Expr

eval :: Expr -> Integer
eval (Value x) = x
eval (Apply2 (Op2 f) expr1 expr2) = f (eval expr1) (eval expr2)

binOps = [('+', BinOp (Op2 (+)) (Prec 3) LeftAssoc),('-', BinOp (Op2 (-)) (Prec 3) LeftAssoc),('*', BinOp (Op2 (*)) (Prec 4) LeftAssoc),('/', BinOp (Op2 div) (Prec 4) LeftAssoc)]

binOpMap = Map.fromList binOps

parseNum :: String -> Maybe (Integer, String)
parseNum [] = Nothing
parseNum xs | isDigit (head xs) = Just (go xs 0)
            | otherwise = Nothing
    where go :: String -> Integer -> (Integer, String)
          go "" v = (v, "")
          go (x:xs) v | isDigit x = let d = toInteger $ (ord x) - (ord '0') in
                                        go (xs) (v * 10 + d)
                      | otherwise = (v, (x:xs))

parseOp :: String -> Maybe (BinOp, String)
parseOp []     = Nothing
parseOp (x:xs) = do
    op <- Map.lookup x binOpMap
    return (op, xs)


stripSpace :: String -> String
stripSpace "" = ""
stripSpace (x:xs) | x == ' '  = stripSpace xs
                  | otherwise = (x:xs)

parsePar :: String -> Maybe (Token, String)
parsePar [] = Nothing
parsePar (x:xs) = do
    token <- if x == '(' then Just LPar else if x == ')' then Just RPar else Nothing
    return (token, xs)

tokenize :: String -> [Token]
tokenize "" = []
tokenize s  = let s' = stripSpace s in
              case parseNum s' of
                  Just (x, r) -> (Literal x) : (tokenize r)
                  Nothing     ->
                      case parseOp s' of
                          Just (op, r) -> (Operator op) : (tokenize r)
                          Nothing      ->
                              case parsePar s' of
                                  Just (par, r) -> par : (tokenize r)
                                  Nothing -> []

parse :: String -> Maybe Expr
parse s = let tokens = tokenize s in
    shuntingYard [] [] tokens

-- [Expr] and [BinOp] are the stacks of operands and pending tokens
shuntingYard :: [Expr] -> [Token] -> [Token] -> Maybe Expr
shuntingYard operands tokens []     = applyRemaining operands tokens
shuntingYard operands tokens (t:ts) = case t of
    LPar        -> shuntingYard operands (LPar : tokens) ts
    RPar        -> do
        (operands', tokens') <- applyRPar operands tokens
        shuntingYard operands' tokens' ts
    Literal x   -> shuntingYard ((Value x) : operands) tokens ts
    Operator op -> do
        (operands', tokens') <- applyCurrent operands tokens op
        shuntingYard operands' tokens' ts


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

applyOnce :: [Expr] -> BinOp -> Maybe [Expr]
applyOnce operands (BinOp g _ _) = do
    right <- safeHead operands
    left  <- safeHead $ tail operands
    let expr = Apply2 g left right
    let operands' = expr : (tail $ tail operands)
    return operands'

applyRPar :: [Expr] -> [Token] -> Maybe ([Expr], [Token])
applyRPar _ []                   = Nothing
applyRPar operands (t:ts) = case t of
    LPar -> Just (operands, ts)
    RPar -> Nothing
    Operator op -> do
        operands' <- applyOnce operands op
        applyRPar operands' ts

applyCurrent :: [Expr] -> [Token] -> BinOp -> Maybe ([Expr], [Token])
applyCurrent operands [] op     = Just (operands, [Operator op])
applyCurrent operands (x:xs) op = case x of
    LPar -> Just (operands, (Operator op) : (x:xs))
    RPar -> Nothing
    (Operator top) -> let (BinOp g p a) = top in
        case compare (prec op) p of
            LT -> do
                operands' <- applyOnce operands top
                applyCurrent operands' xs op
            EQ -> case assoc op of
                LeftAssoc -> do
                    operands' <- applyOnce operands top
                    applyCurrent operands' xs op
                RightAssoc -> Just (operands, (Operator op) : (x:xs))
            GT -> Just (operands, (Operator op) : (x:xs))

applyRemaining :: [Expr] -> [Token] -> Maybe Expr
applyRemaining [] _ = Nothing
applyRemaining (o:os) [] = case os of
    [] -> Just o
    _  -> Nothing
applyRemaining operands (t:ts) = case t of
    Operator op -> do
        operands' <- applyOnce operands op
        applyRemaining operands' ts
    _ -> Nothing