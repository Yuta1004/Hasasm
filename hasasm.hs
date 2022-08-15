import System.Environment(getArgs)
import Control.Monad.State

{-- Expr --}
data OpKind = Add | Minus | Mul | Div deriving Show

op2s :: OpKind -> String
op2s Add = "+"
op2s Minus = "-"
op2s Mul = "*"
op2s Div = "/"

s2op :: String -> OpKind
s2op "+" = Add
s2op "-" = Minus
s2op "*" = Mul
s2op "/" = Div

op2f :: OpKind -> (Int -> Int -> Int)
op2f Add = (+)
op2f Minus = (-)
op2f Mul = (*)
op2f Div = div

priority :: [[OpKind]]
priority = [[Add, Minus], [Mul, Div]]

data Expr = Op { kind :: OpKind, left, right :: Expr }
          | Num { val :: Int }

eval :: Expr -> Int
eval (Op o l r) = (op2f o) (eval l) (eval r)
eval (Num v) = v

parse :: [String] -> Either String Expr
parse s = _parse s priority

_parse :: [String] -> [[OpKind]] -> Either String Expr
_parse es@(_:_:_) pall@(p:ps) =
    case split es p of
        Left es -> _parse es ps
        Right (o, l, r) -> do
            left <- _parse l pall
            right <- _parse r pall
            Right Op { kind = o, left = left, right = right}
_parse (h:[]) (_:_) = Right Num { val = read h :: Int }
_parse _ [] = Left "found unregistered op"
_parse [] _ = Left "found unexpected EOL"

split :: [String] -> [OpKind] -> Either [String] (OpKind, [String], [String])
split es os =
    case break (\e -> elem e $ map op2s os) $ reverse es of
        (es, []) -> Left (reverse es)
        (rrev, lrev) -> let o = s2op $ head lrev
                            l = reverse $ tail lrev
                            r = reverse rrev
                        in Right (o, l, r)

{-- Interpreter --}
type Inner = (Int, Int, [String], [Int])    -- PC, TMP, Memory(Program), Memory(Data)
type MState = State Inner ()

true :: MState
true = state $ \(pc, _, pmem, dmem) -> ((), (pc+1, 1, pmem, dmem))

false :: MState
false = state $ \(pc, _, pmem, dmem) -> ((), (pc+1, 0, pmem, dmem))

jmp :: [String] -> MState
jmp as = state $ \(pc, tmp, pmem, dmem) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, pmem, dmem))
        Right e -> let v = eval e
                    in ((), (pc+v, tmp, pmem, dmem))

jmpt :: [String] -> MState
jmpt as = state $ \(pc, tmp, pmem, dmem) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, pmem, dmem))
        Right e ->
            case (tmp, eval e) of
                (0, _) -> ((), (pc+1, tmp, pmem, dmem))
                (_, d) -> ((), (pc+d, tmp, pmem, dmem))

jmpf :: [String] -> MState
jmpf as = state $ \(pc, tmp, pmem, dmem) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, pmem, dmem))
        Right e ->
            case (tmp, eval e) of
                (0, d) -> ((), (pc+d, tmp, pmem, dmem))
                (_, _) -> ((), (pc+1, tmp, pmem, dmem))

use :: [String] -> MState
use as = state $ \(pc, tmp, pmem, dmem) ->
    case parse as of
        Left _ -> ((), (pc+1, 0, pmem, dmem))
        Right e -> let tmp' = memr dmem (eval e) 0
                    in ((), (pc+1, tmp', pmem, dmem))

set :: [String] -> MState
set as = state $ \(pc, tmp, pmem, dmem) -> do
    let (ns, vst) = split_args as
    let vs = map (\e -> if e == "@" then (show tmp); else e) vst
    case (parse ns, parse vs) of
        (Left _, _) -> ((), (pc+1, tmp, pmem, dmem))
        (_, Left _) -> ((), (pc+1, tmp, pmem, dmem))
        (Right ne, Right ve) -> let dmem' = memw dmem (eval ne) (eval ve)
                                 in ((), (pc+1, tmp, pmem, dmem'))
    where
        split_args = (\(as, bst) -> (as, tail bst)) . break (== ".")

add :: [String] -> MState
add as = state $ \(pc, tmp, pmem, dmem) -> do
    let (ns, vst) = split_args as
    let vs = map (\e -> if e == "@" then (show tmp); else e) vst
    case (parse ns, parse vs) of
        (Left _, _) -> ((), (pc+1, tmp, pmem, dmem))
        (_, Left _) -> ((), (pc+1, tmp, pmem, dmem))
        (Right ne, Right ve) -> let e = (memr dmem (eval ne) 0) + (eval ve)
                                    dmem' = memw dmem (eval ne) e
                                 in ((), (pc+1, tmp, pmem, dmem'))
    where
        split_args = (\(as, bst) -> (as, tail bst)) . break (== ".")

nop :: MState
nop = state $ \(pc, tmp, pmem, gmem) -> ((), (pc+1, tmp, pmem, gmem))

exec :: [String] -> MState
exec [] = nop
exec (c:as) =
    case c of
        "$true" -> true
        "$false" -> false
        "$jmp" -> jmp as
        "$jmpt" -> jmpt as
        "$jmpf" -> jmpf as
        "$use" -> use as
        "$set" -> set as
        "$add" -> add as
        _ -> nop

interpreter :: MState
interpreter = do
    (pc, _, pmem, _) <- get
    case memr0 pmem pc of
        Left _ -> return ()
        Right stmt -> do
            exec $ words stmt
            interpreter

{-- Memory --}
memr0 :: [a] -> Int -> Either () a
memr0 m n | n < length m = Right $ m !! n
          | otherwise = Left ()

memr :: [a] -> Int -> a -> a
memr m n d =
    case memr0 m n of
        Left _ -> d
        Right e -> e

memw0 :: [a] -> Int -> a -> Either () [a]
memw0 m n e | n < length m = Right $ hs ++ [e] ++ ts
            | otherwise = Left ()
    where
        (hs, tts) = splitAt n m
        ts = tail tts

memw :: [a] -> Int -> a -> [a]
memw m n e =
    case memw0 m n e of
        Left _ -> m
        Right m' -> m'

out :: Inner -> IO ()
out i = do
    let (pc, tmp, pmem, dmem) = i
    putStrLn $ "Register"
    putStrLn $ "  * Program Counter: " ++ (show pc)
    putStrLn $ "  * Temporary: " ++ (show tmp)
    putStrLn $ "Memory"
    putStrLn $ "  * Data: " ++ (show dmem)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        1 -> do
            f <- readFile $ args !! 0
            out $ execState interpreter (0, 0, lines f, replicate 10 0)
        _ -> putStrLn $ "Usage: ./hasasm filepath"
