import Data.Char
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
type MState = State Inner (IO ())

true :: MState
true = state $ \(pc, _, pmem, dmem) -> (pure (), (pc+1, 1, pmem, dmem))

false :: MState
false = state $ \(pc, _, pmem, dmem) -> (pure (), (pc+1, 0, pmem, dmem))

jmp :: [Int] -> MState
jmp args = state $ \(pc, tmp, pmem, dmem) -> (pure (), (pc+d, tmp, pmem, dmem))
    where
        d = args !! 0

jmpt :: [Int] -> MState
jmpt args = state $ \(pc, tmp, pmem, dmem) ->
    case (tmp, args !! 0) of
        (0, _) -> (pure (), (pc+1, tmp, pmem, dmem))
        (_, d) -> (pure (), (pc+d, tmp, pmem, dmem))

jmpf :: [Int] -> MState
jmpf args = state $ \(pc, tmp, pmem, dmem) ->
    case (tmp, args !! 0) of
        (0, d) -> (pure (), (pc+d, tmp, pmem, dmem))
        (_, _) -> (pure (), (pc+1, tmp, pmem, dmem))

use :: [Int] -> MState
use args = state $ \(pc, tmp, pmem, dmem) ->
    let tmp' = memr dmem (args !! 0) 0
    in (pure (), (pc+1, tmp', pmem, dmem))

set :: [Int] -> MState
set args = state $ \(pc, tmp, pmem, dmem) ->
    let dmem' = memw dmem (args !! 0) (args !! 1)
    in (pure (), (pc+1, tmp, pmem, dmem'))

add :: [Int] -> MState
add args = state $ \(pc, tmp, pmem, dmem) ->
    let e = (memr dmem (args !! 0) 0) + (args !! 1)
        dmem' = memw dmem (args !! 0) e
    in (pure (), (pc+1, tmp, pmem, dmem'))

nop :: MState
nop = state $ \(pc, tmp, pmem, gmem) -> (pure (), (pc+1, tmp, pmem, gmem))

mprintRaw :: MState
mprintRaw = state $ \(pc, tmp, pmem, gmem) -> (putStr $ show tmp, (pc+1, tmp, pmem, gmem))

mprintChar :: MState
mprintChar = state $ \(pc, tmp, pmem, gmem) -> (putChar $ chr tmp, (pc+1, tmp, pmem, gmem))

readArgs :: [String] -> Int -> [Int]
readArgs [] _ = []
readArgs args v =
    case break (== ".") args of
        (hs, []) -> [ss2i hs]
        (hs, ts) -> [ss2i hs] ++ (readArgs (tail ts) v)
    where
        ss2i ss = case parse $ map (\e -> if e == "@" then (show v); else e) ss of
            Left _ -> 0
            Right e -> eval e

exec :: [String] -> MState
exec [] = nop
exec (c:args) = do
    (_, tmp, _, _) <- get
    let argsi = readArgs args tmp
    case c of
        "$true" -> true
        "$false" -> false
        "$jmp" -> jmp argsi
        "$jmpt" -> jmpt argsi
        "$jmpf" -> jmpf argsi
        "$use" -> use argsi
        "$set" -> set argsi
        "$add" -> add argsi
        "$printr" -> mprintRaw
        "$printc" -> mprintChar
        _ -> nop

interpreter :: MState
interpreter = do
    (pc, tmp, pmem, gmem) <- get
    case memr0 pmem pc of
        Left _ -> return $ pure ()
        Right stmt -> do
            let (io, stat) = runState (exec $ words stmt) (pc, tmp, pmem, gmem)
            let (io2, stat2) = runState interpreter stat
            state $ \_ -> (io >>= \_ -> io2, stat2)

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

{-- main --}
debug :: MState -> Inner -> IO ()
debug m i = do
    let (io, (pc, tmp, pmem, dmem)) = runState m i
    io
    putStrLn $ "---"
    putStrLn $ "Register"
    putStrLn $ "  * Program Counter: " ++ (show pc)
    putStrLn $ "  * Temporary: " ++ (show tmp)
    putStrLn $ "Memory"
    putStrLn $ "  * Data: " ++ (show dmem)

prod :: MState -> Inner -> IO ()
prod m i = evalState m i

main :: IO ()
main = do
    args <- getArgs
    case length args of
        2 -> do
            f <- readFile $ args !! 0
            prod interpreter (0, 0, lines f, replicate 100 0)
        _ -> putStrLn $ "Usage: ./hasasm filepath"
