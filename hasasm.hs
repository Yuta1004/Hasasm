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

{-- Command --}
type MState = State (Int, Int, Int, Int) ()

true :: MState
true = state $ \(pc, _, r0, r1) -> ((), (pc+1, 1, r0, r1))

false :: MState
false = state $ \(pc, _, r0, r1) -> ((), (pc+1, 0, r0, r1))

jmp :: [String] -> MState
jmp as = state $ \(pc, tmp, r0, r1) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, r0, r1))
        Right e -> let v = eval e
                    in ((), (pc+v, tmp, r0, r1))

jmpt :: [String] -> MState
jmpt as = state $ \(pc, tmp, r0, r1) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, r0, r1))
        Right e ->
            case (tmp, eval e) of
                (0, _) -> ((), (pc+1, tmp, r0, r1))
                (_, d) -> ((), (pc+d, tmp, r0, r1))

jmpf :: [String] -> MState
jmpf as = state $ \(pc, tmp, r0, r1) ->
    case parse as of
        Left _ -> ((), (pc+1, tmp, r0, r1))
        Right e ->
            case (tmp, eval e) of
                (0, d) -> ((), (pc+d, tmp, r0, r1))
                (_, _) -> ((), (pc+1, tmp, r0, r1))

use :: [String] -> MState
use as = state $ \(pc, tmp, r0, r1) ->
    case parse as of
        Left _ -> ((), (pc+1, 0, r0, r1))
        Right e ->
            case eval e of
                0 -> ((), (pc+1, r0, r0, r1))
                1 -> ((), (pc+1, r1, r0, r1))

set :: [String] -> MState
set as = state $ \(pc, tmp, r0, r1) -> do
    let (ns, vst) = split_args as
    let vs = map (\e -> if e == "@" then (show tmp); else e) vst
    case (parse ns, parse vs) of
        (Left _, _) -> ((), (pc+1, tmp, r0, r1))
        (_, Left _) -> ((), (pc+1, tmp, r0, r1))
        (Right ne, Right ve) ->
            case (eval ne, eval ve) of
                (0, v) -> ((), (pc+1, tmp, v, r1))
                (1, v) -> ((), (pc+1, tmp, r0, v))
    where
        split_args = (\(as, bst) -> (as, tail bst)) . break (== ".")

add :: [String] -> MState
add as = state $ \(pc, tmp, r0, r1) -> do
    let (ns, vst) = split_args as
    let vs = map (\e -> if e == "@" then (show tmp); else e) vst
    case (parse ns, parse vs) of
        (Left _, _) -> ((), (pc+1, tmp, r0, r1))
        (_, Left _) -> ((), (pc+1, tmp, r0, r1))
        (Right ne, Right ve) ->
            case (eval ne, eval ve) of
                (0, v) -> ((), (pc+1, tmp, r0+v, r1))
                (1, v) -> ((), (pc+1, tmp, r0, r1+v))
    where
        split_args = (\(as, bst) -> (as, tail bst)) . break (== ".")

exec :: [String] -> MState
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

interpreter :: MState
interpreter = do
    (pc, tmp, r0, r1) <- get
    case program pc of
        Left _ -> return ()
        Right stmt -> do
            exec stmt
            interpreter

memr :: [a] -> Int -> a -> a
memr m n d | n < length m = m !! n
           | otherwise = d

memw :: [a] -> Int -> a -> [a]
memw m n e | n < length m =  hs ++ [e] ++ ts
           | otherwise = m
    where
        (hs, tts) = splitAt n m
        ts = tail tts

program :: Int -> Either () [String]
program n | n < length program = Right $ program !! n
          | otherwise = Left ()
    where
        program = map words ["$set 0 . 10", "$set 1 . 0", "$use 0", "$jmpf 5", "$use 0", "$add 1 . @ * 10", "$add 0 . -1", "$jmp -5", "$use 1", "$set 1 . @ / 10"]

main :: IO ()
main = print $ runState interpreter (0, 0, 0, 0)

{--

$set 0 . 10     // レジスタ0に値10をセット
$set 1 . 0      // レジスタ1に値0をセット
$use 0          // 一時レジスタにレジスタ0の値をセット
$jmpf 5         // 比較結果がfalse(=一時レジスタの値が0)なら5命令ジャンプ
$use 0          // 一時レジスタにレジスタ0の値をセット
$add 1 . @ * 10 // レジスタ1に一時レジスタの値を10倍した結果を加算
$add 0 . -1     // レジスタ0に-1を加算
$jmp -5         // -5命令ジャンプ
$use 1          // 一時レジスタにレジスタ1の値をセット
$set 1 . @ / 10 // レジスタ1に一時レジスタを10で割った結果をセット

--}
