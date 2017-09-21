module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
 Assignment String Expr.T |
 Skip |
 Read String|
 Write Expr.T |
 Begin [Statement] |
 If Expr.T Statement Statement |
 While Expr.T Statement |
 Repeat Statement Expr.T 
 deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

read' = accept "read" -# word #- require ";" >-> buildRead
buildRead str = Read str

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite w= Write w

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin l= Begin l

if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((c, s1), s2) = If  c s1 s2

while' = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (w,b)= While w b

repeat' = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (a,b) = Repeat a b

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

--cond wstmts "should change place"
exec (Repeat cond wstmts:stmts) dict input =
  if (Expr.value wstmts dict) > 0
  then exec (cond:stmts) dict input 
  else exec (cond:Repeat cond wstmts:stmts) dict input

--if
exec (If cond thenStmts elseStmts:stmts) dict input = 
  if (Expr.value cond dict)>0 
  then exec (thenStmts: stmts) dict input
  else exec (elseStmts: stmts) dict input
--skip
exec (Skip:tail) dict input = 
  exec tail dict input

--read
exec (Read var:tail) dict (x:xs) = 
  exec tail  (Dictionary.insert (var, x) dict) xs

--write
exec (Write var:tail) dict input = 
  (Expr.value var dict):(exec tail dict input)

--begin
exec (Begin var:tail) dict input =
  exec (var ++ tail) dict input
--while
exec (While cond wstmts:stmts) dict input =
  if (Expr.value cond dict) > 0
  then exec (wstmts:(While cond wstmts:stmts)) dict input
  else exec stmts dict input
 
--assignment
exec ((Assignment str var):tail) dict input = 
  exec tail (Dictionary.insert(str, Expr.value var dict) dict) input

exec [] _ _ = []

instance Parse Statement where
 parse = (assignment ! skip ! read' ! write ! begin ! if' ! while' ! repeat')
 --shitty name i was in a hurry
 toString = whatever

--whatever :: T -> String
whatever(Assignment str expr) =
 str ++ ":=" ++ (Expr.toString expr) ++ ";\n"

whatever(Skip ) = 
 "skip;\n" 
whatever(Read str) = 
 "read" ++ str ++ ";\n"

whatever(Write expr) =
 "write" ++ (Expr.toString expr) ++ ";\n"
whatever(Begin stmts) =
 "Begin" ++ concat (map whatever stmts) ++ "end\n"

whatever(If expr thenStmts elseStmts) =
 "if" ++ (Expr.toString expr) ++ "then" ++ whatever thenStmts ++ "else" ++ whatever elseStmts ++ "\n"

whatever(While expr stmts)=
 "while" ++ (Expr.toString expr) ++ " do \n" ++ whatever stmts

--whatever(Repeat expr stmts)=
-- "repeat" ++ (Expr.toString expr) ++ "until" ++ whatever stmts ++ ";\n"



--toStringBegin (stmts:tail) =
--	Expr.toString stmts ++ toStringBegin tail

