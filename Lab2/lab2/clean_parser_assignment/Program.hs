module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] -- to be defined


instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = writer
writer :: T -> String
writer (Program []) = []
writer (Program (head:tail)) = Statement.toString head ++ (writer (Program tail))

exec :: T -> [Integer] -> [Integer]
exec _ [] = []             
exec(Program startProg) intlist = Statement.exec startProg (Dictionary.empty) intlist
