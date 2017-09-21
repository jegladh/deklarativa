--      The value size is an integer (Int), defined to be 
--      the sum of twelve and thirteen.
import Pictures
size :: Int
size = 12+13

--      The function to square an integer.

square :: Int -> Int
square n = n*n

--      The function to double an integer.
        
double :: Int -> Int
double n = 2*n

--      An example using double, square and size.
         
example :: Int
example = double (size - square (2+2))

doubleSquare :: Int -> Int
doubleSquare n = square (double n)

squareDouble :: Int -> Int
squareDouble n = double (square n)

prefix :: Int -> [a] -> [a]
prefix _ [] = []
prefix 0 _ = []
prefix n (x:xs) = take n (x:xs)
