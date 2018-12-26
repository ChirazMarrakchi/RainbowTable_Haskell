
import  Data.Maybe 
import RainbowAssign
import qualified Data.Map as Map


pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 4            -- length of each password
nLetters = 3            -- number of letters to use in passwords: 
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt" -- filename to store the table

convertBase :: Int -> Int -> Int -> Int -> Int
convertBase _ _ 0 res = res
convertBase n b pLen res =
       
       convertBase (div n b) b (pLen-1) (res*10 + (mod n b))
-- this function flips the digits in a number exp = 476 -> 674
flipDigits :: Int -> Int -> Int
flipDigits 0 sum = sum
flipDigits n sum = flipDigits (div n 10) (sum*10 + mod n 10)

-- this function is the last step in conversion 
cnvrtobase :: Int -> Int-> Int -> Int
cnvrtobase n b pLen = flipDigits h 0
       where h = convertBase n b pLen 0

-- take pwLength least significant digits
leastDig :: Int -> Int -> Int
leastDig pwLength n = mod n (10^pwLength)
leastDig' = leastDig pwLength

-- convert number to string
toString :: Int -> Int -> Passwd
toString 1 _ = []
toString pwLength n = toLetter (div n (10^pwLength)) : toString  (pwLength-1) (mod n  (10^pwLength))
toString' = toString (pwLength+1)

--  pwReduce takes pwdh : hashed password 



pwReduce ::  Hash -> Passwd
pwReduce pwdh = toString' (leastDig' (cnvrtobase  (fromEnum(pwdh)) nLetters pwLength))
-- draft example 
pw = "welcome"
pwq = pwHash "baca"
pwq' = pwReduce pwq

-- performs #= width hash/reduce function
opera :: Passwd -> Int -> Hash
opera pwd 1 = pwHash pwd
opera pwd width  = opera (pwReduce (hs pwd) ) (width-1) 
      where hs pwd =  pwHash pwd

{-
Create a function rainbowTable that takes two arguments: the “width” of the table, and the list of initial passwords. 
It should return a Map.Map that maps the final Hash values onto the Passwd values at the start of the chain.
-}

table :: Map.Map Hash Passwd  
table = Map.fromList  []

-- Function addElt aims to add elements (key , value) to Table

addElt ::  Passwd -> Map.Map Hash Passwd
addElt  x  = Map.insert   (opera x width ) x table

rainbowTable ::Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable width []   = table
rainbowTable width (x:lst)   = Map.union  (addElt  x ) (rainbowTable width lst  )

--Creating Reading Writing Tables 

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

testa = do
  table <- readTable filename
  return (Map.lookup 0 table)


--Reverse Hashing 
-- draft example table table = Map.fromList([ ("John",pwHash("John")) , ("Bob",pwHash("IT"))])
-- iterate findKey h times where h = height of a table if key not found
findKey :: Hash->Int -> Maybe  Passwd
findKey _ 0 = Nothing
findKey pwdh h
    | Map.lookup pwdh table == Nothing = findKey (pwHash(pwReduce pwdh )) (h-1)
    | otherwise = Map.lookup pwdh table

{- the function findhash aims to compare pwdh & pwhs at each iteration and
 return corresponding password if equal oherwise  repeating comparison of pwdhs and pwHash(pwReduce pwhs) until reaching the end of table -}



findhash :: Hash -> Hash -> Int -> Map.Map Hash Passwd -> Maybe Passwd
findhash _ _ 0 _ = Nothing
findhash pwdh pwhs width table  
    | pwhs == pwdh =  Map.lookup pwdh table 
    | otherwise = findhash pwdh (pwHash(pwReduce(pwhs))) (width-1) table 


-- findPassword 

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table width pwdh = do
           pw <- findKey pwdh height 
           findhash pwdh (pwHash pw) width table


-- Experimenting
test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = mapMaybe (findPassword table width) hs
  return (result, length result)


-- Compiling
main :: IO ()
main = do
  generateTable
  res <- test2 10000
  print res

