import System.Environment (getArgs)

-- Created the data type that I will be using for this simulation.
data Bit = O | I deriving (Show, Eq)

-- not gate
not_ :: Bit -> Bit
not_ x = if x == I then O else I

-- and gate
and_ :: Bit -> Bit -> Bit
and_ a b = if a == b && b == I then I else O

-- or gate
or_ :: Bit -> Bit -> Bit
or_ a b = if a == b && b == O then O else I

-- xor gate
xor_ :: Bit -> Bit -> Bit
xor_ a b = if a == b then O else I

-- 1-bit ALU
alu1bit :: Bit -> Bit -> Bit -> Bit -> (Bit , Bit)
alu1bit a b op cin 
  | op == O = ((a `xor_` b) `xor_` cin, (a `and_` b) `or_` (cin `and_` (a `xor_` b)))
  | op == I = ((a `xor_` b') `xor_` cin, (a `and_` b') `or_` (cin `and_` (a `xor_` b')))
  where b' = not_ b

-- Main ALU function that calls alu' passing in op as the cin and reverses the list of bits because alu' is head recursive. 
-- Also checks for overflow. Final result has to be reversed as well.
-- Also checks that the inputs are the same size before performing ALU operation on them.
alu :: [Bit] -> [Bit] -> Bit -> [Bit]
alu as bs op = 
  if (length as) /= (length bs)
     then error "Inputs not the same size"
  else if (op == O && head as == head bs && head as /= head result) 
     || (op == I && head as /= head bs && head as /= head result) 
     then error "Overflow Error" 
     else result
  where
    result = reverse (alu' (reverse as) (reverse bs) op op)

-- ALU version that doesn't check for overflow. Used in subtracting 1 from count.
-- It is needed because count is non-signed.
alu_ :: [Bit] -> [Bit] -> Bit -> [Bit]
alu_ as bs op = reverse (alu' (reverse as) (reverse bs) op op)

-- The contents of the registers are reversed at this point so that I could use head recursion. 
-- (much more efficient than tail even though I have to reverse 3 times.
-- Actual ALU calculations are dont in this function.
alu' :: [Bit] -> [Bit] -> Bit -> Bit -> [Bit]
alu' [] [] op cin = []
alu' (a:as) (b:bs) op cin = let (r, c) = alu1bit a b op cin
                            in r : alu' as bs op c

-- Arithmetic shift to the right.
shiftRa :: [Bit] -> [Bit]
shiftRa xs = head xs : init xs

-- inverse Log function to determine size of Cycle-Counter register from. ilog 16 -> 4, ilog 32 -> 5
ilog :: Int -> Int
ilog 0 = -1
ilog m = 1 + ilog (div m 2)

-- Main booths function which calls to booths' helper function.
booths :: [Bit] -> [Bit] -> IO [Bit]
booths md mq = do
  putStrLn "Cycle-counter                MD                                 AC                                 MQ                  MQ(-1)"
  putStrLn "----------------------------------------------------------------------------------------------------------------------------"
  (count, md', ac, mq', mq1) <- booths' (I:(replicate (ilog (length md)) O)) md (replicate (length md) O) mq O
  putStr "\n"
  putStr "Final Result:   "
  putStrLn ((show (ac ++ mq')) ++ "\n")
  return (ac ++ mq')

-- MD and MQ will be in a list taken from a text file. This function will apply Booths to the correct arguments inside that list.
applyBooths2List :: [[Bit]] -> IO [Bit]
applyBooths2List as = booths (as!!0) (as!!1)

-- Helper function which will take into its parameters in order: Cycle-Counter, MD, AC, MQ, MQ-1.
-- Returns the result of multiplying MD by MQ using Booth's algorithm. Return type is a list of Bits.
-- mq1 stands for the MQ(-1) position.
booths' :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> IO ([Bit], [Bit], [Bit], [Bit], Bit)
booths' count md ac mq mq1 = do
  putStrLn (concat [show count, "   ", show md, "  ", show ac, "  ", show mq, "  ", show mq1])
  case count of
    [O,O,O,O,O] -> return (count, md, ac, mq, mq1)
    _ -> do
      let (count'', md'', ac'', mq'', mq1'') = step1 count md ac mq mq1
          (count', md', ac', mq', mq1') = step2 count'' md'' ac'' mq'' mq1''
      putStrLn (concat [show count, "   ", show md, "  ", show ac, "  ", show mq, "  ", show mq1])
      booths' count' md' ac' mq' mq1'

-- Result from running one iteration of Booths algothim. No shifting.
-- Return type is a quintuple of all registers.
-- Gere only AC register is modified (with +MD, -MD, or nothing).
step1 :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> ([Bit], [Bit], [Bit], [Bit], Bit)
step1 count md ac mq mq1 = (count, md, ac', mq, mq1)
  where
    ac' | last mq == O && mq1 == I = alu ac md O
        | last mq == I && mq1 == O = alu ac md I
        | otherwise = ac
  
-- The arithmetic shift portion of Booth's algorithm
-- counter--, AC/MQ >> 1
step2 :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> ([Bit], [Bit], [Bit], [Bit], Bit)
step2 count md ac mq mq1 = (count', md, ac', mq', mq1')
  where
    count' = alu_ count [O,O,O,O,I] I 
    ac' = shiftRa ac
    mq' = last ac : init mq
    mq1' = last mq

-- Helper function for dealing with an input from file.
-- Seperates a string by a given delimiter and places contents in a list.
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Converts the input (of type String) into a Maybe Bit type. This deals with invalid inputs by declaring them `Nothing`.
fromString2MaybeBit :: String -> Maybe Bit
fromString2MaybeBit s | s == "O" = Just O
                      | s == "I" = Just I
                      | otherwise = Nothing

-- Converts from the Maybe Bit type to a Bit if its a valid type. Otherwise throws an error. 
fromMaybeBit2Bit :: Maybe Bit -> Bit
fromMaybeBit2Bit (Just a) = a
fromMaybeBit2Bit Nothing = error "Invalid Input"

-- Main function. Seperates the input into list of lists of cases. Each case is a list of 2 lists each containing the inputs.
-- Each MQ and MD itself is a list.
main = do
  [inpFile] <- getArgs
  input <- readFile inpFile
  let cases = (map . map . map) (fromMaybeBit2Bit . fromString2MaybeBit) $ (map . map) words $ map (wordsWhen (==';')) $ lines input
  mapM_ applyBooths2List cases
