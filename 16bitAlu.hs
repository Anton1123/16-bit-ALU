data Bit = O | I deriving (Show, Eq)

-- not
not_ :: Bit -> Bit
not_ x = if x == I then O else I

-- and 
and_ :: Bit -> Bit -> Bit
and_ a b = if a == b && b == I then I else O

-- or
or_ :: Bit -> Bit -> Bit
or_ a b = if a == b && b == O then O else I

-- xor
xor_ :: Bit -> Bit -> Bit
xor_ a b = if a == b then O else I


-- 1-bit ALU
alu1bit :: Bit -> Bit -> Bit -> Bit -> (Bit , Bit)
alu1bit a b op cin 
  | op == O = ((a `xor_` b) `xor_` cin, (a `and_` b) `or_` (cin `and_` (a `xor_` b)))
  | op == I = ((a `xor_` b') `xor_` cin, (a `and_` b') `or_` (cin `and_` (a `xor_` b')))
  where b' = not_ b

-- Main ALU function that calls alu' passing in op as the cin and reverses the list of bits because alu' is head recursive. Also checks for overflow. Final result has to be reversed as well
alu :: [Bit] -> [Bit] -> Bit -> [Bit]
alu as bs op = if (op == O && head as == head bs && head as /= head result) || (op == I && head as /= head bs && head as /= head result) then error "Overflow Error" else result
  where
    result = reverse (alu' (reverse as) (reverse bs) op op)

-- The contents of the registers are reversed at this point so that I could use head recursion (much more efficient than tail even though I have to reverse 3 times
alu' :: [Bit] -> [Bit] -> Bit -> Bit -> [Bit]
alu' [] [] op cin = []
alu' (a:as) (b:bs) op cin = let (r, c) = alu1bit a b op cin
                            in r : alu' as bs op c

-- Arithmetic shift to the right
shiftRa :: [Bit] -> [Bit]
shiftRa xs = head xs : (init xs) 

-- Main booths function which calls to booths' helper function
booths :: [Bit] -> [Bit] -> [Bit]
booths md mq = undefined

-- Helper function which will take into its parameters in order: Cycle-Counter, MD, AC, MQ, MQ-1 and return the result of multiplying MD by MQ using Booth's algorithm. Return type is a list of Bits
-- mq1 stands for the MQ(-1) position.
booths' :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> [Bit]
booths' count md ac mq mq1 | all (==O) count = ac ++ mq 
                           | otherwise = booths' count' md' ac' mq' mq1'
                           where (count', md', ac', mq', mq1') = let (count'', md'', ac'', mq'', mq1'') = boothsstep1 count md ac mq mq1
                                                                 in boothsstep2 count'' md'' ac'' mq'' mq1''

-- Result from running one iteration of Booths algothim. (Without any shift)
-- Return type is a quintuple of all registers.
-- here only AC register is modified
boothsstep1 :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> ([Bit], [Bit], [Bit], [Bit], Bit)
boothsstep1 count md ac mq mq1 = (count, md, ac', mq, mq1)
  where
    ac' | last mq == O && mq1 == I = alu ac md O
        | last mq == I && mq1 == O = alu ac md I
        | otherwise = ac
  
-- The arithmetic shift portion of Booth's algorithm
-- counter--, AC/MQ >> 1
boothsstep2 :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> ([Bit], [Bit], [Bit], [Bit], Bit)
boothsstep2 count md ac mq mq1 = (count', md, ac', mq', mq1')
  where
    count' = alu count [O,O,O,O,O,I] I 
    ac' = shiftRa ac
    mq' = last ac : (init mq)
    mq1' = last mq















