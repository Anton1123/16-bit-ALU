# 16-bit-ALU
This simulation written in Haskell with any size ALU.
Cycle-Counter and AC registers are created with respect to the size of MD/MQ registers (inputs)

Test cases for the ALU are provided from a text file. 
Each case is on a seperate line with MD and MQ seperated by a semicolon.
Each MD and MQ consists of characters `O` and `I` (representing 0's and 1's) seperated by a space.
Example of a input text file with 2 cases (shortened to a 8-bit ALU):
O O I I O I O I; O I O I I I I I
I I I O I I O O; O I O O O I I I

To run the program, must have ghci (Haskell Platform) installed.
Compile the program by running $> ghc 16bitAlu.hs
Run the program with the command $> ./16BitAlu text.txt
