-- Part 1a)
{-
Functional programming is a declarative programming paradigm in which computation consists solely of applying functions to arguments in a compiler. 
Functional programmes, such as Haskell, are more concise than other languages, despite their high-level nature; they provide the clearest possible perspective of the core ideas in modern computing.
Consider the following python programme: 'take the sum of the first n numbers in a list.' This requires the input function, variable declaration, arithmetic operators, while loops, and the print function. 
Functional programming, on the other hand, is typical '2-10 times' shorter than other languages when just performed in Haskell; the sum and the (..) function is employed (sum [1..5]), making it more concise 
and expressive for the programmer. Because an iterator variable is not necessary to reside at the centre of a loop, it is condensed and removed from your code. 
Pure functions are a subset of functional programming, and simple equational reasoning can be used to execute programmes, prove property proofs, and even obtain programmes directly from behaviour specifications. 
As a result, functions and arguments written in functional programming are simpler to debug. All we must do is examine the arguments and the function inputted, and then walk through the function recursively until we locate the error. 
Debugging is simplified because it simply requires checking the result returned by the functions, which have no apparent side effects and always return the same value when given the same inputs.
Functional programming especially Haskell uses lazy evaluation to execute programmes that are based on the principle that no computation should be performed until its result is required. 
There is a thorough evaluation that goes into the value storage. This saves the need for the functional program's inputs to be evaluated again and again. In Haskell, this process ensures that the user can go back to the data structure 
if it’s not caught in dependencies. Lazy evaluation, in addition to eliminating unnecessary processing, assures that programmes finish whenever possible, supports modular programming using intermediate data structures, 
and even allows data structures with an unlimited number of members, such as an infinite list of numbers.
Lastly, when compared to other methods that do not require functional programmes, functional programming makes type signatures far more valid and meaningful. 
The pure variables of functional programming languages ensure that the signature contains all the information about how the function operates, including the types such as Int, String, Char, Bool, Integers, and other information. 
It is advantageous for programmers, particularly newcomers, to match their functions to the right type signatures.	
-}

-- Part 1b)
{-
A Mathematical function executes one or two arguments/ input also known as the domain and finds a single result/output also known as the range. 
Similarly, to Haskell, it is usually in the form of an equation that gives a name for the arguments, functions and how the results are going to be executed in the body of the function. For example: 
double :: Int -> Int
double n = 2*n
It takes the name of a function, double, that accepts input, n, and output of type Int and executes the equation 2*n and outputs a value of type Int.
-}

-- Part 1c)
{-
c)	A higher order function is a function that can be inputs and outputs of other functions like other types. For example, string, char, int etc. 
They tend to have other functions as arguments or results. An example of higher-order function is: 
add3 :: (Int -> Int) -> Int
add3 x = x 3
Here we take function as an argument, function of type Int and we return type Int.  
x is the function with type int to int and the result will be application of function x on 3 of type Int. 
Further example – Main > add3(add7) => 10. The result is 10. We are applying the function add3 on function add7 (Int -> Int), resulting in the int value 10. 
In terms x and y, x + y, we apply the function add7 on y and it returns x + y. 7 + 3 = 10


Another example: 
twice :: (a -> a) -> a -> a 
twice x y = x (x y)
twice is a higher-order because it takes a function (a -> a) as its function argument
-}

-- Part 2a)

-- Defining a type Horse as a tuple made up of a String and an Integer 
-- where string is the name of the horse and Integer is the height of the horse
type Horse = (String, Int)

-- Part 2b)

-- Function create_horse_list that takes two lists to creates a Horse.
create_horse_list :: [String] -> [Int] -> [Horse]
create_horse_list xs []	= []
create_horse_list [] ys	= []
create_horse_list (x:xs) (y:ys) = (x, y) : zip xs ys

-- Part 2c)

-- Function sort_horse_list that sorts a list of horses by their name in ascending order.
sort_horse_listfs :: [Horse] -> [Horse]

-- An empty list when sorted is always an empty list
sort_horse_listfs [] = []
sort_horse_listfs (x:xs) = 
               -- Sort the elements that are less than or equal to the pivot element
               sort_horse_listfs [y | y <- xs, y <= x ]
               -- Use the first element as the pivot and put it in the middle
               ++ [x]
               -- Sort the elements that are greater than the pivot element
               ++ sort_horse_listfs [y | y <- xs, y > x]

-- Function sort_horse_list that sorts a list of horses by their height in ascending order.
sort_horse_list :: [Horse] -> [Horse]

-- Part 2d)

-- An empty list when sorted is always an empty list
sort_horse_list [] = []
sort_horse_list (x:xs) = 
               -- Sort the elements that are less than or equal to the pivot element
               sort_horse_list [y | y <- xs,  snd y <= snd x ]
               -- Use the first element as the pivot and put it in the middle
               ++ [x]
               -- Sort the elements that are greater than the pivot element
               ++ sort_horse_list [y | y <- xs, snd y > snd x]

-- Function remove_smallest_horse to remove the k smallest horses from the list by number (k) specified
remove_smallest_horses :: Int -> [Horse] -> [Horse]
remove_smallest_horses k xs
       | ((k <= 0) || null xs) = xs
       | otherwise = remove_smallest_horses (k-1) (tail xs)

-- Part 2e)
-- filter function
remove_tall_horses :: [Horse] -> [Horse]
remove_tall_horses xs = filter (\(x,y) -> y < 152) xs

-- Part 3a)

-- this function gets the individual 'increasing' lines to be copied
increasinglines :: Int -> Int -> [[Char]]
increasinglines x 0 = []
increasinglines x y = [(replicate (x+(x*m)) '*') | m<-[0..y-1] ]

-- this copies the list element x times within the list right next to the orginal
repeatforlists :: Int -> [[Char]] -> [[Char]]
repeatforlists x xs = concatMap (replicate x) xs

-- String conversion and using '\n' to transfer to a new line
increasing_decreasing :: [String] -> String
increasing_decreasing [] = ""
increasing_decreasing (x:xs) = x  ++ "\n" ++ (increasing_decreasing xs)

-- reversing the line and putting it into a list to be processed
reverselines :: Int -> Int -> [[Char]]
reverselines y 0 = []
reverselines y z = [(replicate (y*m) '*') | m<-[z,z-1..1] ]

-- merge both increasing and decreasing
merge :: [String] -> [String] -> String
merge xs ys = (increasing_decreasing xs) ++ (increasing_decreasing ys)

-- this function should get the 'increasing' and 'decreasing' strings and put them into one line for outputting
steps :: Int -> Int -> Int -> String
steps x y z =  merge(repeatforlists x (increasinglines y z)) (repeatforlists x (reverselines y z))


-- Part 3b)
-- the flag function would call the 'increasing_decreasing' function from 3a

-- this function should choose the the insides of the flag to output! 
insides :: Int -> Int -> Int -> String
insides y z m
  |z < 0 = (replicate m ' ') ++ "*" ++ (replicate m ' ')
  |otherwise = (replicate m ' ') ++ "*" ++ (replicate y ' ') ++ "*" ++ (replicate m ' ')


-- this function should choose the the insides of the flag to output when an even detereminer 'z' is input
insides_even :: Int -> Int -> Int -> String
insides_even  y z m
  |z <=0 = (replicate m ' ') ++ "*" ++ (replicate m ' ')
  |otherwise = (replicate m ' ') ++ "*" ++ (replicate y ' ') ++ "*" ++ (replicate m ' ')

-- this function gets the top half "*"s of the flag's insides
insides_tophalf :: Int -> Int -> Int -> [String]
insides_tophalf y z m 
  | y<0 = []
  | z `mod` 2 == 0 = [(insides_even y z m)] ++ insides_tophalf (y-2) (z-2) (m+1)
  | otherwise =  [(insides y z m)] ++ insides_tophalf (y-2) (z-2) (m+1)

-- this function adds a '*' on each side of the flag's insides
addedges :: [String] -> [String]
addedges [] = []
addedges (x:xs) = [("*" ++ x ++ "*")] ++ addedges xs

-- this function adds the end line/border of '*'s to the head of a calculated list of string
add_border :: Int -> [String]
add_border n = [(replicate n '*')] ++ (addedges(insides_tophalf (n-4) (n-4) 0))

-- gets the entire flag insides: adds a border line to the decreasing insides, add their reverse and makes the entire 'flag' into a string!
getfullinside :: Int -> String
getfullinside n 
  | 0 == n `mod` 2 =  increasing_decreasing ((add_border (1+n)) ++ reverse (add_border (1+n)))
  | otherwise = increasing_decreasing ((add_border n) ++ ["*" ++ (replicate ((n `div` 2)-1) ' ') ++ "*" ++ ((replicate ((n `div` 2)-1)  ' ') )++ "*"] ++ reverse (add_border n)) 

-- flagpattern repeated m times 
repeatntimes :: String -> Int -> String
repeatntimes xs 0 = []
repeatntimes xs m = xs ++ (repeatntimes xs (m-1)) 

-- merge into a flagpattern
flagpattern :: Int -> Int -> String
flagpattern m n = (repeatntimes (getfullinside m) n)

-- Part 4)
--should take 2 strings and remove any matching characters between them
matchingchar :: String -> String -> String
matchingchar x y = [ m | m <- x, m `notElem` y]

-- this function should check the length on the remaining unmatching letters and determine relationships
definelahi :: String -> String
definelahi x 
  | length uniqueletters `mod` 4 == 1 = "like"
  | length uniqueletters `mod` 4 == 2 = "admire" 
  | length uniqueletters `mod` 4 == 3 = "hate"
  | otherwise = "is indifferent to"
  where
    uniqueletters = [c | c <- x, c /= ' ']


compatibility :: String -> String -> String
compatibility m n = m ++ " " ++ (definelahi (matchingchar m n)) ++ " "  ++ n ++ " " ++ "and" ++ " " ++ n ++ " " ++(definelahi (matchingchar n m)) ++ " " ++ m

--Part 5a)
-- gets all the indexes of the matching values within the list
getindex :: (Eq a) => [a] -> a -> [Int]
getindex xs x = [m| m <- [1..((length xs)-1)], xs !! m == x]


-- distance between the matching elements 
get_matching_counts :: [Int] -> Int -> [Int]
get_matching_counts positions m  | 0 == m = []
                                 | [] == positions = [m]
                                 | otherwise = (head positions): (get_matching_counts (map (\x -> x-1 - (head positions)) (tail positions)) (m - (head positions) - 1 ))



split :: (Eq a) => [a] -> a -> [Int]
split xs m = filter (/=0) (get_matching_counts (getindex xs m) ( length xs))

--Part 5b) 

swapwords :: String -> String -> String -> String
swapwords w1 w2 s
 | top == [] = []
 | top == w1 = w2 ++ []
 | otherwise = swapwords w1 w2 []
 where
 top = take (length w1) s
 rest = drop (length w1) s
