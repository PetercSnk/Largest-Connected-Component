-- ghci version 9.2.5 was used

-- idea is to split matrix into list of nodes and a list of its values then run functions on both 
-- nlcc is wrapper function, extracts needed variables and runs required functions to return the largest connected component
nlcc :: [[Int]] -> Int -> Int
nlcc l v = maxLength where
    rLen = length (head l)
    cLen = length l
    nodes = matrixToNodes l
    vs = concat l
    paths = allConnectedComponents rLen cLen vs v nodes []
    pathLengths = map length paths
    maxLength = findMax pathLengths 0

-- finds the largest positive number in a list
-- input n should be set to 0 initially
-- [1,3,4,9,1] -> 0 -> 9
findMax :: [Int] -> Int -> Int
findMax [] n = n
findMax (x:xs) n
    | x < n = findMax xs n
    | otherwise = findMax xs x

-- removes elements from list 1 that appear list 2
-- [1,2,3] -> [4,3,1] -> [2]
removeFromList :: [Int] -> [Int] -> [Int]
removeFromList [] _ = []
removeFromList (x:xs) ys 
    | containsValue x ys = removeFromList xs ys
    | otherwise = x : removeFromList xs ys

-- returns true or false depending on if a number exists in a list
-- 2 -> [1,2] -> True
containsValue :: Int -> [Int] -> Bool
containsValue _ [] = False
containsValue n (x:xs)
    | n == x = True
    | otherwise = containsValue n xs

-- checks whether or not a value in a list at a certain index is equal to another value, returns the [index] or nothing
-- 1 -> [1,0,0,0,1] -> 4 -> [4]
isValid :: Int -> [Int] -> Int -> [Int]
isValid v vs i
    | (vs !! i) == v = [i]
    | otherwise = []

-- checks whether or not a value in a list at a certain index is equal to another value, returns true or false
-- 1 -> [1,0,0,0,1] -> 4 -> True
isValidBool :: Int -> [Int] -> Int -> Bool
isValidBool v vs i
    | (vs !! i) == v = True
    | otherwise = False

-- flattens a matrix and creates a list of nodes based on its length
-- [[1,0,1],[0,0,0]] -> [0,1,2,3,4,5]
matrixToNodes :: [[Int]] -> [Int]
matrixToNodes m = is where
    is = [0..(length (concat m) - 1)]

-- returns the nodes whos values are the same value as v
-- [0,1,2,3,4] -> [0,1,0,1,1] -> 1 -> [1,3,4]
matchElements :: [Int] -> [Int] -> Int -> [Int]
matchElements ns vs v = matchedElements where
    matchedElements = concatMap (isValid v vs) ns 

-- returns a list of all connected components for each node
-- this method ignores finding connected components that contain a node that already exists ...
-- in another connected component, this function is almost the same as findConnectedComponent
allConnectedComponents :: Int -> Int -> [Int] -> Int -> [Int] -> [Int] -> [[Int]]
allConnectedComponents _ _ _ _ [] _ = []
allConnectedComponents rLen cLen vs v (n:ns) visited
    | containsValue n visited = allConnectedComponents rLen cLen vs v ns visited
    | otherwise = connectedComponent : allConnectedComponents rLen cLen vs v ns (visited ++ connectedComponent)
    where 
        connectedComponent = findConnectedComponent rLen cLen vs v [n] []

-- returns connected component for a single node, accepts a list as ...
-- the valid neighbours are added to its tail before recursing
findConnectedComponent :: Int -> Int -> [Int] -> Int -> [Int] -> [Int] -> [Int]
findConnectedComponent _ _ _ _ [] _ = []
findConnectedComponent rLen cLen vs v (n:ns) visited
    | containsValue n visited = findConnectedComponent rLen cLen vs v ns visited
    | otherwise = n : findConnectedComponent rLen cLen vs v validNeighbours (n : visited)
    where 
        validNeighbours = removeFromList (findValidNeighbours rLen cLen vs v n ++ ns) visited

-- returns the neighbours of a node where the node and its neigbours are all of value v
findValidNeighbours :: Int -> Int -> [Int] -> Int -> Int -> [Int]
findValidNeighbours rLen cLen vs v n
    | isValidBool v vs n = validNeighbours
    | otherwise = []
    where
        neighbours = findNeighbours rLen cLen n
        validNeighbours = concatMap (isValid v vs) neighbours

-- combines all 4 neighbour functions
findNeighbours :: Int -> Int -> Int -> [Int]
findNeighbours rLen cLen n = neighbours where
    nNeighbour = findNorthNeighbour rLen cLen n
    eNeighbour = findEastNeighbour rLen cLen n
    sNeighbour = findSouthNeighbour rLen cLen n
    wNeighbour = findWestNeighbour rLen cLen n
    neighbours = nNeighbour ++ eNeighbour ++ sNeighbour ++ wNeighbour

-- this is how we picture where each node is:
-- [0,      1,      2,      3,
--  4,      5,      6,      7,
--  8,      9,      10,     11,
--  12,     13,     14,     15]

-- returns the above / north neighbour for a node that is not in the top row
findNorthNeighbour :: Int -> Int -> Int -> [Int]
findNorthNeighbour rLen cLen n
    | nNeighbour >= 0 && n < (rLen * cLen) = [nNeighbour]
    | otherwise = []
    where nNeighbour = n - rLen

-- returns the right / east neighbour for a node that is not in the right most column
findEastNeighbour :: Int -> Int -> Int -> [Int]
findEastNeighbour rLen cLen n
    | eNeighbour `mod` rLen /= 0 && n >= 0 && eNeighbour < (rLen * cLen) = [eNeighbour]
    | otherwise = []
    where eNeighbour = n + 1

-- returns the below / south neighbour for a node that is not in the bottom row
findSouthNeighbour :: Int -> Int -> Int -> [Int]
findSouthNeighbour rLen cLen n
    | sNeighbour < (rLen * cLen) && n >= 0 = [sNeighbour]
    | otherwise = []
    where sNeighbour = n + rLen

-- returns the left / west neighbour for a node that is not in the left most column
findWestNeighbour :: Int -> Int -> Int -> [Int]
findWestNeighbour rLen cLen n
    | n `mod` rLen /= 0 && n > 0 && n < (rLen * cLen) = [wNeighbour]
    | otherwise = []
    where wNeighbour = n - 1