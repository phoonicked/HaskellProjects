import Data.List(nub, isPrefixOf)

type Permutation = [(Int, Int)]

inputToPermutation :: String -> Permutation
inputToPermutation input = concatMap parseCycle (extractCycles input)
    where
        parseCycle :: String -> Permutation
        parseCycle cycle =
            let elements = map read (words cycle)
            in zip elements (tail elements ++ [head elements])
    
        extractCycles :: String -> [String]
        extractCycles str = checkDisjoint str []
            where
                checkDisjoint [] current = [reverse current | not (null current)]
                checkDisjoint (x:xs) current
                    | x == '(' = checkDisjoint xs []
                    | x == ')' = reverse current : checkDisjoint xs [] 
                    | otherwise = checkDisjoint xs (x : current)
        
        splitCycles :: [String] -> [[String]]
        splitCycles [] = []
        splitCycles xs = case break (== "") xs of
            (cycle, rest) -> cycle : splitCycles (drop 1 rest)

inversePermutation :: Permutation -> Permutation
inversePermutation perm = concatMap toPairs reversedCycles
  where
    cycles = groupCycles perm 
    reversedCycles = map reverse cycles

    toPairs :: [Int] -> Permutation
    toPairs [] = []
    toPairs cycle = zip cycle (tail cycle ++ [head cycle])

compositePermutation :: Permutation -> Permutation -> Permutation
compositePermutation f g = [(x, findMapping (findMapping x g) f) | x <- domain]
  where
    domain = nub [a | (a, _) <- f ++ g]
    findMapping :: Int -> Permutation -> Int
    findMapping x [] = x
    findMapping x ((a, b):ys)
      | x == a = b 
      | otherwise = findMapping x ys

powerPermutation :: Permutation -> Int -> Permutation
powerPermutation f 0 = identityPermutation f
powerPermutation f n = compositePermutation (powerPermutation f (n-1)) f

identityPermutation :: Permutation -> Permutation
identityPermutation perm =
    let elements = nub [x | (x, _) <- perm] ++ [y | (_, y) <- perm]
    in [(x, x) | x <- elements]

traceCycle :: Int -> Permutation -> [Int]
traceCycle start perm = traceHelper start []
  where
    traceHelper current visited
      | current `elem` visited = visited
      | otherwise = traceHelper next (visited ++ [current])
      where
        next = findMapping current perm

findMapping :: Int -> Permutation -> Int
findMapping x [] = x
findMapping x ((a, b):xs)
  | x == a = b
  | otherwise = findMapping x xs

groupCycles :: Permutation -> [[Int]]
groupCycles [] = []
groupCycles perm = groupCyclesHelper perm []
  where
    groupCyclesHelper :: Permutation -> [Int] -> [[Int]]
    groupCyclesHelper [] _ = []
    groupCyclesHelper perm processed =
        let unprocessed = [x | (x, _) <- perm, x `notElem` processed]
        in case unprocessed of
            [] -> []
            (x:_) ->
                let nextCycle = traceCycle x perm
                    remainingPerm = filter (\(a, _) -> a `notElem` nextCycle) perm
                in nextCycle : groupCyclesHelper remainingPerm (processed ++ nextCycle)

formatCycle :: [Int] -> String
formatCycle [] = ""
formatCycle cycle = "(" ++ unwords (map show cycle) ++ ")"

cycleNotation :: Permutation -> String
cycleNotation perm = concatMap formatCycle (groupCycles perm)

main :: IO ()
main = do
    putStrLn "Enter the first permutation (f) in cycle notation:"
    fInput <- getLine
    let f = inputToPermutation fInput

    putStrLn "Enter the second permutation (g) in cycle notation:"
    gInput <- getLine
    let g = if null gInput then [] else inputToPermutation gInput

    putStrLn "Enter the operation (e.g., 'composite', 'inverse', 'f^2'):"
    operation <- getLine

    let result = case operation of
            "composite" -> compositePermutation f g
            "inverse" -> inversePermutation f
            op | "f^" `isPrefixOf` operation ->
                   let n = read (drop 2 operation) :: Int
                   in powerPermutation f n
            _ -> error "Invalid operation"

    putStrLn $ "Result: " ++ cycleNotation result