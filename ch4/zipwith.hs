giveMeSevenHelper :: Int -> [a] -> [a]
giveMeSevenHelper i (x : xs) =
    if i `rem` 7 == 0
        then x : giveMeSevenHelper (i + 1) xs
        else giveMeSevenHelper (i + 1) xs
giveMeSevenHelper i [] = []

giveMeSeven :: [a] -> [a]
giveMeSeven = giveMeSevenHelper 0

fib = 0 : 1 : (zipWith (+) fib $ tail fib)

main :: IO()
main = do
    print $ giveMeSeven [1..29]
    print $ last $ take 10000 fib