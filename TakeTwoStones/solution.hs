solution :: Int -> String
solution x = if mod x 2 == 1
    then "Alice"
    else "Bob"

run :: String -> String 
run x = solution (read x)

main :: IO ()
main = do
    a <- getLine
    putStrLn (run a)
    