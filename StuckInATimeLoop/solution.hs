solution :: Int -> String -> String
solution x acc | x <= 1     =  show x ++" Abracadabra"++acc
               | x > 1      = (solution (x-1)) acc++"\n" ++(show x ++ " Abracadabra")

start :: Int -> String
start x = solution x ""

main :: IO ()
main = do
    a <- getLine
    (putStrLn . start . read) a