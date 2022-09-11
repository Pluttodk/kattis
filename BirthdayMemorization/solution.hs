import Data.Map ( Map, fromListWith , elems)
import Data.List (sortOn, sort)
import Data.Function (on)

data Birthday = Birthday String Int String deriving (Show)

names :: [(String, Int)] -> String
names a = fst (last (sortOn snd a))


sortAndGroup :: [Birthday] -> Map String [(String, Int)]
sortAndGroup assocs = fromListWith (++) [(date, [(name, score)]) | a@(Birthday name score date) <- assocs]


parse :: [String] -> Birthday
parse (name:score:date:tail)        = Birthday name (read score) date 
parse _                             = Birthday "" 0 ""

splits :: String -> Birthday
splits line = parse (words line)

solution :: [String] -> [Birthday]
solution (line:rest) = splits line : solution rest
solution _ = []

main :: IO ()
main = do
    n <- getLine
    nums <- mapM (const getLine) [1..(read n)]
    
    let birthdays = solution nums

    let groups = sortAndGroup birthdays

    let answer = map names (elems groups)

    print (length answer)
    mapM_ putStrLn (sort answer)