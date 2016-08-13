import System.Environment
import Report

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

count :: [Char] -> (Report, Char)
count = foldl addToReport (Report 0 0 1, 'x') -- init char could be anything

-- it's very interesting that reduce + state monad are similar for this
-- problem!
foldStyle :: IO ()
foldStyle = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    print $ fst $ count file

main = foldStyle

