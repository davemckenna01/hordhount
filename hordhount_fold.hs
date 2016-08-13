import System.Environment

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > (12, 3, 0, 'x') 12 characters, 3 words, 0 lines, last char = 'x'

data Report = Report {
    characters :: Int,
    words      :: Int,
    lines      :: Int
} deriving Show

-- this could be called "categorize"... it advances the state of the counters
-- based on how it categorizes a character
addToReport :: (Report, Char) -> Char -> (Report, Char)
addToReport (Report c w ln, last) x 
    | x == ' '  && last /= ' '                 = (Report (c + 1) (w + 1)  ln,      x)
    | x == '\n' && last /= ' ' && last /= '\n' = (Report  c      (w + 1) (ln + 1), x)
    | x == '\n'                                = (Report  c       w      (ln + 1), x)
    | otherwise                                = (Report (c + 1)  w       ln,      x)

count :: [Char] -> (Report, Char)
count = foldl addToReport (Report 0 0 1, 'x') -- init char could be anything

-- it's very interesting that reduce + state monad are similar for this
-- problem!
foldStyle :: IO ()
foldStyle = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    print $ count file

main = foldStyle

