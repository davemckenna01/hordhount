import System.Environment

-- A simple word count "wc" clone

-- count 'dave is cool'
-- > [12, 3, 0] 12 characters, 3 words, 0 lines

categorize :: (Int, Int, Int, Char) -> Char -> (Int, Int, Int, Char)
categorize (c, w, ln, last) x 
    | x == ' '  && last /= ' '                 = (c + 1, w + 1, ln,     x)
    | x == '\n' && last /= ' ' && last /= '\n' = (c,     w + 1, ln + 1, x)
    | x == '\n'                                = (c,     w,     ln + 1, x)
    | otherwise                                = (c + 1, w,     ln,     x)

count :: [Char] -> (Int, Int, Int, Char)
count = foldl categorize (0,0,1,'x') -- init char could be anything

main = do
    args <- getArgs
    let filename = head args
    file <- readFile filename
    print $ count file



