module Report where

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
