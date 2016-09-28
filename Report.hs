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


reportMap f (Report a b c) = Report (f a) (f b) (f c)

-- --------------------------------------------------------

data Report2 a = Report2 a a a deriving (Show)

-- What's the purpose of type classes again?
instance Functor Report2 where
    fmap f (Report2 a b c) = Report2 (f a) (f b) (f c)

