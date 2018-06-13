{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.CreateCrossword where
import Data.Text (Text)
import Import

import Control.Monad.Trans.List
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M
import Control.Concurrent (forkIO)


data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]


data Pairs = Pairs{
    questions :: [Text],
    answers :: [Text]
}        
instance FromJSON Pairs where 
    parseJSON (Object o) = Pairs
        <$> o .: "questions"
        <*> o .: "answers"

instance ToJSON Pairs where 
    toJSON Pairs {..} = object
        [ "questions" .= questions
        , "answers" .= answers
        ]

data PosPairs = PosPairs{
    posQuestions :: [Text],
    posAnswers :: [Text],
    positions :: [[Int]]
}

answersExample :: [Text]
answersExample =  ["00020000000000000","002tomek100000000", "000r0000000000000", "000a0000000000000", "000k0000000000000", "000t0000000000000", "000o0000000000000", "2paranienormalni1","00010000000000000"]

--indeks i odpowiada pozycji i-tego question
returnExample :: [Text] -> PosPairs
returnExample questions = PosPairs questions answersExample [[1,2], [0,3],[7,0]]

instance ToJSON PosPairs where
    toJSON PosPairs {..} = object
        [ "questions" .= posQuestions
        , "answers" .= posAnswers
        , "positions" .= positions
        ]

instance ToJSON Crossword where 
    toJSON Crossword {..} = object
        [ "set" .= set 
        , "minx" .= minx
        , "maxx" .= maxx
        , "miny" .= miny
        , "maxy" .= maxy
        ]

postCreateCrosswordR :: Handler Value
postCreateCrosswordR = do
    pairs <- (requireJsonBody :: Handler Pairs)
    --returnJson $ returnExample $ questions pairs
    case solveRun basicState $ Prelude.map unpack $ answers pairs of
        Just solution -> returnJson $ solution
        --Nothing = returnJson ["error":"impossible to solve"]

data Crossword = Crossword {
    set :: M.Map (Int,Int) Char,
    minx :: Int,
    maxx :: Int,
    miny :: Int,
    maxy :: Int
} deriving (Show)

basicState :: Crossword
basicState = Crossword M.empty 0 0 0 0

examples :: [String]
examples = ["tomek","traktor","paranienormalni"]

firstLoop :: String -> Int -> Crossword -> Crossword
firstLoop [] num (Crossword m minx maxx miny maxy) = (Crossword (M.insert (num,0) '1' m) minx maxx miny maxy)
firstLoop (e:es) num (Crossword m minx maxx miny maxy) = firstLoop es (num + 1) (Crossword (M.insert (num,0) e m) minx (maxx+1) miny maxy)

firstInsert :: String -> Crossword 
firstInsert element = firstLoop element 0 basicState

splits :: [a] -> [([a], a, [a])]
splits (x:xs) = splits' ([], x, xs)
    where
    splits' p@(_,_,[]) = [p]
    splits' p@(pre,curr,(y:ys)) = p:(splits' ((Prelude.++) pre (curr:[]),y,ys))
splits [] = []

findPlaces :: (String, Char, String) -> Crossword -> [(Int, Int)]
findPlaces (prev, t, tail) (Crossword m minx maxx miny maxy) = M.foldlWithKey (\b k val -> if val /= '1' && val == t then k:b else b) [] m

checkNeighbourhood :: [(Int, Int)] -> M.Map (Int, Int) Char -> Bool
checkNeighbourhood els m = foldl (\b (x', y') -> b && (M.lookup (x',y') m == Nothing || M.lookup (x',y') m == (Just '1'))) True els
--checkNeighbourhood _ _ = True

tryHorizontal :: (Int,Int) -> String -> Int -> Crossword -> [Crossword]
tryHorizontal (x,y) [] _ (Crossword m minx maxx miny maxy) =  if M.lookup (x,y) m == Nothing || M.lookup (x,y) m == (Just '1') then [Crossword (M.insert (x,y) '1' m) minx maxx miny maxy] else []
tryHorizontal start@(x, y) (w:ws) pos c@(Crossword m minx maxx miny maxy) = 
    --nalezaloby tez sprawdzac sasiadow :o
    if (M.lookup (x,y) m == Nothing && (checkNeighbourhood [(x-1,y),(x+1,y)] m)) || (M.lookup (x,y) m == (Just w))
        then tryHorizontal (x,y+1) ws (pos+1) (Crossword (M.insert start w m) minx maxx miny maxy) 
        else []

tryVertical :: (Int,Int) -> String -> Int -> Crossword -> [Crossword]
tryVertical (x,y) [] _ (Crossword m minx maxx miny maxy) = if M.lookup (x,y) m == Nothing || M.lookup (x,y) m == (Just '1') then [Crossword (M.insert (x,y) '1' m) minx maxx miny maxy] else []
tryVertical start@(x, y) (w:ws) pos c@(Crossword m minx maxx miny maxy) = 
    if (M.lookup (x,y) m == Nothing && (checkNeighbourhood [(x,y-1),(x,y+1)] m))  || (M.lookup (x,y) m == (Just w))  
        then tryVertical (x+1,y) ws (pos+1) (Crossword (M.insert start w m) minx maxx miny maxy) 
        else []


tryPutWord :: (Int,Int) -> (String, Char, String) -> Crossword -> [Crossword]
tryPutWord cor@(x, y) (prev, t, tail) c@(Crossword m minx maxx miny maxy) 
    | M.lookup (x-1, y) m == Nothing && M.lookup (x+1, y) m == Nothing = tryVertical (x-dl,y) ((Prelude.++) prev (t:tail)) 0 (Crossword m (min minx (x-dl)) (max maxx (x+dlt)) miny maxy)  
    | M.lookup (x, y-1) m == Nothing && M.lookup (x, y+1) m == Nothing = tryHorizontal (x,y-dl) ((Prelude.++) prev (t:tail)) 0 (Crossword m minx maxx (min miny (y-dl)) (max maxy (y+dlt)))
    | otherwise = []
    where dl = Prelude.length prev
          dlt = Prelude.length tail  

nextSteps :: String -> Crossword -> [Crossword]
nextSteps element crossword 
    | Prelude.null $ set crossword = return $ firstInsert element
--dla kazdej literki inny watek :)
    | otherwise = do
        el <- splits element
        --tu zrob forka
        newPlace <- findPlaces el crossword
        tryPutWord newPlace el crossword

solveCrossword :: [String] -> [String] -> Crossword -> ListT (State (Maybe Crossword)) ()
solveCrossword [] [] problem = do
    currentBest <- lift Control.Monad.Trans.State.Strict.get
    if maybe False (\(Crossword map minx' maxx' miny' maxy') -> (maxx'-minx')*(maxy'-miny') <= ((maxx problem - (minx problem))*(maxy problem-(miny problem)))) currentBest
        then 
        return ()
        else
        lift $ put $ Just problem
solveCrossword [] x problem = return ()
solveCrossword (x:xs) y problem = do
    ListT (return $ (nextSteps x problem)) >>= solveCrossword xs y
    --if filter == [] then x:y and try with (head xs) (we have to change state)
    --if filter == [] && (x:xs) == [] then return () - kupa, nie ma currentBest

solveRun :: Crossword -> [String] -> Maybe Crossword
solveRun problem elements = execState
    (runListT $ solveCrossword elements [] problem) Nothing

-- moveToZero :: Crossword -> Crossword
-- moveToZero (Crossword m minx maxx miny maxy) = Crossword (foldl f (M.empty :: M.Map (Int,Int) String) (M.keys m)) 0 0 0 0
--     where f = (\b (kx, ky) -> M.insert (kx-minx,ky-miny) (maybe '0' id (M.lookup (kx, ky) m)))

