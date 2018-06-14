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
import Control.Parallel

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

-- data PosPairs = PosPairs{
--     posQuestions :: [Text],
--     posAnswers :: [Text],
--     positions :: [[Int]]
-- }

answersExample :: [Text]
answersExample =  ["00020000000000000","002tomek100000000", "000r0000000000000", "000a0000000000000", "000k0000000000000", "000t0000000000000", "000o0000000000000", "2paranienormalni1","00010000000000000"]

--indeks i odpowiada pozycji i-tego question
-- returnExample :: [Text] -> PosPairs
-- returnExample questions = PosPairs questions answersExample [[1,2], [0,3],[7,0]]

-- instance ToJSON PosPairs where
--     toJSON PosPairs {..} = object
--         [ "questions" .= posQuestions
--         , "answers" .= posAnswers
--         , "positions" .= positions
--         ]

instance ToJSON Crossword where 
    toJSON Crossword {..} = object
        [ "set" .= set 
        , "positions" .= positions
        , "remaining" .= remaining
        , "minx" .= minx
        , "maxx" .= maxx
        , "miny" .= miny
        , "maxy" .= maxy
        ]

postCreateCrosswordR :: Handler Value
postCreateCrosswordR = do
    pairs <- (requireJsonBody :: Handler Pairs)
    --returnJson $ returnExample $ questions pairs
    case solveRun $ Prelude.map unpack $ answers pairs of
        Just solution -> returnJson $ solution
        Nothing -> error "Impossible to create crossword!"

data Crossword = Crossword {
    set :: M.Map (Int,Int) Char,
    positions :: [(Int, Int)],
    remaining :: [String],
    minx :: Int,
    maxx :: Int,
    miny :: Int,
    maxy :: Int
} deriving (Show)

basicState :: Crossword
basicState = Crossword M.empty [] examples 0 0 0 0

examples :: [String]
examples = ["tomek","traktor","paranienormalni"]

firstLoop :: String -> Int -> Crossword -> Crossword
firstLoop [] num (Crossword m pos r minx maxx miny maxy) = (Crossword (M.insert (num,0) '1' m) ((Prelude.++) pos [(-1,0)]) r (-1) maxx miny maxy)
firstLoop (e:es) num (Crossword m pos r minx maxx miny maxy) 
    | num == 0 = firstLoop es (num + 1) (Crossword (M.insert (num,0) e m) pos (foldl (\b a -> if a == (e:es) then b else a:b) [] r) minx (maxx+1) miny maxy)
    | otherwise = firstLoop es (num + 1) (Crossword (M.insert (num,0) e m) pos r minx (maxx+1) miny maxy)

firstInsert :: Crossword -> String -> Crossword 
firstInsert crossword element = firstLoop element 0 crossword

splits :: [a] -> [([a], a, [a])]
splits (x:xs) = splits' ([], x, xs)
    where
    splits' p@(_,_,[]) = [p]
    splits' p@(pre,curr,(y:ys)) = p:(splits' ((Prelude.++) pre (curr:[]),y,ys))
splits [] = []

findPlaces :: (String, Char, String) -> Crossword -> [(Int, Int)]
findPlaces (prev, t, tail) (Crossword m pos r minx maxx miny maxy) = M.foldlWithKey (\b k val -> if val /= '1' && val == t then k:b else b) [] m

checkNeighbourhood :: [(Int, Int)] -> M.Map (Int, Int) Char -> Bool
checkNeighbourhood els m = foldl (\b (x', y') -> b && (M.lookup (x',y') m == Nothing || M.lookup (x',y') m == (Just '1'))) True els
--checkNeighbourhood _ _ = True

tryHorizontal :: (Int,Int) -> String -> Crossword -> [Crossword]
tryHorizontal (x,y) [] (Crossword m pos r minx maxx miny maxy) =  if M.lookup (x,y) m == Nothing || M.lookup (x,y) m == (Just '1') then [Crossword (M.insert (x,y) '1' m) pos r minx maxx miny maxy] else []
tryHorizontal start@(x, y) (w:ws) c@(Crossword m pos r minx maxx miny maxy) = 
    --nalezaloby tez sprawdzac sasiadow :o
    if (M.lookup (x,y) m == Nothing && (checkNeighbourhood [(x-1,y),(x+1,y)] m)) || (M.lookup (x,y) m == (Just w))
        then tryHorizontal (x,y+1) ws (Crossword (M.insert start w m) pos r minx maxx miny maxy) 
        else []

tryVertical :: (Int,Int) -> String -> Crossword -> [Crossword]
tryVertical (x,y) [] (Crossword m pos r minx maxx miny maxy) = if M.lookup (x,y) m == Nothing || M.lookup (x,y) m == (Just '1') then [Crossword (M.insert (x,y) '1' m) pos r minx maxx miny maxy] else []
tryVertical start@(x, y) (w:ws) c@(Crossword m pos r minx maxx miny maxy) = 
    if (M.lookup (x,y) m == Nothing && (checkNeighbourhood [(x,y-1),(x,y+1)] m))  || (M.lookup (x,y) m == (Just w))  
        then tryVertical (x+1,y) ws (Crossword (M.insert start w m) pos r minx maxx miny maxy) 
        else []


tryPutWord :: (Int,Int) -> (String, Char, String) -> Crossword -> [Crossword]
tryPutWord cor@(x, y) (prev, t, tail) c@(Crossword m pos r minx maxx miny maxy) 
    | M.lookup (x-1, y) m == Nothing && M.lookup (x+1, y) m == Nothing = tryVertical (x-dl,y) ((Prelude.++) prev (t:tail)) (Crossword m ((Prelude.++) pos [(x-dl-1,y)]) r (min minx (x-dl-1)) (max maxx (x+dlt)) miny maxy)  
    | M.lookup (x, y-1) m == Nothing && M.lookup (x, y+1) m == Nothing = tryHorizontal (x,y-dl) ((Prelude.++) prev (t:tail)) (Crossword m ((Prelude.++) pos [(x,y-dl-1)]) r minx maxx (min miny (y-dl-1   )) (max maxy (y+dlt)))
    | otherwise = []
    where dl = Prelude.length prev
          dlt = Prelude.length tail  

nextSteps :: String -> Crossword -> [Crossword]
nextSteps element crossword@(Crossword m p r minx maxx miny maxy)
    | Prelude.null $ set crossword = return $ firstInsert crossword element
--dla kazdej literki inny watek :)
    | otherwise = do
        el <- splits element
        --tu zrob forka
        newPlace <- findPlaces el crossword
        tryPutWord newPlace el (Crossword m p (foldl (\b a -> if a == element then b else a:b) [] r) minx maxx miny maxy)

solveCrossword :: Crossword -> ListT (State (Maybe Crossword)) ()
solveCrossword problem
    | Prelude.null $ remaining problem = do
        currentBest <- lift Control.Monad.Trans.State.Strict.get
        if maybe False (\(Crossword map pos r minx' maxx' miny' maxy') -> (maxx'-minx')*(maxy'-miny') <= ((maxx problem - (minx problem))*(maxy problem-(miny problem)))) currentBest
            then 
            return ()
            else
            lift $ put $ Just problem
    | otherwise =  ListT (return $ (Prelude.foldMap (\a -> (nextSteps a problem)) (remaining problem))) >>= solveCrossword 
  
solveRun :: [String] -> Maybe Crossword
solveRun elements = execState
    (runListT $ solveCrossword (Crossword M.empty [] elements 0 0 0 0)) Nothing

-- moveToZero :: Crossword -> Crossword
-- moveToZero (Crossword m minx maxx miny maxy) = Crossword (foldl f (M.empty :: M.Map (Int,Int) String) (M.keys m)) 0 0 0 0
--     where f = (\b (kx, ky) -> M.insert (kx-minx,ky-miny) (maybe '0' id (M.lookup (kx, ky) m)))


