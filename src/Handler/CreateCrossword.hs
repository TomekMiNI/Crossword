{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.CreateCrossword where
import Data.Text (Text)
import Import

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

postCreateCrosswordR :: Handler Value
postCreateCrosswordR = do
    pairs <- (requireJsonBody :: Handler Pairs)
    returnJson $ returnExample $ questions pairs
