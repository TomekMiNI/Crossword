{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Crossword where

import Import

getCrosswordR :: Handler Html
getCrosswordR = do
    defaultLayout $ do   
         $(widgetFile "crossword")
