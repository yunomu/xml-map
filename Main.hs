module Main where

import Control.Applicative
import Text.XML.Light (Content(Elem, Text, CRef), Element, CData)
import qualified Text.XML.Light as XML

type Name = String

data Object
    = Object Name [Object]
    | Data String
  deriving (Show)

content :: (Element -> a) -> (CData -> a) -> (String -> a) -> Content -> a
content f _ _ (Elem a) = f a
content _ f _ (Text a) = f a
content _ _ f (CRef a) = f a

mkContent :: Content -> Object
mkContent = content
    el
    (Data . XML.cdData)
    undefined
  where
    el = Object
        <$> XML.qName . XML.elName
        <*> map mkContent . XML.elContent

trim :: [Object] -> Maybe Object
trim = g . f
  where
    f []                         = []
    f (o@(Object _ [Data _]):os) = o:f os
    f ((Object n ios):os)        = (Object n (f ios)):f os
    f ((Data _):os)              = f os
    g [Object "?xml" [o@(Object _ _)]] = Just o
    g _                                = Nothing

parseXML :: String -> Maybe Object
parseXML = trim . map mkContent . XML.parseXML

main :: IO ()
main = do
    str <- readFile "test.xml"
    print $ parseXML str
