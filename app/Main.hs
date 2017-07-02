{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok

import Data.Text.IO (putStrLn)
import Prelude hiding (putStrLn)

data JLPT =
  JLPT0 |
  JLPT1 |
  JLPT2 |
  JLPT3 |
  JLPT4 deriving (Eq,Read,Show)

instance FromField JLPT where
  fromField (Field (SQLInteger b) _) =
    case b of
      0 -> return JLPT0
      1 -> return JLPT1
      2 -> return JLPT2
      3 -> return JLPT3
      4 -> return JLPT4

data Entry =
  Entry 
    { coreId     :: !Int
    , lexeme     :: !Text
    , kana       :: !Text
    , definition :: !Text
    , pos        :: !Text
    , jlpt       :: !JLPT
    } deriving (Eq,Read,Show)

instance FromRow Entry where
  fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field

getEntries :: String -> IO [Entry]
getEntries file = do 
  conn <- open file
  -- flds <- query_ conn "SELECT core_id,lexeme,kana,definition,pos,jlpt FROM je WHERE jlpt=1 ORDER BY RANDOM() LIMIT 3" :: IO [Entry]
  -- SELECT * FROM table ORDER BY RANDOM() LIMIT X
  flds <- query_ conn "SELECT core_id,lexeme,kana,definition,pos,jlpt FROM je ORDER BY RANDOM() LIMIT 30" :: IO [Entry]
  mapM_ (putStrLn . kana) flds
  close conn
  return flds
  
transform :: [Entry] -> [[(Text,Text,Text)]]
transform [] = []
transform (a:b:c:rst) = [[zed a, zed b, zed c]] ++ transform rst
  where
    zed x = (kana x, lexeme x, definition x)
transform _ = []

{-
make6000Db :: String -> [Entry] -> IO ()
make6000Db file entries = do
  conn <- open file
  execute_ conn createTableQuery 
  forM_ entries $ \n -> execute conn jeInsert n
  close conn
  
  where 
    createTableQuery = 
         "CREATE TABLE IF NOT EXISTS je ("
      <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
      <> "core_id INT,"
      <> "lexeme TEXT NOT NULL,"
      <> "kana TEXT NOT NULL,"
      <> "definition TEXT NOT NULL,"
      <> "pos TEXT NOT NULL,"
      <> "jlpt INT"
      <> ");"
    jeInsert = "INSERT INTO je (core_id,lexeme,kana,definition,pos,jlpt) VALUES (?,?,?,?,?,?)"
  
-}


{-
10 x 3
-}

simple :: [[(Text,Text,Text)]]
simple = 
  [ [("おとおさん","お父さん","father"),("おかあさん","お母さん","mother"),("おにいさん","お兄さん","younger brother")]
  , [("おとおさん","お父さん","father"),("おかあさん","お母さん","mother"),("おにいさん","お兄さん","younger brother")]
  ]
  
main :: IO ()
main = do 
  es <- transform <$> getEntries "je.sqlite"
  print es
  --print es
  --run simple
  run es
