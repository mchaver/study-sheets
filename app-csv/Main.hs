{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (isSuffixOf)
import Data.List.Split
import qualified Data.Vector as V
import System.Directory
import System.IO (readFile)
import System.Process
import Text.LaTeX.StudySheets.CJK.Vertical

instance FromRecord JPCell where
  parseRecord v 
    | length v == 3 = JPCell <$> v .! 0 <*> v .! 1 <*> v .! 2
    | otherwise = mzero

splitIntoThrees :: [a] -> [[a]]
splitIntoThrees as = step [] as 
  where
    step rs [] = rs
    step rs as = 
      let (bs,cs) = splitAt 3 as
      in  step (rs ++ [bs]) cs

fillCell :: [JPCell] -> [JPCell]
fillCell cells =
  case length cells of
    1 -> cells ++ [JPCell "" "" "", JPCell "" "" ""]
    2 -> cells ++ [JPCell "" "" ""]
    _ -> cells

fillLast :: [[JPCell]] -> [[JPCell]]
fillLast cells =
  case cl of 
    0 -> cells
    _ -> 
      let (cellsHead,cellsTail) = splitAt (cl - 1) cells
          lastCell = head cellsTail
      in  cellsHead ++ [fillCell lastCell]
  where 
    cl = length cells

csvToTex :: FilePath -> FilePath
csvToTex fp = (head $ splitOn ".csv" fp) ++ ".tex"

csvToDvi :: FilePath -> FilePath 
csvToDvi fp = (head $ splitOn ".csv" fp) ++ ".dvi"

csvToPdf :: FilePath -> FilePath 
csvToPdf fp = (head $ splitOn ".csv" fp) ++ ".pdf"

main :: IO ()
main = do
  -- has to be less than 30
  jpCSVFiles <- filter (isSuffixOf ".csv") <$> getDirectoryContents jpCSVDir
  forM_ jpCSVFiles $ \ff -> do
    let f = jpCSVDir ++ "/" ++ ff
    print $ f
    csv <- BL.readFile f
    let eres = decode NoHeader csv :: Either String (V.Vector JPCell)
    case fillLast . splitIntoThrees . V.toList <$> eres of
      Left  err -> print err
      Right cells -> do 
        print cells
        mkJPVerticalStudySheet (jpPDFDir ++ "/" ++ csvToTex ff) cells
        rawSystem "/Library/TeX/texbin/uplatex"  ["-output-directory", jpPDFDir, (jpPDFDir ++ "/" ++ csvToTex ff)]
        rawSystem "/Library/TeX/texbin/dvipdfmx" ["-o", (jpPDFDir ++ "/" ++ csvToPdf ff), (jpPDFDir ++ "/" ++ csvToDvi ff)]
        -- /Library/TeX/texbin/uplatex -output-directory csv-sheets/jp/pdfs/ csv-sheets/jp/pdfs/2017-07-16-01.tex
        -- /Library/TeX/texbin/dvipdfmx -o csv-sheets/jp/pdfs/2017-07-16-01.pdf csv-sheets/jp/pdfs/2017-07-16-01.dvi
        return ()

  where
    jpCSVDir = "csv-sheets/jp"
    jpPDFDir = "csv-sheets/jp/pdfs"
