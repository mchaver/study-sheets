{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix

import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

run :: IO ()
-- run = execLaTeXT tables >>= renderFile "tables.tex"
run = execLaTeXT tables >>= \l -> writeFile "tables.tex" (prettyLaTeX l)

tables :: Monad m => LaTeXT m ()
tables = thePreamble >> document theBody

utarticle :: ClassName
utarticle = "utarticle"

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do 
  documentclass [CustomOption "a4j", CustomOption "12pt", landscape] utarticle
  usepackage [] "hhline"
  suppressPageNumbering
  
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "textheight"), FixArg ".6in"]
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "topmargin"), FixArg "-.75in"]
  
  -- comm1 str = liftL $ \l -> TeXComm str [FixArg l]
  --comm2 "addtolength" "\textheight" ".6in"
  --comm2 "addtolength" "\topmargin"  "-.75in"
  -- \addtolength{\textheight}{.6in}
  -- \addtolength{\topmargin}{-.75in}

suppressPageNumbering :: LaTeXC l => l
suppressPageNumbering = comm1 "pagenumbering" "gobble"

-- \pagenumbering{gobble}

theBody :: Monad m => LaTeXT m ()
theBody = do
  -- Table from a simple matrix
  center $ matrixTabular (fmap textbf ["x","y","z"]) $
    fromList 3 3 [ (1 :: Int)..]
  -- Table from a matrix calculated in-place
  center $ matrixTabular (fmap textbf ["Number","Square root"]) $
    matrix 9 2 $ \(i,j) -> if j == 1 then I i else R $ sqrt $ fromIntegral i

-- Creating custom instances of Texy to display elements
-- within a table.

data Number = R Double | I Int

instance Texy Number where
  texy (R x) = texy x
  texy (I i) = texy i
