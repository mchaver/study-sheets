{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix

import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

import qualified Data.Text as T

import Data.List (intercalate)

run :: IO ()
run = execLaTeXT tables >>= \l -> writeFile "tables.tex" (prettyLaTeX l)

tables :: Monad m => LaTeXT m ()
tables = thePreamble >> document (theBody [ [("おとおさん","お父さん","father"),("おかあさん","お母さん","mother"),("おにいさん","お兄さん","younger brother")]
                                          , [("おとおさん","お父さん","father"),("おかあさん","お母さん","mother"),("おにいさん","お兄さん","younger brother")]
                                          ])

utarticle :: ClassName
utarticle = "utarticle"

balanced :: [[a]] -> Bool
balanced as =
  case length as of
    0 -> True
    _ -> foldl1 (&&) $ (\x -> length x == (length $ head as)) <$> as

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do 
  documentclass [CustomOption "12pt", CustomOption "a4j", landscape, CustomOption "dvipdfmx"] utarticle
  usepackage [] "color"  
  usepackage [] "hhline"
  suppressPageNumbering
  
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "textheight"), FixArg ".6in"]
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "topmargin"), FixArg "-.75in"]

suppressPageNumbering :: LaTeXC l => l
suppressPageNumbering = comm1 "pagenumbering" "gobble"

tabularnewline :: LaTeXC l => l
tabularnewline = comm0 "tabularnewline"

hhline :: LaTeXC l => l -> l
hhline = liftL $ \l -> TeXComm "hhline" [FixArg l]

-- \hhline{|t:=:t:=:t:=:t|}
hhlineTop :: LaTeXC l => Int -> l
hhlineTop x = hhline (mconcat ["|", xs, end])
  where
    xs = mconcat $ replicate x "t:=:"
    end = if x > 0 then "t|" else "|"

-- \hhline{|:=::=::=:|}
hhlineMiddle :: LaTeXC l => Int -> l
hhlineMiddle x = hhline (mconcat ["|", xs, "|"])
  where
    xs = mconcat $ replicate x ":=:"

-- \hhline{|b:=:b:=:b:=:b|}
hhlineBottom :: LaTeXC l => Int -> l
hhlineBottom x = hhline (mconcat ["|", xs, end])
  where
    xs = mconcat $ replicate x "b:=:"
    end = if x > 0 then "b|" else "|"

cell :: Monad m => (Text,Text,Text) -> LaTeXT m ()
cell (t1,t2,t3) = do
  fromLaTeX $ TeXComm "rule" [FixArg "0pt", FixArg "3ex"]
  comm1 "hspace*" "-.4cm"
  large3 $ raw t1
  newline
  
  fromLaTeX $ TeXComm "rule" [FixArg "0pt", FixArg "3ex"]
  comm1 "hspace*" ".4cm"
  small $ raw t2
  newline
  
  fromLaTeX $ TeXComm "rule" [FixArg "0pt", FixArg "3ex"]
  comm1 "hspace*" ".425cm"
  small $ raw t3

subTailInit :: [[a]] -> [[a]]
subTailInit as = (reverse $ tail sa) ++ [first_clean]
  where 
    sa          = reverse as
    first       = head sa
    first_clean = init first
    

theBody :: Monad m => [[(Text,Text,Text)]] -> LaTeXT m ()
theBody as = do
  noindent
  tabular Nothing [DVerticalLine, ParColumnTop "5.5cm", DVerticalLine, ParColumnTop "5.5cm", DVerticalLine, ParColumnTop "5.5cm", DVerticalLine] 
    (mconcat (
      [ hhlineTop width] ++
      -- flatted   put & in middle of cells, remove last hhlineMiddle3, 
      -- , (mconcat $ foldl1 (&) <$> subTailInit $ (++ [tabularnewline, hhlineMiddle width]) <$> fmap cell <$> as)
        -- ( foldl1 (&) <$> fmap cell <$> as)
        -- ( intercalate [tabularnewline, hhlineMiddle width] $ fmap cell <$> as)
        ( intercalate [tabularnewline, hhlineMiddle width] ((:[]) <$> foldl1 (&) <$> fmap cell <$> as))
      --, ((cell "おとおさん" "お父さん" "father") & (cell "おかあさん" "お母さん" "mother") & (cell "おにいさん" "お兄さん" "younger brother")), tabularnewline, hhlineMiddle 3
      --, ((cell "おとおさん" "お父さん" "father") & (cell "おかあさん" "お母さん" "mother") & (cell "おにいさん" "お兄さん" "younger brother")), tabularnewline
      ++ [tabularnewline, hhlineBottom width])
    )
  where
    width = length $ head as
