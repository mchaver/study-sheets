{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.StudySheets.Utils where 

import           Text.LaTeX
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax

------------------
-- HaTeX functions
------------------

-- | Compile with uplatex for vertical CJK
utarticle :: ClassName
utarticle = "utarticle"

-- | Remove page numbers
suppressPageNumbering :: LaTeXC l => l
suppressPageNumbering = comm1 "pagenumbering" "gobble"

-- | Force content into a single page
singlepage :: LaTeXC l => l -> l
singlepage = liftL $ TeXEnv "minipage" [OptArg "t", OptArg "0pt", FixArg (commS "linewidth")]

-- | New line in tables
tabularnewline :: LaTeXC l => l
tabularnewline = comm0 "tabularnewline"

-- | Double barred lines
hhline :: LaTeXC l => l -> l
hhline = liftL $ \l -> TeXComm "hhline" [FixArg l]

-- | \hhline{|t:=:t:=:t:=:t|}
hhlineTop :: LaTeXC l => Int -> l
hhlineTop x = hhline (mconcat ["|", xs, end])
  where
    xs = mconcat $ replicate x "t:=:"
    end = if x > 0 then "t|" else "|"

-- | \hhline{|:=::=::=:|}
hhlineMiddle :: LaTeXC l => Int -> l
hhlineMiddle x = hhline (mconcat ["|", xs, "|"])
  where
    xs = mconcat $ replicate x ":=:"

-- | \hhline{|b:=:b:=:b:=:b|}
hhlineBottom :: LaTeXC l => Int -> l
hhlineBottom x = hhline (mconcat ["|", xs, end])
  where
    xs = mconcat $ replicate x "b:=:"
    end = if x > 0 then "b|" else "|"


--------------------
-- General functions
--------------------

-- | Check if all lists are same length in 2D list
balanced :: [[a]] -> Bool
balanced as =
  case length as of
    0 -> True
    _ -> foldl1 (&&) $ (\x -> length x == (length $ head as)) <$> as

-- |
subTailInit :: [[a]] -> [[a]]
subTailInit as = (reverse $ tail sa) ++ [first_clean]
  where 
    sa          = reverse as
    first       = head sa
    first_clean = init first
