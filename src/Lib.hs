{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Data.Matrix

import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax

import qualified Data.Text as T

run :: IO ()
-- run = execLaTeXT tables >>= renderFile "tables.tex"
run = execLaTeXT tables >>= \l -> writeFile "tables.tex" (prettyLaTeX l)

tables :: Monad m => LaTeXT m ()
tables = thePreamble >> document theBody

utarticle :: ClassName
utarticle = "utarticle"

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do 
  documentclass [CustomOption "12pt", CustomOption "a4j", landscape, CustomOption "dvipdfmx"] utarticle
  usepackage [] "color"  
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

{-
tabular :: LaTeXC l =>
           Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                       --   Defaulted to 'Center'.
        -> [TableSpec] -- ^ Table specification of columns and vertical lines.
        -> l       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
        -> l       -- ^ Resulting table syntax.
tabular Nothing ts  = liftL $ TeXEnv "tabular" [ FixArg $ TeXRaw $ renderAppend ts ]
tabular (Just p) ts = liftL $ TeXEnv "tabular" [ OptArg $ TeXRaw $ render p , FixArg $ TeXRaw $ renderAppend ts ]


matrixTabular :: (LaTeXC l, Texy a)
              => [l] -- ^ (Non-empty) List of column titles
              -> Matrix a -- ^ Matrix of data
              -> l -- ^ Data organized in a tabular environment
matrixTabular ts m =
  let spec = VerticalLine : (intersperse VerticalLine $ replicate (ncols m) CenterColumn) ++ [VerticalLine]
  in  tabular Nothing spec $ mconcat
        [ hline
        , foldl1 (&) ts
        , lnbk
        , hline
        , mconcat $ fmap (
            \i -> mconcat [ foldl1 (&) $ fmap (\j -> texy (m ! (i,j))) [1 .. ncols m]
                          , lnbk
                          , hline
                            ] ) [1 .. nrows m]
]
-}

-- | Start a new line. In a 'tabular', it starts a new row, so use 'newline' instead.
-- lnbk  :: LaTeXC l => l
-- lnbk = fromLaTeX $ TeXLineBreak Nothing False

tabularnewline :: LaTeXC l => l
tabularnewline = comm0 "tabularnewline"

--textbf :: LaTeXC l => l -> l
--textbf = liftL $ \l -> TeXComm "textbf" [FixArg l]
-- maketitle :: LaTeXC l => l
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

{-
--\def\cell(#1,#2,#3){
--  \rule{0pt}{3ex} \hspace*{-.4cm}  {\LARGE #1} \newline
--  \rule{0pt}{3ex} \hspace*{.4cm}   {\small #2} \newline
--  \rule{0pt}{3ex} \hspace*{.425cm} {\small #3}
--}
-}

cell :: Monad m => Text -> Text -> Text -> LaTeXT m ()
cell t1 t2 t3 = do
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

-- \begin{tabular}{||p{5.5cm} || p{5.5cm} || p{5.5cm} ||}
{-
\cell(おとおさん,お父さん,father) & \cell(おかあさん,お母さん,mother) & \cell(おにいさん,お兄さん,younger brother)
\tnewline

\cell(おじさん,叔父さん,uncle) & \cell(たのしい,楽しい,fun) & \cell(かえる,帰る,go home)
\tnewline
-}
theBody :: Monad m => LaTeXT m ()
theBody = do
  noindent
  tabular Nothing [DVerticalLine, ParColumnTop "5.5cm", DVerticalLine, ParColumnTop "5.5cm", DVerticalLine, ParColumnTop "5.5cm", DVerticalLine] 
    (mconcat 
      [ hhlineTop 3
      , ((cell "おとおさん" "お父さん" "father") & (cell "おかあさん" "お母さん" "mother") & (cell "おにいさん" "お兄さん" "younger brother")), tabularnewline, hhlineMiddle 3
      , ((cell "おとおさん" "お父さん" "father") & (cell "おかあさん" "お母さん" "mother") & (cell "おにいさん" "お兄さん" "younger brother")), tabularnewline
      , hhlineBottom 3
      ]
    )
