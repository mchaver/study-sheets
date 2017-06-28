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

tnln :: LaTeXC l => l
tnln = comm0 "tabularnewline"

--textbf :: LaTeXC l => l -> l
--textbf = liftL $ \l -> TeXComm "textbf" [FixArg l]
-- maketitle :: LaTeXC l => l
hhline :: LaTeXC l => l -> l
hhline = liftL $ \l -> TeXComm "hhline" [FixArg l]

hhlineMiddle :: LaTeXC l => Int -> l
hhlineMiddle x = hhline (mconcat ["|", xs, "|"])
  where
    xs = mconcat $ replicate x ":=:"
-- \hhline{|:=::=::=:|}

cell :: LaTeXC l => Text -> Text -> Text -> l
cell a b c = ((raw a) & (raw b) & (raw c))

theBody :: Monad m => LaTeXT m ()
theBody = do
  tabular Nothing [VerticalLine, CenterColumn, VerticalLine, CenterColumn, VerticalLine] 
    (mconcat [noindent, (cell "Hello" "Goodbye" "Goodnight"), tnln, hhlineMiddle 3])
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
