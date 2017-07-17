{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.StudySheets.CJK.Vertical (
    JPCell(..)
  , mkJPVerticalStudySheet
  ) where
  
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Text.LaTeX
import           Text.LaTeX.Base.Pretty (prettyLaTeX)
import           Text.LaTeX.Base.Class
import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.StudySheets.Utils

data JPCell =
  JPCell 
    { jpLexeme     :: Text
    , jpKana       :: Text
    , jpDefinition :: Text
    } deriving (Eq,Read,Show)

mkJPVerticalStudySheet :: FilePath -> [[JPCell]] -> IO ()
mkJPVerticalStudySheet file cells = 
  if balanced cells
    then execLaTeXT (mkPreambleAndDocument cells) >>= writeFile file . prettyLaTeX
    else T.putStrLn "Cells are unbalanced. Unable to create study sheet"

mkPreambleAndDocument :: Monad m => [[JPCell]] -> LaTeXT m ()
mkPreambleAndDocument l = mkPreamble >> document (mkBody l)

mkPreamble :: Monad m => LaTeXT m ()
mkPreamble = do 
  documentclass [CustomOption "12pt", CustomOption "a4j", landscape, CustomOption "dvipdfmx"] utarticle
  usepackage [] "color"  
  usepackage [] "hhline"
  suppressPageNumbering
  
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "textheight"), FixArg ".6in"]
  fromLaTeX $ TeXComm "addtolength" [FixArg (commS "topmargin"), FixArg "-.75in"]

mkCell :: Monad m => JPCell -> LaTeXT m ()
mkCell (JPCell t1 t2 t3) = do
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

-- as must be greater that zero
mkBody :: Monad m => [[JPCell]] -> LaTeXT m ()
mkBody as = singlepage $ do
  noindent
  tabular Nothing header
    -- || p(5.5cm) || p(5.5cm) || etc.
    (mconcat (
      [ hhlineTop width] ++
        ( intercalate [tabularnewline, hhlineMiddle width] ((:[]) <$> foldl1 (&) <$> fmap mkCell <$> as))
      ++ [tabularnewline, hhlineBottom width])
    )
  where
    width = length $ head as
    columnWidth = 16.5 / (fromIntegral width) :: Double
    header = [DVerticalLine] ++ (concat $ replicate width ([ParColumnTop ((raw . T.pack . show $ columnWidth) <> "cm"), DVerticalLine]))
