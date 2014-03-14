{-# LANGUAGE RankNTypes #-}

import System.Environment
import System.Random
import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe

type Name = String

lineToName :: String -> String -> Maybe Name
lineToName targetSex s
    | sex == "x" || sex == targetSex = Just name
    | otherwise                      = Nothing
    where (sex,_:name) = break (==',') s

readNames :: String -> IO [Name]
readNames sex = do
    ls <- lines <$> readFile "Artifact.txt"
    return $ catMaybes $ map (lineToName sex) ls

type Rand a = Monad m => StateT StdGen m a

getRandomR :: Random a => (a,a) -> Rand a
getRandomR = state . randomR

randElem :: [a] -> Rand a
randElem as = (as!!) `liftM` getRandomR (0, length as - 1)

name :: String -> StateT StdGen IO String
name sex = do
    ns <- liftIO (readNames sex)
    n  <- getRandomR (1,6)
    rs <- replicateM n (randElem ns)
    return $ intercalate " " rs

main :: IO ()
main = do
    [sex] <- getArgs
    g     <- newStdGen
    print =<< evalStateT (name sex) g
