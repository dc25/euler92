module Main where

import Control.Monad.State
import Data.IntMap as DMA

ssqr :: Int -> Int
ssqr x = if x == 0
         then 0
         else let (d,m) = divMod x 10
              in m*m + ssqr d


ssd :: Int -> Int
ssd = s 0 where s a x = if x == 0 
                        then a 
                        else let (d,m)=divMod x 10 
                             in s (a+m*m) d


checkOneM :: Int -> State (IntMap Int) Int
checkOneM v = 
    case v of
        1 -> return 1
        89 -> return 89
        n -> do
                m <- get
                case DMA.lookup v m of
                          Nothing -> do 
                                       s <- checkOneM (ssqr v)
                                       modify (DMA.insert v s) 
                                       return s
                          Just found -> return found

checkAllM :: Int -> State (IntMap Int) [Int]
checkAllM m = 
    mapM checkOneM [1..m]
    
checkOne :: Int -> Int
checkOne v = 
    case v of
        1 -> 1
        89 -> 89
        n -> checkOne $ ssd n

checkAll :: Int -> [Int]
checkAll m = 
    fmap checkOne [1..m]
    
main :: IO ()
main = do
    print $ length $ Prelude.filter (==89) $ evalState (checkAllM 10000000) DMA.empty
    print $ length $ Prelude.filter (==89) $ checkAll 10000000 
