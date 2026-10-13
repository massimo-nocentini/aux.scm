{-# Language TypeOperators, FlexibleContexts #-}
-- Bird Migration problem 1 (one-bird logistic regression)
-- from the PPAML Challenge
-- ghc -O2 -main-is Birds1 -i.. Birds1
{-

-}

module Birds1 where

import ReadCSV
import System.IO

import Syntax
import Util
import Data.Foldable (forM_)
import Control.Applicative
import Control.Monad (foldM_)
import System.Environment

import Data.Array.Unboxed
import Data.Array.IO
import Data.Int
import Data.List (foldl')

import Debug.Trace

-- Parameters of the data set
nYears = 30::Int
nDays  = 20::Int                        -- per year

type Year = Int                         -- 1..nYears
type Day  = Int                         -- 1..nDays

gridDim = 4
type CellNum = Int                      -- 1..16, column-wise, see the doc
nCells = gridDim * gridDim

nFeatures = 4::Int                      -- The number of features
type FeatureIdx = Int                   -- 1..FeatureIdx

-- The count of birds observed at a given day (after sunset) of a given year
-- at a given cell
type ObsData = UArray (Year,Day,CellNum) Int16

--                              fromCell toCell
type Features = UArray (Year,Day,CellNum,CellNum,FeatureIdx) Float


model :: Features -> ObsData -> Model [Double]
model features obs = do
  b1  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b2  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b3  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  b4  <- abs <$> dist normal 0 10 -- see Statement of Problem, p9
  let bs = collect [b1,b2,b3,b4]
  forM_ [1..nYears] $ \year -> do -- each year
    foldM_ (\n day -> do -- one bird exists in cell n on each day(1..nDays-1)
      let trans n bs = [ (j,theta year day n j features bs) | j<-neighbors n]
      -- n' cell where the bird is on day+1
      let day' = day + 1
      {-
      n' <- case birdpos obs year day' of
          Just nreal -> dist (nreal `condition` categorical) $
                        liftA2 trans n bs 
          Nothing    -> dist categorical $ liftA2 trans n bs
      -}
      n' <- dist categorical $ liftA2 trans n bs 
      forM_ [1..nCells] $ \i ->
        dist (fromIntegral (obs ! (year,day',i)) `condition` poisson)
           ((\nv -> if i==nv then 1 else 0) <$> n')
      return n'
      ) (1 :: SExp Int) [1..nDays-1]
  return bs

-- where is the single bird on the day d of the year y
birdpos :: ObsData -> Year -> Day -> Maybe CellNum
birdpos obs y d = case [i | i <- [1..nCells], obs ! (y,d,i) > 0] of
  [i] -> Just i
  _   -> Nothing

phi :: Year -> Day -> CellNum -> CellNum -> Features -> [Double] -> Double
phi y d from to features bs = sum $ zipWith (*) fs bs
  where
    fs = [ realToFrac $ features ! (y, d, from, to, i) | i <- [1..nFeatures]]

neighbors :: CellNum -> [CellNum]
neighbors n = filter (\i -> dist n i <= d_max) [1..nCells]
 where
  dist n m = let (ix,iy) = divMod (n-1) gridDim
                 (jx,jy) = divMod (m-1) gridDim
             in (ix-jx)^2+(iy-jy)^2
  d_max = 18

-- multinomial (0; a,b,c) --> 0
-- multinomial (1,theta(i,1),,,theta(i,16))  -->
--     categorical [(1,theta(i,1)),(2,(theta(i,2))),... (16,theta(i,16))]
theta :: Year -> Day -> CellNum -> CellNum -> Features -> [Double] -> Double
theta y d from to features bs = exp $ phi y d from to features bs



-- normals means std
-- returns several independently distributed normal random variables
-- whose means are given by the list means, with the same std
normals :: [SExp Double] -> SExp Double -> MCMCM [SExp Double]
normals means std = mapM (\m -> dist normal m std) means


dot :: [Double] -> [SExp Double] -> SExp Double
dot xs ays = foldr1 (+) $ zipWith (\x ay -> (*x) <$> ay) xs ays

collect :: [SExp a] -> SExp [a]
collect = sequenceA

-- ----------------------------------------------------------------------
-- Loading input data

-- Observations
-- The type of input observation data
type ObsRec =
  Rec (Year :+ Day :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int :+
       Int)

-- Load the bird observations
read_obs :: IO ObsData
read_obs = withFile "onebird-observations.csv" ReadMode go
  where
    go h = hGetLine h >> loop h []         -- skip the header line
    loop h acc = hIsEOF h >>= \b ->
      if b then return (array ((1,1,1),(nYears,nDays,nCells)) acc)
      else do
      Rec (y :+ d :+
           x1 :+ x2 :+ x3 :+ x4 :+ x5 :+ x6 :+ x7 :+ x8 :+
           x9 :+ x10 :+ x11 :+ x12 :+ x13 :+ x14 :+ x15 :+ x16) <- readCSV h
      let rows = [((y,d,1),x1),((y,d,2),x2),((y,d,3),x3),((y,d,4),x4),
                  ((y,d,5),x5),((y,d,6),x6),((y,d,7),x7),((y,d,8),x8),
                  ((y,d,9),x9),((y,d,10),x10),((y,d,11),x11),((y,d,12),x12),
                  ((y,d,13),x13),((y,d,14),x14),((y,d,15),x15),((y,d,16),x16)]
      loop h (rows ++ acc)

test_obs = do
  r <- read_obs
  print $ r ! (1,1,1)
  print $ r ! (1,1,16)
  print $ r ! (30,20,1)
  print $ r ! (30,20,16)
  print $ r ! (30,10,16)

-- Features

type FeaturesRec =
  Rec (Year :+ Day :+ CellNum :+ CellNum :+ Float :+ Float :+ Float :+ Float)

read_features :: IO Features
read_features = withFile "onebird_features.csv" ReadMode go
  where
    bounds = ((1,1,1,1,1),
              (nYears,nDays-1,nCells,nCells,nFeatures))
    go h = do
      hGetLine h        -- skip the header line
      arr <- newArray bounds 0 ::
        IO (IOUArray (Year,Day,CellNum,CellNum,FeatureIdx) Float)
      loop h arr
      
    loop h arr = hIsEOF h >>= \b ->
      if b then freeze arr
      else do
      Rec (y :+ d :+ cfrom :+ cto :+ f1 :+ f2 :+ f3 :+ f4) <- readCSV h
      writeArray arr (y,d,cfrom,cto,1) f1
      writeArray arr (y,d,cfrom,cto,2) f2
      writeArray arr (y,d,cfrom,cto,3) f3
      writeArray arr (y,d,cfrom,cto,4) f4
      loop h arr

test_features = do
  r <- read_features
  mapM_ (\i -> putStrLn $ unwords [show i, show $ r ! i]) $
        [(1,1,1,1,f)   | f <- [1..4]] ++
        [(1,1,16,16,f) | f <- [1..4]] ++
        [(30,19,1,1,f) | f <- [1..4]] ++
        [(30,19,16,15,f) | f <- [1..4]]

-- ----------------------------------------------------------------------
-- Main program

main1 burn n = do
  obs      <- read_obs
  features <- read_features
  let r = drop burn $ mcmC n $ model features obs
--  return $ (momenta (map head r), momenta (map (head . tail) r))
--  return $ map (/(fromIntegral $ length r)) $ foldr1 (zipWith (+)) r
  return . map (/(fromIntegral $ length r)) .
           foldl' (zipWith (+)) (repeat 0) $ r

main = do
  [burn',n'] <- getArgs
  let burn = read burn' :: Int
  let    n = read n' :: Integer
  main1 burn n >>= print



-- Results from the old Hakaru
-- b1,b2,b3,b4
-- 3.3779743775135875,4.66617475773739,4.1061948941490245,5.593322825143362
--
-- time ./Birds1 1000 1000000
-- [4.3696541828670865,6.5965237230656415,5.780875501909224,6.593019768389584]
-- real 2m34.868s
-- user 2m34.456s
-- sys  0m0.387s
