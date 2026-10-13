{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution where

import System.Random

import qualified Data.Array as A
import Data.Array.Base (unsafeAt)

type LogLikelihood = Double

class Distribution d a | d -> a where
  logDensity :: d -> a -> LogLikelihood
  sample     :: RandomGen g => d -> g -> (a, LogLikelihood, g)

-- re-sampleable distribution. This is the place to specify
-- custom proposals
class Distribution d a => RDistribution d a | d -> a where
  resample :: RandomGen g =>
     d ->
     a -> LogLikelihood -> -- old value and its LL
     g ->
     ((a,              -- new sample
       LogLikelihood,  -- its likelihood
       g),             -- new seed
      LogLikelihood)   -- (fwd - bwd) for the proposal step.
  -- By default, resample is really sample again
  resample d _ llold g =
    let s@(_,ll,_) = sample d g in
    (s,ll-llold)

data SomeRDistr a where
  Dist :: RDistribution d a => !d -> SomeRDistr a

instance Distribution (SomeRDistr a) a where
  logDensity (Dist d) = logDensity d
  sample     (Dist d) = sample d
instance RDistribution (SomeRDistr a) a where
  resample   (Dist d) = resample d

minusInf = log 0

newtype Dirac a = Dirac a
dirac :: a -> Dirac a
dirac = Dirac

instance Eq a => Distribution (Dirac a) a where
  logDensity (Dirac theta) x = if x == theta then 0 else minusInf
  sample     (Dirac theta) g = (theta,0,g)


data Bern = Bern !Double !Double !Double
bern :: Double -> Bern
bern p = Bern p (log p) (log (1-p))

instance Distribution Bern Bool where
  logDensity (Bern _ lT lF) x = if x then lT else lF
  sample     (Bern p lT lF) g =
    let (t, g') = randomR (0, 1) g in
    if t <= p then (True, lT, g') else (False, lF, g')

-- resampling always flips, so fwd and bwd steps are 0
instance RDistribution Bern Bool where
{-
  resample d x _ g =
    let x' = not x in
    ((x', logDensity d x', g),0)
-}


-- Uniformly from a given set
data Uniformly a = Uniformly !(A.Array Int a) !Double
uniformly :: Eq a => [a] -> Uniformly a
uniformly [] = error "uniformly: empty list"
uniformly lst = Uniformly (A.listArray (0,len-1) lst) (- log (fromIntegral len))
 where len = length lst
       
instance Eq a => Distribution (Uniformly a) a where
  logDensity (Uniformly a ll) x = if x `elem` a then ll else minusInf
  sample     (Uniformly a ll) g = let (i,g1) = randomR (A.bounds a) g in
                           (unsafeAt a i, ll, g1)

instance Eq a => RDistribution (Uniformly a) a

data Categorical a = Categorical ![(a,Double)]
categorical :: Eq a => [(a,Double)] -> Categorical a
categorical lst =
  let ptotal = sum $ map snd lst in
  Categorical $ ptotal `seq` map (\ (i,p) -> (i,p/ptotal)) lst

-- Make sure we report the real LL rather than up to the constant
-- (which may depend on the argument to |categorical|).
-- When logDensity is called during the scoring of a proposal,
-- the parameters of the distribution have likely changed
instance Eq a => Distribution (Categorical a) a where
  logDensity (Categorical lst) x = maybe minusInf log $ (lookup x lst)
  sample     (Categorical lst) g = loop p lst
    where
      (p, g1) = randomR (0, 1) g
      loop p ((x,p0):t) | p < p0 = (x,log p0,g1)
                        | otherwise = loop (p-p0) t

instance Eq a => RDistribution (Categorical a) a

data Uniform = Uniform !Double !Double !Double
uniform :: Double -> Double -> Uniform
uniform lo hi = Uniform lo hi (log (recip (hi - lo)))

instance Distribution Uniform Double where
  logDensity (Uniform lo hi l) x =
    if lo <= x && x <= hi then l else minusInf
  sample     (Uniform lo hi l) g = case randomR (lo, hi) g of
                                     (x,g1) -> (x, l, g1)

instance RDistribution Uniform Double

marsaglia :: (RandomGen g, Random a, Ord a, Floating a) => g -> ((a, a), g)
marsaglia g0 = -- "Marsaglia polar method"
  let (x, g1) = randomR (-1,1) g0
      (y, g ) = randomR (-1,1) g1
      s       = x * x + y * y
      q       = sqrt ((-2) * log s / s)
  in if 1 >= s && s > 0 then ((x * q, y * q), g) else marsaglia g

{-
choose :: (RandomGen g) => Mixture k -> g -> (k, Prob, g)
choose (Mixture m) g0 =
  let peak = maximum (M.elems m)
      unMix = M.map (LF.fromLogFloat . (/peak)) m
      total = M.foldl' (+) (0::Double) unMix
      (p, g) = randomR (0, total) g0
      f !k !v b !p0 = let p1 = p0 + v in if p <= p1 then k else b p1
      err p0 = error ("choose: failure p0=" ++ show p0 ++
                      " total=" ++ show total ++
                      " size=" ++ show (M.size m))
  in (M.foldrWithKey f err unMix 0, LF.logFloat total * peak, g)

chooseIndex :: (RandomGen g) => [Double] -> g -> (Int, g)
chooseIndex probs g0 =
  let (p, g) = random g0
      k = fromMaybe (error ("chooseIndex: failure p=" ++ show p))
                    (findIndex (p <=) (scanl1 (+) probs))
  in (k, g)
-}

{-# INLINE normal_rng #-}
normal_rng :: (Real a, Floating a, Random a, RandomGen g) =>
              a -> a -> g -> (a, g)
normal_rng mu sd g | sd > 0 = case marsaglia g of
                                ((x, _), g1) -> (mu + sd * x, g1)
normal_rng _ _ _ = error "normal: invalid parameters"

data Normal = Normal !Double !Double !Double !Double
normal :: Double -> Double -> Normal
normal mu sd = Normal mu sd tau (log (tau / pi / 2))
 where tau = 1 / (sd*sd)

instance Distribution Normal Double where
  logDensity (Normal mu sd tau lf) x =
   (-tau * square (x - mu) + lf) / 2 where square y = y * y
  sample d@(Normal mu sd _ _) g = case normal_rng mu sd g of
                                  (x,g1) -> (x, logDensity d x, g1)

instance RDistribution Normal Double


{-
lnFact :: Integer -> Double
lnFact = logFactorial

-- Makes use of Atkinson's algorithm as described in:
-- Monte Carlo Statistical Methods pg. 55
--
-- Further discussion at:
-- http://www.johndcook.com/blog/2010/06/14/generating-poisson-random-values/
poisson_rng :: (RandomGen g) => Double -> g -> (Integer, g)
poisson_rng lambda g0 = make_poisson g0
   where smu = sqrt lambda
         b  = 0.931 + 2.53*smu
         a  = -0.059 + 0.02483*b
         vr = 0.9277 - 3.6224/(b - 2)
         arep  = 1.1239 + 1.1368/(b-3.4)
         lnlam = log lambda

         make_poisson :: (RandomGen g) => g -> (Integer,g)
         make_poisson g = let (u, g1) = randomR (-0.5,0.5) g
                              (v, g2) = randomR (0,1) g1
                              us = 0.5 - abs u
                              k = floor $ (2*a / us + b)*u + lambda + 0.43 in
                          case () of
                            () | us >= 0.07 && v <= vr -> (k, g2)
                            () | k < 0 -> make_poisson g2
                            () | us <= 0.013 && v > us -> make_poisson g2
                            () | accept_region us v k -> (k, g2)
                            _  -> make_poisson g2

         accept_region :: Double -> Double -> Integer -> Bool
         accept_region us v k = log (v * arep / (a/(us*us)+b)) <=
                                -lambda + (fromIntegral k)*lnlam - lnFact k

poisson :: Double -> Dist Integer
poisson l =
    let poissonLogDensity l' x | l' > 0 && x> 0 = (fromIntegral x)*(log l') - lnFact x - l'
        poissonLogDensity l' x | x==0 = -l'
        poissonLogDensity _ _ = log 0
    in Dist {logDensity = poissonLogDensity l . fromDiscrete,
             distSample = mapFst Discrete . poisson_rng l}
-}

-- Adapted from http://people.sc.fsu.edu/~burkardt/f_src/asa245/asa245.html

-- | Compute the logarithm of the gamma function Γ(/x/).  Uses
-- Algorithm AS 245 by Macleod.
--
-- Gives an accuracy of 10-12 significant decimal digits, except
-- for small regions around /x/ = 1 and /x/ = 2, where the function
-- goes to zero.  For greater accuracy, use 'logGammaL'.
--
-- Returns ∞ if the input is outside of the range (0 < /x/ ≤ 1e305).
-- | Positive infinity.
m_pos_inf :: Double
m_pos_inf = 1/0
{-# INLINE m_pos_inf #-}

logGamma :: Double -> Double
logGamma x
    | x <= 0    = m_pos_inf
    -- Handle positive infinity. logGamma overflows before 1e308 so
    -- it's safe
    | x > 1e308 = m_pos_inf
    -- Normal cases
    | x < 1.5   = a + c *
                  ((((r1_4 * b + r1_3) * b + r1_2) * b + r1_1) * b + r1_0) /
                  ((((b + r1_8) * b + r1_7) * b + r1_6) * b + r1_5)
    | x < 4     = (x - 2) *
                  ((((r2_4 * x + r2_3) * x + r2_2) * x + r2_1) * x + r2_0) /
                  ((((x + r2_8) * x + r2_7) * x + r2_6) * x + r2_5)
    | x < 12    = ((((r3_4 * x + r3_3) * x + r3_2) * x + r3_1) * x + r3_0) /
                  ((((x + r3_8) * x + r3_7) * x + r3_6) * x + r3_5)
    | x > 3e6   = k
    | otherwise = k + x1 *
                  ((r4_2 * x2 + r4_1) * x2 + r4_0) /
                  ((x2 + r4_4) * x2 + r4_3)
  where
    (a , b , c)
        | x < 0.5   = (-y , x + 1 , x)
        | otherwise = (0  , x     , x - 1)

    y      = log x
    k      = x * (y-1) - 0.5 * y + alr2pi
    alr2pi = 0.918938533204673

    x1 = 1 / x
    x2 = x1 * x1

    r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
    r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
    r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070

    r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
    r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
    r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514

    r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
    r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
    r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2

    r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
    r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
    r4_4 = 6.012459259764103

-- Direct implementation of  "A Simple Method for Generating Gamma Variables"
-- by George Marsaglia and Wai Wan Tsang.
gamma_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
gamma_rng shape _   _ | shape <= 0.0  = error "gamma: got a negative shape paramater"
gamma_rng _     scl _ | scl <= 0.0  = error "gamma: got a negative scale paramater"
gamma_rng shape scl g | shape <  1.0  = (gvar2, g2)
                      where (gvar1, g1) = gamma_rng (shape + 1) scl g
                            (w,  g2) = randomR (0,1) g1
                            gvar2 = scl * gvar1 * (w ** recip shape) 
gamma_rng shape scl g = 
    let d = shape - 1/3
        c = recip $ sqrt $ 9*d
        -- Algorithm recommends inlining normal generator
        n = normal_rng 1 c
        (v, g2) = until (\y -> fst y > 0.0) (\ (_, g') -> normal_rng 1 c g') (n g)
        x = (v - 1) / c
        sqr = x * x
        v3 = v * v * v
        (u, g3) = randomR (0.0, 1.0) g2
        accept  = u < 1.0 - 0.0331*(sqr*sqr) || log u < 0.5*sqr + d*(1.0 - v3 + log v3)
    in case accept of
         True -> (scl*d*v3, g3)
         False -> gamma_rng shape scl g3

gammaLogDensity :: Double -> Double -> Double -> Double
gammaLogDensity shape scl | shape > 0 && scl > 0 =
  let logShape =  log shape
      logGShape = logGamma shape in
  \x -> if x < 0 then minusInf else
          scl * logShape - scl * x + (shape - 1) * log x - logGShape
gammaLogDensity _ _ = \_ -> minusInf

data Gamma = Gamma !Double !Double
gamma :: Double -> Double -> Gamma
gamma shape scl = Gamma shape scl

instance Distribution Gamma Double where
  logDensity (Gamma shape scl) x = gammaLogDensity shape scl x
  sample   d@(Gamma shape scl) g =
    let (x,g1) = gamma_rng shape scl g in (x,logDensity d x, g1)

instance RDistribution Gamma Double

beta_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
beta_rng a b g | a <= 1.0 && b <= 1.0 =
                 let (u, g1) = randomR (0.0, 1.0) g
                     (v, g2) = randomR (0.0, 1.0) g1
                     x = u ** (recip a)
                     y = v ** (recip b)
                 in  case (x+y) <= 1.0 of
                       True -> (x / (x + y), g2)
                       False -> beta_rng a b g2
beta_rng a b g = let (ga, g1) = gamma_rng a 1 g
                     (gb, g2) = gamma_rng b 1 g1
                 in (ga / (ga + gb), g2)

betaLogDensity :: Double -> Double -> Double -> Double
betaLogDensity a b | a <= 0 || b <= 0 =
  error "beta: parameters must be positve" 
betaLogDensity a b =
  let factor = logGamma (a + b)
               - logGamma a
               - logGamma b
  in \x -> if x < 0 || x > 1 then error "beta: value must be between 0 and 1"
           else factor + (a - 1) * log x
                       + (b - 1) * log (1 - x)

data Beta = Beta !Double !Double
beta :: Double -> Double -> Beta
beta a b = Beta a b

instance Distribution Beta Double where
  logDensity (Beta a b) x = betaLogDensity a b x
  sample   d@(Beta a b) g = let (x,g1) = beta_rng a b g in
                              (x, logDensity d x, g1)

instance RDistribution Beta Double

-- Poisson distribution
data Poisson = Poisson !Double !Double
poisson :: Double -> Poisson
poisson lambda | lambda >= 0 = Poisson lambda (exp(-lambda))
poisson lambda = error "In Poisson distribution, lambda cannot be negative"

-- if lambda = 0 then poisson(0) = dirac(0)
instance Distribution Poisson Int where
  -- density is (lambda ^ x) * exp(-lambda) / x!
  logDensity (Poisson lambda expl) x
    | x == 0      = -lambda
    | lambda == 0 = minusInf
    | x == 1      = log lambda - lambda
    | x == 2      = log (lambda*lambda/2) - lambda
    | otherwise   = fromIntegral x * log lambda - lambda
                     - logGamma (fromIntegral x + 1)

  -- Knuth algorithm
  sample (Poisson lambda expl) g
    | lambda == 0 = (0,0,g)
    | otherwise = let (u, g1) = randomR (0, 1) g
                  in loop u 1 1 g1
    where 
      -- acc is lamda^(k-1)/(k-1)!
      loop p k acc g | p > expl =
           let (u, g1) = randomR (0, 1) g
           in loop (p*u) (k+1) (acc*lambda/(fromIntegral k)) g1
                     | otherwise = (k-1, acc*expl, g)

instance RDistribution Poisson Int

{-
laplace_rng :: (RandomGen g) => Double -> Double -> g -> (Double, g)
laplace_rng mu sd g = sample (randomR (0.0, 1.0) g)
   where sample (u, g1) = case u < 0.5 of
                            True  -> (mu + sd * log (u + u), g1)
                            False -> (mu - sd * log (2.0 - u - u), g1)

laplaceLogDensity :: Floating a => a -> a -> a -> a
laplaceLogDensity mu sd x = - log (2 * sd) - abs (x - mu) / sd

laplace :: Double -> Double -> Dist Double
laplace mu sd = Dist {logDensity = laplaceLogDensity mu sd . fromLebesgue,
                      distSample = mapFst Lebesgue . laplace_rng mu sd}

-- Consider having dirichlet return Vector
-- Note: This is acutally symmetric dirichlet
dirichlet_rng :: (RandomGen g) => Int ->  Double -> g -> ([Double], g)
dirichlet_rng n' a g' = normalize (gammas g' n')
  where gammas g 0 = ([], 0, g)
        gammas g n = let (xs, total, g1) = gammas g (n-1)
                         ( x, g2) = gamma_rng a 1 g1 
                     in ((x : xs), x+total, g2)
        normalize (b, total, h) = (map (/ total) b, h)

dirichletLogDensity :: [Double] -> [Double] -> Double
dirichletLogDensity a x | all (> 0) x = sum' (zipWith logTerm a x) + logGamma (sum a)
  where sum' = foldl' (+) 0
        logTerm b y = (b-1) * log y - logGamma b
dirichletLogDensity _ _ = error "dirichlet: all values must be between 0 and 1"
-}

