{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import System.Environment

import Data.Yarr as Y
import Data.Yarr.Walk
import Data.Yarr.Shape as S
import Data.Yarr.Repr.Delayed
import Data.Yarr.Utils.FixedVector as V
import Data.Yarr.IO.List as Y

-- Generic stuff --

type Distance = VecList N3 Double
type Position = VecList N3 Double
type Force = VecList N3 Double
type Speed = VecList N3 Double
type Mass = Double
type Time = Double

type Array r = UArray r L Dim1

type Velocities r = Array r Speed
type Positions r = Array r Position
type Distances r = Array r Distance
type Forces r = Array r Force
type Masses r = Array r Mass

gConst :: Double
gConst = 6.67384e-11  -- gravitational constant

-- softening constant squared
-- FIXME: Probably far too small
eps2 :: Double
eps2 = 0.25

forces
    :: (USource r1 L Dim1 Position, DefaultFusion r1 D L Dim1,
        USource r2 L Dim1 Mass, DefaultFusion r2 D L Dim1)
    => Positions r1 -> Masses r2 -> Forces D
forces ps ms =
    delayLinear $ reduceInner accForces (const (return V.zero)) partialForces
  where
    accForces = reduceL S.foldl (V.zipWith (+))
    partialForces = icartProduct2 forceBetween pms pms
    pms = Y.dzipWith (,) ps ms
    forceBetween (i, j) (pos1, mass1) (pos2, mass2)
        | i == j    = V.zero
        | otherwise = V.map (* b) posDelta
      where posDelta = V.zipWith (-) pos1 pos2
            dist2 = V.sum $ V.map (^ 2) posDelta
            a = 1.0 / (dist2 + eps2)
            b = (negate gConst) * mass1 * mass2 * a * (sqrt a)

stepVelocity
    :: (USource r1 L Dim1 Speed, DefaultFusion r1 D L Dim1,
        USource r2 L Dim1 Force, DefaultFusion r2 D L Dim1,
        USource r3 L Dim1 Mass, DefaultFusion r3 D L Dim1)
    => Time -> Velocities r1 -> Forces r2 -> Masses r3 -> Velocities D
stepVelocity timeStep vs fs ms = Y.dzipWith3 newSpeed vs fs ms
  where newSpeed speed force mass =
            V.zipWith (+) speed (V.map (\f -> f * timeStep / mass) force)

stepPosition
    :: (USource r1 L Dim1 Position, DefaultFusion r1 D L Dim1,
        USource r2 L Dim1 Speed, DefaultFusion r2 D L Dim1)
    => Time -> Positions r1 -> Velocities r2 -> Positions D
stepPosition timeStep ps vs = Y.dzipWith newPos ps vs
  where newPos pos speed = V.zipWith (+) pos (V.map (* timeStep) speed)


-- [Sun -- Earth -- Jupiter] system -- -- >> --

nBodies = 3

timeStep :: Time
timeStep = 864000.0

sunMass, jupiterMass, earthMass :: Mass
sunMass     = 1.9889e30
jupiterMass = 1.8986e27
earthMass   = 5.9722e24

jupiterPerihelion :: Double
jupiterPerihelion = 7.405736e11

earthPerihelion :: Double
earthPerihelion = 1.470983e11

jupiterV :: Speed
jupiterV = vl_3 (-1.0965244901087316e02) (-1.3710001990210707e04) 0

jupiterR :: Distance
jupiterR = vl_3 (negate jupiterPerihelion) 0 0

earthV :: Speed
earthV = vl_3 2.694354528161541e03 3.016946927465788e04 0

earthR :: Distance
earthR = vl_3 earthPerihelion 0 0

sunV :: Speed
sunV = V.zero

sunR :: Distance
sunR = V.zero

initVs :: IO (Velocities F)
initVs = Y.fromList nBodies [earthV, jupiterV, sunV]

initRs :: IO (Distances F)
initRs = Y.fromList nBodies [earthR, jupiterR, sunR]

initMasses :: IO (Masses F)
initMasses = Y.fromList nBodies [earthMass, jupiterMass, sunMass]

stepOnce :: Masses F -> Positions F -> Velocities F -> Forces F -> IO ()
stepOnce ms ps vs fs = do
    loadS S.fill (forces ps ms) fs
    loadS S.fill (stepVelocity timeStep vs fs ms) vs
    loadS S.fill (stepPosition timeStep ps vs) ps

main :: IO ()
main = do
    [nSteps] <- fmap (fmap read) getArgs
    vs <- initVs
    ps <- initRs
    ms <- initMasses
    fs <- new nBodies
    replicateM_ nSteps $ stepOnce ms ps vs fs
    posList <- Y.toList ps
    speedList <- Y.toList vs
    print (posList, speedList)