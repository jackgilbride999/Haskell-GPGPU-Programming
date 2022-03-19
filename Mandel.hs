{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}


module Mandel where

import Data.Array.Accelerate                              as A hiding ( fromInteger )
import Data.Array.Accelerate.Data.Complex                 as A
import Data.Array.Accelerate.Data.Colour.RGB              as A
import Data.Array.Accelerate.Data.Colour.Names            as A

import Prelude                                            ( fromInteger )
import qualified Prelude                                  as P


mandelbrot
    :: forall a b. (RealFloat a, FromIntegral Int a, Ord b, P.Num b, P.Num (Exp b))
    => Int                                  -- ^ image width
    -> Int                                  -- ^ image height
    -> Acc (Scalar a)                       -- ^ centre x
    -> Acc (Scalar a)                       -- ^ centre y
    -> Acc (Scalar a)                       -- ^ view width
    -> Acc (Scalar b)                   -- ^ iteration limit
    -> Acc (Scalar a)                       -- ^ divergence radius
    -> Acc (Array DIM2 (Complex a, b))
mandelbrot screenX screenY (the -> x0) (the -> y0) (the -> width) (the -> limit) (the -> radius) =
  A.generate (A.constant (Z :. screenY :. screenX)) mandelLoop
  where
    mandelLoop :: Exp DIM2 -> Exp (Complex a, b)
    mandelLoop ix = while condition updation initialization
      where
        condition zi = snd zi < limit && dot (fst zi) < radius
        updation zi = step z0 zi
        initialization = lift (z0, constant 0)
        z0 = complexOfPixel ix screenX screenY x0 y0 width limit radius

-- Convert the given array index, representing a pixel in the final image,
-- into the corresponding point on the complex plane.
--
complexOfPixel :: forall a b . (RealFloat a, FromIntegral Int a) => Exp DIM2 -> Int -> Int -> Exp a -> Exp a -> Exp a -> Exp b -> Exp a -> Exp (Complex a) 
complexOfPixel (unlift -> Z :. y :. x) screenX screenY x0 y0 width limit radius =
  lift (re :+ im)
    where
      height = P.fromIntegral screenY / P.fromIntegral screenX * width
      xmin   = x0 - width  / 2
      ymin   = y0 - height / 2
      re     = xmin + fromIntegral x * width  / fromIntegral (constant screenX)
      im     = ymin + fromIntegral y * height / fromIntegral (constant screenY)

-- Divergence condition
dot :: (Elt a, P.Num (Exp a)) => Exp (Complex a) -> Exp a
dot (unlift -> x :+ y) = x*x + y*y

-- Take a single step of the recurrence relation
step :: (RealFloat a, P.Num (Exp b), Elt b, P.Num b) => Exp a -> Exp (a, b) -> Exp (a, b)
step c (unlift -> (z, i)) = lift (next c z, i + constant 1)

next :: (P.Num a) => a -> a -> a
next c z = c + z * z

-- Convert the iteration count on escape to a colour.
--
-- Uses the method described here:
-- <http://stackoverflow.com/questions/16500656/which-color-gradient-is-used-to-color-mandelbrot-in-wikipedia>
--
escapeToColour
    :: (RealFloat a, ToFloating Int32 a)
    => Acc (Scalar Int32)
    -> Exp (Complex a, Int32)
    -> Exp Word32
escapeToColour (the -> limit) (unlift -> (z, n)) =
  n == limit ? (packRGB black, packRGB $ ultra (toFloating ix / toFloating points))
      where
        mag     = magnitude z
        smooth  = logBase 2 (logBase 2 mag)
        ix      = truncate (sqrt (toFloating n + 1 - smooth) * scale + shift) `mod` points
        --
        scale   = 256
        shift   = 1664
        points  = 2048 :: Exp Int

-- Pick a nice colour, given a number in the range [0,1].
--
ultra :: Exp Float -> Exp Colour
ultra p =
  p <= p1 ? (
    interp (p0,p1) (c0,c1) p,
    p <= p2 ? (
      interp (p1,p2) (c1,c2) p,
      p <= p3 ? (
        interp (p2,p3) (c2,c3) p,
        p <= p4 ? (
          interp (p3,p4) (c3,c4) p,
                  interp (p4,p5) (c4,c5) p))))
  where
    p0 = 0.0     ; c0 = rgb8 0   7   100
    p1 = 0.16    ; c1 = rgb8 32  107 203
    p2 = 0.42    ; c2 = rgb8 237 255 255
    p3 = 0.6425  ; c3 = rgb8 255 170 0
    p4 = 0.8575  ; c4 = rgb8 0   2   0
    p5 = 1.0     ; c5 = c0

-- interpolate each of the RGB components
interp :: (Exp Float, Exp Float)-> (Exp Colour , Exp Colour) -> Exp Float -> Exp Colour
interp (x0,x1) (y0,y1) x =
        rgb 
          (linear (x0,x1) (r0,r1) x)
          (linear (x0,x1) (g0,g1) x)
          (linear (x0,x1) (b0,b1) x)
  where
      RGB r0 g0 b0 = unlift y0 :: RGB (Exp Float)
      RGB r1 g1 b1 = unlift y1 :: RGB (Exp Float)


-- linear interpolation
linear :: (P.Fractional a) => (a, a) -> (a, a) -> a -> a
linear (x0,x1) (y0,y1) x =
  y0 + (x - x0) * (y1 - y0) / (x1 - x0)
