-- Adapted from Mandelbrot Set example at http://www.acceleratehs.org/examples/mandelbrot.html

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Data.Complex                 as A
import Data.Array.Accelerate.Data.Colour.RGB              as A
import Data.Array.Accelerate.Data.Colour.Names            as A

import Data.Array.Accelerate.LLVM.Native                  as CPU   

import qualified Prelude                                  as P

-- compute the value z_(n+1) at a given point c
next :: Exp (Complex Float) -> Exp (Complex Float) -> Exp (Complex Float)
next c z = c + z * z

-- iterate next for a fixed number of times, if it has not diverged it is in the set
step :: Exp (Complex Float) -> Exp (Complex Float, Int) -> Exp (Complex Float, Int)
step c (unlift -> (z, i)) = lift (next c z, i + constant 1)

-- to test whether the point has diverged yet, compute hypothenuse, or square both sides and check x^2 + x^2
dot :: Exp (Complex Float) -> Exp Float 
dot (unlift -> x :+ y) = x*x + y*y

mandelbrot 
    :: Int                                      -- image width
    -> Int                                      -- image height
    -> Int                                      -- iteration limit
    -> Float                                    -- divergence radius
    -> Complex Float                            -- view centre
    -> Acc (Array DIM2 (Complex Float, Int))    -- view width
mandelbrot screenX screenY depth radius (x0 :+ y0) width = 
    A.generate (A.constant (Z :. screenY :. screenX))
                (\ix -> let z0 = complexOfPixel ix
                            zn = while  (\zi -> snd zi  < constant depth
                                            && dot (fst zi) < constant radius)
                                        (\zi -> step z0 zi)
                                        (lift (z0, constant 0))
                        in
                        zn)
    where 
        complexOfPixel :: Exp DIM2 -> Exp (Complex Float)
        complexOfPixel (unlift -> Z :. y :. x) = 
            let
                height = P.fromIntegral screenY / P.fromIntegral screenX * width
                xmin   = x0 - width  / 2
                ymin   = y0 - height / 2
                --
                re     = constant xmin + (fromIntegral x * constant width)  / constant (P.fromIntegral screenX)
                im     = constant ymin + (fromIntegral y * constant height) / constant (P.fromIntegral screenY)
            in
            lift (re :+ im)