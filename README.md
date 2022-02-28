# Haskell-GPGPU-Programming

Haskell accelerate implementation of Mandelbrot Set generation adapted from http://www.acceleratehs.org/examples/mandelbrot.html

v1.0

Adapted from example at https://www.acceleratehs.org/examples/mandelbrot.html.

Updated to use linear interpolation instead of cubic interpolation.
Updated to use Data.Array.Accelerate.IO.Codec.BMP instead of Data.Array.Accelerate.IO.
Updated to generate 10000 x 10000 image instead of 800 by 600. 

## Usage
```
ghc -O2 -threaded mandelbrot
./mandelbrot 
```