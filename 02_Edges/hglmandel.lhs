 ## Only the edges

<div style="display:hidden">

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef
> newtype Complex = C (Float,Float) deriving (Show,Eq)
> instance Num Complex where
>     fromInteger n = C (fromIntegral n,0.0)
>     C (x,y) * C (z,t) = C (z*x - y*t, y*z + x*t)
>     C (x,y) + C (z,t) = C (x+z, y+t)
>     abs (C (x,y))     = C (sqrt (x*x + y*y),0.0)
>     signum (C (x,y))  = C (signum x , 0.0)
> complex :: Float -> Float -> Complex
> complex x y = C (x,y)
> 
> real :: Complex -> Float
> real (C (x,y))    = x
> 
> im :: Complex -> Float
> im   (C (x,y))    = y
> 
> magnitude :: Complex -> Float
> magnitude = real.abs
> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   initialDisplayMode $= [DoubleBuffered]
>   -- We create a window with some title
>   createWindow "Mandelbrot Set with Haskell and OpenGL"
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   displayCallback $= display
>   -- We enter the main loop
>   mainLoop
> display = do
>   clear [ColorBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen
> 
> width = 320 :: GLfloat
> height = 320 :: GLfloat


</div>

This time, instead of drawing all points, I'll simply want to draw the edges of the Mandelbrot set.
We change slightly the drawMandelbrot function.
We replace the `Points` by `LineLoop`

> drawMandelbrot =
>   -- We will print Points (not triangles for example) 
>   renderPrimitive LineLoop $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,c) = do
>           color c -- set the current color to c
>           -- then draw the point at position (x,y,0)
>           -- remember we're in 3D
>           vertex $ Vertex3 x y 0 

And now, we should change our list of points.
Instead of drawing every point of the visible surface, 
we will choose only point on the surface.

> allPoints = positivePoints ++ map (\(x,y,c) -> (x,-y,c)) (reverse positivePoints)

We only need to compute the positive point.
The mandelbrot set is symetric on the abscisse axis.

> positivePoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
> positivePoints = do
>               x <- [-width..width]
>               let y = findMaxOrdFor (mandel x) 0 height 10 -- log height
>               return (x/width,y/height,colorFromValue $ mandel x y)


We make a simple dichotomic search:

> findMaxOrdFor func minval maxval 0 = (minval+maxval)/2
> findMaxOrdFor func minval maxval n = 
>   if (func medpoint) /= 0 
>        then findMaxOrdFor func minval medpoint (n-1)
>        else findMaxOrdFor func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2

<div style="display:hidden">

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

> mandel x y = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>   in
>       f (complex r i) 0 64

> f :: Complex -> Complex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

</div>

