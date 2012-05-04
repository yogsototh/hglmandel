 ## 3D Mandelbrot?

Why only draw the edge? 
It is clearly not as nice as drawing the complete surface.
Yeah, I know, but, as we use OpenGL, why not show something in 3D.

But, complex number are only in 2D and there is no 3D equivalent to complex.
In fact, the only extension known are quaternions, 4D.
As I know almost nothing about quaternions, I will use some extended complex.
I am pretty sure this construction is not useful for numbers.
But it will be enough for us to create something nice.

<div style="display:none">

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef

> type ColoredPoint = (GLfloat,GLfloat,GLfloat,Color3 GLfloat)

</div>

> newtype ExtComplex = C (Float,Float,Float) deriving (Show,Eq)
> instance Num ExtComplex where
>     fromInteger n = C (fromIntegral n,0.0,0.0)
>     C (x,y,u) * C (z,t,v) = C (z*x - y*t + u*v, y*z + x*t - u*v, x*v + y*t + u*z )
>     C (x,y,u) + C (z,t,v) = C (x+z, y+t, u+v)
>     abs (C (x,y,z))     = C (sqrt (x*x + y*y + z*z),0.0,0.0)
>     signum (C (x,y,z))  = C (signum x , 0.0, 0.0)
> extcomplex :: Float -> Float -> Float -> ExtComplex
> extcomplex x y z = C (x,y,z)
> 
> real :: ExtComplex -> Float
> real (C (x,y,z))    = x
> 
> im :: ExtComplex -> Float
> im   (C (x,y,z))    = y
>
> strange :: ExtComplex -> Float
> strange (C (x,y,z)) = z
> 
> magnitude :: ExtComplex -> Float
> magnitude = real.abs
> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   initialDisplayMode $= [DoubleBuffered]
>   -- We create a window with some title
>   createWindow "Mandelbrot Set with Haskell and OpenGL"
>   angle <- newIORef (0.0 :: GLfloat)
>   idleCallback $= Just (idle angle)
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   displayCallback $= display angle
>   -- We enter the main loop
>   mainLoop
> idle angle = do
>   a <- get angle
>   angle $=! (a + 2)
>   postRedisplay Nothing

> display angle = do
>   clear [ColorBuffer,DepthBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   a <- get angle
>   rotate a $ Vector3 1.0 0.0 (0.0::GLfloat)
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen
> 
> width  = 50 :: GLfloat
> height = 50 :: GLfloat
> deep   = 50 :: GLfloat


</div>

This time, instead of drawing all points, I'll simply want to draw the edges of the Mandelbrot set.
We change slightly the drawMandelbrot function.
We replace the `Points` by `LineLoop`

> drawMandelbrot =
>   -- We will print Points (not triangles for example) 
>   renderPrimitive Points $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,z,c) = do
>           color c -- set the current color to c
>           -- then draw the point at position (x,y,0)
>           -- remember we're in 3D
>           vertex $ Vertex3 x y z

And now, we should change our list of points.
Instead of drawing every point of the visible surface, 
we will choose only point on the surface.


> allPoints :: [ColoredPoint]
> allPoints = do
>   x <- [-width..width]
>   y <- [-height..height]
>   z <- [-deep..deep]
>   if mandel x y z /= 0
>   then []
>   else return (x/width,y/height,z/deep,colorFromValue (truncate z))

This function is interresting. 
For those not used to the list monad here is a natural language version of this function:

~~~
positivePoints =
    for all x in the range [-width..width]
    let y be smallest number s.t. mandel x y > 0
    if y is on 0 then don't return a point
    else return the value corresonding to (x,y,color for (x+iy))
~~~

In fact using the list monad you write like if you consider only one element at a time and the computation is done non deterministically.
To find the smallest number such that mandel x y > 0 we create a simple dichotomic search:

> findMaxOrdFor func minval maxval 0 = (minval+maxval)/2
> findMaxOrdFor func minval maxval n = 
>   if (func medpoint) /= 0 
>        then findMaxOrdFor func minval medpoint (n-1)
>        else findMaxOrdFor func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2

The new mandel function

> mandel x y z = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>       s = 2.0 * z / deep
>   in
>       f (extcomplex r i s) 0 64


<div style="display:none">

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.7 + 0.3*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

> f :: ExtComplex -> ExtComplex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

</div>

