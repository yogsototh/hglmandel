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

> data ExtComplex = C (GLfloat,GLfloat,GLfloat) deriving (Show,Eq)
> instance Num ExtComplex where
>     fromInteger n = C (fromIntegral n,0.0,0.0)
>     C (x,y,u) * C (z,t,v) = C (z*x - y*t + u*v, y*z + x*t - u*v, x*v + y*t + u*z )
>     C (x,y,u) + C (z,t,v) = C (x+z, y+t, u+v)
>     abs (C (x,y,z))     = C (sqrt (x*x + y*y + z*z),0.0,0.0)
>     signum (C (x,y,z))  = C (signum x , 0.0, 0.0)
> extcomplex :: GLfloat -> GLfloat -> GLfloat -> ExtComplex
> extcomplex x y z = C (x,y,z)
> 
> real :: ExtComplex -> GLfloat
> real (C (x,y,z))    = x
> 
> im :: ExtComplex -> GLfloat
> im   (C (x,y,z))    = y
>
> strange :: ExtComplex -> GLfloat
> strange (C (x,y,z)) = z
> 
> magnitude :: ExtComplex -> GLfloat
> magnitude = real.abs
> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   initialDisplayMode $= [DoubleBuffered]
>   -- We create a window with some title
>   createWindow "Mandelbrot Set with Haskell and OpenGL"
>   -- Some state variables
>   angle <- newIORef (0.0 :: GLfloat)
>   delta <- newIORef (0.05 :: GLfloat)
>   zoom <- newIORef (1.00 :: GLfloat)
>   position <- newIORef (0.0::GLfloat,0.0)
>   -- Action to call when waiting
>   idleCallback $= Just (idle angle delta)
>   keyboardMouseCallback $= Just (keyboardMouse delta zoom position)
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   displayCallback $= display angle zoom position
>   -- We enter the main loop
>   mainLoop
> idle angle delta = do
>   a <- get angle
>   d <- get delta
>   angle $=! (a + d)
>   postRedisplay Nothing

> keyboardMouse delta zoom pos key state modifiers position =
>   keyboardAct delta zoom pos key state
>   where 
>     keyboardAct d _ _ (Char ' ') Down = do
>           d' <- get d
>           d $= 0
>     keyboardAct d _ _ (Char 'j') Down = do
>           d' <- get d
>           d $= (d'+0.01)
>     keyboardAct d _ _ (Char 'k') Down = do
>           d' <- get d
>           d $= (d'-0.01)
>     keyboardAct _ s _ (Char 'o') Down = do
>           s' <- get s
>           s $= (s'*1.1)
>     keyboardAct _ s _ (Char 'i') Down = do
>           s' <- get s
>           s $= (s'*0.9)
>     keyboardAct _ _ p (Char 'f') Down = do
>           (x,y) <- get p
>           p $= (x+0.1,y)
>     keyboardAct _ _ p (Char 's') Down = do
>           (x,y) <- get p
>           p $= (x-0.1,y)
>     keyboardAct _ _ p (Char 'e') Down = do
>           (x,y) <- get p
>           p $= (x,y-0.1)
>     keyboardAct _ _ p (Char 'd') Down = do
>           (x,y) <- get p
>           p $= (x,y+0.1)
>     keyboardAct _ _ _ _ _ = return ()

> display angle zoom position = do
>   clear [ColorBuffer,DepthBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   (x,y) <- get position
>   translate $ Vector3 x y 0 
>   z <- get zoom
>   scale z z z
>   a <- get angle
>   rotate a $ Vector3 1.0 0.0 (0.0::GLfloat)
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen
> 
> width  = 500 :: GLfloat
> height = 500 :: GLfloat
> deep   = 500 :: GLfloat


</div>

This time, instead of drawing all points, I'll simply want to draw the edges of the Mandelbrot set.
We change slightly the drawMandelbrot function.
We replace the `Points` by `LineLoop`

> drawMandelbrot =
>   -- We will print Points (not triangles for example) 
>   renderPrimitive LineLoop $ do
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
>   let z = findMaxOrdFor (mandel x y) 0 deep 7
>   if mandel x y z /= 0
>   then []
>   else return (x/width,y/height,z/deep,colorFromValue $ mandel x y (z+1))

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
