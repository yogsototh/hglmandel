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

</div>

As the code start to be more complex, I'll use some aliases:

> type ColoredPoint = (GLfloat,GLfloat,GLfloat,Color3 GLfloat)

Then I declare the new type `ExtComplex` (for exttended complex). 
An extension of complex numbers:

> data ExtComplex = C (GLfloat,GLfloat,GLfloat) 
>                   deriving (Show,Eq)
> instance Num ExtComplex where
>     fromInteger n = C (fromIntegral n,0.0,0.0)
>     -- The shape of the 3D mandelbrot 
>     -- will depend on this formula
>     C (x,y,u) * C (z,t,v) = C (x*z - y*t - u*v, 
>                                x*t + y*z + u*v, 
>                                x*v + z*u )
>     -- The rest is straightforward
>     C (x,y,u) + C (z,t,v) = C (x+z, y+t, u+v)
>     abs (C (x,y,z))     = C (sqrt (x*x + y*y + z*z),0.0,0.0)
>     signum (C (x,y,z))  = C (signum x , 0.0, 0.0)

The most important part is the new multiplication instance.
Instead of searching the holy grail of 3D Mandelbrot, I just found a nice one.

Then I list some functions to use this new type:

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

As we will use some 3D, we add some new directive in the boilerplate.
But mainly, we simply state that will use some depth buffer.
And also we will listen the keyboard.

> main :: IO ()
> main = do
>   -- GLUT need to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We will use the double buffered mode (GL constraint)
>   -- We also Add the DepthBuffer (for 3D)
>   initialDisplayMode $= 
>       [WithDepthBuffer,DoubleBuffered,RGBMode]
>   -- We create a window with some title
>   createWindow "3D HOpengGL Mandelbrot"
>   -- We add some directives
>   depthFunc  $= Just Less
>   -- matrixMode $= Projection
>   windowSize $= Size 500 500
>   -- Some state variables (I know it feels BAD)
>   angle   <- newIORef ((1.0 :: GLfloat,0.0))
>   zoom    <- newIORef (1.0 :: GLfloat)
>   campos  <- newIORef (0.0 :: GLfloat,0.0)
>   -- Action to call when waiting
>   idleCallback $= Just idle
>   -- We will use the keyboard
>   keyboardMouseCallback $= 
>           Just (keyboardMouse angle zoom campos)
>   -- Each time we will need to update the display
>   -- we will call the function 'display'
>   -- But this time, we add some parameters
>   displayCallback $= display angle zoom campos
>   -- We enter the main loop
>   mainLoop

We rotate the shape.

> idle = do
>   postRedisplay Nothing

We introduce some helper function to manipulate
standard `IORef`.

> modVar v f = do
>   v' <- get v
>   v $= (f v')
> mapFst f (x,y) = (f x,y)
> mapSnd f (x,y) = (x,f y)

And we use them to code the function handling keyboard.
We will use the keys `hjkl` to rotate, 
`oi` to zoom and `sedf` to move.
Also, hitting space will reset the view.

> keyboardMouse angle zoom pos key state modifiers position =
>   kact angle zoom pos key state
>   where 
>     -- reset view when hitting space
>     kact a z p (Char ' ') Down = do
>           a $= (0,0)
>           z $= 1
>           p $= (0,0)
>     -- use of hjkl to rotate
>     kact a _ _ (Char 'h') Down = modVar a (mapFst (+0.5))
>     kact a _ _ (Char 'l') Down = modVar a (mapFst (+(-0.5)))
>     kact a _ _ (Char 'j') Down = modVar a (mapSnd (+0.5))
>     kact a _ _ (Char 'k') Down = modVar a (mapSnd (+(-0.5)))
>     -- use o and i to zoom
>     kact _ s _ (Char 'o') Down = modVar s (*1.1)
>     kact _ s _ (Char 'i') Down = modVar s (*0.9)
>     -- use sdfe to move the camera
>     kact _ _ p (Char 's') Down = modVar p (mapFst (+0.1))
>     kact _ _ p (Char 'f') Down = modVar p (mapFst (+(-0.1)))
>     kact _ _ p (Char 'd') Down = modVar p (mapSnd (+0.1))
>     kact _ _ p (Char 'e') Down = modVar p (mapSnd (+(-0.1)))
>     -- any other keys does nothing
>     kact _ _ _ _ _ = return ()

Now, we will show the object using the display function.
Note, this time, display take some parameters.
Mainly, this function if full of boilerplate:

> display angle zoom position = do
>    -- make the window black
>   clear [ColorBuffer,DepthBuffer]
>   -- Transformation to change the view
>   loadIdentity -- reset any transformation
>   (x,y) <- get position
>   translate $ Vector3 x y 0 
>   z <- get zoom
>   scale z z z
>   (xangle,yangle) <- get angle
>   rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
>   rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
>   -- Now that all transformation were made
>   -- We create the object(s)
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen

Not much to say about this function.
Mainly there are two parts: apply some transformations, draw the object.

 ### The 3D Mandelbrot

First, we will set the resolution to 180 pixels.

> nbDetails = 180 :: GLfloat
> width  = nbDetails
> height = nbDetails
> deep   = nbDetails

This time, instead of just drawing some line or some group of points,
we will show triangles.
The idea is that we should provide points three by three.

> drawMandelbrot = do
>   -- We will print Points (not triangles for example) 
>   renderPrimitive Triangles $ do
>     mapM_ drawColoredPoint allPoints
>   where
>       drawColoredPoint (x,y,z,c) = do
>           color c
>           vertex $ Vertex3 x y z

Now instead of providing only one point at a time, we will provide six points.

blogimage("triangles.png","Explain triangles")

> allPoints :: [ColoredPoint]
> allPoints = depthPoints ++ map inverse  depthPoints
>   where inverse (x,y,z,c) = (x,y,-z+1/deep,c)
> 
> depthPoints :: [ColoredPoint]
> depthPoints = do
>   x <- [-width..width]
>   y <- [-height..height]
>   let 
>       z = findMaxOrdFor (mandel x y) 0 deep 7
>       z' = findMaxOrdFor (mandel (x+1) y) 0 deep 7
>       z'' = findMaxOrdFor (mandel (x+1) (y+1)) 0 deep 7
>       z''' = findMaxOrdFor (mandel x (y+1)) 0 deep 7
>       c1 = mandel    x    y   (z+1)
>       c2 = mandel (x+1)   y   (z'+1)
>       c3 = mandel (x+1) (y+1) (z''+1)
>       c4 = mandel    x  (y+1) (z'''+1)
>       p1 = (    x/width,    y/height,  z/deep,colorFromValue c1)
>       p2 = ((x+1)/width,    y/height, z'/deep,colorFromValue c2)
>       p3 = ((x+1)/width,(y+1)/height,z''/deep,colorFromValue c3 )
>       p4 = (    x/width,(y+1)/height,z'''/deep,colorFromValue c4 )
>   if (and $ map (>=57) [c1,c2,c3,c4])
>   then []
>   else [p1,p2,p3,p1,p3,p4]

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
