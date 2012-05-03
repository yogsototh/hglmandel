 ## First version

Let us start with the general architecture.

First, the import system of Haskell is full of boilerplate.
A bit like Java. Let's play the song of our people:

> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef

Also, for efficiency reason, I won't use the default Haskell `Complex` data type. We declare our type:

> newtype Complex = C (Float,Float) deriving (Show,Eq)

and make it an element of the typeclass `Num`:

> instance Num Complex where
>     fromInteger n = C (fromIntegral n,0.0)
>     C (x,y) * C (z,t) = C (z*x - y*t, y*z + x*t)
>     C (x,y) + C (z,t) = C (x+z, y+t)
>     abs (C (x,y))     = C (sqrt (x*x + y*y),0.0)
>     signum (C (x,y))  = C (signum x , 0.0)

We declare some useful functions for manipulating complex numbers:

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

Well, up until here we didn't made something useful.
Just a lot of boilerplate and default value.
Sorry but it is not completely the end.
We start by giving the main architecture of our program:

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

The only interresting part is we declared that the function `display` will be used to render the graphics:

> display = do
>   clear [ColorBuffer] -- make the window black
>   loadIdentity -- reset any transformation
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen

Also here, there is only one interresting part, 
the draw will occurs in the function `drawMandelbrot`.

Now we must speak a bit about how OpenGL works.
We said that OpenGL is imperative by design.
In fact, you must write the list of actions in the right order.
No easy parallel drawing here.
Here is the function which will render something on the screen:

> drawMandelbrot =
>   renderPrimitive Points $ do
>     mapM_ (\(x,y,c) -> do
>                       color c
>                       vertex $ Vertex3 x y 0) allPoints

This is simple, without the `mapM_` function, it would be equivalent to:

~~~
drawMandelbrot = 
  renderPrimitive Points $ do
    color color1
    vertex $ Vertex3 x1 y1 0
    ...
    color colorN
    vertex $ Vertex3 xN yN 0
~~~

This is all the orders given in the right order.
Mainly it is, set the color, draw the point, set another color, draw another point, etc...

We need some kind of global variable, in fact, this is the proof of a bad design. But this is our first start:

> width = 320 :: GLfloat
> height = 320 :: GLfloat

And of course our list of colored points.
In OpenGL the default coordinate are from -1 to 1.

> allPoints :: [(GLfloat,GLfloat,Color3 GLfloat)]
> allPoints = [ (x/width,y/height,colorFromValue $ mandel x y) | 
>                   x <- [-width..width], 
>                   y <- [-height..height]]
>

We need a function which transform an integer value to some color:

> colorFromValue n =
>   let 
>       t :: Int -> GLfloat
>       t i = 0.5 + 0.5*cos( fromIntegral i / 10 )
>   in
>     Color3 (t n) (t (n+5)) (t (n+10))

And now the mandel function. 
Given two coordinates in pixels, it returns some integer value:

> mandel x y = 
>   let r = 2.0 * x / width
>       i = 2.0 * y / height
>   in
>       f (complex r i) 0 64

It uses the main mandelbrot function for each complex z.

> f :: Complex -> Complex -> Int -> Int
> f c z 0 = 0
> f c z n = if (magnitude z > 2 ) 
>           then n
>           else f c ((z*z)+c) (n-1)

Well, use this file, and see what occurs!

blogimage("hglmandel_v01.png","The mandelbrot set version 1")

But see what occurs, if we make the window bigger:

blogimage("hglmandel_v01_too_wide.png","The mandelbrot too wide, black lines and columns")
