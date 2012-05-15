 ## Functional organization?

Some points:

1. OpenGL and GLUT are linked to the C library.
   In particular the `mainLoop` function is a direct link to the C library (FFI).
   This function if so far from the pure spirit of functional languages.
   Could we make this better?
   We will have two choices, or create our own `mainLoop` function to make it more functional.
   Or deal with the imperative nature of the GLUT `mainLoop` function.
   As a goal of this article is to understand how to deal with existing library and particularly the one coming from impertive language we will continue to use the `mainLoop` function.
2. Or main problem come from user interaction.
   If you ask the Internet, about how to deal with user interaction with a functional paradigm, the main answer is to use _functional reactive programming_ (FRP).
   I read very few about FRP, and I might be completely wrong when I say that it is about creating a DSL where atoms are time functions.
   While I'm writting these lines, I don't know if I'll do something looking close to that.
   For now I'll simply try to resolve the first problem.

Then here is how I imagine things should go.
First, what the main loop should look like:

<code class="haskell">
functionalMainLoop =
    Read user inputs and provide a list of actions
    Apply all actions to the World
    Display one frame 
    repetere aeternum
</code>

Clearly, ideally we should provide only three parameters to this main loop function:

- an initial World state
- a mapping between the user interaction and function which modify the world
- a function which transform the world without user interaction.

The mapping between user input and actions.

> import YGL -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths

> -- Centralize all user input interaction
> inputActionMap :: InputMap
> inputActionMap = inputMapFromList [
>      (Press 'k' , rotate X 30)
>     ,(Press 'i' , rotate X (-30))
>     ,(Press 'j' , rotate Y 30)
>     ,(Press 'l' , rotate Y (-30))
>     ,(Press 'o' , rotate Z 30)
>     ,(Press 'u' , rotate Z (-30))
>     ,(Press 'f' , translate X 0.05)
>     ,(Press 's' , translate X (-0.05))
>     ,(Press 'e' , translate Y 0.05)
>     ,(Press 'd' , translate Y (-0.05))
>     ,(Press '+' , zoom (1.10))
>     ,(Press '-' , zoom (0.9))
>     ]

The type of each couple should be of the form
`(user input, f)` where (in a first time) `f:World -> World`.
It means, the user input will transform the world state.

And of course a type design the World State:

> -- I prefer to set my own name for these types
> data World = World {
>       angle       :: Point3D
>     , zoom        :: Point1D
>     , position    :: Point3D
>     , details     :: Point3D
>     , shape       :: Function3D
>     } 

With all associated functions:

> X = (1,0,0) :: Point3D
> Y = (0,1,0) :: Point3D
> Z = (0,0,1) :: Point3D
>
> xproj (x,_,_) = x
> yproj (_,y,_) = y
> zproj (_,_,z) = z
> 
> rotate :: Point3D -> Scalar -> World -> World
> rotate (a,b,c) angle world = world {
>     angle = angle world {
>           x = x (angle world) + a
>         , y = y (angle world) + b
>         , z = z (angle world) + c }}
> 
> rotate :: Point3D -> Scalar -> World -> World
> translate (a,b,c) len world = world {
>     position = position world {
>           x = x (position world) + a*len
>         , y = y (position world) + b*len
>         , z = z (position world) + c*len }}
> 
> rotate :: Scalar -> World -> World
> zoom z world = world {
>     zoom = z * (zoom world) }

- [`YBoiler.hs`](code/04_Mandelbulb/YBoiler.hs), the 3D rendering
- [`Mandel`](code/04_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/04_Mandelbulb/ExtComplex.hs), the extended complexes

> 
> -- yMainLoop takes two arguments
> --   the title of the window
> --   a function from time to triangles
> main :: IO ()
> main = yMainLoop "3D Mandelbrot" inputActionMap world 
>
> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- the number of details
> -- And the shape function
> world :: World
> world = World {
>    angle = (0,0,0)
>  , positon = (0,0,0)
>  , zoom = 1
>  , details = (100,100,100)
>  , shape x y = 
>       let depth = zproj (world details)
>       findMaxOrdFor (ymandel x y) 0 depth (truncate $ log depth)
> }
> 
> 
> findMaxOrdFor :: (a -> b) -> a -> a -> Int -> a
> findMaxOrdFor func minval maxval 0 = (minval+maxval)/2
> findMaxOrdFor func minval maxval n = 
>   if (func medpoint) /= 0 
>        then findMaxOrdFor func minval medpoint (n-1)
>        else findMaxOrdFor func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2
> 
> ymandel x y z = mandel (2*x/width) (2*y/height) (2*z/depth) 64
