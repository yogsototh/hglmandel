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
- a function which transorm the world without user interaction.

The mapping between user input and actions.

> import YGL -- Most the OpenGL Boilerplate
> import Mandel -- The 3D Mandelbrot maths

> -- Centralize all user input interaction
> inputActionMap :: InputMap World
> inputActionMap = inputMapFromList [
>      (Press 'k' , rotate xdir 5)
>     ,(Press 'i' , rotate xdir (-5))
>     ,(Press 'j' , rotate ydir 5)
>     ,(Press 'l' , rotate ydir (-5))
>     ,(Press 'o' , rotate zdir 5)
>     ,(Press 'u' , rotate zdir (-5))
>     ,(Press 'f' , translate xdir 0.1)
>     ,(Press 's' , translate xdir (-0.1))
>     ,(Press 'e' , translate ydir 0.1)
>     ,(Press 'd' , translate ydir (-0.1))
>     ,(Press '+' , zoom 1.1)
>     ,(Press '-' , zoom (1/1.1))
>     ,(Press 'r' , resize 1.2)
>     ,(Press 'z' , resize (1/1.2))
>     ]

The type of each couple should be of the form
`(user input, f)` where (in a first time) `f:World -> World`.
It means, the user input will transform the world state.

And of course a type design the World State:

> -- I prefer to set my own name for these types
> data World = World {
>       angle       :: Point3D
>     , scale       :: Scalar
>     , position    :: Point3D
>     , shape       :: Function3D
>     , box         :: Box3D
>     } 

> instance DisplayableWorld World where
>   camera w = Camera {
>         camPos = position w, 
>         camDir = angle w,
>         camZoom = scale w }
>   objects w = [XYFunc (shape w) (box w)]

With all associated functions:

> xdir :: Point3D
> xdir = makePoint3D (1,0,0)
> ydir :: Point3D
> ydir = makePoint3D (0,1,0)
> zdir :: Point3D
> zdir = makePoint3D (0,0,1)
>
> rotate :: Point3D -> Scalar -> World -> World
> rotate dir angleValue world = 
>   world {
>      angle = (angle world) + (angleValue -*< dir) }
> 
> translate :: Point3D -> Scalar -> World -> World
> translate dir len world = 
>   world {
>     position = (position world) + (len -*< dir) }
> 
> zoom :: Scalar -> World -> World
> zoom z world = world {
>     scale = z * scale world }
> 
> resize :: Scalar -> World -> World
> resize r world = world {
>     box = (box world) {
>      resolution = (resolution (box world)) * r }}

- [`YBoiler.hs`](code/04_Mandelbulb/YBoiler.hs), the 3D rendering
- [`Mandel`](code/04_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/04_Mandelbulb/ExtComplex.hs), the extended complexes

> 
> -- yMainLoop takes two arguments
> --   the title of the window
> --   a function from time to triangles
> main :: IO ()
> main = yMainLoop "3D Mandelbrot" inputActionMap initialWorld
>
> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- And the shape function
> initialWorld :: World
> initialWorld = World {
>    angle = makePoint3D (0,1,0)
>  , position = makePoint3D (0,0,0)
>  , scale = 0.2
>  , shape = shapeFunc 
>  , box = Box3D { minPoint = makePoint3D (-2,-2,-2)
>                , maxPoint =  makePoint3D (2,2,2)
>                , resolution =  0.2 }
>  }
> 
> shapeFunc :: Function3D
> shapeFunc x y = 
>   let 
>       z = findMaxOrdFor (ymandel x y) 0 1 20
>   in
>   if z < 0.000001
>       then Nothing 
>       else Just z
> 
> 
> findMaxOrdFor :: (Fractional a,Num a,Num b,Eq b) => 
>                  (a -> b) -> a -> a -> Int -> a
> findMaxOrdFor _ minval maxval 0 = (minval+maxval)/2
> findMaxOrdFor func minval maxval n = 
>   if func medpoint /= 0 
>        then findMaxOrdFor func minval medpoint (n-1)
>        else findMaxOrdFor func medpoint maxval (n-1)
>   where medpoint = (minval+maxval)/2
> 
> ymandel :: Point -> Point -> Point -> Point
> ymandel x y z = fromIntegral (mandel x y z 64) / 64
