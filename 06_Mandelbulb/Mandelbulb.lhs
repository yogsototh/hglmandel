 ## Optimization


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
>     ,(Press 'z' , translate zdir 0.1)
>     ,(Press 'r' , translate zdir (-0.1))
>     ,(Press '+' , zoom 1.1)
>     ,(Press '-' , zoom (1/1.1))
>     ,(Press 'h' , resize 1.2)
>     ,(Press 'g' , resize (1/1.2))
>     ]


> -- I prefer to set my own name for these types
> data World = World {
>       angle       :: Point3D
>     , scale       :: Scalar
>     , position    :: Point3D
>     , box         :: Box3D
>     , told        :: Time -- last frame time
>     , cache       :: [YObject]
>     } 


> instance DisplayableWorld World where
>   winTitle _ = "The YGL Mandelbulb"
>   camera w = Camera {
>         camPos = position w, 
>         camDir = angle w,
>         camZoom = scale w }
>   objects = cache

<div style="display:hidden">

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
> resize r world = 
>   tmpWorld { cache = objectFunctionFromWorld tmpWorld }
>   where 
>       tmpWorld = world { box = (box world) {
>               resolution = sqrt ((resolution (box world))**2 * r) }}
> 
> main :: IO ()
> main = yMainLoop inputActionMap idleAction initialWorld

> -- We initialize the world state
> -- then angle, position and zoom of the camera
> -- And the shape function
> initialWorld :: World
> initialWorld = World {
>    angle = makePoint3D (-30,-30,0)
>  , position = makePoint3D (0,0,0)
>  , scale = 0.8
>  , box = Box3D { minPoint = makePoint3D (-2,-2,-2)
>                , maxPoint =  makePoint3D (2,2,2)
>                , resolution =  0.16 }
>  , told = 0
>  , cache = objectFunctionFromWorld initialWorld
>  }
> 
> objectFunctionFromWorld w = [Atoms atomList]
>   where atomListPositive = 
>           getObject3DFromShapeFunction (shapeFunc (resolution (box w))) (box w)
>         atomList = atomListPositive ++ 
>           map negativeTriangle atomListPositive
>         negativeTriangle (ColoredTriangle (p1,p2,p3,c)) = 
>               ColoredTriangle (negz p1,negz p2,negz p3,c)
>               where negz (P (x,y,z)) = P (x,y,-z)
>
> idleAction :: Time -> World -> World
> idleAction tnew world = 
>       world {
>         angle = (angle world) + (delta -*< zdir)
>       , told = tnew
>       }
>   where 
>       anglePerSec = 5.0
>       delta = anglePerSec * elapsed / 1000.0
>       elapsed = fromIntegral (tnew - (told world))
> 
> shapeFunc :: Scalar -> Function3D
> shapeFunc res x y = 
>   let 
>       z = findMaxOrdFor (ymandel x y) 0 1 20
>   in
>   if and [ findMaxOrdFor (ymandel (x+xeps) (y+yeps)) 0 1 20 < 0.000001 |
>               val <- [res], xeps <- [-val,val], yeps<-[-val,val]]
>       then Nothing 
>       else Just (z,colorFromValue ((ymandel x y z) * 64))
> 
> colorFromValue :: Point -> Color
> colorFromValue n =
>   let 
>       t :: Point -> Scalar
>       t i = 0.7 + 0.3*cos( i / 10 )
>   in
>     makeColor (t n) (t (n+5)) (t (n+10))
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

</div>

- [`YGL.hs`](code/06_Mandelbulb/YGL.hs), the 3D rendering framework
- [`Mandel`](code/06_Mandelbulb/Mandel.hs), the mandel function
- [`ExtComplex`](code/06_Mandelbulb/ExtComplex.hs), the extended complexes

