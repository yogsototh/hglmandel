module YGL (
    -- Datas
    Point 
    , Scalar
    , Point3D (Point3D,xpoint,ypoint,zpoint)
    , makePoint3D -- helper (x,y,z) -> Point3D
    , (-*<) -- scalar product on Point3D
    , (-+<) -- add two Point3D
    , Function3D
    -- Your world state must be an instance
    -- of the DisplayableWorld type class
    , DisplayableWorld (camera,lights,objects)
    -- Datas related to DisplayableWorld
    , Camera (Camera,camPos,camDir,camZoom)
    , YObject (XYFunc, Tri)
    -- Datas related to user Input
    , InputMap
    , UserInput (Press,Ctrl,Alt,CtrlAlt)
    , inputMapFromList
    -- The main loop function to call
    , yMainLoop) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Monad (when)
import Data.Maybe (isNothing)

{-- Things start to be complex here.
- Just take the time to follow me.
--}

{-- A lot of declaration that I find helpful,
- I don't like default naming convention --}

-- | A 1D point
type Point   = GLfloat 
-- | A Scalar value
type Scalar  = GLfloat
-- | A 3D Point mainly '(x,y,z)'
data Point3D = Point3D { 
      xpoint :: Point
    , ypoint :: Point
    , zpoint :: Point }

makePoint3D :: (Point,Point,Point) -> Point3D
makePoint3D (x,y,z) = Point3D {xpoint=x, ypoint=y, zpoint=z}


infixr 5 -*<
(-*<) :: Scalar -> Point3D -> Point3D
(-*<) s p = Point3D {
              xpoint=s*xpoint p,
              ypoint=s*ypoint p,
              zpoint=s*zpoint p}

infixr 5 -+<
(-+<) :: Point3D -> Point3D -> Point3D
(-+<) p q = Point3D {
              xpoint=xpoint p*xpoint q,
              ypoint=ypoint p*ypoint q,
              zpoint=zpoint p*zpoint q}

toGLVector3 :: Point3D -> Vector3 GLfloat
toGLVector3 p = Vector3 (xpoint p) (ypoint p) (zpoint p)

toGLVertex3 :: Point3D -> Vertex3 GLfloat
toGLVertex3 p = Vertex3 (xpoint p) (ypoint p) (zpoint p)

-- | The Box3D type represent a 3D bounding box
-- | Note if minPoint = (x,y,z) and maxPoint = (x',y',z')
-- | Then to have a non empty box you must have
-- | x<x' & y<y' & z<z'
data Box3D = Box3D {
         minPoint :: Point3D 
       , maxPoint :: Point3D
       , resolution :: Scalar }

makeBox :: (Point,Point,Point) -> (Point,Point,Point) -> Scalar -> Box3D
makeBox mini maxi res = Box3D {
      minPoint = makePoint3D mini
    , maxPoint = makePoint3D maxi
    , resolution = res  }

type Function3D = Point -> Point -> Maybe Point
data YObject =   XYFunc Function3D
               | Tri [Point3D]

triangles :: YObject -> Box3D -> [Point3D]
triangles (XYFunc f) b = getObject3DFromShapeFunction f b
triangles (Tri tri) _ = tri

-- | We decalre the input map type we need here
-- | It is our API
type InputMap worldType = Map.Map UserInput (worldType -> worldType)
data UserInput = Press Char | Ctrl Char | Alt Char | CtrlAlt Char 
                 deriving (Eq,Ord,Show,Read)

-- | A displayable world 
class DisplayableWorld world where
    camera :: world -> Camera
    camera _ = defaultCamera 
    lights :: world -> [Light]
    lights _ = []
    objects :: world -> [YObject]
    objects _ = []

-- | the Camera type to know how to
-- | Transform the scene to see the right view.
data Camera = Camera {
          camPos  :: Point3D
        , camDir  :: Point3D
        , camZoom :: Scalar }

defaultCamera :: Camera
defaultCamera = Camera {
      camPos = makePoint3D (0,0,0)
    , camDir = makePoint3D (0,0,0)
    , camZoom = 1 }


-- Given a shape function and a delimited Box3D
-- return a list of Triangles to be displayed
getObject3DFromShapeFunction :: Function3D -> Box3D -> [Point3D]
getObject3DFromShapeFunction shape box = do
  x <- [xmin,xmin+res..xmax]
  y <- [ymin,ymin+res..ymax]
  let 
    neighbors = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
    -- zs are 3D points with found depth
    zs = map (\(u,v) -> (u,v,shape u v)) neighbors
    -- ps are 3D opengl points + color value
    removeMaybe (u,v,Just w) = (u,v,w)
    removeMaybe (_,_,Nothing) = (0,0,0)
    ps = map removeMaybe zs
  -- If the point diverged too fast, don't display it
  if any (\(_,_,z) -> isNothing z) zs
  then []
  -- Draw two triangles
  --    3 - 2
  --    | / |
  --    0 - 1
  else map makePoint3D [ps!!0,ps!!1,ps!!2,ps!!0,ps!!2,ps!!3]
  where
    -- some naming to make it 
    -- easier to read
    xmin = xpoint $ minPoint box
    xmax = xpoint $ maxPoint box
    ymin = ypoint $ minPoint box
    ymax = ypoint $ maxPoint box
    res = resolution box

inputMapFromList :: (DisplayableWorld world) => 
    [(UserInput,world -> world)] -> InputMap world
inputMapFromList = Map.fromList

{-- 
- We set our mainLoop function
- As you can see the code is _not_ pure 
- and not even functionnal friendly!
- But when called,
- it will look like a standard function.
--}
yMainLoop :: (DisplayableWorld worldType) =>
             String       -- window name
             -> InputMap worldType -- the mapping user input / world
             -> worldType -- the world state
             -> IO ()     -- into IO () for obvious reason
yMainLoop winTitle 
          inputActionMap 
          world = do
  -- The boilerplate
  _ <- getArgsAndInitialize
  initialDisplayMode $= 
      [WithDepthBuffer,DoubleBuffered,RGBMode]
  _ <- createWindow winTitle
  depthFunc  $= Just Less
  windowSize $= Size 500 500
  -- The state variables for the world (I know it feels BAD)
  worldRef <- newIORef world
  -- Action to call when waiting
  idleCallback $= Just idle
  -- the keyboard will update the world
  keyboardMouseCallback $= 
          Just (keyboardMouse inputActionMap worldRef)
  -- We generate one frame using the callback
  displayCallback $= display worldRef
  -- We enter the main loop
  mainLoop

-- When no user input entered do nothing
idle :: IO ()
idle = postRedisplay Nothing

-- Get User Input
-- both cleaner, terser and more expendable than the preceeding code
keyboardMouse :: InputMap a -> IORef a
                 -> Key -> KeyState -> Modifiers -> Position -> IO()
keyboardMouse input world key state _ _ =
    when (state == Down) $
         let 
            charFromKey (Char c) = c
            -- To replace
            charFromKey (SpecialKey _) = '#'
            charFromKey (MouseButton _) = '#'

            transformator = input ! Press (charFromKey key)
         in do
            w <- get world
            world $= transformator w


-- The function that will display datas
display :: (HasGetter g, DisplayableWorld world) => 
           g world -> IO ()
display worldRef = do
    -- BEWARE UGLINESS!!!!
    -- SHOULD NEVER MODIFY worldRef HERE!!!!
    --
    -- I SAID NEVER.
    w <- get worldRef
    -- NO REALLY, NEVER!!!!
    -- If someone write a line starting by
    -- w $= ... Shoot him immediately in the head
    --          and refere to competent authorities
    let cam = camera w
    -- set the background color (dark solarized theme)
    clearColor $= Color4 0 0.1686 0.2117 1
    clear [ColorBuffer,DepthBuffer]
    -- Transformation to change the view
    loadIdentity -- reset any transformation
    -- tranlate
    translate $ toGLVector3 (camPos cam)
    -- zoom
    scale (camZoom cam) (camZoom cam) (camZoom cam)
    -- rotate
    rotate (xpoint (camDir cam)) $ Vector3 1.0 0.0 (0.0::GLfloat)
    rotate (ypoint (camDir cam)) $ Vector3 0.0 1.0 (0.0::GLfloat)
    rotate (zpoint (camDir cam)) $ Vector3 0.0 0.0 (1.0::GLfloat)
    -- Now that all transformation were made
    -- We create the object(s)
    let 
        objs = objects w
    _ <- preservingMatrix $ mapM drawObject objs
    swapBuffers -- refresh screen

-- drawObject :: (YObject obj) => obj -> IO()
drawObject :: YObject -> IO()
drawObject shape = 
  -- We will print Points (not triangles for example) 
  renderPrimitive Triangles $ 
    mapM_ drawPoint (triangles shape unityBox)
  where
    drawPoint p = vertex (toGLVertex3 p)
    unityBox = makeBox (-2,-2,-2) (2,2,2) 0.2 
