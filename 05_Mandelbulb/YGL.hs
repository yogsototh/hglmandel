{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module YGL (
    Point
    , DisplayableWorld
    , Camera
    , YObject
    , Scalar
    , Point3D
    , Function3D
    , yMainLoop
    , InputMap
    , inputMapFromList) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Map ((!))
import qualified Data.Map as Map

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

makePoint3D :: (Scalar,Scalar,Scalar) -> Point3D
makePoint3D (x,y,z) = Point3D {xpoint=x, ypoint=y, zpoint=z}

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

makeBox mini maxi resolution = Box3D {
      minPoint = makePoint3D mini
    , maxPoint = makePoint3D maxi
    , resolution = resolution  }

-- | We want to be able to create object with 
-- | many different ways.
-- | We then made a type class.
-- | A type is in the YObject class if we declare
-- | a function triangles which take this type
-- | and a bounded box, and return a list of triangles.
class YObject objectType where
    triangles :: objectType -> Box3D -> [Point3D]

-- | We declare Function3D as f(x,y) -> z
type Function3D = Point -> Point -> Maybe Point
instance YObject Function3D where
    -- | The details of the code somewhere else
    triangles = getObject3DFromShapeFunction

-- | We decalre the input map type we need here
-- | It is our API
type InputMap worldType = Map.Map UserInput (worldType -> worldType)
data UserInput = Press Char | Ctrl Char | Alt Char | CtrlAlt Char 
                 deriving (Eq,Ord,Show,Read)

-- | A displayable world 
class DisplayableWorld a where
    camera :: a -> Camera
    camera _ = defaultCamera 
    lights :: a -> [Light]
    lights _ = []
    objects :: (YObject o) => a -> [o]
    objects _ = []

-- | the Camera type to know how to
-- | Transform the scene to see the right view.
data Camera = Camera {
          camPos  :: Point3D
        , camDir  :: Point3D
        , camZoom :: Scalar }

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
    removeMaybe (u,v,Nothing) = (0,0,0)
    ps = map removeMaybe zs
  -- If the point diverged too fast, don't display it
  if (and $ map (\(_,_,z) -> z==Nothing) zs)
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
    width = xmax - xmin
    ymin = ypoint $ minPoint box
    ymax = ypoint $ maxPoint box
    height = ymax - ymin
    zmin = zpoint $ minPoint box
    zmax = zpoint $ maxPoint box
    depth = zmax - zmin
    res = resolution box

inputMapFromList :: (DisplayableWorld world) => [(UserInput,world -> world)] -> InputMap world
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
yMainLoop windowTitle 
          inputActionMap 
          world = do
  -- The boilerplate
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= 
      [WithDepthBuffer,DoubleBuffered,RGBMode]
  createWindow windowTitle
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
idle = postRedisplay Nothing

-- Get User Input
-- both cleaner, terser and more expendable than the preceeding code
keyboardMouse :: InputMap a -> (IORef a) 
                 -> Key -> KeyState -> Modifiers -> Position -> IO()
keyboardMouse input world key state modifiers position =
    if state == Down
    then
         let 
            charFromKey (Char c) = c
            transformator = input ! (Press (charFromKey key))
         in do
            w <- get world
            world $= (transformator w)
    else return ()


-- The function that will display datas
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
    t <- get elapsedTime
    let 
        objs = objects w
    preservingMatrix $ mapM drawObject objs
    swapBuffers -- refresh screen

drawObject :: (YObject o) => o -> IO ()
drawObject shape = do
  -- We will print Points (not triangles for example) 
  renderPrimitive Triangles $ do
    mapM_ drawPoint (triangles shape unityBox)
  where
    drawPoint p = vertex (toGLVertex3 p)
    unityBox = makeBox ((-2),(-2),(-2)) (2,2,2) 0.2 
