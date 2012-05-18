module YGL (
    Point
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

-- | A 1D point
type Point   = GLfloat 
-- | A Scalar value
type Scalar  = GLfloat
-- | A 3D Point mainly '(x,y,z)'
data Point3D = Point3D { 
      xpoint :: Point
    , ypoint :: Point
    , zpoint :: Point }

class YObject a where
    triangles a :: Box3D -> [Points3D]

type Function3D = Point -> Point -> Maybe Point
instance YObject Function3D
    triangles = flip getObject3DFromShapeFunction

-- | We decalre the input map type we need here
-- | It is our API
type InputMap worldType = Map.Map UserInput (worldType -> worldType)
data UserInput = Press Char | Ctrl Char | Alt Char | CtrlAlt Char

-- | A displayable world 
class DisplayableWorld a where
    camera  :: a -> Camera
    objects :: a -> [YObject]

data Camera = Camera {
          position  :: Point3D
        , direction :: Point3D
        , zoom      :: Scalar
    }



data Camera = Camera {
          xcam :: Point
        , ycam :: Point
        , zcam :: Point
        }
data Object3D = [Point3D]
data Box3D = R {
       minPoint :: Point3D 
       maxPoint :: Point3D
    }
zero3D = Point3D { xpoint = 0, ypoint = 0, zpoint = 0}
one3D = Point3D { xpoint = 1, ypoint = 1, zpoint = 1}
unityBox = { zero3D, one3D }

-- Given a shape function and a delimited Box3D
-- return a list of Triangles to be displayed
getObject3DFromShapeFunction :: Function3D -> Box3D -> [Point3D]
getObject3DFromShapeFunction shape box =
  x <- [xmin..xmax]
  y <- [ymin..ymax]
  let 
    neighbors = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
    -- zs are 3D points with found depth
    zs = map (\(u,v) -> (u,v,shape u v)) neighbors
    -- ps are 3D opengl points + color value
    ps = map (\(u,v,w,c') -> 
        (u/width,v/height,w/depth)) zs
  -- If the point diverged too fast, don't display it
  if (and $ map (\(_,_,z) -> z==Nothing) ts)
  then []
  -- Draw two triangles
  --    3 - 2
  --    | / |
  --    0 - 1
  else [ps!!0,ps!!1,ps!!2,ps!!0,ps!!2,ps!!3]
  where
    -- some naming to make it 
    -- easier to read
    xmin = xpoint minPoint box
    xmax = xpoint maxPoint box
    width = xmax - xmin
    ymin = ypoint minPoint box
    ymay = ypoint mayPoint box
    height = ymax - ymin
    zmin = zpoint minPoint box
    zmaz = zpoint mazPoint box
    depth = zmax - zmin


inputMapFromList = Map.fromList

-- We set our mainLoop function
-- As you can see it is _not_ functional!
-- This can be perceived as BAD.
-- But I wanted to use the imperative GLUT library
-- We have no choice for now.
yMainLoop :: String       -- window name
             -> InputMap  -- the mapping user input / world
             -> worldType -- the world state
             -> (worldType -> ViewState) -- a function from world state to view
             -> IO ()     -- into IO () for obvious reason
yMainLoop windowTitle 
          inputActionMap 
          camera 
          world 
          viewFromWorld = do
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
  displayCallback $= display viewFromWorld worldRef
  -- We enter the main loop
  mainLoop

-- When no user input entered do nothing
idle = postRedisplay Nothing

-- Get User Input
-- both cleaner, terser and more expendable than the preceeding code
keyboardMouse input world state modifiers position =
    if modifiers == Down
    then
         let transformator = input ! (Press state)
         in do
            w <- get world
            world $= (transformator w)
    else return ()


-- The function that will display datas
display worldRef getViewFromWorld = do
    -- BEWAREA UGLINESS!!!!
    -- SHOULD NEVER MODIFY worldRef HERE!!!!
    w <- get worldRef
    let view = getViewFromWorld w
    -- set the background color (dark solarized theme)
    clearColor $= Color4 0 0.1686 0.2117 1
    clear [ColorBuffer,DepthBuffer]
    -- Transformation to change the view
    loadIdentity -- reset any transformation
    -- tranlate
    (x,y) <- get position
    translate $ Vector3 x y 0 
    -- zoom
    z <- get zoom
    scale z z z
    -- rotate
    (xangle,yangle,zangle) <- get angle
    rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
    rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
    rotate zangle $ Vector3 0.0 0.0 (1.0::GLfloat)
    -- Now that all transformation were made
    -- We create the object(s)
    t <- get elapsedTime
    preservingMatrix $ drawObject (triangles t)
    swapBuffers -- refresh screen

-- The function that will display datas
display angle zoom position triangles = do
   -- set the background color (dark solarized theme)
  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ColorBuffer,DepthBuffer]
  -- Transformation to change the view
  loadIdentity -- reset any transformation
  -- tranlate
  (x,y) <- get position
  translate $ Vector3 x y 0 
  -- zoom
  z <- get zoom
  scale z z z
  -- rotate
  (xangle,yangle,zangle) <- get angle
  rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
  rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
  rotate zangle $ Vector3 0.0 0.0 (1.0::GLfloat)
  -- Now that all transformation were made
  -- We create the object(s)
  t <- get elapsedTime
  preservingMatrix $ drawObject (shape w)
  swapBuffers -- refresh screen

drawObject shape = do
  -- We will print Points (not triangles for example) 
  renderPrimitive Triangles $ do
    mapM_ drawPoint (triangles unityBox shape)
  where
      drawPoint (x,y,z) = vertex $ Vertex3 x y z
