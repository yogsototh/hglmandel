module YGL (GLfloat,yMainLoop,InputMap,inputMapFromList) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Map ((!))
import qualified Data.Map as Map

-- We decalre the input map type we need here
-- It is our API
type InputMap worldType = Map.Map UserInput (worldType -> worldType)
data UserInput = Press Char | Ctrl Char | Alt Char | CtrlAlt Char

inputMapFromList = Map.fromList

-- We set our mainLoop function
-- As you can see it is _not_ functional!
-- This can be perceived as BAD.
-- But I wanted to use the imperative GLUT library
-- We have no choice for now.
yMainLoop :: String -> InputMap  -> worldType -> IO ()
yMainLoop windowTitle inputActionMap world = do
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

idle = postRedisplay Nothing

-- Get User Input
keyboardMouse input world state modifiers position =
    if modifiers == Down
    then
         let transformator = input ! (Press state)
         in do
            w <- get world
            world $= (transformator w)
    else return ()


-- The function that will display datas
display world = do
  w <- get world
   -- set the background color (dark solarized theme)
  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ColorBuffer,DepthBuffer]
  -- Transformation to change the view
  loadIdentity -- reset any transformation
  -- tranlate
  let (x,y,z) = position w
      s = zoom w
      (xangle,yangle,zangle) = angle w
  translate $ Vector3 x y z 
  scale s s s
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

red   (r,_,_) = r
green (_,g,_) = g
blue  (_,_,b) = b

drawObject shape = do
  -- We will print Points (not triangles for example) 
  renderPrimitive Triangles $ do
    mapM_ drawColoredPoint triangles
  where
      drawColoredPoint (x,y,z) = vertex $ Vertex3 x y z
      triangles = allPoints

depthPoints :: Scalar -> Scalar -> Function3D -> [ColoredPoint]
depthPoints width height shape = do
  x <- [-width..width]
  y <- [0..height]
  let 
    neighbors = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]
    -- zs are 3D points with found depth
    zs = map (\(u,v) -> (u,v,shape u v)) neighbors
    -- ts are 3D pixels + mandel value
    ts = map (\(u,v,w) -> (u,v,w,ymandel u v (w+1))) zs
    -- ps are 3D opengl points + color value
    ps = map (\(u,v,w,c') -> 
        (u/width,v/height,w/depth,colorFromValue c')) ts
  -- If the point diverged too fast, don't display it
  if (and $ map (\(_,_,_,c) -> c>=57) ts)
  then []
  -- Draw two triangles
  else [ps!!0,ps!!1,ps!!2,ps!!0,ps!!2,ps!!3]

allPoints :: [ColoredPoint]
allPoints = planPoints ++ map inverseDepth  planPoints
  where 
      planPoints = depthPoints ++ map inverseHeight depthPoints
      inverseHeight (x,y,z,c) = (x,-y,z,c)
      inverseDepth (x,y,z,c) = (x,y,-z+1/depth,c)
