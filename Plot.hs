{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{- | Example implementation for displaying signals

-}
module Plot(
      StyledSignal
    , SignalStyle(..) 
    , PlotStyle(..)
    , AnySignal(..)
    , defaultPlotStyle
    , defaultSignalStyle
    , discreteSignalsWithStyle
    , signalsWithStyle
    , display
    , drawStringLabel
    , LabelStyle(..)
    , Pair(..)
    , Mono(..)
    , Resource(..)
    , runPixmap
    , Pixmap(..)
    , Canvas(..)
    , PlotCoordinates(..)
    , PictureCoordinates(..)
    , CoordinateMapping(..)
    ) where 

import Graphics.PDF
import Displayable 
import Viewer
import Text.Printf
import qualified Graphics.PDF as PDF(Orientation(..))
import Control.Monad(when) 
import Data.Maybe(isJust,fromJust)
import Signal
import Fixed(HasDoubleRepresentation(..))
import MultiRate
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed.Mutable as M 
import qualified Data.Vector.Unboxed as U 
import Control.Monad.ST 
import Control.Monad.Primitive
import Data.Word
import Data.Bits
import Data.List(sortBy,unfoldr,sort)
import Data.Function(on)
import Data.STRef

import Debug.Trace
debug a = trace (show a) a

maximumPoints = 800

-- | A list fo signals wth style information for the plot and the signals
-- The signals are using the same units
data StyledSignal a b = StyledSignal Bool [a] [[b]] (PlotStyle a b)

-- | Style for a signal (only color used in this version)
data SignalStyle = SignalStyle {
                  signalColor :: !Color 
                , signalWidth :: !Double
                , signalOpacity :: !Double
}

-- | Style for a label
data LabelStyle = LabelStyle !Int !Justification !(PDF.Orientation) 


{-

Default styles

-}
hUnitStyle = LabelStyle 7 LeftJustification PDF.E
vUnitStyle = LabelStyle 7 Centered PDF.S

hTickStyle = LabelStyle 7 Centered PDF.N
vTickStyle = LabelStyle 7 RightJustification PDF.W

titleStyle = LabelStyle 14 Centered PDF.N

-- | Draw a string value with style and wrapping
drawStringLabel :: LabelStyle 
                -> String 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> Draw () 
drawStringLabel (LabelStyle fs j o) s x y w h = do
  let (r,b) = drawTextBox x y w h o NormalParagraph (Font (PDFFont Times_Roman fs) black black) $ do
                setJustification j
                paragraph $ do
                    txt $ s
  b

-- | Default style for a signal
defaultSignalStyle :: Double -> Color -> SignalStyle 
defaultSignalStyle opacity color = SignalStyle color 1.0 opacity

type PictureCoordinates a b = (a,b) -> Point 
type PlotCoordinates a b = Point -> (a,b)
type CoordinateMapping a b = (PictureCoordinates a b, PlotCoordinates a b)

-- | Style for a plot
data PlotStyle a b = PlotStyle {
                       title :: Maybe String 
                     , leftMargin :: !Double 
                     , rightMargin :: !Double 
                     , topMargin :: !Double 
                     , bottomMargin :: !Double 
                     , horizontalTickValues :: Double -> Double -> [Double]
                     , verticalTickValues :: Double -> Double -> [Double] 
                     , horizontalTickRepresentation :: Double -> String
                     , verticalTickRepresentation :: Double -> String
                     , horizontalLabel :: Maybe String 
                     , verticalLabel:: Maybe String
                     , prologRsrc :: Int -> Int -> CoordinateMapping a b -> PDF (Maybe (PDFReference RawImage))
                     , prolog :: Maybe (PDFReference RawImage) -> CoordinateMapping a b -> Draw ()
                     , epilog :: CoordinateMapping a b -> Draw () 
                     , signalStyles :: [SignalStyle]
                     , axis :: !Bool
                     , interpolation :: !Bool
                     , horizontalBounds :: Maybe (Double,Double)
                     , verticalBounds :: Maybe (Double,Double)
}

-- | Default ticks values in [ma,mb]
tenTicks :: Double -> Double -> [Double]
tenTicks ma mb = map (\t -> (fromIntegral t)/(fromIntegral 10)*(mb-ma) + ma) ([0..10] :: [Int])

-- | Formatting function for floats
simpleFloat :: HasDoubleRepresentation a => a -> String
simpleFloat a = 
    let s = printf "%1.2f" (toDouble a) 
    in 
    s

-- | Default style for plots
defaultPlotStyle :: PlotStyle a b 
defaultPlotStyle = 
    PlotStyle {
                title = Nothing 
              , leftMargin = 50 
              , rightMargin = 50 
              , topMargin = 50 
              , bottomMargin = 20 
              , horizontalTickValues = tenTicks 
              , verticalTickValues = tenTicks 
              , horizontalTickRepresentation = simpleFloat
              , verticalTickRepresentation = simpleFloat
              , horizontalLabel = Just "s"
              , verticalLabel = Just "Energy"
              , prologRsrc = \_ -> \_ -> \_ -> return Nothing
              , prolog = \_ -> \_ -> return ()
              , epilog = const (return ()) 
              , signalStyles = repeat (defaultSignalStyle 1.0 (Rgb 0.6 0.6 1.0))
              , axis = True
              , interpolation = True
              , horizontalBounds = Nothing 
              , verticalBounds = Nothing
              }

-- | Create a plot description with signals and a plot style
signalsWithStyle :: Bool -> [a] -> [[b]] -> PlotStyle a b -> StyledSignal a b 
signalsWithStyle c times signals style = StyledSignal c times signals style

data AnySignal t = forall b. HasDoubleRepresentation b => AS (Signal t b) 

-- | Create a plot description with discrete signals and a plot style
discreteSignalsWithStyle :: HasDoubleRepresentation t 
                         => Int
                         -> PlotStyle Double Double 
                         -> Signal t t
                         -> [AnySignal t] 
                         -> StyledSignal Double Double
discreteSignalsWithStyle nbPoints style timeSignal signals1  = 
    let theTimes = {-# SCC "InternalTimes" #-} take nbPointsToDraw . map toDouble . getSamples $ timeSignal
        reduce = (nbPoints `quot` maximumPoints) - 1 
        nbPointsToDraw = nbPoints 
        convertSignal :: AnySignal t -> Signal t Double
        convertSignal (AS s) = mapS toDouble s        
        timedSignal s = take nbPointsToDraw . getSamples . convertSignal $ s
        theCurves :: [[Double]]
        theCurves = {-# SCC theCurves #-} map timedSignal signals1
        --complex = True
        complex = reduce >= 0
    in 
    signalsWithStyle complex theTimes theCurves style

type Drawing a = (Int -> Int -> PDF a, a -> Int -> Int -> Draw ())
data Pair a = Pair (Drawing a) (Drawing a) (Draw () -> Draw () -> Draw ())
data Mono a = Mono (Drawing a) (Draw () -> Draw ())

instance Displayable (Pair a) (a,a) where 
   drawing (Pair pa pb c) = 
    let (ra, da) = pa
        (rb, db) = pb 
        sep = 20.0 :: Double
        r = \w -> \h -> do 
            let h' = floor $ ((fromIntegral h - sep) / 2.0)
            a <- ra w h'
            b <- rb w h'
            return (a,b)
        action = \(a,b) -> \w -> \h -> do
           let h' = floor ((fromIntegral h - sep) / 2.0)
           c (da a w h') (db b w h') 
    in 
    (r,action)

instance Displayable (Mono a) a where 
    drawing (Mono pa c) = 
        let (ra,da) = pa 
            action = \a -> \w -> \h -> c (da a w h)
        in
        (ra, action)

class Monad m => Canvas m where 
    moveTo :: Point -> m ()
    lineTo :: Point -> m () 
    setColor :: Color -> Double -> m ()
    resetAlpha :: m ()
    drawPath :: m ()
    pixel :: Point -> m ()

instance Canvas Draw where 
    moveTo = beginPath 
    lineTo = addLineToPath 
    pixel p = do
      beginPath p 
      addLineToPath p
    setColor c a = do 
        strokeColor c
        setStrokeAlpha a 
    resetAlpha = setStrokeAlpha 1.0 
    drawPath = strokePath

instance Functor Complex where 
    fmap f (a :+ b) = f a :+ f b

ppixel :: Point -> PState -> STRef s (M.MVector s Word32) -> ST s (STRef s (M.MVector s Word32))
ppixel pa@(xa :+ ya) p mvref = do 
  mv <- readSTRef mvref
  let maxLen = w p * h p - 1
      Rgb r g b = currentColor p
      alpha = currentAlpha p
      ri,gi,bi :: Word32
      ri = (floor $ r * 255) .&. 0x0FF 
      gi = (floor $ g * 255) .&. 0x0FF 
      bi = (floor $ b * 255) .&. 0x0FF 
      c :: Word32
      c = (ri `shift` 16) .|. (gi `shift` 8) .|. bi
      wi = w p 
      he = h p
      plotPoint :: M.MVector s Word32 -> (Complex Int) -> ST s ()
      plotPoint !a (x :+ y) | pos >= 0 && pos < maxLen = M.write a pos c
                            | otherwise = return ()
         where 
             pos = wi * (he - 1 - y) + x
  plotPoint mv (floor xa :+ floor ya)
  return mvref


drawLine ::  Point -> PState -> STRef s (M.MVector s Word32) -> ST s (STRef s (M.MVector s Word32))
drawLine pb@(xb :+ yb) p  mvref = do 
    mv <- readSTRef mvref
    let maxLen = w p * h p - 1
        pa@(xa :+ ya) = currentPos p
        Rgb r g b = currentColor p
        alpha = currentAlpha p
        ri,gi,bi :: Word32
        ri = (floor $ r * 255) .&. 0x0FF 
        gi = (floor $ g * 255) .&. 0x0FF 
        bi = (floor $ b * 255) .&. 0x0FF 
        c :: Word32
        c = (ri `shift` 16) .|. (gi `shift` 8) .|. bi
        wi = w p 
        he = h p
        vertical = floor xb == floor xa 
        horizontal = floor yb == floor ya
        {-# INLINE plotPoint #-}
        plotPoint :: M.MVector s Word32 -> (Complex Int) -> ST s ()
        plotPoint !a (x :+ y) | pos >= 0 && pos < maxLen = do 
                                  old <- M.read a pos
                                  let new = combineColor old
                                  M.write a pos new
                              | otherwise = return ()
           where 
               pos = wi * (he - 1 - y) + x

        {-# INLINE combineColor #-}
        combineColor :: Word32 -> Word32
        combineColor old = c
            --let oldr = (old `shiftR` 16) .&. 0x0FF
            --    oldg = (old `shiftR` 8) .&. 0x0FF
            --    oldb = old  .&. 0x0FF
            --    newr = (floor $ fromIntegral oldr*(1-alpha) + fromIntegral ri*alpha) .&. 0x0FF
            --    newg = (floor $ fromIntegral oldg*(1-alpha) + fromIntegral gi*alpha) .&. 0x0FF
            --    newb = (floor $ fromIntegral oldb*(1-alpha) + fromIntegral bi*alpha) .&. 0x0FF
            --    new = (newr `shift` 16) .|. (newg `shift` 8) .|. newb
            --in 
            --new
        verticalLine :: M.MVector s Word32 -> Int -> Int -> Int -> ST s () 
        verticalLine !a x yi yj =
            let [y0,y1] = sort [yi,yj]
                start | y0 >= 0 = y0 
                      | otherwise = 0 
                end | y1 < he = y1
                    | otherwise = he - 1
                pts  = [start, start+1..end]
                pos = wi*(he - 1 -start)+x 
                allPos | pos < maxLen = [pos, pos - wi .. 0]
                       | otherwise = [maxLen-1,maxLen-1-wi .. 0]
                addPoint (p,_) = do 
                  old <- M.read a p 
                  let new = combineColor old
                  M.write a p new
            in 
            mapM_ addPoint (zip allPos pts)   

        horizontalLine :: M.MVector s Word32 -> Int -> Int -> Int -> ST s () 
        horizontalLine !a y0 xi xj =
            let [x0,x1] = sort [xi,xj]
                start | x0 >= 0 = x0 
                      | otherwise = 0 
                end | x1 < wi = x1
                    | otherwise = wi - 1
                pts  = [start, start+1..end]
                pos = wi*(he - 1 - y)  +  start
                y | y0 == he = he-1 
                  | otherwise = y0
                allPos | pos >=0 = [pos, pos + 1 .. maxLen - 1]
                       | otherwise = [0,1 .. maxLen-1]
                addPoint (p,_) = do
                  old <- M.read a p 
                  let new = combineColor old
                  M.write a p new
            in 
            mapM_ addPoint (zip allPos pts)        


        reversal thep@(x :+ y) | floor xb == floor xa = thep
                               | floor yb == floor ya = thep
                               | steep > 1.0 = (y :+ x) 
                               | otherwise = thep
               where 
                   steep = abs ((yb - ya) / (xb - xa))

        line s e = 
            let f x = floor $! x
                [s',e'] = sortBy (compare `on` realPart) [s,e]
                x0 = f (realPart s')
                y0 = f (imagPart s')
                x1 = f (realPart e')
                y1 = f (imagPart e')
                dx = x1 - x0  :: Int
                dy = y1 - y0 :: Int
                sy = signum dy
                start = x0 :+ y0
            in
            if dx == 0
               then 
                   if dy == 0 
                       then [start]
                       else let vertical thep@(x :+ y) | sy > 0 && y > y1 = Nothing
                                                       | sy < 0 && y < y1 = Nothing 
                                                       | otherwise = Just (thep, x :+ (y+sy))
                            in
                            start:unfoldr vertical start 
               else 
                   let delta = fromIntegral (abs dy) / fromIntegral dx :: Double
                       step (c@(cx :+ cy),err) | cx > x1 = Nothing
                                               | newErr > 0.5 = Just (c , ((cx+1) :+ (cy+sy) , newErr - 1.0) )
                                               | otherwise = Just (c , ((cx +1) :+ cy, newErr))
                         where 
                          newErr = err + delta
                   in 
                   start:unfoldr step (start,0.0)
           
    let action | vertical =  verticalLine mv (floor xa) (floor ya) (floor yb)
               | horizontal =  horizontalLine mv (floor ya) (floor xa) (floor xb)
               | otherwise = mapM_ (plotPoint mv . reversal)  $ line (reversal pa) (reversal pb)
    action
    return mvref
   
data PState   =          PState {  currentColor :: !Color 
                                 , currentAlpha :: !Double 
                                 , currentPos :: !Point
                                 , w :: !Int 
                                 , h :: !Int
                                 , pixels :: forall s. ST s (STRef s (M.MVector s Word32)) 
                                 } 

data Pixmap  a = Pixmap {getPixmap :: State (PState ) a} 

instance Monad (Pixmap )  where 
    return = Pixmap . return 
    (Pixmap a) >>= f = Pixmap (a >>= getPixmap . f) 

instance MonadState (PState ) (Pixmap ) where 
    get = Pixmap get 
    put s = Pixmap (put s)

instance Canvas (Pixmap ) where 
    moveTo p = modify $! \s -> 
       s {currentPos = p}
    lineTo p = modify $! \s -> 
       let newPixel = pixels s >>= drawLine p s
       in
       s {currentPos = p, pixels = newPixel}
    pixel p = modify $! \s -> 
       let newPixel = pixels s >>= ppixel p s
       in
       s {currentPos = p, pixels = newPixel}
    setColor c a = modify $! \s -> 
       s {currentColor = c, currentAlpha = a}
    drawPath = return ()
    resetAlpha = modify $! \s -> 
      s {currentAlpha = 1.0}

runPixmap :: Int -> Int -> Pixmap () -> PDF (PDFReference RawImage)  
runPixmap width height p = 
    let finalState = execState (getPixmap p) $ (PState (Rgb 0.0 0.0 0.0) 1.0 (0.0 :+ 0.0) width height startState)
        
        startState :: ST s (STRef s (M.MVector s Word32)) 
        startState = do
          v <- M.replicate  (width*height) 0x00FFFFFF 
          newSTRef v
          
        result p = runST $ do 
           refmv <- pixels p
           mv <- readSTRef refmv
           U.freeze mv
    in 
    createPDFRawImage (fromIntegral width) (fromIntegral height) False $ (result finalState)

segmentedDraw :: Canvas c 
              => ((a,b) -> Point) 
              -> (a,b) 
              -> [(a,b)] 
              -> c ()
segmentedDraw pt h (n:l) = do 
                let (ha :+ hb) = pt h 
                    (na :+ nb) = pt n 
                lineTo (na :+ hb) 
                lineTo (na :+ nb) 
                segmentedDraw pt n l
segmentedDraw _ h [] = return ()

getPath :: Canvas c 
        => PlotStyle a b
        -> ((a,b) -> Point) 
        -> [(a,b)] 
        -> c ()
getPath s pt l = do 
    moveTo (pt . head $ l) 
    if (interpolation s)
        then do              
            mapM_ (lineTo . pt) (tail l)
        else do 
            segmentedDraw pt (head l) (tail l)

simplifySignal:: (HasDoubleRepresentation a, Ord b) 
              => ((a,b) -> Point)
              -> ([(a,b)],SignalStyle) 
              -> ([(a,b)],SignalStyle) 
simplifySignal pt (l,sty) = (simpl s0 v0 v0 (tail l), sty) 
 where 
  test (c,x) (a,y) = 
    let (ta :+ _) = pt (c,x) 
        (tb :+ _) = pt (a,y) 
    in 
    floor ta == floor tb

  getNew l = let (s,v) = head l 
             in 
             (s, v)
  (s0,v0) = getNew l
  simpl !c !mi !ma [] = (c,mi):(c,ma):[]
  simpl !c !mi !ma ((a,b):l) | test (c,mi) (a,b) = simpl c (min mi b) (max ma b) l 
                          | otherwise = 
                            if null l 
                               then (c,mi):(c,ma):[] 
                               else let (s,v) = getNew l 
                                    in 
                                    (c,mi):(c,ma):simpl s v v l

drawSignal :: (Canvas c, Show b, Show a) 
           => PlotStyle a b 
           -> ((a,b) -> Point)
           -> ([(a,b)],SignalStyle) 
           -> c ()
drawSignal s pt (l,signalstyle) = do 
    setColor (signalColor signalstyle) (signalOpacity signalstyle)
    getPath s pt l
    drawPath
    resetAlpha

data Resource = R { plotPixmap :: Maybe  (PDFReference RawImage)
                  , prologPixmap :: Maybe (PDFReference RawImage)
                  }

-- | A plot description is Displayable
instance (Show b, Show a, Ord a, Ord b, HasDoubleRepresentation a, HasDoubleRepresentation b) =>  Displayable (StyledSignal a b) Resource where 
    drawing (StyledSignal complex theTimes signals s) = 
        let (ta,tb) = maybe ( minimum . map toDouble $ theTimes 
                            , maximum . map toDouble $ theTimes) id (horizontalBounds s)
            (ya,yb) = maybe ( minimum . map (minimum . map toDouble ) $ signals 
                            , maximum . map (maximum . map toDouble ) $ signals) id (verticalBounds s) 
            timed = map (zip theTimes)
            action myRsrc wi hi = do
              let width = fromIntegral wi 
                  height = fromIntegral hi
                  tickSize = 6
                  tickLabelSep = 5
                  hUnitSep = 5
                  vUnitSep = 15
                  titleSep = 5
                  
                  h a = (toDouble a - ta) / (tb - ta)*(width - leftMargin s - rightMargin s) + leftMargin s 
                  v b = (toDouble b - ya) / (yb - ya)*(height - topMargin s - bottomMargin s) + bottomMargin s
                  pt (a,b) = (h a) :+ (v b)
                  iw a = fromDouble $ (toDouble a - leftMargin s) / rw * (tb - ta) + ta 
                    where 
                      rw = width - leftMargin s - rightMargin s
                  ih a = fromDouble $ (toDouble a - bottomMargin s) / rh * (yb - ya) + ya
                    where 
                      rh = height - topMargin s - bottomMargin s
                  ippt (a :+ b) = (iw a,ih b)
                  drawVTick x y = do 
                      let (a :+ b) = pt (x,y) 
                      stroke $ Line (a - tickSize) b a b
                      drawStringLabel vTickStyle ((verticalTickRepresentation s) y) 
                           (a - tickSize - tickLabelSep) b (leftMargin s) (bottomMargin s) 
                  drawHTick y x = do 
                      let (a :+ b) = pt (x,y) 
                      stroke $ Line a b a (b - tickSize)
                      drawStringLabel hTickStyle ((horizontalTickRepresentation s) x) 
                          a (b - tickSize - tickLabelSep) (leftMargin s) (bottomMargin s) 
                  drawYAxis x = do 
                      strokeColor black 
                      let (sa :+ sb) = pt (x,ya)  
                          (_ :+ eb) = pt (x,yb)
                      stroke $ Line sa sb sa eb
                      mapM_ (drawVTick x) (filter (\y -> y >= ya && y <= yb) $ (verticalTickValues s) ya yb)
                  drawXAxis y = do 
                      strokeColor black 
                      let (sa :+ sb) = pt (ta,y) 
                          (ea :+ _) = pt (tb,y) 
                      stroke $ Line sa sb ea sb
                      mapM_ (drawHTick y) (filter (\t -> t >= ta && t <= tb) $ (horizontalTickValues s) ta tb)
                  drawHLabel _ Nothing = return () 
                  drawHLabel y (Just label) = do 
                          let b = v y
                          drawStringLabel hUnitStyle label (width - rightMargin s + hUnitSep) b  (rightMargin s - hUnitSep) (bottomMargin s) 
                  drawYLabel _ Nothing = return () 
                  drawYLabel x (Just label) = do
                          let a = h x 
                          drawStringLabel vUnitStyle label a (height - topMargin s + vUnitSep) (leftMargin s) (topMargin s - vUnitSep)
              withNewContext $ do
                  addShape $ Rectangle (leftMargin s :+ bottomMargin s) ((width - rightMargin s) :+ (height - topMargin s))
                  setAsClipPath
                  let R imgR roR = myRsrc
                  (prolog s) roR (pt,ippt)
  
                  case imgR of 
                      Nothing  -> mapM_ (drawSignal s pt) (zip (timed signals) (cycle $ signalStyles s))
                      (Just imgref)  -> do
                          withNewContext $ do
                                  applyMatrix $ translate (leftMargin s :+ bottomMargin s)
                                  drawXObject imgref
              if (axis s) 
                  then do 
                      let xaxis = if ta <=0 && tb >=0 then 0 else ta 
                          yaxis = if ya <=0 && yb >=0 then 0 else ya 
                      drawXAxis yaxis 
                      drawYAxis xaxis 
                      drawHLabel yaxis (horizontalLabel s)
                      drawYLabel xaxis (verticalLabel s)
                  else do
                      let xaxis = ta 
                          yaxis = ya
                      drawXAxis yaxis 
                      drawYAxis xaxis
                      drawHLabel yaxis (horizontalLabel s)
                      drawYLabel xaxis (verticalLabel s)
              when (isJust (title s)) $ do 
                  let t = fromJust (title s)
                  drawStringLabel titleStyle t (width / 2.0) (height - titleSep) width (topMargin s)
              withNewContext $ do
                  addShape $ Rectangle (leftMargin s :+ bottomMargin s) ((width - rightMargin s) :+ (height - topMargin s))
                  setAsClipPath
                  (epilog s) (pt,ippt)
              
        in
        let rsrc wi hi | complex = do 
                             let rw = fromIntegral wi - leftMargin s - rightMargin s 
                                 rh = fromIntegral hi - topMargin s - bottomMargin s 
                                 h a = (toDouble a - ta) / (tb - ta)*rw
                                 v b = (toDouble b - ya) / (yb - ya)*rh
                                 ppt (a,b) = h a  :+ v b
                                 iw a = fromDouble $ toDouble a /  rw * (tb - ta) + ta 
                                 ih a = fromDouble $ toDouble a /  rh * (yb - ya) + ya
                                 ippt (a :+ b) = (iw a,ih b)
                             pr <- (prologRsrc s) wi hi (ppt,ippt)
                             r <- runPixmap (floor rw) (floor rh) $ do
                                       mapM_ (drawSignal s ppt . simplifySignal ppt) $ (zip (timed signals) (cycle $ signalStyles s))
                                       --setColor (Rgb 1.0 0 0) 1.0 
                                       --moveTo (0 :+ 0)
                                       --lineTo (100 :+ 100)
                                       --setColor (Rgb 0 0 1.0) 1.0 
                                       --moveTo (100 :+ 200)
                                       --lineTo (200 :+ 200)
                                       --moveTo (200 :+ 100)
                                       --lineTo (100 :+ 100)
                                       
                             return $! R (Just $! r) $! pr
                       | otherwise = do 
                           let rw = fromIntegral wi - leftMargin s - rightMargin s 
                               rh = fromIntegral hi - topMargin s - bottomMargin s 
                               h a = (toDouble a - ta) / (tb - ta)*rw
                               v b = (toDouble b - ya) / (yb - ya)*rh
                               ppt (a,b) = h a  :+ v b
                               iw a = fromDouble $ toDouble a /  rw * (tb - ta) + ta 
                               ih a = fromDouble $ toDouble a /  rh * (yb - ya) + ya
                               ippt (a :+ b) = (iw a,ih b)
                           pr <- (prologRsrc s) wi hi (ppt,ippt)
                           return (R Nothing $! pr)
        in
        (rsrc, action)

