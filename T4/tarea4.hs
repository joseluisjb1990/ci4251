import Data.Word
import Control.Exception
import Control.Parallel.Strategies
import Control.DeepSeq

initial :: (Double, Double)
initial = (0, 0)

converge :: (Double,Double) -> Word8
converge = converge' initial 0
  where
    converge' z@(zr, zi) n c@(cr, ci) = 
        if n == 255 || mag z > 2 then
          n
          else 
          converge' ((zr^2 - zi^2) + cr, 2*zr*zi + ci) (n+1) c
    mag (x, y) = sqrt $ (x^2) + (y^2)

step n = 4 / (fromIntegral n)

w = 640
h = 320

mandel w h = let (w', h') = (w-1, h-1)
                 (sr, si) = (step w', step h')
             in buildMatrix w' h' sr si

buildMatrix w h sr si = buildMatrix' w sr si h []
  where
    buildMatrix' w sr si i r =
      let t = buildFile w sr (f si i) 
          s = t : r in
        if i == 0 then 
          s
        else
          buildMatrix' w sr si (i-1) s

buildFile w sr y = buildFile' sr y w []
  where
    buildFile' si y j r =
      let t = (y, f sr j) 
          s = t : r in
        if j == 0 then
          s
        else
          buildFile' sr y (j-1) s

f :: Double -> Word32 -> Double
f s i = -2.0 + s * (fromIntegral i)

deep :: NFData a => a -> a
deep a = deepseq a a

main = do
  evaluate $ deep (map (map converge) (mandel w h) `using` parList rseq)
-- runSimulation w h f t =
--     G.runGraphics $ do
--       window <- G.openWindow t (w, h)
--       G.getKey window
--       G.closeWindow window
