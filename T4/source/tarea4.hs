import Data.Word
import Control.Exception
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RA
import Control.Monad.Identity
import qualified Control.Monad.Par as PAR
import qualified Control.Parallel.Strategies as ST
import qualified Graphics.HGL as G

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

convergeG :: ((Word32, Word32), (Double,Double)) -> ((Word32, Word32), Word8)
convergeG = converge' initial 0
  where
    converge' z@(zr, zi) n c@((f', c'), (cr, ci)) = 
        if n == 255 || mag z > 2 then
          ((f', c'), n)
          else 
          converge' ((zr^2 - zi^2) + cr, 2*zr*zi + ci) (n+1) c
    mag (x, y) = sqrt $ (x^2) + (y^2)

step n = 4 / (fromIntegral n)

w = 320
h = 240

mandelG w h = let (w', h') = (w-1, h-1)
                  (sr, si) = (step w', step h')
             in buildMatrixG w' h' sr si

buildMatrixG w h sr si = buildMatrix' w sr si h []
  where
    buildMatrix' w sr si i r =
      let t = buildFileG w sr (f si i) i r
      in
        if i == 0 then 
          t
        else
          buildMatrix' w sr si (i-1) t

buildFileG w sr y i r = buildFile' sr y w r i
  where
    buildFile' si y j r i =
      let t = ((j, i), ( f sr j, y))
          s = t : r in
        if j == 0 then
          s
        else
          buildFile' sr y (j-1) s i

mandel w h = let (w', h') = (w-1, h-1)
                 (sr, si) = (step w', step h')
             in buildMatrix w' h' sr si

buildMatrix w h sr si = buildMatrix' w sr si h []
  where
    buildMatrix' w sr si i r =
      let t = buildFile w sr (f si i) r in
        if i == 0 then 
          t
        else
          buildMatrix' w sr si (i-1) t

buildFile w sr y r = buildFile' sr y w r
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

mandelS w h = let (w', h') = (w-1, h-1)
                  (sr, si) = (step w', step h')
             in buildMatrixS w' h' sr si

buildMatrixS w h sr si = buildMatrix' w sr si h []
  where
    buildMatrix' w sr si i r =
      let t = buildFileS w sr (f si i) 
          s = t : r in
        if i == 0 then 
          s
        else
          buildMatrix' w sr si (i-1) s

buildFileS w sr y = buildFile' sr y w []
  where
    buildFile' si y j r =
      let t = (y, f sr j) 
          s = t : r in
        if j == 0 then
          s
        else
          buildFile' sr y (j-1) s

mandelPar :: Word32 -> Word32 -> [Word8]
mandelPar w h = PAR.runPar $ PAR.parMap converge $ mandel w h

mandelREPA :: Word32 -> Word32 -> R.Array R.U R.DIM2 Word8
mandelREPA w h = runIdentity $ R.computeUnboxedP $ R.map converge $ R.fromListUnboxed (R.Z R.:. fromIntegral h R.:. fromIntegral w) (mandel w h)

mandelStrat :: Word32 -> Word32 -> [[Word8]]
mandelStrat w h = map (\xs -> (map converge xs `ST.using` ST.parList ST.rdeepseq)) (mandelS w h) `ST.using` ST.evalList ST.rdeepseq

mandelParG :: Word32 -> Word32 -> [((Word32, Word32), Word8)]
mandelParG w h = PAR.runPar $ PAR.parMap convergeG $ mandelG w h

makeGraphic w ((f', c'), col) = 
    let 
      f = fromIntegral f'
      c = fromIntegral c'
    in
      if (f' >= w) then G.emptyGraphic
      else G.withRGB (G.RGB col col col) (G.line (f,c) (f+1, c))

runSimulation w h t =
    G.runGraphics $ do
      window <- G.openWindow t (fromIntegral w, fromIntegral h)
      
      G.drawInWindow window $ G.overGraphics ( (map (makeGraphic w) (mandelParG w h)) )

      G.getKey window
      G.closeWindow window

--main = 
--    defaultMain [ bench "Strategies" $ whnf (mandelStrat w) h
--                , bench "REPA"       $ whnf (mandelREPA  w) h
--                , bench "Monad Par"  $ whnf (mandelPar   w) h
--                ]

main = runSimulation 1024 768 "Monad Par"
