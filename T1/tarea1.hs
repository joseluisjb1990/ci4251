import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data Sample a = Sample { x :: [a], y :: a}
      deriving (Show, Read)

data Hypothesis a = Hypothesis { c :: [a] }
      deriving (Show)

alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }

veryClose :: Double -> Double -> Bool
veryClose v0 v1 = abs (v0 - v1) <= epsilon

addOnes :: [Sample Double] -> [Sample Double]
addOnes = map (\s -> Sample (1:(x s)) (y s))

theta :: Hypothesis Double -> Sample Double -> Double
theta h s  = sum $ map (\(h', s') -> h' * s') (zip (c h) (x s)) 

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = let res = foldl' g (0.0, 0) l
              in f res
            where f (t, m)    = t / (2 * m)
                  g (t, m) t' = (t + t', m + 1)
                  l           =  map f' ss
                  f' s        = ((theta h s) - (y s)) ^ 2

descend :: Double -> Hypothesis Double -> [Sample Double] -> Hypothesis Double
descend alpha h ss = Hypothesis $ reverse $ fst $ foldl' (\(t, m) hj -> ((hj - g (f m)):t, m + 1)) ([], 0) (c h)
                     where f j            = foldl' (f' j) (0.0, 0) ss
                           f' j (t, m) s  = (t + (((theta h s) - (y s)) * ((x s) !! j)), m + 1)
                           g (t, m)       = t * alpha / m

gd :: Double -> Hypothesis Double -> [Sample Double] -> [(Integer, Hypothesis Double, Double)]
gd alpha h ss = unfoldr f (0, h, cost h ss') 
                where ss'          = addOnes ss
                      f (n, h', c') = let h'' = descend alpha h' ss' in 
                                       let c'' = cost h'' ss' in 
                                         if veryClose c'' c' then Nothing else Just ((n, h', c'), (n+1, h'', c''))

-- getSamples :: String -> [Sample Double]
-- getSamples s = read s
-- 
-- second (_, h, _) = h
-- 
-- sr = Sample { x = [1.0, -0.44127, -0.22368], y = undefined }
-- 
-- getHypo = second . last . (gd alpha guess) . getSamples
-- 
-- main = interact (\s -> ((show(theta (getHypo s) sr))))

data Filesystem a = File a | Directory a [ Filesystem a ]

data Breadcrumbs a = WentDown a  [Filesystem a] Breadcrumbs a
                   | WentLeft   ([Filesystem a], [Filesystem a]) Breadcrumbs a
                   | WentRight  ([Filesystem a], [Filesystem a]) Breadcrumbs a
                   | EmptyBreadCrumb

type Zipper a = ( Filesystem a , Breadcrumbs a )

focus :: Filesystem a -> Zipper a
focus fs = (fs, EmptyBreadCrumb)

goDown :: Zipper a -> Maybe Zipper a
goDown (Directory y x:xs, r) = Just (x, WentDown y xs r)
goDown _                     = Nothing

goRight :: Zipper a -> Maybe Zipper a
goRight (fd, WentDown z y:ys r)      = Just (y, WentRight ([fd]   , ys) WentDown z y:ys r)
goRight (fd, WentRight (ys, z:zs) r) = Just (z, WentRight (fd : ys, zs) WentRight (ys, z:zs) r))
goRight (fd, WentLeft  (ys, z:zs) r) = Just (z, WentRight (fd : ys, zs) WentLeft  (ys, z:zs) r))
goRight _                            = Nothing


goLeft :: Zipper a -> Maybe Zipper a
goLeft (fd, WentRight (y:ys, zs r)) = Just (y, WentLeft (ys, fd : zs) WentRight (y:ys, zs r))
goLeft (fd, WentLeft  (y:ys, zs r)) = Just (y, WentLeft (ys, fd : zs) WentLeft  (y:ys, zs r))
goLeft _                            = Nothing