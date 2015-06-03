import Data.List
import Data.Functor
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Tree
import Data.Maybe (fromJust)
--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo

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
addOnes = map f
          where f s = Sample (1:(x s)) (y s)

theta :: Hypothesis Double -> Sample Double -> Double
theta h s = foldl' f 0 (zip (c h) (x s))
            where
              f acc (h, s) = acc + h * s

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = let res = foldl' g (0.0, 0) ss
            in f res
            where f (t, m)    = t / (2 * m)
                  g (t, m) s  = (t + ((theta h s) - (y s)) ^ 2, m + 1)

descend :: Double -> Hypothesis Double -> [Sample Double] -> Hypothesis Double
descend alpha h ss = Hypothesis $ reverse $ fst $ foldl' k ([], 0) (c h)
                     where f j            = foldl' (f' j) (0.0, 0) ss
                           f' j (t, m) s  = (t + (((theta h s) - (y s)) * ((x s) !! j)), m + 1)
                           g (t, m)       = t * alpha / m
                           k (t, m) hj    = ((hj - g (f m)):t, m + 1)

gd :: Double -> Hypothesis Double -> [Sample Double] -> [(Integer, Hypothesis Double, Double)]
gd alpha h ss = unfoldr f (0, h, cost h ss') 
                where ss'          = addOnes ss
                      f (n, h', c') = let h'' = descend alpha h' ss' 
                                          c'' = cost h'' ss' in 
                                          if veryClose c'' c' then Nothing else Just ((n, h', c'), (n+1, h'', c''))

getSamples :: String -> [Sample Double]
getSamples s = read s
 
second (_, h, _) = h
 
sr = Sample { x = [1.0, -0.44127, -0.22368], y = undefined }
 
getHypo = second . last . (gd alpha guess) . getSamples

main = interact (\s -> ((show(theta (getHypo s) sr))))

newtype Max a = Max { getMax :: Maybe a }
          deriving (Show)

instance Ord a => Monoid (Max a) where
  mempty                                  = Max Nothing
  mappend (Max (Just n1)) (Max (Just n2)) = Max $ Just $ max n1 n2
  mappend (Max (Nothing)) (Max (Just n2)) = Max $ Just $ n2
  mappend (Max (Just n1)) (Max (Nothing)) = Max $ Just $ n1
  mappend _ _                             = Max Nothing

data Filesystem a = File a | Directory a [ Filesystem a ]
      deriving(Show)

data Breadcrumbs a = WentDown a  [Filesystem a]                (Breadcrumbs a)
                   | WentLeft    [Filesystem a] [Filesystem a] (Breadcrumbs a)
                   | WentRight   [Filesystem a] [Filesystem a] (Breadcrumbs a)
                   | EmptyBreadCrumb
      deriving(Show)

type Zipper a = ( Filesystem a , Breadcrumbs a )

focus :: Filesystem a -> Zipper a
focus fs = (fs, EmptyBreadCrumb)

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Directory y (x:xs), r) = Just (x, WentDown y xs r)
goDown _                       = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (fd, WentDown z (y:ys) r)   = Just (y, WentRight ([fd]) ys  (WentDown z (y:ys) r))
goRight (fd, WentRight ys (z:zs) r) = Just (z, WentRight (fd:ys) zs (WentRight ys (z:zs) r))
goRight (fd, WentLeft  ys (z:zs) r) = Just (z, WentRight (fd:ys) zs (WentLeft  ys (z:zs) r))
goRight _                           = Nothing


goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (fd, WentRight yys@(y:ys) zs r) = Just (y, (WentLeft ys (fd:zs)) (WentRight yys zs r))
goLeft (fd, WentLeft  yys@(y:ys) zs r) = Just (y, (WentLeft ys (fd:zs)) (WentLeft  yys zs r))
goLeft _                               = Nothing

goBack :: Zipper a -> Maybe (Zipper a)
goBack (_, WentRight (y:yz) zs lb) = Just (y, lb)
goBack (_, WentLeft  ys (z:zs) lb) = Just (z, lb)
goBack (fd, WentDown e r lb)       = Just (Directory e (fd:r), lb)
goBack _                           = Nothing

tothetop :: Zipper a -> Zipper a
tothetop (t, EmptyBreadCrumb) = (t, EmptyBreadCrumb)
tothetop z                    = tothetop $ fromJust $ goBack $ z

defocus :: Zipper a -> Filesystem a
defocus (fs, _) = fs

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (File x, bs)         = (File (f x), bs)
modify f (Directory x xs, bs) = (Directory (f x) xs, bs)

f1 = File 1
f2 = File 4
f3 = File 3
f4 = File 1

d1 = Directory 5 [f1, f2]
d2 = Directory 6 [f3, f4]

dr = Directory 7 [d1, d2]
