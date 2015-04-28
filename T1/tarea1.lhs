\documentclass[11pt,fleqn]{article}

\usepackage{tikz}
\usepackage{multicol}
\usepackage{latexsym}
\usepackage{array}
\usepackage[english,spanish]{babel}
\usepackage{lmodern}
\usepackage{listings}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}
\usepackage{xcolor}

\usepackage{algorithmic}
\usepackage{algorithm}

\usetikzlibrary{positioning,shapes,folding,positioning,shapes,trees}

\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue
}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}



\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey}
}

\long\def\ignore#1{}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 1}

\author{Jose Luis Jimenez\\
10-10839\\
\href{mailto:jljb1990@gmail.com}{<jljb1990@gmail.com>}}

\date{Abril 30, 2015}

\maketitle

\section{Machine Learning}

A continuacion veremos las definiciones de todas las funciones con una pequenia explicacion de su funcionamiento.

Defininicion de Hypothesis.
\begin{lstlisting}

> data Hypothesis a = Hypothesis { c :: [a] }
>      deriving (Show)

\end{lstlisting}

Definicion de alpha.

\begin{lstlisting}

> alpha :: Double
> alpha = 0.03

\end{lstlisting}

Definicion de epsilon.

\begin{lstlisting}

> epsilon :: Double
> epsilon = 0.0000001

\end{lstlisting}

Definicion de la hipotesis inicial.

\begin{lstlisting}

> guess :: Hypothesis Double
> guess = Hypothesis { c = [0.0, 0.0, 0.0] }

\end{lstlisting}

\subsection{Muestras de Entrenamiento}

Definicion de training.

\begin{lstlisting}

> training :: [Sample Double]

\end{lstlisting}

\subsection{Comparar en punto flotante}

Para hacer comparacion entre numeros en punto flotante obtenemos el valor absoluto de la diferencia entre estos y lo
comparamos con el epsilon. Si la diferencia es menor o igual, entonces son epsilon despreciables.

\begin{lstlisting}

> veryClose :: Double -> Double -> Bool
> veryClose v0 v1 = abs (v0 - v1) <= epsilon

\end{lstlisting}

\subsection{Congruencia dimensional}

Para addOnes utilizamos la funcion map para sustituir cada una de las muestras por su version con un 1 en la primera posicion.

\begin{lstlisting}

> addOnes :: [Sample Double] -> [Sample Double]
> addOnes = map f
>           where f s = Sample (1:(x s)) (y s)

\end{lstlisting}

\subsection{Evaluando Hipótesis}

Para el producto punto entre los dos vectores usamos la funcion zip para unir ambos valores que deseamos multiplicar. Posteriormente multiplicamos y sumamos el total utilzando un foldl.

\begin{lstlisting}

> theta :: Hypothesis Double -> Sample Double -> Double
> theta h s = foldl' f 0 (zip (c h) (x s)) 
>             where
>               f acc (h, s) = acc + h * s

\end{lstlisting}

Para la función cost usaremos un fold cuyo auxiliar g se encarga de calcular y acumular los valores individuales de la sumatoria $$ J(\theta) = \frac{1}{2m} \sum_{i=1}^{m}{(h_\theta(x^{(i)}) - y^{(i)})^2} $$ apoyandose en la función theta. Para esto utiliza una tupla como acumulador en la cual la primera componente es la sumatoria acumulada y la segunda es la cantidad de valores que se han incluido en la sumatoria. Al final se aplica la función f, la cual calcula la división final.

\begin{lstlisting}

> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss = let res = foldl' g (0.0, 0) ss
>             in f res
>             where f (t, m)    = t / (2 * m)
>                   g (t, m) s  = (t + ((theta h s) - (y s)) ^ 2, m + 1)

\end{lstlisting}

\subsection{Bajando por el gradiente}

Para la función descend utilizaremos dos foldl anidados. El foldl general recorre la hipótesis, usando una tupla como acumulador la cual mantiene una lista y un número entero. La lista se utiliza para acumular los valores pertenecientes a la nueva hipótesis mientras que el segundo valor acumula la cantidad de valores en la hipótesis. Para calcular los nuevos valores, se utiliza un nuevo fold que trabaja sobre la lista de muestras aplcando la fórmula.

\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double]
>         -> Hypothesis Double
> descend alpha h ss = Hypothesis $ reverse $ fst $ foldl' (\(t, m) hj -> ((hj - g (f m)):t, m + 1)) ([], 0) (c h)
>                      where f j            = foldl' (f' j) (0.0, 0) ss
>                            f' j (t, m) s  = (t + (((theta h s) - (y s)) * ((x s) !! j)), m + 1)
>                            g (t, m)       = t * alpha / m

\end{lstlisting}

\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss = unfoldr f (0, h, cost h ss') 
>                 where ss'           = addOnes ss
>                       f (n, h', c') = let h'' = descend alpha h' ss' in 
>                                         let c'' = cost h'' ss' in 
>                                           if veryClose c'' c' then Nothing else Just ((n, h', c'), (n+1, h'', c''))

\end{lstlisting}

\section{Monoids}

> newtype Max a = Max { getMax :: Maybe a }
>           deriving (Show)

> instance Ord a => Monoid (Max a) where
>   mempty                                  = Max Nothing
>   mappend (Max (Just n1)) (Max (Just n2)) = Max $ Just $ max n1 n2
>   mappend (Max (Nothing)) (Max (Just n2)) = Max $ Just $ n2
>   mappend (Max (Just n1)) (Max (Nothing)) = Max $ Just $ n1
>   mappend _ _                             = Max Nothing

\section{Zippers}

> data Filesystem a = File a | Directory a [Filesystem a]
>
> data Breadcrumbs a = WentDown a  [Filesystem a]                (Breadcrumbs a)
                     | WentLeft    [Filesystem a] [Filesystem a] (Breadcrumbs a)
                     | WentRight   [Filesystem a] [Filesystem a] (Breadcrumbs a)
                     | EmptyBreadCrumb
        deriving(Show)
>
> type Zipper a = (Filesystem a, Breadcrumbs a)
>
> goDown :: Zipper a -> Maybe (Zipper a)
> goDown (Directory y (x:xs), r) = Just (x, WentDown y xs r)
> goDown _                       = Nothing

> goRight :: Zipper a -> Maybe (Zipper a)
> goRight (fd, WentDown z (y:ys) r)   = Just (y, WentRight ([fd]) ys  (WentDown z (y:ys) r))
> goRight (fd, WentRight ys (z:zs) r) = Just (z, WentRight (fd:ys) zs (WentRight ys (z:zs) r))
> goRight (fd, WentLeft  ys (z:zs) r) = Just (z, WentRight (fd:ys) zs (WentLeft  ys (z:zs) r))
> goRight _                           = Nothing

> goLeft :: Zipper a -> Maybe (Zipper a)
> goLeft (fd, WentRight yys@(y:ys) zs r) = Just (y, (WentLeft ys (fd:zs)) (WentRight yys zs r))
> goLeft (fd, WentLeft  yys@(y:ys) zs r) = Just (y, (WentLeft ys (fd:zs)) (WentLeft  yys zs r))
> goLeft _                               = Nothing

> goBack :: Zipper a -> Maybe (Zipper a)
> goBack (_, WentRight (y:yz) zs lb) = Just (y, lb)
> goBack (_, WentLeft  ys (z:zs) lb) = Just (z, lb)
> goBack (fd, WentDown e r lb)       = Just (Directory e (fd:r), lb)
> goBack _                           = Nothing

> tothetop :: 
> modify   ::
>
> focus :: Filesystem a -> Zipper a
> focus fs = (fs, EmptyBreadCrumb)

> defocus :: Zipper a -> Filesystem a
> defocus (fs, _) = fs

\pagebreak