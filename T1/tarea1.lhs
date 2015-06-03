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

\author{José Luis Jiménez\\
10-10839\\
\href{mailto:jljb1990@gmail.com}{<jljb1990@gmail.com>}}

\date{Mayo 1, 2015}

\maketitle

\pagebreak

\section{Definiciones Generales}

\begin{lstlisting}
> import Data.List
> import Data.Functor
> import Data.Monoid
> import Data.Foldable (foldMap)
> import Data.Tree
> import Data.Maybe
\end{lstlisting}

\section{Machine Learning}

A continuación daremos las definiciones de todas las funciones con una pequeña explicación de su funcionamiento. \hfill \break 

Definición de Hypothesis.

\begin{lstlisting}
> data Hypothesis a = Hypothesis { c :: [a] }
>      deriving (Show)
\end{lstlisting}

Definición de Sample.

\begin{lstlisting}
> data Sample a = Sample { x :: [a], y :: a }
>      deriving (Show, Read)
\end{lstlisting}

Definición de alpha.

\begin{lstlisting}
> alpha :: Double
> alpha = 0.03
\end{lstlisting}

Definición de epsilon.

\begin{lstlisting}
> epsilon :: Double
> epsilon = 0.0000001
\end{lstlisting}

Definición de la hipótesis inicial.

\begin{lstlisting}
> guess :: Hypothesis Double
> guess = Hypothesis { c = [0.0, 0.0, 0.0] }
\end{lstlisting}

\subsection{Comparar en punto flotante}

Para hacer comparación entre números en punto flotante obtenemos el valor absoluto de la diferencia entre estos y lo
comparamos con el epsilon. Si la diferencia es menor o igual, entonces son epsilon despreciables.

\begin{lstlisting}
> veryClose :: Double -> Double -> Bool
> veryClose v0 v1 = abs (v0 - v1) <= epsilon
\end{lstlisting}

\subsection{Congruencia dimensional}

Para la función \texttt{addOnes} utilizamos la función map para sustituir cada una de las muestras
por su versión con un valor \texttt{1} en la primera posición.

\begin{lstlisting}
> addOnes :: [Sample Double] -> [Sample Double]
> addOnes = map f
>           where f s = Sample (1:(x s)) (y s)
\end{lstlisting}

\subsection{Evaluando Hipótesis}

Para el producto punto entre los dos vectores usamos la función zip para unir cada uno de los valores que deseamos multiplicar.
Posteriormente multiplicamos por posición y sumamos el total utilizando un fold.

\begin{lstlisting}
> theta :: Hypothesis Double -> Sample Double -> Double
> theta h s = foldl' f 0 (zip (c h) (x s)) 
>             where
>               f acc (h, s) = acc + h * s
\end{lstlisting}

Para la función cost usaremos un fold cuyo auxiliar \texttt{g} se encarga de calcular y acumular los valores individuales
de la sumatoria $$ \sum_{i=1}^{m}{(h_\theta(x^{(i)}) - y^{(i)})^2} $$ apoyandose en la función theta. Para esto utiliza
una tupla como acumulador en la cual la primera componente es la sumatoria acumulada y la segunda es la cantidad de valores que están incluidos en la sumatoria.
Al final se aplica la función f, la cual calcula la división final.

\begin{lstlisting}
> cost :: Hypothesis Double -> [Sample Double] -> Double
> cost h ss = let res = foldl' g (0.0, 0) ss
>             in f res
>   where f (t, m)    = t / (2 * m)
>         g (t, m) s  = (t + ((theta h s) - (y s)) ^ 2, m + 1)
\end{lstlisting}

\subsection{Bajando por el gradiente}

Para la función descend utilizaremos dos fold anidados. El fold general recorre la hipótesis, usando una
tupla como acumulador la cual mantiene una lista y un número entero. La lista es utilizada para acumular
los valores pertenecientes a la nueva hipótesis mientras que el segundo valor acumula la cantidad de valores en la hipótesis.
Para calcular las nuevas componentes de la hipótesis, con un fold se recorre la lista de muestras aplicando la fórmula
$$\sum_{i=1}^m{(h_\theta(x^{(i)})-y^{(i)})x_j^{(i)}}$$ Éste fold usa como acumulador una tupla con un número en punto flotante y un
entero. El primero es usado para acumular los valores parciales de la sumatoria y el segundo para contar la cantidad de muestras.

\begin{lstlisting}

> descend :: Double -> Hypothesis Double -> [Sample Double] 
>                   -> Hypothesis Double
> descend alpha h ss  = Hypothesis $ reverse $ fst $ p 
>   where p           = foldl' k ([], 0) (c h)
>         f j         = foldl' (l j) (0.0, 0) ss
>         l j (t,m) s = (t + (r s j), m + 1)
>         g (t, m)    = t * alpha / m
>         k (t, m) hj = ((hj - g (f m)):t, m + 1)
>         r s j       = ((theta h s) - (y s)) * ((x s) !! j)

\end{lstlisting}

Para la función \emph{gd} utilizaremos un unfold que nos permite acumular las hipótesis mejoradas y el costo de cada una de estas.

\begin{lstlisting}

> gd :: Double -> Hypothesis Double -> [Sample Double]
>    -> [(Integer,Hypothesis Double,Double)]
> gd alpha h ss = unfoldr f (0, h, cost h ss') 
>   where ss'           = addOnes ss
>         f (n, h', c') = let h'' = descend alpha h' ss' 
>                             c'' = cost h'' ss' in 
>                               if   veryClose c'' c' then Nothing
>                               else Just ((n  , h' , c' )
>                                         ,(n+1, h'', c'')
>                                         )

\end{lstlisting}

\pagebreak
\section{Monoids}

\begin{lstlisting}
> newtype Max a = Max { getMax :: Maybe a }
>           deriving (Show)

> instance Ord a => Monoid (Max a) where
>   mempty                                = Max Nothing
>   mappend (Max(Just n1)) (Max(Just n2)) = Max $ Just $ max n1 n2
>   mappend (Max(Nothing)) (Max(Just n2)) = Max $ Just $ n2
>   mappend (Max(Just n1)) (Max(Nothing)) = Max $ Just $ n1
>   mappend _ _                           = Max Nothing

\end{lstlisting}

\pagebreak

\section{Zippers}

\begin{lstlisting}
> data Filesystem a = File a | Directory a [Filesystem a]
>        deriving(Show)
>
\end{lstlisting}
Para la definición de \emph{Breadcrumbs} tomamos en cuenta todas las direcciones de movimiento. El constructor \emph{WD} representa bajar un nivel
dentro del árbol. Los constructor \emph{WL} representan el movimiento de izquierda a derecha, mientras que el constructor \emph{WR}
representa el de derecha a izquierda, ambos en el mismo nivel del árbol. Para ambos casos mantenemos dos listas, una para representar
todos los nodos en órden a los que debemos movernos en caso de movernos hacia la derecha y otra para el caso en que deseamos movernos hacia la izquierda.
Para mantener el camino recorrido cada constructor es recursivo, ésto nos permitirá guardar los pasos que hemos dado dentro del árbol.

\begin{lstlisting}
> data Breadcrumbs a =
>        WD a [Filesystem a] (Breadcrumbs a)
>      | WL   [Filesystem a] [Filesystem a] (Breadcrumbs a)
>      | WR   [Filesystem a] [Filesystem a] (Breadcrumbs a)
>      | EmptyBreadCrumb
>     deriving(Show)
>
> type Zipper a = (Filesystem a, Breadcrumbs a)
>
\end{lstlisting}
Para la función \emph{goDown} solo es necesario considerar el caso en que el árbol en el cual deseamos movernos. Solo podemos bajar un
nivel cuando estemos en un nodo directorio, por lo que retornamos el primer nodo hijo del directorio junto con el constructor \emph{WD}.
Para cualquier otro caso, retornamos \emph{Nothing}.

\begin{lstlisting}
> goDown :: Zipper a -> Maybe (Zipper a)
> goDown (Directory y (x:xs), r) = Just (x, WD y xs r)
> goDown _                       = Nothing
\end{lstlisting}
Para el caso en que deseamos movernos hacia la derecha, debemos considerar de donde venimos. Si acabamos de bajar un nivel, entonces estamos
en el primer nodo hijo del directorio y lo agregamos a la lista de nodos a los que podríamos devolvernos. Además, extraemos el primer elemento
de la lista de nodos siguientes y lo agregamos como nodo actual. Para los casos en que nos habíamos movido lateralmente solo debemos extraer el primer
nodo de la lista de posibles movimientos hacia la derecha y agregar el nodo en el que estabamos de primero a la lista de nodos a los que podemos
devolvernos.

\begin{lstlisting}
> goRight :: Zipper a -> Maybe (Zipper a)
> goRight (fd,WD z  a@(y:ys) r)=Just (y,WR ([fd])  ys (WD z  a r))
> goRight (fd,WR ys a@(z:zs) r)=Just (z,WR (fd:ys) zs (WR ys a r))
> goRight (fd,WL ys a@(z:zs) r)=Just (z,WR (fd:ys) zs (WL ys a r))
> goRight _                    =Nothing
\end{lstlisting}
Para el caso en que deseamos movernos hacia la izquierda también debemos considerar de donde venimos. Para los casos en que nos movemos
lateralmente solo hace falta extraer el primer nodo de la lista de nodos anteriores y agregarlo como nodo actual, mientras agregamos el nodo en que estabamos
de primero en la lista de nodos siguientes. El caso en que acabamos de bajar no hace falta considerarlo porque estaríamos en el primer nodo del nivel y no
podríamos movernos hacia la izquierda porque nos saldríamos de los límites del árbol.

\begin{lstlisting}
> goLeft :: Zipper a -> Maybe (Zipper a)
> goLeft (fd,WR a@(y:ys) zs r)= Just (y,WL ys (fd:zs) (WR a zs r))
> goLeft (fd,WL a@(y:ys) zs r)= Just (y,WL ys (fd:zs) (WL a zs r))
> goLeft _                    = Nothing
\end{lstlisting}
Permite volver al punto del árbol en que estabamos anteriormente. En el caso que el movimiento anterior fue lateral, entonces solo hace falta extraer el nodo
de la lista correspondiente y usarlo como actual. En el caso que venimos de bajar un nivel, debemos reconstruir el directorio en el que estabamos.

\begin{lstlisting}
> goBack :: Zipper a -> Maybe (Zipper a)
> goBack (_ , WR (y:yz) zs lb) = Just (y, lb)
> goBack (_ , WL ys (z:zs) lb) = Just (z, lb)
> goBack (fd, WD e r lb)       = Just (Directory e (fd:r), lb)
> goBack _                     = Nothing
\end{lstlisting}
Función que permite volver al estado inicial.

\begin{lstlisting}
> tothetop :: Zipper a -> Zipper a
> tothetop (t, EmptyBreadCrumb) = (t, EmptyBreadCrumb)
> tothetop z                    = tothetop $ fromJust $ goBack $ z
\end{lstlisting}
Función que permite sustituir valores del árbol.
 
\begin{lstlisting}
> modify :: (a -> a) -> Zipper a -> Zipper a
> modify f (File x, bs)         = (File (f x), bs)
> modify f (Directory x xs, bs) = (Directory (f x) xs, bs)
\end{lstlisting}
Función para montar el \emph{zipper}.

\begin{lstlisting}
> focus :: Filesystem a -> Zipper a
> focus fs = (fs, EmptyBreadCrumb)
\end{lstlisting}
Función para desmontar el \emph{zipper}.

\begin{lstlisting}
> defocus :: Zipper a -> Filesystem a
> defocus (fs, _) = fs
\end{lstlisting}
\pagebreak
