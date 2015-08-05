\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}

\usepackage{amsmath}
\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

\lstset{
   language=Haskell,
   gobble=2,
   frame=single,
   framerule=1pt,
   showstringspaces=false,
   basicstyle=\footnotesize\ttfamily,
   keywordstyle=\textbf,
   backgroundcolor=\color{lightgrey},
   literate={á}{{\'a}}1
            {é}{{\'e}}1
            {í}{{\'i}}1
            {ó}{{\'o}}1
            {ú}{{\'u}}1
            {ñ}{{\~n}}1
}

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 4}

\author{José Luis Jiménez\\
10-10839\\
\href{mailto:jljb1990@gmail.com}{<jljb1990@gmail.com>}}

\date{Junio 17, 2015}

\maketitle

\pagebreak

\section*{Soluciones}

\subsection*{Liberías importadas}

\begin{lstlisting}

> import Data.Word
> import Control.Exception
> import qualified Data.Array.Repa as R
> import Control.Monad.Identity
> import qualified Control.Monad.Par as PAR
> import qualified Control.Parallel.Strategies as ST
> import qualified Graphics.HGL as G
> import Criterion.Main

\end{lstlisting}

\subsection*{Funciones comunes}

\begin{lstlisting}

> mandel w h = let (w', h') = (w-1, h-1)
>                  (sr, si) = (step w', step h')
>              in buildMatrix w' h' sr si
> 
> buildMatrix w h sr si = buildMatrix' w sr si h []
>   where
>     buildMatrix' w sr si i r =
>       let t = buildFile w sr (f si i) r in
>         if i == 0 then 
>           t
>         else
>           buildMatrix' w sr si (i-1) t
> 
> buildFile w sr y r = buildFile' sr y w r
>   where
>     buildFile' si y j r =
>       let t = (y, f sr j)
>           s = t : r in
>         if j == 0 then
>           s
>         else
>           buildFile' sr y (j-1) s

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> converge :: (Double,Double) -> Word8
> converge = converge' initial 0
>   where
>     converge' z@(zr, zi) n c@(cr, ci) = 
>         if n == 255 || mag z > 2 then
>           n
>           else 
>           converge' ((zr^2 - zi^2) + cr, 2*zr*zi + ci) (n+1) c
>     mag (x, y) = sqrt $ (x^2) + (y^2)
> 
> step n = 4 / (fromIntegral n)
>
> f :: Double -> Word32 -> Double
> f s i = -2.0 + s * (fromIntegral i)
>
> initial :: (Double, Double)
> initial = (0, 0)
>
> w = 256
> h = 128
>
> mandelS w h = let (w', h') = (w-1, h-1)
>                   (sr, si) = (step w', step h')
>              in buildMatrixS w' h' sr si
> 
> buildMatrixS w h sr si = buildMatrix' w sr si h []
>   where
>     buildMatrix' w sr si i r =
>       let t = buildFileS w sr (f si i) 
>           s = t : r in
>         if i == 0 then 
>           s
>         else
>           buildMatrix' w sr si (i-1) s
> 
> buildFileS w sr y = buildFile' sr y w []
>   where
>     buildFile' si y j r =
>       let t = (y, f sr j) 
>           s = t : r in
>         if j == 0 then
>           s
>         else
>           buildFile' sr y (j-1) s

\end{lstlisting}

\subsection*{Solución usando la librería de estrategias}

\begin{lstlisting}

> mandelStrat :: Word32 -> Word32 -> [[Word8]]
> mandelStrat w h = 
>   map (\xs -> 
>         (map converge xs 
>           `ST.using` 
>          ST.parList ST.rdeepseq
>         )
>       ) (mandelS w h) 
>           `ST.using` 
>         ST.evalList ST.rdeepseq

\end{lstlisting}

\subsection*{Solución usando el monad par}

\begin{lstlisting}

> mandelPar :: Word32 -> Word32 -> [Word8]
> mandelPar w h = PAR.runPar $ PAR.parMap converge $ mandel w h

\end{lstlisting}

\subsection*{Solución usando la librería REPA}

\begin{lstlisting}

> mandelREPA :: Word32 -> Word32 -> R.Array R.U R.DIM2 Word8
> mandelREPA w h = 
>  runIdentity $ R.computeUnboxedP $ R.map converge $ 
>  R.fromListUnboxed (R.Z R.:. fromIntegral h R.:. fromIntegral w) 
>  (mandel w h)

\end{lstlisting}

\subsection*{Solución gráfica}

\noindent
Para la solución gráfica tomamos en cuenta el resultado de hacer las
simulaciones usando la librería \texttt{Criterion}. La librería
de paralelización que ofreció mejores tiempos fue el monad par. Por
lo tanto escribimos una versión de la función mandel que retorna una
lista de tuplas con información del pixel y el número imaginario correspondiente.

\noindent
El resto de las funciones fueron reescritas para adaptarlas a esta versión de la 
función mandel.

\pagebreak

\begin{lstlisting}

> convergeG :: ((Word32, Word32), (Double,Double))
>           -> ((Word32, Word32), Word8)
> convergeG = converge' initial 0
>   where
>     converge' z@(zr, zi) n c@((f', c'), (cr, ci)) = 
>         if n == 255 || mag z > 2 then
>           ((f', c'), n)
>           else 
>           converge' ((zr^2 - zi^2) + cr, 2*zr*zi + ci) (n+1) c
>     mag (x, y) = sqrt $ (x^2) + (y^2)
> 
> mandelG w h = let (w', h') = (w-1, h-1)
>                   (sr, si) = (step w', step h')
>              in buildMatrixG w' h' sr si
> 
> buildMatrixG w h sr si = buildMatrix' w sr si h []
>   where
>     buildMatrix' w sr si i r =
>       let t = buildFileG w sr (f si i) i r
>       in
>         if i == 0 then 
>           t
>         else
>           buildMatrix' w sr si (i-1) t
> 
> buildFileG w sr y i r = buildFile' sr y w r i
>   where
>     buildFile' si y j r i =
>       let t = ((j, i), ( f sr j, y))
>           s = t : r in
>         if j == 0 then
>           s
>         else
>           buildFile' sr y (j-1) s i
>
> mandelParG :: Word32 -> Word32 -> [((Word32, Word32), Word8)]
> mandelParG w h = 
>   PAR.runPar $ PAR.parMap convergeG $ mandelG w h

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> makeGraphic w ((f', c'), col) = 
>     let 
>       f = fromIntegral f'
>       c = fromIntegral c'
>     in
>       if (f' >= w) then G.emptyGraphic
>       else G.withRGB (G.RGB col col col) (G.line (f,c) (f+1, c))
> 
> runSimulation w h t =
>     G.runGraphics $ do
>       window <- G.openWindow t (fromIntegral w, fromIntegral h)
>       
>       G.drawInWindow window $ 
>           G.overGraphics $ map (makeGraphic w) (mandelParG w h)
> 
>       G.getKey window
>       G.closeWindow window

\end{lstlisting}

\subsection*{Función principal}

\begin{lstlisting}

> main = 
>     defaultMain [ bench "Strategies" $ whnf (mandelStrat w) h
>                 , bench "REPA"       $ whnf (mandelREPA  w) h
>                 , bench "Monad Par"  $ whnf (mandelPar   w) h
>                 ]

\end{lstlisting}

\end{document}
