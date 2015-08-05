\documentclass[11pt,fleqn]{article}

\usepackage{color}
\usepackage{tikz}
\usetikzlibrary{positioning,shapes}

\definecolor{brown}{rgb}{0.7,0.2,0}
\definecolor{darkgreen}{rgb}{0,0.6,0.1}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}


\usepackage[spanish]{babel}
\usepackage{lmodern}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{listings}
\usepackage[colorlinks=true,urlcolor=blue]{hyperref}

\usepackage{mathrsfs}
\usepackage{amsmath}

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

\title{CI4251 - Programación Funcional Avanzada \\ Tareas 5}

\author{José Luis Jiménez\\
10-10839\\
\href{mailto:jljb990@gmail.com}{<jljb990@gmail.com>}}

\date{Junio 22, 2015}

\maketitle

\pagebreak

\section*{Solución propuesta}

\begin{lstlisting}

> import Text.Parsec
> import Control.Monad.Identity
> import qualified Control.Applicative as AP
> 
> data Symbol = SymTrue | SymFalse | SymAnd | SymOr | SymXor
>             deriving (Show,Eq)
> 
> type Parser a = ParsecT String () Identity a
> 
> symbolTrue :: Parser Symbol
> symbolTrue  = string "true"  >> return SymTrue
> 
> symbolFalse :: Parser Symbol
> symbolFalse = string "false" >> return SymFalse
> 
> symbolAnd :: Parser Symbol
> symbolAnd   = string "and"   >> return SymAnd
> 
> symbolOr :: Parser Symbol
> symbolOr    = string "or"    >> return SymOr
> 
> symbolXor :: Parser Symbol
> symbolXor   = string "xor"   >> return SymXor
> 
> symbol :: Parser Symbol
> symbol = symbolTrue 
>      <|> symbolFalse 
>      <|> symbolAnd 
>      <|> symbolOr 
>      <|> symbolXor
> 
> expresion :: Parser [Symbol]
> expresion =
>   try (do s <- symbol
>           spaces
>           r <- expresion
>           return $ s : r
>       )
>   <|> return []
> 
> expresiones :: Parser [[Symbol]]
> expresiones = try( do spaces
>                       eof
>                       return []
>                  )
>               <|> do spaces
>                      e <- expresion
>                      spaces
>                      char ';'
>                      r <- expresiones 
>                      return $ e : r
> 
> trueWays xs = d
>   where d = let size = (length xs `div` 2) in
>               [
>                 [ delta m n | n <- [0..size] ]
>                 | m <- [0..size]
>               ]
>         delta m n = 
>           if m == n then 
>               if xs !! (2 * m) == SymTrue then 
>                 (1, 0)
>               else 
>                 (0, 1)
>           else if n > m then
>               calculate m n xs d 0 (0, 0)
>           else
>               (0, 0)
>         calculate i j xs d g r@(r1, r2) =
>           if j == g then
>              r 
>           else
>             let k   = i + g
>                 tik = (fst $ d !! i !! k)
>                       + (snd $ d !! i !! k)
>                 tkj = (fst $ d !! (k + 1) !! j) 
>                       + (snd $ d !! (k + 1) !! j)
>             in
>               if xs !! (2 * k + 1) == SymAnd then
>                  calculate i j xs d (g+1) 
>                  (r1 + (fst $ d !! i !! k)
>                      * (fst $ d !! (k + 1) !! j)
>                  ,r2 + tik * tkj - (fst $ d !! i !! k)
>                      * (fst $ d !! (k + 1) !! j))
>               else if xs !! (2 * k + 1) == SymOr then
>                  calculate i j xs d (g+1)
>                  (r1 + tik * tkj - (snd $ d !! i !! k)
>                      * (snd $ d !! (k + 1) !! j)
>                  ,r2 + (snd $ d !! i !! k)
>                      * (snd $ d !! (k + 1) !! j))
>               else if xs !! (2 * k + 1) == SymXor then
>                  calculate i j xs d (g+1)
>                  (r1 + (snd $ d !! i !! k)
>                      * (fst $ d !! (k + 1) !! j)
>                      + (fst $ d !! i !! k) 
>                      * (snd $ d !! (k + 1) !! j)
>                  ,r2 + (fst $ d !! i !! k)
>                      * (fst $ d !! (k + 1) !! j)
>                      + (snd $ d !! i !! k)
>                      * (snd $ d !! (k + 1) !! j))
>               else
>                 r
> main =
>   do putStrLn "Por favor, introduzca el nombre del archivo"
>      n <- getLine
>      l <- readFile n
>      putStrLn $ show $ fmap (map trueWays) 
>                      $ runParser expresiones () "" l

\end{lstlisting}

\end{document}
