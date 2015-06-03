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

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 2}

\author{José Luis Jiménez\\
10-10839\\
\href{mailto:jljb1990@gmail.com}{<jljb1990@gmail.com>}}

\date{Mayo 22, 2015}

\maketitle

\pagebreak

\section*{Autómatas Finitos No Determinísticos}

\begin{lstlisting}

> import Control.Monad
> import Control.Monad.RWS
> import Control.Monad.Error
> import qualified Data.Sequence as Seq
> import qualified Data.Set as DS
> import Data.Char
> import Data.Either
> import Test.QuickCheck 

\end{lstlisting}

\begin{lstlisting}

> newtype NFANode = Node Int 
>                 deriving (Eq,Ord)
>
> instance Show NFANode where
>    show (Node i) = "q" ++ show i

\end{lstlisting}

\begin{lstlisting}

> data Transition = Move   { from, to :: NFANode, sym :: Char }
>                 | Lambda { from, to :: NFANode }
>                 deriving (Eq,Ord)
>
> instance Show Transition where
>   show (Move f t i) = show f ++ 
>                       " -" ++ show i ++ "-> " ++
>                       show t
>   show (Lambda f t) = show f ++ " ---> " ++ show t
>

\end{lstlisting}

\begin{lstlisting}

> data NFA = NFA { 
>                   sigma   :: (DS.Set Char),
>                   states  :: (DS.Set NFANode),
>                   moves   :: (DS.Set Transition),
>                   initial :: NFANode,
>                   final   :: (DS.Set NFANode)
>                }
>          deriving (Eq,Show)

\end{lstlisting}

\begin{lstlisting}

> nfa0 = NFA {
>              sigma  = DS.fromList "ab",
>              states = DS.fromList $ fmap Node [0..3],
>              moves  = DS.fromList [
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 0, sym = 'a' },
>                Move { from = Node 0, to = Node 1, sym = 'a' },
>                Move { from = Node 1, to = Node 2, sym = 'b' },
>                Move { from = Node 2, to = Node 3, sym = 'b' }
>              ],
>              initial = Node 0,
>              final = DS.fromList [ Node 3 ]
>            }

\end{lstlisting}

\pagebreak

\section*{Generando $\lambda-$NFAs}

\begin{lstlisting}

> instance Arbitrary NFANode where
>   arbitrary = fmap (Node . getPositive) (arbitrary)

\end{lstlisting}

\begin{lstlisting}

> instance Arbitrary NFA where
>  arbitrary = 
>    do si<-fmap DS.fromList $ listOf1 $ genLowerAsciiChar
>       st<-fmap DS.fromList $ fmap (inNode:) $ listOf $ arbitrary
>       mo<-fmap DS.fromList $ listOf $ (arbitraryTrans st si) 
>       fi<-fmap DS.fromList $ listOf $ elements $ DS.elems st
>       return $ NFA si st mo inNode fi

> arbitraryTrans:: DS.Set NFANode -> DS.Set Char -> Gen Transition
> arbitraryTrans states sigma = 
>   let genState = elements $ DS.elems states
>       genSigma = elements $ DS.elems sigma 
>   in
>   frequency [
>               (5, liftM3 Move   genState genState genSigma),
>               (1, liftM2 Lambda genState genState)
>             ]

\end{lstlisting}

\section*{Simulador de $\lambda-$NFA}

\begin{lstlisting}

> isMove :: Transition -> Bool
> isMove (Move _ _ _) = True
> isMove _            = False
> 
> isLambda :: Transition -> Bool
> isLambda = not . isMove  

\end{lstlisting}

\begin{lstlisting}

> lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
> lambdaMoves nfa n =  flip DS.insert set n
>   where
>     set        = DS.foldr f (DS.empty) (moves nfa)
>     f move acc = if isLambda move then 
>                     let f = from move in 
>                       if f == n then DS.insert (to move) acc
>                       else acc
>                  else acc

\end{lstlisting}

\begin{lstlisting}

> normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
> normalMoves nfa c n = DS.foldr f (DS.empty) (moves nfa)
>   where
>     f move acc = if isMove move then
>                     let f  = from move 
>                         c' = sym  move 
>                     in 
>                       if f == n && c' == c then
>                         DS.insert (to move) acc
>                       else
>                         acc
>                  else acc

\end{lstlisting}

\begin{lstlisting}

> destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
> destinations nfa c n = let lc  = lambdaMoves nfa n
>                            lc' = DS.foldr f DS.empty lc in
>                            fixSet (lambdaMoves nfa) lc'
>   where
>     f n' acc = let on = normalMoves nfa c n' in DS.union on acc

\end{lstlisting}

\begin{lstlisting}

> fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
> fixSet f s = let lt = DS.foldr g DS.empty s 
>                  un = DS.union lt s in
>              if DS.null lt then un
>              else fixSet f un
>   where
>     g n' acc = let sn = f n' in DS.union sn acc

\end{lstlisting}

\begin{lstlisting}

> runNFA :: NFA -> [Char] -> IO ()
> runNFA nfa word = putStrLn $ runNFA' nfa word 
>   where
>     runNFA' nfa word =
>       let t = evalRWS (runErrorT start) nfa (initialState word)
>       in  case fst t of
>             { Left  error -> show error 
>             ; Right _     -> (show . snd) t
>             }

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> data NFAReject = Stuck (DS.Set NFANode) String
>                | Reject (DS.Set NFANode)
>                deriving (Show)
>
> instance Error NFAReject

\end{lstlisting}

\begin{lstlisting}

> data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
>             deriving (Show,Eq)

\end{lstlisting}

\begin{lstlisting}

> initialState :: String -> NFARun
> initialState word = NFARun { w = word, qs = DS.empty }

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> accepting :: NFA -> DS.Set NFANode -> Bool
> accepting nfa = not . DS.null . DS.intersection (final nfa)

> start :: ErrorT (NFAReject) 
>                 (RWS NFA (Seq.Seq (DS.Set NFANode)) NFARun)
>                 ()
> start = do st <- get
>            put $ st { qs = DS.fromList [inNode] }
>            flow

> flow :: ErrorT (NFAReject) 
>                (RWS NFA (Seq.Seq (DS.Set NFANode)) NFARun)
>                ()
> flow = 
>   do sta <- get
>      nfa <- ask
>      let as = qs sta in
>        do tell $ Seq.singleton $ as
>           if ((null . w) sta) then
>             if accepting nfa (getMultiSet as (lambdaMoves nfa)) 
>               then return ()
>             else 
>               do throwError $ Reject as
>           else 
>             do let wo  = w sta 
>                    f   = destinations nfa (head wo)
>                    qs' = getMultiSet as f
>                 in 
>                    if (DS.null qs') then
>                      throwError $ Stuck as wo
>                    else 
>                      do put $ NFARun { w = tail wo, qs =  qs' }
>                         flow


\end{lstlisting}

\begin{lstlisting}

> prop_acceptsemptyword :: NFA -> Property
> prop_acceptsemptyword nfa = accepting nfa inSetLamda
>                               ==> evalWord nfa ""
>   where
>     inSetLamda = lambdaMoves nfa (initial nfa)
>     evalWord nfa word = isRight $ fst $ runWord nfa word

\end{lstlisting}

\pagebreak

\begin{lstlisting}

> prop_acceptancelength :: NFA -> String -> Property
> prop_acceptancelength nfa w =
>   let t = runWord nfa w
>       lr = fst t
>   in
>     isRight lr ==> (Seq.length $ snd t) - 1 == length w

\end{lstlisting}

\newpage
\section*{Otro Beta}

\begin{lstlisting}

> data Otro a = Otro { fromOtro :: ((a -> Beta) -> Beta) }

\end{lstlisting}

\begin{lstlisting}

> data Beta = Chamba (IO Beta)
>           | Convive Beta Beta
>           | Quieto

\end{lstlisting}

\begin{lstlisting}

> instance Show Beta where
>    show (Chamba x)    = " chamba "
>    show (Convive x y) = " convive(" ++ show x 
>                                     ++ "," 
>                                     ++ show y ++ ") "
>    show Quieto        = " quieto "

\end{lstlisting}

\begin{lstlisting}

> hacer :: Otro a -> Beta
> hacer (Otro f) = f (const Quieto)

\end{lstlisting}

\begin{lstlisting}

> quieto :: Otro a
> quieto = Otro $ \ _ -> Quieto

\end{lstlisting}

\begin{lstlisting}

> chambea :: IO a -> Otro a
> chambea x = Otro $ \f -> Chamba $ fmap f x

\end{lstlisting}

\begin{lstlisting}

> convive :: Otro a -> Otro ()
> convive x = Otro $ \f -> Convive (hacer x) (f ())

\end{lstlisting}

\begin{lstlisting}

> pana :: Otro a -> Otro a -> Otro a
> pana x y = Otro $ \f -> (Convive (hacer x) (hacer y))

\end{lstlisting}

\begin{lstlisting}

> vaca :: [Beta] -> IO ()
> vaca []                   = putStrLn ""
> vaca (Quieto:xs)          = vaca xs
> vaca ((Convive x y): xs)  = vaca $ xs ++ [x, y]
> vaca ((Chamba x):xs)      = do b <- x
>                                vaca (b:xs)

\end{lstlisting}

\begin{lstlisting}

> instance Monad Otro where
>   return x       = Otro $ \k -> k x
>   (Otro f) >>= g = Otro $ \k -> f (\x -> fromOtro (g x) k) 

\end{lstlisting}

\ignore{
\begin{lstlisting}

> cartel :: Otro ()
> cartel = pana (dale (clavo 42)) 
>               (pana (dale (clavo 69))
>                     (pana (dale (clavo 17)) 
>                           (dale (clavo 23) >> chambea (putStrLn ""))))
> 
> quedo :: Otro a -> IO ()
> quedo x = vaca [hacer x]
> 
> clavo :: Int -> String
> clavo 17 = "/nlmce"
> clavo 23 = "/y./p6"
> clavo 42 = "htptuc2"
> clavo 69 = "t:irofr"
> 
> dale :: String -> Otro ()
> dale xs = mapM_ (chambea . putChar) xs

> genLowerAsciiChar = suchThat (suchThat (arbitrary) isLower) isAscii

> inNode = Node 0

> isRight e = case e of
>               { Right _   -> True
>               ; otherwise -> False
>               }

> runWord nfa w = evalRWS (runErrorT start) nfa (initialState w) 

> getMultiSet set f = DS.foldr (DS.union) DS.empty (DS.map f set)

\end{lstlisting}
}

\end{document}
