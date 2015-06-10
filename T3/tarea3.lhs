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

\begin{document}

\title{CI4251 - Programación Funcional Avanzada \\ Tarea 3}

\author{José Luis Jiménez\\
10-10839\\
\href{mailto:jljb1990@gmail.com}{<jljb1990@gmail.com>}}

\date{Junio 05, 2015}

\maketitle

\pagebreak

\section*{Modulos Importados}

\begin{lstlisting}

> import System.Random
> import Control.Concurrent
> import Control.Monad
> import Data.Sequence as DS hiding (replicateM, take)
> import Control.Exception
> import System.Posix.Signals
> import Control.Concurrent.STM
> import qualified Data.Foldable as DF

\end{lstlisting}

\section*{Funciones comunes}

\subsection*{Funciones para generar valores pseudo-aleatorios}

\begin{lstlisting}

> getRandoms :: Int -> Int -> Int -> [Int]
> getRandoms n lo hi = randomRs (lo, hi) (mkStdGen n)
>
> randomSeed :: Int
> randomSeed = 42

\end{lstlisting}

\subsection*{Funciones para dar formato a los resultados}

\begin{lstlisting}

> formatParroquiano :: Int -> Seq Int -> [String]
> formatParroquiano n l = 
>     case viewl l of 
>       x :< xs ->  ("\nParroquiando " 
>                     ++ show n ++ ":   "
>                     ++ show x)
>                     : formatParroquiano (n+1) xs
>       EmptyL -> []
> 
> formatRafita :: Int -> String
> formatRafita r = "\nRafita preparó " ++ show r ++ " empanadas"
> 
> formatTotal :: Seq Int -> String 
> formatTotal l = "\nTotal:   " ++ show(DF.sum l)

\end{lstlisting}

\subsection*{Función para correr la simulación}

\begin{lstlisting}

> runSimulation :: (Int -> Int -> IO ()) -> Int -> Int -> IO ()
> runSimulation f m n = 
>    catch (f m n) 
>          (\e -> case e of 
>                   ThreadKilled -> putStrLn "" 
>                   otherwise    -> putStrLn $ show e
>          )

\end{lstlisting}

\section*{Solución usando concurrencia}

\subsection*{Tipos definidos}

\begin{lstlisting}

> type Colador    = MVar Int
> type MBuffer a  = Chan a
> type MSem       = MVar Bool
> type MSeq a     = MVar (DS.Seq a)
> type RandomList = MVar [Int]

\end{lstlisting}

\subsection*{Función crear una lista de valores pseudo-aleatorios}

\begin{lstlisting}

> newRandomList :: Int -> Int -> IO RandomList
> newRandomList lo hi = 
>     newMVar $ getRandoms randomSeed (lo*1000000) (hi*1000000)

\end{lstlisting}

\pagebreak

\subsection*{Funciones para el buffer de salida}

\begin{lstlisting}

> newMBuffer :: IO (MBuffer a)
> newMBuffer = newChan
>
> mPut :: MBuffer a -> a -> IO()
> mPut = writeChan
>  
> mGet :: MBuffer a -> IO a
> mGet = readChan
> 
> mOutput :: MBuffer String -> IO ()
> mOutput buffer = 
>     do e <- mGet buffer
>        putStrLn $ e
>        mOutput buffer

\end{lstlisting}

\subsection*{Funciones para el conteo de las empanadas}

\begin{lstlisting}

> newMSeqInt :: Int -> IO(MSeq Int)
> newMSeqInt m = newMVar $ fromList $ take m $ repeat 0
> 
> ateEmpanadaC :: MSeq Int -> Int -> IO()
> ateEmpanadaC si id =
>   do v <- takeMVar si
>      putMVar si (DS.adjust (+1) id v)

\end{lstlisting}

\subsection*{Funciones para el control de concurrencia}

\begin{lstlisting}

> newMSem :: IO MSem
> newMSem = newEmptyMVar
> 
> vC :: MSem -> IO ()
> vC s = do tryPutMVar s True
>           return ()
> 
> pC :: MSem -> IO (Bool)
> pC = takeMVar

\end{lstlisting}

\subsection*{Funciones para el colador}

\begin{lstlisting}

> takeFromColador :: Colador -> MSem -> IO ()
> takeFromColador c s = 
>   do e <- takeMVar c
>      if    (e >  0) then putMVar c (e - 1)
>      else  do vC s
>               takeFromColador c s
> newColador :: Int -> IO Colador
> newColador = newMVar
> 
> fillColador :: Colador -> Int -> IO ()
> fillColador c m = putMVar c m
> 
> oneMoreRoundC :: Colador -> Int -> IO ()
> oneMoreRoundC c m =
>   do v <- takeMVar c
>      putMVar c (v + m)

\end{lstlisting}

\subsection*{Función para retrasar a los hilos}

\begin{lstlisting}

> randomDelay :: MVar [Int] -> IO ()
> randomDelay rs = 
>   do r' <- takeMVar rs
>      putMVar rs $ tail r'
>      threadDelay $ head r'

\end{lstlisting}

\pagebreak

\subsection*{Función describir el comportamiento de Rafita}

\begin{lstlisting}

> rafitaC :: Colador 
>         -> MSem 
>         -> Int 
>         -> Colador 
>         -> MVar [Int] 
>         -> MBuffer [Char] 
>         -> IO ()
> rafitaC c s m tr rs out =
>     do pC s
>        mPut out $ "Rafita está cocinando"
>        randomDelay rs
>        fillColador c m
>        oneMoreRoundC tr m
>        mPut out $ "Rafita sirvió las empanadas"
>        rafitaC c s m tr rs out

\end{lstlisting}

\subsection*{Función describir el comportamiento de los parroquianos}

\begin{lstlisting}

> parroquianoC ::  Int 
>               -> Colador 
>               -> MSem 
>               -> MSeq Int 
>               -> MVar [Int] 
>               -> MBuffer [Char] 
>               -> IO ()
> parroquianoC id c s si rs out = 
>   do randomDelay rs
>      mPut out $ "El parroquiano " ++ show id ++ " tiene hambre"
>      takeFromColador c s
>      ateEmpanadaC si id
>      mPut out $ "El parroquiano " ++ show id ++ " come empanada"
>      parroquianoC id c s si rs out

\end{lstlisting}

\pagebreak

\subsection*{Función imprimir los resultados de la simulación}

\begin{lstlisting}

> putResumeC ::  ThreadId 
>             -> [ThreadId] 
>             -> MVar (Seq Int) 
>             -> MVar Int 
>             -> IO ()
> putResumeC tid idp si tr = 
>     do  mapM_ killThread idp
>         r  <- takeMVar tr
>         putStrLn $ formatRafita r
>         pv <- takeMVar si
>         mapM_ putStr (formatParroquiano 0 pv)
>         putStrLn $ formatTotal pv
>         killThread tid

\end{lstlisting}

\subsection*{Función principal}

\begin{lstlisting}

> classic' :: Int -> Int -> IO ()
> classic' m n =
>     do out <- newMBuffer
>        c   <- newColador m
>        tr  <- newColador m
>        s   <- newMSem
>        si  <- newMSeqInt n
>        rs  <- newRandomList 1 7
>        rsr <- newRandomList 3 5
>        idr <- forkIO (rafitaC c s m tr rsr out)
>        idp <- forM [0..n-1] 
>                 $ \i -> forkIO (parroquianoC i c s si rs out)
>        tid <- myThreadId
>        installHandler keyboardSignal 
>                         (Catch (putResumeC tid (idr:idp) si tr))
>                         Nothing
>        mOutput out
> 
> classic :: Int -> Int -> IO ()
> classic m n = runSimulation classic' m n

\end{lstlisting}

\pagebreak

\section*{Solución usando memoria transaccional}

\subsection*{Tipos definidos}

\begin{lstlisting}

> type RandomTList = TVar [Int]
> type Rafita      = TVar Int
> -- Tipo tomado del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> type Semaphore   = TVar Bool
> -- Tipo tomado del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> type Buffer a    = TVar (DS.Seq a)

\end{lstlisting}

\section*{Funciones para generar valores psudio-aleatorios y retrasar hilos}

\begin{lstlisting}

> randomTDelay rs = 
>   do r' <- atomically $ readTVar rs
>      atomically $ writeTVar rs $ tail r'
>      threadDelay $ head r'
>
> newRandomTList :: Int -> Int -> IO RandomTList
> newRandomTList lo hi = newTVarIO $ 
>     getRandoms randomSeed (lo*1000000) (hi*1000000)

\end{lstlisting}

\pagebreak

\section*{Funciones para el manejo de semáforos}

\begin{lstlisting}

> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> newSem :: Bool -> IO Semaphore
> newSem available = newTVarIO available
> 
> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> p :: Semaphore -> STM ()
> p sem = do b <- readTVar sem
>            if b
>               then writeTVar sem False
>               else retry
> 
> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> v :: Semaphore -> STM ()
> v sem = writeTVar sem True

\end{lstlisting}

\pagebreak

\section*{Funciones para el manejo de buffers}

\begin{lstlisting}

> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> newBuffer :: IO (Buffer a)
> newBuffer = newTVarIO DS.empty
> 
> newEmptyBufferInt :: Int -> IO (Buffer Int)
> newEmptyBufferInt n = newTVarIO $ fromList $ take n $ repeat 0
> 
> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> put :: Buffer a -> a -> STM ()
> put buffer item = do ls <- readTVar buffer
>                      writeTVar buffer (ls |> item)
>  
> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> get :: Buffer a -> STM a
> get buffer = do ls <- readTVar buffer
>                 case viewl ls of
>                   EmptyL       -> retry
>                   item :< rest -> do writeTVar buffer rest
>                                      return item
> 
> -- Función tomada del archivo cena.hs creado 
> -- por Ernesto Hernández-Novich
> -- Puede ser descargarse del siguiente link 
> -- http://ldc.usb.ve/~emhn/cursos/ci4251/201504/cena.hs
> output buffer = 
>     do str <- atomically $ get buffer
>        putStrLn $ str
>        output buffer

\end{lstlisting}

\pagebreak

\section*{Funciones para el control de la concurrencia}

\begin{lstlisting}

> newRafita :: Int -> IO Rafita
> newRafita m = do r <- newTVarIO m
>                  return r  
> 
> thereIsEmpanada :: Rafita -> STM (Bool)
> thereIsEmpanada r = 
>     do  m <- readTVar r
>         return $ m /= 0 
> 
> takeEmpanada :: Rafita -> STM ()
> takeEmpanada r =
>   do  m <- readTVar r
>       if   m == 0 then retry
>       else do writeTVar r (m - 1)
> 
> oneMoreRound :: Rafita -> STM ()
> oneMoreRound r =
>   do  m <- readTVar r
>       writeTVar r (m + 1)
> 
> putEmpanadas :: Int -> Rafita -> STM()
> putEmpanadas n r =
>   do  m <- readTVar r
>       writeTVar r n

\end{lstlisting}

\subsection*{Función para el conteo de las empanadas}

\begin{lstlisting}

> ateEmpanada :: Int -> Buffer Int -> STM ()
> ateEmpanada id bi = 
>   do  s <- readTVar bi
>       writeTVar bi (DS.adjust (+1) id s)

\end{lstlisting}

\pagebreak

\subsection*{Función describir el comportamiento de Rafita}

\begin{lstlisting}

> rafita :: Int 
>        -> Rafita 
>        -> Semaphore 
>        -> Rafita 
>        -> RandomTList 
>        -> Buffer [Char] 
>        -> IO ()
> rafita n r e c rs buffer = 
>     do atomically $ p e
>        atomically $ put buffer ("Rafita está cocinando")
>        randomTDelay rs
>        atomically $ putEmpanadas n r
>        atomically $ oneMoreRound c
>        atomically $ put buffer ("Rafita sirvio las empanadas")
>        rafita n r e c rs buffer

\end{lstlisting}

\subsection*{Función describir el comportamiento de los parroquianos}

\begin{lstlisting}

> parroquiano :: Int 
>             -> Rafita 
>             -> Semaphore 
>             -> Buffer Int 
>             -> RandomTList 
>             -> Buffer String 
>             -> IO ()
> parroquiano id r tr bi rs buffer = 
>     do  randomTDelay rs
>         atomically $ 
>           put buffer ("El parroquiano " 
>                         ++ show id 
>                         ++ " tiene hambre") 
>         atomically $ takeEmpanada r
>         atomically $ ateEmpanada id bi
>         atomically $ 
>           put buffer ("El parroquiano " 
>                         ++ show id 
>                         ++ " esta comiendo")
> 
>         l <- atomically $ thereIsEmpanada r
>         if not l then atomically $ v tr
>         else return ()
>         
>         parroquiano id r tr bi rs buffer

\end{lstlisting}

\subsection*{Función imprimir los resultados de la simulación}

\begin{lstlisting}

> putResume :: ThreadId 
>           -> Buffer Int 
>           -> [ThreadId] 
>           -> Rafita 
>           -> IO ()
> putResume tid bi idp c = 
>     do  mapM_ killThread idp
>         c' <- atomically $ readTVar c
>         putStrLn $ formatRafita c'
>         l  <- atomically $ readTVar bi
>         mapM_ putStr (formatParroquiano 0 l)
>         putStrLn $ formatTotal l
>         killThread tid

\end{lstlisting}

\subsection*{Función principal}

\begin{lstlisting}

> transactional' :: Int -> Int -> IO ()
> transactional' m n = 
>     do  out <- newBuffer
>         r   <- newRafita m
>         c   <- newRafita m
>         e   <- newSem False
>         bi  <- newEmptyBufferInt n
>         tid <- myThreadId
>         rs  <- newRandomTList 1 7
>         rsr <- newRandomTList 3 5
>         idr <- forkIO (rafita m r e c rsr out)
>         idp <- forM [0..n-1] 
>                  $ \i -> forkIO (parroquiano i r e bi rs out)
>         installHandler keyboardSignal 
>                        (Catch (putResume tid bi (idr:idp) c)) 
>                        Nothing
>         output out
>
> transactional :: Int -> Int -> IO ()
> transactional m n = runSimulation transactional' m n

\end{lstlisting}

 \end{document}
