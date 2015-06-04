import System.Random
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Sequence as DS hiding (replicateM, take)
import Control.Exception
import Data.Typeable
import System.Posix.Signals
import qualified Data.Foldable as DF

type Buffer a   = TVar (DS.Seq a)
type Rafita     = TVar Int
type Semaphore  = TVar Bool

newSem :: Bool -> IO Semaphore
newSem available = newTVarIO available

p :: Semaphore -> STM ()
p sem = do b <- readTVar sem
           if b
              then writeTVar sem False
              else retry

v :: Semaphore -> STM ()
v sem = writeTVar sem True

newBuffer :: IO (Buffer a)
newBuffer = newTVarIO DS.empty

newEmptyBufferInt :: Int -> IO (Buffer Int)
newEmptyBufferInt n = newTVarIO $ fromList $ take n $ repeat 0

put :: Buffer a -> a -> STM ()
put buffer item = do ls <- readTVar buffer
                     writeTVar buffer (ls |> item)
 
get :: Buffer a -> STM a
get buffer = do ls <- readTVar buffer
                case viewl ls of
                  EmptyL       -> retry
                  item :< rest -> do writeTVar buffer rest
                                     return item

output buffer = 
    do str <- atomically $ get buffer
       putStrLn $ str
       output buffer

bufferToList :: Buffer a -> STM (DS.Seq a)
bufferToList buffer = do ls <- readTVar buffer
                         return $ ls

newRafita :: Int -> IO Rafita
newRafita m = do r <- newTVarIO m
                 return r  

thereIsEmpanada :: Rafita -> STM (Bool)
thereIsEmpanada r = 
    do  m <- readTVar r
        return $ m /= 0 

takeEmpanada :: Rafita -> STM ()
takeEmpanada r =
  do  m <- readTVar r
      if   m == 0 then retry
      else do writeTVar r (m - 1)

oneMoreRound :: Rafita -> STM ()
oneMoreRound r =
  do  m <- readTVar r
      writeTVar r (m + 1)

putEmpanadas :: Int -> Rafita -> STM()
putEmpanadas n r =
  do  m <- readTVar r
      writeTVar r n

randomSeed :: Int
randomSeed = 42

classic :: Int -> Int -> IO ()
classic m n = undefined

ateEmpanada :: Int -> Buffer Int -> STM ()
ateEmpanada id bi = do  s <- readTVar bi
                        writeTVar bi (DS.adjust (+1) id s)

parroquiano id r tr bi buffer = 
    do  atomically $ put buffer ("El parroquiano " ++ show id ++ " tiene hambre") 
        atomically $ takeEmpanada r
        atomically $ ateEmpanada id bi
        atomically $ put buffer ("El parroquiano " ++ show id ++ " esta comiendo")


        l <- atomically $ thereIsEmpanada r      
        if not l then
          do  atomically $ v tr
              randomDelayP
              parroquiano id r tr bi buffer
        else
          do randomDelayP
             parroquiano id r tr bi buffer

rafita n r e c buffer = 
    do atomically $ p e
       atomically $ put buffer ("Rafita está cocinando")
       randomDelayR
       atomically $ putEmpanadas n r
       atomically $ oneMoreRound c
       atomically $ put buffer ("Rafita sirvio las empanadas")
       rafita n r e c buffer

randomDelayP = do r <- randomRIO (100000,700000)
                  threadDelay r

randomDelayR = do r <- randomRIO (300000,500000)
                  threadDelay r

formatParroquiano n l = 
    case viewl l of 
      x :< xs -> ("\nParroquiando " ++ show n ++ ":   " ++ show x) : formatParroquiano (n+1) xs
      EmptyL -> []

getRafita n = atomically $ readTVar n
 
formatRafita r c = "\nRafita preparó " ++ show (r*c) ++ " empanadas"

formatTotal l = "\nTotal:   " ++ show(DF.sum l)

putResume tid bi idp c r = 
    do  forM_ idp $ \id -> killThread id
        c' <- getRafita c
        putStrLn $ formatRafita r c'
        l  <- atomically $ bufferToList bi
        mapM_ putStr (formatParroquiano 0 l)
        putStrLn $ formatTotal l
        killThread tid

transactional' m n = 
    do  out <- newBuffer
        r   <- newRafita m
        c   <- newRafita 1
        e   <- newSem False
        bi  <- newEmptyBufferInt n
        ids <- newBuffer
        tid <- myThreadId
        atomically $ put ids tid
        idr <- forkIO (rafita m r e c out)
        atomically $ put ids idr
        idp <- forM [0..n-1] $ \i -> forkIO (parroquiano i r e bi out)
        forM_ idp $ \id -> atomically $ put ids id
        installHandler keyboardSignal (Catch (putResume tid bi idp c m)) Nothing
        output out

transactional :: Int -> Int -> IO ()
transactional m n = catch (transactional' m n) 
                          (\e -> case e of 
                                   ThreadKilled -> putStrLn "" 
                                   otherwise    -> putStrLn "Ocurrió un error inesperado en el programa")

-- Cada parroquiano se ejcuta en un hilo distinto y cuando se acaban las empanadas
-- generan una excepcion. El hilo principal (rafita) atrapa la excepcion y prepara mas
-- empanadas.

-- transactional :: Int -> Int -> IO ()
-- transactional m n = do e <- newRafita m
--                        parroquiano e
--                
--                     -- Arranco los parroquianos. Cada parroquiano piensa un tiempo random y toma una empanada
-- 
-- parroquiano :: Rafita -> IO ()
-- parroquiano r = forever ( do t <- randomR (1,7) randomSeed
--                              threadDelay (fst t)
--                              e <- readTVar r
--                              writeTVar (e - 1)
--                         )
-- 
-- newRafita :: Int -> STM Rafita
-- newRafita m = do r <- newTVar m
--                  -- always (do epn <- readTVar r
--                  --            return (epn >= 0))
--                  return r  


--
--data NoMoreEmpanadas = NoMoreEmpanadas
 --   deriving(Show, Typeable)

--instance Exception NoMoreEmpanadas

-- thereIsEmpanada :: Rafita -> STM (Bool)
-- thereIsEmpanada r = 
--  do  m <- readTVar r
--      return $ m /= 0 
