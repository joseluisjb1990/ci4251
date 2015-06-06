import System.Random
import Control.Concurrent
import Control.Monad
import Data.Sequence as DS hiding (replicateM, take)
import Control.Exception
import System.Posix.Signals
import Control.Concurrent.STM
import qualified Data.Foldable as DF

type Colador    = MVar Int
type MBuffer a  = Chan a
type MSem       = MVar Bool
type MSeq a     = MVar (DS.Seq a)

newMBuffer :: IO (MBuffer a)
newMBuffer = newChan

newMSeqInt :: Int -> IO(MSeq Int)
newMSeqInt m = newMVar $ fromList $ take m $ repeat 0

ateEmpanadaC :: MSeq Int -> Int -> IO()
ateEmpanadaC si id =
  do v <- takeMVar si
     putMVar si (DS.adjust (+1) id v)

mPut :: MBuffer a -> a -> IO()
mPut = writeChan
 
mGet :: MBuffer a -> IO a
mGet = readChan

mOutput buffer = 
    do e <- mGet buffer
       putStrLn $ e
       mOutput buffer

takeFromColador :: Colador -> MSem -> IO ()
takeFromColador c s = 
  do e <- takeMVar c
     if    (e >  0) then putMVar c (e - 1)
     else  do vC s
              takeFromColador c s

newColador :: Int -> IO Colador
newColador = newMVar

fillColador :: Colador -> Int -> IO ()
fillColador c m = putMVar c m

newMSem :: IO MSem
newMSem = newEmptyMVar

emptyColador :: Colador -> IO (Bool)
emptyColador = isEmptyMVar

vC :: MSem -> IO ()
vC s = 
    do tryPutMVar s True
       return ()

pC :: MSem -> IO (Bool)
pC = takeMVar

oneMoreRoundC :: Colador -> Int -> IO ()
oneMoreRoundC c m =
  do v <- takeMVar c
     putMVar c (v + m)

rafitaC c s m tr out =
    do pC s
       mPut out $ "Rafita está cocinando"
       randomDelayR
       fillColador c m
       oneMoreRoundC tr m
       mPut out $ "Rafita sirvió las empanadas"
       rafitaC c s m tr out     

parroquianoC id c s si out = 
  do randomDelayP
     mPut out $ "El parroquiano " ++ show id ++ " tiene hambre"
     takeFromColador c s
     ateEmpanadaC si id
     mPut out $ "El parroquiano " ++ show id ++ " come empanada"
     parroquianoC id c s si out

putResumeC tid idp si tr = 
    do  mapM_ killThread idp
        r  <- takeMVar tr
        putStrLn $ formatRafita r
        pv <- takeMVar si
        mapM_ putStr (formatParroquiano 0 pv)
        putStrLn $ formatTotal pv
        killThread tid

classic' :: Int -> Int -> IO ()
classic' m n =
    do out <- newMBuffer
       c   <- newColador m
       tr  <- newColador m
       s   <- newMSem
       si  <- newMSeqInt n
       idr <- forkIO (rafitaC c s m tr out)
       idp <- forM [0..n-1] $ \i -> forkIO (parroquianoC i c s si out)
       tid <- myThreadId
       installHandler keyboardSignal (Catch (putResumeC tid (idr:idp) si tr)) Nothing
       mOutput out


classic :: Int -> Int -> IO ()
classic m n = runSimulation classic' m n

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

readBufferInt :: Buffer a -> STM (DS.Seq a)
readBufferInt buffer = do ls <- readTVar buffer
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

oneMoreRound :: Int -> Rafita -> STM ()
oneMoreRound n r =
  do  m <- readTVar r
      writeTVar r (m + n)

putEmpanadas :: Int -> Rafita -> STM()
putEmpanadas n r =
  do  m <- readTVar r
      writeTVar r n

randomSeed :: Int
randomSeed = 42

ateEmpanada :: Int -> Buffer Int -> STM ()
ateEmpanada id bi = 
  do  s <- readTVar bi
      writeTVar bi (DS.adjust (+1) id s)

parroquiano id r tr bi buffer = 
    do  randomDelayP
        atomically $ put buffer ("El parroquiano " ++ show id ++ " tiene hambre") 
        atomically $ takeEmpanada r
        atomically $ ateEmpanada id bi
        atomically $ put buffer ("El parroquiano " ++ show id ++ " esta comiendo")

        l <- atomically $ thereIsEmpanada r
        if not l then atomically $ v tr
        else return ()
        
        parroquiano id r tr bi buffer

rafita n r e c buffer = 
    do atomically $ p e
       atomically $ put buffer ("Rafita está cocinando")
       randomDelayR
       atomically $ putEmpanadas n r
       atomically $ oneMoreRound n c
       atomically $ put buffer ("Rafita sirvio las empanadas")
       rafita n r e c buffer

-- getRandoms :: Int -> [Int]
-- getRandoms n = randomRs (1 * 1000000,7 * 1000000) (mkStdGen n)
-- 
-- randomDelayP = 
--   do threadDelay $ head $ getRandoms randomSeed

randomDelayP = 
  do r <- randomRIO (100000,700000)
     threadDelay r

randomDelayR = 
  do r <- randomRIO (300000,500000)
     threadDelay r

formatParroquiano n l = 
    case viewl l of 
      x :< xs -> ("\nParroquiando " ++ show n ++ ":   " ++ show x) : formatParroquiano (n+1) xs
      EmptyL -> []

getRafita n = atomically $ readTVar n
 
formatRafita c = "\nRafita preparó " ++ show c ++ " empanadas"

formatTotal l = "\nTotal:   " ++ show(DF.sum l)

putResume tid bi idp c = 
    do  mapM_ killThread idp
        c' <- getRafita c
        putStrLn $ formatRafita c'
        l  <- atomically $ readBufferInt bi
        mapM_ putStr (formatParroquiano 0 l)
        putStrLn $ formatTotal l
        killThread tid

transactional' m n = 
    do  out <- newBuffer
        r   <- newRafita m
        c   <- newRafita m
        e   <- newSem False
        bi  <- newEmptyBufferInt n
        tid <- myThreadId
        idr <- forkIO (rafita m r e c out)
        idp <- forM [0..n-1] $ \i -> forkIO (parroquiano i r e bi out)
        installHandler keyboardSignal (Catch (putResume tid bi (idr:idp) c)) Nothing
        output out

transactional :: Int -> Int -> IO ()
transactional m n = runSimulation transactional' m n

runSimulation :: (Int -> Int -> IO ()) -> Int -> Int -> IO ()
runSimulation f m n = catch (f m n) 
                          (\e -> case e of 
                                   ThreadKilled -> putStrLn "" 
                                   otherwise    -> putStrLn $ "Ocurrió un error inesperado en el programa " ++ show e
                          )
