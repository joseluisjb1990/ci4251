import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Error
import qualified Data.Sequence as Seq
import qualified Data.Set as DS
import Data.Char
import Data.Either
import Test.QuickCheck

newtype NFANode = Node Int 
                deriving (Eq,Ord)

instance Show NFANode where
  show (Node i) = "q" ++ show i

instance Arbitrary NFANode where
  arbitrary = fmap (Node . getPositive) (arbitrary)

data Transition = Move   { from, to :: NFANode, sym :: Char }
                | Lambda { from, to :: NFANode }
                deriving (Eq,Ord)

instance Show Transition where
  show (Move f t i) = show f ++ 
                     " -" ++ show i ++ "-> " ++
                     show t
  show (Lambda f t) = show f ++ " ---> " ++ show t

arbitraryTrans states sigma = let genState = elements $ DS.elems states
                                  genSigma = elements $ DS.elems sigma 
                              in
                              frequency [
                                          (5, liftM3 Move   genState genState genSigma),
                                          (1, liftM2 Lambda genState genState)
                                        ]
data NFA = NFA { 
                  sigma   :: (DS.Set Char),
                  states  :: (DS.Set NFANode),
                  moves   :: (DS.Set Transition),
                  initial :: NFANode,
                  final   :: (DS.Set NFANode)
               }
         deriving (Eq,Show)

instance Arbitrary NFA where
   arbitrary = do si <- fmap DS.fromList $ listOf1 $ genLowerAsciiChar
                  st <- fmap DS.fromList $ fmap (initialNode:) $ listOf $ arbitrary
                  mo <- fmap DS.fromList $ listOf $ (arbitraryTrans st si) 
                  fi <- fmap DS.fromList $ listOf $ elements $ DS.elems st
                  return $ NFA si st mo initialNode fi

initialNode = Node 0

isValidTransition sigma state trans = inStateFrom && inStateTo && inSigmaSym
    where
       inStateFrom  = DS.member (from trans) state
       inStateTo    = DS.member (to trans  ) state
       inSigmaSym   = DS.member (sym trans ) sigma
 
genLowerAsciiChar = suchThat (suchThat (arbitrary) isLower) isAscii

isMove :: Transition -> Bool
isMove (Move _ _ _) = True
isMove _            = False

isLambda :: Transition -> Bool
isLambda = not . isMove  

lambdaMoves :: NFA -> NFANode -> DS.Set NFANode
lambdaMoves nfa n =  DS.insert n $ DS.fromList $ DS.foldr f [] (moves nfa)
  where
    f move acc = if isLambda move then let f = from move in if f == n then (to move) : acc else acc
                                  else acc

normalMoves :: NFA -> Char -> NFANode -> DS.Set NFANode
normalMoves nfa c n = DS.fromList $ DS.foldr f [] (moves nfa)
  where
    f move acc = if isMove move then let f  = from move 
                                         c' = sym  move in 
                                         if f == n && c' == c then (to move) : acc else acc
                                else acc

destinations :: NFA -> Char -> NFANode -> DS.Set NFANode
destinations nfa c n = let lc  = lambdaMoves nfa n
                           lc' = DS.foldr f DS.empty lc in
                           fixSet (lambdaMoves nfa) lc'
  where
    f n' acc = let on = normalMoves nfa c n' in DS.union on acc
                       
fixSet :: Ord a => (a -> DS.Set a) -> DS.Set a -> DS.Set a
fixSet f s = let lt = DS.foldr g DS.empty s 
                 un = DS.union lt s in
             if DS.null lt then un
             else fixSet f un
  where
    g n' acc = let sn = f n' in DS.union sn acc

runNFA :: NFA -> [Char] -> IO ()
runNFA nfa word = putStrLn $ runNFA' nfa word 
  where
    runNFA' nfa word = let t = evalRWS (runErrorT start) nfa (initialState word) in
                          case fst t of
                          { Left  error -> show error 
                          ; Right _     -> (show . snd) t
                          }

data NFAReject = Stuck (DS.Set NFANode) String
               | Reject (DS.Set NFANode)
               deriving (Show)

instance Error NFAReject

data NFARun = NFARun { w :: String, qs :: DS.Set NFANode }
            deriving (Show,Eq)

initialState :: String -> NFARun
initialState word = NFARun { w = word, qs = DS.empty }

nfa0 = NFA {
             sigma  = DS.fromList "ab",
             states = DS.fromList $ fmap Node [0..3],
             moves  = DS.fromList [
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 0, sym = 'a' },
               Move { from = Node 0, to = Node 1, sym = 'a' },
               Move { from = Node 1, to = Node 2, sym = 'b' },
               Move { from = Node 2, to = Node 3, sym = 'b' }
             ],
             initial = Node 0,
             final = DS.fromList [ Node 3 ]
           }

accepting :: NFA -> DS.Set NFANode -> Bool
accepting nfa = not . DS.null . DS.intersection (final nfa)

start :: ErrorT (NFAReject) (RWS NFA (Seq.Seq (DS.Set NFANode)) NFARun) ()
start = do st <- get
           put $ st { qs = DS.fromList [initialNode] }
           flow

flow :: ErrorT (NFAReject) (RWS NFA (Seq.Seq (DS.Set NFANode)) NFARun) ()
flow = do sta <- get
          nfa <- ask
          let as = qs sta in
            do tell $ Seq.singleton $ as
               if((null . w) sta) then
                 if accepting nfa (DS.foldr (DS.union) (DS.empty) (DS.map (lambdaMoves nfa) as)) then return ()
                 else do throwError $ Reject as
               else do let wo  = w sta 
                           qs' = DS.foldr (DS.union) DS.empty (DS.map (destinations nfa (head wo)) as) in 
                            if(DS.null qs') then do throwError $ Stuck as wo
                            else do put $ NFARun { w = tail wo, qs =  qs' }
                                    flow

prop_acceptsemptyword :: NFA -> Property
prop_acceptsemptyword nfa = accepting nfa (lambdaMoves nfa (initial nfa)) ==> evalWord nfa ""
    where
      evalWord nfa word = isRight $ fst $ evalRWS (runErrorT start) nfa (initialState word)
      isRight e = case e of
                    { Right _   -> True
                    ; otherwise -> False
                    }


prop_acceptancelength :: NFA -> String -> Property
prop_acceptancelength nfa w = let t  = evalRWS (runErrorT start) nfa (initialState w) 
                                  lr = fst t in
                                  isValid lr ==> (Seq.length $ snd t) - 1 == length w
                                  where
                                    isValid lr = case lr of 
                                                 { Right _   -> True
                                                 ; otherwise -> False
                                                 }
nfa1 = NFA {
              sigma   = DS.fromList "cst", 
              states  = DS.fromList [Node 0,Node 3,Node 4], 
              moves   = DS.fromList [Move {from = Node 0, to = Node 4, sym = 'c'},Move {from = Node 3, to = Node 0, sym = 'c'},Move {from = Node 4, to = Node 3, sym = 'c'},Lambda {from = Node 0, to = Node 3}],
              initial = Node 0, 
              final   = DS.fromList [Node 3,Node 4]
            }

data Otro a = Otro { fromOtro :: ((a -> Beta) -> Beta) }

data Beta = Chamba (IO Beta)
          | Convive Beta Beta
          | Quieto

instance Show Beta where
   show (Chamba x)    = " chamba "
   show (Convive x y) = " convive(" ++ show x 
                                    ++ "," 
                                    ++ show y ++ ") "
   show Quieto        = " quieto "

-- thread
hacer :: Otro a -> Beta
hacer (Otro f) = f (const Quieto)

-- cEnd
quieto :: Otro a
quieto = Otro $ \ _ -> Quieto

--cPrint
chambea :: IO a -> Otro a
chambea x = Otro $ \f -> Chamba $ fmap f x

-- cFork
convive :: Otro a -> Otro ()
convive x = Otro $ \f -> Convive (hacer x) (f ())

pana :: Otro a -> Otro a -> Otro a
pana x y = Otro $ \f -> (Convive (hacer x) (hacer y))

vaca :: [Beta] -> IO ()
vaca = mapM_ f
  where
    f (Chamba x)       = do b <- x
                            f b
    f (Convive b1 b2)  = f b1 >> f b2
    f (Quieto)         = putStr "" 

instance Monad Otro where
  return x       = Otro $ \k -> k x
  --(Otro f) >>= g = Otro $ \k -> f (\x -> fromOtro (g x) k) 
  (Otro f) >>= g = Otro $ \k -> f (\x -> fromOtro (g x) k) 

-- k es del tipo (a -> Beta)
-- x es del tipo a
-- Otro ((a -> Beta) -> Beta)
-- \ k -> k x
-- k es del tipo a -> Beta
-- Entonces x es del tipo (a -> Beta) -> Beta para que cuando hagas k x Beta 
-- k es una funcion
-- (>>=) m a -> (a -> m b) -> m b
-- (>>=) Otro a -> (a -> Otro b) -> Otro b
-- (>>=) ((a -> Beta) -> Beta) -> (a -> ((b -> Beta) -> Beta)) -> ((b -> Beta) -> Beta)
-- (>>=) ((->) ((->) a Beta) Beta) -> ((->) a ((->) ((->) b Beta) Beta)) -> ((->) ((->) b Beta) Beta)
-- f = ((->) a Beta) && g = ((->) b Beta)
-- (>>=) ((->) f Beta) -> ((->) a ((->) g Beta)) -> ((->) g Beta)

-- (>>=) m a -> (a -> m b) -> m b
-- (>>=) ((->) r a) -> ((->) a ((->) r b)) -> ((->) r b)
-- a = Beta, b = Beta
-- (>>=) ((->) r Beta) -> ((->) Beta ((->) r Beta)) -> ((->) r Beta)
cartel :: Otro ()
cartel = pana (dale (clavo 42)) 
              (pana (dale (clavo 69))
                    (pana (dale (clavo 17)) 
                          (dale (clavo 23) >> chambea (putStrLn ""))))

quedo :: Otro a -> IO ()
quedo x = vaca [hacer x]

clavo :: Int -> String
clavo 17 = "/nlmce"
clavo 23 = "/y./p6"
clavo 42 = "htptuc2"
clavo 69 = "t:irofr"

dale :: String -> Otro ()
dale xs = mapM_ (chambea . putChar) xs
