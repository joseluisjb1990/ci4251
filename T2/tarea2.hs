import Control.Monad
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
                  fi <- fmap DS.fromList $ listOf1 $ elements $ DS.elems st
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
lambdaMoves nfa n = DS.fromList $ DS.foldr f [] (moves nfa)
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
destinations nfa c n = let lc  = DS.insert n (lambdaMoves nfa n)
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
runNFA nfa word = undefined

data NFAReject = Stuck (DS.Set NFANode) String
               | Reject (DS.Set NFANode)
               deriving (Show)

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

flow :: RWS NFA (DS.Set NFANode) NFARun Bool
flow = do nfa <- ask
          sta <- get
          put $ NFARun { w = tail(w sta), qs = DS.foldr (DS.union) DS.empty (DS.map (destinations nfa (head(w sta))) (qs sta)) }
          return $ True
