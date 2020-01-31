-- Morra.hs

-- 1. Write the game Morra7 using StateT and IO. The state being accumulated is the score of the player and the computer AI opponent. To start, make the computer choose its play randomly. On exit, report the scores for the player and the computer, congratulating the winner.

-- done.

-- 2. Add a human vs. human mode to the game with interstitial screens between input prompts so the players can change out of the hot seat without seeing the other player’s answer.

-- done

-- 3. Improve the computer AI slightly by making it remember 3-grams of the player’s behavior, adjusting its answer instead of deciding randomly when the player’s behavior matches a known behavior.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.List
  (find, intercalate, groupBy, nub, sort)
import System.Random

newtype Name =
  Name { getName :: String }
  deriving (Eq, Show)

newtype ShortName =
  ShortName { getShortName :: Char }
  deriving (Eq, Show)

data Player =
    Player Name ShortName
  | Computer Name ShortName
  deriving (Eq, Show)

data Role =
    Odds Player
  | Evens Player
  deriving (Eq, Show)

newtype Move = Move { getMv :: Int }
  deriving (Eq, Show, Num, Ord,
            Enum, Real, Integral)

data Profile =
  Profile { role :: Role
          , score :: Int
          , priorMoves :: [Move]
  } deriving (Eq, Show)

type Profiles = (Profile, Profile)
type Game = StateT Profiles
              (ReaderT Option IO)

boolToRole :: Bool -> Player -> Role
boolToRole True = Evens
boolToRole False = Odds

newComputer :: Bool -> Role
newComputer b =
  boolToRole b $ Computer
    (Name "Computer")
    (ShortName 'C')

newPlayer :: Bool -> String -> Char -> Role
newPlayer b n c =
  boolToRole b $ Player (Name n) (ShortName c)

defaultPlayer :: Bool -> Role
defaultPlayer b = newPlayer b "Player" 'P'

getNewPlayer :: Bool -> IO Role
getNewPlayer b = do
  putStrLn "Enter a player name: "
  nm <- getLine
  if not (null nm)
  then return $ newPlayer b nm $ head nm
  else getNewPlayer b

flipCoin :: IO Bool
flipCoin = randomIO

newGame :: Option -> IO Profiles
newGame opt = do
  b <- flipCoin
  (r1, r2) <- case opt of
    TwoPeople -> (,) <$> getNewPlayer b
                     <*> getNewPlayer (not b)
    _  -> return (newComputer $ not b,
                 defaultPlayer b)
  return (Profile r1 0 [],
          Profile r2 0 [])

getPlayerMove :: IO Move
getPlayerMove = Move . read <$> getLine

boolToMove :: Bool -> Int
boolToMove True = 2
boolToMove False = 1

getNextMoveProf :: Game Profile
getNextMoveProf = do
  (prof1, prof2) <- get
  let l1 = length . priorMoves $ prof1
      l2 = length . priorMoves $ prof2
  return $ if l1 <= l2
           then prof1
           else prof2

getPrevMoveProf :: Game Profile
getPrevMoveProf = do
  (prof1, prof2) <- get
  nextProf <- getNextMoveProf
  return $ if nextProf == prof1
           then prof2
           else prof1

isComputer :: Player -> Bool
isComputer (Computer _ _) = True
isComputer _              = False

type Tri = (Move, Move, [Move])

defaultTrigrams :: [Tri]
defaultTrigrams =
  [(Move x, Move y, []) | x <- [1, 2], y <- [1, 2]]

similarTrigrams :: Tri -> Tri -> Bool
similarTrigrams (a, b, _) (c, d, _) =
  a == c && b == d

subsequencesOfLength :: Int -> [a] -> [[a]]
subsequencesOfLength n xs
  | length (take n xs) < n   = []
  | otherwise =
    take n xs : subsequencesOfLength n (drop 1 xs)

populateTrigrams :: [Move] -> [Tri]
populateTrigrams =
    map (simplifyHist . foldSimilarTups)
  . groupBy similarTrigrams
  . map (\(a:b:c:_) -> (c, b, [a]))
  . sort
  . subsequencesOfLength 3
  where
    foldSimilarTups =
      foldr1 (\(_, _, cs) (d, e, fs)
                       -> (d, e, cs <> fs))
    simplifyHist (a, b, cs) = (a, b, nub cs)


predNextMove :: [Move]
             -> Move
predNextMove hist@(a:b:xs) =
  let tris = populateTrigrams hist
      mMatch = find (similarTrigrams (b, a, [])) tris
  in maybe a (head . \(_, _, xs) -> xs) mMatch

calcWinMove :: Move -> Profile
            -> Move
calcWinMove oppMove opp =
  let oppEven         = evens $ role opp
      evens (Evens _) = True
      evens _         = False
      oppMoveEven     = even oppMove
      best
        | oppEven /= oppMoveEven = 2
        | otherwise              = 1
  in Move best

randomMove :: IO Move
randomMove = do
  b <- randomIO
  let m = boolToMove b
  print m
  return $ Move m

trigramMove :: Game Move
trigramMove = do
  opp <- getPrevMoveProf
  let pri = priorMoves opp
      trigramCand = take 3 pri
      triTup (a:b:c:_) = (c, b, a)
  if length trigramCand < 3
  then liftIO randomMove
  else do
    let opNextMove = predNextMove pri
        winMove = calcWinMove opNextMove opp
    liftIO . print . getMv $ winMove
    return winMove

solicitMove :: Game Move
solicitMove = do
  prof <- getNextMoveProf
  opt <- lift ask
  let isComp = isComputer . roleToPlayer . role $ prof
  if not isComp
  then liftIO getPlayerMove
  else if opt == TrigramComputer
       then trigramMove
       else liftIO randomMove

roleToPlayer :: Role -> Player
roleToPlayer (Odds p) = p
roleToPlayer (Evens p) = p

playerToName :: Player -> Name
playerToName (Player n _) = n
playerToName (Computer n _) = n

playerToShortName :: Player -> ShortName
playerToShortName (Player _ s) = s
playerToShortName (Computer _ s) = s

roleToShortName :: Role -> ShortName
roleToShortName = playerToShortName . roleToPlayer

morraPlay :: Role -> Game Move
morraPlay r = do
  liftIO $ do
    putStr $ show
           . getShortName
           . roleToShortName $ r
    putStr ": "
  move <- solicitMove
  opt <- lift ask
  case opt of
    TwoPeople -> do
      liftIO . replicateM_ 50 $ putStrLn ""
      return move
    _         -> return move

isEven :: Role -> Bool
isEven (Evens _) = True
isEven _         = False

roundWinner :: Role -> Move
            -> Role -> Move
            -> Role
roundWinner r1 i1 r2 i2
  | isEven r1 /= odd (i1 + i2) = r1
  | otherwise                  = r2

printRoundWin :: Player -> IO ()
printRoundWin p = do
  putStr "- "
  putStr $ show
         . getShortName
         . playerToShortName
         $ p
  putStrLn " wins"

bumpScore :: Profile -> Profile
bumpScore (Profile r s pm) =
  Profile r (s+1) pm

recordMove :: Move -> Profile -> Profile
recordMove i (Profile r s pm) =
  Profile r s (i : pm)

morraRound :: Game Profile
morraRound = do
  (prof1, prof2) <- get
  let r1 = role prof1
      r2 = role prof2

  i1 <- morraPlay r1
  modify $ first (recordMove i1)

  i2 <- morraPlay r2
  modify $ second (recordMove i2)

  let r = roundWinner r1 i1 r2 i2
  liftIO . printRoundWin . roleToPlayer $ r
  if r == role prof1
    then do
      modify (first bumpScore)
      fst <$> get
    else do
      modify (second bumpScore)
      snd <$> get


checkGameOver :: Game Bool
checkGameOver = do
  (prof1, prof2) <- get
  return $ score prof1 == 3
        || score prof2 == 3

explainPlayerName :: Player -> String
explainPlayerName p =
  "-- " <>
  (show . getShortName . playerToShortName $ p) <>
  " is " <>
  (show . getName . playerToName $ p)

explainRoles :: [Role] -> String
explainRoles roles =
  "-- " <>
  intercalate ", " (fmap explainRole roles) <>
  "."
  where
    explainRole (Odds p) =
      (getName . playerToName $ p) <> " is odds"
    explainRole (Evens p) =
      (getName . playerToName $ p) <> " is evens"

introGame :: Game ()
introGame = do
  (prof1, prof2) <- get
  liftIO $ do
    putStrLn . explainPlayerName . roleToPlayer $ role prof1
    putStrLn . explainPlayerName . roleToPlayer $ role prof2
    putStrLn . explainRoles $ [role prof1, role prof2]

reportScore :: Profile -> String
reportScore (Profile r s _) =
  (getName . playerToName . roleToPlayer $ r)
    <> " scored "
    <> show s
    <> " points."

congratulateWinner :: Role -> String
congratulateWinner r =
  "Congratulations! " <>
  (getName . playerToName . roleToPlayer $ r) <>
  " won."

exitGame :: Game ()
exitGame = do
  (prof1, prof2) <- get
  liftIO $ do
    putStrLn . reportScore $ prof1
    putStrLn . reportScore $ prof2
    putStrLn . congratulateWinner $
      if score prof1 > score prof2
      then role prof1
      else role prof2


playMorraGame :: Game ()
playMorraGame = do
  introGame
  playG
  exitGame
 where
  playG = do
    morraRound
    gameOver <- checkGameOver
    unless gameOver playG

playMorra :: Option -> IO ()
playMorra opt = do
  scores <- newGame opt
  let rtas = runStateT playMorraGame scores
  fst <$> runReaderT rtas opt

data Option =
    TwoPeople
  | RandComputer
  | TrigramComputer
  deriving (Eq, Show)
