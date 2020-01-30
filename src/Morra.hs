-- Morra.hs

-- 1. Write the game Morra7 using StateT and IO. The state being accumulated is the score of the player and the computer AI opponent. To start, make the computer choose its play randomly. On exit, report the scores for the player and the computer, congratulating the winner.

-- done.

-- 2. Add a human vs. human mode to the game with interstitial screens between input prompts so the players can change out of the hot seat without seeing the other player’s answer.



module Morra where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.List (intercalate)
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

type Score = (Role, Int)
type Scores = (Score, Score)

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

type Game = StateT Scores
              (ReaderT Option IO)

flipCoin :: IO Bool
flipCoin = randomIO

newGame :: Option -> IO Scores
newGame RandComputer = do
  b <- flipCoin
  let player = defaultPlayer b
      comp = newComputer $ not b
  return ((player, 0), (comp, 0))
newGame TwoPeople = do
  b <- flipCoin
  r1 <- getNewPlayer b
  r2 <- getNewPlayer $ not b
  return ((r1, 0), (r2, 0))

getPlayerMove :: IO Int
getPlayerMove = read <$> getLine

boolToMove :: Bool -> Int
boolToMove True = 2
boolToMove False = 1

solicitMove :: Player -> IO Int
solicitMove (Computer _ _) = do
  i <- boolToMove <$> randomIO
  print i
  return i
solicitMove (Player _ _) = getPlayerMove

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

morraPlay :: Role -> Game Int
morraPlay r = do
  move <- liftIO $ do
    putStr $ show
           . getShortName
           . roleToShortName $ r
    putStr $ ": "
    solicitMove . roleToPlayer $ r
  opt <- lift ask
  case opt of
    TwoPeople -> (liftIO $ replicateM_ 50
                        $ putStrLn "")
                 *> return move
    _         -> return move

isEven :: Role -> Bool
isEven (Evens _) = True
isEven _         = False

roundWinner :: (Role, Int)
            -> (Role, Int)
            -> Role
roundWinner (r1, i1) (r2, i2)
  | isEven r1 /= odd (i1 + i2) = r1
  | otherwise                  = r2

printRoundWin :: Player -> IO ()
printRoundWin p = do
  putStr $ "- "
  putStr $ show
         . getShortName
         . playerToShortName
         $ p
  putStrLn " wins"

morraRound :: Game Score
morraRound = do
  ((r1, s1), (r2, s2)) <- get
  i1 <- morraPlay r1
  i2 <- morraPlay r2
  let r = roundWinner (r1, i1) (r2, i2)
  liftIO . printRoundWin . roleToPlayer $ r
  if r == r1
    then put ((r1, s1 + 1), (r2, s2))
         *> fmap fst get
    else put ((r1, s1), (r2, s2 + 1))
         *> fmap snd get

checkGameOver :: Game Bool
checkGameOver = do
  ((_, i1), (_, i2)) <- get
  return $ i1 == 3 || i2 == 3

explainPlayerName :: Player -> String
explainPlayerName p =
  "-- " <>
  (show . getShortName . playerToShortName $ p) <>
  " is " <>
  (show . getName . playerToName $ p)

explainRoles :: [Role] -> String
explainRoles roles =
  "-- " <>
  (intercalate ", " $ fmap explainRole roles) <>
  "."
  where
    explainRole (Odds p) =
      (getName . playerToName $ p) <> " is odds"
    explainRole (Evens p) =
      (getName . playerToName $ p) <> " is evens"

introGame :: Game ()
introGame = do
  ((r1, _), (r2, _)) <- get
  liftIO $ putStrLn
         . explainPlayerName
         . roleToPlayer $ r1
  liftIO $ putStrLn
         . explainPlayerName
         . roleToPlayer $ r2
  liftIO $ putStrLn
         . explainRoles
         $ [r1, r2]

reportScore :: Score -> String
reportScore (r, s) =
  (getName . playerToName . roleToPlayer $ r) <>
  " scored " <> show s <> " points."

congratulateWinner :: Role -> String
congratulateWinner r =
  "Congratulations! " <>
  (getName . playerToName . roleToPlayer $ r) <>
  " won."

exitGame :: Game ()
exitGame = do
  ((r1, s1), (r2, s2)) <- get
  liftIO $ do
    putStrLn . reportScore $ (r1, s1)
    putStrLn . reportScore $ (r2, s2)
    putStrLn . congratulateWinner $
      if s1 > s2
      then r1
      else r2


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
  -- | TrigramComputer
  deriving (Eq, Show)
