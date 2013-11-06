{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random (randomRIO)
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Char8
import Options.Applicative as Opt
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromMaybe, isJust, fromJust)
import Options.Applicative.Types
import System.Console.Haskeline
import Pipes
import qualified Pipes.Prelude as P

data Dice = Dice Int Int
    deriving Show

data Roll = Roll Dice (Maybe Int)
    deriving Show

parseDice = Dice <$> decimal
                 <*> (char 'd' *> decimal)

parseBonus = optional $ char '+' *> decimal

parseRoll = Roll <$> parseDice <*> parseBonus

maybeRoll :: String -> Maybe Roll
maybeRoll = maybeResult . flip feed "" . parse parseRoll . Char8.pack

rollReader :: String -> ReadM Roll
rollReader string = ReadM $ case maybeRoll string of
    Nothing -> Left $ ErrorMsg "Roll should be formatted NdK[+P], where N, K, and P are non-negative integers."
    Just roll -> Right roll

data RollOpts = RollOpts
    { verbose :: Bool
    , interactive :: Bool
    , roll :: Maybe Roll
    }

rollInfo :: ParserInfo RollOpts
rollInfo = info (helper <*> rollOpts)  
                (fullDesc <> progDesc "Simulate d20-style rolls" 
                          <> header "roll - a less fun way to roll dice")

rollOpts :: Opt.Parser RollOpts
rollOpts = RollOpts
    <$> switch (long "verbose" <> short 'v' <> metavar "VERBOSITY" <> help "Show all rolls")
    <*> switch (long "interactive" <> short 'i' <> metavar "INTERACTIVE" <> help "Interactive mode")
    <*> optional (nullOption $ long "roll" <> short 'r' <> reader rollReader <> metavar "ROLL")

main :: IO ()
main = do
    RollOpts verbose interactive roll <- execParser rollInfo
    when (isJust roll) $ runInputEffect $ each [fromJust roll] >-> roll' verbose
    when interactive $ runInteractive verbose

runInputEffect :: Effect (InputT IO) a -> IO a
runInputEffect = runInputT defaultSettings . runEffect

haskeline :: Producer String (InputT IO) ()
haskeline = forever $ do 
        mLine <- lift $ getInputLine "roll: "
        case mLine of
            Just line -> yield line
            Nothing -> return ()

parseRolls :: Monad m => Pipe String Roll m ()
parseRolls = forever $ do
    str <- await
    case maybeRoll str of
        Nothing -> parseRolls
        Just r -> yield r

runInteractive :: Bool -> IO ()
runInteractive verbose = runInputEffect $ haskeline >-> parseRolls >-> roll' verbose

rolls :: MonadIO m => Int -> Producer Int m ()
rolls size = forever $ liftIO (randomRIO (1,size)) >>= yield

yelp :: Bool -> Pipe Int Int (InputT IO) ()
yelp verbose = forever $ do
    val <- await
    when verbose $ lift $ outputStrLn . ("rolled a " ++) . show $ val
    yield val

roll' :: Bool -> Consumer Roll (InputT IO) ()
roll' verbose = forever $ do
    (Roll (Dice num size) bonus) <- await
    total <- lift $ P.sum $ rolls size >-> P.take num >-> yelp verbose
    if isJust bonus then do
        when verbose $ lift . outputStrLn $ "bonus is " ++ show (fromJust bonus)
        lift . outputStrLn $ "total: " ++ show (total + fromJust bonus)
                      else lift . outputStrLn $ "total: " ++ show total
