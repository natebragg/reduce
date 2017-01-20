module Main where

import Parser (term)
import DynSem (eval)

import Text.Parsec (parse)
import System.IO (hFlush, stdout)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad (forever)
import Control.Monad (mzero)

data Mode = NormalForm | Step

usage :: IO ()
usage = do n <- getProgName
           putStrLn $ "Usage: " ++ n ++ " [-ns]"

nf :: (Monad m) => a -> m a
nf = return

step :: (Show t) => ReaderT t (StateT a (MaybeT IO)) t
step = do t <- ask
          liftIO $ print t
          lift $ lift doGoOn
          return t

doGoOn :: MaybeT IO ()
doGoOn = do liftIO $ putStr "=> "
            liftIO $ hFlush stdout
            inp <- liftIO $ getLine
            case inp of
              x | x `elem` ["s", "step"] -> return ()
              x | x `elem` ["q", "quit"] -> mzero
              otherwise -> doGoOn

main :: IO ()
main = do args <- getArgs
          mode <- case args of
                    [] -> return NormalForm
                    ["-n"] -> return NormalForm
                    ["-s"] -> return Step
                    otherwise -> usage >> exitFailure
          forever $ do
            putStr "-> "
            hFlush stdout
            inp <- getLine
            case parse term "" inp of
              Left err -> putStrLn $ "error: " ++ show err
              Right t ->
                case mode of
                    NormalForm -> print =<< eval t nf
                    Step -> do t' <- runMaybeT $ eval t $ runReaderT step
                               maybe (return ()) print t'
