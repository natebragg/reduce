module Main where

import Lambda.Parser (term)
import Lambda.Semantics (eval)

import Impcore.Parser (imp)

import Text.Parsec (parse)
import System.IO (hFlush, stdout, stdin, readFile)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad (forever, when)
import Control.Monad (mzero)
import Data.List (isPrefixOf)

data Mode = NormalForm
          | Step
          | Impcore

usage :: IO ()
usage = do n <- getProgName
           putStrLn $ "Usage: " ++ n ++ " [-nsi] [filename]"

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
          let (opts, params) = span (isPrefixOf "-") args
          mode <- case opts of
                    [] -> return NormalForm
                    ["-n"] -> return NormalForm
                    ["-s"] -> return Step
                    ["-i"] -> return Impcore
                    otherwise -> usage >> exitFailure
          case params of
                    [] -> return ()
                    [fn] -> readFile fn >>= run mode
                    otherwise -> usage >> exitFailure
          forever $ do
            putStr "-> "
            hFlush stdout
            inp <- getLine
            run mode inp
    where run mode inp =
            case mode of
                Impcore -> runImpcore inp
                otherwise -> runLambda mode inp
          runImpcore inp =
            case parse imp "" inp of
              Left err -> putStrLn $ "error: " ++ show err
              Right t -> print t
          runLambda mode inp =
            case parse term "" inp of
              Left err -> putStrLn $ "error: " ++ show err
              Right t ->
                case mode of
                    NormalForm -> print =<< eval t nf
                    Step -> do t' <- runMaybeT $ eval t $ runReaderT step
                               maybe (return ()) print t'
