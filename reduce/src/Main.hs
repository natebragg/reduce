module Main where

import Lambda.Parser (term)
import qualified Lambda.Semantics as L (eval)

import Impcore.Parser (imp)
import qualified Impcore.Semantics as I (eval)

import qualified Impcore.Interp as II (eval)

import Text.Parsec (parse)
import System.IO (hFlush, stdout, stdin, readFile)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Monad (forever, when)
import Control.Monad (mzero)
import Data.List (isPrefixOf)

data Mode = NormalForm
          | Step

data Lang = Lambda
          | Impcore
          | ImpcoreInterp

usage :: IO ()
usage = do n <- getProgName
           putStrLn $ "Usage: " ++ n ++ " [-nsid] [filename]"

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
          (mode, lang) <- case opts of
                    []     -> return (NormalForm, Lambda)
                    ["-n"] -> return (NormalForm, Lambda)
                    ["-s"] -> return (Step,       Lambda)
                    ["-i"] -> return (NormalForm, Impcore)
                    ["-d"] -> return (Step,       Impcore)
                    ["-t"] -> return (NormalForm, ImpcoreInterp)
                    otherwise -> usage >> exitFailure
          case params of
                    [] -> return ()
                    [fn] -> readFile fn >>= run lang mode
                    otherwise -> usage >> exitFailure
          forever $ do
            putStr "-> "
            hFlush stdout
            inp <- getLine
            run lang mode inp
    where run lang mode inp =
            case lang of
                Impcore -> runParser (parse imp) I.eval I.eval mode inp
                Lambda  -> runParser (parse term) L.eval L.eval mode inp
                ImpcoreInterp -> case (parse imp) "" inp of
                                   Left err -> putStrLn $ "error: " ++ show err
                                   Right t -> print $ II.eval t
          runParser p eval eval2 mode inp =
            case p "" inp of
              Left err -> putStrLn $ "error: " ++ show err
              Right t ->
                case mode of
                    NormalForm -> print =<< eval t nf
                    Step -> do t' <- runMaybeT $ eval2 t $ runReaderT step
                               maybe (return ()) print t'
