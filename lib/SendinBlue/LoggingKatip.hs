{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI Version: 3.0.1
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : SendinBlue.LoggingKatip
Katip Logging functions
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SendinBlue.LoggingKatip where

import qualified Control.Exception.Safe as E
import qualified Control.Monad.IO.Class as P
import qualified Control.Monad.Trans.Reader as P
import qualified Data.Text as T
import qualified Lens.Micro as L
import qualified System.IO as IO

import Data.Text (Text)
import GHC.Exts (IsString(..))

import qualified Katip as LG

-- * Type Aliases (for compatibility)

-- | Runs a Katip logging block with the Log environment
type LogExecWithContext = forall m. P.MonadIO m =>
                                    LogContext -> LogExec m

-- | A Katip logging block
type LogExec m = forall a. LG.KatipT m a -> m a

-- | A Katip Log environment
type LogContext = LG.LogEnv

-- | A Katip Log severity
type LogLevel = LG.Severity

-- * default logger

-- | the default log environment
initLogContext :: IO LogContext
initLogContext = LG.initLogEnv "SendinBlue" "dev"

-- | Runs a Katip logging block with the Log environment
runDefaultLogExecWithContext :: LogExecWithContext
runDefaultLogExecWithContext = LG.runKatipT

-- * stdout logger

-- | Runs a Katip logging block with the Log environment
stdoutLoggingExec :: LogExecWithContext
stdoutLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stdout
stdoutLoggingContext :: LogContext -> IO LogContext
stdoutLoggingContext cxt = do
    handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stdout (LG.permitItem LG.InfoS) LG.V2
    LG.registerScribe "stdout" handleScribe LG.defaultScribeSettings cxt

-- * stderr logger

-- | Runs a Katip logging block with the Log environment
stderrLoggingExec :: LogExecWithContext
stderrLoggingExec = runDefaultLogExecWithContext

-- | A Katip Log environment which targets stderr
stderrLoggingContext :: LogContext -> IO LogContext
stderrLoggingContext cxt = do
    handleScribe <- LG.mkHandleScribe LG.ColorIfTerminal IO.stderr (LG.permitItem LG.InfoS) LG.V2
    LG.registerScribe "stderr" handleScribe LG.defaultScribeSettings cxt

-- * Null logger

-- | Disables Katip logging
runNullLogExec :: LogExecWithContext
runNullLogExec le (LG.KatipT f) = P.runReaderT f (L.set LG.logEnvScribes mempty le)

-- * Log Msg

-- | Log a katip message
_log :: (Applicative m, LG.Katip m) => Text -> LogLevel -> Text -> m ()
_log src level msg = do
  LG.logMsg (fromString $ T.unpack src) level (LG.logStr msg)

-- * Log Exceptions

-- | re-throws exceptions after logging them
logExceptions
  :: (LG.Katip m, E.MonadCatch m, Applicative m)
  => Text -> m a -> m a
logExceptions src =
  E.handle
    (\(e :: E.SomeException) -> do
       _log src LG.ErrorS ((T.pack . show) e)
       E.throw e)

-- * Log Level

levelInfo :: LogLevel
levelInfo = LG.InfoS

levelError :: LogLevel
levelError = LG.ErrorS

levelDebug :: LogLevel
levelDebug = LG.DebugS

