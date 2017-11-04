module Main where

import Data.Monoid
import Data.Text (pack)
import Immortelle.CMS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative

import qualified Data.Text.IO as T

data Options = Options {
  optsConfig  :: FilePath
, optsCommand :: Command
}

data Command = CommandListen

options :: Parser Options
options = Options
  <$> strArgument (
      metavar "CONFIG_PATH"
    )
  <*> subparser (
       command "listen" (info (pure CommandListen <**> helper) $ progDesc "Start web service")
  )

main :: IO ()
main = startServer =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Starts Immortelle CMS server"
     <> header "immortelle-cms - a web server for product management for Immortelle" )

startServer :: Options -> IO ()
startServer Options{..} = do
  cfg <- loadConfig optsConfig
  env <- newEnv cfg
  case optsCommand of
    CommandListen -> do
      let logger = makeLogger cfg
          warpSettings = setPort (configPort cfg) defaultSettings
      T.putStrLn $ "Server started at http://" <> configHost cfg <> ":" <> (pack . show . configPort $ cfg)
      runSettings warpSettings $ logger $ immortelleCmsApp env

-- | Make WAI logger from bind configuration
makeLogger :: Config -> Middleware
makeLogger Config{..} = if configDetailedLogging then logStdoutDev else logStdout
