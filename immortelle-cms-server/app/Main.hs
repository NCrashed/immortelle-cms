module Main where

import Data.Aeson
import Data.Monoid
import Data.Text (pack)
import Immortelle.CMS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Options.Applicative

import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS

data Options = Options {
  optsConfig  :: FilePath
, optsCommand :: Command
}

data Command =
    CommandListen
  | CommandExport FilePath
  | CommandImport FilePath

options :: Parser Options
options = Options
  <$> strArgument (
      metavar "CONFIG_PATH"
    )
  <*> subparser (
       command "listen" (info (pure CommandListen <**> helper) $ progDesc "Start web service")
    <> command "export" (info (exportArgs <**> helper) $ progDesc "Export DB data as JSON")
    <> command "import" (info (importArgs <**> helper) $ progDesc "Export DB data from JSON")
  )
  where
    exportArgs = CommandExport
      <$> strArgument (
            metavar "EXPORT_FILE"
          )
    importArgs = CommandImport
      <$> strArgument (
            metavar "IMPORT_FILE"
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
      let app = logger $ immortelleCmsApp env
      runSettings warpSettings app
    CommandExport filename -> do
      ps <- runServerMIO env $ runQuery ExportAll
      BS.writeFile filename $ encode ps
    CommandImport filename -> do
      res <- eitherDecodeStrict' . BS.toStrict <$> BS.readFile filename
      case res of
        Left e -> fail e
        Right ps -> runServerMIO env $ runUpdate $ ImportAll ps

-- | Make WAI logger from bind configuration
makeLogger :: Config -> Middleware
makeLogger Config{..} = if configDetailedLogging then logStdoutDev else logStdout
