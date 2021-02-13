module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Discord
import Discord.Requests
import Discord.Types
import Language.JSScript hiding (main)
import Language.JSScript.AST
import Language.JSScript.Interpreter
import Language.JSScript.Parser
import System.Environment
import System.Timeout
import Text.Parsec
import Text.Parsec.Text

main :: IO ()
main = do
  tok <- pack <$> getEnv "JSSBOT_TOKEN"
  err <-
    runDiscord
      def
        { discordToken = tok,
          discordOnStart = liftIO $ putStrLn "JSSBot ready.",
          discordOnEvent = eventHandler
        }
  T.putStrLn err

parseCodeBlocks :: Parser [Text]
parseCodeBlocks = do
  many (pack <$> getBlock)
  where
    nonTick = noneOf "`"
    getBlock :: Parser String
    getBlock =
      skipMany nonTick
        *> ( try (between (char '`') (char '`') (many1 nonTick))
               <|> between (string "```") (string "```") (many1 nonTick)
           )

eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate m) = do
  myId <- _currentUser <$> readCache
  when (userId myId `elem` (userId <$> messageMentions m)) $ do
    liftIO $ T.putStrLn . messageText $ m
    case parse parseCodeBlocks "" (messageText m) of
      Left _ -> liftIO $ putStrLn "failed parse"
      Right blocks -> do
        dis <- ask
        let actions = mapMaybe parseBlock blocks
            prog = traverse (evalToDiscord dis (messageChannel m)) actions
        stdlib <- liftIO stdlibIO
        merr <- liftIO $ timeout (10 ^ 7) $ evalStateT (runExceptT prog) stdlib
        case merr of
          Nothing -> void $ restCall $ CreateMessage (messageChannel m) "```[computation timed out]```"
          Just (Left err) -> void $ restCall $ CreateMessage (messageChannel m) $ "```error: " <> err <> "```"
          _ -> pure ()
  where
    parseBlock btext =
      let b = unpack btext
       in case parse (expr <* eof) "jssbot" b of
            Right e -> Just $ Left e
            Left _ -> case parse (many1 stmt <* eof) "jssbot" b of
              Left _ -> Nothing
              Right s -> Just $ Right (StmtBlock s)

evalToDiscord :: DiscordHandle -> ChannelId -> Either Expr Stmt -> EvalM ()
evalToDiscord dis ch runme = do
  case runme of
    Left e -> do
      res <- eval e
      void $ liftIO $ flip runReaderT dis $ restCall $ CreateMessage ch $ "```" <> pack (anyToString res) <> "```"
    Right s -> exec s