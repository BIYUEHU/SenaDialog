module Main where

import Command
import Control.Concurrent (threadDelay)
import Core
import Script

data GameState = Playing [ScriptCommand] Int | None

evaluate :: AppState GameState -> IO (AppState GameState)
evaluate state@(AppState {extra = Playing [] _}) = pure state -- finished all scripts
evaluate state@(AppState {extra = Playing (cmd : cmds) line}) = case cmd of
  Say who what -> pure $ state {display = Static $ who ++ ":" ++ what, extra = newExtra}
  Title msg -> pure $ state {display = Static $ "\x1b[1m" ++ msg ++ "\x1b[0m", extra = newExtra}
  Pause n -> do
    threadDelay $ n * 1000000
    pure $ state {extra = newExtra}
  _ -> pure state
  where
    newExtra = Playing cmds $ line + 1
evaluate state = pure state

processer :: AppCommand -> AppState GameState -> IO (AppState GameState)
processer cmd state@(AppState {extra = game}) = case cmd of
  New -> do
    script <- getScript "script.txt"
    evaluate $ state {extra = Playing script 0}
  Enter ->
    case game of
      Playing [] _ -> pure state {display = Static "Home"}
      None -> pure state
      _ -> evaluate $ state {extra = game}
  Quit -> pure state {handler = Just $ \_ _ -> putStrLn ""}
  Home -> pure state {display = Static "Home", extra = None}
  Invalid msg -> pure state {display = Static msg}
  _ -> pure state

main :: IO ()
main = do
  -- setupTerminal
  getMainLoop (processer . parseAppCommand) $ AppState "== Visual Novel ==\n" "> " Nothing (Static "hello world") 0 None