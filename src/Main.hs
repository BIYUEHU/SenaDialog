module Main where

import Control.Concurrent (threadDelay)
import Src.Command
import Src.Core
import Src.Script

-- data GameState = GameState
--   { scripts :: [ScriptCommand]
--   }
type GameState = Maybe [ScriptCommand]

evaluate :: AppState GameState -> IO (AppState GameState)
evaluate state@(AppState {extra = Just []}) = pure state -- finished all scripts
evaluate state@(AppState {extra = Just (cmd : cmds)}) = case cmd of
  Say who what -> pure $ state {display = Static $ who ++ ":" ++ what, extra = Just cmds}
  Title msg -> pure $ state {display = Static $ "\x1b[1m" ++ msg ++ "\x1b[0m", extra = Just cmds}
  Pause n -> do
    threadDelay $ n * 1000000
    pure $ state {extra = Just cmds}
evaluate state = pure state

processCommand :: AppCommand -> AppState GameState -> IO (AppState GameState)
processCommand cmd state@(AppState {extra = game}) = case cmd of
  New -> do
    script <- getScript "script.txt"
    evaluate $ state {extra = Just script}
  Enter ->
    case game of
      Just [] -> pure state {display = Static "Home"}
      Nothing -> pure state
      _ -> evaluate $ state {extra = game}
  Quit -> pure state {handler = Just $ \_ _ -> putStrLn ""}
  Home -> pure state {display = Static "Home", extra = Nothing}
  Invalid msg -> pure state {display = Static msg}
  _ -> pure state

-- \| cmd == " " =
--     let mState = if extra state == Playing then Stopped else Playing
--    pure $ state {display = getDisplay mState, extra = mState}
-- \| cmd == "q" =
--     pure
--       state
--         { handler = Just $ \state _ -> do
--             renderDisplay state (Static "Goodbye!")
--             putStrLn ""
--             pure ()
--         }
-- \| cmd == "l" = renderDisplay state (Dynamic (["Loading |", "Loading /", "Loading -", "Loading \\"], 200, 5))
-- \| otherwise = pure state

-- setupTerminal :: IO ()
-- setupTerminal = do
--   hSetBuffering stdin LineBuffering
--   hSetBuffering stdout NoBuffering
--   hSetEcho stdin True

main :: IO ()
main = do
  -- setupTerminal
  getMainLoop (processCommand . parseAppCommand) $ AppState "== Visual Novel ==\n" "> " Nothing (Static "hello world") 0 Nothing