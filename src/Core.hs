module Src.Core (Display (..), AppState (..), render, renderDisplay, getMainLoop) where

import Control.Concurrent
import Control.Monad
import System.IO

data Display = Static String | Dynamic ([String], Int, Int) deriving (Show)

data AppState a = AppState
  { header :: String,
    footer :: String,
    handler :: Maybe (AppState a -> (AppState a -> IO ()) -> IO ()),
    display :: Display,
    displayLines :: Int,
    extra :: a
  }

moveCursorUp :: Int -> IO ()
moveCursorUp n = putStr $ "\ESC[" ++ show n ++ "A"

moveCursorToLineStart :: IO ()
moveCursorToLineStart = putStr "\ESC[1G"

clearFromCursorToEnd :: IO ()
clearFromCursorToEnd = putStr "\ESC[0J"

clearCurrentLine :: IO ()
clearCurrentLine = putStr "\ESC[2K"

findBackLineCount :: String -> Int
findBackLineCount [] = 0
findBackLineCount (c : cs) = if c == '\n' then 1 + findBackLineCount cs else findBackLineCount cs

render :: AppState a -> IO (AppState a)
render state =
  case display state of
    Static str -> do
      when (displayLines state > 0) $ do
        moveCursorUp $ displayLines state + findBackLineCount (header state)
        moveCursorToLineStart
        clearFromCursorToEnd

      putStr $ header state
      mapM_ putStrLn $ lines str
      putStr $ footer state

      hFlush stdout
      pure $
        state
          { displayLines = length $ lines str
          }
    Dynamic (lines, delay, count) ->
      let frame :: [String] -> AppState a -> IO (AppState a)
          frame [] state = renderDisplay state $ if count == 0 then Static "..." else Dynamic (lines, delay, count - 1)
          frame (line : rest) state = do
            state <- renderDisplay state (Static line)
            threadDelay $ delay * 1000
            frame rest state
       in frame lines state

renderDisplay :: AppState a -> Display -> IO (AppState a)
renderDisplay state display = render $ state {display = display}

getMainLoop :: (String -> AppState a -> IO (AppState a)) -> AppState a -> IO ()
getMainLoop f =
  loop
  where
    next state = do
      input <- getLine
      moveCursorUp 1
      state <- f input state
      loop state

    loop state = do
      state <- render state
      case handler state of
        Just cb -> cb (state {handler = Nothing}) next
        Nothing -> next state
