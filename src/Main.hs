module Main where

import Src.Core

data MusicState = Playing | Stopped deriving (Eq, Show)

class AState a where
  getDisplay :: a -> Display

instance AState MusicState where
  getDisplay Playing = Static "状态: 播放中量: 75\n进度: 2:30 / 4:15"
  getDisplay Stopped = Static "状态: 已停止\n音量: 75%\n进度: 0:00 / 0:00"

processCommand :: String -> AppState MusicState -> IO (AppState MusicState)
processCommand cmd state
  | cmd == " " =
      let mState = if extra state == Playing then Stopped else Playing
       in pure $ state {display = getDisplay mState, extra = mState}
  | cmd == "q" =
      pure
        state
          { handler = Just $ \state _ -> do
              renderDisplay state (Static "Goodbye!")
              putStrLn ""
              pure ()
          }
  | cmd == "l" = renderDisplay state (Dynamic (["Loading |", "Loading /", "Loading -", "Loading \\"], 200, 5))
  | otherwise = pure state

-- setupTerminal :: IO ()
-- setupTerminal = do
--   hSetBuffering stdin LineBuffering
--   hSetBuffering stdout NoBuffering
--   hSetEcho stdin True

main :: IO ()
main = do
  -- setupTerminal
  getMainLoop processCommand $ AppState "音乐播放器控制台 - 命令: [空格]=切换播放/停止, [q]=退出, [l]=加载动画\n" "> " Nothing (Static "hello world") 0 Stopped