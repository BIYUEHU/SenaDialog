import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Data.Char (toLower)
import System.IO

-- 音乐播放状态
data MusicState = Playing | Stopped deriving (Eq, Show)

-- 应用状态
data AppState = AppState
  { musicState :: MusicState,
    shouldExit :: Bool,
    displayLines :: Int
  }
  deriving (Show)

-- 初始状态
initialState :: AppState
initialState = AppState Stopped False 0

-- 移动光标到相对当前位置的上方n行
moveCursorUp :: Int -> IO ()
moveCursorUp n = putStr $ "\ESC[" ++ show n ++ "A"

-- 移动光标到行首并清除当前行
clearCurrentLine :: IO ()
clearCurrentLine = putStr "\ESC[1G\ESC[K"

-- 清除从当前位置到屏幕底部的所有内容
clearFromCursorToEnd :: IO ()
clearFromCursorToEnd = putStr "\ESC[0J"

-- 保存/恢复光标位置
saveCursor :: IO ()
saveCursor = putStr "\ESC[s"

restoreCursor :: IO ()
restoreCursor = putStr "\ESC[u"

-- 将多行字符串按\n分割
splitLines :: String -> [String]
splitLines = lines

-- 静态内容渲染：完全重新渲染固定内容
renderStaticContent :: AppState -> String -> IO AppState
renderStaticContent state content = do
  let contentLines = splitLines content

  -- 如果不是第一次渲染，需要清除之前的内容
  when (displayLines state > 0) $ do
    moveCursorUp (displayLines state + 1) -- +1 包含输入行
    clearCurrentLine
    clearFromCursorToEnd

  -- 输出新内容
  mapM_ putStrLn contentLines

  -- 用户输入提示行
  putStr ": "
  hFlush stdout

  let newDisplayLines = length contentLines
  return state {displayLines = newDisplayLines}

-- 简单的旋转动画（参考你的spinner实现）
spinnerAnimation :: MVar Bool -> IO ()
spinnerAnimation stopVar = do
  let frames = cycle ["-", "\\", "|", "/"]
  loop frames
  where
    loop (currentFrame : remainingFrames) = do
      -- 检查是否需要停止
      shouldStop <- tryReadMVar stopVar
      case shouldStop of
        Just True -> return ()
        _ -> do
          -- 保存光标位置，清除当前行，显示动画帧
          putStr $ "\r加载中... " ++ currentFrame
          hFlush stdout
          threadDelay 200000 -- 200ms 延迟
          loop remainingFrames

-- 加载动画演示
showLoadingAnimation :: IO ()
showLoadingAnimation = do
  stopVar <- newEmptyMVar

  -- 启动动画线程
  animationThread <- forkIO (spinnerAnimation stopVar)

  -- 等待用户按键
  putStrLn "\n按回车键停止动画..."
  _ <- getLine

  -- 停止动画
  putMVar stopVar True
  killThread animationThread

  -- 清除动画行
  putStr "\r\ESC[K"
  putStrLn "动画已停止"

-- 获取状态信息
getStatusContent :: MusicState -> String
getStatusContent Playing =
  "音乐播放器控制台 - 命令: [空格]=切换, [q]=退出, [l]=加载动画\n状态: 播放中\n音量: 75%\n进度: 2:30 / 4:15"
getStatusContent Stopped =
  "音乐播放器控制台 - 命令: [空格]=切换, [q]=退出, [l]=加载动画\n状态: 已停止\n音量: 75%\n进度: 0:00 / 0:00"

-- 处理用户输入命令
processCommand :: String -> AppState -> IO AppState
processCommand cmd state = do
  case cmd of
    " " -> do
      let newMusicState = toggleMusic (musicState state)
      renderStaticContent state (getStatusContent newMusicState)
    "q" -> do
      return state {shouldExit = True}
    "l" -> do
      -- 显示当前状态
      newState <- renderStaticContent state (getStatusContent (musicState state))

      -- 显示加载动画
      showLoadingAnimation

      -- 动画结束后重新显示状态
      renderStaticContent newState (getStatusContent (musicState state))
    _ -> renderStaticContent state (getStatusContent (musicState state))
  where
    toggleMusic Playing = Stopped
    toggleMusic Stopped = Playing

-- 主循环
mainLoop :: AppState -> IO ()
mainLoop state = do
  if shouldExit state
    then do
      putStrLn "\n再见！"
      return ()
    else do
      -- 读取用户输入
      input <- getLine
      newState <- processCommand input state
      mainLoop newState

-- 设置终端模式
setupTerminal :: IO ()
setupTerminal = do
  -- LineBuffering: 按行缓冲输入，等待回车键才处理
  hSetBuffering stdin LineBuffering
  -- NoBuffering: 输出无缓冲，立即显示到屏幕
  hSetBuffering stdout NoBuffering
  -- True: 允许输入回显，能看到输入的字符
  hSetEcho stdin True

-- 主函数
main :: IO ()
main = do
  setupTerminal

  -- 初始渲染
  initialState' <- renderStaticContent initialState (getStatusContent (musicState initialState))
  mainLoop initialState'