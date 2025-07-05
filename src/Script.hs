module Src.Script (ScriptCommand (..), parseScript, getScript) where

data ScriptCommand = Say String String | Title String | Prompt String | Confirm String | Choice [String] | Pause Int

parseScript :: String -> [ScriptCommand]
parseScript source = map parseLine $ filter (not . null) $ lines source

parseLine :: String -> ScriptCommand
parseLine line = case words line of
  ["say", name, message] -> Say name message
  ["title", title] -> Title title
  ["prompt", message] -> Prompt message
  ["confirm", message] -> Confirm message
  ["choice", options] -> Choice (words options)
  ["pause", seconds] -> Pause (read seconds)
  _ -> error $ "Invalid script command: " ++ line

getScript :: String -> IO [ScriptCommand]
getScript filename = do
  source <- readFile filename
  return $ parseScript source