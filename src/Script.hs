module Src.Script (ScriptCommand (..), parseScript, getScript) where

data ScriptCommand = Say String String | Title String | Prompt String | Confirm String | Choice [String] | Pause Int

parseScript :: String -> [ScriptCommand]
parseScript source = map parseLine $ filter (not . null) $ lines source

parseLine :: String -> ScriptCommand
parseLine line = case tryGetDialog line of
  Just command -> command
  Nothing -> case parseCommand line of
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

tryGetDialog :: String -> Maybe ScriptCommand
tryGetDialog input = case break (== ':') input of
  (_, []) -> Nothing -- 没有找到冒号
  (before, ':' : after) -> Just $ Say before after

parseCommand :: String -> [String]
parseCommand input = parseTokens input []
  where
    parseTokens :: String -> [String] -> [String]
    parseTokens [] acc = reverse acc
    parseTokens (' ' : xs) acc = parseTokens (dropWhile (== ' ') xs) acc
    parseTokens ('"' : xs) acc =
      let (token, rest) = parseQuoted xs ""
       in parseTokens rest (token : acc)
    parseTokens ('\\' : x : xs) acc =
      let (token, rest) = parseUnquoted (x : xs) ""
       in parseTokens rest (token : acc)
    parseTokens xs acc =
      let (token, rest) = parseUnquoted xs ""
       in parseTokens rest (token : acc)

    parseQuoted :: String -> String -> (String, String)
    parseQuoted [] acc = (reverse acc, [])
    parseQuoted ('"' : xs) acc = (reverse acc, xs)
    parseQuoted ('\\' : x : xs) acc = parseQuoted xs (x : acc)
    parseQuoted (x : xs) acc = parseQuoted xs (x : acc)

    parseUnquoted :: String -> String -> (String, String)
    parseUnquoted [] acc = (reverse acc, [])
    parseUnquoted (' ' : xs) acc = (reverse acc, ' ' : xs)
    parseUnquoted ('"' : xs) acc = (reverse acc, '"' : xs)
    parseUnquoted ('\\' : x : xs) acc = parseUnquoted xs (x : acc)
    parseUnquoted (x : xs) acc = parseUnquoted xs (x : acc)