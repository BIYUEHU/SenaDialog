module Command where

data AppCommand = New | Save Int | Load Int | Quit | Home | List Int | Help | Invalid String

parseAppCommand :: String -> AppCommand
parseAppCommand "n" = New
parseAppCommand "he" = Help
parseAppCommand "q" = Quit
parseAppCommand "h" = Home
parseAppCommand "li" = List 1
parseAppCommand "s" = Invalid "save requires an argument"
parseAppCommand "l" = Invalid "load requires an argument"
parseAppCommand s = case words s of
  ["s", n] -> tryToPositiveInt n Save
  ["l", n] -> tryToPositiveInt n Load
  ["li", n] -> tryToPositiveInt n List
  _ -> Invalid $ "unknown command: " ++ s
  where
    tryToPositiveInt :: String -> (Int -> AppCommand) -> AppCommand
    tryToPositiveInt s f = case reads s of
      [(n, "")] -> if n > 0 then f n else Invalid $ "argument must be positive: " ++ s
      _ -> Invalid $ "invalid argument: " ++ s
