module Command (AppCommand (..), parseAppCommand) where

data AppCommand = New | Save Int | Load Int | Quit | Home | List Int | Help | Invalid String | Back | Enter

parseAppCommand :: String -> AppCommand
parseAppCommand "n" = New
parseAppCommand "he" = Help
parseAppCommand "q" = Quit
parseAppCommand "h" = Home
parseAppCommand "t" = List 1
parseAppCommand "s" = Invalid "save requires an argument"
parseAppCommand "l" = Invalid "load requires an argument"
parseAppCommand "b" = Back
parseAppCommand "e" = Enter
parseAppCommand "" = Enter
parseAppCommand s = case words s of
  ["s", n] -> tryToPositiveInt n Save
  ["l", n] -> tryToPositiveInt n Load
  ["li", n] -> tryToPositiveInt n List
  _ -> Invalid $ "unknown command: " ++ s
  where
    tryToPositiveInt :: String -> (Int -> AppCommand) -> AppCommand
    tryToPositiveInt str f = case reads str of
      [(n, "")] -> if n > 0 then f n else Invalid $ "argument must be positive: " ++ str
      _ -> Invalid $ "invalid argument: " ++ str
