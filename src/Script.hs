module Script where

data ScriptCommand = Say String String | Title String | Prompt String | Confirm String | Choice [String] | Pause Int
