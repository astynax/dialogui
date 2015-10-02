{-# LANGUAGE LambdaCase, CPP #-}

module Main where

import           UI.Dialogui
import           UI.Dialogui.TUI


main :: IO ()
main =
  let setup = writeLn "Echo service is ready!"
  in  runTUI setup echo


echo :: (Monad m) => Controller m Int
echo = Controller { initialize  = return 1
                  , finalize    = const $ return ()
                  , communicate = (return .) . communicate' }
  where
    communicate' cnt = \case
      ""   -> showHelp

      ":?" -> showHelp

      ":q" -> quit

      ":r" -> clear <> clearInput

      msg  -> setState (cnt + 1)
              <> clearInput
              <> write (show cnt)
              <> write ": "
              <> writeLn msg

    showHelp = write
               $ unlines [ "Use:"
                         , " \":?\" to show this help"
                         , " \":q\" to quit (Ctrl+D works same way)"
                         , " \":r\" to reset state"
                         , " <msg> to see an echo" ]
    clearInput = setInput ""
