{-# LANGUAGE LambdaCase #-}
{- | The __TUI__ (__T__ext __U__ser __I__nterface) works like the
command line of the OS-shell or GHCi. This example runs a
'UI.Dialogui.voidController' using TUI:

>>> let setup = writeLn "Hello" <> writeLn "World"
>>> runTUI setup voidController
Hello
World
> ^D
Bye!
>>>

(As you can see, app was stopped by pressing the @Ctrl+D@)
-}

module UI.Dialogui.TUI (runTUI) where

import           System.IO       (hFlush, stdout)
import           System.IO.Error (catchIOError, isEOFError)

import           UI.Dialogui


type TUIState a = State String a


tuiOutput :: Output (TUIState a -> TUIState a)
tuiOutput =
  Output
  { writeTo = \msg -> modUIState (++ msg)
  , clearOutput = modUIState (const [])
  , scrollTo = const id  -- TUI can't scroll
  }


{- | Returns the 'UI.Dialogui.UI', which preforms some setup (list of Action's)
just before starting the interaction with User.
-}
runTUI
  :: [Action a]  -- ^ Setup
  -> UI a  -- ^ Resulting UI
runTUI setup ctl =
  loop . perform tuiOutput setup . state "" =<< initialize ctl
  where
    loop s
      | finished s =
        finalize ctl (ctlState s)
        >> putStrLn "\nBye!"

      | otherwise = do
        putStr (uiState s)

        let s' = modUIState (const []) s

        mbInput <- getInput

        loop =<< maybe
          (return $ finish s')
          (\input -> do
              actions <- communicate ctl (ctlState s') input
              return $ perform tuiOutput actions s'
          )
          mbInput


getInput :: IO (Maybe String)
getInput =
  catchIOError
  (putStr "> " >> hFlush stdout >> Just <$> getLine)
  $ \e ->
    if isEOFError e
    then return Nothing
    else ioError e
