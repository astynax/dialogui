module UI.Dialogui (
  -- * UI

  -- ** Types
  UI, Output(..), Input,

  -- * Controller

  -- ** Types
  Controller(..), Action(..), Result, ScrollTarget(..),


  -- ** eDSL

  -- $example
  quit, clear,
  write, writeLn,
  setInput, setState,
  scrollToBegin, scrollToEnd,
  (<>),

  -- ** Other functions
  voidController, interact,

  -- * Simple pure state

  -- ** State type
  State(),

  -- ** Getters and modifiers
  finished, ctlState, uiState, uiInput,
  state, perform, finish,
  modUIState, modCtlState
  ) where

import           Data.Monoid ((<>))
import           Prelude     hiding (interact)


data Output t = Output { writeTo     :: String -> t
                       , clearOutput :: t
                       , scrollTo    :: ScrollTarget -> t }

{- | __U__ser __I__nterface which runs the 'Prelude.IO'-flavoured 'Controller'
as 'Prelude.IO'-action.
-}
type UI a = Controller IO a -> IO ()

-- | Position to scroll the ouput widget to.
data ScrollTarget = BeginOfText | EndOfText

-- | Controller's action.
data Action a = Write String
              | ClearOutput
              | SetInput String
              | SetState a
              | ScrollTo ScrollTarget
              | Quit

-- | User input.
type Input = String

-- | Result of evaluation of the user's 'Input' by 'Controller'.
type Result a = [Action a]

{- | Encapsulates the computation, dealing with 'Input'
and producing some 'Result'.
-}
data Controller m a =
  Controller { initialize  :: m a
               -- ^ Computation producing the initial state,
             , finalize    :: a -> m ()
               -- ^ Computation using the final state
               -- to do something before quit,
             , communicate :: a -> Input -> m (Result a)
               -- ^ Computation producing a couple of actions
               -- as reaction to the user input.
             }


{- | Pure State encapsulating both the state of 'UI'
and the state of 'Controller'.
-}
data State a b =
  State { finished :: Bool
          -- ^ Returns @True@ if controller returned a 'quit'
          -- and 'UI' must be stopped.
        , uiInput  :: Maybe String
          -- ^ Returns @Just x@ if value of the UI's input widget
          -- must be set to @x@.
        , uiState  :: a
          -- ^ Returns new state of 'UI'
        , ctlState :: b
          -- ^ Returns new state of 'Controller'
        }


-- | Controller which does nothing and have no state.
--   This is a good template for the simple effectful controllers.
voidController :: Monad m => Controller m ()
voidController = Controller { initialize  =                 return ()
                            , finalize    = const $         return ()
                            , communicate = const $ const $ return [] }


-- | Analogue of the good old 'Prelude.interact' but runnable with 'UI'.
interact :: (String -> String) -> Controller IO ()
interact f = voidController { communicate = const $ \x ->
                               return $ write (f x) <> setInput "" }

-- "Words" of the controller eDSL

{- $example Example:

@
answer :: Input -> Result a
answer []   = quit
answer name = clearOutput
              <> write "Hello, " <> write name <> writeLn "!"
              <> scrollToEnd
              <> setInput ""
@

-}

quit, clear, scrollToBegin, scrollToEnd :: Result a
write, writeLn, setInput :: String -> Result a
setState :: a -> Result a

-- | Commands 'UI' to quit.
quit     = [Quit]
-- | Commands 'UI' to clear the output.
clear    = [ClearOutput]

-- | Commands 'UI' to write some string to the output.
write    = (:[]) . Write
-- | Commands 'UI' to write some string to the output and end the line.
writeLn  = (:[]) . Write . (++ "\n")

-- | Commands 'UI' to set value of the input widget.
setInput = (:[]) . SetInput

-- | Commands 'UI' to set value of the Controller state.
setState = (:[]) . SetState

-- | Commands 'UI' to scroll the output widget to the begin of text.
scrollToBegin = [ScrollTo BeginOfText]
-- | Commands 'UI' to scroll the output widget to the end of text.
scrollToEnd   = [ScrollTo EndOfText]


-- Functions, helping to work with the pure state

-- | Produces the initial 'State'.
state :: a          -- ^ UI state
      -> b          -- ^ Controller state
      -> State a b
state = State False Nothing

-- | Performs the actions does some output and modifies the state.
perform :: Output (State a b -> State a b) -- ^ UI output
        -> [Action b]                      -- ^ Actions to perform
        -> State a b                       -- ^ Old state
        -> State a b                       -- ^ New state
perform out = flip (foldl apply)
  where
    apply s _ | finished s = s
    apply s Quit           = finish s
    apply s act            =
      case act of
        Write msg   -> writeTo out msg
        ClearOutput -> clearOutput out
        ScrollTo t  -> out `scrollTo` t
        SetState x  -> \s' -> s' { ctlState =      x }
        SetInput x  -> \s' -> s' { uiInput  = Just x }
        Quit        -> error "This case should't be reached!"
      $ s

modUIState  :: (a -> a) -> State a b -> State a b
modCtlState :: (b -> b) -> State a b -> State a b
finish      ::             State a b -> State a b
modUIState  f s = s { uiState  = f (uiState  s) }
modCtlState f s = s { ctlState = f (ctlState s) }
finish        s = s { finished = True }
