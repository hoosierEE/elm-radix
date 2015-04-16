import Graphics.Element exposing (..)
import Task exposing (..)

main =
  Signal.map show hello.signal

hello : Signal.Mailbox String
hello = Signal.mailbox "loading"

port rh : Task x ()
port rh =
  (Task.succeed "Hello world") `andThen` Signal.send hello.address

port runHello : Signal String
port runHello = hello.signal
