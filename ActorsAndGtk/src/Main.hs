{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}

import Graphics.UI.Gtk hiding (get)
import Control.Monad.IO.Class
import Data.Time.LocalTime
import qualified Control.Concurrent as C
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable,forkProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment
import Data.Typeable
import Data.Binary

data Message = Inc Int
             | View

data MessageRemote = IncRemote Int
                     | ViewRemote (SendPort Int)
                     deriving (Typeable)

{-! 
deriving instance Binary MessageRemote
!-}

controllerActor :: C.Chan Message -> SendPort MessageRemote -> Process()
controllerActor chan port = do
    msg <- liftIO . C.readChan $ chan
    case msg of
        (Inc n) -> sendChan port (IncRemote n)
        View    -> sendChan port (ViewRemote undefined) -- How could I get the sendport of this process
    controllerActor chan port

remoteActor :: Int -> ReceivePort MessageRemote-> Process ()
remoteActor acc rport = do
    msg <- receiveChan rport
    case msg of
        (IncRemote n) -> remoteActor (acc+n) rport
        (ViewRemote sport) -> do
                                sendChan sport acc
                                remoteActor acc rport


ignition :: C.Chan Message -> Process ()
ignition chan = do
    -- Fire the actor with the logic
    remoteSPort <- spawnChannelLocal $ remoteActor 0 
    -- Fire the actor that acts as a proxy for the GUI
    controllerSPort <- spawnChannelLocal $ \(rport :: ReceivePort MessageRemote) -> controllerActor chan remoteSPort
    return ()

main = do
   -- GUI 
   initGUI
   window <- windowNew
   incButton <- buttonNewWithLabel "Increment"
   viewButton <- buttonNewWithLabel "View"
   textBox <- entryNew
   vLayout <- vBoxNew True 2
   hLayout <- hBoxNew True 2
   onClicked incButton (onClckButton textBox)
   containerAdd window vLayout
   containerAdd vLayout hLayout
   containerAdd hLayout incButton
   containerAdd hLayout viewButton
   containerAdd vLayout textBox

   -- Actors
   [host,port] <- getArgs
   chan <- C.newChan 
   backend <- initializeBackend host port initRemoteTable 
   thisNode <- newLocalNode backend
   forkProcess thisNode (ignition chan)

   -- Handlers
   widgetShowAll window
   window `on` deleteEvent $ tryEvent (liftIO mainQuit)

   mainGUI


onClckButton :: Entry -> IO()
onClckButton en = do
    time <- getZonedTime -- Instead of this print the returned value from the remote process
    entrySetText en (show time)
