import GameState_Client
import Service_Types
import Network
import Network.Socket.Internal
import System.IO
import System.Environment (getArgs)
import Data.Word

-- Thrift libraries
import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Server

-- Constants (TODO)
port = 4390
host = "127.0.0.1"

testMove = Move (Just 0) (Just 1) (Just 2) (Just "dbl")

clientFunc :: HostName -> PortNumber -> IO ()
clientFunc host p = do
  putStrLn "Client go!"
  h <- connectTo host $ PortNumber p
  let proto = BinaryProtocol h
  processMove (proto,proto) testMove
  tFlush h
  putStrLn "Move sent"
  slots <- getLastUpdate (proto,proto)
  putStrLn "Receiving slots"
  print slots
  tClose h

main = do
    args <- getArgs
    let (port',host') = if (length args) == 1 
                        then (port,host)
                        else (Network.Socket.Internal.PortNum $ read (args !! 1)::PortNumber, args !! 2)
    clientFunc host port
    
