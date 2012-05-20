import GameState_Client
import Service_Types
import Network
import System.IO
import System.Environment (getArgs)

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
    clientFunc host port
    
