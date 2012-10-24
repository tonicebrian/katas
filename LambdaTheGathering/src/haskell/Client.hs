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

readOrders :: FilePath -> IO [Move]
readOrders filename = do
    content <- readFile filename
    let ws = map words $ lines content
    let mkMove desc = Move (Just a) (Just b) (Just c) (Just d)
            where
                a = nums !! 0
                b = nums !! 1
                c = nums !! 2
                d = desc !! 3
                nums = map (\x -> read x) $ take 3 desc
    return $ map mkMove ws

clientFunc :: HostName -> PortNumber -> FilePath ->  IO ()
clientFunc host p fileOrders = do
  putStrLn "Client go!"
  h <- connectTo host $ PortNumber p
  let proto = BinaryProtocol h
  orders <- readOrders fileOrders
  mapM_ (\m -> processMove (proto,proto) m >> tFlush h) orders
  putStrLn "Moves sent"
  slots <- getLastUpdate (proto,proto)
  putStrLn "Receiving slots"
  print slots
  tClose h

main = do
    args <- getArgs
    let (host',port',ordersFile) = if (length args == 0 )
                            then (host,port,"test/resources/Orders.txt")
                            else (args!!0, fromIntegral $ (read (args!!1) :: Integer),args !! 2)
    clientFunc host port ordersFile
    
