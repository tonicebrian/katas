import GameState
import GameState_Iface
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

-- Constants
port = 4390
host = "127.0.0.1"

data ServerCore = ServerCore

instance GameState_Iface ServerCore where
    processMove a (Just m) = print m
    processMove a Nothing = putStrLn "Recibido un Nothing"
    getLastUpdate a = do
             putStrLn "Recibido un Last update"
             return $ [Slot (Just 0) (Just 1) (Just "zero")]

main = do
    args <- getArgs
    let port' = if (length args == 0 )
                then port
                else fromIntegral $ (read (args!!0) :: Integer)
    runBasicServer ServerCore GameState.process port'
    putStrLn "Stopping the server"
    
