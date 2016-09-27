import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket.ByteString (recv, sendAll)


--type Handler = ( S.ByteString, (S.ByteString -> S.ByteString ) )

--handleDave :: S.ByteString -> S.ByteString
--handleDave arg = C8.tail arg

-- contrived processing of route (we just get req.body.arg's tail)
handlers = ("dave\r\n", \arg -> tail arg) : ("kev\r\n", \arg -> reverse arg) : []

dispatchRequest :: Socket -> S.ByteString -> IO ()
dispatchRequest conn msg = do
    -- let converted = UTF8.fromString "wobbalobbaΩdingdong"
    -- let handler = 
    -- let trimmed
    let param = "testarg"
    case lookup (C8.unpack msg) handlers of Just handler -> sendAll conn $ C8.pack $ handler param
                                            Nothing      -> sendAll conn $ C8.pack "not found"
main :: IO ()
main = withSocketsDo $ do
        addrinfos <- getAddrInfo
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing (Just "3999")
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bind sock (addrAddress serveraddr)
        listen sock 1
        (conn, _) <- accept sock
        talk conn
        close conn
        close sock

    where
        talk :: Socket -> IO ()
        talk conn = do
            msg <- recv conn 1024
            unless (S.null msg) $ dispatchRequest conn msg >> talk conn


