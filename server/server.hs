import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as UTF8
import Network.Socket.ByteString (recv, sendAll)

dispatchRequest :: Socket -> S.ByteString -> IO ()
dispatchRequest conn msg = do
    let converted = UTF8.fromString "wobbalobbaΩdingdong"
    sendAll conn converted

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