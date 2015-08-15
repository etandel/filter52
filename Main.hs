{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Control.Concurrent (forkIO)
import Control.Monad (forever, (>=>), void)
import Data.Default (Default(def))
import Data.List (partition)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import Data.Attoparsec.Char8  -- TODO: This is deprecated. What's the correct way of doing stuff?
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL


data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  , nameservers :: [HostName]
  }

instance Default Conf where
    def = Conf
      { bufSize     = 512
      , timeOut     = 10 * 1000 * 1000
      , nameservers = []
      }


toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right


filterIPs :: DNSMessage -> DNSMessage
filterIPs DNSMessage{..} =
    DNSMessage { header = header  -- TODO: Use lens to simplify this update
               , answer = filter not52 answer
               , question = question
               , authority = authority
               , additional = additional }
  where
    not52 :: ResourceRecord -> Bool
    not52 ResourceRecord{..} =  -- TODO: These patterns should be refactored
        case rdata of
            RD_A ip -> Prelude.take 2 (show ip) /= "52"
            _       -> True
    not52 _ = True


{-- Proxy dns request to a real dns server. --}
proxyRequest :: Conf -> HostName -> DNSMessage -> IO (Either String DNSMessage)
proxyRequest Conf{..} server req = do
    let rc = defaultResolvConf { resolvInfo = RCHostName server }
        worker Resolver{..} = do
            let packet = B.concat . BL.toChunks $ encode req
            sendAll dnsSock packet
            receive dnsSock
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (toEither "proxy request timeout" >=> check >=> (Right . filterIPs))
        <$> timeout timeOut (worker r)
  where
    ident = identifier . header $ req

    check :: DNSMessage -> Either String DNSMessage
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Right rsp
                        else Left "identifier not match"


{-- Handle A query for domain suffixes configured, and proxy other requests to
 -  a real dns server.
 --}
handleRequest :: Conf -> DNSMessage -> IO (Either String DNSMessage)
handleRequest conf req =
    case nameservers conf of
        []    -> return $ Left "nameserver not configured."
        srv:_ -> proxyRequest conf srv req


{-- Parse request and compose response. --}
handlePacket :: Conf -> Socket -> SockAddr -> B.ByteString -> IO ()
handlePacket conf@Conf{..} sock addr s =
    case decode (BL.fromChunks [s]) of
        Left errmsg -> putStrLn $ "decode fail:" ++ errmsg
        Right req ->
            handleRequest conf req >>=
                either
                putStrLn
                (\rsp -> let packet = B.concat . BL.toChunks $ encode rsp
                         in  timeout timeOut (sendAllTo sock packet addr) >>=
                             maybe (putStrLn "send response timeout") return
                )


run :: Conf -> IO ()
run conf = withSocketsDo $ do
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
    bindSocket sock (addrAddress addrinfo)
    forever $ do
        (s, addr) <- recvFrom sock (bufSize conf)
        forkIO $ handlePacket conf sock addr s


{-- parse config file. --}
readConf :: FilePath -> IO [HostName]
readConf filename = do
    content <- B.readFile filename
    either (fail . ("fail parsing conf: " ++)) return $ parseHosts content
  where
    parseHosts :: B.ByteString -> Either String [HostName]
    parseHosts s = let (serverLines, _) = partition (B.isPrefixOf "nameserver") (B.lines s)
                   in  mapM (parseOnly nameserver) serverLines

    nameserver :: Parser HostName
    nameserver = do
        void $ string "nameserver"
        void $ space
        skipSpace
        B.unpack <$> takeWhile (not . isSpace)


main :: IO ()
main = do
    args <- getArgs
    servers <- readConf $ fromMaybe "./resolv.conf" (listToMaybe args)
    print servers
    run def{nameservers=servers}

