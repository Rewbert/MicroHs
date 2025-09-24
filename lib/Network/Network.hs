module Network.Network where

import Data.Char
import Data.Word
import Data.Bits
import Data.List

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

foreign import ccall "sys/socket.h socket"  c_socket  :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h connect" c_connect :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind"    c_bind    :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept"  c_accept  :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall "sys/socket.h listen"  c_listen  :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h send"    c_send    :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize
foreign import ccall "sys/socket.h recv"    c_recv    :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize

foreign import ccall "unistd.h     close"   c_close   :: CInt -> IO CInt

foreign import ccall "errno.h      &errno"  cerrno    :: IO (Ptr CInt)

newtype Socket = Socket CInt

data Domain = AF_INET | INETV6 -- many more

instance Enum Domain where
    toEnum 2 = AF_INET
    toEnum 10 = INETV6

    fromEnum AF_INET = 2
    fromEnum INETV6 = 10

data StreamType = SOCK_STREAM | DGRAM -- many more

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum _ = error "idk yet, look it up"

    fromEnum SOCK_STREAM = 1
    fromEnum DGRAM = error "idk yet, look it up"

data SockAddr = SockAddrInet Int String

mkSockAddr :: Int -> Maybe String -> SockAddr
mkSockAddr port (Just address) = SockAddrInet port address
mkSockAddr port Nothing        = SockAddrInet port "0.0.0.0"

sizeOfSockAddr :: Int
sizeOfSockAddr = 16 -- 8 bytes of data, and 8 of padding

-- | Create a new socket, throwing an exception if creation failed
socket :: Domain -> StreamType -> IO Socket
socket d st = do
    i <- c_socket (CInt $ fromEnum d) (CInt $ fromEnum st) 0
    if i < 0
        then error "error allocating socket"
        else return $ Socket i

close :: Socket -> IO ()
close (Socket fd@(CInt fd')) = do
    CInt c <- c_close fd
    if c < 0
        then do c <- errno
                error $ "error closing socket " ++ show fd' ++ ", errno is " ++ show c
        else return ()

connect :: Socket -> SockAddr -> IO ()
connect (Socket socketfd) sockaddr =
    withSockAddr sockaddr $ \p -> do
        CInt e <- c_connect socketfd p (CInt sizeOfSockAddr)
        if e < 0
            then do c <- errno
                    error $ "error in connect, errno is " ++ show c
            else return ()

bind :: Socket -> SockAddr -> IO ()
bind (Socket socketfd) sockaddr =
    withSockAddr sockaddr $ \p -> do
        CInt e <- c_bind socketfd p (CInt sizeOfSockAddr)
        if e < 0
            then do c <- errno
                    error $ "error in bind, errno is " ++ show c
            else return ()

accept :: Socket -> IO (Socket, SockAddr)
accept (Socket server_fd) = do
    allocaBytes sizeOfSockAddr $ \p ->
        allocaBytes 4 $ \p_size -> do
            poke p_size 16

            CInt e <- c_accept server_fd p p_size
            if e < 0
                then do c <- errno
                        error $ "error in accept, errno is " ++ show c
                else do sockaddr <- peekSockAddr p
                        return (Socket (CInt e), sockaddr)

listen :: Socket -> IO ()
listen (Socket df) = do
    CInt e <- c_listen df (CInt 1)
    if e < 0
        then do c <- errno
                error $ "error in listen, errno is " ++ show c
        else return ()

sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf (Socket socketfd) buf len = do
    CSSize e <- c_send socketfd buf (CSize (fromIntegral len)) (CInt 0)
    if e == -1
        then do c <- errno
                error $ "error in send, errno is " ++ show c
        else return e

sendString :: Socket -> String -> IO Int
sendString socket str =
    let bytes = map (fromIntegral . ord) str :: [Word8]
    in withArray bytes $ \ptr ->
        sendBuf socket ptr (length bytes)

recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf (Socket socketfd) buf len = do
    CSSize e <- c_recv socketfd buf (CSize (fromIntegral len)) (CInt 0)
    if e == -1
        then do c <- errno
                error $ "error in recv, errno is " ++ show c
        else return e

recvString :: Socket -> Int -> IO String
recvString sock len = allocaBytes len $ \buf -> do
    n <- recvBuf sock buf len
    bytes <- mapM (\i -> peekByteOff buf i :: IO Word8) [0 .. (n-1)]
    return $ map (chr . fromIntegral) bytes

{-

struct sockaddr_in {
    sa_family_t     sin_family;     /* AF_INET */
    in_port_t       sin_port;       /* Port number */
    struct in_addr  sin_addr;       /* IPv4 address */
};

struct in_addr {
    in_addr_t s_addr;
};

typedef uint32_t in_addr_t;
typedef uint16_t in_port_t;




// sin_family = AF_INET (2)
raw[0] = 0x02;
raw[1] = 0x00;

// sin_port = htons(0x1234) → network byte order
raw[2] = 0x12;  // high byte
raw[3] = 0x34;  // low byte

// sin_addr = 192.168.0.1
raw[4] = 192;
raw[5] = 168;
raw[6] = 0;
raw[7] = 1;

-}

withSockAddr :: SockAddr -> (Ptr SockAddr -> IO a) -> IO a
withSockAddr sockaddr f =
    callocaBytes sizeOfSockAddr $ \p -> do
        pokeSockAddr p sockaddr
        f (castPtr p)

pokeSockAddr :: Ptr Word8 -> SockAddr -> IO ()
pokeSockAddr p (SockAddrInet port address) = do
    -- write AF_INET = 2 to sin_family
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [0..] sin_family)

    -- write port to sin_port
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [2..] sin_port)

    -- write address to sin_addr
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [4..] sin_addr)
  where
    sin_family :: [Word8]
    sin_family = [0x02, 0x00]

    sin_port :: [Word8]
    sin_port =
        let w16 = fromIntegral port :: Word16
            high = fromIntegral ((w16 `shiftR` 8) .&. 0xFF)
            low = fromIntegral (w16 .&. 0xFF)
        in [high, low]
    
    sin_addr :: [Word8]
    sin_addr = map read $ splitOn '.' address
      where
        splitOn :: Eq a => a -> [a] -> [[a]]
        splitOn _ [] = []
        splitOn i xs = let pref = takeWhile ((/=) i) xs
                           suff = dropWhile ((/=) i) xs
                       in case suff of
                            [] -> [pref]
                            (_:xs') -> pref : splitOn i xs'

peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p =
    let p' :: Ptr Word8
        p' = castPtr p
    in do
        sin_family <- mapM ((peekByteOff :: Ptr Word8 -> Int -> IO Word8) p') [0,1]

        sin_port <- mapM ((peekByteOff :: Ptr Word8 -> Int -> IO Word8) p') [2,3]

        sin_addr <- mapM ((peekByteOff :: Ptr Word8 -> Int -> IO Word8) p') [4,5,6,7]

        let [high, low] = sin_port
            high16      = (0xFFFF .&. high) `shiftL` 16
            low16       =  0xFFFF .&. low
            w16         =  high16 .&. low16
            port        = fromIntegral w16 :: Int
            address = intercalate "." $ map show sin_addr
        return $ SockAddrInet port address



errno :: IO Int
errno = do
    p <- cerrno
    CInt i <- peek p
    return i