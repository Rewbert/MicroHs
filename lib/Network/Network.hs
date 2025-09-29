{-| Simple Socket interface, that only supports AF_INET so far. There are other addressing families (e.g. BT)
that are not supported (because I am lazy).

Most functions only exist as IO () variants, where in reality they might fail with an error code.
These variants can be trivially added.

-}
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

foreign import ccall "sys/socket.h socket"  c_socket        :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h connect" c_connect       :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h bind"    c_bind          :: CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall "sys/socket.h accept"  c_accept        :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall "sys/socket.h listen"  c_listen        :: CInt -> CInt -> IO CInt
foreign import ccall "sys/socket.h send"    c_send          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize
foreign import ccall "sys/socket.h recv"    c_recv          :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSSize
foreign import ccall "sys/socket.h setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall "unistd.h     close"   c_close   :: CInt -> IO CInt

foreign import ccall "errno.h      &errno"  cerrno    :: IO (Ptr CInt)

-- | Socket type
newtype Socket = Socket CInt

-- | Adressing family
data Domain
    = AF_INET -- ^ IPV4
    | INETV6  -- ^ IPV6
    -- many more that are not added

instance Enum Domain where
    toEnum 2 = AF_INET
    toEnum 10 = INETV6
    toEnum _ = "not added yet"

    fromEnum AF_INET = 2
    fromEnum INETV6 = 10

-- | Stream type
data StreamType
    = SOCK_STREAM -- ^ TCP
    | DGRAM       -- ^ UDP
    -- many more to add

instance Enum StreamType where
    toEnum 1 = SOCK_STREAM
    toEnum _ = error "not added yet"

    fromEnum SOCK_STREAM = 1
    fromEnum DGRAM = error "not added yet"

-- | Socket Address type, don't expose internals of this one
data SockAddr = SockAddrInet Int String

{- | Create a SockAddr. If the second parameter is Nothing, the address is assumed
to be INADDR_ANY (0.0.0.0) -}
mkSockAddr :: Int -> Maybe String -> SockAddr
mkSockAddr port (Just address) = SockAddrInet port address
mkSockAddr port Nothing        = SockAddrInet port "0.0.0.0" -- INADDR_ANY

-- | There really should be a Storable instance instead, where we can implement the sizeOf function
sizeOfSockAddr :: Int
sizeOfSockAddr = 16 -- 8 bytes of data, and 8 of padding

-- | Socket options
data SockOpt
    = SO_REUSEADDR -- ^ Allow reuse of local addresses
    -- and many more to add, I just need this one for my specific application

instance Enum SockOpt where
    toEnum 2 = SO_REUSEADDR
    toEnum _ = error "not added yet"

    fromEnum SO_REUSEADDR = 2

-- | Create a new socket, throwing an exception if creation failed
socket :: Domain -> StreamType -> IO Socket
socket d st = do
    i <- c_socket (CInt $ fromEnum d) (CInt $ fromEnum st) 0
    if i < 0
        then error "error allocating socket"
        else return $ Socket i

-- | Set a socket option
setsocketopt :: Socket -> SockOpt -> IO ()
setsocketopt (Socket socketfd) so = do
    -- please see the C setsocketopt to see what the parameters are, if it is unclear
    -- (it was unclear to me first)
    callocaBytes (sizeOf (undefined :: CInt)) $ \p -> do
        poke (castPtr p) (1 :: CInt)
        CInt e <- c_setsockopt socketfd (CInt 1 {- SOL_SOCKET = 1 -}) (CInt $ fromEnum so) p (CInt (sizeOf (undefined :: CInt)))
        if e == -1
            then do c <- errno
                    error $ "error in setsocketopt, errno is " ++ show c
            else return ()

-- | Close a socket
close :: Socket -> IO ()
close (Socket fd@(CInt fd')) = do
    CInt c <- c_close fd
    if c < 0
        then do c <- errno
                error $ "error closing socket " ++ show fd' ++ ", errno is " ++ show c
        else return ()

-- | Establish a connection
connect :: Socket -> SockAddr -> IO ()
connect (Socket socketfd) sockaddr =
    withSockAddr sockaddr $ \p -> do
        CInt e <- c_connect socketfd p (CInt sizeOfSockAddr)
        if e < 0
            then do c <- errno
                    error $ "error in connect, errno is " ++ show c
            else return ()

-- | Establish a connection, returning a Bool to indicate whether the action was successful
connect' :: Socket -> SockAddr -> IO Bool
connect' (Socket socketfd) sockaddr =
    withSockAddr sockaddr $ \p -> do
        CInt e <- c_connect socketfd p (CInt sizeOfSockAddr)
        return $ e >= 0

-- | Bind a socket to a SockAddr
bind :: Socket -> SockAddr -> IO ()
bind (Socket socketfd) sockaddr =
    withSockAddr sockaddr $ \p -> do
        CInt e <- c_bind socketfd p (CInt sizeOfSockAddr)
        if e < 0
            then do c <- errno
                    error $ "error in bind, errno is " ++ show c
            else return ()

-- | Accept an incoming connection request
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

-- | Set a socket to listening mode
listen :: Socket -> IO ()
listen (Socket df) = do
    CInt e <- c_listen df (CInt 1)
    if e < 0
        then do c <- errno
                error $ "error in listen, errno is " ++ show c
        else return ()

-- | Send a buffer of data over a socket
sendBuf :: Socket -> Ptr Word8 -> Int -> IO Int
sendBuf (Socket socketfd) buf len = do
    CSSize e <- c_send socketfd buf (CSize (fromIntegral len)) (CInt 0)
    if e == -1
        then do c <- errno
                error $ "error in send, errno is " ++ show c
        else return e

-- | Convenience function to transmit a String over a socket
sendString :: Socket -> String -> IO Int
sendString socket str =
    let bytes = map (fromIntegral . ord) str :: [Word8]
    in withArray bytes $ \ptr ->
        sendBuf socket ptr (length bytes)

-- | Receive data over a socket
recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf (Socket socketfd) buf len = do
    CSSize e <- c_recv socketfd buf (CSize (fromIntegral len)) (CInt 0)
    if e == -1
        then do c <- errno
                error $ "error in recv, errno is " ++ show c
        else return e

-- | Convenience function to receive a string (of a maximum length)
recvString :: Socket -> Int -> IO String
recvString sock len = allocaBytes len $ \buf -> do
    n <- recvBuf sock buf len
    bytes <- mapM (\i -> peekByteOff buf i :: IO Word8) [0 .. (n-1)]
    return $ map (chr . fromIntegral) bytes

{- | Socket functions require a pointer to a SockAddr, and this function takes a SockAddr and
a such a function, and turns the SockAddr into a 'pointable' value (by writing it to the heap). -}
withSockAddr :: SockAddr -> (Ptr SockAddr -> IO a) -> IO a
withSockAddr sockaddr f =
    callocaBytes sizeOfSockAddr $ \p -> do
        pokeSockAddr p sockaddr
        f (castPtr p)

-- | Write a SockAddr to a buffer
pokeSockAddr :: Ptr Word8 -> SockAddr -> IO ()
pokeSockAddr p (SockAddrInet port address) = do
    -- write AF_INET = 2 to sin_family
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [0..] sin_family)

    -- write port to sin_port
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [2..] sin_port)

    -- write address to sin_addr
    mapM_ (\(i, b) -> pokeByteOff p i b) (zip [4..] sin_addr)
  where
    -- hard-coded AF_INET for now
    sin_family :: [Word8]
    sin_family = [0x02, 0x00]

    -- the port in network order
    sin_port :: [Word8]
    sin_port =
        let w16 = fromIntegral port :: Word16
            high = fromIntegral ((w16 `shiftR` 8) .&. 0xFF)
            low = fromIntegral (w16 .&. 0xFF)
        in [high, low]
    
    -- the address (e.g. "127.0.0.1" -> [127,0,0,1] :: [Word8])
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

-- | Read a SockAddr from a buffer
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

-- | The current Errno
errno :: IO Int
errno = do
    p <- cerrno
    CInt i <- peek p
    return i