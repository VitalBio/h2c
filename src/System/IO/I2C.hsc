{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module System.IO.I2C
  ( I2Cfd()
  , openI2Cfd
  , I2CFunctionality(..)
  , getI2CFunctionality
  , I2CTransaction()
  , I2CAddress
  , writeToSlave
  , readFromSlave
  , runI2CTransaction
  ) where

import           Control.Monad.State
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.Int
import           Data.Word
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

#include <linux/i2c-dev.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include "typedefs.h"


foreign import ccall "open" c_open :: Ptr CChar -> #{type int} -> IO #{type int}
foreign import ccall "close" c_close :: #{type int} -> IO #{type int}
foreign import ccall "ioctl" c_ioctl_ptr :: #{type int} -> #{type unsigned long} -> Ptr () -> IO #{type int}

-- | A handle to an I2C bus
newtype I2Cfd = I2Cfd { i2cFD :: #{type int} }

-- | Open a handle to an I2C bus in the 'ResourceT' monad
openI2Cfd :: MonadResource m => String -> m (ReleaseKey, I2Cfd)
openI2Cfd path = do
  (pathReleaseKey, pathCString) <- allocate (newCString path) free
  releaseKeyAndFd <- allocate (fmap I2Cfd $ throwErrnoIf (< 0) ("open i2c " ++ path) $ c_open pathCString oRdwr) (throwErrnoIf_ (< 0) "close" . c_close . i2cFD)
  release pathReleaseKey
  return releaseKeyAndFd

-- | Flags of I2C functionality
data I2CFunctionality
  = I2CFunctionality
  { i2cTenBitAddresses :: Bool
  , i2cSmbusPec :: Bool
  , i2cI2C :: Bool
  } deriving Show

-- | Get the I2C functionality available on a specific bus
getI2CFunctionality :: I2Cfd -> IO I2CFunctionality
getI2CFunctionality (I2Cfd {..}) = runResourceT $ do
  functionalityPtr <- snd <$> allocate (mallocBytes #{size unsigned long}) free
  liftIO $ throwErrnoIf_ (< 0) ("ioctl(" ++ show i2cFD ++ ", I2C_FUNCS, " ++ show functionalityPtr) $ c_ioctl_ptr i2cFD i2cFuncs functionalityPtr
  functionality <- liftIO $ peek $ castPtr functionalityPtr :: ResourceT IO #{type unsigned long}
  return $ I2CFunctionality (functionality .&. i2cFunc10bitAddr > 0) (functionality .&. i2cFuncSmbusPec > 0) (functionality .&. i2cFuncI2c > 0)
  

type UnsignedLongHSC = #{type unsigned long}
type IntHSC = #{type int}

type I2CAddress = #{type long}

#{enum Word16, , I2C_M_TEN, I2C_M_RD, I2C_M_NOSTART, I2C_M_REV_DIR_ADDR, I2C_M_IGNORE_NAK, I2C_M_NO_RD_ACK }
#{enum UnsignedLongHSC, , I2C_SLAVE, I2C_TENBIT, I2C_PEC, I2C_FUNCS, I2C_RDWR }
#{enum IntHSC, , O_RDWR}
#{enum UnsignedLongHSC, , I2C_FUNC_10BIT_ADDR, I2C_FUNC_SMBUS_PEC, I2C_FUNC_I2C}

-- | A description of writes and reads to make with "repeated start" to the I2C bus
--
-- Transactions are intended to be built in sequence using the 'Applicative' instance
data I2CTransaction a where
  Pure :: a -> I2CTransaction a
  Write :: #{type long} -> ByteString -> I2CTransaction ()
  Read :: #{type long} -> #{type __u16} -> I2CTransaction ByteString
  Apply :: I2CTransaction (a -> b) -> I2CTransaction a -> I2CTransaction b

instance Functor I2CTransaction where
  fmap = Apply . Pure

instance Applicative I2CTransaction where
  pure = Pure
  (<*>) = Apply

-- | A master->slave write in a transaction
writeToSlave :: #{type long} -> ByteString -> I2CTransaction ()
writeToSlave address bytestring = Write address bytestring

-- A slave->master read in a transaction
readFromSlave :: #{type long} -> #{type __u16} -> I2CTransaction ByteString
readFromSlave address length = Read address length

-- | Run an i2c repeated-start transaction
runI2CTransaction :: I2Cfd -> I2CTransaction a -> IO a
runI2CTransaction (I2Cfd {..}) transaction = runResourceT $ do
  let messageCount = length i2CMessages
  (_, rdwrPtr) <-
    allocate (mallocBytes #{size i2c_rdwr_ioctl_data_t}) free
  (_, messagesPtr) <-
    allocate (mallocBytes $ #{size i2c_msg_t} * messageCount) free
  liftIO $ #{poke i2c_rdwr_ioctl_data_t, msgs} rdwrPtr messagesPtr 
  forM (zip i2CMessages [0..])
    (\(message, index) ->
      either
        -- Write
        (\(address, message) -> do
          let messagePtr = plusPtr messagesPtr $ index * #{size i2c_msg_t}
          (_, (bufferPtr, messageLength)) <-
            allocate
              (ByteString.useAsCStringLen message $ \ (string, messageLength) -> do
                 bufferPtr <- mallocBytes messageLength
                 copyBytes bufferPtr string messageLength
                 return (bufferPtr, messageLength))
              (free . fst)
          liftIO $ #{poke i2c_msg_t, buf} messagePtr bufferPtr
          liftIO $ #{poke i2c_msg_t, addr} messagePtr address
          liftIO $ #{poke i2c_msg_t, flags} messagePtr (0 :: Word16)
          liftIO $ #{poke i2c_msg_t, len} messagePtr messageLength
        )
        -- Read
        (\(address, length) -> do
          let messagePtr = plusPtr messagesPtr $ index * #{size i2c_msg_t}
          (_, bufferPtr) <- allocate (mallocBytes $ fromIntegral length) free
          liftIO $ #{poke i2c_msg_t, buf} messagePtr bufferPtr
          liftIO $ #{poke i2c_msg_t, addr} messagePtr address
          liftIO $ #{poke i2c_msg_t, flags} messagePtr i2cMRd
          liftIO $ #{poke i2c_msg_t, len} messagePtr length
        )
        message)
  liftIO $ throwErrnoIf_ (< 0) ("ioctl(" ++ show i2cFD ++ ", I2C_RDWR, " ++ show rdwrPtr ++ ")") $ c_ioctl_ptr i2cFD i2cRdwr rdwrPtr
  liftIO $ reassembleI2CMessages messagesPtr transaction
         
  where
    i2CMessages :: [Either (#{type long}, ByteString) (#{type long}, #{type __u16})]
    i2CMessages = buildI2CMessages transaction []

    buildI2CMessages
      :: I2CTransaction a
      -> (   [Either (#{type long}, ByteString) (#{type long}, #{type __u16})]
          -> [Either (#{type long}, ByteString) (#{type long}, #{type __u16})])
    buildI2CMessages (Pure _) = id
    buildI2CMessages (Write address bytestring) = (Left (address, bytestring) :)
    buildI2CMessages (Read address length) = (Right (address, length) :)
    buildI2CMessages (Apply l r) = buildI2CMessages l . buildI2CMessages r

    reassembleI2CMessages :: Ptr () -> I2CTransaction a -> IO a
    reassembleI2CMessages ptr transaction = flip evalStateT 0 $ reassembleI2CMessagesFromTransaction ptr transaction

    reassembleI2CMessagesFromTransaction :: (MonadIO m, MonadState Int m) => Ptr () -> I2CTransaction a -> m a
    reassembleI2CMessagesFromTransaction _ (Pure x) = return x
    reassembleI2CMessagesFromTransaction _ (Write _ _) = modify succ -- skip a message, it was data we wrote to the slave
    reassembleI2CMessagesFromTransaction ptr (Read _ length) = do
      index <- get
      modify succ -- bump the index
      let thisMessagePtr = plusPtr ptr $ index * #{size i2c_msg_t} 
      buffer <- liftIO $ #{peek i2c_msg_t, buf} ptr
      len <- liftIO $ #{peek i2c_msg_t, len} ptr -- sanity check
      if len /= length then liftIO $ error "Read length not equal to requested length" else return ()
      liftIO $ ByteString.packCStringLen (buffer, fromIntegral len)
    reassembleI2CMessagesFromTransaction ptr (Apply f x) =
          ($)
      <$> reassembleI2CMessagesFromTransaction ptr f
      <*> reassembleI2CMessagesFromTransaction ptr x
