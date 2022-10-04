-- Search for comments to find commentary of interest
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.ByteString.Internal
import Data.Void
import Foreign.C.Error
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Generics
import Observe.Event
import Observe.Event.Render.IO.JSON
import Observe.Event.Render.JSON
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

-- Pretend this is in a separate module File where

-- We take an EventBackend, polymorphic in r, supporting our domain-specific selector type
writeToFile :: EventBackend IO r FileSelector -> FilePath -> ByteString -> IO ()
writeToFile backend path bs = do
  let (fptr, base_off, sz) = toForeignPtr bs
  -- We start an event, selected by OpenFile
  fd <- withEvent backend OpenFile $ \ev -> do
    -- We add a Filename field to our current active event
    addField ev $ Filename path

    fd <- openFd path WriteOnly (Just regularFileMode) defaultFileFlags
    when (fd == -1) $ do
      errno <- getErrno
      -- Throw an exception which we can render as JSON
      throw $ BadOpen path errno

    addField ev $ FileFd fd
    pure fd
  withForeignPtr fptr $ \ptr -> do
    let bcSz = fromIntegral sz
        go :: ByteCount -> IO ()
        go offset = do
          newOffset <- withEvent backend WriteFile $ \ev -> do
            let ct = bcSz - offset
            addField ev $ BytesAsked ct
            written <- fdWriteBuf fd (plusPtr ptr (base_off + fromIntegral offset)) ct
            addField ev $ BytesActual written
            pure $ offset + written
          when (newOffset < bcSz) $
            go newOffset
    go 0
  closeFd fd
  pure ()

-- Define our selector type. A GADT, parameterized by field type
data FileSelector f where
  OpenFile :: FileSelector OpenField
  WriteFile :: FileSelector WriteField

-- Define a renderer for our selector type
renderFileSelector :: RenderSelectorJSON FileSelector
renderFileSelector OpenFile = ("open-file", renderOpenField) -- Expressed in terms of renderers for our field tyeps
renderFileSelector WriteFile = ("write-file", renderWriteField)

-- Define a field type
data OpenField
  = Filename !FilePath
  | FileFd !Fd

-- Define a renderer for a field type
renderOpenField :: RenderFieldJSON OpenField
renderOpenField (Filename path) = ("file-name", toJSON path)
renderOpenField (FileFd fd) = ("file-fd", toJSON fd)

data WriteField
  = BytesAsked !ByteCount
  | BytesActual !ByteCount

renderWriteField :: RenderFieldJSON WriteField
renderWriteField (BytesAsked ct) = ("asked", toJSON ct)
renderWriteField (BytesActual ct) = ("actual", toJSON ct)

-- Define a new exception that can be used with simpleJsonStderrBackend
data BadOpen = BadOpen
  { path :: !FilePath,
    errno :: !Errno
  }
  deriving (Show, ToJSON, Generic)

-- Our exception is beneath SomeJSONException in the hierarchy
instance Exception BadOpen where
  toException = jsonExceptionToException toJSON
  fromException = jsonExceptionFromException

-- end module File

-- Note a different selector type than writeToFile
instrumentedMain :: EventBackend IO r MainSelector -> IO ()
instrumentedMain backend = do
  withEvent backend UsingTempDir $ \ev -> do
    withSystemTempDirectory "example" $ \dir -> do
      addField ev dir
      let -- Create a new EventBackend where all parentless events are made children of our current event
          subBackend = subEventBackend ev
          -- Narrow subBackend to create events from FileSelectors instead of MainSelectors
          narrowerBackend = narrowEventBackend InjectFileSelector subBackend
      -- Pass our narrower backend to writeToFile
      writeToFile narrowerBackend (dir </> "example.txt") "example"

main :: IO ()
main =
  -- Initialize a backend to write JSON to stderr and use it.
  simpleJsonStderrBackend renderMainSelector >>= instrumentedMain

-- Instrumentation definitions
data MainSelector f where
  UsingTempDir :: MainSelector FilePath
  InjectFileSelector :: FileSelector a -> MainSelector a -- A typical pattern together with narrowEventBackend to call functions with a more specialized selector type

renderMainSelector :: RenderSelectorJSON MainSelector
renderMainSelector UsingTempDir = ("using-tmp-dir", \f -> ("path", toJSON f))
renderMainSelector (InjectFileSelector filesel) = renderFileSelector filesel -- Note that we reuse the selector renderer from our File "module"

deriving instance Show Errno

deriving instance ToJSON Errno

deriving instance Generic Errno

deriving instance ToJSON CInt

deriving instance Generic CInt

deriving instance ToJSON Fd

deriving instance Generic Fd

deriving instance ToJSON ByteCount

deriving instance Generic ByteCount
