-- Search for comments to find commentary of interest
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Exception.Safe
import Control.Monad
import Control.Natural.Control
import Data.Aeson
import Data.ByteString.Internal
import Data.Void
import Foreign.C.Error
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.Generics
import Observe.Event
import Observe.Event.DSL
import Observe.Event.Render.JSON
import Observe.Event.Render.JSON.DSL.Compile
import Observe.Event.Render.JSON.Handle
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import System.Posix.IO
import System.Posix.Types

-- Pretend this is in a separate module File where

deriving instance Show Errno

deriving instance ToJSON Errno

deriving instance Generic Errno

deriving instance ToJSON CInt

deriving instance Generic CInt

deriving instance ToJSON Fd

deriving instance Generic Fd

deriving instance ToJSON ByteCount

deriving instance Generic ByteCount

-- Define our selector type and give it instances to render as JSON
compile $
  SelectorSpec
    "file" -- Creates a type FileSelector
    [ ["open", "file"] -- Creates a constructor OpenFile :: FileSelector OpenField
        ≔ FieldSpec
          "open" -- Creates a type OpenField
          [ "filename" ≔ ''FilePath, -- creates a constructor Filename :: !FilePath -> OpenField
            ["file", "fd"] ≔ ''Fd -- creates a constructor FileFd :: !Fd -> OpenField
          ],
      "write"
        ≔ FieldSpec
          "write"
          [ ["bytes", "asked"] ≔ ''ByteCount,
            ["bytes", "actual"] ≔ ''ByteCount
          ]
    ]

-- We run in EventT, polymorphic in r, supporting our domain-specific selector type
writeToFile :: FilePath -> ByteString -> EventT IO r FileSelector ()
writeToFile path bs = do
  let (fptr, base_off, sz) = toForeignPtr bs
  -- We start an event, selected by OpenFile
  fd <- withEvent OpenFile $ \ev -> do
    -- We add a Filename field to our current active event
    addField ev $ Filename path

    fd <- toNatural eventLift $ openFd path WriteOnly (Just regularFileMode) defaultFileFlags
    when (fd == -1) $ do
      errno <- toNatural eventLift $ getErrno
      -- Throw an exception which we can render as JSON
      throw $ BadOpen path errno

    addField ev $ FileFd fd
    pure fd
  statelessTransWith eventLift $ \runInIO -> withForeignPtr fptr $ \ptr -> runInIO $ do
    let bcSz = fromIntegral sz
        go (offset :: ByteCount) = do
          newOffset <- withEvent Write $ \ev -> do
            let ct = bcSz - offset
            addField ev $ BytesAsked ct
            written <- toNatural eventLift $ fdWriteBuf fd (plusPtr ptr (base_off + fromIntegral offset)) ct
            addField ev $ BytesActual written
            pure $ offset + written
          when (newOffset < bcSz) $
            go newOffset
    go 0
  toNatural eventLift $ closeFd fd
  pure ()

-- Define a new exception that can be used with simpleJsonStderrBackend
data BadOpen = BadOpen
  { path :: !FilePath,
    errno :: !Errno
  }
  deriving (Show, ToJSON, Generic)

-- Our exception is beneath SomeJSONException in the hierarchy
instance Exception BadOpen where
  toException = jsonExceptionToException
  fromException = jsonExceptionFromException

-- end module File

compile $
  SelectorSpec
    "main"
    [ ["using", "temp", "dir"] ≔ ''FilePath, -- Creates a constructor UsingTempDir :: MainSelector FilePath
      "writing" ≔ Inject ''FileSelector -- Creates a constructor Writing :: FileSelector x -> MainSelector x
    ]

-- Note a different selector type than writeToFile
instrumentedMain :: EventT IO r MainSelector ()
instrumentedMain = do
  withNarrowingEvent (injectSelector Writing) UsingTempDir $ \ev -> statelessTransWith eventLift $ \runInIO -> do
    withSystemTempDirectory "example" $ \dir -> runInIO $ do
      addField ev dir
      -- In writeToFile, all parentless events are made children of our current event
      writeToFile (dir </> "example.txt") "example"

main :: IO ()
main =
  -- Initialize a backend to write JSON to stderr and use it.
  simpleJsonStderrBackend defaultRenderSelectorJSON >>= runEventT instrumentedMain
