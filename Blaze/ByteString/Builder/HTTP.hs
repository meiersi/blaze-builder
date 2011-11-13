{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings, MonoPatBinds #-}
-- | Support for HTTP response encoding.
--
-- TODO: Improve documentation.
module Blaze.ByteString.Builder.HTTP (
  -- * Chunked HTTP transfer encoding
    chunkedTransferEncoding
  , chunkedTransferTerminator
  ) where

import           Data.ByteString.Char8 () -- IsString instance

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import qualified Data.ByteString.Lazy.Builder.BasicEncoding        as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Extras as E

------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
chunkedTransferEncoding :: Builder -> Builder
chunkedTransferEncoding = E.encodeChunked 
    16                                -- minimal free space guaranteed for a chunk body
    (E.word64HexFixedBound '0')       -- 0 padded, hexadecimal chunk length before chunk
    (E.fromF (const ('\r','\n') E.>$< E.char8 E.>*< E.char8)) -- CRLF after chunk

-- | The zero-length chunk '0\r\n\r\n' signaling the termination of the data transfer.
chunkedTransferTerminator :: Builder
chunkedTransferTerminator = byteStringCopy "0\r\n\r\n"
