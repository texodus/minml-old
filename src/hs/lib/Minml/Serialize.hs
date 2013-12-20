------------------------------------------------------------------------------

-- | Serialization

------------------------------------------------------------------------------

module Minml.Serialize where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as G
import qualified Data.Serialize as S

------------------------------------------------------------------------------

unpack :: S.Serialize a => B.ByteString -> Either String a
unpack =
    S.decode . BS.concat . BL.toChunks . G.decompress . BL.fromChunks . (:[])

pack :: S.Serialize a => a -> B.ByteString
pack =
    B.concat . BL.toChunks . G.compress . BL.fromChunks . (:[]) . S.encode

------------------------------------------------------------------------------
