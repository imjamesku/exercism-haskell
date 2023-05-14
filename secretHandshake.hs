module SecretHandshake (handshake) where

import Data.Bits (Bits ((.&.)))

handshake :: Int -> [String]
handshake n = let actions = codes >>= f in if n .&. 16 /= 0 then reverse actions else actions
  where
    f (i, s) = if n .&. i /= 0 then [s] else []
    codes = [(1, "wink"), (2, "double blink"), (4, "close your eyes"), (8, "jump")]
