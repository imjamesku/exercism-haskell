module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock Int
  deriving (Eq)

_24Hours :: Int
_24Hours = 24 * 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = normalize $ Clock (hour * 60 + min)

toString :: Clock -> String
toString (Clock n) = printf "%02d:%02d" (n `div` 60 `mod` 24) (n `mod` 60)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock n) = normalize $ Clock (n + hour * 60 + min)

normalize :: Clock -> Clock
normalize (Clock n) = Clock (n `mod` _24Hours)