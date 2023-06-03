module Robot
  ( Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
  )
where

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data Robot = Robot Bearing (Integer, Integer) deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ (i, j)) = (i, j)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

left :: Bearing -> Bearing
left North = West
left West = South
left South = East
left East = North

right :: Bearing -> Bearing
right North = East
right East = South
right South = West
right West = North

advance :: Robot -> Robot
advance (Robot b (i, j))
  | b == North = Robot b (i, j + 1)
  | b == East = Robot b (i + 1, j)
  | b == South = Robot b (i, j - 1)
  | b == West = Robot b (i - 1, j)

move :: Robot -> String -> Robot
move robot [] = robot
move robot instructions
  | x == 'L' = move (Robot (left b) (i, j)) xs
  | x == 'R' = move (Robot (right b) (i, j)) xs
  | x == 'A' = move (advance robot) xs
  where
    (Robot b (i, j)) = robot
    (x : xs) = instructions
