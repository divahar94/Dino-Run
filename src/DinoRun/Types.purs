module DinoRun.Types where
  
type ObjectMetrics = {
  x :: Int
, y :: Int
, w :: Int
, h :: Int
}

type GameDashRunState = {
  ground :: ObjectMetrics
, runner :: ObjectMetrics
, clouds :: Array ObjectMetrics
, rocks :: Array ObjectMetrics
, flies :: Array ObjectMetrics
, missile :: Array ObjectMetrics
, egravity :: Int
, initialVelocity :: Int
, finalVelocity :: Int
, currTime :: Int
, gameState :: Boolean
, jump :: Boolean
, timer :: Int
, gameBackground :: String
, topStone :: Array ObjectMetrics
, dinoImage :: String
, cloudImage :: String
}