module Main where

import Control.Monad.Eff.Random
import Data.Maybe
import DinoRun.Constants
import DinoRun.Types
import Prelude
import PrestoDOM.Core
import PrestoDOM.Elements
import PrestoDOM.Events
import PrestoDOM.Properties
import PrestoDOM.Types
import PrestoDOM.Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM.HTML.History (state)
import Data.Array (concatMap, (..))
import Data.Traversable (for, traverse)
import FRP (FRP)
import FRP.Behavior.Keyboard (key)
import FRP.Event.Keyboard (down)
import FRP.Event.Time (animationFrame)
import PrestoDOM.Util as U

-- foreign import click :: MEvent
-- foreign import getId :: Int

getImageObject :: forall i a. String -> String -> ObjectMetrics -> VDom (Array (Prop i)) a
getImageObject imageId url imageMetrics = imageView
                        [ width $ V imageMetrics.w
                        , height $ V imageMetrics.h
                        , margin ((show imageMetrics.x) <> ","<>(show imageMetrics.y)<>",0,0")
                        , imageUrl url
                        ]

obstacles :: Array ObjectMetrics
obstacles = (map (\i -> { x:  (i * rockGap) , y: groundY - 38, w:  40, h: 45}) (1..5))

clouds :: Array ObjectMetrics
clouds = (map (\i -> { x:  (i * cloudsGap) , y: (groundY - 300), w:  150, h: 80}) (1..5))


fly :: Array ObjectMetrics
fly = (map (\i -> { x:  (i * flyGap) , y: (groundY - 150), w:  32, h: 40}) (1..5))

missile :: Array ObjectMetrics
missile = (map (\i -> { x:  (i * (flyGap + 200)) , y: (groundY - 300), w:  60, h: 25}) (1..3))


topStone :: Array ObjectMetrics
topStone = (map (\i -> { x: (i * 60) + 200 , y: 0, w:  200, h: 100}) (1..20))


updateRocks :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateRocks (state::GameDashRunState) 
      | (state.gameState == isAlive) = (U.updateState "rocks" (map (\o->o{x=((o.x+(if o.x < -100
                                                              then 1500
                                                              else 0
        ))-velocityX)}) state.rocks))
      | otherwise = pure state

updateClouds :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateClouds (state::GameDashRunState) 
      | (state.gameState == isAlive) = (U.updateState "clouds" (map (\o->o{x=((o.x+(if o.x < -300
                                                              then 1500
                                                              else 0
        ))-velocityX)}) state.clouds))
      | otherwise = pure state

updateFlies :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateFlies (state::GameDashRunState) 
      | (state.gameState == isAlive) = (U.updateState "flies" (map (\o->o{x=((o.x+(if o.x < -200
                                                              then 1500
                                                              else 0
        ))-6)}) state.flies))
      | otherwise = pure state


updateMissile :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateMissile (state::GameDashRunState) 
      | (state.gameState == isAlive) = (U.updateState "missile" (map (\o->o{x=((o.x+(if o.x < -200
                                                              then 1500
                                                              else 0
        ))-12)}) state.missile))
      | otherwise = pure state

updateTopStone :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateTopStone (state::GameDashRunState) 
      | (state.gameState == isAlive) = (U.updateState "topStone" (map (\o->o{x=((o.x+(if o.x < -200
                                                              then 1200
                                                              else 0
        ))-velocityX)}) state.topStone))
      | otherwise = pure state

updateRunner :: forall a.GameDashRunState -> Eff (console :: CONSOLE | a) GameDashRunState
updateRunner (state::GameDashRunState) 
      | (state.gameState == isAlive) =do
          _ <- if (state.jump)
            then do
                state<- U.updateState "currTime" (state.currTime+20)
                if (state.finalVelocity==state.initialVelocity) && (state.finalVelocity>0)
                  then do
                    _ <- U.updateState "egravity" (-1)
                    pure unit
                  else
                    pure unit

                state <- U.getState

                if state.finalVelocity==0
                  then do
                    _ <- U.updateState "egravity" (-1)
                    _ <- U.updateState "initialVelocity" 0
                    pure unit
                  else
                    pure unit

                state<-U.getState
                (state::GameDashRunState)<-U.updateState "finalVelocity" ((state.initialVelocity + (state.egravity*((state.currTime-startTime)/100))))
                newY <- if((state.runner.y-state.finalVelocity)> (groundY - state.runner.h))
                            then do
                              _ <- (U.updateState "jump" false)
                              pure (groundY-state.runner.h)

                            else pure (state.runner.y-state.finalVelocity)
                U.updateState "runner" (state.runner{y=newY})

            else
              U.getState

          U.getState
      | otherwise = pure state

initializeVel :: forall a.Eff (| a) GameDashRunState
initializeVel = do 
  _ <- U.updateState "jump" true
  _ <- U.updateState "initialVelocity" 5
  _ <- U.updateState "egravity" 1
  _ <- U.updateState "finalVelocity" 5
  U.updateState "currTime" 0


getScore :: GameDashRunState -> Int
getScore state = state.timer/500


updateTimer :: forall a.GameDashRunState -> Eff (| a) GameDashRunState
updateTimer (state::GameDashRunState) 
      | (state.gameState == isAlive) = do 
        _ <- (U.updateState "timer" (state.timer+20))
        if (state.timer/500) >= 50
          then do 
          _ <- U.updateState "gameBackground" "#111111"
          U.updateState "cloudImage" "cloudRain"  
          else U.getState
      | otherwise = pure state



collisionDetected :: ObjectMetrics -> ObjectMetrics -> Boolean
collisionDetected r1 r2 = do 
  (((r1.x + r1.w) > r2.x) && (r1.x < (r2.x+r2.w)) && ((r1.y+r1.h) > r2.y) && (r1.y < (r2.y + r2.h))) 
               

widget :: forall i a. GameDashRunState -> VDom (Array (Prop i)) a
widget state = relativeLayout
              [height Match_Parent 
              , width Match_Parent
              , background $ state.gameBackground
              , name "rootNode"
              ]
              [
              linearLayout
              [height (V 200)
              , width Match_Parent
              , background "#9B6442"
              , margin "0,500,0,0"
              ][], 
              linearLayout
              [height (V 2)
              , width Match_Parent
              , background "#000000"
              , margin ((show state.ground.x) <> ","<>(show state.ground.y)<>",0,0")
              ][],
              relativeLayout
              [width Match_Parent
              , height Match_Parent
              ]
              (map (\o -> (getImageObject ("id"<>show o.x) "rock" o)) state.rocks),
              (getImageObject "dino" (state.dinoImage) (state.runner)),
              relativeLayout
              [width Match_Parent
              , height Match_Parent
              ]
              (map (\o -> (getImageObject ("id"<>show o.x) "missile" o)) state.missile),
              relativeLayout
              [width Match_Parent
              , height Match_Parent
              ]
              (map (\o -> (getImageObject ("id"<>show o.x) state.cloudImage o)) state.clouds),
              relativeLayout
              [width Match_Parent
              , height Match_Parent
              ]
              (map (\o -> (getImageObject ("id"<>show o.x) "fly" o)) state.flies),

              relativeLayout
              [width Match_Parent
              , height Match_Parent
              ]
              (map (\o -> (getImageObject ("id"<>show o.x) "topstone" o)) state.topStone),

              (getGameState state),

              textView
              [
                id_ "score"
              , width (V 200)
              , height (V 70)
              , color "#000000"
              , textSize "18"
              , margin "600,550,0,0"
              , text (show $ "Score "<> (show $ getScore state) <> "m")
              , fontStyle "Rammetto One"
              ]
              
            ]

getGameState :: forall i a. GameDashRunState -> VDom (Array (Prop i)) a
getGameState state = relativeLayout
                [ width $ V 500
                , height  $ V 200
                , margin "250,200,0,0"
                , visibility ( if state.gameState 
                                        then "gone"
                                        else "visible")
                ]
                [
                  textView
                  [ width $ V 400
                  , height $ V 80
                  , fontStyle "Rammetto One"
                  , text "Game Over"
                  , textSize "45"
                  , margin "220,80,0,0"
                  , color "#F51D1D"
                  ]
                ]


main = do
  --- Init State {} empty record--
  U.initializeState

  --- Update State ----
  _ <- U.updateState "rocks" obstacles
  _ <- U.updateState "clouds" clouds
  _ <- U.updateState "flies" fly
  _ <- U.updateState "missile" missile
  _ <- U.updateState "topStone" topStone
  _ <- U.updateState "egravity" 0
  _ <- U.updateState "init" 5
  _ <- U.updateState "finalVelocity" 5
  _ <- U.updateState "currTime" 0
  _ <- U.updateState "jump" false
  _ <- U.updateState "timer" 0
  _ <- U.updateState "gameState" isAlive
  _ <- U.updateState "gameBackground" "#85C1E9"
  _ <- U.updateState "runner" {x:50,y:groundY-40,w: 40,h: 40}
  _ <- U.updateState "dinoImage" "dino"
  _ <- U.updateState "cloudImage" "clouds"
  state <- U.updateState "ground" {x : 0, y: groundY, w :  1200, h :  5}

  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

gameEvent keyPressed = initializeVel 

frameEval :: forall a . Boolean -> Eff (console :: CONSOLE | a) GameDashRunState
frameEval _ =  do
  (state::GameDashRunState) <- U.getState
  _<- (traverse (\o -> do
                  let collision = collisionDetected state.runner o
                  if collision
                    then  do
                    void $ U.updateState "gameState" $ not isAlive
                    U.updateState "dinoImage" "dinoDead"
                    else  U.getState)
                 (state.rocks <> state.flies <> state.missile <> state.topStone))
  state <- updateRocks state
  state <- updateClouds state
  state <- updateFlies state
  state <- updateTimer state
  state <- updateMissile state
  state <- updateTopStone state
  updateRunner state




listen :: forall a.Eff( frp :: FRP, console :: CONSOLE| a)(Eff( frp :: FRP, console :: CONSOLE| a ) Unit)
listen = do
  let keyBehavior = gameEvent <$> (key 32)
  let keyEvent = down

  let frameBehaviour = frameEval <$> (key 33) 
  _ <- U.patch widget keyBehavior keyEvent
  U.patch widget frameBehaviour animationFrame

