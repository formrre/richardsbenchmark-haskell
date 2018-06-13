{-# LANGUAGE DuplicateRecordFields #-}
import Data.Word
import Data.Dynamic
import Control.Monad.Cont

data Packet = Packet { link :: Maybe Packet
                     , id :: Int
                     , kind :: Int
                     , a1 :: Int
                     , a2 :: [Int] }

data GlobalState = GlobalState { tcb :: TCB
                               , v1 :: Dynamic
                               , v2 :: Dynamic}
data TCB = TCB { link :: Maybe TCB
               , id :: Int
               , pri :: Word
               , wkq :: Packet
               , state :: TaskState
               , fn :: State a ()
               , v1 :: RegisterValue
               , v2 :: RegisterValue}

data RegisterValue = {- Device,Handler -} PacketVal Packet | {- Idle -} SeedVal Int | {- Work -} IdleVal Int | 

data TaskState = TaskState {nonempty : Bool, awaitingpacket : Bool, blocked : Bool}

data Tasks = Tasks {idle :: TCB, work :: TCB, handlera :: TCB, handlerb :: TCB, deva :: TCB, devb :: TCB}

data IdleState = IdleState {activationCount :: Int}

data HandlerState = HandlerState {buffer :: [Int]}

data SimulationState = SimulationState {idleState :: IdleState, handleraState :: HandlerState, handlerbState :: HandlerState}

-- Work
-- 1. no packet received -> suspend
-- 2. switch handler to send packet to
-- 3. update packet
-- 4. queue the packet
-- suspend itself

-- Handler
-- 1. add the received packet, if it was from work

-- Schedule is the main loop
-- while there is tcb linked run next tcb
