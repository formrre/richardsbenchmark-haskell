{-# LANGUAGE DuplicateRecordFields #-}
import Prelude hiding(id)
import Data.Word
import Data.Dynamic
import Control.Monad.Cont
import Control.Monad.State.Lazy 
import Data.Map hiding(take)
import Data.Bits

{- 
    Implementation plan:
        we need to store all packets in a map of current packets, stored by id and instead of following links as in imperative implementation we need to lookup the id in the current state so we need a data structure with following operations
    efficient delete by id
    efficient insert
    efficient last inserted element access
    efficient first inserted element access
    list of current packets from back to front
 -}

{-
data IdPktStore = IdPktStore {_firstpkt :: Packet, _storemap :: (Map Int Packet), _storelist :: [Packet], _lastpkt :: Packet}

insert :: IdPktStore -> Packet -> IdPktStore
insert (IdPktStore first storemap storelist lastpkt) pkt = IdPktStore first () 
--deleteFirstPkt
--deleteLastPkt


data Kind = KindWork | KindDevice

data Packet = Packet { pid :: Int
                     , kind :: Kind
                     , a1 :: Int
                     , a2 :: [Int] }

data TCB = TCB { tlink :: Maybe TCB
               , tid :: Int
               , pri :: Word
               , wkq :: Packet
               , state :: TaskState
               , fn :: State SimulationState ()}
newtype TCBmap = TCBmap (Map Int TCB)

data PacketMap = PacketMap (Map Int Packet)

data RegisterValue = EmptyReg | {- Device,Handler -} PacketVal Packet | {- Idle -} SeedVal Int | {- Work -} IdleVal Int

data TaskState = TaskState {nonempty :: Bool, awaitingpacket :: Bool, blocked :: Bool}

data Tasks = Tasks {idle :: TCB, work :: TCB, handlera :: TCB, handlerb :: TCB, deva :: TCB, devb :: TCB}

data IdleState = IdleState {activationCount :: Int}

data HandlerState = HandlerState {buffer :: [Int]}

data SimulationState = SimulationState {tcb :: TCB, idleState :: IdleState, handleraState :: HandlerState, handlerbState :: HandlerState,v1 :: RegisterValue,v2 :: RegisterValue, scheduler :: Scheduler}

data Scheduler = Scheduler {queueCount :: Word, holdCount :: Word, blocks :: [Maybe TCB], list :: [TCB], currentTCB :: Maybe TCB, currentId :: Int}
-}
dataSize = 4
idIdle = 0
idWorker = 1
idHandlerA = 2
idHandlerB = 3
idDeviceA = 4
idDeviceB = 5
{-
markAsSuspended :: TaskState -> TaskState
markAsSuspended t = t{awaitingpacket = True} 

markRunnable t = t{nonempty = True,awaitingpacket=False,blocked=False}


queue :: SimulationState -> Packet -> (SimulationState,Maybe Packet)
queue simstate pkt = case t of
        Nothing -> (simstate,Nothing)
        (Just p) -> (simstate{scheduler=(scheduler simstate){queueCount=(queueCount (scheduler simstate))+1})
    where
        t = (blocks (scheduler simstate)) !! pid pkt
-}
-- Worker
-- 1. no packet received -> suspend
-- 2. switch handler to send packet to
-- 3. update packet
-- 4. queue the packet
-- suspend itself
{-
worker :: Maybe Packet -> State SimulationState ()
worker Nothing = suspendCurrent
worker (Just p) = do
    simState <- get
    let newV1 = if v1 simState == idHandlerA then idHandlerB else idHandlerA
    let newPacket = Packet (link p) (v1 simState) (kind packet) 0 undefined
    suspendCurrent
    where
        --newA2 = zipWith (a2 p) [0..] (\x y -> if y > 26)
        newV2 = v2rec 0  (v2 simstate)
        v2rec i cv2 = if cv2 > 26 then v2rec
-}
--mark as suspended return new tcb
--note we need references as current tcb is not always head of the list
--suspendCurrent :: State SimulationState TCB
--suspendCurrent = _

-- Handler
-- 1. add the received packet, if it was from work
{-
data HandlerName = A | B
handlerRun :: HandlerName -> Maybe Packet -> SimulationState -> SimulationState
handlerRun  hn (Just pkt) = _
handlerRun hn Nothing = handlerCommon
-}
{-
handlerCommon :: HandlerName -> SimulationState -> SimulationState
handlerCommon hn simstate@(SimulationState _tcb _idle _handlerA _handlerB _v1 _v2 _sched) = case _v1 of
    EmptyReg -> suspendCurrent simstate
    PacketVal p -> suspendCurrent (queue newsimstate v) where
        v = undefined
        newsimstate = undefined
        count = a1 _v1
    _ -> undefined    
    where
        handlerAB = case hn of
            A -> _handlerA
            B -> _handlerB
-}
-- schedule is the main loop
-- while there is tcb linked run next tcb
{-
schedule :: SimulationState -> SimulationState
schedule simstate = if length (list simstate) /= 0 then scheduleLoop simstate{currentTCB=(uncons (list simstate))>>=fst} else simstate
-}
{-
scheduleLoop :: SimulationState -> SimulationState
scheduleLoop simState = case currentTCB simState of
    Nothing -> simState
    Just t ->  scheduleLoop newSimState
        where
            newSimState = if isHeldOrSuspended currentTCB then thenBranchSimState else elseBranchSimState
            thenBranchSimState = simState{currentTCB = tlink(fromJust $ currentTCB (scheduler simState))}
--            newSimState = if isHeldOrSuspended currentTCB then simstate{currentTCB=head (tlink $ currentTCB simstate)} else simstate{currentId=currentTcb id,currentTCB=run interimSimState (currentTCB $ scheduler interimSimState)}
            --elseBranchSimState = elseIterimSimState{scheduler = (scheduler elseInterimSimState){currentTCB = run (scheduler elseInterimSimState)}}
            elseInterimSimState = simState{scheduler = (scheduler simState){currentId=tid t}}

-- high-level description of the algorithm-- use a collection and store packets in a tree, sorted by id
-- traverse the tree to find the next priority
-- to delete a packet delete the id

-- marks current tcb as held
holdCurrent :: SimulationState -> (SimulationState,Maybe TCB)
holdCurrent simState = (simRes,tlinkRes)
    where
        simRes = _
        tlinkRes = currentTcb (scheduler simState)

-- mark block not held
-- return currentTcb
release :: SimulationState -> Int -> (SimulationState,Maybe TCB)
release simState id =
-}
-- queue
-- add the packet to end of work queue and
{-
type ResultSim a = (SimulationState,a)

queue :: SimulationState -> Packet -> ResultSim (Maybe ())
queue simState pkt = si 
    where
        
        newQueueCount = (queueCount simState) + 1 
-}
{-
type DevNum = Int
workerTask :: MachineState -> Maybe Packet -> MachineState
workerTask (MachineState _idleState workerState workQueue) Nothing = MachineState 
-}
data TaskState = TaskState 
data IdleState = IdleState {_count :: Int, i_v1 :: Int}
data WorkerState = WorkerState {w_v1 :: Int,w_v2 :: Int}
data DeviceState = DeviceState {d_v1 :: Maybe Packet}
data Packet = Packet {pid :: Int, a1 :: Int, a2 :: [Int]}
data WorkQueue = WorkQueue [Packet]
data MachineState = MachineState IdleState WorkerState WorkerState DeviceState WorkQueue SchedulerState
data SchedulerState = SchedulerState {currentTcb :: Tcb,queueCount :: Int, blocks :: Array}
data Tcb = Tcb (Maybe Tcb) Int Int WorkQueue

idleTask :: MachineState -> Maybe Packet -> MachineState
idleTask (MachineState (IdleState count v1) w1 w2 dev workQueue _) _ = if count == 0 then _ else MachineState (IdleState (count-1) newV1)
    where
        newV1 = if v1 .&. 0 == 0 then v1 `shiftL` 1 else (v1 `shiftL` 1) `xor` 0xD008

deviceTask :: MachineState -> Maybe Packet -> MachineState
deviceTask (MachineState idle w1 w2 (DeviceState dev) workQueue _) Nothing = 
    case d_v1 dev of
        Nothing -> _ -- suspend current
        Just v -> MachineState idle w1 w2 (DeviceState Nothing) (enqueue workQueue v)
deviceTask (MachineState idle w1 w2 dev workQueue) pkt@(Just _) = MachineState idle w1 w2 (DeviceState pkt) dev workQueue -- add holdcurrent

enqueue :: MachineState -> Packet -> MachineState
enqueue = _

id_idle = 0
id_worker = 1
id_handler_a = 2
id_handler_b = 3
id_device_a = 4
id_device_b = 5

workerTask :: Int -> MachineState -> Packet -> MachineState
workerTask mach Nothing _ = _ -- suspend current
workerTask mach m@(MachineState idle w1 w2 dev workQueue _) pkt@(Just _)
    | mach == 0 = MachineState idle newWState w2 dev (enqueue workQueue newpkt)
    | mach == 1 = MachineState idle w1 newWState dev (enqueue workQueue newpkt)
    where
        wT = if mach == 0 then w1 else w2
        new_v1 = if w_v1 wT == id_handler_a then id_handler_b else id_handler_a
        {-  new_a2 is first DATA_SIZE elements of this list, [(v2+1)..26,1..26,1..26,..] -}
        {- new_v2 is just last element of new_a2 -}
        new_v2 = last new_a2
        new_a2 = take dataSize cutList
        cutList = [((w_v2 wT)+1)..26] ++ infList
        infList = concat $ repeat [1..26]
        newpkt = Packet new_v1 0 new_a2 
        newWState = WorkerState new_v1 new_v2

-- callGraph
-- schedule -> run : tcb
-- run : tcb -> run : task
-- run : task :- run : specifictask
-- specifictask = idletask,devicetask,workertask,handlertask
-- devicetask -> suspendcurrent
-- suspendcurrent -> ()

