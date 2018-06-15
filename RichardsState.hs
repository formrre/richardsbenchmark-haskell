module RichardsState where
import Data.STRef
import Data.Array.ST
import Control.Monad.ST
import Control.Applicative
import Data.Word
import Data.Bits

data Packet s = None | Packet {plink :: STRef s (Packet s),pid :: Int,kind :: Int,a1 :: Int,a2 :: STArray s Word Int}
isNone None = True
isNone _ = False

addToPacket :: Packet s -> Packet s -> ST s (Packet s)
addToPacket this queue = do
    writeSTRef (plink this) None
    if isNone queue
        then pure this
        else (lastPacket queue) >>= (\result -> writeSTRef (plink result) this >> pure queue)

lastPacket :: Packet s -> ST s (Packet s)
lastPacket pkt = do
    link <- readSTRef (plink pkt)
    case link of
        None -> pure link
        _ -> lastPacket link

data Tcb s = Tcb {queue :: STRef s (Packet s),priority :: STRef s Int,state :: STRef s Int}
{-
checkPriorityAdd :: Tcb s -> Tcb s -> Packet s -> ST s (Tcb s)
checkPriorityAdd this task pkt = do
    valQueue <- readSTRef (queue this)
    if isNone $ valQueue
        then
            writeSTRef (queue this) pkt >> markRunnable this >> (readSTRef (priority this) >>=(\pthis->readSTRef (priority task) >>=(\ptask->if pthis > ptask then pure this else pure task)))
        else
            addToPacket pkt valQueue >>= writeSTRef (addToPacket this pkt) >> pure task
-}
data Scheduler s = Scheduler {queueCount :: STRef s Int,holdCount :: STRef s Int,blocks :: STArray s Int (Maybe (Tcb s)),list :: Maybe (STRef s (Tcb s)), currentTcb :: STRef s (Maybe (Tcb s)), currentId :: Maybe Int}
--we know that currenttcb is always initialized at this point
enqueue :: Scheduler s -> Packet s -> ST s (Maybe (Tcb s))
enqueue this pkt = do
    (Just currTcb) <- readSTRef (currentTcb this)
    t <- readSTArray (this blocks) (pid pkt)
    case t of
        Nothing -> pure Nothing
        Just tIn -> modifySTRef (queueCount this) (+1) >> writeSTRef (plink packet) None >> writeSTRef (pid packet) (currentId this) >> ((readSTRef (currentId this)) >>= writeSTRef (pid pkt)) >> checkPriorityAdd tIn currTcb pkt
stateRunnable :: Int
stateRunnable = 1
markRunnable :: Tcb s -> ST s ()
markRunnable this = readSTRef (state this) >>= (\oldState -> writeSTRef (state this) (oldState .|. stateRunnable))
