module Hardware.SpaceInvaders.SimBoard where

import Clash.Prelude
import Clash.Annotations.TH

import RetroClash.Utils
import RetroClash.Barbies
import Hardware.SpaceInvaders.MainBoard
import Hardware.SpaceInvaders.Video

topEntity
    :: "CLK"         ::: Clock System
    -> "RESET"       ::: Reset System
    -> "SWITCHES"    ::: Signal System (BitVector 8)
    -> "TILT"        ::: Signal System Bool
    -> "COIN"        ::: Signal System Bool
    -> "PLAYER1"     ::: Signal System (Pure Player)
    -> "PLAYER2"     ::: Signal System (Pure Player)
    -> "VID_READ"    ::: Signal System (Maybe (Unsigned 8))
    -> "VID_LINE"    ::: Signal System (Maybe (Index VidY))
    -> ( "VID_ADDR"  ::: Signal System (Maybe VidAddr)
       , "VID_WRITE" ::: Signal System (Maybe (Unsigned 8))
       )
topEntity = withEnableGen mainBoard

makeTopEntity 'topEntity
