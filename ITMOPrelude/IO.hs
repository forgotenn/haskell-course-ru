{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

--State RealWorld Nat
--RealWorld -> (RealWorld, Nat)
getNat' :: RealWorld -> (RealWorld, Nat)
getNat' (RealWorld stdIn stdOut exitCode) = (RealWorld (tail stdIn) stdOut exitCode,
                                             head stdIn)

getNat :: IO Nat
getNat = State $ getNat' 

putNat' :: Nat -> RealWorld -> (RealWorld, ())
putNat' a (RealWorld stdIn stdOut exitCode) = (RealWorld stdIn (Cons a stdOut) exitCode,())

--Nat -> (State RealWorld ())
--Nat -> (RealWorld -> (RealWorld, ())
putNat :: Nat -> IO ()
putNat a = State $ putNat' a 

setExitCode' :: Nat -> RealWorld -> (RealWorld, ())
setExitCode' e (RealWorld stdIn stdOut exitCode) = (RealWorld stdIn stdOut e, ())

setExitCode :: Nat -> IO ()
setExitCode e = State $ setExitCode' e 
