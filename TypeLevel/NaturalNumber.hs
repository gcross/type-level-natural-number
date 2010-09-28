{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}
{- Copyright (c) 2010, Gregory M. Crosswhite. All rights reserved. -}

{-# LANGUAGE EmptyDataDecls #-}

module TypeLevel.NaturalNumber where

import Data.Typeable

data Zero
data SuccessorTo n

type N0  = Zero
type N1  = SuccessorTo N0
type N2  = SuccessorTo N1
type N3  = SuccessorTo N2
type N4  = SuccessorTo N3
type N5  = SuccessorTo N4
type N6  = SuccessorTo N5
type N7  = SuccessorTo N6
type N8  = SuccessorTo N7
type N9  = SuccessorTo N8
type N10 = SuccessorTo N9
type N11 = SuccessorTo N10
type N12 = SuccessorTo N11
type N13 = SuccessorTo N12
type N14 = SuccessorTo N13
type N15 = SuccessorTo N14

type One      = N1
type Two      = N2
type Three    = N3
type Four     = N4
type Five     = N5
type Six      = N6
type Seven    = N7
type Eight    = N8
type Nine     = N9
type Ten      = N10
type Eleven   = N11
type Twelve   = N12
type Thirteen = N13
type Fourteen = N14
type Fifteen  = N15

n0  :: N0  ; n0  = undefined
n1  :: N1  ; n1  = undefined
n2  :: N2  ; n2  = undefined
n3  :: N3  ; n3  = undefined
n4  :: N4  ; n4  = undefined
n5  :: N5  ; n5  = undefined
n6  :: N6  ; n6  = undefined
n7  :: N7  ; n7  = undefined
n8  :: N8  ; n8  = undefined
n9  :: N9  ; n9  = undefined
n10 :: N10 ; n10  = undefined
n11 :: N11 ; n11  = undefined
n12 :: N12 ; n12  = undefined
n13 :: N13 ; n13  = undefined
n14 :: N14 ; n14  = undefined
n15 :: N15 ; n15  = undefined

predecessorOf :: SuccessorTo n -> n
predecessorOf _ = undefined

successorTo :: n -> SuccessorTo n
successorTo _ = undefined

class NaturalNumber n where
    naturalNumberAsInt :: n -> Int
instance NaturalNumber Zero where
    naturalNumberAsInt _ = 0
instance NaturalNumber n => NaturalNumber (SuccessorTo n) where
    naturalNumberAsInt x = 1 + naturalNumberAsInt (predecessorOf x)

instance Show Zero where
    show _ = "0"
instance NaturalNumber n => Show (SuccessorTo n) where
    show = show . (+1) . naturalNumberAsInt . predecessorOf

instance Eq Zero where
    (==) _ _ = True
    (/=) _ _ = False
instance NaturalNumber n => Eq (SuccessorTo n) where
    (==) _ _ = True
    (/=) _ _ = False

instance Ord Zero where
    compare _ _ = EQ
instance NaturalNumber n => Ord (SuccessorTo n) where
    compare _ _ = EQ

instance Typeable Zero where
    typeOf n = mkTyConApp (mkTyCon $ "N#0") []
instance NaturalNumber n => Typeable (SuccessorTo n) where
    typeOf n = mkTyConApp (mkTyCon $ "N#" ++ show (naturalNumberAsInt n + 1)) []
