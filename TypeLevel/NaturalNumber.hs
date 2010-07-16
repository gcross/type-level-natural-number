{- Copyright (c) 2008, Scott E. Dillard. All rights reserved. -}
{- Copyright (c) 2010, Gregory M. Crosswhite. All rights reserved. -}

{-# LANGUAGE EmptyDataDecls #-}

module TypeLevel.NaturalNumber where

data N0
data Succ a

type N1  = Succ N0
type N2  = Succ N1
type N3  = Succ N2
type N4  = Succ N3
type N5  = Succ N4
type N6  = Succ N5
type N7  = Succ N6
type N8  = Succ N7
type N9  = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15

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

predNat :: Succ a -> a
predNat _ = undefined

succNat :: a -> Succ a
succNat _ = undefined

class Nat n where nat :: n -> Int
instance Nat N0 where nat _ = 0
instance Nat a => Nat (Succ a) where nat x = 1 + nat (predNat x)

instance Show N0 where show _ = "0"
instance Nat n => Show (Succ n) where show x = show (1 + nat x)

instance Eq N0 where (==) _ _ = True
instance Nat n => Eq (Succ n) where (==) _ _ = True

instance Ord N0 where compare _ _ = EQ
instance Nat n => Ord (Succ n) where compare _ _ = EQ
