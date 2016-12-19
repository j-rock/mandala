{-# LANGUAGE BangPatterns #-}
module Mandala.Types.Math where

data V2 rep ann = V2 { _v2x :: !rep
                     , _v2y :: !rep
                     } deriving (Eq, Show)

instance Num rep => Num (V2 rep ann) where
  V2 x y + V2 u v = V2 (x+u) (y+v)
  {-# INLINE (+) #-}
  V2 x y * V2 u v = V2 (x*u) (y*v)
  {-# INLINE (*) #-}
  abs (V2 x y) = V2 (abs x) (abs y)
  {-# INLINE abs #-}
  signum (V2 x y) = V2 (signum x) (signum y)
  {-# INLINE signum #-}
  fromInteger x = V2 (fromInteger x) (fromInteger x)
  {-# INLINE fromInteger #-}
  negate (V2 x y) = V2 (negate x) (negate y)
  {-# INLINE negate #-}

scale2 :: Num rep => V2 rep ann -> rep -> V2 rep ann
scale2 (V2 x y) k = V2 (k*x) (k*y)
{-# INLINE scale2 #-}

data V3 rep ann = V3 { _v3x :: !rep
                     , _v3y :: !rep
                     , _v3z :: !rep
                     } deriving (Eq, Show)

scale3 :: Num rep => V3 rep ann -> rep -> V3 rep ann
scale3 (V3 x y z) k = V3 (k*x) (k*y) (k*z)
{-# INLINE scale3 #-}

instance Num rep => Num (V3 rep ann) where
  V3 x y z + V3 u v w = V3 (x+u) (y+v) (z+w)
  {-# INLINE (+) #-}
  V3 x y z * V3 u v w = V3 (x*u) (y*v) (z*w)
  {-# INLINE (*) #-}
  abs (V3 x y z) = V3 (abs x) (abs y) (abs z)
  {-# INLINE abs #-}
  signum (V3 x y z) = V3 (signum x) (signum y) (signum z)
  {-# INLINE signum #-}
  fromInteger x = V3 (fromInteger x) (fromInteger x) (fromInteger x)
  {-# INLINE fromInteger #-}
  negate (V3 x y z) = V3 (negate x) (negate y) (negate z)
  {-# INLINE negate #-}
