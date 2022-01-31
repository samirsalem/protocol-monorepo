module Superfluid.Instances.Simple.Address
    ( SimpleAddress
    , createSimpleAddress
    ) where


{- Simple Superfluid Address type
-}
newtype SimpleAddress = SimpleAddress String

instance Eq SimpleAddress where
    (==) (SimpleAddress a) (SimpleAddress b)= a == b
instance Ord SimpleAddress where
    compare (SimpleAddress a) (SimpleAddress b)= compare a b
instance Show SimpleAddress where
    show (SimpleAddress a )= a

createSimpleAddress :: String -> SimpleAddress
createSimpleAddress x = SimpleAddress x
