module Superfluid where

{- Basic types -}
type Timestamp = Int

{- Show utilities -}
toWad wad = round $ wad * (10^18)
wad2human wad = fromIntegral wad / (10^18)

{- Real-time Balance -}

data RealtimeBalance = RealtimeBalance {
    availableBalance :: Integer,
    deposit :: Integer,
    owedDeposit :: Integer
}
add (RealtimeBalance a1 a2 a3) (RealtimeBalance b1 b2 b3) =
    (RealtimeBalance (a1 + b1) (a2 + b2) (a3 + b3))
instance Show RealtimeBalance where
    show (RealtimeBalance a1 _ _) = show a1

class SuperAgreementClass a where
    subBalanceOf :: a -> address -> Timestamp -> RealtimeBalance

{- Super Agreements -}
data SuperAgreement = CFA_v1 {
    lastUpdatedAt :: Timestamp,
    subBalance :: Integer,
    flowRate :: Integer
} |  IDA_v1

instance SuperAgreementClass SuperAgreement where
    subBalanceOf (CFA_v1 {flowRate = r, subBalance = b, lastUpdatedAt = ut}) a t =
        RealtimeBalance (toInteger(t - ut) * r + b) 0 0
    subBalanceOf IDA_v1 a t = RealtimeBalance 0 0 0

instance Show SuperAgreement where
    show (CFA_v1 ut b r) = "CFA "
        ++ " flowrate is " ++ (show r)
        ++ ", and sub balance is " ++ (show b)
        ++ ", last updated at " ++ (show ut)

{- SuperfluidAccount -}

data SuperfluidAccount = SuperfluidAccount {
    address :: String,
    staticBalance :: Integer,
    agreements :: [SuperAgreement]
}

balanceOf :: SuperfluidAccount -> Timestamp -> RealtimeBalance
balanceOf (SuperfluidAccount {agreements = agreements, staticBalance = sb, address = a}) t =
    foldl add (RealtimeBalance sb 0 0) (map (\agr -> subBalanceOf agr a t) agreements)
cfa :: SuperfluidAccount -> SuperAgreement
cfa sa = head $ cfaOnlyList $ agreements sa
cfaGet agr = case agr of
    CFA_v1 _ _ _ -> [agr]
    _ -> []
cfaOnlyList (x:xs) = (cfaGet x) ++ (cfaOnlyList xs)
cfaOnlyList _ = []
noImplAgreement agr = case agr of
    CFA_v1 _ _ _ -> []
    _ -> [agr]
unknownAgreementsList (x:xs) = (noImplAgreement x) ++ (unknownAgreementsList xs)
unknownAgreementsList _ = []

updateFlowRate :: SuperAgreement -> Integer -> Timestamp -> SuperAgreement
updateFlowRate (CFA_v1 t1 b1 r) d t2 = CFA_v1 t2 (b1 + (toInteger(t2-t1) * r)) (r + d)

updateFlow :: SuperfluidAccount -> SuperfluidAccount -> Integer -> Timestamp -> (SuperfluidAccount, SuperfluidAccount)
updateFlow a b r t2 = (
        (SuperfluidAccount {address = (address a), staticBalance = (staticBalance a), agreements = (
            [(updateFlowRate (cfa a) (r * (-1)) t2)]
            ++ (unknownAgreementsList (agreements a))
        )}),
        (SuperfluidAccount {address = (address b), staticBalance = (staticBalance b), agreements = (
            [(updateFlowRate (cfa b) r t2)]
            ++ (unknownAgreementsList (agreements b))
        )}))

{- SuperfluidAccountSnapshot -}

data SuperfluidAccountSnapshot = SuperfluidAccountSnapshot SuperfluidAccount Timestamp
instance Show SuperfluidAccountSnapshot where
    show (SuperfluidAccountSnapshot sa  t) =
        "Balance of " ++ (address sa) ++ " is "
        ++  (show $ wad2human $ availableBalance $ sa `balanceOf` t)
