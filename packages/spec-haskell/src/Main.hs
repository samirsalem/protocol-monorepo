module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Superfluid

now =  do
    t <- round `fmap` getPOSIXTime
    return $ fromIntegral t

createSuperfluidAccount address t = Superfluid.Account address (100*(10^18)) [
    (Superfluid.CFA_v1 t 0 0)
    ]

sumAll accounts t = foldl Superfluid.add (Superfluid.RealtimeBalance 0 0 0) (map (flip Superfluid.balanceOf t) accounts)

main :: IO ()
main = do
    -- t1
    t1 <- now
    let alice1 = createSuperfluidAccount "alice" t1
    let bob1 = createSuperfluidAccount "bob" t1
    let carol1 = createSuperfluidAccount "carol" t1
    print $ "alice1 cfa: " ++ (show $ Superfluid.cfa alice1)
    print $ "bob1 cfa: " ++ (show $ Superfluid.cfa bob1)
    print $ "carol1 cfa: " ++ (show $ Superfluid.cfa carol1)
    print $ Superfluid.AccountSnapshot alice1 t1
    print $ Superfluid.AccountSnapshot bob1 t1
    print $ Superfluid.AccountSnapshot carol1 t1
    let (alice2, bob2) = Superfluid.updateFlow alice1 bob1 (Superfluid.toWad 0.0001) t1
    let (alice3, carol2) = Superfluid.updateFlow alice2 carol1 (Superfluid.toWad 0.0002) t1
    -- t2, advancing 1 day
    print "Advance for 1 day"
    let t2 = t1 + (3600 * 24)
    print $ "alice3 cfa: " ++ (show $ Superfluid.cfa alice3)
    print $ "bob2 cfa: " ++ (show $ Superfluid.cfa bob2)
    print $ "carol2 cfa: " ++ (show $ Superfluid.cfa carol2)
    print $ Superfluid.AccountSnapshot alice3 t2
    print $ Superfluid.AccountSnapshot bob2 t2
    print $ Superfluid.AccountSnapshot carol2 t2
    print $ "Total balance sum: " ++ (show $ Superfluid.wad2human $ Superfluid.availableBalance $ sumAll [alice3, bob2, carol2] t2)
