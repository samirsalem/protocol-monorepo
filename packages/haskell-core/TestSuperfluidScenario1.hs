import Data.Time.Clock.POSIX (getPOSIXTime)
import Superfluid

now =  do
    t <- round `fmap` getPOSIXTime
    return $ fromIntegral t

createSuperfluidAccount address t = SuperfluidAccount address (100*(10^18)) [
    (CFA_v1 t 0 0)
    ]
sumAll accounts t = foldl add (RealtimeBalance 0 0 0) (map (flip balanceOf t) accounts)

main = do
    -- t1
    t1 <- now
    let alice1 = createSuperfluidAccount "alice" t1
    let bob1 = createSuperfluidAccount "bob" t1
    let carol1 = createSuperfluidAccount "carol" t1
    print $ "alice1 cfa: " ++ (show $ cfa alice1)
    print $ "bob1 cfa: " ++ (show $ cfa bob1)
    print $ "carol1 cfa: " ++ (show $ cfa carol1)
    print $ SuperfluidAccountSnapshot alice1 t1
    print $ SuperfluidAccountSnapshot bob1 t1
    print $ SuperfluidAccountSnapshot carol1 t1
    let (alice2, bob2) = updateFlow alice1 bob1 (toWad 0.0001) t1
    let (alice3, carol2) = updateFlow alice2 carol1 (toWad 0.0002) t1
    -- t2, advancing 1 day
    print "Advance for 1 day"
    let t2 = t1 + (3600 * 24)
    print $ "alice3 cfa: " ++ (show $ cfa alice3)
    print $ "bob2 cfa: " ++ (show $ cfa bob2)
    print $ "carol2 cfa: " ++ (show $ cfa carol2)
    print $ SuperfluidAccountSnapshot alice3 t2
    print $ SuperfluidAccountSnapshot bob2 t2
    print $ SuperfluidAccountSnapshot carol2 t2
    print $ "Total balance sum: " ++ (show $ wad2human $ availableBalance $ sumAll [alice3, bob2, carol2] t2)
