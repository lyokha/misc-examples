-- Compile: ghc --make asyncTest.hs
-- Run: ./asyncTest

import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async

asyncTask1 = async $ do
    result <- return 10 `catch`
        (const $ return 20 :: SomeException -> IO Int)
    uninterruptibleMask_ (threadDelay 2000000) >> print result

asyncTask2 = async $ do
    result <-
        (threadDelay 2000000 >> return 10) `catch`
            (const $ putStrLn "Caught!" >>
                uninterruptibleMask_ (threadDelay 2000000) >>
                    return 20 ::
                SomeException -> IO Int
            )
    print result

asyncTask3 = async $ do
    result <-
        (threadDelay 2000000 >> return 10) `catch`
            (const $ putStrLn "Caught!" >>
                uninterruptibleMask_ (threadDelay 2000000) >>
                    return 20 ::
                SomeException -> IO Int
            )
    mask_ $ print result

asyncTask4 = async $ mask $ \restore -> do
    result <-
        restore (threadDelay 2000000 >> return 10) `catch`
            (const $ putStrLn "Caught!" >>
                uninterruptibleMask_ (threadDelay 2000000) >>
                    return 20 ::
                SomeException -> IO Int
            )
    uninterruptibleMask_ (threadDelay 2000000) >> print result

asyncTask5 = mask $ \restore -> async $ do
    result <-
        restore (threadDelay 2000000 >> return 10) `catch`
            (const $ putStrLn "Caught!" >>
                uninterruptibleMask_ (threadDelay 2000000) >>
                    return 20 ::
                SomeException -> IO Int
            )
    uninterruptibleMask_ (threadDelay 2000000) >> print result

-- do not let waiting for a task break main function!
catchInMain = handle
    (const $ putStrLn "Caught in main!" :: SomeException -> IO ())

main = do
    a1 <- asyncTask1
    threadDelay 1000000
    throwTo (asyncThreadId a1) ThreadKilled
    catchInMain $ wait a1

    a2 <- asyncTask2
    threadDelay 1000000
    throwTo (asyncThreadId a2) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a2) ThreadKilled
    catchInMain $ wait a2

    a3 <- asyncTask3
    threadDelay 1000000
    throwTo (asyncThreadId a3) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a3) ThreadKilled
    catchInMain $ wait a3

    a4 <- asyncTask4
    threadDelay 1000000
    throwTo (asyncThreadId a4) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a4) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a4) ThreadKilled
    wait a4

    a5 <- asyncTask5
    threadDelay 1000000
    throwTo (asyncThreadId a5) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a5) ThreadKilled
    threadDelay 1000000
    throwTo (asyncThreadId a5) ThreadKilled
    wait a5

