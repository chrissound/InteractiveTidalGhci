:set -XOverloadedStrings
import Sound.Tidal.Context
:l Tidal
import Foreign.Store

(_, getNow) <- bpsUtils

lookupStore 0 :: IO (Maybe (Store (ParamPattern -> IO ())))
d0 <- maybe (fst <$> superDirtSetters getNow) (readStore) it
store1 <- writeStore (Store 0) (d0)

lookupStore 1 :: IO (Maybe (Store (ParamPattern -> IO ())))
d1 <- maybe (fst <$> superDirtSetters getNow) (readStore) it
store1 <- writeStore (Store 1) (d1)

let hush = mapM_ ($ silence) [d0,d1]

d0 $ silence
d1 $ silence
d0Action d0
d1Action d1

threadDelay 1500000

