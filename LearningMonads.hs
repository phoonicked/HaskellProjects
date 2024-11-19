-- Practice 1: Custom type Maybe
data MyMaybe a = Success a | Failure 
    deriving (Eq, Show)

instance Functor MyMaybe where
    fmap f (Success x) = Success (f x)
    fmap f Failure = Failure

instance Applicative MyMaybe where
    pure x = (Success x)
    (<*>) (Success f) (Success x) = Success (f x)
    (<*>) _ _ = Failure

instance Monad MyMaybe where
    (Success x) >>= f = f x
    Failure >>= _ = Failure

-- Test function for MyMaybe
safeDivide :: Int -> Int -> MyMaybe Int
safeDivide _ 0 = Failure
safeDivide x y = Success (x `div` y)

------------------------------------------------------------------------------------------------

-- Practice 2: Logger
data Logger a = Logger a [String]
    deriving (Eq, Show)

instance Functor Logger where
    fmap f (Logger a logs) = Logger (f a) logs

instance Applicative Logger where
    pure x = Logger x []
    (<*>) (Logger f logs1) (Logger a logs2) = Logger (f a) (logs1 ++ logs2)

instance Monad Logger where
    (Logger x logs) >>= f =
        let Logger y newLogs = f x
        in Logger y (logs ++ newLogs)

-- Test functions for Logger
add :: Int -> Int -> Logger Int
add x y = Logger (x+y) ["Added " ++ show x ++ " to " ++ show y]

multiply:: Int -> Int -> Logger Int
multiply x y = Logger (x * y) ["Multiplied " ++ show x ++ " to " ++ show y]

testFunction1 :: Int -> Logger Int
testFunction1 x = Logger x ["Starting with " ++ show x]
    >>= (`add` 159)
    >>= (`multiply` 2)

------------------------------------------------------------------------------------------------

-- Practice 3: Simple state Monad
newtype MyState s a = MyState { runState :: s -> (a, s) }

instance Functor (MyState s) where
    fmap f (MyState g) = MyState $ \s ->
        let (a, newState) = g s
        in (f a, newState)

instance Applicative (MyState s) where
    pure x = MyState $ \s -> (x, s)
    (<*>) (MyState sf) (MyState sx) = MyState $ \s ->
        let (f, newState1) = sf s
            (x, newState2) = sx newState1
        in (f x, newState2)

instance Monad (MyState s) where
    (MyState sx) >>= f = MyState $ \s ->
        let (x, newState) = sx s
            MyState sy = f x
        in sy newState

modifyState :: (s -> s) -> MyState s ()
modifyState f = MyState $ \s -> ((), f s)

getState :: MyState s s 
getState = MyState $ \s -> (s,s)

setState :: s -> MyState s ()
setState newState = MyState $ \_ -> ((), newState)

testFunction2 :: MyState Int Int
testFunction2 = do
    modifyState (+1)
    modifyState (*2)
    getState

------------------------------------------------------------------------------------------------

-- Practice 4: Counter monad
newtype Counter a = Counter { runCounter :: Int -> (a, Int)}

instance Functor Counter where
    fmap f (Counter g) = Counter $ \counter -> 
        let (a, newCounter) = g counter
        in (f a, newCounter)

instance Applicative Counter where
    pure x = Counter $ \counter -> (x, counter)
    (<*>) (Counter sf)(Counter sx) = Counter $ \counter ->
        let (f, newCounter1) = sf counter
            (x, newCounter2) = sx newCounter1
        in (f x, newCounter2)

instance Monad Counter where
    (Counter sx) >>= f = Counter $ \counter ->
        let (x, newCounter) = sx counter
            Counter sy = f x
        in sy newCounter

modifyCounter :: (Int -> Int) -> Counter ()
modifyCounter f = Counter $ \counter -> ((), f counter)

getCounter :: Counter Int
getCounter = Counter $ \counter -> (counter, counter)

setCounter :: Int -> Counter ()
setCounter counter = Counter $ \_ -> ((), counter)

testFunction3 :: Counter Int
testFunction3 = do
    modifyCounter (+1)
    modifyCounter (*2)
    getCounter