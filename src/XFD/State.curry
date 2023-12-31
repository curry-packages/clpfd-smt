--- This auxiliary module implements a state monad in Curry.

module XFD.State where

infixl 1 `bindS`, `bindS_`

type State s a = s -> (a, s)

bindS :: State s a -> (a -> State s b) -> State s b
bindS state f s = case state s of
                    (x, newS) -> newS `seq` f x newS

bindS_ :: State s a -> State s b -> State s b
bindS_ a b = a `bindS` \_ -> b

returnS :: a -> State s a
returnS x s = (x, s)

getS :: State s s
getS s = (s, s)

putS :: s -> State s ()
putS newState _ = ((), newState)

modifyS :: (s -> s) -> State s ()
modifyS f s = ((), f s)

sequenceS :: [State s a] -> State s [a]
sequenceS =
 foldr (\s newS -> s    `bindS` \a  ->
                   newS `bindS` \as ->
                   returnS (a:as))
       (returnS [])

sequenceS_ :: [State s a] -> State s ()
sequenceS_ = foldr bindS_ (returnS ())

mapS :: (a -> State s b) -> [a] -> State s [b]
mapS f = sequenceS . map f

mapS_ :: (a -> State s b) -> [a] -> State s ()
mapS_ f = sequenceS_ . map f

runState :: State s a -> s -> (a, s)
runState state s = state s

evalState :: State s a -> s -> a
evalState state s = fst (runState state s)

execState :: State s a -> s -> s
execState state s = snd (runState state s)

liftS :: (a -> b) -> State s a -> State s b
liftS f act = act `bindS` returnS . f

liftS2 :: (a -> b -> c) -> State s a -> State s b -> State s c
liftS2 f a b  = a `bindS` \x -> b `bindS` \y -> returnS (f x y)
