{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда


--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State $ \s -> (s, a)
-- State a -> (a -> State b) -> State b
    (>>=) (State f) g = State $ \s -> let 
        (s', a) = f s
        (s'', b) = runState (g a) s' in (s'', b)

instance Functor List where
	fmap = map
	
instance Functor Maybe where
	fmap function Nothing = Nothing
	fmap function (Just a) = Just $ function a

instance Functor (Either e) where
	fmap function (Left a) = Left a 
	fmap function (Right a) = Right $ function a
	

instance Monad List where
	(>>=) l function = concat $ map function l
	return a = ITMOPrelude.List.Cons a ITMOPrelude.List.Nil
	
instance Monad Maybe where
	(>>=) Nothing function = Nothing
	(>>=) (Just a) function = function a
	return a = Just a
	
class Category cat where
	id :: cat a a
	(#) :: cat b c -> cat a b -> cat a c

instance Category (->) where
	id a = a
	(#) f g = f . g
	

