{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module ITMOPrelude.Categories where

import ITMOPrelude.List
import ITMOPrelude.Primitive

class Functor f where
	fmap :: (a -> b) -> f a -> f b

instance Functor List where
	fmap = map
	
instance Functor Maybe where
	fmap function Nothing = Nothing
	fmap function (Just a) = Just $ function a

instance Functor (Either e) where
	fmap function (Left a) = Left a 
	fmap function (Right a) = Right $ function a
	
class Functor m => Monad m where
	(>>=) :: m a -> (a -> m b) -> m b
	return :: a -> m a

instance Monad List where
	(>>=) l function = concat $ map function l
	return a = Cons a Nil
	
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
	

	
