{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module ITMOPrelude.Algebra where

import ITMOPrelude.Primitive
import ITMOPrelude.List

class Monoid m where
	mempty :: m 
	mappend :: m -> m -> m
	
newtype Sum a = Sum { getSum :: a }	
	
instance Monoid (Sum Nat) where 
	mempty = Sum Zero
	mappend a b = Sum $ getSum a +. getSum b

instance Monoid (Sum Int) where 
	mempty = Sum intZero
	mappend a b = Sum $ getSum a .+. getSum b

instance Monoid (Sum Rat) where
	mempty = Sum $ Rat intZero (Succ Zero)
	mappend a b = Sum $ getSum a %+ getSum b
	
instance Monoid (Product Rat) where
	mempty = Product $ Rat intOne (Succ Zero)
	mappend a b = Product $ getProduct a %* getProduct b
	
newtype Product a = Product { getProduct :: a }	
	
instance Monoid (Product Nat) where 
	mempty = Product $ Succ Zero
	mappend a b = Product $ getProduct a *. getProduct b
	
instance Monoid (Product Int) where 
	mempty = Product $ intOne
	mappend a b = Product $ getProduct a .*. getProduct b

newtype And = And { getAnd :: Bool }
	
instance Monoid And where
	mempty = And True
	mappend a b = And $ getAnd a && getAnd b

newtype Or = Or { getOr :: Bool }

instance Monoid Or where
	mempty = Or False
	mappend a b = Or $ getOr a && getOr b


instance Monoid (List a) where
	mempty = Nil
	mappend = (++)

instance Monoid (Maybe a) where 
	mempty = Nothing
	mappend Nothing a = a
	mappend a b = a
	
class Monoid m => Group m where
	ginverse :: m -> m
	
instance Group (Sum Int) where
	ginverse a = Sum $ intNeg $ getSum a
	
instance Group (Sum Rat) where
	ginverse a = Sum $ ratNeg $ getSum a
	
instance Group (Product Rat) where
	ginverse a = Product $ ratInv $ getProduct a
	
