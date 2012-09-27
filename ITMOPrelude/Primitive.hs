{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero _ = LT
natCmp _ Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
Zero -. _ = Zero
Succ(n) -. Succ(m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n m = Pair (natDiv n m) (natDiv n m)

natDiv :: Nat -> Nat -> Nat
natDiv Zero Zero = undefined
natDiv Zero _ = Zero
natDiv n m = if' (natLt n m) Zero (Succ(natDiv(n -. m) m)) 

natMod :: Nat -> Nat -> Nat
natMod Zero Zero = undefined
natMod Zero _ = Zero
natMod n m = if' (natLt n m) m (Succ(natDiv(n -. m) m)) 

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd Zero m = m
gcd n m = gcd (natMod m n) m

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos (Succ Zero)     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos (Succ a)) = (Neg a)
intNeg (Neg a) = Pos (Succ a)

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Neg _) (Pos _) = LT
intCmp (Pos _) (Neg _) = GT
intCmp (Pos n) (Pos m) = natCmp n m
intCmp (Neg n) (Neg m) = natCmp m n

intEq :: Int -> Int -> Bool
intEq n m = case intCmp n m of 
	EQ -> True
	_ -> False

intLt :: Int -> Int -> Bool
intLt n m = case intCmp n m of
	LT -> True
	_ -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos n) .+. (Pos m) = Pos (n +. m)
(Neg n) .+. (Neg m) = Neg (n +. m +. Succ(Zero))
Pos n .+. Neg m = if' (natLt n m) (Neg (m -. n +. Succ(Zero))) (Pos (n -. m -. Succ(Zero)))
n .+. m = m .+. n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
Pos n .*. Pos m = Pos (n *. m)
Neg n .*. Neg m = intNeg (Neg n) .*. intNeg (Neg m)
Pos n .*. Neg m = intNeg (Pos n .*. intNeg (Neg m))
n .*. m = m .*. n

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos n) m) = (Rat (Pos m) n)
ratInv (Rat (Neg n) m) = ratNeg(Rat (intNeg(Neg m)) n)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. (Pos d)) ((Pos b) .*. c)

ratEq :: Rat -> Rat -> Bool
ratEq n m = case ratCmp n m of 
	EQ -> True
	_ -> False

ratLt :: Rat -> Rat -> Bool
ratLt n m = case ratCmp n m of
	LT -> True
	_ -> False


infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a b) %+ (Rat c d) = Rat p q where 
	p = (a .*. Pos d) .+. (Pos b .*. c) 
	q = b *. d

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) = Rat p q where 
	p = a .*. c
	q = b *. d

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
