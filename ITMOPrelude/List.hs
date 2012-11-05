{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a b) = Succ(length b)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
(Cons a b) ++ c = Cons a (b ++ c)

-- Список без первого элемента
tail :: List a -> List a
tail (Cons a b) = b

-- Список без последнего элемента
init :: List a -> List a
init (Cons a Nil) = Nil
init (Cons a b) = Cons a (init b)

-- Первый элемент
head :: List a -> a
head (Cons a b) = a

-- Последний элемент
last :: List a -> a
last (Cons a Nil) = a
last (Cons a b) = last b

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero a = Nil
take _ Nil = Nil
take (Succ n) (Cons a b) = Cons a (take n b)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero a = a
drop _ Nil = Nil
drop (Succ n) (Cons a b) = drop n b

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons a b) = if' (p a) (Cons a (filter p b)) (filter p b)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons a b) = case p a of
	Nothing -> gfilter p b
	Just c -> Cons c (gfilter p b)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons a b) = if' (p a) (Cons a (takeWhile p b)) (Nil)

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (Cons a b) = if' (p a) (dropWhile p b) (Cons a b)

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ Nil = Pair Nil Nil
span p (Cons a b) = if' (p a) (Pair r q) (Pair Nil (Cons a b)) where
	r = Cons a (fst res)
	q = snd res 
	res = span p b

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span (not.p)

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
Cons a b !! Zero = a
Cons a b !! Succ(n) = b !! n

helpReverse :: List a -> List a -> List a
helpReverse a Nil = a
helpReverse a (Cons b c) = helpReverse (Cons b a) c

-- Список задом на перёд
reverse :: List a -> List a
reverse = helpReverse Nil

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons a b) = map (Cons a) p ++ p where p = subsequences b

permHelp :: a -> List a -> List (List a)
permHelp a Nil = Cons (Cons a Nil) Nil
permHelp c (Cons a b) = Cons (Cons c (Cons a b))  (map (Cons a) (permHelp c b))

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations Nil = Cons Nil Nil 
permutations (Cons a b) = concatMap (permHelp a) (permutations b)

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a b) = foldl f (f z a) b

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a b) = Cons p (scanl f p b) where p = (f z a)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a b) = f a (foldr f z b)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons a b) = Cons (f a (head p)) p where p = scanr f z b

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat (Cons a Nil) = a
concat (Cons a b) = a ++ (concat b)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a b) = (f a) ++ (concatMap f b)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip = zipWith Pair

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil
zipWith f (Cons a b) (Cons c d) = Cons (f a c) (zipWith f b d)
