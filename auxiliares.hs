
map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : (map2 f xs)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = if (f x)	then x : (filter2 f xs)
							else (filter2 f xs)

-- La funcion f a aplicar -> mi neutro -> la lista en la que quiero aplicar f -> el resultado final
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z [] = z
foldr2 f z (x:xs) = f x (foldr2 f z xs)

-- cantTotal = foldr ((+) . len) 0 \\ len = length??

main = print 1
