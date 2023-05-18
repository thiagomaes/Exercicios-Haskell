listac2 = [x^3|x<-[1..8]]

duplicalist [] = []
duplicalist (x:xs) = x:x:duplicalist xs

impares [] = []
impares (x:xs) = if mod x 2 == 1 then x: impares xs else impares xs

impares2 [] = []
impares2 (a:b) | mod a 2 == 1 = a : impares2 b
               | otherwise = impares2 b

nprimeiros 0 _ = []
nprimeiros _ [] = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

-- 1. função pertence
pertence _ [] = False
pertence p (x:xs) = p == x || pertence p xs

-- 2. função interseção
intersecao [] _ = []
intersecao (x:xs) y  | pertence x y = x : intersecao xs y
                     | otherwise = intersecao xs y

-- 3. função sem usar reverse
invertlist [] = []
invertlist (x:xs) = invertlist xs ++ [x]

-- 4. função retorna os ultimos
nultimos _ [] = []
nultimos n (x:xs) = reverse(nprimeiros n (reverse(x:xs)))

-- 5. soma o primeiro cada lista
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys

-- 6. potencias de 2 até 2^n
pot2 n = [2^x | x <- [1..n]]

--7
intercalacao [] ls = ls
intercalacao ls [] = ls
intercalacao (x:xs) (y:ys) | x < y = x: intercalacao xs (y:ys)
                           |otherwise = y: intercalacao (x:xs) ys

-- 8. menor numero da lista
menor [x] = x
menor (x:y:xs)  | x <= y = menor (x:xs)
                | otherwise = menor (y:xs)
--9
removerElem n [] = []
removerElem n (x:xs) | n == x = xs
                     |otherwise = x: removerElem n xs

--10
ordenar [] = []
ordenar (x:xs) = let min = menor (x:xs)
                     novaLista = removerElem min (x:xs)
                     resposta = min: ordenar novaLista
                 in resposta

--11
insereOrd n [] = []
insereOrd n (x:xs) | n < x = n:x:xs
                   | n == x = x:xs
                   |otherwise = x: insereOrd n xs

--12
enesimo n (x:xs) | n == 1 = x
                 |otherwise = enesimo (n-1) xs

--13            
repetir 0 e = []
repetir n e = e: repetir (n-1) e

--14
removeTab [] = []
removeTab (x:xs) | '\t' == x = ' ' : removeTab xs
                 |otherwise = x: removeTab xs

--15
minusculas [] = []
minusculas (x:xs) | pertence x ['A'..'Z'] = toLower x : minusculas xs
                  |otherwise = x: minusculas xs

-- 16. função para inverso
inversoDupla [] = []
inversoDupla ((x,y):xs) = (y,x) : inversoDupla xs                  
                
--17
simetrico [] = []
simetrico ((a,b):xs) |(a==b) = True : simetrico xs
                     |otherwise = False : simetrico xs

--18
numString' 0 = []
numString' n = chr ((rem n 10) +48) : numString' (div n 10)
numString n = inverso (numString' n)

--19
stringNum' [] = []
stringNum' (x:xs) = ord x -48 : stringNum' xs
stringNum'' [] = 0
stringNum'' (x:xs) = x*10^(length(x:xs)-1) + stringNum'' xs
stringNum ls = stringNum'' (stringNum' ls)

--20
decBin' 0 = [0]
decBin' 1 = [1]
decBin' n | mod n 2 == 1 = 1 : decBin' (div n 2)
          | mod n 2 == 0 = 0 : decBin' (div n 2)
decBin'' n = inverso (decBin' n)
decBin n = numString (stringNum'' (decBin'' n))

--21
binDec' (x:xs) = stringNum' (x:xs)
binDec'' [] = 0
binDec'' (x:xs) = x*2^(length(x:xs)-1) + binDec'' xs
binDec ls = binDec'' (binDec' ls)

--22
trocoCafe' 0 = []
trocoCafe' x | x >= 50 = (50,div x 50) : trocoCafe' (mod x 50)
             | x >= 20 = (20,div x 20) : trocoCafe' (mod x 20)
             | x >= 10 = (10,div x 10) : trocoCafe' (mod x 10)
             | otherwise = (5,div x 5) : trocoCafe' (mod x 5)
trocoCafe x y | (y-x) < 5 = []
              | otherwise = trocoCafe' (y-x)

