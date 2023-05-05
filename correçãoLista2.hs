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

-- 4. função retorna os ultimos
nultimos _ [] = []
nultimos n (x:xs) = reverse(nprimeiros n (reverse(x:xs)))

-- 3. função sem usar reverse
invertlist [] = []
invertlist (x:xs) = invertlist xs ++ [x]


-- 1. função pertence
pertence _ [] = False
pertence p (x:xs) = p == x || pertence p xs

-- 2. função interseção
intersecao [] _ = []
intersecao (x:xs) y  | pertence x y = x : intersecao xs y
                     | otherwise = intersecao xs y


-- 5. soma o primeiro cada lista
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x + y) : soma2 xs ys

-- 6. potencias de 2 até 2^n
pot2 n = [2^x | x <- [1..n]]

-- 7.

-- 8. menor numero da lista
menor [x] = x
menor (x:y:xs)  | x <= y = menor (x:xs)
                | otherwise = menor (y:xs)
                
-- 16. função para inverso
inversoDupla [] = []
inversoDupla ((x,y):xs) = (y,x) : inversoDupla xs                  
                
