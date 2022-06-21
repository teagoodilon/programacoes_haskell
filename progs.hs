valorAbsoluto x
    | x > 0     = x
    | otherwise = -x

maiorDeDois :: (Ord t) => t -> t -> t
maiorDeDois a b
    | a < b     = b
    | otherwise = a

menorDeDois :: (Ord t) => t -> t -> t
menorDeDois a b
    | a > b     = b
    | otherwise = a

nroElementos :: [t] -> Int
nroElementos (c:r) = 1 + (nroElementos r)
nroElementos []    = 0

pertence :: (Eq t) => t -> [t] -> Bool
pertence _ [] = False
pertence elem (c:r)
    | elem  == c   = True
    | otherwise    = pertence elem r

maior :: (Ord t) => [t] -> t
maior [c] = c
maior (c:r) = maiorDeDois c (maior r)

nroOcorrencia :: (Eq t) => t -> [t] -> Int
nroOcorrencia e (c:r)
    | e == c = 1 + (nroOcorrencia e r)
    | otherwise = nroOcorrencia e r
nroOcorrencia _ [] = 0

unicaOcorrencia :: (Eq t) => t -> [t] -> Bool
unicaOcorrencia e (c:r)
    | e == c    = not (pertence e r)
    | otherwise = unicaOcorrencia e r
unicaOcorrencia _ [] = False

maioresQue :: (Ord t) => t -> [t] -> [t]
maioresQue n (c:r)
    | c > n     = c:(maioresQue n r)
    | otherwise = maioresQue n r
maioresQue _ [] = []

concatena :: [t] -> [t] -> [t]
concatena [] l = l
concatena (c:r) l = c:(concatena r l)

remove :: (Eq t) => t -> [t] -> [t]
remove e (c:r)
    | e == c    = r
    | otherwise = c:(remove e r)

removeUltimo :: (Eq t) => [t] -> [t]
removeUltimo (c:r)
    | r == []   = []
    | otherwise = c:(removeUltimo r)

removeRepetidos :: (Eq t) => [t] -> [t]
removeRepetidos [a]     = [a]
removeRepetidos (c:r)   = c:(removeRepetidos(removeTodos c r))

removeTodos :: (Eq t) => t -> [t] -> [t]
removeTodos _ [] = []
removeTodos e (c:r)
    | e == c    = removeTodos e r
    | otherwise = c:(removeTodos e r)

menor :: (Ord t) => [t] -> t
menor [e]   = e
menor (c:r) = menorDeDois c (menor r)

removeUltimoMenor l = reverse (remove(menor l) (reverse l))

inverte :: [t] -> [t]
inverte []    = []
inverte (c:r) = concatena (inverte r) [c]

trocaParam :: (a -> b -> b) -> (b -> a -> b)
trocaParam f a b = f b a

intercala :: [t] -> [t] -> [t]
intercala l [] = l
intercala [] l = l
intercala (x:xs)(y:ys) = x:y:(intercala xs ys)

uniao :: (Eq t) => [t] -> [t] -> [t]
uniao l1 (c:r)
    | pertence c l1 = uniao l1 r
    | otherwise     = uniao (l1 ++ [c]) r
uniao l1 []         = l1

interseccao :: (Eq t) => [t] -> [t] -> [t]
interseccao (c:r) l
    | pertence c l  = c:(interseccao r l)
    | otherwise     = interseccao r l
interseccao [] _    = []

mesmosElementos :: (Eq t) => [t] -> [t] -> Bool
mesmosElementos [] [] = True
mesmosElementos [] _  = False
mesmosElementos (c:r) l
    | pertence c l = mesmosElementos r (remove c l)
    | otherwise    = False

sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = m:(sequencia(n-1)(m+1))

insereOrdenado :: (Ord t) => [t] -> t -> [t]
insereOrdenado (c:r) e
    | e <= c    = e:c:r
    | otherwise = c:(insereOrdenado r e)
insereOrdenado [] e = [e]

ordenado :: (Ord t) => [t] -> Bool
ordenado [_]    = True
ordenado (a:b:resto)
    | a <= b    = ordenado (b:resto)
    | otherwise = False   
ordenado []     = True

algumaOrdem :: (Ord t) => [t] -> Bool
algumaOrdem l = (ordenado l) || (ordenado(inverte l))

ordena :: (Ord t) => [t] -> [t]
ordena []       = []
ordena (c:r)    = insereOrdenado (ordena r) c


picos :: (Ord t) => [t] -> [t]
picos [e]                 = []
picos (c:r)               = picos' (((last r):c:r) ++ [c])
    where
    picos' (a:b:c:resto)
        | b > a && b > c = b:(picos' (c:resto))
        | otherwise      = picos' (b:c:resto)
    picos' _             = []
picos _                  = []

rodarEsq :: Int -> [t] -> [t]
rodarEsq n (c:r)
    | n > 0     = rodarEsq (n-1) (r++[c])
    | otherwise = c:r


quickSort :: (Ord t) => [t] -> [t]
quickSort (c:r) = (quickSort [x|x<-r, x<=c]) ++ [c] ++ (quickSort [y|y<-r, y>c])
quickSort []    = []
 

