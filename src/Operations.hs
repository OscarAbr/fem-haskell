module Operations where 

type Matrix = [[Double]]

-- comatrix(i,j) matrices de cofacteur, pour générer la comatrice ou calculer le déterminant
--cofac :: Double->Double->Matrix->Matrix
--cofac i j m = iter 0 i j m
 --where iter  l i j (x:xs) = if (l==i) then xs' else x':(iter (l+1) i j xs)
   --     where x' = iter2 0 i j x
    --          xs'= map (iter2 0 i j) xs
      -- iter2 l i j (x:xs) = if (l==j) then xs else x:(iter2 (l+1) i j xs)


-- "for" loop
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

-- built identity matrix of dimension n
idt :: Int -> Matrix
idt n = map (\i->line 0 i n) [0..(n-1)]
 where line i n m = 
        if (i==m) then [] else
         if (i==n) then 1:(line (i+1) n m) else 0:(line (i+1) n m)
 

-- multiplication of a matrix m by a constant k
multk :: Double->Matrix->Matrix
multk k m = map (map (*k)) m

-- matrix sum and difference
plus, minus::Matrix->Matrix->Matrix
plus  m m' = zipWith (zipWith (+)) m m'
minus m m' = zipWith (zipWith (-)) m m'

-- scalar product of two vectors
dot v v' = sum (zipWith (*) v v')

-- matrix multiplication
mult::Matrix->Matrix->Matrix
mult m m' = map (\v->map (\v'->dot v v') (trans m')) m

-- matrix transposition
trans :: Matrix->Matrix
trans ([]:_) = []
trans xs     = (map head xs):(trans (map tail xs))

findIndex :: Double -> [a] -> a
findIndex 0 (x:_) = x
findIndex i (_:xs) = findIndex (i-1) xs

-- obtenir un coefficient de coordonnées (i,j) sur une matrice m
coeff :: Double -> Double -> Matrix -> Double
coeff i j m = findIndex j (findIndex i m)

--convertit juste en doubles
deter :: Matrix -> Double
deter m = deter' m (convToDouble ((length m)-1))

convToDouble :: Int -> Double
convToDouble x = 1.0 * fromIntegral x


--calcul du déterminant d'une matrice, selon la definition
deter' :: Matrix -> Double -> Double
deter' [[x]] j = x
deter' m 0 = coeff 0 0 m * deter (cofac 0 0 m)
deter' m j = coeff 0 j m * (-1)^ (round j) * deter (cofac 0 j m) + deter' m (j-1)


-- comatrice
com :: Matrix -> Matrix
com m = map(\j -> line j 0 n) [0..(n-1)]
 where line i j n = if (j == n) then [] else (-1)^ round (i+j) * deter (cofac  i j m) : line i (j+1) n
       n = convToDouble (length m)

inverse :: Matrix -> Matrix
inverse m = multk (1/deter m)  (trans . com $ m)

buildLine :: Int -> [Double] -> Int -> Int -> [Double]
buildLine n motif sizeMatrix sizeMotif = take n  (repeat 0) ++ motif ++ take (sizeMatrix - sizeMotif - n) (repeat 0)




buildMatrix' :: Int -> [Double]  -> Int -> Matrix
buildMatrix' 0 motif sizeMatrix = [buildLine 0 motif sizeMatrix (length motif)]
buildMatrix' i motif sizeMatrix = buildMatrix' (i-1) motif sizeMatrix  ++ [buildLine i motif sizeMatrix (length motif)]

buildMatrix :: [Double]  -> Int -> Matrix
buildMatrix motif sizeMatrix = [buildLine 0 (tail motif) sizeMatrix (length motif - 1)] ++ buildMatrix' (sizeMatrix - 3) motif sizeMatrix ++ [buildLine (sizeMatrix- (length (init motif))) (init motif) sizeMatrix (length motif - 1)]

--version améliroée de buildMatrix, on donne une fonction f(x,y) et elle genère une matrice en appliquant la fonction aux indices.

genMatrix :: ((Double,Double) -> Double) -> Double -> Matrix
genMatrix f n = map(\j -> line j 0 n) [0..(n-1)]
 where line i j n 
            | j == n = [] 
            | otherwise = f(i,j) : line i (j+1) n

-- des exemples
-- pour matrice identité
funcEqual :: (Double,Double) -> Double
funcEqual (i,j) = if (i == j) then 1.0 else 0.0
--motif du 1er exemple presenté (1D)
funcMotif :: (Double,Double) -> Double
funcMotif (i,j)  
    | i == j    = -2.0
    | i == j+1  = 1.0
    | i == j-1  = 1.0 
    | otherwise = 0.0


removeIndex :: Double -> [a] -> [a]
removeIndex 0 (x:xs) =  xs
removeIndex i (x:xs) = x : (removeIndex (i-1) xs)

--utile pour comatrice/deter
removeCross :: Double -> Matrix -> Matrix
removeCross x m  = removeIndex x (map (removeIndex x) m)

--pas utilisé
subMatrix :: [Double] -> Matrix -> Matrix
subMatrix [] m = m
subMatrix (x:xs) m = removeCross x (subMatrix xs m)
--pas utilisé
removeIndexes :: [Double] -> [Double] -> [Double]
removeIndexes [] l = l
removeIndexes (x:xs) l = removeIndexes xs (removeIndex x l)


--  matrices de cofacteur, pour générer la comatrice ou calculer le déterminant
-- cofac 1 2 [[1,2,3],[4,5,6],[7,8,9]] enleve ligne 1, colonne 2
-- => [[1.0,2.0],[7.0,8.0]]
cofac :: Double -> Double -> Matrix -> Matrix
cofac x y m  = removeIndex x (map (removeIndex y) m)