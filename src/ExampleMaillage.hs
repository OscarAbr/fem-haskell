module ExampleMaillage where
import Fem
import Operations
import Maillage

cleanIdx [(_,xy)] = [xy]
cleanIdx ((_,xy):q) = xy: (cleanIdx q)
pairToList [(a,b)] = [[a,b]]
pairToList ((a,b):q) = [a,b]: (pairToList q)


findIndexOf i e (x:xs) 
    | e == x    = i
    | x:xs == [x] = -1
    | otherwise = findIndexOf (i+1) e xs





intLtoDoubleL :: [Int] -> [Double]
intLtoDoubleL [t] = [convToDouble t]
intLtoDoubleL (t:q) = convToDouble t : intLtoDoubleL q

intMtoDoubleM:: [[Int]] -> [[Double]]
intMtoDoubleM [t] = [intLtoDoubleL t]
intMtoDoubleM (t:q) = intLtoDoubleL t : intMtoDoubleM q


triangleOriginal = [[0.0,0.0],[1.0,1.74],[2.0,0.0]]
--meshed with depth 1
triangleMaillage1 = discretize 1 [triangleOriginal]

trianglePointsIdx1 = points triangleMaillage1

trianglePointsOnly1 = cleanIdx trianglePointsIdx1

triangleMaillageLines1 = set (lins trianglePointsIdx1 triangleMaillage1)

triangleMaillageLines12 = pairToList(triangleMaillageLines1)
listLiaisonsMaillage1 = intMtoDoubleM triangleMaillageLines12

ktriangle1 = total (intMtoDoubleM triangleMaillageLines12) trianglePointsOnly1

ktriangle_lim1 = subMatrix[0,1,10,11] ktriangle1

--force exercée sur le point [1.0,1.74] direction x norme 0.1
forceTriangle_lim1 = [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.3],[0.0]]

uiTriangle1 = concat (deplacementMatrix ktriangle_lim1 forceTriangle_lim1)
uTriangle1 = insertZeros uiTriangle1

resultatTriangleMaillage1 :: [Double]
resultatTriangleMaillage1 = zipWith (+) (concat trianglePointsOnly1) uTriangle1


--generalizing

triangleMaillage n = discretize n [triangleOriginal]

trianglePointsIdx n = points (triangleMaillage n)


trianglePointsOnly n = cleanIdx (trianglePointsIdx n)


triangleMaillageLines n = set (lins (trianglePointsIdx n) (triangleMaillage n))

triangleMaillageLines2 n = pairToList(triangleMaillageLines n)

listLiaisonsMaillage n = intMtoDoubleM (triangleMaillageLines2 n)

ktriangle n = total (intMtoDoubleM (triangleMaillageLines2 n)) (trianglePointsOnly n)

ktriangleLim n = subMatrix[0,1,convToDouble(length (ktriangle n) - 2),convToDouble(length (ktriangle n) - 1)] (ktriangle n)







forceTriangleLim:: Int -> Int -> Matrix
forceTriangleLim i n
 | i == 2 *(findIndexOf 0 [1.0,1.74] (removeIndex (siz - 2.0) (removeIndex 0.0 m)))    =[0.1]:forceTriangleLim (i+1) n  
 | i == k = [[0.0]]
 | otherwise = [0.0]:forceTriangleLim (i+1) n
 where k = length (ktriangleLim n) -1
       m = trianglePointsOnly n
       m2 = subMatrix[0.0,siz-1.0] m
       siz = convToDouble (length m)


uiTriangle n = concat (deplacementMatrix (ktriangleLim n) (forceTriangleLim 0 n))

uTriangle n = insertZeros (uiTriangle n)



resultatTriangleMaillage n = zipWith (+) (concat (trianglePointsOnly n)) (uTriangle n)


