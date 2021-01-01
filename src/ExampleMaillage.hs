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


--stage 1 apply force[0.1],[0.0] if [1.0,1.74] is encountered
forceTriangle []  = [] 
forceTriangle ([1.0,1.74]:q) = [0.1] : ([0.0]:forceTriangle q)
forceTriangle (_:q) = [0.0] : ([0.0]:forceTriangle q)
--stage 2 apply force f  if p is encountered
forceTriangle2 :: [Double] -> [Double] -> Matrix -> Matrix
forceTriangle2 p f []  = [] 
forceTriangle2 p f (point:q) 
 | p == point = [head f] : ((tail f):forceTriangle2 p f q)
 | otherwise = [0.0] : ([0.0]:(forceTriangle2 p f q))

--stage 3 apply force f1...fs to p1...ps if p1...ps is encountered (f1:fs) (p1:ps)
--I could have used the previous function and summed the results but this is easy to follow too
forceTriangle3 :: Matrix -> Matrix -> Matrix -> Matrix ->Matrix -> Matrix
forceTriangle3 ptsInit fInit _ _ []  = [] 
forceTriangle3 ptsInit fInit [] fs (h:t)  = [0.0] : ([0.0]:forceTriangle3 ptsInit fInit ptsInit fInit t)
forceTriangle3 ptsInit fInit ps [] (h:t)  = [0.0] : ([0.0]:forceTriangle3 ptsInit fInit ptsInit fInit t)
forceTriangle3  ptsInit fInit (p1:ps) (f1:fs) (point:q) 
 | p1 == point = [head f1] : ((tail f1):forceTriangle3 ptsInit fInit ptsInit fInit q)
 | otherwise = forceTriangle3 ptsInit fInit ps fs  (point:q) 

---applique les forces aux points donnés, sors la matrice des forces
--exemple : forceTriangleLim [[1.0,1.74],[1.0,0.0]] [[0.1,0.0],[0.0,-1]] [[1.0,0.0],[0.5,0.87],[1.5,0.87],[1.0,1.74]]
--applique la force [0.1,0.0] si la 3eme liste comporte le point [1.0,1.74]
--on aura en sortie de cette exemple:
--[[0.0],[-1.0],[0.0],[0.0],[0.0],[0.0],[0.1],[0.0]]
-- force en direction de -y norme 1 pour le 1er point, direction de x norme 0.1 pour le dernier  
forceTriangleLim :: Matrix -> Matrix ->Matrix-> Matrix
forceTriangleLim ptsInit fInit listPoints = forceTriangle3 ptsInit fInit ptsInit fInit listPoints

--avec pts fixes
testFLim n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,0.2],[-0.3,0.1],[0.3,0.1],[0.0,-0.2]] (init(tail(trianglePointsOnly n)))

--sans points fixes
testFLim2 :: Deep -> Matrix
testFLim2 n = forceTriangleLim [[1.0,1.74],[0.5,0.87],[1.5,0.87],[1.0,0.0]] [[0.0,0.2],[0.0,0.1],[0.0,0.1],[0.0,0.0]] (trianglePointsOnly n)


uiTriangle n = concat (deplacementMatrix (ktriangleLim n) (testFLim n))


--avec points fixes
uTriangle n = insertZeros (uiTriangle n)

---sans points ifxes
uTriangle2 n = concat (deplacementMatrix (ktriangle n) (testFLim2 n))

--utilise les pts fixes de base (aux extrémités)
resultatTriangleMaillage n = zipWith (+) (concat (trianglePointsOnly n)) (uTriangle n)