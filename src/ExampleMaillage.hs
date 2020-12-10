module ExampleMaillage where
import Fem
import Operations
import Maillage

cleanIdx [(_,xy)] = [xy]
cleanIdx ((_,xy):q) = xy: (cleanIdx q)
pairToList [(a,b)] = [[a,b]]
pairToList ((a,b):q) = [a,b]: (pairToList q)

intLtoDoubleL :: [Int] -> [Double]
intLtoDoubleL [t] = [convToDouble t]
intLtoDoubleL (t:q) = convToDouble t : intLtoDoubleL q

intMtoDoubleM:: [[Int]] -> [[Double]]
intMtoDoubleM [t] = [intLtoDoubleL t]
intMtoDoubleM (t:q) = intLtoDoubleL t : intMtoDoubleM q


triangleOriginal = [[0.0,0.0],[1.0,1.74],[2.0,0.0]]

triangleMaillage = discretize 1 [triangleOriginal]

trianglePointsIdx = points triangleMaillage

trianglePointsOnly = cleanIdx trianglePointsIdx

triangleMaillageLines = set (lins trianglePointsIdx triangleMaillage)

triangleMaillageLines2 = pairToList(triangleMaillageLines)


ktriangle = total (intMtoDoubleM triangleMaillageLines2) trianglePointsOnly

ktriangle_lim = subMatrix[0,1,10,11] ktriangle

--force exercée sur le point [1.0,1.74] direction x norme 0.1
forceTriangle_lim = [[0.0],[0.0],[0.0],[0.0],[0.0],[0.0],[0.1],[0.0]]

uiTriangle = concat (deplacementMatrix ktriangle_lim forceTriangle_lim)
uTriangle = insertZeros uiTriangle

resultatTriangleMaillage :: [Double]
resultatTriangleMaillage = zipWith (+) (concat trianglePointsOnly) uTriangle





