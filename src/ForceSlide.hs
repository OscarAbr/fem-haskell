module ForceSlide where

import ExampleMaillage
import Fem

resultatTriangleMaillageSlide n f = zipWith (+) (concat (trianglePointsOnly n)) (uTriangleSlide n f)

uiTriangleSlide n f = concat (deplacementMatrix (ktriangleLim n) (testFLimSlide n f))

--avec points fixes
uTriangleSlide n f = insertZeros (uiTriangleSlide n f)

--avec pts fixes
testFLimSlide n f = forceTriangleLim [[1.0,1.74]] [[0.1 * fromIntegral f,0.0]] (init(tail(trianglePointsOnly n)))
