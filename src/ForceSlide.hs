module ForceSlide where

import ExampleMaillage
import Fem

resultatTriangleMaillageSlide n f = zipWith (+) (concat (trianglePointsOnly n)) (uTriangleSlide n f)

uiTriangleSlide n f = concat (deplacementMatrix (ktriangleLim n) (testFLimSlide n f))

--avec points fixes
uTriangleSlide n f = insertZeros (uiTriangleSlide n f)
a= -0.05
--avec pts fixes
testFLimSlide n f = forceTriangleLim [[1.0,0.0],[0.75,1.305],[0.25,0.435],[0.5,0.87],[1.25,1.305],[1.75,0.435],[1.5,0.87]] [[0.0,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f]] (init(tail(trianglePointsOnly n)))
