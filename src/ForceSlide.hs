module ForceSlide where

import ExampleMaillage
import Fem
import Interpret
--Un exemple pour le slider, f étant la variable du slider (1-10)
--resultatTriangleMaillageSlide n f = zipWith (+) (concat (trianglePointsOnly n)) (uTriangleSlide n f)

--uiTriangleSlide n f = concat (deplacementMatrix (ktriangleLim n) (testFLimSlide n f))

--avec points fixes
--uTriangleSlide n f = insertZeros (uiTriangleSlide n f)



uCircleSlide n  f = concat (deplacementMatrix (kcircle n) (testCFLimSlide n f))

resultatCircleMaillageSlide n f = zipWith (+) (concat (circlePointsOnly n)) (uCircleSlide n f)

a= -0.1
--avec pts fixes
---testFLimSlide n f = forceTriangleLim [[1.0,0.0],[0.75,1.30],[0.25,0.435]],[0.5,0.87],[1.25,1.305],[1.75,0.435],[1.5,0.87]] [[0.0,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f],[-a *fromIntegral f,-a *fromIntegral f]] (init(tail(trianglePointsOnly n)))

testCFLimSlide n f = forceTriangleLim [[4.6875,2.2733375000000002],[4.6875,1.6238125]] [[a *fromIntegral f,0.0], [a *fromIntegral f,0.0]] (circlePointsOnly n)