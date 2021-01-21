module ForceSlide where

import ExampleMaillage
import Fem

import Interpret
--Un exemple pour le slider, f étant la variable du slider (1-10)
--resultatTriangleMaillageSlide n f = zipWith (+) (concat (trianglePointsOnly n)) (uTriangleSlide n f)




--uiTriangleSlide n f = concat (deplacementMatrix (ktriangleLim n) (testFLimSlide n f))

--avec points fixes
--uTriangleSlide n f = insertZeros (uiTriangleSlide n f)



uCircleSlide n  f = concat (deplacementMatrix (kcircle  n) (testCFLimSlide n f))

resultatCircleMaillageSlide n f = zipWith (+) (concat (circlePointsOnly n)) (uCircleSlide n f)


uTriangleSlide n f = concat (deplacementMatrix (ktriangle n) (testFLimSlide n f))

resultatTriangleMaillageSlide n f = zipWith (+) (concat (trianglePointsOnly n)) (uTriangleSlide n f)


a= -0.03
--avec pts fixes
testFLimSlide n f = forceTriangleLim [[6.0,8.0]] [[-a *fromIntegral f,0.0]] (trianglePointsOnly n)

testCFLimSlide n f = forceTriangleLim [[4.6875,2.2733375000000002],[4.6875,1.6238125]] [[a *fromIntegral f,0.0], [a *fromIntegral f,0.0]] (circlePointsOnly n)