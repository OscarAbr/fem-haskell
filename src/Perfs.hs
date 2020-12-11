module Perfs where

import qualified Operations as O (inverse, funcMotif, genMatrix)
import qualified Invert as I (invert)


matrixTest n = O.genMatrix O.funcMotif n

lente m = O.inverse m
rapide m = I.invert m

