module Example2 where
import Fem
import Operations

--exemple 2 (pont à 5 points) on voit ici qu'il est relativement aisé d'implémenter des exemples.
listPointex2 = [[0.0,0.0],[2.0,2.0],[4.0,0.0],[6.0,2.0],[8.0,0.0]]


listLiaisonex2 = [[0.0,1.0],[0.0,2.0],[1.0,2.0],[1.0,3.0],[2.0,3.0],[2.0,4.0],[3.0,4.0]]

k1 = total listLiaisonex2 listPointex2

k1_lim = subMatrix[0,1,8,9] k1
forceEx2 = [0.0,0.0,0.0,0.0,0.0,-1.0,0.0,0.0,0.0,0.0]
forceEx2_lim = [[0.0],[0.0],[0.0],[-0.1],[0.0],[0.0]]

ui2 = concat (deplacementMatrix k1_lim forceEx2_lim)
u2 = insertZeros ui2

resultat2 :: [Double]
resultat2 = zipWith (+) (concat listPointex2) u2

--pas utilisé
csvx2 = csvX resultat2

csvy2 = csvY resultat2