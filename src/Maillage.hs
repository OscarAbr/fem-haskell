module Maillage where

import Prelude hiding ((<>),(<*>),id)

--- MODEL ---

dx = 400
dy = 400

type Point   = [Double]
type Element = [Point]

e :: Element
e = [[0,0],[dx,0],[0.5*dx,0.87*dy]]

infixr <+>
xs <+> ys = zipWith (+) xs ys

infixr <>
k <> xs = map (k*) xs

mesh0 :: Element -> [Element]
mesh0 [p1,p2,p3] = [e1,e2,e3,e4]
 where p12 = 0.5 <> (p1 <+> p2)
       p23 = 0.5 <> (p2 <+> p3)
       p31 = 0.5 <> (p3 <+> p1)
       e1  = [p1,p31,p12]
       e2  = [p23,p2,p12]
       e3  = [p31,p23,p3]
       e4  = [p23,p31,p12]

mesh :: [Element] -> [Element]
mesh [e]    = mesh0 e
mesh (e:es) = e'++es'
 where e' = mesh0 e
       es'= mesh es

type Deep = Int

discretize :: Deep -> [Element] -> [Element]
discretize 0 es = es
discretize n es = discretize (n-1) (mesh es)

es = discretize 1 [e]

--- VIEW ---

line x1 y1 x2 y2 = concat
  ["\t<line"
  ,  " x1=\"",show' x1,"\" y1=\"",show' y1
  ,"\" x2=\"",show' x2,"\" y2=\"",show' y2
  ,"\"  stroke=\"red\" />\n"]
 where show' = show . round

element [[x1,y1],[x2,y2],[x3,y3]] =
 (line x1 y1 x2 y2)++(line x2 y2 x3 y3)++(line x3 y3 x1 y1)

svg es = concat 
 ["<svg height=\"",show dx,"\" width=\"",show dy,"\">\n"
 ,"\t<rect height=\"",show dx,"\" width=\"",show dy,"\" fill=\"white\"/>\n"
 ,concatMap element es
 ,"</svg>" ]

save file es = writeFile file (svg es)

e0 = [e]
e1 = discretize 1 e0
e2 = discretize 1 e1
e3 = discretize 1 e2
e4 = discretize 1 e3
e5 = discretize 1 e4
e6 = discretize 1 e5
e7 = discretize 1 e6

-- ghc --make Maillage && ./Maillage
-- mogrify -format jpg *.svg
-- animate -delay 100 *.jpg
-- convert -delay 75 -loop 1 *.svg out.gif
main1 = do
 save "out0.svg" e0
 save "out1.svg" e1
 save "out2.svg" e2
 save "out3.svg" e3
 save "out4.svg" e4
 save "out5.svg" e5
 save "out6.svg" e6
 save "out7.svg" e7
 putStrLn ("Finish with "++(show (length e7))++" elements !")
 -- 16300 elements !

--- EXTENSION (circle shape) ---

barycentre [p1,p2,p3] = 0.33 <> (p1 <+> p2 <+> p3)
bar = barycentre e

[p1,p2,p3] = e
p12 = 0.5 <> (p1 <+> p2)

infixr <->
xs <-> ys = zipWith (-) xs ys
infixr <*>
xs <*> ys = zipWith (*) xs ys
infixr <.>
xs <.> ys = sum (xs <*> ys)

distance p p' = sqrt ((p <-> p') <.> (p <-> p'))

rayon = distance p12 bar

cercle c r x = d <= r
 where d = distance c x

shape = cercle bar rayon

select0 shape [p1,p2,p3] = (shape p1)&&(shape p2)&&(shape p3)

select shape []     = []
select shape (e:es) = if (select0 shape e) then e:es' else es'
 where es' = select shape es

[c0,c1,c2,c3,c4,c5,c6,c7] = map (select shape) [e0,e1,e2,e3,e4,e5,e6,e7]
-- c0 == c1 == []

main2 = do
 save "out2.svg" c2
 save "out3.svg" c3
 save "out4.svg" c4
 save "out5.svg" c5
 save "out6.svg" c6
 save "out7.svg" c7
 putStrLn ("Finish with "++(show (length c7))++" elements !")
-- 9497 elements !

--- INDEXATION ---

type Index = Int
type Map   = [(Index,Point)]

indx :: Index -> Point -> Map -> (Map,Index)
indx k p []     = ([(k,p)],k+1)
indx k p ((k',q):qs) = 
  if (p==q) then ((k',q):qs,k) else ((k',q):qs',k'')
 where (qs',k'') = indx k p qs

inde k m [p1,p2,p3] = r
 where (m1,k1) = indx k  p1 m
       (m2,k2) = indx k1 p2 m1
       r       = indx k2 p3 m2

index k m []     = (m,k)
index k m (e:es) = r
 where (m' ,k' ) = inde k m e
       r = index k' m' es

points es = fst (index 0 [] es)


[ps0,ps1,ps2,ps3,ps4,ps5]       = map points [e0,e1,e2,e3,e4,e5]
[ps2b,ps3b,ps4b,ps5b,ps6b,ps7b] = map points [c2,c3,c4,c5,c6,c7]

id ((k,q):qs) p = if (p==q) then k else id qs p

lin m [p1,p2,p3] = [(k1,k2),(k2,k3),(k3,k1)]
 where k1 = id m p1
       k2 = id m p2
       k3 = id m p3

lins m []     = []
lins m (e:es) = e'++es'
 where e' = lin  m e
       es'= lins m es
[ls0,ls1,ls2,ls3,ls4,ls5]       = zipWith lins [ps0,ps1,ps2,ps3,ps4,ps5] [e0,e1,e2,e3,e4,e5]
[ls2b,ls3b,ls4b,ls5b,ls6b,ls7b] = zipWith lins [ps2b,ps3b,ps4b,ps5b,ps6b,ps7b] [c2,c3,c4,c5,c6,c7]

equ (x,y) (x',y') = ((x==x')&&(y==y')) || ((x==y')&&(y==x'))

insrt p []     = [p]
insrt p (q:qs) = if (equ p q) then q:qs else q:(insrt p qs)

set []     = []
set (p:ps) = insrt p (set ps)

[ls0',ls1',ls2',ls3',ls4',ls5']       = map set [ls0,ls1,ls2,ls3,ls4,ls5]
[ls2b',ls3b',ls4b',ls5b',ls6b',ls7b'] = map set [ls2b,ls3b,ls4b,ls5b,ls6b,ls7b]

main = do
 writeFile "pointsCircle6.txt" (show ps6b)
 writeFile "linesCircle6.txt"  (show ls6b')
