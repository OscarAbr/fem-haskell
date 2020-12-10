module Invert where

-- Gauss-Seidel Inversion
-- Ref: Mathematical Methods for Physics and Engineering, K.F. Riley & al.

import Prelude hiding ((<*>),(<>))

nb = 10

diag m = r
 where n = length m
       m'= zip [0..] m
       r = map (\(i,v)->v!!i) m'

norm (m,v) = (m2,v2)
 where d = diag m
       m'= zip d m
       v'= zip d v
       m2= map (\(k,v)->map (/k) v) m'
       v2= map (\(k,e)->e/k) v'

ident n = map (\i->(z i)++[1.0]++(z (n-i-1))) [0..(n-1)] 
 where z i = take i (repeat 0.0) 

loop 0 f x = x
loop n f x = loop (n-1) f (f x)

solve (a,b) = loop nb fx x
 where (c,d) = norm (a,b)
       n = length c
       f = (ident n) <--> c
       x = d
       fx x = (f <> x) <+> d

invert m = loop nb fx x
 where k = diag m
       c = map (\(k,v)->map (/k) v) (zip k m)
       i = ident (length m)
       d = map (\(k,v)->map (/k) v) (zip k i)
       x = d
       f = i <--> c
       fx x = (f <**> x) <++> d

infixr <+>
(<+>) = zipWith (+)
infixr <*>
(<*>) = zipWith (*)
infixr <.>
v <.> v' = sum (v <*> v')

infixr <>
m <> v = map (<.>v) m
infixr <-->
(<-->) = zipWith (zipWith (-))
infixr <++>
(<++>) = zipWith (zipWith (+))
infixr <**>
m <**> m' = r
 where t = trans m'
       r = map (\v->map (<.>v) t) m

trans ([]:_) = []
trans xs     = (map head xs):(trans (map tail xs))

