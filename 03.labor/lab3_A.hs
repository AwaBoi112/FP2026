import Data.Foldable (minimumBy)
import Data.Function (on)
atlag ls = (sum ls)/fromIntegral (length ls)

myLength []=0
myLength (x : xs)=1+myLength xs

myLength2 ls =foldr (\x db->1+db) 0 ls

myLength3 xs =foldr (\_-> (+) 1) 0

myProduct []=1
myProduct (x:xs)=x*myProduct xs

myProduct2 [] res=res
myProduct2 (x:xs) res=myProduct2 xs (res*x)

myProduct3 ls =foldr (*) 1 ls

myMinimum [x]=x
myMinimum (x1:x2:xs)= if x1<x2 then myMinimum (x1:xs) else myMinimum(x2:xs)

myMinimum3 ls=minimum ls

myMinimum4 :: (Foldable t, Ord b) => b -> t b -> b
myMinimum4 ls= foldr min ls

myMaximum [x]=x
myMaximum (x1:x2:xs)= if x1>x2 then myMaximum (x1:xs) else myMaximum(x2:xs)

myMaximum2 (x1:x2:xs)
    |x1>x2=myMaximum2(x1:xs)
    |otherwise=myMaximum2(x2:xs)

myMaximum3 ls =foldl1 max ls

listaN ls n=ls !! n

listaN2 ls n
    |ls==[]=error "ures lista"
    |length ls<=n=error "nem jp"
    |otherwise =ls !! n

ls3=[2]
listaNMap=map (\x-> listaN x 0)

listaFus ls1 ls2 = ls1 ++ ls2

palidrom ls=if ls==reverse ls then "palidorm " else "nem palidrom"

palidrom2 ls =if head ls==last  ls then palidrom2(init $ tail ls) else False

szjLs x
    |x<10=[x]
    |otherwise=szjLs (div x 10) ++[mod x 10]

elsoUtolso (x:xs)=xs ++[x]

elsoUtolso2 xs=tail xs ++ [head xs]

lsAtlag ls =osszeg/hossz
    where
        osszeg=sum ls
        hossz=fromIntegral(length ls)

decP x p= decP (div x p) p ++ [mod x p]

ls7=[(4,2),(100,16)]
decPMap ls =map (uncurry decP) ls

decP2 x p
    |x<p=[x]
    |otherwise= decP (div x p) p ++ [mod x p]

pDec ls p=foldl (\sg x->sg*p+x)0 ls

myMinimumMap ls=map myMinimum ls

myMaximumMap ls=map myMaximum ls


ls4=[]
ls5=[]

listaFuzMap=map (uncurry listaFus) (zip ls4 ls5)

aLs=[3,-2,5,-7]

x0=2
poli [] x0=0
poli (a:aLs) x0 = a + x0*(poli aLs x0)


lsP=[(2.3,4.5),(1.2,6.7)]
p=(3.6,8.9)

tavolsag (x1,y1) (x2,y2)= sqrt((x1-x2)**2+(y1-y2)**2)
minPont lsP p=foldl1 aux lsP
    where
        aux p1 p2=if tavolsag p1 p < tavolsag p2 p then  p1 else p2

minPont2 lsP p=minimumBy (compare `on` tavolsag p) lsP