import Control.Monad.Trans.Cont (reset)
osszeg a b = a + b

kivonas a b = a - b

osztmar a b = mod a b

elsof a b = (-b)/a

abszl a = if a < 0 then -a else a

szamelojel a
  | a < 0 = "negativ"
  | a > 0 = "pozitiv"
  | otherwise = "nulla"


maxi a b = if a > b then a else b

max2 a b
    | a > b = a
    | otherwise = b


min2 a b
    | a < b = a
    | otherwise = b

masodf a b c
    | delta < 0 = 0
    | otherwise = (gy1 , gy2)
    where
      delta = b**2-4*a*c
      gy1 = (-b + sqrt delta)/(2*a)
      gy2 = (-b - sqrt delta)/(2*a)


elempar ep1 ep2 = (a==c && b==d)||(a==d && b==c)
    where
        (a,b)=ep1
        (c,d)=ep2

elempar2 (a,b) (c,d) = (a==c && b==d)||(a==d && b==c)
fakt1 0=1
fak1 n =n*fakt1 (n-1)


fakt2 n
    | n<0=error "neg szam"
    | n==0=1
    | otherwise=n*fakt2 (n-1)

fakt3 n res
    | n < 0     = error "neg szam"
    | n == 0    = res
    | otherwise = fakt3 (n-1) (res*n)


hatvanyX x n
    | n<0=error "neg szam"
    | otherwise =x**n

hatvanyX2 x n
    |   n<0=error "neg szam"
    |   otherwise =x^n

hatvanyX3 x n
    | n<0=error "neg szam"
    | n==0=1
    | otherwise=x*hatvanyX3 x (n-1)

negyzetgyok n = [sqrt i | i <- [1..n]]

negyzetszam n=[i ^ 2 | i<-[1..n]]

kobszam n=[i ^ 3 | i<-[1..n]]

nemNegyzet n= [i | i<-[1..n] ,i/=(sqrt i ^2)]

hatvany x n =[x^i|i<-[1..n]]

parosOsztok x= [i | i<-[1..x],mod x i==0,mod i 2==0]

osztok x=[i|i<-[1..x],mod x 1 ==0]

primszam x=osztok x==[1,x]

primszamN n=[i|i<-[1..n],primszam i]

osszetett n=[i|i<-[1..n], primszam i == False]

osszetettParatlan n=[i|i<-[1..n], primszam i == False,mod i 2==1]

pitagorasz n=[(a,b,c)| c<-[1..n],b<-[1..c],a<-[1..b],a^2+b^2==c^2]

betuszam= zip['a'..'z'] [0..25]

szamok =zip[0..5][5,4..0]










main :: IO()
main = do
    putStr "elempar (6,7) (7,6)"


