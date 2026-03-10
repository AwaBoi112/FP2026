import Control.Monad.Trans.Cont (reset)
szamjegyszorzat 0=1
szamjegyszorzat n = mod n 10*szamjegyszorzat (div n 10)

szjSzorzat n
    | n<0=error"neg szam"
    | div n 10 ==0=n
    | otherwise=mod n 10 *szjSzorzat(div n 10)

szjSzorzatLs ls =map szjSzorzat ls

szamJOsszeg n szj
    | szj>9=error "nem szamjegy"
    | n<10 =if n==szj then szj else 0
    |otherwise =
        if mod n 10 == szj 
            then szj+szamJOsszeg(div n 10) szj
            else szamJOsszeg(div n 10) szj

szjOsszeg ls = map szamJOsszeg ls


parosSzj n
    | n<0= parosSzj(abs n)
    | n<10= if even n then 1 else 0
    |otherwise =if mod (mod n 10) 2== 0 
                    then 1+parosSzj(div n 10 ) 
                    else parosSzj(div n 10)

lnSzj n ln
    |n<0=lnSzj(abs n) ln
    |n<10=max n ln
    |otherwise=if mod n 10> ln then lnSzj(div n 10) (mod n 10) else
        lnSzj (div n 10) ln

bzamDSzj n b d
    | n<0=error "helytelen"
    | n<b= if n==d then 1 else 0
    | otherwise=if mod n b==d then 1+bzamDSzj (div n b) b d else
        bzamDSzj (div n b) b d

fiboN n=fiboSg 0 1 0 n
    where
        fiboSg a b res n
            |n==0=res
            |otherwise= fiboSg b res (res +b) (n-1)
fibo _ _ res 0=res
fibo a b res n =fibo b res (res+b) n-1

fiboN2 n =fibo 0 1 0 1000

fiboSzamok n = map (fibo 0 1 0) [0 .. n] 

