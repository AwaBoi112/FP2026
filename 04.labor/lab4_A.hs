import System.Win32 (LOCALESIGNATURE(lsCsbDefault))

elsoNParosNegyzete n = [ x^2 | x <- take n [2,4.. n*2] ]

fel2 n=take n (ls 1) 
    where 
        ls i=replicate i i :ls(i+1)


fel2_2 n=aux n
    where 
        aux 1=replicate 1 1
        aux i =aux(i-1) ++ replicate i i


fel 3 n= aux n
    where
        aux 1=replicate 1 2
        aux i=aux (i-1)++replicate i (i*2)

fel4 n=[n,n-1 ..1 ]++[1..n]

fel5 n=take n ls
    where
        ls=[True,False] ++ls

fel6 n=take n ls
    where
        ls=[0,1 ,-1]++ls

osztok n = foldl (\res x -> if mod n x == 0 then res + 1 else res) 1 [1 .. n `div` 2]

maxParatlanOszto n =maximum[i|i<-[1..n], mod n i==0,odd i]