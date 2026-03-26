import Data.Char
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
import GHC.Num.Backend (c_mpn_add_1)
lsTo = "ez egy PrOBA szoveg. ez egy masik proBa! Tobbfele irasJEL ::Hasznalat i"
--tokenize :: [Char] -> [String]
tokenize = words . map (irasjelHelyettesit .toLower)

irasjelHelyettesit c
    | notElem c ",.;:!?\"'()[]<>" = c
    | otherwise = ' '


lengthLista ls = map length ls


myMinimum2 :: (Num b, Enum b, Ord a) => [a] -> (a, [b])
myMinimum2 ls = (m, map snd $ filter fg $ zip ls [0,1..])
    where
    m = maximum ls
    fg k = fst k == m

talalat x ls =l1
        where
            zipls=zip ls [0,1..]
            l1=map snd $ filter (\y-> fst y ==x) zipls

ps ls=sum [t2| (t1,t2,t3)<-ls]

main = do
        let ls=[("golya",25,"ms"),("sas",12,"ab")]
        let result=ps ls
        let madarls=concatMap (<> "")[t1 |(t1,t2,t3)<-ls]
        putStrLn $ madarls <> "pop szam" ++ show result



    -- let lista="ez egy PrOBA szoveg. ez egy masik proBa! Tobbfele irasJEL ::"
    -- let l1=tokenize lista
    -- let l2=lengthLista l1
    -- --let m1= minimum l2
    -- putStrLn "a szavak hossza:"
    -- print l2
    -- let (minim,indexek)=myMinimum2 l2
    -- let l3=zip l1 l2
    -- print l3
    -- let r1 =[fst(l3!!i)|i<-indexek]
    -- print r1
        -- let a=5
        -- let l1=[3,23,45,651]
        -- let t1=talalat a l1
        -- let a2='e'
        -- let l2="vig viSes"
        -- let t2=talalat a2 l2
        -- let c1=concatMap ((<> "") . show) t1
        -- putStrLn $ show a <> "talalat pozicio" <> c1
        -- let c2=concatMap ((<> "") . show) t2
        -- putStrLn $ show a2 <> "talalat pozicio" <> c2

        