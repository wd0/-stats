rnd :: Int -> Double -> Double
rnd n x = (fromIntegral (floor (x * t))) / t
    where t = 10^n
    
printLn :: Show a => a -> IO ()
printLn x = putStrLn $ show x
    
getPooled asuc an bsuc bn = (asuc + bsuc)/(an + bn)
getPooledSE p an bn = sqrt $ p*(1 - p)/an + p*(1-p)/bn

getSE as an bs bn = sqrt $ as^2/an + bs^2/bn
getPropSE ap an bp bn = sqrt $ ap*(1-ap)/an + bp*(1-bp)/bn
getCritZ z
  | z == 0.90 = 1.645
  | z == 0.95 = 1.96
  | z == 0.99 = 2.575
getConfInterval confLevel se ap bp = (mean - errMargin, mean + errMargin)
  where mean = ap - bp 
        errMargin = (getCritZ confLevel)*se
        
getDF as an bs bn = numer/denom
  where arat = (as^2/an)^2
        brat = (bs^2/bn)^2
        numer = (^2) $ arat + brat 
        denom = 1/(an - 1)*arat + 1/(bn - 1)*brat 

main = do
  let asuc = 174
      an = 1057
      bsuc = 184
      bn = 890
      p = rnd 4 $ getPooled asuc an bsuc bn
  printLn $ rnd 4 $ getPooledSE p an bn 
  printLn $ getConfInterval 0.95 (getPropSE 0.857 12636 0.842 12894) 0.857 0.842
