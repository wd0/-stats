rnd :: Int -> Double -> Double
rnd n x = (fromIntegral (floor (x * t))) / t
    where t = 10^n
    
printLn :: Show a => a -> IO ()
printLn x = putStrLn $ show x
    
getPooled asuc an bsuc bn = (asuc + bsuc)/(an + bn)
getPooledSE asuc an bsuc bn = sqrt $ p*(1 - p)/an + p*(1-p)/bn
  where p = getPooled asuc an bsuc bn
getTestStat asuc an bsuc bn = (ap - bp)/pooledSE
  where ap = asuc / an
        bp = bsuc / bn
        pooledSE = getPooledSE asuc an bsuc bn 

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
  let asuc = 47
      an = 146
      bsuc = 5 
      bn = 76
      ap = asuc/an
      bp = bsuc/bn
      se = getPropSE ap an bp bn
      pool = getPooled asuc an bsuc bn
      poolSE = getPooledSE pool an bn
  printLn $ getConfInterval 0.95 se bp ap
  printLn $ getTestStat asuc an bsuc bn
