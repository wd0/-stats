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
--getPopDevConf confLevel mean s n = (mean - errMargin, mean + errMargin)
--  where errMargin = (getCritZ confLevel) * s/(sqrt n)
  
getPopDevConf confLevel mean s n = (errMargin, sqrt n)
  where errMargin = (getCritZ confLevel) * s/(sqrt n)

getMean xs = (fromIntegral $ sum xs) / (fromIntegral $ length xs)
getS xs = map  xs
getSD s n = s/(sqrt n)

getSE as an bs bn = sqrt $ as^2/an + bs^2/bn
getPropSE p n = sqrt $ p*(1-p)/n
getPairPropSE ap an bp bn = sqrt $ ap*(1-ap)/an + bp*(1-bp)/bn
getCritZ z
  | z == 0.90 = 1.645
  | z == 0.95 = 1.96
  | z == 0.99 = 2.575
  
getPropConfInterval confLevel ap bp se = (mean - errMargin, mean + errMargin)
  where mean = ap - bp 
        errMargin = (getCritZ confLevel)*se
        
getConfInterval confLevel mean s n = (mean - errMargin, mean + errMargin)
  where errMargin = (getCritZ confLevel) * (getSD s n)
  
getT y mean se = (y - mean)/se
        
getDF as an bs bn = truncate $ numer/denom
  where arat = (as^2/an)
        brat = (bs^2/bn)
        numer = (^2) $ arat + brat 
        denom = arat^2/(an - 1) + brat^2/(bn - 1) 

main = do
  let ay = 2.48
      as = 1.68
      an = 104
      by = 4.43
      bs = 1.59
      bn = 99
  let t = getT ay by (getSE as an bs bn)
  let df = getDF as an bs bn
  printLn $ t
  printLn $ df
