len = fromIntegral . length
rnd n x = (fromIntegral (floor (x * t))) / t
  where t = 10^n
put = putStrLn . show

mean xs = sum xs / (len xs)

stddev xs = sqrt $ var xs

var xs = (/(n-1)) $ sum [(x - m)^2 | x <- xs]
  where m = mean xs
        n = len xs
        
covar xs ys = (/(n - 1)) . sum $ zipWith (*) [x - xm | x <- xs] [y - ym | y <- ys]
  where xm = mean xs
        ym = mean ys
        n = len xs
        
correl xs ys = (/ (sdx * sdy)) $ covar xs ys 
  where sdx = stddev xs
        sdy = stddev ys

main = do
  let xs = [310, 285, 205, 170, 125, 140, 115, 127]
      ys = [10, 13, 20, 18, 31, 23, 29, 29]

  -- -0.936
  put $ correl xs ys
