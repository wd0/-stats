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
        
linreg xs ys = 
  \x -> b0 + b1 * x
  where r = correl xs ys
        b1 = r * (stddev ys)/(stddev xs)
        b0 = (mean ys) - b1*(mean xs)

main = do
  let xs = [20,31,33,35,38,38,44]
      ys = [430,580,570,550,660,690,650]
      r = correl xs ys
  put $ r^2
  put $ (linreg xs ys) 20
  let b1 = r * (stddev ys)/(stddev xs)
  let b0 = (mean ys) - b1*(mean xs)
  put $ b1
  put $ b0
