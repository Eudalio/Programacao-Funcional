neglist xs = length (filter (<0) xs)

main = do
   inputdata <- getContents
   print $ neglist $ map (read :: String->Int) (lines inputdata)

