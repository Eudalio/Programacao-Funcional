todosPrefixos xs = [ take i xs | i <- [0..n] ]
	where n = length xs

main = do	 
	inputdata <- getContents
	let entrada = lines inputdata	
	print $ todosPrefixos $ map (read :: String->Int) (entrada)

