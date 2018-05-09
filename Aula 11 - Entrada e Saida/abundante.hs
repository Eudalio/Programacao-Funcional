
divisoresProprios n = filter (\x-> mod n x == 0) [1..(n-1)]

abundante n = n < (sum (divisoresProprios n) )

main = do	  
	line <- getLine	
	let n       = (read::String->Int) line 
	print $ abundante n

