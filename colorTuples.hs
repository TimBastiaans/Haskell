colorTuples :: [a] -> [(a, a)]
colorTuples [] = []
colorTuples (x:xs) = map ((,)x) xs ++ colorTuples xs 
