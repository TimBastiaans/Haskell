reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (h:t) = if (h >= t) then reverseList t ++ [h] else reverseList t ++ [h]