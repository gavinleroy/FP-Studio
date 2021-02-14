module Move where

newtype Move a = M ([Move a] -> a -> [a])

app :: Move a -> ([Move a] -> a -> [a])
app (M f) = f


