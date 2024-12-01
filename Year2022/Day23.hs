module Day23 (solution23) where

solution23 :: IO ()
solution23 = do
    mylines <- lines <$> readFile "input23"
    print $ mylines

