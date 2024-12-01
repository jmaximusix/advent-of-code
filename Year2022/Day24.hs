module Day24 (solution24) where

solution24 :: IO ()
solution24 = do
    mylines <- lines <$> readFile "input24"
    print $ mylines

