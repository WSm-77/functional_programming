import Data.Either (isLeft, isRight)
import Data.Char

main = do
    print $ fmap (+1) (*10) 1
    print $ fmap (+1) (0,0,0)

    print $ (+2) $ 3

    print $ (+2) <$> Just 3     -- Just 5
    print $ fmap (+2) (Just 3)  -- The same as previous line

    -- toUpper <$> getChar >>= putChar

    -- getLine >>= \line ->  putStrLn . map toUpper $ line

    -- print $ fmap (+1) Nothing

    let x = Just 1
    print $ fmap (+1) x
    print $ 5 <$ x

    print $ "Hello" <$ [1..5]

    42 <$ getLine >>= putStrLn . show

    print "End"
