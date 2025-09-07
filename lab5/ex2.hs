dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

numberGuesser :: IO ()
numberGuesser = putStr "Pick a number: "
        >> getLine
        >>= \number -> let num = read number :: Int in
            if num == 25
                then putStrLn "You win!!!"
            else if num < 25
                then putStrLn "Your number is too small" >> numberGuesser
                else putStrLn "Your number is too large" >> numberGuesser

twoQuestions :: IO ()
twoQuestions = do
    putStrLn "What is you Name?"
    name <- getLine
    putStrLn "What is you Name?"
    age <- getLine
    print (name, age)

twoQuestionsWithoutDo :: IO ()
twoQuestionsWithoutDo = putStrLn "What is your name?"
    >> getLine >>= \name -> putStrLn "What is your age" >> getLine >>= \age -> putStrLn $ name ++ " " ++ age

main = do
    -- putChar 'a' >> putChar '\n'

    -- let echo1 = getLine >>= \s -> putStrLn $ s ++ ", hello"
    -- echo1

    -- let echo2 = getLine >>= \str1 -> getLine >>= \str2 -> putStrLn $ str1 ++ " " ++ str2

    -- echo2

    -- putStrLn "a"

    -- dialog
    -- numberGuesser

    twoQuestionsWithoutDo
