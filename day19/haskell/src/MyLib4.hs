
{-
rather than have complex quasi quote libraries and deal with interpretation
barrier between what haskell string is in memory and
when haskell prints a string to screen
simply use a list of list of ints , represent the ascii codes of the characters
the newlines
line feed \n  has ascii value 10
carriage return \r has ascii value 13


-}



module MyLib4  where



-- example = [r|""
-- "abc"
-- "aaa\"aaa"
-- "\x27"|]

-- "" encodes to "\"\"", an increase from 2 characters to 6.
-- "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
-- "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
-- "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.
-- splitExample = split example  


someFunc :: IO ()
someFunc = do putStrLn ("From MyLib4 ... " ++ " ... " ++ " ... "  )
              -- print splitExample

              


