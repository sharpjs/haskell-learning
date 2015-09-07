

main = do 
    code <- getContents
    args <- getArgs
    I.run $ if null args
        then I.hGetContents stdin
        else I.concat . map I.readFile $ args

    

