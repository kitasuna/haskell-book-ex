twoo :: IO ()
twoo = do c  <- getChar
          c' <- getChar
          if c == c'
            then putStrLn "True"
            else return ()
