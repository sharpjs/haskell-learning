module Main where

import Types

main :: IO ()
main = do
    let n = valueWidth $ IntT 2 3 True
    putStrLn $ "hello world " ++ show n

