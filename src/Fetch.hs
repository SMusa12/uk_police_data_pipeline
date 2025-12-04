module Fetch where

fetchData :: IO String
fetchData = do
    putStrLn "Fetching data from UK Police API..."
    pure "{}"
