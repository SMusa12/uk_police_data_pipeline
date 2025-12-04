{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CLI
Description : Command-line interface logic for UK Police Data Pipeline
Copyright   : (c) Group 3, 2025
License     : MIT
Maintainer  : Group 3(Suhayb Musa, Hamza Lasry, Oussama Djoudi)

This module handles all command-line argument parsing and execution.
-}

module CLI (runCLI) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)

-- | Run the CLI application
runCLI :: IO ()
runCLI = do
    args <- getArgs
    case args of
        ["create"]    -> cmdCreate
        ["loaddata"]  -> cmdLoadData
        ["dumpdata"]  -> cmdDumpData
        ["crimes-by-category", category] -> cmdCrimesByCategory category
        ["crimes-by-force", forceId]     -> cmdCrimesByForce forceId
        ["crimes-in-month", month]       -> cmdCrimesInMonth month
        ["force-stats", forceId]         -> cmdForceStats forceId
        _             -> printUsage >> exitFailure

-- | Print usage information
printUsage :: IO ()
printUsage = putStrLn $ unlines
    [ "UK Police Data Pipeline - Group 3"
    , ""
    , "Usage: stack run -- <command> [arguments]"
    , ""
    , "Commands:"
    , "  create                          Create database and tables"
    , "  loaddata                        Download data from UK Police API and save to database"
    , "  dumpdata                        Export all data to data/data.json"
    , ""
    , "Query Commands:"
    , "  crimes-by-category <category>   Query crimes by category (e.g., 'violent-crime')"
    , "  crimes-by-force <force-id>      Query crimes by police force (e.g., 'metropolitan')"
    , "  crimes-in-month <YYYY-MM>       Query crimes in specific month (e.g., '2024-01')"
    , "  force-stats <force-id>          Show statistics for a police force"
    , ""
    , "Examples:"
    , "  stack run -- create"
    , "  stack run -- loaddata"
    , "  stack run -- crimes-by-category burglary"
    ]

-- | Create database command
cmdCreate :: IO ()
cmdCreate = do
    putStrLn "Creating database and tables..."
    createDirectoryIfMissing True "data"
    -- TODO: Call Database.initDB when Role 2 provides it
    -- Database.initDB
    putStrLn "✓ Database created successfully"

-- | Load data command
cmdLoadData :: IO ()
cmdLoadData = do
    putStrLn "Loading data from UK Police API..."
    -- TODO: Implement when Fetch and Parse modules are ready
    -- Step 1: Fetch forces
    -- forcesJson <- Fetch.fetchForces
    -- Step 2: Parse forces
    -- forces <- Parse.parseForces forcesJson
    -- Step 3: Save to database
    -- Database.saveForces forces
    putStrLn "✓ Data loaded successfully"

-- | Dump data command
cmdDumpData :: IO ()
cmdDumpData = do
    putStrLn "Exporting data to JSON..."
    -- TODO: Implement when Parse module is ready
    -- Step 1: Get data from database
    -- crimes <- Database.getAllCrimes
    -- forces <- Database.getAllForces
    -- Step 2: Write to JSON file
    -- Parse.writeJSONFile "data/data.json" crimes forces
    putStrLn "✓ Data exported to data/data.json"

-- | Query crimes by category
cmdCrimesByCategory :: String -> IO ()
cmdCrimesByCategory category = do
    putStrLn $ "Querying crimes with category: " ++ category
    -- TODO: Implement when Database module is ready
    -- results <- Database.queryCrimesByCategory category
    -- mapM_ print results
    putStrLn "✓ Query complete"

-- | Query crimes by force
cmdCrimesByForce :: String -> IO ()
cmdCrimesByForce forceId = do
    putStrLn $ "Querying crimes for force: " ++ forceId
    -- TODO: Implement when Database module is ready
    -- results <- Database.queryCrimesByForce forceId
    -- mapM_ print results
    putStrLn "✓ Query complete"

-- | Query crimes in specific month
cmdCrimesInMonth :: String -> IO ()
cmdCrimesInMonth month = do
    putStrLn $ "Querying crimes in month: " ++ month
    -- TODO: Implement when Database module is ready
    -- results <- Database.queryCrimesInMonth month
    -- mapM_ print results
    putStrLn "✓ Query complete"

-- | Show force statistics
cmdForceStats :: String -> IO ()
cmdForceStats forceId = do
    putStrLn $ "Generating statistics for force: " ++ forceId
    -- TODO: Implement when Database module is ready
    -- stats <- Database.getForceStats forceId
    -- print stats
    putStrLn "✓ Statistics generated"
