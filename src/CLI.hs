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
import Control.Monad (forM_)
import qualified Data.Text as T
import Database.SQLite.Simple (close)

import qualified Fetch
import qualified Parse
import qualified Database
import Types (Crime(..), Force(..), CrimeCategory(..))

-- Run the CLI application
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
        ["force-stats"]                  -> cmdForceStats
        _             -> printUsage >> exitFailure

-- Print usage information
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
    , "  force-stats                     Show crime statistics by category"
    , ""
    , "Examples:"
    , "  stack run -- create"
    , "  stack run -- loaddata"
    , "  stack run -- crimes-by-category violent-crime"
    ]

-- Create database and tables
cmdCreate :: IO ()
cmdCreate = do
    putStrLn "Creating database and tables..."
    createDirectoryIfMissing True "data"
    conn <- Database.initDatabase "data/police.db"
    close conn
    putStrLn "Database created successfully at data/police.db"

-- Download data from API and save to database
cmdLoadData :: IO ()
cmdLoadData = do
    putStrLn "Loading data from UK Police API..."
    conn <- Database.initDatabase "data/police.db"

    putStrLn "Fetching police forces..."
    forcesResult <- Fetch.fetchForces
    case forcesResult of
        Left err -> putStrLn $ "Error fetching forces: " ++ show err
        Right forcesJson -> case Parse.parseForces forcesJson of
            Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
            Right forces -> do
                forM_ forces $ \force -> Database.insertForce conn force
                putStrLn $ "Saved " ++ show (length forces) ++ " police forces"

    putStrLn "Fetching crime categories..."
    categoriesResult <- Fetch.fetchCrimeCategories
    case categoriesResult of
        Left err -> putStrLn $ "Error fetching categories: " ++ show err
        Right categoriesJson -> case Parse.parseCrimeCategories categoriesJson of
            Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
            Right categories -> do
                forM_ categories $ \cat -> Database.insertCrimeCategory conn cat
                putStrLn $ "Saved " ++ show (length categories) ++ " crime categories"

    putStrLn "Fetching crimes from London..."
    crimesResult <- Fetch.fetchCrimesByLocation 51.5074 (-0.1278) "2024-01"
    case crimesResult of
        Left err -> putStrLn $ "Error: " ++ show err
        Right crimesJson -> case Parse.parseCrimes crimesJson of
            Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
            Right crimes -> do
                forM_ crimes $ \crime -> Database.insertCrime conn crime
                putStrLn $ "Saved " ++ show (length crimes) ++ " crimes from London"

    putStrLn "Fetching crimes from Manchester..."
    crimes2Result <- Fetch.fetchCrimesByLocation 53.4808 (-2.2426) "2024-01"
    case crimes2Result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right crimesJson -> case Parse.parseCrimes crimesJson of
            Left parseErr -> putStrLn $ "Parse error: " ++ parseErr
            Right crimes -> do
                forM_ crimes $ \crime -> Database.insertCrime conn crime
                putStrLn $ "Saved " ++ show (length crimes) ++ " crimes from Manchester"

    close conn
    putStrLn "Data loaded successfully"

-- Export all data to JSON file
cmdDumpData :: IO ()
cmdDumpData = do
    putStrLn "Exporting data to JSON..."
    conn <- Database.initDatabase "data/police.db"

    crimes <- Database.getAllCrimes conn
    forces <- Database.getAllForces conn
    categories <- Database.getAllCrimeCategories conn

    Parse.writeAllData "data/data.json" crimes forces categories

    close conn
    putStrLn $ "Exported " ++ show (length crimes) ++ " crimes to data/data.json"

-- Query crimes by category
cmdCrimesByCategory :: String -> IO ()
cmdCrimesByCategory category = do
    putStrLn $ "Querying crimes with category: " ++ category
    conn <- Database.initDatabase "data/police.db"

    results <- Database.queryCrimesByCategory conn (T.pack category)

    putStrLn $ "Found " ++ show (length results) ++ " crimes:"
    forM_ (take 10 results) $ \crime -> do
        putStrLn $ "  - " ++ T.unpack (crimeCategory crime) ++
                  " at (" ++ show (crimeLatitude crime) ++ ", " ++
                  show (crimeLongitude crime) ++ ")"

    when (length results > 10) $
        putStrLn $ "  ... and " ++ show (length results - 10) ++ " more"

    close conn
    putStrLn "Query complete"
    where
        when cond action = if cond then action else return ()

-- Query crimes by force
cmdCrimesByForce :: String -> IO ()
cmdCrimesByForce forceId = do
    putStrLn $ "Querying crimes for force: " ++ forceId
    putStrLn "Note: Force filtering not yet implemented in database schema"
    putStrLn "Query complete"

-- Query crimes by month
cmdCrimesInMonth :: String -> IO ()
cmdCrimesInMonth month = do
    putStrLn $ "Querying crimes in month: " ++ month
    conn <- Database.initDatabase "data/police.db"

    results <- Database.queryCrimesByMonth conn (T.pack month)

    putStrLn $ "Found " ++ show (length results) ++ " crimes in " ++ month
    forM_ (take 10 results) $ \crime -> do
        putStrLn $ "  - " ++ T.unpack (crimeCategory crime) ++
                  " in " ++ T.unpack (crimeMonth crime)

    when (length results > 10) $
        putStrLn $ "  ... and " ++ show (length results - 10) ++ " more"

    close conn
    putStrLn "Query complete"
    where
        when cond action = if cond then action else return ()

-- Show crime statistics
cmdForceStats :: IO ()
cmdForceStats = do
    putStrLn "Generating crime statistics..."
    conn <- Database.initDatabase "data/police.db"

    stats <- Database.getCrimeStats conn

    putStrLn "Crime Statistics by Category:"
    forM_ stats $ \(category, count) ->
        putStrLn $ T.unpack category ++ ": " ++ show count
    putStrLn $ "Total crimes: " ++ show (sum $ map snd stats)

    close conn
    putStrLn "Statistics generated"
