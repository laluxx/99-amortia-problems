{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Web.Scotty
import Network.HTTP.Types.Status (status409)
import Data.Aeson (FromJSON, ToJSON(..), object, (.=), pairs)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.IO (hPutStr, hClose, writeFile)
import System.FilePath ((</>), takeBaseName)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (setFileMode, ownerExecuteMode, ownerReadMode, ownerWriteMode, unionFileModes)
import System.Directory (createDirectoryIfMissing, removePathForcibly, listDirectory, getCurrentDirectory, setCurrentDirectory)
import Control.Monad (forM_)
import Control.Exception (bracket)

-- Data Types
data Solution = Solution
  { solutionProblemId :: T.Text
  , solutionCode :: T.Text
  , solutionUsername :: Maybe T.Text
  , solutionSubmittedAt :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON Solution

instance ToJSON Solution where
  toJSON (Solution pid cod uname subAt) = object
    [ "problemId" .= pid
    , "code" .= cod
    , "username" .= uname
    , "submittedAt" .= subAt
    ]
  toEncoding (Solution pid cod uname subAt) = pairs
    ( "problemId" .= pid
    <> "code" .= cod
    <> "username" .= uname
    <> "submittedAt" .= subAt
    )

instance FromRow Solution where
  fromRow = Solution <$> field <*> field <*> field <*> field

data TestRequest = TestRequest
  { problemId :: T.Text
  , code :: T.Text
  } deriving (Show, Generic)

instance FromJSON TestRequest
instance ToJSON TestRequest

data SubmitRequest = SubmitRequest
  { problemId :: T.Text
  , code :: T.Text
  , username :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON SubmitRequest
instance ToJSON SubmitRequest

data TestResult = TestResult
  { testName :: T.Text
  , testPassed :: Bool
  , testError :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON TestResult
instance ToJSON TestResult

data TestResponse = TestResponse
  { results :: [TestResult]
  , allPassed :: Bool
  } deriving (Show, Generic)

instance FromJSON TestResponse
instance ToJSON TestResponse

-- Counter for unique test names
{-# NOINLINE testCounter #-}
testCounter :: IORef Int
testCounter = unsafePerformIO (newIORef 0)

getNextTestId :: IO Int
getNextTestId = atomicModifyIORef' testCounter (\n -> (n + 1, n))

-- Clean up test directory for a problem
cleanTestDirectory :: T.Text -> IO ()
cleanTestDirectory problemId = do
  let testDir = "tests" </> T.unpack problemId
  removePathForcibly testDir
  createDirectoryIfMissing True testDir

-- Database Functions
initDB :: IO Connection
initDB = do
  conn <- open "problems.db"
  
  execute_ conn "CREATE TABLE IF NOT EXISTS solutions \
                 \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
                 \ problem_id TEXT NOT NULL, \
                 \ code TEXT NOT NULL, \
                 \ username TEXT, \
                 \ submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, \
                 \ UNIQUE(problem_id, username))"
  
  return conn

-- Check if user has already submitted a solution
hasUserSubmitted :: Connection -> T.Text -> T.Text -> IO Bool
hasUserSubmitted conn problemId uname = do
  results <- query conn
    "SELECT COUNT(*) FROM solutions WHERE problem_id = ? AND username = ?"
    (problemId, uname) :: IO [Only Int]
  case results of
    [Only count] -> return (count > 0)
    _ -> return False

-- Compiler Integration
runAmortiaCode :: T.Text -> T.Text -> T.Text -> IO (Either T.Text T.Text)
runAmortiaCode problemId testName code = do
  let testDir = "tests" </> T.unpack problemId
      testNameStr = T.unpack testName
      amorFile = testNameStr ++ ".amor"
      execFile = "./" ++ testNameStr
  
  -- Ensure test directory exists
  createDirectoryIfMissing True testDir
  
  -- Get current directory and change to test directory
  currentDir <- getCurrentDirectory
  
  bracket
    (setCurrentDirectory testDir)
    (\_ -> setCurrentDirectory currentDir)
    (\_ -> do
      -- Write Amortia source in test directory
      Prelude.writeFile amorFile (T.unpack code)
      
      -- Compile with Amortia using relative path to compiler
      let compilerPath = currentDir </> "amortia"
      putStrLn $ "Compiling: " ++ compilerPath ++ " " ++ amorFile
      (exitCode1, stdout1, stderr1) <- readProcessWithExitCode compilerPath [amorFile] ""
  
      putStrLn $ "Amortia compile exit code: " ++ show exitCode1
      putStrLn $ "Amortia stdout: " ++ stdout1
      putStrLn $ "Amortia stderr: " ++ stderr1
      
      -- Check for compilation errors (both parse errors and Erlang compilation errors)
      let combinedOutput = stdout1 ++ stderr1
      
      if "Parse error:" `T.isInfixOf` T.pack combinedOutput
        then return $ Left $ T.pack $ stdout1 ++ stderr1
        else if "Erlang compilation failed:" `T.isInfixOf` T.pack stdout1
          then do
            -- Extract the Erlang error message
            let erlangError = extractErlangError (T.pack stdout1)
            return $ Left erlangError
          else case exitCode1 of
            ExitFailure _ -> return $ Left $ T.pack $ stdout1 ++ "\n" ++ stderr1
            ExitSuccess -> do
              -- Run the generated executable (now in current test directory)
              putStrLn $ "Running: " ++ execFile
              (exitCode2, stdout2, stderr2) <- readProcessWithExitCode
                "timeout"
                ["5s", execFile]
                ""
              
              putStrLn $ "Run exit code: " ++ show exitCode2
              putStrLn $ "Run stdout: " ++ stdout2
              putStrLn $ "Run stderr: " ++ stderr2
              
              case exitCode2 of
                ExitSuccess -> return $ Right $ T.pack stdout2
                ExitFailure _ -> return $ Left $ T.pack $ "Runtime error:\n" ++ stdout2 ++ "\n" ++ stderr2
    )

-- Extract the relevant Erlang error message
extractErlangError :: T.Text -> T.Text
extractErlangError output =
  let errorLines = T.lines output
      -- Find lines between "Erlang compilation failed:" and the success message
      relevantLines = takeWhile (not . T.isPrefixOf "✓ Successfully compiled") $
                     dropWhile (not . T.isPrefixOf "Erlang compilation failed:") errorLines
      -- Format the error nicely
      errorMsg = T.unlines $ drop 1 relevantLines  -- Skip the "Erlang compilation failed:" line
  in if T.null errorMsg
     then "Compilation error (Erlang)"
     else "Compilation error:\n" <> T.strip errorMsg

-- Test Cases for Problems
getTestCases :: T.Text -> [(T.Text, T.Text, T.Text)]
getTestCases "p01" = 
  [ ("Test 1: last [1,2,3,4]", "display last [1,2,3,4]", "4")
  , ("Test 2: last [42]", "display last [42]", "42")
  , ("Test 3: last [\"a\",\"b\",\"c\"]", "display last [\"a\",\"b\",\"c\"]", "c")
  ]
getTestCases "p02" = 
  [ ("Test 1: penultimate [1,1,2,3,5,8]", "display penultimate [1,1,2,3,5,8]", "5")
  , ("Test 2: penultimate [\"x\",\"y\"]", "display penultimate [\"x\",\"y\"]", "x")
  , ("Test 3: penultimate [1,2]", "display penultimate [1,2]", "1")
  ]
getTestCases "p03" = 
  [ ("Test 1: kth [0,1,2,3] 2", "display kth [0,1,2,3] 2", "2")
  , ("Test 2: kth [\"a\",\"b\",\"c\"] 0", "display kth [\"a\",\"b\",\"c\"] 0", "a")
  ]
getTestCases "p04" = 
  [ ("Test 1: len [1,2,3]", "display len [1,2,3]", "3")
  , ("Test 2: len []", "display len []", "0")
  , ("Test 3: len [1]", "display len [1]", "1")
  ]
getTestCases "p05" = 
  [ ("Test 1: reverse [1,2,3,4]", "display reverse [1,2,3,4]", "[4,3,2,1]")
  , ("Test 2: reverse []", "display reverse []", "[]")
  ]
getTestCases "p06" = 
  [ ("Test 1: isPalindrome [1,2,3,2,1]", "display isPalindrome [1,2,3,2,1]", "true")
  , ("Test 2: isPalindrome [1,2,3]", "display isPalindrome [1,2,3]", "false")
  ]
getTestCases _ = []

-- Run Tests
runTests :: TestRequest -> IO TestResponse
runTests (TestRequest pid cod) = do
  -- Clean up old test files for this problem
  cleanTestDirectory pid
  
  let testCases = getTestCases pid
  results <- mapM (runSingleTest pid cod) (zip [1..] testCases)
  return $ TestResponse results (all testPassed results)

runSingleTest :: T.Text -> T.Text -> (Int, (T.Text, T.Text, T.Text)) -> IO TestResult
runSingleTest problemId userCode (testNum, (name, testCall, expected)) = do
  let testName = T.pack $ T.unpack problemId ++ "_test_" ++ show testNum
  
  -- Wrap user code with a main function that calls the test
  let fullCode = userCode <> "\n\ndefn main :: () {\n    " <> testCall <> "\n    halt 0\n}"
  result <- runAmortiaCode problemId testName fullCode
  
  return $ case result of
    Right output -> 
      let cleanOutput = T.strip output
          cleanExpected = T.strip expected
      in if cleanOutput == cleanExpected
        then TestResult name True Nothing
        else TestResult name False (Just $ "Expected: " <> cleanExpected <> ", Got: " <> cleanOutput)
    Left err -> TestResult name False (Just err)

-- API Routes
main :: IO ()
main = do
  conn <- initDB
  
  putStrLn "Starting Amortia Problems server on http://localhost:3000"
  
  scotty 3000 $ do
    -- Middleware
    middleware logStdoutDev
    middleware $ cors (const $ Just corsPolicy)
    
    -- Serve static files
    get "/" $ file "problems.html"
    get "/style.css" $ file "style.css"
    get "/script.js" $ file "script.js"
    
    -- Test endpoint
    post "/api/test" $ do
      req <- jsonData :: ActionM TestRequest
      testResp <- liftIO $ runTests req
      json testResp
    
    -- Check if user has submitted
    get "/api/check-submission/:problemId/:username" $ do
      problemId <- pathParam "problemId"
      username <- pathParam "username"
      hasSubmitted <- liftIO $ hasUserSubmitted conn problemId username
      json $ object ["hasSubmitted" .= hasSubmitted]
    
    -- Submit solution
    post "/api/submit" $ do
      req <- jsonData :: ActionM SubmitRequest
      let SubmitRequest pid cod uname = req
      let username = case uname of
                      Just u | not (T.null u) -> u
                      _ -> "Anonymous"
      
      -- Check if already submitted
      alreadySubmitted <- liftIO $ hasUserSubmitted conn pid username
      
      if alreadySubmitted
        then do
          status status409
          json $ object 
            [ "success" .= False
            , "error" .= ("You have already submitted a solution for this problem" :: T.Text)
            ]
        else do
          result <- liftIO $ E.try $ execute conn 
            "INSERT INTO solutions (problem_id, code, username) VALUES (?, ?, ?)"
            (pid, cod, username) :: ActionM (Either SomeException ())
          case result of
            Right _ -> json $ object ["success" .= True]
            Left _ -> do
              status status409
              json $ object 
                [ "success" .= False
                , "error" .= ("Error submitting solution" :: T.Text)
                ]
    
    -- Get solutions for a problem
    get "/api/solutions/:problemId" $ do
      problemId <- pathParam "problemId"
      solutions <- liftIO $ query conn
        "SELECT problem_id, code, username, submitted_at \
        \FROM solutions WHERE problem_id = ? \
        \ORDER BY submitted_at DESC LIMIT 20"
        (Only (problemId :: T.Text)) :: ActionM [Solution]
      json solutions
    
    -- Health check
    get "/health" $ text "OK"

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }
