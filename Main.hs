{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Web.Scotty
import Network.HTTP.Types.Status (status409, status403, status401)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), pairs, eitherDecodeFileStrict, withObject, (.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Database.SQLite.Simple
import qualified Control.Exception as E
import Control.Exception (SomeException, bracket)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (createDirectoryIfMissing, removePathForcibly, getCurrentDirectory, setCurrentDirectory, listDirectory)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, UTCTime, addUTCTime)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import System.Random (randomRIO)
import Data.List (isSuffixOf, sortOn)

-- Session Management
type SessionToken = T.Text
type Username = T.Text

data SessionData = SessionData
  { sessionUsername :: Username
  , sessionExpiry :: UTCTime
  , sessionPassedTests :: Map.Map T.Text Bool  -- problemId -> passed
  } deriving (Show)

{-# NOINLINE sessionStore #-}
sessionStore :: IORef (Map.Map SessionToken SessionData)
sessionStore = unsafePerformIO (newIORef Map.empty)

-- Generate secure session token
generateSessionToken :: IO SessionToken
generateSessionToken = do
  randomBytes <- sequence [randomRIO (0, 255) :: IO Int | _ <- [(1::Int)..32]]
  let bytes = TE.encodeUtf8 $ T.pack $ show randomBytes
  return $ TE.decodeUtf8 $ B64.encode $ SHA256.hash bytes

-- Create or get session
createSession :: Username -> IO SessionToken
createSession username = do
  token <- generateSessionToken
  now <- getCurrentTime
  let expiry = addUTCTime (3600 * 24) now  -- 24 hour expiry
  let session = SessionData username expiry Map.empty
  atomicModifyIORef' sessionStore $ \store ->
    (Map.insert token session store, ())
  return token

-- Get session
getSession :: SessionToken -> IO (Maybe SessionData)
getSession token = do
  now <- getCurrentTime
  store <- readIORef sessionStore
  case Map.lookup token store of
    Nothing -> return Nothing
    Just session -> 
      if sessionExpiry session > now
        then return (Just session)
        else do
          -- Clean expired session
          atomicModifyIORef' sessionStore $ \s ->
            (Map.delete token s, ())
          return Nothing

-- Mark test as passed for session
markTestPassed :: SessionToken -> T.Text -> IO ()
markTestPassed token problemId = do
  atomicModifyIORef' sessionStore $ \store ->
    case Map.lookup token store of
      Nothing -> (store, ())
      Just session ->
        let updatedTests = Map.insert problemId True (sessionPassedTests session)
            updatedSession = session { sessionPassedTests = updatedTests }
        in (Map.insert token updatedSession store, ())

data ProblemExample = ProblemExample
  { exampleCode :: T.Text
  , exampleResult :: T.Text
  } deriving (Show, Generic)

instance FromJSON ProblemExample where
  parseJSON = withObject "ProblemExample" $ \v -> ProblemExample
    <$> v .: "code"
    <*> v .: "result"

instance ToJSON ProblemExample where
  toJSON (ProblemExample code result) = object
    [ "code" .= code
    , "result" .= result
    ]

data TestCase = TestCase
  { testCaseName :: T.Text
  , testCaseCall :: T.Text
  , testCaseExpected :: T.Text
  } deriving (Show, Generic)

instance FromJSON TestCase where
instance ToJSON TestCase where

data Problem = Problem
  { problemId :: T.Text
  , problemNumber :: T.Text
  , problemDifficulty :: T.Text
  , problemTitle :: T.Text
  , problemDescription :: T.Text
  , problemExample :: ProblemExample
  , problemFunctionSignature :: T.Text
  , problemTestCases :: [TestCase]
  , problemSection :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON Problem where
instance ToJSON Problem where

-- Global problem store
{-# NOINLINE problemStore #-}
problemStore :: IORef [Problem]
problemStore = unsafePerformIO (newIORef [])

-- Load all problems from JSON files
loadProblems :: IO [Problem]
loadProblems = do
  createDirectoryIfMissing True "problems"
  files <- listDirectory "problems"
  let jsonFiles = filter (isSuffixOf ".json") files
  problems <- mapM loadProblem jsonFiles
  let validProblems = [p | Right p <- problems]
  -- Sort by problem number
  let sorted = sortOn problemNumber validProblems
  atomicModifyIORef' problemStore $ \_ -> (sorted, ())
  putStrLn $ "Loaded " ++ show (length sorted) ++ " problems"
  return sorted

loadProblem :: FilePath -> IO (Either String Problem)
loadProblem filename = do
  result <- eitherDecodeFileStrict ("problems" </> filename)
  case result of
    Left err -> do
      putStrLn $ "Error loading " ++ filename ++ ": " ++ err
      return $ Left err
    Right problem -> return $ Right problem

getProblems :: IO [Problem]
getProblems = readIORef problemStore

getProblemById :: T.Text -> IO (Maybe Problem)
getProblemById pid = do
  problems <- getProblems
  return $ case filter (\(Problem { problemId = pId }) -> pId == pid) problems of
    (p:_) -> Just p
    [] -> Nothing

-- Data Types
data Solution = Solution
  { solutionProblemId :: T.Text
  , solutionCode :: T.Text
  , solutionUsername :: Maybe T.Text
  , solutionSubmittedAt :: Maybe T.Text
  , solutionVerified :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON Solution

instance ToJSON Solution where
  toJSON (Solution pid cod uname subAt verified) = object
    [ "problemId" .= pid
    , "code" .= cod
    , "username" .= uname
    , "submittedAt" .= subAt
    , "verified" .= verified
    ]
  toEncoding (Solution pid cod uname subAt verified) = pairs
    ( "problemId" .= pid
    <> "code" .= cod
    <> "username" .= uname
    <> "submittedAt" .= subAt
    <> "verified" .= verified
    )

instance FromRow Solution where
  fromRow = Solution <$> field <*> field <*> field <*> field <*> field

data TestRequest = TestRequest
  { problemId :: T.Text
  , code :: T.Text
  , sessionToken :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON TestRequest
instance ToJSON TestRequest

data SubmitRequest = SubmitRequest
  { problemId :: T.Text
  , code :: T.Text
  , username :: Maybe T.Text
  , sessionToken :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON SubmitRequest
instance ToJSON SubmitRequest

newtype LoginRequest = LoginRequest
  { username :: T.Text
  } deriving (Show, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

data RegisterRequest = RegisterRequest
  { username :: T.Text
  , password :: Maybe T.Text  -- Optional password
  } deriving (Show, Generic)

instance FromJSON RegisterRequest
instance ToJSON RegisterRequest

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

newtype VerifySessionRequest = VerifySessionRequest
  { verifyToken :: T.Text
  } deriving (Show, Generic)

instance FromJSON VerifySessionRequest
instance ToJSON VerifySessionRequest

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
  
  -- Solutions table with verified flag
  execute_ conn "CREATE TABLE IF NOT EXISTS solutions \
                 \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
                 \ problem_id TEXT NOT NULL, \
                 \ code TEXT NOT NULL, \
                 \ username TEXT NOT NULL, \
                 \ submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, \
                 \ verified BOOLEAN DEFAULT 0, \
                 \ UNIQUE(problem_id, username))"
  
  -- Reserved usernames table with optional password hash
  execute_ conn "CREATE TABLE IF NOT EXISTS reserved_usernames \
                 \(username TEXT PRIMARY KEY, \
                 \ password_hash TEXT, \
                 \ created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"
  
  return conn

-- Hash password using SHA256
hashPassword :: T.Text -> T.Text
hashPassword password = 
  TE.decodeUtf8 $ B64.encode $ SHA256.hash $ TE.encodeUtf8 password

-- Reserve a username with optional password
reserveUsernameWithPassword :: Connection -> T.Text -> Maybe T.Text -> IO Bool
reserveUsernameWithPassword conn username mPassword = do
  let hashedPassword = fmap hashPassword mPassword
  result <- E.try $ execute conn
    "INSERT INTO reserved_usernames (username, password_hash) VALUES (?, ?)"
    (username, hashedPassword) :: IO (Either SomeException ())
  case result of
    Right _ -> return True
    Left _ -> return False

-- Verify password for username
verifyPassword :: Connection -> T.Text -> T.Text -> IO Bool
verifyPassword conn username password = do
  results <- query conn
    "SELECT password_hash FROM reserved_usernames WHERE username = ?"
    (Only username) :: IO [Only (Maybe T.Text)]
  case results of
    [Only (Just hash)] -> return $ hash == hashPassword password
    [Only Nothing] -> return False  -- Username exists but no password set
    _ -> return False

-- Check if username has password protection
hasPassword :: Connection -> T.Text -> IO Bool
hasPassword conn username = do
  results <- query conn
    "SELECT password_hash FROM reserved_usernames WHERE username = ?"
    (Only username) :: IO [Only (Maybe T.Text)]
  case results of
    [Only (Just _)] -> return True
    _ -> return False

-- Check if username is reserved
isUsernameReserved :: Connection -> T.Text -> IO Bool
isUsernameReserved conn username = do
  results <- query conn
    "SELECT COUNT(*) FROM reserved_usernames WHERE username = ?"
    (Only username) :: IO [Only Int]
  case results of
    [Only count] -> return (count > 0)
    _ -> return False

-- Check if user has already submitted a solution
hasUserSubmitted :: Connection -> T.Text -> T.Text -> IO Bool
hasUserSubmitted conn problemId uname = do
  results <- query conn
    "SELECT COUNT(*) FROM solutions WHERE problem_id = ? AND username = ?"
    (problemId, uname) :: IO [Only Int]
  case results of
    [Only count] -> return (count > 0)
    _ -> return False

-- NEW: Get all submitted solutions for a user (for imports)
getUserSolutions :: Connection -> T.Text -> IO [Solution]
getUserSolutions conn username = do
  query conn
    "SELECT problem_id, code, username, submitted_at, verified \
    \FROM solutions WHERE username = ? \
    \ORDER BY submitted_at ASC"
    (Only username)

-- NEW: Extract function definitions from code
extractFunctions :: T.Text -> T.Text
extractFunctions code = 
  T.unlines $ filter isFunction $ T.lines code
  where
    isFunction line = "defn " `T.isPrefixOf` T.strip line || isContinuation line
    isContinuation line = 
      let stripped = T.strip line
      in not (T.null stripped) && 
         not ("defn " `T.isPrefixOf` stripped) &&
         (T.any (`elem` ['{', '}', '-', '|', ',']) stripped || 
          T.any (`elem` ['a'..'z']) stripped)

-- NEW: Build library code from previous solutions
buildLibraryCode :: Connection -> T.Text -> T.Text -> IO T.Text
buildLibraryCode conn username currentProblemId = do
  solutions <- getUserSolutions conn username
  
  -- Filter out the current problem to avoid conflicts
  let previousSolutions = filter (\s -> solutionProblemId s /= currentProblemId) solutions
  
  -- Extract all function definitions
  let functionDefs = map (extractFunctions . solutionCode) previousSolutions
  
  -- Combine all function definitions
  return $ T.intercalate "\n\n" $ filter (not . T.null) functionDefs

-- Compiler Integration
runAmortiaCode :: T.Text -> T.Text -> T.Text -> IO (Either T.Text T.Text)
runAmortiaCode problemId testName code = do
  let testDir = "tests" </> T.unpack problemId
      testNameStr = T.unpack testName
      amorFile = testNameStr ++ ".amor"
      execFile = "./" ++ testNameStr
  
  createDirectoryIfMissing True testDir
  currentDir <- getCurrentDirectory
  
  bracket
    (setCurrentDirectory testDir)
    (\_ -> setCurrentDirectory currentDir)
    (\_ -> do
      writeFile amorFile (T.unpack code)
      
      let compilerPath = currentDir </> "amortia"
      (exitCode1, stdout1, stderr1) <- readProcessWithExitCode compilerPath [amorFile] ""
      
      let combinedOutput = stdout1 ++ stderr1
      
      if "Parse error:" `T.isInfixOf` T.pack combinedOutput
        then return $ Left $ T.pack $ stdout1 ++ stderr1
        else if "Erlang compilation failed:" `T.isInfixOf` T.pack stdout1
          then do
            let erlangError = extractErlangError (T.pack stdout1)
            return $ Left erlangError
          else case exitCode1 of
            ExitFailure _ -> return $ Left $ T.pack $ stdout1 ++ "\n" ++ stderr1
            ExitSuccess -> do
              (exitCode2, stdout2, stderr2) <- readProcessWithExitCode
                "timeout"
                ["5s", execFile]
                ""
              
              case exitCode2 of
                ExitSuccess -> return $ Right $ T.pack stdout2
                ExitFailure _ -> return $ Left $ T.pack $ "Runtime error:\n" ++ stdout2 ++ "\n" ++ stderr2
    )

extractErlangError :: T.Text -> T.Text
extractErlangError output =
  let errorLines = T.lines output
      relevantLines = takeWhile (not . T.isPrefixOf "✓ Successfully compiled") $
                     dropWhile (not . T.isPrefixOf "Erlang compilation failed:") errorLines
      errorMsg = T.unlines $ drop 1 relevantLines
  in if T.null errorMsg
     then "Compilation error (Erlang)"
     else "Compilation error:\n" <> T.strip errorMsg

-- Run Tests (MODIFIED to include library code)
runTests :: Connection -> TestRequest -> T.Text -> IO (Either String TestResponse)
runTests conn (TestRequest pid cod _) username = do
  mProblem <- getProblemById pid
  case mProblem of
    Nothing -> return $ Left "Problem not found"
    Just problem -> do
      cleanTestDirectory pid
      
      -- Build library from previous solutions
      libraryCode <- buildLibraryCode conn username pid
      
      let testCases = problemTestCases problem
      results <- mapM (runSingleTest conn pid cod libraryCode username) (zip [1..] testCases)
      return $ Right $ TestResponse results (all testPassed results)

runSingleTest :: Connection -> T.Text -> T.Text -> T.Text -> T.Text -> (Int, TestCase) -> IO TestResult
runSingleTest conn problemId userCode libraryCode username (testNum, testCase) = do
  let testName = T.pack $ T.unpack problemId ++ "_test_" ++ show testNum
  
  -- Combine library code with user code and test
  let fullCode = libraryCode <> "\n\n" <> userCode <> "\n\ndefn main :: () {\n    " <> testCaseCall testCase <> "\n    halt 0\n}"
  
  result <- runAmortiaCode problemId testName fullCode
  
  return $ case result of
    Right output -> 
      let cleanOutput = T.strip output
          cleanExpected = T.strip (testCaseExpected testCase)
      in if cleanOutput == cleanExpected
        then TestResult (testCaseName testCase) True Nothing
        else TestResult (testCaseName testCase) False (Just $ "Expected: " <> cleanExpected <> ", Got: " <> cleanOutput)
    Left err -> TestResult (testCaseName testCase) False (Just err)

-- API Routes
main :: IO ()
main = do
  conn <- initDB
  
  -- Load problems from JSON
  _ <- loadProblems
  
  putStrLn "Starting Amortia Problems server on http://localhost:3000"
  
  scotty 3000 $ do
    middleware logStdoutDev
    middleware $ cors (const $ Just corsPolicy)
    
    -- Serve static files
    get "/" $ file "problems.html"
    get "/style.css" $ file "style.css"
    get "/script.js" $ file "script.js"
    get "/logo.png" $ file "logo.png"    

    -- Get all problems
    get "/api/problems" $ do
      problems <- liftIO getProblems
      json problems

    -- Login/Session endpoint
    post "/api/login" $ do
      RegisterRequest username mPassword <- jsonData
      
      -- Validate username
      let cleanUsername = T.strip username
      if T.null cleanUsername || T.length cleanUsername > 50
        then do
          status status403
          json $ object ["error" .= ("Invalid username" :: T.Text)]
        else do
          -- Check if username is already reserved
          reserved <- liftIO $ isUsernameReserved conn cleanUsername
          
          if not reserved
            then do
              -- New user registration
              success <- liftIO $ reserveUsernameWithPassword conn cleanUsername mPassword
              if success
                then do
                  token <- liftIO $ createSession cleanUsername
                  let isVerified = case mPassword of
                        Just p | not (T.null p) -> True
                        _ -> False
                  json $ object 
                    [ "token" .= token
                    , "username" .= cleanUsername
                    , "newUser" .= True
                    , "verified" .= isVerified
                    ]
                else do
                  status status409
                  json $ object ["error" .= ("Username already taken" :: T.Text)]
            else do
              -- Existing user login
              hasPass <- liftIO $ hasPassword conn cleanUsername
              
              if hasPass
                then case mPassword of
                  Nothing -> do
                    status status403
                    json $ object ["error" .= ("Password required for this username" :: T.Text)]
                  Just password -> do
                    valid <- liftIO $ verifyPassword conn cleanUsername password
                    if valid
                      then do
                        token <- liftIO $ createSession cleanUsername
                        json $ object 
                          [ "token" .= token
                          , "username" .= cleanUsername
                          , "newUser" .= False
                          , "verified" .= True
                          ]
                      else do
                        status status403
                        json $ object ["error" .= ("Incorrect password" :: T.Text)]
                else do
                  -- Username reserved but no password - allow login
                  token <- liftIO $ createSession cleanUsername
                  json $ object 
                    [ "token" .= token
                    , "username" .= cleanUsername
                    , "newUser" .= False
                    , "verified" .= False
                    ]
    
    -- Verify session endpoint
    post "/api/verify-session" $ do
      VerifySessionRequest token <- jsonData
      mSession <- liftIO $ getSession token
      case mSession of
        Nothing -> do
          status status401
          json $ object ["valid" .= False]
        Just _ -> json $ object ["valid" .= True]
    
    -- Test endpoint with session validation (MODIFIED)
    post "/api/test" $ do
      req <- jsonData :: ActionM TestRequest
      let TestRequest pid cod mToken = req
      
      case mToken of
        Nothing -> do
          status status401
          json $ object ["error" .= ("Session required" :: T.Text)]
        Just token -> do
          mSession <- liftIO $ getSession token
          case mSession of
            Nothing -> do
              status status401
              json $ object ["error" .= ("Invalid or expired session" :: T.Text)]
            Just session -> do
              -- Pass username to runTests for library code
              testResult <- liftIO $ runTests conn req (sessionUsername session)
              
              case testResult of
                Left err -> do
                  status status403
                  json $ object ["error" .= err]
                Right testResp -> do
                  -- If all tests passed, mark in session
                  when (allPassed testResp) $ do
                    liftIO $ markTestPassed token pid
                  
                  json testResp
    
    -- Check if user has submitted
    get "/api/check-submission/:problemId/:username" $ do
      problemId <- captureParam "problemId"
      username <- captureParam "username"
      hasSubmitted <- liftIO $ hasUserSubmitted conn problemId username
      json $ object ["hasSubmitted" .= hasSubmitted]
    
    -- Submit solution with server-side validation (MODIFIED)
    post "/api/submit" $ do
      req <- jsonData :: ActionM SubmitRequest
      let SubmitRequest pid code _ mToken = req
      
      case mToken of
        Nothing -> do
          status status401
          json $ object 
            [ "success" .= False
            , "error" .= ("Session required" :: T.Text)
            ]
        Just token -> do
          mSession <- liftIO $ getSession token
          case mSession of
            Nothing -> do
              status status401
              json $ object 
                [ "success" .= False
                , "error" .= ("Invalid or expired session" :: T.Text)
                ]
            Just session -> do
              let username = sessionUsername session
              
              -- SERVER-SIDE VALIDATION: Re-run tests to verify (with library code)
              testResult <- liftIO $ runTests conn (TestRequest pid code Nothing) username
              
              case testResult of
                Left err -> do
                  status status403
                  json $ object 
                    [ "success" .= False
                    , "error" .= ("Error running tests: " <> T.pack err)
                    ]
                Right testResp -> do
                  if not (allPassed testResp)
                    then do
                      status status403
                      json $ object 
                        [ "success" .= False
                        , "error" .= ("Tests must pass before submission" :: T.Text)
                        ]
                    else do
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
                          -- Get verified status from session
                          hasPass <- liftIO $ hasPassword conn username
                          
                          result <- liftIO $ E.try $ execute conn 
                            "INSERT INTO solutions (problem_id, code, username, verified) VALUES (?, ?, ?, ?)"
                            (pid, code, username, hasPass) :: ActionM (Either SomeException ())
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
      problemId <- captureParam "problemId"
      solutions <- liftIO $ query conn
        "SELECT problem_id, code, username, submitted_at, verified \
        \FROM solutions WHERE problem_id = ? \
        \ORDER BY verified DESC, submitted_at DESC LIMIT 20"
        (Only (problemId :: T.Text)) :: ActionM [Solution]
      json solutions
    
    -- Health check
    get "/health" $ text "OK"

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }
