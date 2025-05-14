-- File: EdTest.hs
-- Purpose: Ed testing tool.
-- Author: James Barnes

-- Complie and run test individually, using Ed's custom test method.
-- run command: ghc -v0 -O2 EdTest && ./EdTest

import Control.Arrow (Arrow (first, second))
import Control.Exception (SomeException, handle)
import Data.Char (isAscii, isControl, toLower)
import Data.Functor ((<&>))
import Data.List (intercalate, uncons)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (clamp)
import Numeric (showHex)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (FilePath, (</>))
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- The suite of tests to run
tests :: [Test]
tests =
  [ validationTest "Validation test" (expected "0"),
    test "Real test" (scored "1")
  ]
  where
    compile i = compileHaskell (dir i </> "Main.hs") (exe i)
    -- compile i = compileProlog (dir i </> "main.pl") (exe i)
    expected i = compile i $ timed testTime $ runExpected (dir i </> "expected.txt") (exe i) []
    scored i = compile i $ timed testTime $ runScored (exe i) []
    dir i = "." </> i
    exe i = dir i </> "out"

-- How long to run each test for, if timed
testTime :: Double
testTime = 5.0

-- Default score for a test
testScore :: Double
testScore = 1.0

-- Default pass threshold for a test, 50%
testThreshold :: Double
testThreshold = testScore / 2

-- Add a dummy test to ensure total score is positive and doesn't exceed max, or not
capTestScores :: Bool
capTestScores = True

-- Privacy of non-validation tests
testPrivacy :: Privacy
testPrivacy = Private

-- opinionated test constuctors
test :: String -> IO Feedback -> Test
test name runner = Test name testPrivacy Visible runner 0.0 testScore testThreshold False True

validationTest :: String -> IO Feedback -> Test
validationTest name runner = Test name Public Visible runner 0.0 0.0 0.0 False True

-- Entry-point. Run all tests and output in Ed's custom test JSON format
main :: IO ()
main = do
  results <-
    normaliseResults capTestScores . zip tests
      <$> mapM (handled False "The testing code encountered an exception." . runner) tests
  putStrLn $ json [("testcases", show results)]

-------------------------------------------------------------------------------
-- YOU PROBABLY DONT NEED TO MODIFY ANYTHING BELOW HERE
-------------------------------------------------------------------------------

data Test
  = Test
  { -- | The name of the test
    name :: String,
    -- | The privacy of the test
    privacy :: Privacy,
    -- | The viibility of the test
    visibility :: Visibility,
    -- | How to run the test
    runner :: IO Feedback,
    -- | Minimum score for the test
    minScore :: Double,
    -- | Maximum score for the test
    maxScore :: Double,
    -- | Score required to pass the test
    passThreshold :: Double,
    -- | Scale test score by maxScore? Useful for "scored" tests
    scale :: Bool,
    -- | Clamp the score between minScore and maxScore
    doClamp :: Bool
  }

data Visibility
  = Hidden
  | Visible
  deriving (Eq)

data Privacy
  = Private
  | Public
  deriving (Eq)

data Feedback = Feedback
  { -- | Did the test case encounter an unexpected error?
    ok :: Bool,
    -- | Score of the test
    score :: Double,
    -- | Feedback to give to students
    feedback :: String,
    -- | Observed output of this test
    observedOutput :: Maybe String,
    -- | Expected output of this test
    expectedOutput :: Maybe String
  }

data Result
  = Result
  { -- | Did this test pass?
    passed :: Bool,
    -- | Test case that was run
    testcase :: Test,
    -- | Feedback from running the test
    output :: Feedback
  }

instance Show Result where
  show
    Result
      { passed,
        testcase = Test {name, privacy, visibility, minScore, maxScore},
        output = Feedback {ok, score, feedback, observedOutput, expectedOutput}
      } =
      json
        [ ("name", string name),
          ("ok", bool ok),
          ("private", bool $ privacy == Private),
          ("hidden", bool $ visibility == Hidden),
          ("score", double score),
          ("passed", bool passed),
          ("feedback", string feedback),
          ("min_score", double minScore),
          ("max_score", double maxScore),
          ("observed", null_ (string <$> observedOutput)),
          ("expected", null_ (string <$> expectedOutput))
        ]

-- Normalise scores for tests with given feedback.
-- If capScore is True, add a negatively scored result if the total score exceeds the max score,
-- or a positivly scored result if below zero
normaliseResults :: Bool -> [(Test, Feedback)] -> [Result]
normaliseResults capScore = adjust . map normScore
  where
    normScore (test@Test {doClamp, minScore, maxScore, scale, passThreshold}, feedback@Feedback {score}) =
      let scaledScore = if scale then maxScore * score else score
          clampedScore = (if doClamp then clamp (minScore, maxScore) else id) scaledScore
          finalScore = if isInfinite clampedScore then 0.0 else clampedScore
          passed = if minScore == maxScore then scaledScore >= maxScore else scaledScore >= passThreshold
       in Result passed test feedback {score = finalScore}
    adjust results
      | capScore =
          results
            ++ [ Result True (test "Score adjustment" (runSimple "echo" [])) {maxScore = 0} $
                   if totalScore > totalMaxScore
                     then successFeedback ("Score exceeded maximum allowed score: " ++ double totalScore) (totalMaxScore - totalScore)
                     else
                       if totalScore < 0
                         then successFeedback ("Score was below zero: " ++ double totalScore) (-totalScore)
                         else successFeedback "Score did not require adjustment" 0.0
               ]
      | otherwise = results
      where
        totalScore = sum $ map (score . output) results
        totalMaxScore = sum $ map (maxScore . testcase) results

-- \* Compile tests

compile :: FilePath -> [String] -> IO Feedback -> IO Feedback
compile cc args next =
  handled True "Runtime exception occured during compilation." $ do
    (code, _, err) <- exec cc args
    if code == ExitSuccess
      then next
      else return $ errorFeedback True "Compilation error." (Just err)

compileHaskell :: FilePath -> FilePath -> IO Feedback -> IO Feedback
compileHaskell mainFile out = compile "ghc" ["-O2", "-no-keep-hi-files", "-no-keep-o-files", "-o", out, mainFile]

compileProlog :: FilePath -> FilePath -> IO Feedback -> IO Feedback
compileProlog mainFile out = compile "swipl" ["-O", "-g", "go", "-t", "halt", "-o", out, "-c", mainFile]

-- \* Run tests

-- Provide feedback based on the stdout of the program
runFeedback :: (String -> IO Feedback) -> FilePath -> [String] -> IO Feedback
runFeedback feedback exe args =
  handled True "Runtime exception occured." $ do
    (code, out, err) <- exec exe args
    if code == ExitSuccess
      then feedback out
      else return $ errorFeedback True "Program exitted with a non-zero exit code." $ Just $ out ++ "\n" ++ err

-- Succeed if program executes sucessfully, with exit code 0
runSimple :: FilePath -> [String] -> IO Feedback
runSimple = runFeedback (const $ return $ successFeedback "Code ran successfully." infinity)

-- Compare output of program with contents of a file, suceeding iff they match
runExpected :: FilePath -> FilePath -> [String] -> IO Feedback
runExpected expectedFile = runFeedback ((readFile expectedFile <&>) . expectedFeedback)

-- Compare output of program with a string, suceeding iff they match
runExpectedString :: String -> FilePath -> [String] -> IO Feedback
runExpectedString expectedOutput = runFeedback $ return . (`expectedFeedback` expectedOutput)

-- Run the program, reading the feedback and score from stdout
-- score is the last line, pared as a double, feedback is the rest
runScored :: FilePath -> [String] -> IO Feedback
runScored =
  runFeedback
    ( \out ->
        return $ case first readMaybe <$> uncons (reverse (lines out)) of
          Just (Just score, feedback) -> successFeedback (unlines (reverse feedback)) score
          _ -> errorFeedback False "Unknown error parsing output." $ Just out
    )

-- Limit the time for some feedback, in seconds
timed :: Double -> IO Feedback -> IO Feedback
timed timeLimit =
  fmap (fromMaybe (errorFeedback True "Program failed to run within time limit." Nothing))
    . timeout (floor $ timeLimit * 1_000_000)

-- \* Utils

handled :: Bool -> String -> IO Feedback -> IO Feedback
handled ok message = handle (return . exceptionFeedback ok message)

exec :: FilePath -> [String] -> IO (ExitCode, String, String)
exec exe args = readCreateProcessWithExitCode (proc exe args) ""

exceptionFeedback :: Bool -> String -> SomeException -> Feedback
exceptionFeedback ok feedback exception = Feedback ok (-infinity) feedback (Just $ show exception) Nothing

errorFeedback :: Bool -> String -> Maybe String -> Feedback
errorFeedback ok feedback output = Feedback ok (-infinity) feedback output Nothing

successFeedback :: String -> Double -> Feedback
successFeedback feedback score = Feedback True score feedback Nothing Nothing

expectedFeedback :: String -> String -> Feedback
expectedFeedback observed expected =
  ( if expected == observed
      then Feedback True infinity expected
      else Feedback True (-infinity) "See diff."
  )
    (Just observed)
    (Just expected)

infinity :: Double
infinity = read "Infinity"

json :: [(String, String)] -> String
json kvs = "{" ++ intercalate "," (map (\(field, value) -> string field ++ ":" ++ value) kvs) ++ "}"

bool :: Bool -> String
bool = map toLower . show

null_ :: Maybe String -> String
null_ = fromMaybe "null"

double :: Double -> String
double = printf "%.3f"

string :: String -> String
string = ("\"" ++) . (++ "\"") . concatMap esc
  where
    esc '\"' = "\\\""
    esc '\\' = "\\\\"
    esc '\b' = "\\b"
    esc '\f' = "\\f"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c
      | isAscii c && not (isControl c) = [c]
      | otherwise = "\\u" ++ replicate (4 - length hex) '0' ++ hex
      where
        hex = showHex (fromEnum c) ""
