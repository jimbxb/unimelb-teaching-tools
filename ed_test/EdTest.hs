-- File: EdTest.hs
-- Purpose: Custom Ed testing tool.

-- Complie and run test individually, using Ed's custom test format.
-- run command: ghc -O2 EdTest.hs > /dev/null && ./EdTest

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

tests :: [Test]
tests =
  [ validationTest "Validation test" (scoredTest "0"),
    test "Real test" (scoredTest "1")
  ]

testTime :: Double
testTime = 5.0

testScore :: Double
testScore = 10.0

testThreshold :: Double
testThreshold = testScore / 2

capTestScores :: Bool
capTestScores = True

testPrivacy :: Privacy
testPrivacy = Private

expectedTest :: String -> IO Feedback
expectedTest i = preprocess dir out $ timed testTime $ expected (dir </> "expected.txt") out
  where
    dir = "." </> i; out = dir </> "out"

scoredTest :: String -> IO Feedback
scoredTest i = preprocess dir out $ timed testTime $ scored out
  where
    dir = "." </> i; out = dir </> "out"

preprocess :: FilePath -> FilePath -> IO Feedback -> IO Feedback
-- preprocess dir = compileProlog (dir </> "main.pl")
preprocess dir = compileHaskell (dir </> "Main.hs")

main :: IO ()
main = do
  results <- normaliseResults capTestScores . zip tests <$> mapM runTest tests
  putStrLn $ json [("testcases", show results)]

-------------------------------------------------------------------------------
-- YOU PROBABLY DONT NEED TO MODIFY ANYTHING BELOW HERE
-------------------------------------------------------------------------------

-- opinionated test constuctors
test :: String -> IO Feedback -> Test
test name runner = Test name testPrivacy Visible runner 0.0 testScore testThreshold False True

validationTest :: String -> IO Feedback -> Test
validationTest name runner = Test name Public Visible runner 0.0 0.0 0.0 False True

data Test
  = Test
  { name :: String,
    privacy :: Privacy,
    visibility :: Visibility,
    runner :: IO Feedback,
    minScore :: Double,
    maxScore :: Double,
    passThreshold :: Double,
    scale :: Bool,
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
  { ok :: Bool,
    score :: Double,
    feedback :: String,
    observedOutput :: Maybe String,
    expectedOutput :: Maybe String
  }

data Result
  = Result
  { passed :: Bool,
    test_ :: Test,
    output :: Feedback
  }

instance Show Result where
  show
    Result
      { passed,
        test_ = Test {name, privacy, visibility, minScore, maxScore},
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
-- If capScore is True, add a negatively scored test if the scores exceed the max score
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
            ++ [ Result True (test "Score adjustment" (simple "echo")) {maxScore = 0} $
                   if totalScore > totalMaxScore
                     then successFeedback ("Score exceeded maximum allowed score: " ++ double totalScore) (totalMaxScore - totalScore)
                     else successFeedback "Score did not exceed maximum allowed score" 0.0
               ]
      | otherwise = results
      where
        totalScore = sum $ map (score . output) results
        totalMaxScore = sum $ map (maxScore . test_) results

runTest :: Test -> IO Feedback
runTest Test {runner} =
  handled False "The testing code encountered an exception." runner

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
runFeedback :: (String -> IO Feedback) -> FilePath -> IO Feedback
runFeedback feedback exe =
  handled True "Runtime exception occured." $ do
    (code, out, _) <- exec exe []
    if code == ExitSuccess
      then feedback out
      else return $ errorFeedback True "Program exitted with a non-zero exit code." $ Just out

-- Suceed if program executes sucessfully, with exit code 0
simple :: FilePath -> IO Feedback
simple = runFeedback (const $ return $ successFeedback "Program ran successfully." infinity)

-- Compare expected output of program with an expected file, suceeding iff they match
expected :: FilePath -> FilePath -> IO Feedback
expected expectedFile = runFeedback $ (readFile expectedFile <&>) . expectedFeedback

-- Run the program, reading the feedback and score from stdout
-- score is the last line, pared as a double, feedback is the rest
scored :: FilePath -> IO Feedback
scored =
  runFeedback $ \out ->
    return $ case first readMaybe <$> uncons (reverse (lines out)) of
      Just (Just score, feedback) -> successFeedback (unlines (reverse feedback)) score
      _ -> errorFeedback False "Unknown error parsing output." $ Just out

-- limit the time for some feedback, in seconds
timed :: Double -> IO Feedback -> IO Feedback
timed timeLimit test =
  fromMaybe (errorFeedback True "Program failed to run within time limit." Nothing)
    <$> timeout (floor $ timeLimit * 1_000_000) test

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