import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.Nemnem.Parse.Cpp

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unit_tests]

unit_tests = testGroup "Nemnem.Parse.Cpp"
  [ testGroup "No includes"
      [ testCase "NoCpp" $ test_no_cpp
      , testCase "EraseLine" $ test_erase_line
      , testCase "KeepLine" $ test_keep_line
      , testCase "ModifyLine" $ test_modify_line
      ]
  ]

dummy_src_name = "X.hs"
cpp_line_prefix = "{-# LINE 1 \"X.hs\" #-}\n"

test_no_cpp = do
  let src = "module Main where"
  (transformed, mods) <- unCpp [] dummy_src_name src
  transformed @?= src
  mods @?= Just []

test_erase_line = do
  let src = "module Main where\n#ifdef APPLE\nx = 1\n#endif\ny = 2"
      expected = "module Main where\n\n\n\ny = 2"
  (transformed, mods) <- unCpp [] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  mods @?= Just
    [ ModLines 1 2 Generated
    , ModLines 3 4 (Deleted "#ifdef APPLE")
    , ModLines 4 5 (Deleted "x = 1")
    , ModLines 5 6 (Deleted "#endif")
    ]

test_keep_line = do
  let src = "module Main where\n#ifdef APPLE\nx = 1\n#endif\ny = 2"
      expected = "module Main where\n\nx = 1\n\ny = 2"
  (transformed, mods) <- unCpp [("APPLE", "1")] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  mods @?= Just
    [ ModLines 1 2 Generated
    , ModLines 3 4 (Deleted "#ifdef APPLE")
    , ModLines 5 6 (Deleted "#endif")
    ]

test_modify_line = do
  let src = "module Main where\n#define APPLE(x) apples = x\nAPPLE(3)"
      expected = "module Main where\n\napples = 3"
  (transformed, mods) <- unCpp [] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  mods @?= Just
    [ ModLines 1 2 Generated
    , ModLines 3 4 (Deleted "#define APPLE(x) apples = x")
    , ModLines 4 5 (ExpandedFrom "APPLE(3)")
    ]
