module Test.Language.Haskell.Nemnem where

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
  (transformed, cpp_lines) <- unCpp [] dummy_src_name src
  transformed @?= cpp_line_prefix ++ src
  cpp_lines @?= CppLines S.empty S.empty

test_erase_line = do
  let src = "module Main where\n#ifdef APPLE\nx = 1\n#endif\ny = 2"
      expected = "module Main where\n\n\n\ny = 2"
  (transformed, cpp_lines) <- unCpp [] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  cpp_lines @?= CppLines
                  { cppErased = S.fromList [2, 3, 4]
                  , cppExpanded = S.empty }

test_keep_line = do
  let src = "module Main where\n#ifdef APPLE\nx = 1\n#endif\ny = 2"
      expected = "module Main where\n\nx = 1\n\ny = 2"
  (transformed, cpp_lines) <- unCpp [("APPLE", "1")] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  cpp_lines @?= CppLines
                  { cppErased = S.fromList [2, 4]
                  , cppExpanded = S.empty }

test_modify_line = do
  let src = "module Main where\n#define APPLE(x) apples = x\nAPPLE(3)"
      expected = "module Main where\n\napples = 3"
  (transformed, cpp_lines) <- unCpp [] dummy_src_name src
  transformed @?= cpp_line_prefix ++ expected
  cpp_lines @?= CppLines
                  { cppErased = S.singleton 2
                  , cppExpanded = S.singleton 3 }
