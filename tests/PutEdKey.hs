import Test.HUnit
import Lib

tests = TestList [TestLabel "test1" test1]
test1 = TestCase (assertEqual "hello world" True True)