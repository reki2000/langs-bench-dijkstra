import Test.Hspec
import qualified Data.ByteString.Char8 as B

import MapLoader

main :: IO ()
main = hspec spec

strtof100test :: String -> Int
strtof100test str = strtof100 str 0 (- 1)

spec :: Spec
spec = do
    describe "strtof100" $ do 
        it "enough fractions" $
            strtof100test "123.4567" `shouldBe` 12345
        it "2 decimal" $
            strtof100test "123.45" `shouldBe` 12345
        it "1 decimal" $
            strtof100test "123.4" `shouldBe` 12340
        it "0 decimal" $
            strtof100test "123" `shouldBe` 12300
        it "special case" $
            strtof100test "79.7\r" `shouldBe` 7970

    describe "parseLine" $ do
        it "should parse 3 items" $
            parseLine (B.pack "1,2,3,4,5,6") `shouldBe` (3,4,600)
        it "ignore invalid line" $
            parseLine (B.pack "1,2,3,4,5") `shouldBe` (0,0,0)
        it "ignore empty line" $
            parseLine (B.pack "") `shouldBe` (0,0,0)
