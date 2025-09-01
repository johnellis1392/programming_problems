module AOC2020.Day4Spec (main, spec) where

import AOC2020.Day4
import Common.Utils (parseWith)
import Control.Monad (forM_)
import Test.Hspec
import Text.Parsec qualified as P
import Text.RawString.QQ

main :: IO ()
main = hspec spec

testInput :: String
testInput =
  [r|
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
|]

invalidPassports :: String
invalidPassports =
  [r|
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
|]

validPassports :: String
validPassports =
  [r|
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
|]

spec :: Spec
spec = do
  describe "Field Parsing Tests" $ do
    let testCases :: [(String, Field)] =
          [ ("hcl:#888785", HCL "#888785"),
            ("iyr:2010", IYR "2010"),
            ("hgt:158cm", HGT "158cm"),
            ("ecl:blu", ECL "blu"),
            ("byr:1944", BYR "1944"),
            ("eyr:2021", EYR "2021"),
            ("pid:093154719", PID "093154719")
          ]
    forM_ testCases $ \(input, expected) ->
      it ("parseField(" ++ input ++ ") = " ++ show expected) $ do
        parseWith field "Day4Spec" input `shouldBe` expected

  describe "part1" $ do
    it "should pass basic test" $ do
      let input = parse testInput
      part1 input `shouldBe` 2

  describe "part2 validation tests" $ do
    let testCases :: [(String, Bool)] =
          [ ("hcl:#888785", True),
            ("hcl:888785", False),
            ("hcl:#88785", False),
            ("iyr:2010", True),
            ("iyr:201", False),
            ("iyr:2009", False),
            ("iyr:2021", False),
            ("hgt:158cm", True),
            ("hgt:149cm", False),
            ("hgt:195cm", False),
            ("hgt:70in", True),
            ("hgt:58in", False),
            ("hgt:79in", False),
            ("ecl:blu", True),
            ("ecl:amb", True),
            ("ecl:brn", True),
            ("ecl:gry", True),
            ("ecl:grn", True),
            ("ecl:hzl", True),
            ("ecl:oth", True),
            ("ecl:red", False),
            ("ecl:aaa", False),
            ("ecl:somethingelse", False),
            ("byr:1944", True),
            ("byr:1919", False),
            ("byr:2003", False),
            ("eyr:2021", True),
            ("eyr:2019", False),
            ("eyr:2031", False),
            ("pid:09315471", False),
            ("pid:0931547100", False),
            ("pid:a93154719", False),
            ("pid:093154719", True)
          ]
    forM_ testCases $ \(input, expected) ->
      it ("valid(" ++ input ++ ") = " ++ show expected) $ do
        (valid . parseWith field "Day4Spec") input `shouldBe` expected

  describe "part2" $ do
    it "should parse valid passports" $ do
      (part2 . parse) validPassports `shouldBe` 4

    it "should parse invalid passports" $ do
      (part2 . parse) invalidPassports `shouldBe` 0

  describe "parsec parsing" $ do
    it "should parse correctly" $ do
      P.parse field "" "pid:093154719" `shouldBe` Right (PID "093154719")

    it "should parse passports" $ do
      P.parse passport "" "hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022"
        `shouldBe` Right [HCL "#888785", HGT "164cm", BYR "2001", IYR "2015", CID "88", PID "545766238", ECL "hzl", EYR "2022"]

    it "should parse passports2" $ do
      P.parse passport "" "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
        `shouldBe` Right [IYR "2010", HGT "158cm", HCL "#b6652a", ECL "blu", BYR "1944", EYR "2021", PID "093154719"]