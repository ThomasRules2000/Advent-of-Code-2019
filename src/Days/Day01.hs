module Days.Day01 where
import qualified Program.RunDay as R (runDay)

runDay :: String -> IO (Maybe Double, Maybe Double)
runDay = R.runDay parser part1 part2

type Input = [Int]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map read . lines

part1 :: Input -> Output1
part1 = sum . map requiredFuel

requiredFuel :: Int -> Int
requiredFuel = subtract 2 . flip div 3

part2 :: Input -> Output2
part2 = sum . map fuelCalc

fuelCalc :: Int -> Int
fuelCalc x
    | fuel <= 0 = 0
    | otherwise = fuel + fuelCalc fuel
    where fuel = requiredFuel x
