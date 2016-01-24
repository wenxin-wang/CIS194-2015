import HW01

main :: IO()
main = do
  print $ length $ hanoin 15 "a" "b" ["c", "d"]
  print $ hanoil 15 4
