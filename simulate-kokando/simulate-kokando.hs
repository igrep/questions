data Choice = A | B | C
choices :: [Choice]
choices = [A, B, C]

type Name = String
type Kokando = Int
type Character = (Name, Kokando)
type SaveData = [Character]
initialSaveData :: SaveData
initialSaveData =
  [("", 0)
  ,("", 0)
  ,("", 0)
  ,("", 0)]

type Situation = Choice -> SaveData -> SaveData
situA :: Situation
situB :: Situation
situC :: Situation
situD :: Situation
situE :: Situation

type ChangeKokando = Choice -> Character -> Character

situations :: [Situation]
situations = [situA, situB, situC, situD, situE]

simulateKokando :: [Situation] -> SaveData -> [SaveData]

main :: IO()
main = print $ simulateKokando situations initialSaveData
