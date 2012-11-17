data Choice = A | B | C
choices :: [Choice]
choices = [A, B, C]

type Name = String
type Kokando = Int
type Character = (Name, Kokando)
type SaveData = [Character]
initialSaveData :: SaveData
initialSaveData =
  [("hoge", 0)
  ,("foo", 0)
  ,("bar", 0)
  ,("fuga", 0)]

type Situation = Choice -> SaveData -> SaveData
situA :: Situation

situB :: Situation
situC :: Situation
situD :: Situation
situE :: Situation

changeKokando :: Character -> Int -> Character
changeKokando (name, kokando) i = (name, kokandom + i)

situations :: [Situation]
situations = [situA, situB, situC, situD, situE]

simulateKokando :: [Situation] -> SaveData -> [SaveData]

main :: IO()
main = print $ simulateKokando situations initialSaveData
