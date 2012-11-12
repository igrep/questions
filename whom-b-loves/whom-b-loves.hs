module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Debug.Trace

-- クラスの男子の身長
danshiHeights :: IntMap String
danshiHeights =
  IntMap.fromList [
  (161, "たかし")
  ,(174, "あつし")
  ,(200, "きよし")
  ,(156, "なおや")
  ,(180, "こうじ")
  ,(177, "かずお")
  ,(169, "ゆうや")
  ,(184, "かずき")
  ,(163, "みつを")
  ,(149, "じろう")]

-- 出席番号順に並べたクラスの男子
danshiByNumber :: Map String Int
danshiByNumber =
  Map.fromList [
  ("あつし",  1)
  ,("かずお",  3)
  ,("かずき",  4)
  ,("きよし",  7)
  ,("こうじ",  8)
  ,("じろう", 10)
  ,("たかし", 13)
  ,("なおや", 15)
  ,("みつを", 18)
  ,("ゆうや", 22)]

-- Hogeマンション2号棟2階に住んでいる男の子とその年齢。
-- n番目のリストにある名前が200+n号室に住んでいる男の子の名前をそのまま表す。
-- Nothingである場合はもちろん部屋に男の子が住んでいないということ。
-- 注：nは0ではなく1から数えてください。
hoge22Boys :: [(Maybe (String, Int))]
hoge22Boys =
  [Nothing
  ,Just ("だいざぶろう", 12)
  ,Just ("こうじろう", 7)
  ,Just ("でんじろう", 11)
  ,Just ("しゅうべえ", 3)
  ,Nothing
  ,Just ("とうきちろう", 19)
  ,Just ("ちょうすけ", 9)
  ,Just ("じゅんいちろう", 70)
  ,Nothing]

-- Hogeマンション2号棟2階に住んでいる女の子とその年齢。
-- 男の子と同様。
hoge22Girls :: [Maybe (String, Int)]
hoge22Girls =
  [Just ("はなこ", 17)
  ,Nothing
  ,Just ("よしこ", 13)
  ,Just ("みつこ", 11)
  ,Just ("たかこ", 1)
  ,Nothing
  ,Just ("とうきちこ", 20)
  ,Just ("じゅんこ", 21)
  ,Just ("きくこ", 14)
  ,Just ("きょうこ", 15)]

-- Hogeマンション2号棟2階に住んでいる女の子とその親友
hoge22Friends :: Map String String
hoge22Friends = Map.fromList [
  ("はなこ", "かずえ")
  ,("みつこ", "よしこ")
  ,("よしこ", "みつこ")
  ,("とうきちこ", "じゅんいちこ")
  ,("じゅんこ", "じょんこ")
  ,("きくこ", "さえ")
  ,("きょうこ", "ようこ")]

-- Aさんの調べた女の子とその元カレ
girlsMotoKare :: Map String String
girlsMotoKare = Map.fromList [
  ("かずえ", "きくお")
  ,("みつこ", "じゅんいちろう")
  ,("まつこ", "じょん")
  ,("ようこ", "ようた")
  ,("よしこ", "みつを")
  ,("たかこ", "じゅんいちろう")
  ,("さえ", "たかし")]

-- generic function
monadicIndex :: Monad m => [a] -> Int -> m a
monadicIndex [] _ = fail "monadicIndex: Index out of range!"
monadicIndex (x:_) 0 = return x
monadicIndex (_:xs) n
  | n < 0 = fail "monadicIndex: Index out of range!"
  | otherwise = monadicIndex xs ( n - 1 )
--

lookupAne :: Int -> Int -> Maybe String
-- 厳密な意味で同じ部屋に住んでいる姉を探す関数
lookupAne room boyAge = do
  (aneName, aneAge) <- join $ hoge22Girls `monadicIndex` room
  if aneAge > boyAge
    then return aneName
    else Nothing

-- general function for debugging
inspect :: Show a => String -> a -> a
inspect msg x = trace ( msg ++ ": " ++ show x ) x
--

findWhomBLoves :: Maybe String
findWhomBLoves = do
  danshi180 <- inspect "danshi180" $ IntMap.lookup 180 danshiHeights
  danshiNumber <- inspect "danshiNumber" $ Map.lookup danshi180 danshiByNumber
  (_boy, age) <- inspect "(_boy, age)" $ join $ hoge22Boys `monadicIndex` (danshiNumber - 1)
  ane <- inspect "ane" $ lookupAne danshiNumber age
  friend <- inspect "friend" $ Map.lookup ane hoge22Friends
  Map.lookup friend girlsMotoKare

printResult :: Maybe String -> IO()
printResult Nothing = putStrLn "Bの好きな男は結局分かりませんでした！ダメじゃん！"
printResult (Just s) = putStrLn s

main :: IO ()
main = printResult findWhomBLoves
