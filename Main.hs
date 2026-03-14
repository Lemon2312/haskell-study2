import Control.Monad (guard)

-- ① モナドを使わずに書いた場合 (高階関数 concatMap のネスト)
-- 全ての組み合わせを手動で展開するため、ネストが深くなり非常に読みにくいです。
pythagoreanWithoutMonad :: Int -> [(Int, Int, Int)]
pythagoreanWithoutMonad n =
  concatMap (\x ->
    concatMap (\y ->
      concatMap (\z ->
        if x^2 + y^2 == z^2
          then [(x, y, z)] -- 条件に合えば要素を返す
          else []          -- 合わなければ空リスト(失敗)を返す
        ) [y .. n]
      ) [x .. n]
    ) [1 .. n]

-- ② リストモナド（do記法）を使って書いた場合
-- モナドの力でネストが平坦(フラット)になり、手続き型言語のように直感的に書けます。
pythagoreanWithMonad :: Int ->[(Int, Int, Int)]
pythagoreanWithMonad n = do
  x <- [1 .. n]            -- 1からnまでの複数の可能性を x に束縛
  y <- [x .. n]            -- xからnまでの複数の可能性を y に束縛
  z <- [y .. n]            -- yからnまでの複数の可能性を z に束縛
  guard (x^2 + y^2 == z^2) -- 条件を満たさない計算経路をここで「枝刈り」する
  return (x, y, z)         -- 成功した結果を返す

-- ③ おまけ: リスト内包表記
-- 実はHaskellのリスト内包表記は、②のリストモナドの「構文糖衣(シンタックスシュガー)」です。
pythagoreanComprehension :: Int -> [(Int, Int, Int)]
pythagoreanComprehension n =
  [ (x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [y .. n], x^2 + y^2 == z^2 ]

main :: IO ()
main = do
  let limit = 20
  
  putStrLn "=== ① モナドを使わない場合 (concatMap) ==="
  print $ pythagoreanWithoutMonad limit

  putStrLn "\n=== ② リストモナドを使った場合 (do記法) ==="
  print $ pythagoreanWithMonad limit

  putStrLn "\n=== ③ リスト内包表記を使った場合 ==="
  print $ pythagoreanComprehension limit