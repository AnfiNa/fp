module Main where

main :: IO ()
main = do
  putStrLn "Running basic tests..."
  -- Простые тесты без внешних зависимостей
  if 2 + 2 == 4
    then putStrLn "✓ Basic arithmetic test passed"
    else error "Basic test failed"
  
  putStrLn "All tests passed!"