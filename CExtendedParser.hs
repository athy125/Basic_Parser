main :: IO ()
main = do
  let code =
        "var x = 10; \
        \var y = 20; \
        \var z = x + y; \
        \if (z > 15) { \
        \  var a = 100; \
        \  var b = a + 50; \
        \} else { \
        \  var c = 5; \
        \} \
        \while (z > 0) { \
        \  z = z - 1; \
        \} \
        \func add(a, b) { \
        \  return a + b; \
        \} \
        \var result = add(x, y);"

  case parseCSubset code of
    Left err -> print err
    Right ast -> print ast
