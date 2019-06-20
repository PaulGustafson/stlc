import Lib

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ show $ check (Lit False) Atom
  putStrLn $ show $ synth (Lit False) == (Just Atom)
