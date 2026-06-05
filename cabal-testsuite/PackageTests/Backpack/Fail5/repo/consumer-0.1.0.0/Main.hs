import Framework (greet)
import Utils (utilHelper)
main :: IO ()
main = putStrLn greet >> putStrLn (utilHelper "direct")
