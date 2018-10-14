module Main where

-- Basic Datatype
data Term = Con Int
          | Div Term Term

-- examples
okEx, errorEx :: Term
okEx = Div (Div (Con 1972) (Con 2))(Con 4)
errorEx  = Div (Con 1)(Con 0)


-- Section Basic Evaluator

-- Basic Evaluator
eval :: Term -> Int
eval (Con a)   = a
eval (Div t u) = eval t `div` eval u


-- Section 2.2 Evaluator with Errors

-- Exception definition
type Exception = String
data M2_2 a = Raise Exception
         | Return a
         deriving (Show)


-- Evaluator
eval2_2 :: Term -> M2_2 Int
eval2_2 (Con a) = Return a
eval2_2 (Div t u) = case eval2_2 t of
                      Raise e -> Raise e
                      Return a ->
                          case eval2_2 u of
                            Raise e -> Raise e
                            Return b ->
                              if b == 0
                                 then Raise "Divide by Zero Error"
                                 else Return (a `div` b)

-- Section 2.3 Evaluator with State
-- type definition
type M2_3 a = State -> (a, State)
type State = Int

-- Evaluator
eval2_3 :: Term -> M2_3 Int
eval2_3 (Con a) x = (a, x)
eval2_3 (Div t u) x = let (a, y) = eval2_3 t x in
                      let (b, z) = eval2_3 u y in
                      (a `div` b, z + 1)


-- Run some things
main :: IO ()
main = do
  -- Basic
  -- Works as expected
  --putStrLn $ show $ eval okEx
  -- Divide by Zero expection
  --putStrLn $ show $ eval errorEx

  -- Section 2.2
  putStrLn $ show $ eval2_2 okEx
  -- Divide by Zero expection
  putStrLn $ show $ eval2_2 errorEx

  -- Section 2.3
  putStrLn $ show $ eval2_3 okEx 0




