{-# LANGUAGE RankNTypes #-}

import qualified Data.Map.Strict as M
import System.Environment
import Control.Monad.State
import Control.Lens
import Syntax
import Parser
import Token (alexScanTokens)

type SymbolTable = M.Map String Int

symbol :: String -> (forall f . Functor f => (Int -> f Int) -> M.Map String Int -> f (SymbolTable))
symbol str f env = fmap (\int -> M.insert str int env) (f (env M.! str))

withMapInterpret :: Prog -> StateT (SymbolTable) IO ()
withMapInterpret prog =
        case prog of
            [] -> return ()
            Assign str expr : xs -> do
                result <- simplify expr
                symbol str .= result
                withMapInterpret xs
            Print expr : xs -> do
                result <- simplify expr
                liftIO (print result)
                withMapInterpret xs
            If expr prog1 prog2 : xs -> do
                result <- simplify expr
                case result of
                    0 -> withMapInterpret prog2
                    _ -> withMapInterpret prog1
                withMapInterpret xs
            While expr prog1 : xs -> do
                result <- simplify expr
                case result of
                    0 -> withMapInterpret xs
                    _ -> withMapInterpret prog1 >> withMapInterpret prog


simplify :: Expression -> StateT (SymbolTable) IO Int
simplify expr =
        case expr of
            Constant int -> return int
            Var str -> use (symbol str)
            BinOp op expr1 expr2 -> do
                result1 <- simplify expr1
                result2 <- simplify expr2
                case op of
                    Add -> return (result1 + result2)
                    Sub -> return (result1 - result2)
                    LessThan -> return (fromEnum (result1 < result2))
                    EqualTo -> return (fromEnum (result1 == result2))
                    Multi -> return (result1 * result2)
                    Div -> return (result1 `div` result2)



interpret :: Prog -> IO ()
interpret prog = evalStateT (withMapInterpret prog) M.empty




main :: IO ()
main = do
        filename:_ <- getArgs
        program <- readFile filename
        interpret $ atticParser $ alexScanTokens program
      {-  interpret [Print (BinOp Add (Constant 5) (Constant 6))]
        interpret [(Assign "foo" (Constant 5)), (Print (Var "foo"))]
        interpret [(Assign "foo" (Constant 5)), (While (Var "foo")
                  [(Print (Var "foo")), (Assign "foo" (BinOp Sub (Var "foo") (Constant 1)))])
                  , (Print (Constant 10))]
        interpret [If (Constant 0) [(If (Constant 0) [] [(Print (Constant 5))])] []]
        -}
