{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Compiler where

import qualified LLVM.General.AST.CallingConvention as G
import qualified LLVM.General.AST.IntegerPredicate as P
import qualified LLVM.General.AST.Constant as C
import LLVM.General.AST.Instruction (Named ((:=)))
import Syntax
import qualified LLVM.General.AST as G
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Type as G
import qualified LLVM.General.AST.AddrSpace as G
import Control.Lens
import Control.Monad.State
import Data.Word
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.List
import qualified LLVM.General.AST.Instruction as I

type DList a = [a] -> [a]

data BlockInfo = BlockInfo
               { _count :: Word
               , _blocks :: [G.BasicBlock]
               , _symbolTable :: M.Map String G.Name
               , _currentBlock :: DList (G.Named I.Instruction)
               }

makeLenses ''BlockInfo

-- Making a single block from curreBlock

makeBasicBlock :: G.Name -> G.Named (G.Terminator) -> State BlockInfo ()
makeBasicBlock name terminator = do
        instr <- use currentBlock
        blocks %= \xs -> ((G.BasicBlock name (dListToList instr) terminator):xs)
        currentBlock .= emptyDList

-- Allows constant append and prepend for a list

dListToList :: DList a -> [a]
dListToList dList = dList []

append :: a -> DList a -> DList a
append a f = \xs -> f (a:xs)

prepend :: a -> DList a -> DList a
prepend a f = \xs -> a : (f xs)

emptyDList :: DList a
emptyDList = id

-- End of DList


symbol :: String -> (forall f . Functor f
                    => (G.Name -> f G.Name) -> M.Map String G.Name -> f (M.Map String G.Name))
symbol str f env = fmap (\int -> M.insert str int env) (f (env M.! str))

compileBlockInfo :: [Stmt] -> State BlockInfo ()
compileBlockInfo code = do
        declareVar code
        mapM_ compileStatement code
        k <- newName
        makeBasicBlock k (G.Do (G.Ret (Just (G.ConstantOperand (C.Int 32 0))) []))




compileExpression :: Expression -> State BlockInfo G.Name
compileExpression expr =
        case expr of
            Constant int -> do
                k <- newName
                currentBlock %= append (k := G.Add False False
                                       (G.ConstantOperand (C.Int 64 (fromIntegral int)))
                                       (G.ConstantOperand (C.Int 64 0))
                                       [])
                return k

            Var str -> do
                k <- newName
                var <- use $ symbolTable.(symbol str)
                currentBlock %= append (k := G.Load False (G.LocalReference
                                       (G.PointerType G.i64 (G.AddrSpace 0)) (var))
                                       Nothing 0 [])
                return k
            BinOp oper expr1 expr2 -> do
                k <- newName
                op1 <- compileExpression expr1
                op2 <- compileExpression expr2
                case oper of
                    Add -> do
                        currentBlock %= append (k := G.Add False False
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        return k

                    Sub -> do
                        currentBlock %= append (k := G.Sub False False
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        return k

                    LessThan -> do
                        currentBlock %= append (k := G.ICmp P.SLT
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        k2 <- newName
                        currentBlock %= append (k2 := G.ZExt
                                               (G.LocalReference G.i1 k)
                                               G.i64
                                               [])
                        return k2

                    EqualTo -> do
                        currentBlock %= append (k := G.ICmp P.EQ
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        k2 <- newName
                        currentBlock %= append (k2 := G.ZExt
                                               (G.LocalReference G.i1 k)
                                               G.i64
                                               [])
                        return k2

                    Multi -> do
                        currentBlock %= append (k := G.Mul False False
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        return k

                    Div -> do
                        currentBlock %= append (k := G.SDiv False
                                            (G.LocalReference G.i64 op1)
                                            (G.LocalReference G.i64 op2)
                                            [])
                        return k

getVarNames :: [Stmt] -> [String]
getVarNames code =
        case code of
            [] -> []
            Assign str _ : xs -> str : getVarNames xs
            If _ prog1 prog2 : xs -> getVarNames prog1
                ++ getVarNames prog2 ++ getVarNames xs
            While _ prog1 : xs -> getVarNames prog1 ++ getVarNames xs
            _ : xs -> getVarNames xs

declareVar :: [Stmt] -> State BlockInfo ()
declareVar code = mapM_ addToSymbolTable $ nub $ getVarNames code

addToSymbolTable :: String -> State BlockInfo ()
addToSymbolTable str = do
        k <- newName
        symbolTable.(symbol str) .= k
        currentBlock %= append (k := G.Alloca G.i64 Nothing 0 [])

newName :: State BlockInfo G.Name
newName = do
        n <- use count
        count += 1
        return (G.UnName n)

compileStatement :: Stmt -> State BlockInfo ()
compileStatement stmt =
        case stmt of
            Print expr -> do
                k <- compileExpression expr
                k2 <- newName
                currentBlock %= append (k2 := G.Trunc
                                    (G.LocalReference G.i64 k)
                                    G.i32
                                    [])
                currentBlock %= append (G.Do
                                        (G.Call False G.C
                                        []
                                        (Right (G.ConstantOperand
                                                (C.GlobalReference
                                                (G.FunctionType G.i32 [G.i32] False)
                                                    (G.Name "putchar"))))
                                        [((G.LocalReference G.i32 k2),[])]
                                        []
                                        []))

            Assign str expr -> do
                k <- compileExpression expr
                var <- use $ (symbolTable.(symbol str))
                currentBlock %= append (G.Do
                                       (G.Store False
                                        (G.LocalReference
                                          (G.PointerType G.i64 (G.AddrSpace 0)) (var))
                                        (G.LocalReference G.i64 k)
                                        Nothing
                                        0
                                        []))


compile :: [Stmt] -> G.Module
compile code = G.defaultModule { G.moduleName = "attic"
                          , G.moduleDefinitions = [compileFunction "main" code, malloc, putchar]
                          }

compileFunction :: String -> [Stmt] -> G.Definition
compileFunction name code = G.GlobalDefinition (G.functionDefaults
                                               { G.basicBlocks = blocks
                                               , G.name = G.Name name
                                               , G.returnType = G.i32
                                               , G.parameters = ([], False)
                                               })
        where blocks = _blocks $ execState (compileBlockInfo code) (BlockInfo 0 [] M.empty emptyDList)


putchar :: G.Definition
putchar = G.GlobalDefinition (G.functionDefaults
                            { G.name = G.Name "putchar"
                            , G.returnType = G.i32
                            , G.parameters = ([G.Parameter G.i32 (G.Name "c") []], False)
                            })

malloc :: G.Definition
malloc = G.GlobalDefinition (G.functionDefaults
                            { G.name = G.Name "malloc"
                            , G.returnType = G.PointerType G.i64 (G.AddrSpace 0)
                            , G.parameters = ([G.Parameter G.i64 (G.Name "c") []], False)
                            })
