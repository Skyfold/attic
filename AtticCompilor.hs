import Compiler
import Syntax
import Parser
import Token
import System.Environment
import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.Module
import Control.Monad.Except

compileStuff mod filename = withContext $ \c -> do
    runExceptT $ withModuleFromAST c mod $ \m -> do
        runExceptT $ withDefaultTargetMachine $ \t -> do
            runExceptT $ writeObjectToFile t filename m
                

main :: IO ()
main = do
        filename:_ <- getArgs
        program <- readFile filename
        return ()
        (compileStuff (compile $ atticParser $ alexScanTokens program) (File"out.o")) >>= print

