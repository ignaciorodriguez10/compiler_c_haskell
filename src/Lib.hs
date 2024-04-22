module Lib
    ( interpretar
    , emptyDataStore
    ) where

import Grammar

type MapValue = (String, Int)

emptyDataStore :: [MapValue]
emptyDataStore = []


-- Función principal para interpretar el programa
interpretar :: [Stmt] -> IO ()
interpretar stmts = do
    _ <- evalStatements stmts []
    return ()

-- Función para guardar un valor en el almacén de datos
saveVal :: MapValue -> [MapValue] -> [MapValue]
saveVal (var, val) store = (var, val) : filter (\(v, _) -> v /= var) store

-- Función para evaluar una lista de declaraciones
evalStatements :: [Stmt] -> [MapValue] -> IO [MapValue]
evalStatements [] store = return store
evalStatements (stmt:stmts) store = do
    newStore <- evalStatement stmt store
    evalStatements stmts newStore

-- Función para evaluar una declaración individual
evalStatement :: Stmt -> [MapValue] -> IO [MapValue]
evalStatement (AssignStmt assign) store = evalAssign assign store
evalStatement (PrintStmt printExp) store = evalPrint printExp store
evalStatement (WhileStmt whileLoop) store = evalWhileLoop whileLoop store
evalStatement (IfStmt ifExp) store = evalIfExp ifExp store

-- Función para evaluar una asignación
evalAssign :: Assign -> [MapValue] -> IO [MapValue]
evalAssign (Assign var exp) store = do
    let res = evalExp exp store
    return $ saveVal (var, res) store

-- Función para evaluar una expresión
evalExp :: Exp -> [MapValue] -> Int
evalExp (SymExp var) store =
    case lookup var store of
        Just val -> val
        Nothing  -> 0
evalExp (IntExp val) _ = val
evalExp (PlusExp exp1 exp2) store = evalExp exp1 store + evalExp exp2 store
evalExp (MinusExp exp1 exp2) store = evalExp exp1 store - evalExp exp2 store
evalExp (MultiplyExp exp1 exp2) store = evalExp exp1 store * evalExp exp2 store
evalExp (DivideExp exp1 exp2) store = div (evalExp exp1 store) (evalExp exp2 store)
evalExp (StringExp str) _ = error "No se pueden evaluar cadenas"


evalPrintCadena :: String -> IO Int
evalPrintCadena cadena = do
    putStrLn cadena
    return 0


-- Función para evaluar una impresión
evalPrint :: Print -> [MapValue] -> IO [MapValue]
evalPrint (Print exp) store = do
    putStrLn (show (evalExp exp store))
    return store

-- Función para evaluar un bucle while
evalWhileLoop :: WhileLoop -> [MapValue] -> IO [MapValue]
evalWhileLoop (WhileLoop cond stmts) store = do
    if evalCond cond store
        then do
            newStore <- evalStatements stmts store
            evalWhileLoop (WhileLoop cond stmts) newStore
        else return store

-- Función para evaluar una expresión if
evalIfExp :: IfExp -> [MapValue] -> IO [MapValue]
evalIfExp (IfExp cond stmts) store = do
    if evalCond cond store
        then evalStatements stmts store
        else return store

-- Función para evaluar una condición
evalCond :: Cond -> [MapValue] -> Bool
evalCond (LessCond exp1 exp2) store = evalExp exp1 store < evalExp exp2 store
evalCond (LessEqualCond exp1 exp2) store = evalExp exp1 store <= evalExp exp2 store
evalCond (GreaterCond exp1 exp2) store = evalExp exp1 store > evalExp exp2 store
evalCond (GreaterEqualCond exp1 exp2) store = evalExp exp1 store >= evalExp exp2 store
evalCond (EqualCond exp1 exp2) store = evalExp exp1 store == evalExp exp2 store
evalCond (AndCond cond1 cond2) store = evalCond cond1 store && evalCond cond2 store
evalCond (OrCond cond1 cond2) store = evalCond cond1 store || evalCond cond2 store
