module ProgHandle where
import SymbolTypes
import SymbolTableFunctions
import AST
import IntermediateRepresentation

handleM_Prog::M_Prog -> ST -> Either (String I_prog)
handleM_Prog (M_Prog (decls, stmts)) table = do
    let
        localFuns = filter isFunctionDeclaration decls
        localVars = filter isVariableDeclaration decls
        (newTable, labelNum) = insert_Decls decls table
        --Using the newly genertaed State Table, we type check the declarations
        doDeclsType = typeCheckDeclarations decls newTable
        doStmtExprsType = typeCheckStatements stmts newTable
        --This produces an [I_fbody]
        functionBodies = handleFBody localFuns newTable labelNum
        iStmts = handleIStmtms stmts newTable labelNum
        case doDeclsType of
            Left error -> putstrln error
            Right


--is used by filter to see if a declaration is a function or a var
isFunctionDeclaration:: M_decl -> Bool
isFunctionDeclaration M_fun (_) = True
isFunctionDeclaration _ = False
--is used by filter to see if a declaration is a function or a var
isVariableDeclaration::M_decl -> Bool
isVariableDeclaration M_var (_) = True
isVariableDeclaration _ = False


insert_Decls::[M_decl] -> ST -> (Int, ST)
insert_Decls [] _ = []
insert_Decls (decl:rest) ref =
    let (label, newTable) = insert_Decl decl ref
    in insert_Decls rest newTable

insert_Decl:: M_decl -> -> (Int, ST)
insert_Decl (M_var (name, arrays, type)) ref = insert n ref VARIABLE  
insert_Decl M_fun ref = 


typeCheckDeclarations::[M_decl] -> ST -> Either String SYM_I_DESC
typeCheckDeclarations (decl:decls) ref = case typeCheckDelaration decl ref of
    Left error -> Left error
    Right _ -> typeCheckDeclarations decls ST


typeCheckDeclaration::M_decl -> ST -> Either String SYM_I_DESC
typeCheckDelaration (M_var (name, exprs, varType)) ref = case typrCheckExpression exprs ref of
    Left error -> Left error
    Right _ -> case lookup ref name of
        Left error -> error
        Right desc -> desc





checkExpr::M_expr -> ST -> Either String I_expr
checkOP (M_ival num) ref = 

