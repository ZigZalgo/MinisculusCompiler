module ProgHandle where
import SymbolTypes
import SymbolTableFunctions
import AST
import IntermediateRepresentation

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------INTERMEDIATE REPRESENTATION BUILDER-------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

handleM_Prog::M_Prog -> ST -> I_prog
handleM_Prog (M_Prog (decls, stmts)) table = do
    let
        localFuns = filter isFunctionDeclaration decls
        localVars = filter isVariableDeclaration decls
        (newTable, labelNum) = insert_Decls decls table
        --Using the newly genertaed State Table, we type check the declarations

        --THIS WILL ERROR OUT IF IT DOES NOT TYPE CHECK
        doDeclsType = typeCheckDeclarations decls newTable
        doStmtExprsType = typeCheckStatements stmts newTable
        --This produces an [I_fbody]
        functionBodies = handleF_Bodies localFuns newTable labelNum
        --This produces an [I_stmt]
        iStmts = handleIStmtms stmts newTable labelNum
            in IPROG (functionBodies, (len localVars), /**HOW DO I GET THIS**/, iStmts)

handleF_Bodies::[M_decl] -> ST -> [I_fbody]
handleF_Bodies [] _ = []
handleF_Bodies (fun1:rest) table = ((handleF_Body fun1):(handleF_Bodies rest table))

handleF_Body:: M_decl -> ST -> I_fbody




-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------HELPER FUNCTIONS--------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
        
--is used by filter to see if a declaration is a function or a var
isFunctionDeclaration:: M_decl -> Bool
isFunctionDeclaration M_fun (_) = True
isFunctionDeclaration _ = False
--is used by filter to see if a declaration is a function or a var
isVariableDeclaration::M_decl -> Bool
isVariableDeclaration M_var (_) = True
isVariableDeclaration _ = False



--vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv--
--[[[[[[[I NEED HELP WITH THIS PART HERE]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]--
--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^--
insert_Decls::[M_decl] -> ST -> (Int, ST)
insert_Decls [] _ = []
insert_Decls (decl:rest) ref =
    let (label, newTable) = insert_Decl decl ref
    in insert_Decls rest newTable

insert_Decl:: M_decl -> -> (Int, ST)
insert_Decl (M_var (name, arrays, type)) ref = insert n ref VARIABLE  
insert_Decl M_fun ref = 


