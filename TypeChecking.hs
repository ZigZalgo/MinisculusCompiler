module TypeChecking where
import SymbolTypes
import SymbolTableFunctions
import IntermediateRepresentation
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------DECLARATION TYPE CHECKING--------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--recursivley checks the list of declarations against their defined types in
--the state table
typeCheckDeclarations:: [M_decl] -> ST -> Bool
typeCheckDeclarations [] _ -> True
typeCheckDeclarations (decl:rest) table =
    case typeCheckDeclaration decl table of
        False -> False
        True -> typeCheckDeclarations rest table

--If the declaration is a variable, we simply compare the type
--to that in the table
typeCheckDeclaration:: M_decl -> ST -> Bool
typeCheckDeclaration (M_var (name, _, vType)) table = 
    case lookup table name of
        Left errorMsg -> False
        Right (I_VARIALBE (level, offset, lType, dim)) -> lType == vType

--If the declaration is a function, we first see if it exists in the lookup table
--if so, we type check the return type
--then we type check the argument typs
--then the defined declarations
--finally the statements
--Anding these together will tell us if this type checks
typeCheckDeclaration (M_fun (name, args, fType, decls, stmts)) = 
    case lookup table name of
        Left error -> False
        Right (I_FUNCTION (level, label, lArgs, lType)) ->  let 
            retTypeCheck = (lType == fType) --Make sure the return types check
            argTypeCheck = case len (lArgs == len args) of --Easy check if the args are of the same size
                True -> typeCheckArgs args largs--Make sure the arguments types check
                False -> False
            declsTypeCheck = typeCheckDeclarations decls table --Make sure the declarations type check
            stmtsTypeCheck = typeCheckStatements stmts table --Make sure the statements type check
            booleanList = (retTypeCheck : argTypeCheck : declsTypeCheck : stmtsTypeCheck : []) --List cons
            in (and booleanList) --Anding for result

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------ARGUMENT TYPE CHECKING-----------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------


--Using the list of lookup args, and defined args, type checks by comparing them
typeCheckArgs:: [(String, Int, M_type)] -> [(M_type, Int)] -> Bool
typeCheckArgs [] [] = True
typeCheckArgs (fArgs:restF) (lArgs:restL) = 
    case typeCheckArg fArgs lArgs of
        True -> typeCheckArgs restF restL
        False -> False

--Given a single arg and its lookup, type checks it.
typeCheckArgs:: (String, Int, M_type) -> (M_type, Int) -> Bool
typeCheckArgs (_,_,fType) (lType, _) = (ftype == lType)


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------STATEMENT TYPE CHECKING----------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
typeCheckStatements::[M_stmt] -> Bool
typeCheckStatements [] _ = True
typeCheckStatements (statement:rest) =
    case typeCheckStatement statement of
        True -> typeCheckStatements rest
        False -> False

--Type checks individual statements
typeCheckStatement:: M_stmt -> Bool
--If the statement is an assignment, type check the expressions
typeCheckStatement (M_ass (name, exprs, expr)) = let
    exprChecked = typeCheckExpression expr
    exprsChecked = typeCheckExpressions exprs
    in and (exprChecked : exprsChecked : [])
--If the statement is a while loop, type check the conditional, and the statements
typeCheckStatement (M_while (expr, stmt)) = let
    exprChecked = typeCheckExpression expr
    stmtChecked = typeCheckStatement stmt
    in and (exprChecked:stmtChecked:[])
--If the statement is an if else, type check the conditional, and the statements
typeCheckStatement (M_cond (expr, stmt1, stmt2)) = let
    exprChecked = typeCheckExpression expr
    st1Checked = typeCheckStatement stmt1
    st2Checked = typeCheckStatement stmt2
    in and (exprChecked:st1Checked:st2Checked:[])
--If the statement is a read, type check the expressions
typeCheckStatement (M_read (name, exprs)) = typeCheckExpressions exprs
--If the statement is a print, type check the expression
typeCheckStatement (M_print expr) = typeCheckExpression expr
--If the statement is a return call, type check the returned expression
typeCheckStatement (M_return expr) = typeCheckExpression expr
--If the statement is a block, recursively check the statements, and check the declarations
typeCheckStatement (M_block (decls, stmts)) = let
    declsChecked = typeCheckDeclarations decls
    stmtsChecked = typeCheckStatements stmts
    in and (declsChecked:stmtsChecked:[])

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------EXPRESSION TYPE CHECKING---------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

--Checking expressions done recursively
typeCheckExpressions:: [M_expr] -> ST -> Bool
typeCheckExpressions [] _ = True
typeCheckExpressions (expr:rest) =
        case typeCheckExpression expr of
            True -> typeCheckExpressions rest
            False -> False

--Type checks individual expressions
typeCheckExpression:: M_expr -> Bool
typeCheckExpression (M_id (name, exprs)) = typeCheckExpressions exprs
typeCheckExpression (M_app (opn, (expr:exprs)) = 
        case checkListType (expr:rest) of --If all the expressions are the same constructor
            False -> False
            --Make sure this opn can be used on this type
            True -> checkOPType opn expr
typeCheckExpression _ = True

--Compares a constructor with an Operation and checks whether that 
--operation can be used on the constructor
checkOPType:: M_operation -> M_expr -> Bool
checkOPType M_add M_ival = True
checkOPType M_add M_rval = True
checkOPType M_mul M_ival = True
checkOPType M_mul M_rval = True
checkOpType M_sub M_ival = True
checkOPType M_sub M_rval = True
checkOPType M_div M_ival = True
checkOPType M_div M_rval = True
checkOPType M_neg M_ival = True
checkOPType M_neg M_rval = True
checkOpType M_lt M_ival = True
checkOPType M_lt M_rval = True
checkOPType M_le M_ival = True
checkOPType M_le M_rval = True
checkOpType M_gt M_ival = True
checkOPType M_gt M_rval = True
checkOPType M_ge M_ival = True
checkOPType M_ge M_rval = True
checkOPType M_eq _ = True
checkOPType M_not M_bval = True
checkOPType M_and M_bval = True
checkOPType M_or M_bval = True
checkOPType M_float M_rval = True
checkOPType M_floor M_rval = True
checkOPType M_ceil M_rval = True
checkOPType _ _ = False


--Takes a list of expressions, and makes sure they all share the same constructor
checkListType::[M_expr] -> Bool
checkListType [] = True
checkListType struct1:[] = True
checkListType struc1:struc2:rest = 
    case (sameConstructor struct1 struc2) of
        False -> False
        True -> checkListType (struct2:rest)

--Compares two expressions, and matches constructor type
sameConstructor::M_expr -> M_expr -> Bool
sameConstructor (M_ival _) (M_ival _) = True
sameConstructor (M_rval _) (M_rval _) = True
sameConstructor (M_bval _) (M_bval _) = True
sameConstructor _ _ = False