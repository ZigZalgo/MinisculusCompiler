module TypeChecking where
import SymbolTypes
import SymbolTableFunctions
import IntermediateRepresentation
import AST
{-
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

-}
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------STATEMENT TYPE CHECKING----------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
typeCheckStatements::[M_stmt] -> ST -> Bool
typeCheckStatements [] _ = True
typeCheckStatements (statement:rest) table =
    case typeCheckStatement statement table of
        True -> typeCheckStatements rest table
        False -> False

--Type checks individual statements
typeCheckStatement:: M_stmt -> ST -> Bool
--If the statement is an assignment, type check the expressions
typeCheckStatement (M_ass (name, expr)) table = 
    case symIDesc of
        I_VARIABLE (level, offset, vType) -> (vType == exprType)
        _ -> error "Cannot assign type "++exprTyp++" to variable "++name++" of type "++vType
    where
        symIDesc = lookup table name
        exprType = typeCheckExpression table expr

--If the statement is a while loop, type check the conditional, and the statements
typeCheckStatement (M_while (expr, stmt)) table =
    case exprType of 
        M_bval bool -> stmtChecked
        _ -> error "expression "++expr++" is not a boolean expression"
    where 
        exprType = typeCheckExpression table expr
        stmtChecked = typeCheckStatement stmt table

--If the statement is an if else, type check the conditional, and the statements
typeCheckStatement (M_cond (expr, stmt1, stmt2)) table =
    case exprType of
        M_val bool -> and (st1checked:st2checked:[])
        _ -> error "expression "++expr++" is not a boolean expression"
    where
        exprType = typeCheckExpression table expr
        st1Checked = typeCheckStatement stmt1 table
        st2Checked = typeCheckStatement stmt2 table

--If the statement is a read, type check the expressions
typeCheckStatement (M_read (name, expr)) table = 
    case symIDesc of
        I_VARIABLE (level, offset, vType) -> (vType == exprType)
        _ -> error "Variable "++name++" is not of type "++exprType
    where
        symIDesc = lookup table name
        exprType = typeCheckExpression table expr

--Just gotta make sure the expressions are type consistent
typeCheckStatement (M_print (expr)) table = 
    case typeCheckExpression table expr of
        _ -> True
typeCheckStatement (M_return (expr)) table =
    case typeCheckExpression table expr of
        _ -> True

--If the statement is a block, recursively check the statement
typeCheckStatement (M_block (decls, stmts)) table = typeCheckStatements stmts table

typeCheckStatement _ _= True

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
---------------------EXPRESSION TYPE CHECKING---------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
typeCheckExpressions::ST -> [M_expr] -> M_type
typeCheckExpressions table [] = error "This should not happen"
typeCheckExpressions table exprs = 
    case checkListType typeList of
        true -> typeList !! 0
        false -> error "Types in the expression list "++exprs++" do not match"
    where typeList = (map (typeCheckExpression table) exprs)

--Type checks individual expressions
typeCheckExpression:: ST -> M_expr -> M_type
typeCheckExpression table (M_ival int) = M_int
typeCheckExpression table (M_rval flt) = M_real
typeCheckExpression table (M_bval bool) = M_bool
typeCheckExpression table (M_id name) = 
    case symIDesc of
        I_VARIABLE _ -> error "Cannot call variable "++name++" like a function"
        I_FUNCTION (-> 
    where symIDesc = lookup table name
typeCheckExpression table (M_app (opn, exprList)) =
    case checkOpType opn esType of
        True -> esType
        False -> error "Operation "++opn++" cannot be applied to type "+esType
    where esType = typeCheckExpressions exprList

--Compares a constructor with an Operation and checks whether that 
--operation can be used on the constructor
checkOPType:: M_operation -> M_expr -> Bool
checkOPType M_add M_ival = 
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
checkOPType (M_fn str) (M_id str2)  = True
checkOPType _ _ = False


--Takes a list of expressions, and makes sure they all share the same constructor
checkListType::[M_type] -> Bool
checkListType [] = True
checkListType struct1:[] = True
checkListType struc1:struc2:rest = 
    case (sameConstructor struct1 struc2) of
        False -> False
        True -> checkListType (struct2:rest)

--Compares two expressions, and matches constructor type
sameConstructor::M_type -> M_type -> Bool
sameConstructor (M_int) (M_int) = True
sameConstructor (M_real) (M_real) = True
sameConstructor (M_bool) (M_bool) = True
sameConstructor _ _ = False