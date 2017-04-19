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

--------------------------------------PROG---------------------------------------------------
---------------------------------------------------------------------------------------------
handleM_Prog::M_Prog -> ST -> I_prog
handleM_Prog (M_Prog (decls, stmts)) table = 
    case doStmtExprsType of
        true -> IPROG (functionBodies, (len localVars), iStmts)
        false -> error "There was an error in type checking your statements"
    where
        localFuns = filter isFunctionDeclaration decls
        localVars = filter isVariableDeclaration decls
        (newTable, labelNum) = insert_Decls decls table
        --Using the newly genertaed State Table, we type check the declarations
        --THIS WILL ERROR OUT IF IT DOES NOT TYPE CHECK
        doStmtExprsType = typeCheckStatements stmts newTable
        --This produces an [I_fbody]
        functionBodies = handleI_fbodies localFuns newTable
        --This produces an [I_stmt]
        iStmts = handleI_stmts stmts newTable
--------------------------------------------------------------------------------------------


-------------------------------------FUNCTION-----------------------------------------------
--------------------------------------------------------------------------------------------
handleI_fbodies::[M_decl] -> ST -> [I_fbody]
handleI_fbodies [] _ = []
handleI_fbodies (fun1:rest) table = ((handleI_fbody fun1):(handleI_fbodies rest table))

handleI_fbody:: M_decl -> ST -> I_fbody
handleI_fbody decl table = 
    case decl of
        M_fun (name, args, retType, decls, stmts) -> 
            case (lookup table name) of
                I_FUNCTION (level, label, argTypes, retType) -> let
                    --[I_fbody]
                    innerF_Bodies =  handleI_fbodies decls table
                    --used to get number of local vars
                    vars = filter isVariableDeclaration decls
                    --[I_stmt]
                    innerI_stmts = handleI_stmts stmts table
                    --Int numbr of args
                    argSize = len argTypes
                    in I_fBody (label, innerF_Bodies, (len vars), argSize, innerI_stmts)
                _ -> error "Function insertion must have screwed up somewhere"
        _ -> error decl++" is not a function type"

handleI_stmts::[M_stmt] -> ST -> [I_stmt]
handleI_stmts [] _ = []
handleI_stmts (stmt1:rest) table = ((handleI_stmt stmt1):(handleI_stmts rest table))
-------------------------------------------------------------------------------------------


------------------------------STATEMENT----------------------------------------------------
-------------------------------------------------------------------------------------------
handleI_stmt::M_stmt -> ST -> I_stmt
--ASSIGN
handleI_stmt (M_ass (name, expr)) table = 
    case lookup table name of
        I_VARIABLE (level, offset, vtype) ->
            IASS (level, offset, iexpr)
            where 
                iexpr = handleI_expr expr table
        _ -> error "Function cannot be used as a statement"

--WHILE        
handleI_stmt (M_while (expr stmt)) table = 
    IWHILE (iexpr, istmt)
    where 
        iexpr = handleI_expr expr table
        istmt = handleI_stmt stmt table

--IF, ELSE, THEN
handleI_stmt (M_cond (expr, stmt1, stmt2)) table =
    ICOND (iexpr, istmt1, istmt2)
    where
        iexpr = handleI_expr expr table
        istmt1 = handleI_stmt stmt1 table
        istmt2 = handleI_stmt stmt2 table

--READ
handleI_stmt (M_read (name, expr)) table =
    case lookup table name of
        I_VARIABLE (level, offset, vtype) ->
            case vtype of
                M_int -> IREAD_I (level, offset, (handleI_expr expr table))
                M_bool -> IREAD_B (level, offset, (handleI_expr expr table))
                M_real -> IREAD_F (level, offset, (handleI_expr expr table))
        _ -> error "Function cannot be used as a statement"

--PRINT
handleI_stmt (M_print expr) table =
    case (typeCheckExpression table expr) of
        M_int -> IPRINT_I (handleI_expr expr table)
        M_bool -> IPRINT_B (handleI_expr expr table)
        M_real -> IPRINT_F (handleI_expr expr table)

--RETURN
handleI_stmt (M_return expr) table = IRETURN (handleI_expr expr table)

--BLOCK
handleI_stmt (M_block (decls, stmts)) table = let
    functions = filter isFunctionDeclaration decls
    vars = filter isVariableDeclaration decls
    innerF_Bodies = handleI_fbodies functions table
    innerI_stmts = handleI_stmts stmts table
    in IBLOCK (innerF_Bodies, (len vars), innerI_stmts)
--------------------------------------------------------------------------------------------


------------------------------EXPRESSION---------------------------------------------------
-------------------------------------------------------------------------------------------
handleI_exprs::[M_expr] -> ST -> [I_expr]
handleI_exprs [] _ = []
handleI_exprs (fun1:rest) table = ((handleI_expr fun1):(handleI_exprs rest table))

handleI_expr::M_expr -> ST -> I_expr
--INT
handleI_expr (M_ival int) = IINT int

--FLOAT
handleI_expr (M_rval float) = IREAL float

--BOOL
handleI_expr (M_bval bool) = IBOOL bool

--VAR IDENTIFIER
handleI_expr (M_id name) =
    case (lookup table name) of
        I_VARIABLE (level, offset, _) -> IID (level, offset)
        _ -> error "Function cannot be used as an id"

--EXPRESSION APPLICATION
--Here I pass in to OPN simply because we will need to find the type in opn anyway
handleI_expr (M_app (opn, exprs)) = let 
    iopn = handleI_opn opn exprs table
    iexprs = handleI_exprs exprs
    in IAPP (iopn, iexprs)
-------------------------------------------------------------------------------------------

------------------------------OPERATION----------------------------------------------------
-------------------------------------------------------------------------------------------
handleI_opn::M_operation -> [M_expr] -> ST ->  I_opn
handleI_opn (M_fn name) exprs table =
    case (lookup table name) of 
        I_FUNCTION (level, label, _, _) -> ICALL (label, level)
        _ -> error "cannot call a variable"
handleI_opn opn exprs table =
    case (typeCheckExpressions table exprs) of
        M_int -> case opn of 
            M_add -> IADD
            M_mul -> IMUL
            M_div -> IDIV
            M_sub -> ISUB
            M_neg -> INEG
            M_lt -> ILT
            M_le -> ILE
            M_gt -> IGT
            M_ge -> IGE
            M_eq -> IEQ
            M_float -> IFLOAT
            M_floor -> IFLOOR
            M_ceil ->  ICEIL
            _ -> error "M_INT should have already errored out"
        M_bool -> case opn of
            M_not -> INOT
            M_and -> IAND
            M_or -> IOR
            _ -> error "M_BOOL should have already errored out"
        M_real -> case opn of
            M_add -> IADD_F
            M_mul -> IMUL_F
            M_div -> IDIV_F
            M_sub -> ISUB_F
            M_neg -> INEG_F
            M_lt -> ILT_F
            M_le -> ILE_F
            M_gt -> IGT_F
            M_ge -> IGE_F
            M_eq -> IEQ_F
-------------------------------------------------------------------------------

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



insert_Decls::[M_decl] -> Int -> ST -> (Int, ST)
insert_Decls [] num table = table
insert_Decls (decl:rest) num table =
    case decl of
        M_var (name, exprs, vType) -> insert num table VARIABLE (name, vType)
        M_fun (name, args, retType, decls, stmts) -> 
            let newArgs = grabArgs args
            in insert num table 
    let (label, newTable) = insert FUNCTION (name, newArgs, retType)
    in insert_Decls rest label newTable


grabArgs::[(String, Int, M_type)] -> [(M_type, Int)]
grabArgs [] = []
grabArgs ((name, num, fType):rest) = (fType, num):(grabArgs rest)
