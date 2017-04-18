module ASTConverter where

-- AST converter using BNFC's skeleton.
-- Written by Cooper Davies 2017.

import AbsMinisculus
import qualified AST as A
import ErrM


----STRING STRIPPING----
transIdent :: Ident -> String
transIdent x = case x of

  Ident str  -> str


----MAIN PROG CALL----
transProg :: Prog -> A.M_Prog
transProg x = case x of

  PROG block  -> 
	let outBlock = transBlock block
	in A.M_Prog outBlock


----BLOCK TRANSFORMATION
transBlock :: Block -> ([A.M_decl], [A.M_stmt])
transBlock x = case x of

  BLOCK declarations program_body  -> 
    let decls = map transDeclaration declarations
        stmts = transProgram_body program_body
    in (decls, stmts)


----DECLARATION TRANSFORMA
transDeclaration :: Declaration -> A.M_decl
transDeclaration x = case x of

  DECVAR var_declaration  -> 
    transVar_declaration var_declaration

  DECFUN fun_declaration  -> 
    transFun_declaration fun_declaration


----VARIABLE DECLARATION FUNCTION----
transVar_declaration :: Var_declaration -> A.M_decl
transVar_declaration x = case x of

  VARDEC id array_dimensions newType ->
    let str = transIdent id
        m_exprs = transArray_dimensions array_dimensions
        m_type = transType newType
    in A.M_var (str, m_exprs, m_type)


----TYPE TRANSFORMATION----
transType :: Type -> A.M_type
transType x = case x of

  TVAR  -> A.M_int

  TREAL  -> A.M_real

  TBOOL  -> A.M_bool


----ARRAY_DIMENSION TRANSFORMATION----
transArray_dimensions :: Array_dimensions -> [A.M_expr]
transArray_dimensions x = case x of

  ADIM expr array_dimensions  -> 
    (transExpr expr) : (transArray_dimensions array_dimensions)

  ADIMNULL  -> []


----FUNCTION_DECLARATION TRANSFORMATION----
transFun_declaration :: Fun_declaration -> A.M_decl
transFun_declaration x = case x of

  FUNDEC id param_list newType fun_block  -> 
    let 
        str =  transIdent id
        params = transParam_list param_list
        t = transType newType
        (decl, stmtl) = transFun_block fun_block
    in A.M_fun (str, params, t, decl, stmtl)


----FUNCTION_BLOCK TRANSFORMATION----
transFun_block :: Fun_block -> ([A.M_decl],[A.M_stmt])
transFun_block x = case x of

  FUNBLOCK declarations fun_body  -> 
    let decl = map transDeclaration declarations
        stmt = transFun_body fun_body
    in (decl, stmt)


----PARAMETER_LIST TRANSFORMATION----
transParam_list :: Param_list -> [(String, Int, A.M_type)]
transParam_list x = case x of

  PLIST basic_declarations  -> 
    map transBasic_declaration basic_declarations


----BASIC_DECLARATION TRANSFORMATION----
transBasic_declaration :: Basic_declaration -> (String, Int, A.M_type)
transBasic_declaration x = case x of

  BASICDECL id basic_array_dimensions newType  -> 
    let str = transIdent id
        dim = transBasic_array_dimensions basic_array_dimensions
        ttype = transType newType
    in (str, dim, ttype)


----array[X] TRANSFORMATION---- 
transBasic_array_dimensions :: Basic_array_dimensions -> Int
transBasic_array_dimensions x = case x of

  BASICADIM basic_array_dimensions  -> 
    1 + (transBasic_array_dimensions basic_array_dimensions)

  BASICADIMNULL  -> 0


----PROGRAM_BODY TRANSFORMATION----
transProgram_body :: Program_body -> [A.M_stmt]
transProgram_body x = case x of

  PBODY prog_stmts  -> 
    map transProg_stmt prog_stmts


----FUNCTION_BODY TRANSFORMATION----
transFun_body :: Fun_body -> [A.M_stmt]
transFun_body x = case x of

  FBODY prog_stmts expr  -> 
    let 
        front = map transProg_stmt prog_stmts 
        newExpr = transExpr expr
        ending = A.M_return newExpr
        ls = front ++ [ending]    --We need to add the return stmt to the end of our stmt list
    in ls


----PROGRAM_STATEMTN TRANSFORMATION
transProg_stmt :: Prog_stmt -> A.M_stmt
transProg_stmt x = case x of

  IFTHENELSE expr prog_stmt1 prog_stmt2  -> 
    let ex = transExpr expr
        ps1 = transProg_stmt prog_stmt1
        ps2 = transProg_stmt prog_stmt2
    in A.M_cond (ex, ps1, ps2)

  WHILE expr prog_stmt  -> 
    let ex = transExpr expr
        prog = transProg_stmt prog_stmt
    in A.M_while (ex,  prog)

  READ identifier  -> 
	let iden = transIdentifier identifier
	in A.M_read iden

  ASSIGN identifier expr  -> 
    let (str, exprls) = transIdentifier identifier
        newExpr = transExpr expr
    in A.M_ass (str, exprls, newExpr)

  PRINT expr  -> 
	let newExpr = transExpr expr
	in A.M_print newExpr

  BLOCKSTMT block  -> 
	let newBlocks = transBlock block
	in A.M_block newBlocks


----IDENTIDIER TRANSFORMATION----
-- |Not to be confused with string stripping
transIdentifier :: Identifier -> (String, [A.M_expr])
transIdentifier x = case x of

  EIDENT id array_dimensions  -> 
    let str = transIdent id
        adim = transArray_dimensions array_dimensions
    in (str, adim)


----EXPRESSION TRANSFORMATION
transExpr :: Expr -> A.M_expr
transExpr x = case x of

  OR expr bint_term  -> 
    let orApp = A.M_or
        newExp1 = transExpr expr
        newExp2 = transBint_term bint_term
        ls = newExp1:newExp2:[]
    in A.M_app (orApp, ls)

  EXPR bint_term  ->
    transBint_term bint_term


----B_INT_TERM TRANSFORMATION
transBint_term :: Bint_term -> A.M_expr
transBint_term x = case x of

  AND bint_term bint_factor  -> 
    let and' = A.M_and
        exp1 = transBint_term bint_term
        exp2 = transBint_factor bint_factor
        ls = exp1:exp2:[]
    in A.M_app (and', ls)

  BINTERM bint_factor  -> 
    transBint_factor bint_factor


---B_INT_FACTOR TRANSFORMATION----
transBint_factor :: Bint_factor -> A.M_expr
transBint_factor x = case x of

  NOT bint_factor  -> 
    let not' = A.M_not
        expr = transBint_factor bint_factor
        ls = expr:[]
    in A.M_app (not', ls)

  COMP int_expr1 compare_op2 int_expr3  ->
    let op = transCompare_op compare_op2
        expr1 = transInt_expr int_expr1
        expr2 = transInt_expr int_expr3
        ls = expr1:expr2:[]
    in A.M_app (op, ls)

  BINTFACTOR int_expr  -> 
    transInt_expr int_expr


----COMPARISON TRANSFORMATION----
transCompare_op :: Compare_op -> A.M_operation
transCompare_op x = case x of

  EQUALS  -> A.M_eq

  LESST  -> A.M_lt

  GREATT  -> A.M_gt
  
  LTE  -> A.M_le
  
  GTE  -> A.M_ge


----INT_EXPRESSION TRANSFORMATION----
transInt_expr :: Int_expr -> A.M_expr
transInt_expr x = case x of

  IEXPRTOADDOP int_expr addop int_term  -> 
    let op = transAddop addop
        expr1 = transInt_expr int_expr
        expr2 = transInt_term int_term
        ls = expr1:expr2:[]
    in A.M_app (op, ls)

  INTEXPR int_term  -> 
    transInt_term int_term


----ADDOP TRANSOFRMATION----
transAddop :: Addop -> A.M_operation
transAddop x = case x of

  ADD  -> A.M_add

  SUB  -> A.M_sub


----INT_TERM TRANSFORMATION----
transInt_term :: Int_term -> A.M_expr
transInt_term x = case x of

  ITERMTOMULOP int_term mulop int_factor  -> 
    let op = transMulop mulop
        expr1 = transInt_term int_term
        expr2 = transInt_factor int_factor
        ls = expr1:expr2:[]
    in A.M_app (op, ls)

  INTERM int_factor  -> 
    transInt_factor int_factor


----MULOP TRANSFORMATION----
transMulop :: Mulop -> A.M_operation
transMulop x = case x of

  MUL  -> A.M_mul

  DIV  -> A.M_div


----INT_FACTOR TRANSFORMATION----
transInt_factor :: Int_factor -> A.M_expr
transInt_factor x = case x of

  IFEXPR expr  -> transExpr expr

  SIZE id basic_array_dimensions  -> 
    let str = transIdent id
        dim = transBasic_array_dimensions basic_array_dimensions
    in A.M_size (str, dim)

  FLOAT expr  -> 
    let op = A.M_float
        expr1 = transExpr expr
        ls = expr1:[]
    in A.M_app (op,ls)

  FLOOR expr  -> 
    let op = A.M_floor
        expr1 = transExpr expr
        ls = expr1:[]
    in A.M_app (op,ls)

  CEIL expr  -> 
    let op = A.M_ceil
        expr1 = transExpr expr
        ls = expr1:[]
    in A.M_app (op,ls)

  INTFACTORMODLIST id modifier_list  -> 
    let op = A.M_fn (transIdent id)
        ls = transModifier_list modifier_list
    in A.M_app (op, ls)

  INTEGER n  -> A.M_ival n

  DBL d  -> let newVal = realToFrac d
			in A.M_rval newVal

  FALSE  -> A.M_bval False

  TRUE  -> A.M_bval True

  MINUS int_factor  -> 
    let op = A.M_neg
        expls = transInt_factor int_factor
        ls = expls:[]
    in A.M_app (op,ls)


----MODIFIER_LIST TRANSFORMATION----
transModifier_list :: Modifier_list -> [A.M_expr]
transModifier_list x = case x of

  MODIFIERARGS exprs  -> 
    map transExpr exprs

  MODIFIERLIST array_dimensions  -> 
    transArray_dimensions array_dimensions
 
