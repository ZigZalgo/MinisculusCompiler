module IntermediateRepresentation where

data I_prog  = IPROG    ([I_fbody],Int,[I_stmt])
    -- a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (d) the body: a list of statements
data I_fbody = IFUN     (String,[I_fbody],Int,Int,[I_stmt])
    -- a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (d) the body: a list of statements
data I_stmt = IASS      (Int,Int,I_expr)
                 -- and assignment has argments (<level>,<offset>,expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | IREAD_F   (Int,Int,[I_expr])
            | IREAD_I   (Int,Int,[I_expr])
            | IREAD_B   (Int,Int,[I_expr])
            | IPRINT_F  I_expr
            | IPRINT_I  I_expr
            | IPRINT_B  I_expr
            | IRETURN   I_expr
            | IBLOCK    ([I_fbody],Int,[I_stmt])
             -- a block consists of 
         -- (a) a list of local functions
         -- (b) the number of local varibles declared
         -- (d) the body: a lst of statements
data I_expr = IINT      Int
            | IREAL     Float
            | IBOOL     Bool
            | IID       (Int,Int)   
             --  identifier (<level>,<offset>)
            | IAPP      (I_opn,[I_expr])
data I_opn = ICALL      (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   -- operations for floats
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR;
