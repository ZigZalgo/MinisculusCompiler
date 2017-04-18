module SymbolTypes where

data SYM_DESC = 
      ARGUMENT (String,M_type,Int)
    | VARIABLE (String,M_type,Int)
    | FUNCTION (String,[(M_type,Int)],M_type)

data SYM_I_DESC = 
      I_VARIABLE (Int,Int,M_type,Int)
    | I_FUNCTION (Int,String,[(M_type,Int)],M_type)

data SYM_VALUE = 
      Var_attr (Int,M_type,Int)
    | Fun_attr (String,[(M_type,Int)],M_type)

data SYM_TABLE = 
    Symbol_table (Int,Int,[(String,SYM_VALUE)])

type ST = [ SYM_TABLE]


