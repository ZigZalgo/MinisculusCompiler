module SymbolTableFunctions where
import SymbolTypes


newScope::ST->ST
newScope s = (Symbol_table (0,0,[])):s


lookup::ST -> String -> Either String SYM_I_DESC = 
   look_up s x = find 0 s where
      found level (Var_attr(offset,type,dim)) 
                    = Right (I_VARIABLE(level,offset,type,dim))
      found level (Fun_attr(label,arg_Type,type)) 
                    = Right I_FUNCTION(level,label,arg_Type,type))
      find_level ((str,v):rest)|x == str = Just v
                               |otherwise =  find_level rest
      find_level [] = Nothing
      find n [] = Left ("Could not find "++ str)
      find n (Symbol_table(_,_,vs)::rest) = 
             (case find_level vs of 
              Just v -> found n v
          Nothing -> find (n+1) rest)

insert:: Int -> ST -> SYM_DESC -> (Int,ST) = 
  insert n [] d =  error "Symbol table error: insertion before defining scope."
  insert n ((Symbol_table(nL,nA,sL)):rest) (ARGUMENT(str,t,dim)) 
           | (in_index_list str sL) = error
                ("Symbol table error: " ++ str ++"is already defined.")
           | otherwise = (n,Symbol_table(nL,nA+1
                             ,(str,Var_attr(~(nA+4),T,dim))::sL))
  insert n ((Symbol_table(nL,nA,sL)):rest) (VARIABLE (str,T,dim)) 
           | (in_index_list str sL) 
           = error ("Symbol table error: "++ str ++"is already defined.")
           | otherwise = (n,Symbol_table(nL+1,nA
                             ,(str,Var_attr(nL+1,T,dim))::sL))
  insert n ((Symbol_table(nL,nA,sL)):rest) FUNCTION (str,Ts,T)
           | in_index_list str sL 
           = error ("Symbol table error: "++str++"is already defined.")
           | otherwise = (n+1,
              (Symbol_table(nL,nA,(str,Fun_attr(getlabel n "fn",Ts,T)):sL)):rest)
    where in_index_list str [] = False
              in_index_list str ((x,_):xs)| str==x = True
                                          | otherwise = in_index_list str xs