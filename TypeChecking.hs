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
        Left error -> False
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


typeCheckStatements
