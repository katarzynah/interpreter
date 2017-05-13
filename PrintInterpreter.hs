{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintInterpreter where

-- pretty-printer generated by the BNF converter

import AbsInterpreter
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Prog programheader declarations compoundstatement -> prPrec i 0 (concatD [prt 0 programheader, prt 0 declarations, prt 0 compoundstatement, doc (showString ".")])

instance Print ProgramHeader where
  prt i e = case e of
    ProgHead id -> prPrec i 0 (concatD [doc (showString "program"), prt 0 id, doc (showString ";")])

instance Print Declarations where
  prt i e = case e of
    Dec variabledeclarations proceduredeclarations -> prPrec i 0 (concatD [prt 0 variabledeclarations, prt 0 proceduredeclarations])

instance Print VariableDeclarations where
  prt i e = case e of
    VarDecEmpty -> prPrec i 0 (concatD [])
    VarDecFull variabledeclarationlist -> prPrec i 0 (concatD [doc (showString "var"), prt 0 variabledeclarationlist])

instance Print VariableDeclarationList where
  prt i e = case e of
    VarDecListEnd vardec -> prPrec i 0 (concatD [prt 0 vardec])
    VarDecList vardec variabledeclarationlist -> prPrec i 0 (concatD [prt 0 vardec, prt 0 variabledeclarationlist])

instance Print VarDec where
  prt i e = case e of
    VarDecLabel idlist typespecifier -> prPrec i 0 (concatD [prt 0 idlist, doc (showString ":"), prt 0 typespecifier, doc (showString ";")])

instance Print ProcedureDeclarations where
  prt i e = case e of
    ProcDecEmpty -> prPrec i 0 (concatD [])
    ProcDecLabel procdec proceduredeclarations -> prPrec i 0 (concatD [prt 0 procdec, prt 0 proceduredeclarations])

instance Print ProcDec where
  prt i e = case e of
    ProcDecProc procheader declarations compoundstatement -> prPrec i 0 (concatD [prt 0 procheader, prt 0 declarations, prt 0 compoundstatement, doc (showString ";")])
    ProcDecFun funcheader declarations compoundstatement -> prPrec i 0 (concatD [prt 0 funcheader, prt 0 declarations, prt 0 compoundstatement, doc (showString ";")])

instance Print ProcHeader where
  prt i e = case e of
    ProcHead id arguments -> prPrec i 0 (concatD [doc (showString "procedure"), prt 0 id, prt 0 arguments, doc (showString ";")])

instance Print FuncHeader where
  prt i e = case e of
    FunHead id arguments typespecifier -> prPrec i 0 (concatD [doc (showString "function"), prt 0 id, prt 0 arguments, doc (showString ":"), prt 0 typespecifier, doc (showString ";")])

instance Print Arguments where
  prt i e = case e of
    Args argumentlist -> prPrec i 0 (concatD [doc (showString "("), prt 0 argumentlist, doc (showString ")")])

instance Print ArgumentList where
  prt i e = case e of
    ArgListEmpty -> prPrec i 0 (concatD [])
    ArgList arg argumentlist -> prPrec i 0 (concatD [prt 0 arg, doc (showString ";"), prt 0 argumentlist])

instance Print Arg where
  prt i e = case e of
    ArgLabel idlist typespecifier -> prPrec i 0 (concatD [prt 0 idlist, doc (showString ":"), prt 0 typespecifier])

instance Print CompoundStatement where
  prt i e = case e of
    CompStmnt statementlist -> prPrec i 0 (concatD [doc (showString "begin"), prt 0 statementlist, doc (showString "end")])

instance Print StatementList where
  prt i e = case e of
    StmntListEmpty -> prPrec i 0 (concatD [])
    StmntList statement statementlist -> prPrec i 0 (concatD [prt 0 statement, doc (showString ";"), prt 0 statementlist])

instance Print Statement where
  prt i e = case e of
    SEmpty -> prPrec i 0 (concatD [])
    SComp compoundstatement -> prPrec i 0 (concatD [prt 0 compoundstatement])
    SAss assignmentstatement -> prPrec i 0 (concatD [prt 0 assignmentstatement])
    SProc procedurecall -> prPrec i 0 (concatD [prt 0 procedurecall])
    SFor forstatement -> prPrec i 0 (concatD [prt 0 forstatement])
    SWhile whilestatement -> prPrec i 0 (concatD [prt 0 whilestatement])
    SIf ifstatement -> prPrec i 0 (concatD [prt 0 ifstatement])
    SPrint printstatement -> prPrec i 0 (concatD [prt 0 printstatement])

instance Print AssignmentStatement where
  prt i e = case e of
    AssStmnt id expression -> prPrec i 0 (concatD [prt 0 id, doc (showString ":="), prt 0 expression])
    AssStmntArr id expressionlist expression -> prPrec i 0 (concatD [prt 0 id, doc (showString "["), prt 0 expressionlist, doc (showString "]"), doc (showString ":="), prt 0 expression])

instance Print ProcedureCall where
  prt i e = case e of
    ProcCall id actuals -> prPrec i 0 (concatD [prt 0 id, prt 0 actuals])

instance Print ForStatement where
  prt i e = case e of
    ForStmnt id expression1 expression2 statement -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id, doc (showString ":="), prt 0 expression1, doc (showString "to"), prt 0 expression2, doc (showString "do"), prt 0 statement])

instance Print WhileStatement where
  prt i e = case e of
    WhileStmnt expression statement -> prPrec i 0 (concatD [doc (showString "while"), prt 0 expression, doc (showString "do"), prt 0 statement])

instance Print IfStatement where
  prt i e = case e of
    IfStmnt expression statement -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expression, doc (showString "then"), prt 0 statement, doc (showString "endif")])
    IfStmntWithElse expression statement1 statement2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expression, doc (showString "then"), prt 0 statement1, doc (showString "else"), prt 0 statement2, doc (showString "endif")])

instance Print PrintStatement where
  prt i e = case e of
    PrintStmnt expression -> prPrec i 0 (concatD [doc (showString "print"), prt 0 expression])

instance Print Expression where
  prt i e = case e of
    ExpSimple simpleexpression -> prPrec i 0 (concatD [prt 0 simpleexpression])
    ExpEqual simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString "="), prt 0 simpleexpression2])
    ExpNotEqual simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString "<>"), prt 0 simpleexpression2])
    ExpLess simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString "<"), prt 0 simpleexpression2])
    ExpLessOrEqual simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString "<="), prt 0 simpleexpression2])
    ExpGreater simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString ">"), prt 0 simpleexpression2])
    ExpGreaterOrEqual simpleexpression1 simpleexpression2 -> prPrec i 0 (concatD [prt 0 simpleexpression1, doc (showString ">="), prt 0 simpleexpression2])

instance Print SimpleExpression where
  prt i e = case e of
    SimpleExpTerm term -> prPrec i 0 (concatD [prt 0 term])
    SimpleExpAdd simpleexpression term -> prPrec i 0 (concatD [prt 0 simpleexpression, doc (showString "+"), prt 0 term])
    SimpleExpSubst simpleexpression term -> prPrec i 0 (concatD [prt 0 simpleexpression, doc (showString "-"), prt 0 term])

instance Print Term where
  prt i e = case e of
    TermFactor factor -> prPrec i 0 (concatD [prt 0 factor])
    TermMultiply term factor -> prPrec i 0 (concatD [prt 0 term, doc (showString "*"), prt 0 factor])
    TermDivide term factor -> prPrec i 0 (concatD [prt 0 term, doc (showString "/"), prt 0 factor])

instance Print Factor where
  prt i e = case e of
    FactorExpression expression -> prPrec i 0 (concatD [doc (showString "("), prt 0 expression, doc (showString ")")])
    FactorPlus factor -> prPrec i 0 (concatD [doc (showString "+"), prt 0 factor])
    FactorMinus factor -> prPrec i 0 (concatD [doc (showString "-"), prt 0 factor])
    FactorFunctionCall functioncall -> prPrec i 0 (concatD [prt 0 functioncall])
    FactorConstant constant -> prPrec i 0 (concatD [prt 0 constant])
    FactorIdent id -> prPrec i 0 (concatD [prt 0 id])
    FactorArray id expressionlist -> prPrec i 0 (concatD [prt 0 id, doc (showString "["), prt 0 expressionlist, doc (showString "]")])
    FactorStoI expression -> prPrec i 0 (concatD [doc (showString "string_to_int"), doc (showString "("), prt 0 expression, doc (showString ")")])
    FactorItoS expression -> prPrec i 0 (concatD [doc (showString "int_to_string"), doc (showString "("), prt 0 expression, doc (showString ")")])

instance Print FunctionCall where
  prt i e = case e of
    FunsCall id actuals -> prPrec i 0 (concatD [prt 0 id, prt 0 actuals])

instance Print Actuals where
  prt i e = case e of
    Act expressionlist -> prPrec i 0 (concatD [doc (showString "("), prt 0 expressionlist, doc (showString ")")])

instance Print ExpressionList where
  prt i e = case e of
    ExpListEmpty -> prPrec i 0 (concatD [])
    ExpListOne expression -> prPrec i 0 (concatD [prt 0 expression])
    ExpList expression expressionlist -> prPrec i 0 (concatD [prt 0 expression, doc (showString ","), prt 0 expressionlist])

instance Print IdList where
  prt i e = case e of
    IdLEnd id -> prPrec i 0 (concatD [prt 0 id])
    IdL id idlist -> prPrec i 0 (concatD [prt 0 id, doc (showString ","), prt 0 idlist])

instance Print TypeSpecifier where
  prt i e = case e of
    TypeSpecInt -> prPrec i 0 (concatD [doc (showString "int")])
    TypeSpecBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TypeSpecString -> prPrec i 0 (concatD [doc (showString "string")])
    TypeSpecArray dimensionlist typespecifier -> prPrec i 0 (concatD [doc (showString "array"), doc (showString "["), prt 0 dimensionlist, doc (showString "]"), doc (showString "of"), prt 0 typespecifier])

instance Print DimensionList where
  prt i e = case e of
    DimListEnd n -> prPrec i 0 (concatD [prt 0 n])
    DimList n dimensionlist -> prPrec i 0 (concatD [prt 0 n, doc (showString ","), prt 0 dimensionlist])

instance Print Constant where
  prt i e = case e of
    ConstInt n -> prPrec i 0 (concatD [prt 0 n])
    ConstBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    ConstString str -> prPrec i 0 (concatD [prt 0 str])

instance Print Boolean where
  prt i e = case e of
    BoolTrue -> prPrec i 0 (concatD [doc (showString "true")])
    BoolFalse -> prPrec i 0 (concatD [doc (showString "false")])


