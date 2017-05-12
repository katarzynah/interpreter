

module AbsInterpreter where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)

data Program = Prog ProgramHeader Declarations CompoundStatement
  deriving (Eq, Ord, Show, Read)

data ProgramHeader = ProgHead Ident
  deriving (Eq, Ord, Show, Read)

data Declarations = Dec VariableDeclarations ProcedureDeclarations
  deriving (Eq, Ord, Show, Read)

data VariableDeclarations
    = VarDecEmpty | VarDecFull VariableDeclarationList
  deriving (Eq, Ord, Show, Read)

data VariableDeclarationList
    = VarDecListEnd VarDec | VarDecList VarDec VariableDeclarationList
  deriving (Eq, Ord, Show, Read)

data VarDec = VarDecLabel IdList TypeSpecifier
  deriving (Eq, Ord, Show, Read)

data ProcedureDeclarations
    = ProcDecEmpty | ProcDecLabel ProcDec ProcedureDeclarations
  deriving (Eq, Ord, Show, Read)

data ProcDec
    = ProcDecProc ProcHeader VariableDeclarations CompoundStatement
    | ProcDecFun FuncHeader VariableDeclarations CompoundStatement
  deriving (Eq, Ord, Show, Read)

data ProcHeader = ProcHead Ident Arguments
  deriving (Eq, Ord, Show, Read)

data FuncHeader = FunHead Ident Arguments TypeSpecifier
  deriving (Eq, Ord, Show, Read)

data Arguments = Args ArgumentList
  deriving (Eq, Ord, Show, Read)

data ArgumentList = ArgListEmpty | ArgList Arg ArgumentList
  deriving (Eq, Ord, Show, Read)

data Arg = ArgLabel IdList TypeSpecifier
  deriving (Eq, Ord, Show, Read)

data CompoundStatement = CompStmnt StatementList
  deriving (Eq, Ord, Show, Read)

data StatementList
    = StmntListEmpty | StmntList Statement StatementList
  deriving (Eq, Ord, Show, Read)

data Statement
    = SEmpty
    | SComp CompoundStatement
    | SAss AssignmentStatement
    | SProc ProcedureCall
    | SFor ForStatement
    | SWhile WhileStatement
    | SIf IfStatement
    | SPrint PrintStatement
  deriving (Eq, Ord, Show, Read)

data AssignmentStatement = AssStmnt Ident Expression
  deriving (Eq, Ord, Show, Read)

data ProcedureCall = ProcCall Ident Actuals
  deriving (Eq, Ord, Show, Read)

data ForStatement = ForStmnt Ident Expression Expression Statement
  deriving (Eq, Ord, Show, Read)

data WhileStatement = WhileStmnt Expression Statement
  deriving (Eq, Ord, Show, Read)

data IfStatement
    = IfStmnt Expression Statement
    | IfStmntWithElse Expression Statement Statement
  deriving (Eq, Ord, Show, Read)

data PrintStatement = PrintStmnt Expression
  deriving (Eq, Ord, Show, Read)

data Expression
    = ExpSimple SimpleExpression
    | ExpEqual SimpleExpression SimpleExpression
    | ExpNotEqual SimpleExpression SimpleExpression
    | ExpLess SimpleExpression SimpleExpression
    | ExpLessOrEqual SimpleExpression SimpleExpression
    | ExpGreater SimpleExpression SimpleExpression
    | ExpGreaterOrEqual SimpleExpression SimpleExpression
  deriving (Eq, Ord, Show, Read)

data SimpleExpression
    = SimpleExpTerm Term
    | SimpleExpAdd SimpleExpression Term
    | SimpleExpSubst SimpleExpression Term
  deriving (Eq, Ord, Show, Read)

data Term
    = TermFactor Factor
    | TermMultiply Term Factor
    | TermDivide Term Factor
  deriving (Eq, Ord, Show, Read)

data Factor
    = FactorExpression Expression
    | FactorPlus Factor
    | FactorMinus Factor
    | FactorFunctionCall FunctionCall
    | FactorConstant Constant
    | FactorIdent Ident
    | FactorStoI Expression
    | FactorItoS Expression
  deriving (Eq, Ord, Show, Read)

data FunctionCall = FunsCall Ident Actuals
  deriving (Eq, Ord, Show, Read)

data Actuals = Act ExpressionList
  deriving (Eq, Ord, Show, Read)

data ExpressionList
    = ExpListEmpty | ExpList Expression ExpressionList
  deriving (Eq, Ord, Show, Read)

data IdList = IdLEnd Ident | IdL Ident IdList
  deriving (Eq, Ord, Show, Read)

data TypeSpecifier
    = TypeSpecInt
    | TypeSpecBool
    | TypeSpecString
    | TypeSpecArray DimensionList TypeSpecifier
  deriving (Eq, Ord, Show, Read)

data DimensionList
    = DimListEnd Dimension | DimList Dimension DimensionList
  deriving (Eq, Ord, Show, Read)

data Dimension = DimId Ident | DimConst Constant
  deriving (Eq, Ord, Show, Read)

data Constant
    = ConstInt Integer | ConstBool Boolean | ConstString String
  deriving (Eq, Ord, Show, Read)

data Boolean = BoolTrue | BoolFalse
  deriving (Eq, Ord, Show, Read)
