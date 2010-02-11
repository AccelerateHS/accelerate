module Data.Array.Accelerate.CUDA.Syntax where

import Text.PrettyPrint

------------------------------------------------------------------------------
-- Abstract Syntax
------------------------------------------------------------------------------
-- This abstract syntax definition is based on ISO/IEC 9899:TC2
-- (http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf), and
-- NVIDIA CUDA Compute Unified Device Architecture
-- (http://developer.download.nvidia.com/compute/cuda/1_0/NVIDIA_CUDA_Programming_Guide_1.0.pdf).
data TransUnit   = TransUnit [Prepro] [ExtDecln]
                 deriving Eq

data Prepro      = Include Ident
                 | LocalInclude Ident
                 | Define Ident Exp
                 | IfNDef Ident
                 | EndIf
                 deriving Eq

data ExtDecln    = FuncDef [DeclnSpec] Declr [Decln] CompStmt
                 | GlobalDecln Decln
                 deriving Eq

data JumpStmt    = Goto Ident
                 | Continue
                 | Break
                 | Return (Maybe Exp)
                 deriving Eq

data IterStmt    = While Exp Stmt
                 | DoWhile Stmt Exp
                 | For (Maybe Exp) (Maybe Exp) (Maybe Exp) Stmt
                 -- TODO: For Decln Exp Exp Stmt
                 deriving Eq

data SelectStmt  = If Exp Stmt
                 | IfElse Exp Stmt Stmt
                 | Switch Exp Stmt
                 deriving Eq

data BlkItem     = DeclnItem Decln
                 | StmtItem Stmt
                 deriving Eq

data CompStmt    = Blk [BlkItem]
                 | NestedBlk [BlkItem]
                 deriving Eq

data LabeledStmt = IdentLabel Ident Stmt
                 | Case ConstExp Stmt
                 | Default Stmt
                 deriving Eq

data Stmt        = LabeledStmt LabeledStmt
                 | CompStmt CompStmt
                 | ExpStmt (Maybe Exp)
                 | SelectStmt SelectStmt
                 | IterStmt IterStmt
                 | JumpStmt JumpStmt
                 deriving Eq

data Desr        = ConstDesr ConstExp
                 | DotDesr Ident
                 deriving Eq

data Desn        = Desn [Desr]
                 deriving Eq

data Init        = AssignExp AssignExp
                 | BulkInit [(Maybe Desn, Init)]
                 deriving Eq

data DirectAbstDeclr = NestedAbstDeclr AbstDeclr
                     -- TODO: direct-abstract-declarator [...]
                     -- TODO: direct-abstract-declarator (...)
                 deriving Eq

data AbstDeclr   = AbstDeclrPointer Pointer
                 | DirectAbstDeclr (Maybe Pointer) DirectAbstDeclr
                 deriving Eq

data TyName      = TyName [SpecQual] (Maybe AbstDeclr)
                 deriving Eq

data ParamDecln  = ParamDecln [DeclnSpec] Declr
                 | AbstParamDecln [DeclnSpec] (Maybe AbstDeclr)
                 deriving Eq

data Pointer     = Pointer [[TyQual]]
                 deriving Eq

data DirectDeclr = IdentDeclr Ident
                 | NestedDeclr Declr
                 | ArrayDeclr DirectDeclr (Maybe [TyQual]) (Maybe AssignExp)
                 -- TODO: direct-declarator [...]
                 | FuncDeclr DirectDeclr [ParamDecln]
                 deriving Eq

data Declr       = Declr (Maybe Pointer) DirectDeclr
                 deriving Eq

data FnSpec      = Inline
                 deriving Eq

data TyQual      = Const
                 | Restrict
                 | Volatile
                 -- CUDA Specific
                 | Device
                 | Global
                 | Host
                 | Constant
                 | Shared
                 deriving Eq

data SpecQual    = SpecQualTySpec TySpec
                 | SpecQualVecTySpec VecTySpec
                 | SpecQualTyQual TyQual
                 deriving Eq

data TySpec      = Void
                 | Char     (Maybe SignSpec)
                 | Short    (Maybe SignSpec)
                 | Int      (Maybe SignSpec)
                 | Long     (Maybe SignSpec)
                 | LongLong (Maybe SignSpec)
                 | Float
                 | Double
                 | Bool
                 | Complex
                 | Struct (Maybe Ident) (Maybe [StructDecln])
                 | Union  (Maybe Ident) (Maybe [StructDecln])
                 | Enum   (Maybe Ident) (Maybe [Enumerator])
                 | TypedefName Ident
                 deriving Eq

data SignSpec    = Signed
                 | Unsigned
                 deriving Eq

data StructDecln = StructDecln [SpecQual] [StructDeclr]
                 deriving Eq

data StructDeclr = StructDeclr (Maybe Declr) (Maybe ConstExp)
                 deriving Eq

data Enumerator  = Enumerator Ident (Maybe ConstExp)
                 deriving Eq

data VecTySpec   = Vector TySpec Int
                 deriving Eq

data StSpec      = Typedef
                 | Extern (Maybe StrLit)
                 | Static
                 | Auto
                 | Register
                 deriving Eq

data InitDeclr   = InitDeclr Declr (Maybe Init)
                 deriving Eq

data DeclnSpec   = DeclnFnSpec FnSpec
                 | DeclnTyQual TyQual
                 | DeclnTySpec TySpec
                 | DeclnVecTySpec VecTySpec
                 | DeclnStSpec StSpec
                 deriving Eq

data Decln       = Decln [DeclnSpec] [InitDeclr]
                 deriving Eq

data ConstExp    = ConstCondExp CondExp
                 deriving Eq

data Exp         = Exp [AssignExp]
                 deriving Eq

data AssignExp   = CondExp CondExp
                 | Assign UnaryExp AssignExp
                 | MulAssign UnaryExp AssignExp
                 | DivAssign UnaryExp AssignExp
                 | ModAssign UnaryExp AssignExp
                 | AddAssign UnaryExp AssignExp
                 | SubAssign UnaryExp AssignExp
                 | LShftAssign UnaryExp AssignExp
                 | RShftAssign UnaryExp AssignExp
                 | AndAssign UnaryExp AssignExp
                 | XorAssign UnaryExp AssignExp
                 | OrAssign UnaryExp AssignExp
                 deriving Eq

data CondExp     = LgcOrExp LgcOrExp
                 | Cond LgcOrExp Exp CondExp
                 deriving Eq

data LgcOrExp    = LgcAndExp LgcAndExp
                 | LgcOr LgcOrExp LgcAndExp
                 deriving Eq

data LgcAndExp   = OrExp OrExp
                 | LgcAnd LgcAndExp OrExp
                 deriving Eq

data OrExp       = XorExp XorExp
                 | Or OrExp XorExp
                 deriving Eq

data XorExp      = AndExp AndExp
                 | Xor XorExp AndExp
                 deriving Eq

data AndExp      = EqExp EqExp
                 | And AndExp EqExp
                 deriving Eq

data EqExp       = RelExp RelExp
                 | Eq EqExp RelExp
                 | Neq EqExp RelExp
                 deriving Eq

data RelExp      = ShftExp ShftExp
                 | Lt RelExp ShftExp
                 | Gt RelExp ShftExp
                 | Le RelExp ShftExp
                 | Ge RelExp ShftExp
                 deriving Eq

data ShftExp     = AddExp AddExp
                 | LShft ShftExp AddExp
                 | RShft ShftExp AddExp
                 deriving Eq

data AddExp      = MulExp MulExp
                 | Add AddExp MulExp
                 | Sub AddExp MulExp
                 deriving Eq

data MulExp      = CastExp CastExp
                 | Mul MulExp CastExp
                 | Div MulExp CastExp
                 | Mod MulExp CastExp
                 deriving Eq

data CastExp     = UnaryExp UnaryExp
                 | TyCast TyName CastExp
                 deriving Eq

data UnaryExp    = PostfixExp PostfixExp
                 | UnaryInc UnaryExp
                 | UnaryDec UnaryExp
                 | AddrOf CastExp
                 | PtrDeref CastExp
                 | Positive CastExp
                 | Negative CastExp
                 | BitNot CastExp
                 | LgcNot CastExp
                 | UnaryExpSize UnaryExp
                 | TySize TyName
                 deriving Eq

data ArgExpList  = ArgExpList [AssignExp]
                 deriving Eq

data PostfixExp  = PrimaryExp PrimaryExp
                 | ArrayElem PostfixExp Exp
                 | FuncCall PostfixExp ArgExpList
                 | StructMem PostfixExp Ident
                 | StructPtrMem PostfixExp Ident
                 | PostInc PostfixExp
                 | PostDec PostfixExp
                 deriving Eq

data PrimaryExp  = IdentExp Ident
                 | ConstExp Const
                 | StrLitExp StrLit
                 | NestedExp Exp
                 deriving Eq

data StrLit      = StrLit String
                 deriving Eq

data Const       = IntegerConst Integer
                 | FloatConst Float
                 | DoubleConst Double
                 | EnumConst Ident
                 | CharConst Char
                 | Dim3Const [AssignExp]
                 deriving Eq

data Ident       = Ident String
                 deriving (Eq, Ord)

------------------------------------------------------------------------------
-- Pretty Print
------------------------------------------------------------------------------
instance Show TransUnit where
  showsPrec _ t = shows $ ptransunit t

instance Show Prepro where
  showsPrec _ p = shows $ pprepro p

instance Show ExtDecln where
  showsPrec _ e = shows $ pextdecln e

instance Show JumpStmt where
  showsPrec _ j = shows $ pjumpstmt j

instance Show IterStmt where
  showsPrec _ i = shows $ piterstmt i

instance Show SelectStmt where
  showsPrec _ s = shows $ pselectstmt s

instance Show BlkItem where
  showsPrec _ b = shows $ pblkitem b

instance Show CompStmt where
  showsPrec _ c = shows $ pcompstmt c

instance Show LabeledStmt where
  showsPrec _ l = shows $ plabeledstmt l

instance Show Stmt where
  showsPrec _ s = shows $ pstmt s

instance Show Desr where
  showsPrec _ d = shows $ pdesr d

instance Show Desn where
  showsPrec _ d = shows $ pdesn d

instance Show Init where
  showsPrec _ i = shows $ pinit i

instance Show DirectAbstDeclr where
  showsPrec _ d = shows $ pdirectabstdeclr d

instance Show AbstDeclr where
  showsPrec _ a = shows $ pabstdeclr a

instance Show TyName where
  showsPrec _ t = shows $ ptyname t

instance Show ParamDecln where
  showsPrec _ p = shows $ pparamdecln p

instance Show Pointer where
  showsPrec _ p = shows $ ppointer p

instance Show DirectDeclr where
  showsPrec _ d = shows $ pdirectdeclr d

instance Show Declr where
  showsPrec _ d = shows $ pdeclr d

instance Show FnSpec where
  showsPrec _ f = shows $ pfnspec f

instance Show TyQual where
  showsPrec _ t = shows $ ptyqual t

instance Show SpecQual where
  showsPrec _ s = shows $ pspecqual s

instance Show TySpec where
  showsPrec _ t = shows $ ptyspec t

instance Show SignSpec where
  showsPrec _ s = shows $ psignspec s

instance Show StructDecln where
  showsPrec _ s = shows $ pstructdecln s

instance Show StructDeclr where
  showsPrec _ s = shows $ pstructdeclr s

instance Show Enumerator where
  showsPrec _ e = shows $ penumerator e

instance Show VecTySpec where
  showsPrec _ v = shows $ pvectyspec v

instance Show StSpec where
  showsPrec _ s = shows $ pstspec s

instance Show InitDeclr where
  showsPrec _ i = shows $ pinitdeclr i

instance Show DeclnSpec where
  showsPrec _ d = shows $ pdeclnspec d

instance Show Decln where
  showsPrec _ d = shows $ pdecln d

instance Show ConstExp where
  showsPrec _ c = shows $ pconstexp c

instance Show Exp where
  showsPrec _ e = shows $ pexp e

instance Show AssignExp where
  showsPrec _ a = shows $ passignexp a

instance Show CondExp where
  showsPrec _ c = shows $ pcondexp c

instance Show LgcOrExp where
  showsPrec _ l = shows $ plgcorexp l

instance Show LgcAndExp where
  showsPrec _ l = shows $ plgcandexp l

instance Show OrExp where
  showsPrec _ o = shows $ porexp o

instance Show XorExp where
  showsPrec _ x = shows $ pxorexp x

instance Show AndExp where
  showsPrec _ a = shows $ pandexp a

instance Show EqExp where
  showsPrec _ e = shows $ peqexp e

instance Show RelExp where
  showsPrec _ r = shows $ prelexp r

instance Show ShftExp where
  showsPrec _ s = shows $ pshftexp s

instance Show AddExp where
  showsPrec _ a = shows $ paddexp a

instance Show MulExp where
  showsPrec _ m = shows $ pmulexp m

instance Show CastExp where
  showsPrec _ c = shows $ pcastexp c

instance Show UnaryExp where
  showsPrec _ u = shows $ punaryexp u

instance Show ArgExpList where
  showsPrec _ a = shows $ pargexplist a

instance Show PostfixExp where
  showsPrec _ p = shows $ ppostfixexp p

instance Show PrimaryExp where
  showsPrec _ p = shows $ pprimaryexp p

instance Show StrLit where
  showsPrec _ s = shows $ pstrlit s

instance Show Const where
  showsPrec _ c = shows $ pconst c

instance Show Ident where
  showsPrec _ i = shows $ pident i

-- TransUnit
ptransunit :: TransUnit -> Doc
ptransunit (TransUnit ps es) =
  case ps of
    []      -> empty
    (p:ps') -> pprepro p $$ vcat (map pprepro ps')
  $$
  case es of
    []      -> empty
    (e:es') -> pextdecln e $$ vcat (map pextdecln es')

-- Prepro
pprepro :: Prepro -> Doc
pprepro (Include i) = text "#include" <+> char '<' <> pident i <> char '>'
pprepro (LocalInclude i) = text "#include" <+> doubleQuotes (pident i)
pprepro (Define  i e) = text "#define" <+> pident i <+> pexp e
pprepro (IfNDef i) = text "#ifndef" <+> pident i
pprepro (EndIf) = text "#endif"

-- ExtDecln
pextdecln :: ExtDecln -> Doc
pextdecln (FuncDef dss dr dns c) =
  pdeclnspecs dss <+> pdeclr dr <> parens (pdeclns dns) $$
  lbrace $+$ nest 2 (pcompstmt c) $+$ rbrace $+$ text ""
pextdecln (GlobalDecln d) = pdecln d <> semi $+$ text ""

-- [Decln]
pdeclns :: [Decln] -> Doc
pdeclns []     = empty
pdeclns (d:ds) = pdecln d <> hcat (map ((comma <+>) . pdecln) ds)

-- JumpStmt
pjumpstmt :: JumpStmt -> Doc
pjumpstmt (Goto     i) = text "goto" <+> pident i <> semi
pjumpstmt (Continue  ) = text "continue" <> semi
pjumpstmt (Break     ) = text "break" <> semi
pjumpstmt (Return Nothing ) = text "return" <> semi
pjumpstmt (Return (Just e)) = text "return" <+> pexp e <> semi

-- IterStmt
piterstmt :: IterStmt -> Doc
piterstmt (While   e  s   ) =
  text "while" <+> parens (pexp e) <+> lbrace $+$ nest 2 (pstmt s) $+$ rbrace
piterstmt (DoWhile s  e   ) =
  text "do" <+> lbrace $+$ nest 2 (pstmt s) $+$ rbrace <+>
  text "while" <+> parens (pexp e) <> semi
piterstmt (For  e1 e2 e3 s) =
  text "for" <+>
  parens (poptexp e1 <> semi <+> poptexp e2 <> semi <+> poptexp e3) <+>
  lbrace $$ nest 2 (pstmt s) $+$ rbrace
  where
    poptexp :: Maybe Exp -> Doc
    poptexp (Nothing) = empty
    poptexp (Just e ) = pexp e

-- SelectStmt
pselectstmt :: SelectStmt -> Doc
pselectstmt (If     e s    ) =
  text "if" <+> parens (pexp e) <+> lbrace $+$ nest 2 (pstmt s) $+$ rbrace
pselectstmt (IfElse e s1 s2) =
  pselectstmt (If e s1) <+>
  text "else" <+> lbrace $+$ nest 2 (pstmt s2) $+$ rbrace
pselectstmt (Switch e s    ) =
  text "switch" <+> parens (pexp e) <+> lbrace $+$ nest 2 (pstmt s) $+$ rbrace

-- BlkItem
pblkitem :: BlkItem -> Doc
pblkitem (DeclnItem d) = pdecln d <> semi
pblkitem (StmtItem  s) = pstmt s

-- CompStmt
pcompstmt :: CompStmt -> Doc
pcompstmt (Blk []    ) = empty
pcompstmt (Blk (b:bs)) = pblkitem b $$ vcat (map pblkitem bs)
pcompstmt (NestedBlk []    ) = braces (empty)
pcompstmt (NestedBlk (b:bs)) =
  lbrace $+$ nest 2 (pblkitem b $$ vcat (map pblkitem bs)) $+$ rbrace

-- LabeledStmt
plabeledstmt :: LabeledStmt -> Doc
plabeledstmt (IdentLabel i s) = pident i <> colon <+> nest 2 (pstmt s)
plabeledstmt (Case    c s) =
  text "case" <+> pconstexp c <> colon $+$ nest 2 (pstmt s)
plabeledstmt (Default s  ) = text "default" <> colon <+> nest 2 (pstmt s)

-- Stmt
pstmt :: Stmt -> Doc
pstmt (LabeledStmt l) = plabeledstmt l
pstmt (CompStmt    c) = pcompstmt c
pstmt (SelectStmt  s) = pselectstmt s
pstmt (IterStmt    i) = piterstmt i
pstmt (JumpStmt    j) = pjumpstmt j
pstmt (ExpStmt Nothing ) = semi
pstmt (ExpStmt (Just e)) = pexp e <> semi

-- Desr
pdesr :: Desr -> Doc
pdesr (ConstDesr c) = brackets $ pconstexp c
pdesr (DotDesr   i) = text "." <+> pident i

-- Desn
pdesn :: Desn -> Doc
pdesn (Desn (d:ds)) = pdesr d <>
                      hcat (map ((space <>) . pdesr) ds) <> text "=" <> space

-- Init
pinit :: Init -> Doc
pinit (AssignExp a)     = passignexp a
pinit (BulkInit (i:is)) = braces (peleminit i <>
                                  hcat (map ((text "," <+>) . peleminit) is)
                                 )
    where
        peleminit :: (Maybe Desn, Init) -> Doc
        peleminit (Nothing , i) = pinit i
        peleminit ((Just d), i) = pdesn d <+> pinit i

-- DirectAbstDeclr
pdirectabstdeclr :: DirectAbstDeclr -> Doc
pdirectabstdeclr (NestedAbstDeclr a) = parens $ pabstdeclr a

-- AbstDeclr
pabstdeclr :: AbstDeclr -> Doc
pabstdeclr (AbstDeclrPointer p         ) = ppointer p
pabstdeclr (DirectAbstDeclr  Nothing  d) = pdirectabstdeclr d
pabstdeclr (DirectAbstDeclr  (Just p) d) = ppointer p <+> pdirectabstdeclr d

-- TyName
ptyname :: TyName -> Doc
ptyname (TyName ss Nothing ) = pspecquals ss
ptyname (TyName ss (Just a)) = pspecquals ss <+> pabstdeclr a

-- ParamDecln
pparamdecln :: ParamDecln -> Doc
pparamdecln (ParamDecln      ds d) = pdeclnspecs ds <+> pdeclr d
pparamdecln (AbstParamDecln  ds Nothing ) = pdeclnspecs ds
pparamdecln (AbstParamDecln  ds (Just a)) = pdeclnspecs ds <+> pabstdeclr a

-- [TyQual]
ptyquals :: [TyQual] -> Doc
ptyquals []     = empty
ptyquals (t:ts) = ptyqual t <> hcat (map ((space <>) . ptyqual) ts) <> space

-- Pointer
ppointer :: Pointer -> Doc
ppointer (Pointer (t:ts)) = text "*" <+> ptyquals t <>
                            hcat (map ((text "*" <+>) . ptyquals) ts)

-- DirectDeclr
pdirectdeclr :: DirectDeclr -> Doc
pdirectdeclr (IdentDeclr i)  = pident i
pdirectdeclr (NestedDeclr d) = parens $ pdeclr d
pdirectdeclr (ArrayDeclr d ts a) =
    let
        popttyquals :: (Maybe [TyQual]) -> Doc
        popttyquals (Nothing) = empty
        popttyquals (Just ts) = ptyquals ts
        poptassignexp :: (Maybe AssignExp) -> Doc
        poptassignexp (Nothing) = empty
        poptassignexp (Just  a) = passignexp a
    in pdirectdeclr d <> brackets (popttyquals ts <+> poptassignexp a)
pdirectdeclr (FuncDeclr d []) =
  pdirectdeclr d <> parens empty
pdirectdeclr (FuncDeclr d (p:ps)) =
  pdirectdeclr d <> parens (pparamdecln p <> hcat (map ((comma <+>) . pparamdecln) ps))

-- Declr
pdeclr :: Declr -> Doc
pdeclr (Declr Nothing  d) = pdirectdeclr d
pdeclr (Declr (Just p) d) = ppointer p <> pdirectdeclr d

-- FnSpec
pfnspec :: FnSpec -> Doc
pfnspec (Inline) = text "inline"

-- TyQual
ptyqual :: TyQual -> Doc
ptyqual (Const   ) = text "const"
ptyqual (Restrict) = text "restrict"
ptyqual (Volatile) = text "volatile"
ptyqual (Device  ) = text "__device__"
ptyqual (Global  ) = text "__global__"
ptyqual (Host    ) = text "__host__"
ptyqual (Constant) = text "__constant__"
ptyqual (Shared  ) = text "__shared__"

-- [SpecQual]
pspecquals :: [SpecQual] -> Doc
pspecquals []     = empty
pspecquals (s:ss) = pspecqual s <> hcat (map ((space <>) . pspecqual) ss)

-- SpecQual
pspecqual :: SpecQual -> Doc
pspecqual (SpecQualTySpec     t) = ptyspec     t
pspecqual (SpecQualVecTySpec  v) = pvectyspec  v
pspecqual (SpecQualTyQual     t) = ptyqual     t

-- TySpec
ptyspec :: TySpec -> Doc
ptyspec (Void         ) = text "void"
ptyspec (Char     s   ) =
  case s of
    Just s' -> psignspec s' <+> text "char"
    _       -> text "char"
ptyspec (Short    s   ) =
  case s of
    Just s' -> psignspec s' <+> text "short"
    _       -> text "short"
ptyspec (Int      s   ) =
  case s of
    Just s' -> psignspec s' <+> text "int"
    _       -> text "int"
ptyspec (Long     s   ) =
  case s of
    Just s' -> psignspec s' <+> text "long"
    _       -> text "long"
ptyspec (LongLong s   ) =
  case s of
    Just s' -> psignspec s' <+> text "long long"
    _       -> text "long long"
ptyspec (Float        ) = text "float"
ptyspec (Double       ) = text "double"
ptyspec (Bool         ) = text "_Bool"
ptyspec (Complex      ) = text "_Complex"
ptyspec (Struct   i sl) =
  case (i, sl) of
    (Just i', Just sl') ->
      text "struct" <+> pident i' <+>
      lbrace $+$ nest 2 (pstructdeclns sl') $+$ rbrace
    (Just i', Nothing)  ->
      text "struct" <+> pident i'
    (Nothing, Just sl') ->
      text "struct" <+> lbrace $+$ nest 2 (pstructdeclns sl') $+$ rbrace
    (Nothing, Nothing)  -> empty
ptyspec (Union    i sl) =
  case (i, sl) of
    (Just i', Just sl') ->
      text "union" <+> pident i' <+>
      lbrace $+$ nest 2 (pstructdeclns sl') $+$ rbrace
    (Just i', Nothing)  ->
      text "union" <+> pident i'
    (Nothing, Just sl') ->
      text "union" <+> lbrace $+$ nest 2 (pstructdeclns sl') $+$ rbrace
    (Nothing, Nothing)  -> empty
ptyspec (Enum     i sl) =
  case (i, sl) of
    (Just i', Just el') ->
      text "enum" <+> pident i' <+>
      lbrace $+$ nest 2 (penumerators el') $+$ rbrace
    (Just i', Nothing)  ->
      text "enum" <+> pident i'
    (Nothing, Just el') ->
      text "enum" <+> braces (penumerators el')
    (Nothing, Nothing)  -> empty
ptyspec (TypedefName t) = pident t

-- SignSpec
psignspec :: SignSpec -> Doc
psignspec (Signed)   = text "signed"
psignspec (Unsigned) = text "unsigned"

-- [StructDecln]
pstructdeclns :: [StructDecln] -> Doc
pstructdeclns []     = empty
pstructdeclns (s:ss) =
  pstructdecln s $$ vcat (map (pstructdecln) ss)

-- StructDecln
pstructdecln :: StructDecln -> Doc
pstructdecln (StructDecln sqs sds) =
  pspecquals sqs <+> pstructdeclrs sds <> semi

-- [StructDeclr]
pstructdeclrs :: [StructDeclr] -> Doc
pstructdeclrs []     = empty
pstructdeclrs (s:ss) =
  pstructdeclr s <> hcat (map ((comma <+>) . pstructdeclr) ss)

-- StructDeclr
pstructdeclr :: StructDeclr -> Doc
pstructdeclr (StructDeclr d c) =
  case d of
    Just d' -> pdeclr d'
    Nothing -> empty
  <>
  case c of
    Just c' -> colon <+> pconstexp c'
    Nothing -> empty

-- [Enumerator]
penumerators :: [Enumerator] -> Doc
penumerators (e:[]) = penumerator e
penumerators (e:es) = penumerator e <> comma $$ penumerators es

-- Enumerator
penumerator :: Enumerator -> Doc
penumerator (Enumerator i c) =
  pident i <>
  case c of
    Just c' -> colon <+> pconstexp c'
    Nothing -> empty

-- VecTySpec
pvectyspec :: VecTySpec -> Doc
pvectyspec (Vector t n) =
  case t of
    Char     (Just Unsigned) -> text "uchar"
    Short    (Just Unsigned) -> text "ushort"
    Int      (Just Unsigned) -> text "uint"
    Long     (Just Unsigned) -> text "ulong"
    LongLong (Just Unsigned) -> text "ulonglong"
    LongLong _               -> text "longlong"
    _                        -> ptyspec t
  <> int n

-- StSpec
pstspec :: StSpec -> Doc
pstspec (Typedef ) = text "typedef"
pstspec (Extern Nothing) = text "extern"
pstspec (Extern (Just i)) = text "extern" <+> pstrlit i
pstspec (Static  ) = text "static"
pstspec (Auto    ) = text "auto"
pstspec (Register) = text "register"

-- InitDeclr
pinitdeclr :: InitDeclr -> Doc
pinitdeclr (InitDeclr d Nothing ) = pdeclr d
pinitdeclr (InitDeclr d (Just i)) = pdeclr d <+> text "=" <+> pinit i

-- [DeclnSpecs]
pdeclnspecs :: [DeclnSpec] -> Doc
pdeclnspecs []     = empty
pdeclnspecs (d:ds) = pdeclnspec d <> hcat (map ((space <>) . pdeclnspec) ds)

-- DeclnSpec
pdeclnspec :: DeclnSpec -> Doc
pdeclnspec (DeclnFnSpec     f) = pfnspec     f
pdeclnspec (DeclnTyQual     t) = ptyqual     t
pdeclnspec (DeclnTySpec     t) = ptyspec     t
pdeclnspec (DeclnVecTySpec  t) = pvectyspec  t
pdeclnspec (DeclnStSpec     s) = pstspec     s

-- Decln
pdecln :: Decln -> Doc
pdecln (Decln ds is) =
  case ds of
    []      -> empty
    (d:ds') -> pdeclnspecs ds
  <+>
  case is of
    []      -> empty
    (i:is') -> pinitdeclrs is
  where
    pinitdeclrs :: [InitDeclr] -> Doc
    pinitdeclrs (i:is) = pinitdeclr i <>
                         hcat (map ((comma <+>) . pinitdeclr) is)

-- ConstExp
pconstexp :: ConstExp -> Doc
pconstexp (ConstCondExp c) = pcondexp c

-- Exp
pexp :: Exp -> Doc
pexp (Exp [])     = empty
pexp (Exp (e:es)) = passignexp e <>
                    hcat (map ((comma <+>) . passignexp) es)

-- AssignExp
passignexp :: AssignExp -> Doc
passignexp (CondExp     c  ) = pcondexp c
passignexp (Assign      u a) = punaryexp u <+> text "=" <+> passignexp a
passignexp (MulAssign   u a) = punaryexp u <+> text "*=" <+> passignexp a
passignexp (DivAssign   u a) = punaryexp u <+> text "/=" <+> passignexp a
passignexp (ModAssign   u a) = punaryexp u <+> text "%=" <+> passignexp a
passignexp (AddAssign   u a) = punaryexp u <+> text "+=" <+> passignexp a
passignexp (SubAssign   u a) = punaryexp u <+> text "-=" <+> passignexp a
passignexp (LShftAssign u a) = punaryexp u <+> text "<<=" <+> passignexp a
passignexp (RShftAssign u a) = punaryexp u <+> text ">>=" <+> passignexp a
passignexp (AndAssign   u a) = punaryexp u <+> text "&=" <+> passignexp a
passignexp (XorAssign   u a) = punaryexp u <+> text "^=" <+> passignexp a
passignexp (OrAssign    u a) = punaryexp u <+> text "|=" <+> passignexp a

-- CondExp
pcondexp :: CondExp -> Doc
pcondexp (LgcOrExp o    ) = plgcorexp o
pcondexp (Cond     o e c) = plgcorexp o <+> text "?" <+>
                            pexp e <+> colon <+> pcondexp c

-- LgcOrExp
plgcorexp :: LgcOrExp -> Doc
plgcorexp (LgcAndExp a  ) = plgcandexp a
plgcorexp (LgcOr     o a) = plgcorexp o <+> text "||" <+> plgcandexp a

-- LgcAndExp
plgcandexp :: LgcAndExp -> Doc
plgcandexp (OrExp  o  ) = porexp o
plgcandexp (LgcAnd a o) = plgcandexp a <+> text "&&" <+> porexp o

-- OrExp
porexp :: OrExp -> Doc
porexp (XorExp x  ) = pxorexp x
porexp (Or     o x) = porexp o <+> text "|" <+> pxorexp x

-- XorExp
pxorexp :: XorExp -> Doc
pxorexp (AndExp a  ) = pandexp a
pxorexp (Xor    x a) = pxorexp x <+> text "^" <+> pandexp a

-- AndExp
pandexp :: AndExp -> Doc
pandexp (EqExp e  ) = peqexp e
pandexp (And   a e) = pandexp a <+> text "&" <+> peqexp e

-- EqExp
peqexp :: EqExp -> Doc
peqexp (RelExp r  ) = prelexp r
peqexp (Eq     e r) = peqexp e <+> text "==" <+> prelexp r
peqexp (Neq    e r) = peqexp e <+> text "!=" <+> prelexp r

-- RelExp
prelexp :: RelExp -> Doc
prelexp (ShftExp s  ) = pshftexp s
prelexp (Lt      r s) = prelexp r <+> text "<" <+> pshftexp s
prelexp (Gt      r s) = prelexp r <+> text ">" <+> pshftexp s
prelexp (Le      r s) = prelexp r <+> text "<=" <+> pshftexp s
prelexp (Ge      r s) = prelexp r <+> text ">=" <+> pshftexp s

-- ShftExp
pshftexp :: ShftExp -> Doc
pshftexp (AddExp a  ) = paddexp a
pshftexp (LShft  s a) = pshftexp s <+> text "<<" <+> paddexp a
pshftexp (RShft  s a) = pshftexp s <+> text ">>" <+> paddexp a

-- AddExp
paddexp :: AddExp -> Doc
paddexp (MulExp m  ) = pmulexp m
paddexp (Add    a m) = paddexp a <+> text "+" <+> pmulexp m
paddexp (Sub    a m) = paddexp a <+> text "-" <+> pmulexp m

-- MulExp
pmulexp :: MulExp -> Doc
pmulexp (CastExp c  ) = pcastexp c
pmulexp (Mul     m c) = pmulexp m <+> text "*" <+> pcastexp c
pmulexp (Div     m c) = pmulexp m <+> text "/" <+> pcastexp c
pmulexp (Mod     m c) = pmulexp m <+> text "%" <+> pcastexp c

-- CastExp
pcastexp :: CastExp -> Doc
pcastexp (UnaryExp u) = punaryexp u
pcastexp (TyCast t c) = parens (ptyname t) <+> pcastexp c

-- UnaryExp
punaryexp :: UnaryExp -> Doc
punaryexp (PostfixExp   p) = ppostfixexp p
punaryexp (UnaryInc     u) = text "++" <> punaryexp u
punaryexp (UnaryDec     u) = text "--" <> punaryexp u
punaryexp (AddrOf       c) = text "&" <> pcastexp c
punaryexp (PtrDeref     c) = text "*" <> pcastexp c
punaryexp (Positive     c) = text "+" <> pcastexp c
punaryexp (Negative     c) = text "-" <> pcastexp c
punaryexp (BitNot       c) = text "~" <> pcastexp c
punaryexp (LgcNot     c) = text "!" <> pcastexp c
punaryexp (UnaryExpSize u) = text "sizeof" <+> parens (punaryexp u)
punaryexp (TySize       t) = text "sizeof" <+> parens (ptyname t)

-- ArgExpList
pargexplist :: ArgExpList -> Doc
pargexplist (ArgExpList []    ) = empty
pargexplist (ArgExpList (a:as)) = passignexp a <>
                                  hcat (map ((comma <+>) . passignexp) as)

-- PostfixExp
ppostfixexp :: PostfixExp -> Doc
ppostfixexp (PrimaryExp   p     ) = pprimaryexp p
ppostfixexp (ArrayElem    p e   ) = ppostfixexp p <> brackets (pexp e)
ppostfixexp (FuncCall     p a) =
    ppostfixexp p <> parens (pargexplist a)
ppostfixexp (StructMem    p i   ) = ppostfixexp p <> text "." <> pident i
ppostfixexp (StructPtrMem p i   ) = ppostfixexp p <> text "->" <> pident i
ppostfixexp (PostInc p) = ppostfixexp p <> text "++"
ppostfixexp (PostDec p) = ppostfixexp p <> text "--"

-- PrimaryExp
pprimaryexp :: PrimaryExp -> Doc
pprimaryexp (IdentExp  i) = pident i
pprimaryexp (ConstExp  c) = pconst c
pprimaryexp (StrLitExp s) = pstrlit s
pprimaryexp (NestedExp e) = parens $ pexp e

-- StrLit
pstrlit :: StrLit -> Doc
pstrlit (StrLit s) = doubleQuotes $ text s

-- Constant
pconst :: Const -> Doc
pconst (IntegerConst i) = integer i
pconst (FloatConst   f) = float  f <> text "f"
pconst (DoubleConst  d) = double d
pconst (EnumConst    e) = pident e
pconst (CharConst    c) = quotes $ char c
pconst (Dim3Const  (d:ds)) = text "dim3"
    <+> parens (passignexp d <> hcat (map ((comma <+>) . passignexp) ds))

-- Identifier
pident :: Ident -> Doc
pident (Ident i) = text i

------------------------------------------------------------------------------
-- Graphviz
------------------------------------------------------------------------------
class Dot t where
  dot :: t -> String

instance Dot TransUnit where
  dot t = render $ dtransunit t

-- TransUnit
dtransunit :: TransUnit -> Doc
dtransunit (TransUnit ps es) =
  text "digraph" <+> lbrace $$
  nest 2
    ( dlabel (int 0) (text "TransUnit") $$
      dprepros (int 0) 0 0 empty ps $$
      dextdeclns (int 0) 1 0 empty es) $$
  rbrace

-- Prepro
dprepro :: Doc -> Int -> Prepro -> Doc
dprepro parent idx p =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case p of
    Include i ->
      dlabel node (text "Include") $$
      dident node 0 i
    LocalInclude i ->
      dlabel node (text "LocalInclude") $$
      dident node 0 i
    Define i e ->
      dlabel node (text "Define") $$
      dident node 0 i $$
      dexp node 1 e
    IfNDef i ->
      dlabel node (text "IfNDef") $$
      dident node 0 i
    EndIf ->
      dlabel node (text "EndIf")
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dprepros :: Doc -> Int -> Int -> Doc -> [Prepro] -> Doc
dprepros parent idx idx' acc pl =
  case pl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[Prepro]") $$ acc
    (p:ps) ->
      dprepros parent idx (idx' + 1) (acc $$ dprepro node idx' p) ps
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- ExtDecln
dextdecln :: Doc -> Int -> ExtDecln -> Doc
dextdecln parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    FuncDef dss dr dns c ->
      dlabel node (text "FuncDef") $$
      ddeclnspecs node 0 0 empty dss $$
      ddeclr node 1 dr $$
      ddeclns node 2 0 empty dns $$
      dcompstmt node 3 c
    GlobalDecln d ->
      dlabel node (text "GlobalDecln") $$
      ddecln node 0 d
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dextdeclns :: Doc -> Int -> Int -> Doc -> [ExtDecln] -> Doc
dextdeclns parent idx idx' acc el =
  case el of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[ExtDecln]") $$ acc
    (e:es) ->
      dextdeclns parent idx (idx' + 1) (acc $$ dextdecln node idx' e) es
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- JumpStmt
djumpstmt :: Doc -> Int -> JumpStmt -> Doc
djumpstmt parent idx j =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case j of
    Goto i ->
      dlabel node (text "Goto") $$
      dident node 0 i
    Continue ->
      dlabel node (text "Continue")
    Break ->
      dlabel node (text "Break")
    Return e ->
      dlabel node (text "Return") $$
      case e of
        Just e' -> dexp node 0 e'
        Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- IterStmt
diterstmt :: Doc -> Int -> IterStmt -> Doc
diterstmt parent idx i =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case i of
    While e s ->
      dlabel node (text "While") $$
      dexp node 0 e $$
      dstmt node 1 s
    DoWhile s e ->
      dlabel node (text "DoWhile") $$
      dstmt node 0 s $$
      dexp node 1 e
    For e1 e2 e3 s ->
      dlabel node (text "For") $$
      case e1 of
        Just e1' -> dexp node 0 e1'
        Nothing  -> empty
      $$
      case e2 of
        Just e2' -> dexp node 1 e2'
        Nothing  -> empty
      $$
      case e3 of
        Just e3' -> dexp node 2 e3'
        Nothing  -> empty
      $$
      dstmt node 3 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- SelectStmt
dselectstmt :: Doc -> Int -> SelectStmt -> Doc
dselectstmt parent idx s =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case s of
    If e s ->
      dlabel node (text "If") $$
      dexp node 0 e $$
      dstmt node 1 s
    IfElse e s1 s2 ->
      dlabel node (text "IfElse") $$
      dexp node 0 e $$
      dstmt node 1 s1 $$
      dstmt node 2 s2
    Switch e s ->
      dlabel node (text "Switch") $$
      dexp node 0 e $$
      dstmt node 1 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- BlkItem
dblkitem :: Doc -> Int -> BlkItem -> Doc
dblkitem parent idx b =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case b of
    DeclnItem d ->
      dlabel node (text "DeclnItem") $$
      ddecln node 0 d
    StmtItem s ->
      dlabel node (text "StmtItem") $$
      dstmt node 0 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dblkitems :: Doc -> Int -> Int -> Doc -> [BlkItem] -> Doc
dblkitems parent idx idx' acc bl =
  case bl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[BlkItem]") $$ acc
    (b:bs) ->
      dblkitems parent idx (idx' + 1) (acc $$ dblkitem node idx' b) bs
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- CompStmt
dcompstmt :: Doc -> Int -> CompStmt -> Doc
dcompstmt parent idx c =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case c of
    Blk bs ->
      dlabel node (text "Blk") $$
      dblkitems node 0 0 empty bs
    NestedBlk bs ->
      dlabel node (text "NestedBlk") $$
      dblkitems node 0 0 empty bs
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- LabeledStmt
dlabeledstmt :: Doc -> Int -> LabeledStmt -> Doc
dlabeledstmt parent idx l =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case l of
    IdentLabel i s ->
      dlabel node (text "IdentLabel") $$
      dident node 0 i $$
      dstmt node 1 s
    Case c s ->
      dlabel node (text "Case") $$
      dconstexp node 0 c $$
      dstmt node 1 s
    Default s ->
      dlabel node (text "Default") $$
      dstmt node 0 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Stmt
dstmt :: Doc -> Int -> Stmt -> Doc
dstmt parent idx s =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case s of
    LabeledStmt l ->
      dlabel node (text "LabeledStmt") $$
      dlabeledstmt node 0 l
    CompStmt c ->
      dlabel node (text "CompStmt") $$
      dcompstmt node 0 c
    SelectStmt s ->
      dlabel node (text "SelectStmt") $$
      dselectstmt node 0 s
    IterStmt i ->
      dlabel node (text "IterStmt") $$
      diterstmt node 0 i
    JumpStmt j ->
      dlabel node (text "JumpStmt") $$
      djumpstmt node 0 j
    ExpStmt e ->
      dlabel node (text "ExpStmt") $$
      case e of
        Just e' -> dexp node 0 e'
        Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Desr
ddesr :: Doc -> Int -> Desr -> Doc
ddesr parent idx d =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case d of
    ConstDesr c ->
      dlabel node (text "ConstDesr") $$
      dconstexp node 0 c
    DotDesr i ->
      dlabel node (text "DotDesr") $$
      dident node 0 i
  where
    node :: Doc
    node = parent <> text "-" <> int idx

ddesrs :: Doc -> Int -> Int -> Doc -> [Desr] -> Doc
ddesrs parent idx idx' acc dl =
  case dl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[Desr]") $$ acc
    (d:ds) ->
      ddesrs parent idx (idx' + 1) (acc $$ ddesr node idx' d) ds
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Desn
ddesn :: Doc -> Int -> Desn -> Doc
ddesn parent idx (Desn ds) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Desn") $$
  ddesrs node 0 0 empty ds
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Init
dinit :: Doc -> Int -> Init -> Doc
dinit parent idx i =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case i of
    AssignExp a ->
      dlabel node (text "AssignExp") $$
      dassignexp node 0 a
    BulkInit bs ->
      dlabel node (text "BulkInit") $$
      deleminits node 0 0 empty bs
  where
    node :: Doc
    node = parent <> text "-" <> int idx

deleminit :: Doc -> Int -> (Maybe Desn, Init) -> Doc
deleminit parent idx (d, i) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "(Maybe Desn, Init)") $$
  case d of
    Just d' -> ddesn node 0 d'
    Nothing -> empty
  $$
  dinit node 1 i
  where
    node :: Doc
    node = parent <> text "-" <> int idx

deleminits :: Doc -> Int -> Int -> Doc -> [(Maybe Desn, Init)] -> Doc
deleminits parent idx idx' acc bl =
  case bl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[(Maybe Desn, Init)]") $$ acc
    (t:ts) ->
      deleminits parent idx (idx' + 1) (acc $$ deleminit node idx' t) ts
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- DirectAbstDeclr
ddirectabstdeclr :: Doc -> Int -> DirectAbstDeclr -> Doc
ddirectabstdeclr parent idx (NestedAbstDeclr a) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "NestedAbstDeclr") $$
  dabstdeclr node 0 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- AbstDeclr
dabstdeclr :: Doc -> Int -> AbstDeclr -> Doc
dabstdeclr parent idx a =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case a of
    AbstDeclrPointer p ->
      dlabel node (text "AbstDeclrPointer") $$
      dpointer node 0 0 empty p
    DirectAbstDeclr p d ->
      dlabel node (text "DirectAbstDeclr") $$
      case p of
        Just p' -> dpointer node 0 0 empty p'
        Nothing -> empty
      $$
      ddirectabstdeclr node 1 d
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- TyName
dtyname :: Doc -> Int -> TyName -> Doc
dtyname parent idx (TyName ss a) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "TyName") $$
  dspecquals node 0 0 empty ss $$
  case a of
    Just a' -> dabstdeclr node 1 a'
    Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- ParamDecln
dparamdecln :: Doc -> Int -> ParamDecln -> Doc
dparamdecln parent idx p =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case p of
    ParamDecln ds d ->
      dlabel node (text "ParamDecln") $$
      ddeclnspecs node 0 0 empty ds $$
      ddeclr node 1 d
    AbstParamDecln ds a ->
      dlabel node (text "AbstParamDecln") $$
      ddeclnspecs node 0 0 empty ds $$
      case a of
        Just a' -> dabstdeclr node 1 a'
        Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dparamdeclns :: Doc -> Int -> Int -> Doc -> [ParamDecln] -> Doc
dparamdeclns parent idx idx' acc pl =
  case pl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "ParamDecln") $$ acc
    (p:ps) ->
      dparamdeclns parent idx (idx' + 1) (acc $$ dparamdecln node idx' p) ps
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Pointer
dpointer :: Doc -> Int -> Int -> Doc -> Pointer -> Doc
dpointer parent idx idx' acc pl =
  case pl of
    Pointer [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "Pointer") $$ acc
    Pointer (p:ps) ->
      dpointer
        parent idx (idx' + 1)
        (acc $$ dtyquals node idx' 0 empty p) (Pointer ps)
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- DirectDeclr
ddirectdeclr :: Doc -> Int -> DirectDeclr -> Doc
ddirectdeclr parent idx d =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case d of
    IdentDeclr  i  ->
      dlabel node (text "IdentDeclr") $$
      dident node 0 i
    NestedDeclr d' ->
      dlabel node (text "NestedDeclr") $$
      ddeclr node 0 d'
    ArrayDeclr d' ts a ->
      dlabel node (text "ArrayDeclr") $$
      ddirectdeclr node 0 d' $$
      case ts of
        Just ts' -> dtyquals node 1 0 empty ts'
        Nothing  -> empty
      $$
      case a  of
        Just a'  -> dassignexp node 2 a'
        Nothing  -> empty
    FuncDeclr d' ps ->
      dlabel node (text "FuncDeclr") $$
      ddirectdeclr node 0 d' $$
      dparamdeclns node 1 0 empty ps
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Declr
ddeclr :: Doc -> Int -> Declr -> Doc
ddeclr parent idx (Declr p d) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Declr") $$
  case p of
    Just p' -> dpointer node 0 0 empty p' $$ ddirectdeclr node 1 d
    Nothing -> ddirectdeclr node 0 d
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- FnSpec
dfnspec :: Doc -> Int -> FnSpec -> Doc
dfnspec parent idx (Inline) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Inline")
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- TyQual
dtyqual :: Doc -> Int -> TyQual -> Doc
dtyqual parent idx t =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case t of
    Const    -> dlabel node (text "Const")
    Restrict -> dlabel node (text "Restrict")
    Volatile -> dlabel node (text "Volatile")
    Device   -> dlabel node (text "Device")
    Global   -> dlabel node (text "Global")
    Host     -> dlabel node (text "Host")
    Constant -> dlabel node (text "Constant")
    Shared   -> dlabel node (text "Shared")
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dtyquals :: Doc -> Int -> Int -> Doc -> [TyQual] -> Doc
dtyquals parent idx idx' acc tl =
  case tl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[TyQual]") $$ acc
    (t:ts) ->
      dtyquals parent idx (idx' + 1) (acc $$ dtyqual node idx' t) ts
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- SpecQual
dspecqual :: Doc -> Int -> SpecQual -> Doc
dspecqual parent idx s =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case s of
    SpecQualTySpec s ->
      dlabel node (text "SpecQualTySpec") $$ dtyspec node 0 s
    SpecQualVecTySpec s ->
      dlabel node (text "SpecQualVecTySpec") $$ dvectyspec node 0 s
    SpecQualTyQual s ->
      dlabel node (text "SpecQualTyQual") $$ dtyqual node 0 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dspecquals :: Doc -> Int -> Int -> Doc -> [SpecQual] -> Doc
dspecquals parent idx idx' acc sl =
  case sl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[SpecQual]") $$ acc
    (s:ss) ->
      dspecquals parent idx (idx' + 1) (acc $$ dspecqual node idx' s) ss
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- TySpec
dtyspec :: Doc -> Int -> TySpec -> Doc
dtyspec parent idx t =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case t of
    Void       -> dlabel node (text "Void")
    Char     s ->
      dlabel node (text "Char") $$
      case s of
        Just s' -> dsignspec node 0 s'
        Nothing -> empty
    Short    s ->
      dlabel node (text "Short") $$
      case s of
        Just s' -> dsignspec node 0 s'
        Nothing -> empty
    Int      s ->
      dlabel node (text "Int") $$
      case s of
        Just s' -> dsignspec node 0 s'
        Nothing -> empty
    Long     s ->
      dlabel node (text "Long") $$
      case s of
        Just s' -> dsignspec node 0 s'
        Nothing -> empty
    LongLong s ->
      dlabel node (text "LongLong") $$
      case s of
        Just s' -> dsignspec node 0 s'
        Nothing -> empty
    Float      -> dlabel node (text "Float")
    Double     -> dlabel node (text "Double")
    Bool       -> dlabel node (text "Bool")
    Complex    -> dlabel node (text "Complex")
    Struct i sl ->
      dlabel node (text "Struct") $$
      case i of
        Just i' -> dident node 0 i'
        Nothing -> empty
      $$
      case sl of
        Just sl' -> dstructdeclns node 1 0 empty sl'
        Nothing -> empty
    Union i sl ->
      dlabel node (text "Union") $$
      case i of
        Just i' -> dident node 0 i'
        Nothing -> empty
      $$
      case sl of
        Just sl' -> dstructdeclns node 1 0 empty sl'
        Nothing -> empty
    TypedefName t' -> dlabel node (text "TypedefName") $$ dident node 0 t'
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- SignSpec
dsignspec :: Doc -> Int -> SignSpec -> Doc
dsignspec parent idx t =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case t of
    Signed   -> dlabel node (text "Signed")
    Unsigned -> dlabel node (text "Unsigned")
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- StructDecln
dstructdecln :: Doc -> Int -> StructDecln -> Doc
dstructdecln parent idx (StructDecln sqs sds) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "StructDecln") $$
  dspecquals node 0 0 empty sqs $$
  dstructdeclrs node 1 0 empty sds
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dstructdeclns :: Doc -> Int -> Int -> Doc -> [StructDecln] -> Doc
dstructdeclns parent idx idx' acc sl =
  case sl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[StructDecln]") $$ acc
    (s:ss) ->
      dstructdeclns parent idx (idx' + 1) (acc $$ dstructdecln node idx' s) ss
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- StructDeclr
dstructdeclr :: Doc -> Int -> StructDeclr -> Doc
dstructdeclr parent idx (StructDeclr d c) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "StructDeclr") $$
  case d of
    Just d' -> ddeclr node 0 d'
    Nothing -> empty
  $$
  case c of
    Just c' -> dconstexp node 1 c'
    Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dstructdeclrs :: Doc -> Int -> Int -> Doc -> [StructDeclr] -> Doc
dstructdeclrs parent idx idx' acc sl =
  case sl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[StructDeclr]") $$ acc
    (s:ss) ->
      dstructdeclrs parent idx (idx' + 1) (acc $$ dstructdeclr node idx' s) ss
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- VecTySpec
dvectyspec :: Doc -> Int -> VecTySpec -> Doc
dvectyspec parent idx (Vector t n) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "VecTySpec") $$
  dtyspec node 0 t $$
  doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
  dlabel node' (int n)
  where
    node :: Doc
    node = parent <> text "-" <> int idx
    node' :: Doc
    node' = node <> text "-" <> int 1

-- StSpec
dstspec :: Doc -> Int -> StSpec -> Doc
dstspec parent idx s =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case s of
    Typedef         -> dlabel node (text "Typedef")
    Extern Nothing  -> dlabel node (text "Extern")
    Extern (Just i) -> dlabel node (text "Extern") $$ dstrlit node 0 i
    Static          -> dlabel node (text "Static")
    Auto            -> dlabel node (text "Auto")
    Register        -> dlabel node (text "Register")
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- InitDeclr
dinitdeclr :: Doc -> Int -> InitDeclr -> Doc
dinitdeclr parent idx (InitDeclr d i) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "InitDeclr") $$
  ddeclr node 0 d $$
  case i of
    Just i' -> dinit node 1 i'
    Nothing -> empty
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dinitdeclrs :: Doc -> Int -> Int -> Doc -> [InitDeclr] -> Doc
dinitdeclrs parent idx idx' acc il =
  case il of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[InitDeclr]") $$ acc
    (i:is) ->
      dinitdeclrs parent idx (idx' + 1) (acc $$ dinitdeclr node idx' i) is
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- DeclnSpec
ddeclnspec :: Doc -> Int -> DeclnSpec -> Doc
ddeclnspec parent idx d =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case d of
    DeclnFnSpec f ->
      dlabel node (text "DeclnFnSpec") $$
      dfnspec node 0 f
    DeclnTyQual t ->
      dlabel node (text "DeclnTyQual") $$
      dtyqual node 0 t
    DeclnTySpec t ->
      dlabel node (text "DeclnTySpec") $$
      dtyspec node 0 t
    DeclnVecTySpec v ->
      dlabel node (text "DeclnVecTySpec") $$
      dvectyspec node 0 v
    DeclnStSpec s ->
      dlabel node (text "DeclnStSpec") $$
      dstspec node 0 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

ddeclnspecs :: Doc -> Int -> Int -> Doc -> [DeclnSpec] -> Doc
ddeclnspecs parent idx idx' acc dl =
  case dl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[DeclnSpec]") $$ acc
    (d:ds) ->
      ddeclnspecs parent idx (idx' + 1) (acc $$ ddeclnspec node idx' d) ds
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Decln
ddecln :: Doc -> Int -> Decln -> Doc
ddecln parent idx (Decln dl il) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Decln") $$
  ddeclnspecs node 0 0 empty dl $$
  dinitdeclrs node 1 0 empty il
  where
    node :: Doc
    node = parent <> text "-" <> int idx

ddeclns :: Doc -> Int -> Int -> Doc -> [Decln] -> Doc
ddeclns parent idx idx' acc dl =
  case dl of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[Decln]") $$ acc
    (d:ds) ->
      ddeclns parent idx (idx' + 1) (acc $$ ddecln node idx' d) ds
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- ConstExp
dconstexp :: Doc -> Int -> ConstExp -> Doc
dconstexp parent idx (ConstCondExp c) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "ConstCondExp") $$
  dcondexp node 0 c
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- Exp
dexp :: Doc -> Int -> Exp -> Doc
dexp parent idx (Exp el) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Exp") $$
  dassignexps node 0 0 empty el
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- AssignExp
dassignexp :: Doc -> Int -> AssignExp -> Doc
dassignexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    CondExp c ->
      dlabel node (text "CondExp") $$
      dcondexp node 0 c
    Assign u a ->
      dlabel node (text "Assign (=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    MulAssign u a ->
      dlabel node (text "MulAssign (*=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    DivAssign u a ->
      dlabel node (text "DivAssign (/=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    ModAssign u a ->
      dlabel node (text "ModAssign (%=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    AddAssign u a ->
      dlabel node (text "AddAssign (+=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    SubAssign u a ->
      dlabel node (text "SubAssign (/=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    LShftAssign u a ->
      dlabel node (text "LShftAssign (<<=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    RShftAssign u a ->
      dlabel node (text "RShftAssign (>>=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    AndAssign u a ->
      dlabel node (text "AndAssign (&=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    XorAssign u a ->
      dlabel node (text "XorAssign (^=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
    OrAssign u a ->
      dlabel node (text "OrAssign (|=)") $$
      dunaryexp node 0 u $$
      dassignexp node 1 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

dassignexps :: Doc -> Int -> Int -> Doc -> [AssignExp] -> Doc
dassignexps parent idx idx' acc al =
  case al of
    [] ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "[AssignExp]") $$ acc
    (a:as) ->
      dassignexps parent idx (idx' + 1) (acc $$ dassignexp node idx' a) as
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- CondExp
dcondexp :: Doc -> Int -> CondExp -> Doc
dcondexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    LgcOrExp o ->
      dlabel node (text "LgcOrExp") $$
      dlgcorexp node 0 o
    Cond o e c ->
      dlabel node (text "Cond (?:)") $$
      dlgcorexp node 0 o $$
      dexp node 1 e $$
      dcondexp node 2 c
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- LgcOrExp
dlgcorexp :: Doc -> Int -> LgcOrExp -> Doc
dlgcorexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    LgcAndExp a ->
      dlabel node (text "LgcAndExp") $$
      dlgcandexp node 0 a
    LgcOr o a ->
      dlabel node (text "LgcOr (||)") $$
      dlgcorexp node 0 o $$
      dlgcandexp node 1 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- LgcAndExp
dlgcandexp :: Doc -> Int -> LgcAndExp -> Doc
dlgcandexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    OrExp o ->
      dlabel node (text "OrExp") $$
      dorexp node 0 o
    LgcAnd a o ->
      dlabel node (text "LgcAnd (&&)") $$
      dlgcandexp node 0 a $$
      dorexp node 1 o
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- OrExp
dorexp :: Doc -> Int -> OrExp -> Doc
dorexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    XorExp a ->
      dlabel node (text "XorExp") $$
      dxorexp node 0 a
    Or x a ->
      dlabel node (text "Or (|)") $$
      dorexp node 0 x $$
      dxorexp node 1 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- XorExp
dxorexp :: Doc -> Int -> XorExp -> Doc
dxorexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    AndExp a ->
      dlabel node (text "AndExp") $$
      dandexp node 0 a
    Xor x a ->
      dlabel node (text "Xor (^)") $$
      dxorexp node 0 x $$
      dandexp node 1 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- AndExp
dandexp :: Doc -> Int -> AndExp -> Doc
dandexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    EqExp e' ->
      dlabel node (text "EqExp") $$
      deqexp node 0 e'
    And a e ->
      dlabel node (text "And (&)") $$
      dandexp node 0 a $$
      deqexp node 1 e
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- EqExp
deqexp :: Doc -> Int -> EqExp -> Doc
deqexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    RelExp r ->
      dlabel node (text "RelExp") $$
      drelexp node 0 r
    Eq e r ->
      dlabel node (text "Eq (==)") $$
      deqexp node 0 e $$
      drelexp node 1 r
    Neq e r ->
      dlabel node (text "Neq (!=)") $$
      deqexp node 0 e $$
      drelexp node 1 r
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- RelExp
drelexp :: Doc -> Int -> RelExp -> Doc
drelexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    ShftExp s ->
      dlabel node (text "ShftExp") $$
      dshftexp node 0 s
    Lt r s ->
      dlabel node (text "Lt (<)") $$
      drelexp node 0 r $$
      dshftexp node 1 s
    Gt r s ->
      dlabel node (text "Lt (>)") $$
      drelexp node 0 r $$
      dshftexp node 1 s
    Le r s ->
      dlabel node (text "Le (<=)") $$
      drelexp node 0 r $$
      dshftexp node 1 s
    Ge r s ->
      dlabel node (text "Ge (>=)") $$
      drelexp node 0 r $$
      dshftexp node 1 s
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- ShftExp
dshftexp :: Doc -> Int -> ShftExp -> Doc
dshftexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    AddExp a ->
      dlabel node (text "AddExp") $$
      daddexp node 0 a
    LShft s a ->
      dlabel node (text "LShft (<<)") $$
      dshftexp node 0 s $$
      daddexp node 1 a
    RShft s a ->
      dlabel node (text "RShft (>>)") $$
      dshftexp node 0 s $$
      daddexp node 1 a
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- AddExp
daddexp :: Doc -> Int -> AddExp -> Doc
daddexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    MulExp m ->
      dlabel node (text "MulExp") $$
      dmulexp node 0 m
    Add a m ->
      dlabel node (text "Add (+)") $$
      daddexp node 0 a $$
      dmulexp node 1 m
    Sub a m ->
      dlabel node (text "Sub (-)") $$
      daddexp node 0 a $$
      dmulexp node 1 m
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- MulExp
dmulexp :: Doc -> Int -> MulExp -> Doc
dmulexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    CastExp c ->
      dlabel node (text "CastExp") $$
      dcastexp node 0 c
    Mul m c ->
      dlabel node (text "Mul (*)") $$
      dmulexp node 0 m $$
      dcastexp node 1 c
    Div m c ->
      dlabel node (text "Div (/)") $$
      dmulexp node 0 m $$
      dcastexp node 1 c
    Mod m c ->
      dlabel node (text "Mod (%)") $$
      dmulexp node 0 m $$
      dcastexp node 1 c
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- CastExp
dcastexp :: Doc -> Int -> CastExp -> Doc
dcastexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    UnaryExp u ->
      dlabel node (text "UnaryExp") $$
      dunaryexp node 0 u
    TyCast t c ->
      dlabel node (text "TyCast") $$
      dtyname node 0 t $$
      dcastexp node 1 c
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- UnaryExp
dunaryexp :: Doc -> Int -> UnaryExp -> Doc
dunaryexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    PostfixExp p ->
      dlabel node (text "PostfixExp") $$
      dpostfixexp node 0 p
    UnaryInc u ->
      dlabel node (text "UnaryInc (++)") $$
      dunaryexp node 0 u
    UnaryDec u ->
      dlabel node (text "UnaryDec (--)") $$
      dunaryexp node 0 u
    AddrOf c ->
      dlabel node (text "AddrOf (&)") $$
      dcastexp node 0 c
    PtrDeref c ->
      dlabel node (text "PtrDeref (*)") $$
      dcastexp node 0 c
    Positive c ->
      dlabel node (text "Positive (+)") $$
      dcastexp node 0 c
    Negative c ->
      dlabel node (text "Negative (-)") $$
      dcastexp node 0 c
    BitNot c ->
      dlabel node (text "BitNot (~)") $$
      dcastexp node 0 c
    LgcNot c ->
      dlabel node (text "LgcNot (!)") $$
      dcastexp node 0 c
    UnaryExpSize u ->
      dlabel node (text "UnaryExpSize (sizeof)") $$
      dunaryexp node 0 u
    TySize t ->
      dlabel node (text "TySize (sizeof)") $$
      dtyname node 0 t
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- ArgExpList
dargexplist :: Doc -> Int -> Doc -> ArgExpList -> Doc
dargexplist parent idx acc (ArgExpList ael) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "ArgExpList") $$
  dassignexps node 0 0 empty ael
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- PostfixExp
dpostfixexp :: Doc -> Int -> PostfixExp -> Doc
dpostfixexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    PrimaryExp p ->
      dlabel node (text "PrimaryExp") $$
      dprimaryexp node 0 p
    ArrayElem p e ->
      dlabel node (text "ArrayElem ([])") $$
      dpostfixexp node 0 p $$
      dexp node 1 e
    FuncCall p a ->
      dlabel node (text "FuncCall") $$
      dpostfixexp node 0 p $$
      dargexplist node 1 empty a
    StructMem p i ->
      dlabel node (text "StructMem (.)") $$
      dpostfixexp node 0 p $$
      dident node 1 i
    StructPtrMem p i ->
      dlabel node (text "StructPtrMem (->)") $$
      dpostfixexp node 0 p $$
      dident node 1 i
    PostInc p ->
      dlabel node (text "PostInc (++)") $$
      dpostfixexp node 0 p
    PostDec p ->
      dlabel node (text "PostDec (--)") $$
      dpostfixexp node 0 p
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- PrimaryExp
dprimaryexp :: Doc -> Int -> PrimaryExp -> Doc
dprimaryexp parent idx e =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  case e of
    IdentExp i ->
      dlabel node (text "IdentExp") $$
      dident node 0 i
    ConstExp c ->
      dlabel node (text "ConstExp") $$
      dconst node 0 c
    StrLitExp s ->
      dlabel node (text "StrLitExp") $$
      dstrlit node 0 s
    NestedExp e ->
      dlabel node (text "Nested Exp") $$
      dexp node 0 e
  where
    node :: Doc
    node = parent <> text "-" <> int idx

-- StrLit
dstrlit :: Doc -> Int -> StrLit -> Doc
dstrlit parent idx (StrLit s) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "StrLit") $$
  doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
  dlabel node' (text s)
  where
    node :: Doc
    node = parent <> text "-" <> int idx
    node' :: Doc
    node' = node <> text "-" <> int 0

-- Constant
dconst :: Doc -> Int -> Const -> Doc
dconst parent idx c =
  case c of
    IntegerConst i ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "IntegerConst") $$
      doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
      dlabel node' (integer i)
    FloatConst f ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "FloatConst") $$
      doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
      dlabel node' (float f)
    DoubleConst d ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "DoubleConst") $$
      doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
      dlabel node' (double d)
    EnumConst (Ident i) ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "EnumConst") $$
      doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
      dlabel node' (text i)
    CharConst c         ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "CharConst") $$
      doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
      dlabel node' (quotes $ char c)
    Dim3Const as ->
      doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
      dlabel node (text "Dim3Const") $$
      dassignexps node 0 0 empty as
  where
    node :: Doc
    node = parent <> text "-" <> int idx
    node' :: Doc
    node' = node <> text "-" <> int 0

-- Identifier
dident :: Doc -> Int -> Ident -> Doc
dident parent idx (Ident i) =
  doubleQuotes parent <+> text "->" <+> doubleQuotes node $$
  dlabel node (text "Ident") $$
  doubleQuotes node <+> text "->" <+> doubleQuotes node' $$
  dlabel node' (text i)
  where
    node :: Doc
    node = parent <> text "-" <> int idx
    node' :: Doc
    node' = node <> text "-" <> int 0

dlabel :: Doc -> Doc -> Doc
dlabel n l = doubleQuotes n <+> text "[label=\"" <> l <> text "\"];"

------------------------------------------------------------------------------
-- Conversion functions
------------------------------------------------------------------------------
class CUDAExp a where
    toAssignExp  :: a -> AssignExp
    toCondExp    :: a -> CondExp
    toLgcOrExp   :: a -> LgcOrExp
    toLgcAndExp  :: a -> LgcAndExp
    toOrExp      :: a -> OrExp
    toXorExp     :: a -> XorExp
    toAndExp     :: a -> AndExp
    toEqExp      :: a -> EqExp
    toRelExp     :: a -> RelExp
    toShftExp    :: a -> ShftExp
    toAddExp     :: a -> AddExp
    toMulExp     :: a -> MulExp
    toCastExp    :: a -> CastExp
    toUnaryExp   :: a -> UnaryExp
    toPostfixExp :: a -> PostfixExp
    toPrimaryExp :: a -> PrimaryExp
    toArrayElem  :: Ident -> a -> PostfixExp
instance CUDAExp AssignExp where
    toAssignExp  e           = e
    toCondExp    (CondExp e) = toCondExp e
    toLgcOrExp   (CondExp e) = toLgcOrExp e
    toLgcAndExp  (CondExp e) = toLgcAndExp e
    toOrExp      (CondExp e) = toOrExp e
    toXorExp     (CondExp e) = toXorExp e
    toAndExp     (CondExp e) = toAndExp e
    toEqExp      (CondExp e) = toEqExp e
    toRelExp     (CondExp e) = toRelExp e
    toShftExp    (CondExp e) = toShftExp e
    toAddExp     (CondExp e) = toAddExp e
    toMulExp     (CondExp e) = toMulExp e
    toCastExp    (CondExp e) = toCastExp e
    toUnaryExp   (CondExp e) = toUnaryExp e
    toPostfixExp (CondExp e) = toPostfixExp e
    toPrimaryExp (CondExp e) = toPrimaryExp e
    toArrayElem i e = ArrayElem (PrimaryExp $ IdentExp i) (Exp [e])
instance CUDAExp CondExp where
    toAssignExp  e            = toAssignExp (CondExp e)
    toCondExp    e            = e
    toLgcOrExp   (LgcOrExp e) = toLgcOrExp e
    toLgcAndExp  (LgcOrExp e) = toLgcAndExp e
    toOrExp      (LgcOrExp e) = toOrExp e
    toXorExp     (LgcOrExp e) = toXorExp e
    toAndExp     (LgcOrExp e) = toAndExp e
    toEqExp      (LgcOrExp e) = toEqExp e
    toRelExp     (LgcOrExp e) = toRelExp e
    toShftExp    (LgcOrExp e) = toShftExp e
    toAddExp     (LgcOrExp e) = toAddExp e
    toMulExp     (LgcOrExp e) = toMulExp e
    toCastExp    (LgcOrExp e) = toCastExp e
    toUnaryExp   (LgcOrExp e) = toUnaryExp e
    toPostfixExp (LgcOrExp e) = toPostfixExp e
    toPrimaryExp (LgcOrExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (CondExp e)
instance CUDAExp LgcOrExp where
    toAssignExp  e             = toAssignExp (LgcOrExp e)
    toCondExp    e             = toCondExp (LgcOrExp e)
    toLgcOrExp   e             = e
    toLgcAndExp  (LgcAndExp e) = toLgcAndExp e
    toOrExp      (LgcAndExp e) = toOrExp e
    toXorExp     (LgcAndExp e) = toXorExp e
    toAndExp     (LgcAndExp e) = toAndExp e
    toEqExp      (LgcAndExp e) = toEqExp e
    toRelExp     (LgcAndExp e) = toRelExp e
    toShftExp    (LgcAndExp e) = toShftExp e
    toAddExp     (LgcAndExp e) = toAddExp e
    toMulExp     (LgcAndExp e) = toMulExp e
    toCastExp    (LgcAndExp e) = toCastExp e
    toUnaryExp   (LgcAndExp e) = toUnaryExp e
    toPostfixExp (LgcAndExp e) = toPostfixExp e
    toPrimaryExp (LgcAndExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (LgcOrExp e)
instance CUDAExp LgcAndExp where
    toAssignExp  e         = toAssignExp (LgcAndExp e)
    toCondExp    e         = toCondExp (LgcAndExp e)
    toLgcOrExp   e         = toLgcOrExp (LgcAndExp e)
    toLgcAndExp  e         = e
    toOrExp      (OrExp e) = toOrExp e
    toXorExp     (OrExp e) = toXorExp e
    toAndExp     (OrExp e) = toAndExp e
    toEqExp      (OrExp e) = toEqExp e
    toRelExp     (OrExp e) = toRelExp e
    toShftExp    (OrExp e) = toShftExp e
    toAddExp     (OrExp e) = toAddExp e
    toMulExp     (OrExp e) = toMulExp e
    toCastExp    (OrExp e) = toCastExp e
    toUnaryExp   (OrExp e) = toUnaryExp e
    toPostfixExp (OrExp e) = toPostfixExp e
    toPrimaryExp (OrExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (LgcAndExp e)
instance CUDAExp OrExp where
    toAssignExp  e          = toAssignExp (OrExp e)
    toCondExp    e          = toCondExp (OrExp e)
    toLgcOrExp   e          = toLgcOrExp (OrExp e)
    toLgcAndExp  e          = toLgcAndExp (OrExp e)
    toOrExp      e          = e
    toXorExp     (XorExp e) = toXorExp e
    toAndExp     (XorExp e) = toAndExp e
    toEqExp      (XorExp e) = toEqExp e
    toRelExp     (XorExp e) = toRelExp e
    toShftExp    (XorExp e) = toShftExp e
    toAddExp     (XorExp e) = toAddExp e
    toMulExp     (XorExp e) = toMulExp e
    toCastExp    (XorExp e) = toCastExp e
    toUnaryExp   (XorExp e) = toUnaryExp e
    toPostfixExp (XorExp e) = toPostfixExp e
    toPrimaryExp (XorExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (OrExp e)
instance CUDAExp XorExp where
    toAssignExp  e          = toAssignExp (XorExp e)
    toCondExp    e          = toCondExp (XorExp e)
    toLgcOrExp   e          = toLgcOrExp (XorExp e)
    toLgcAndExp  e          = toLgcAndExp (XorExp e)
    toOrExp      e          = toOrExp (XorExp e)
    toXorExp     e          = e
    toAndExp     (AndExp e) = toAndExp e
    toEqExp      (AndExp e) = toEqExp e
    toRelExp     (AndExp e) = toRelExp e
    toShftExp    (AndExp e) = toShftExp e
    toAddExp     (AndExp e) = toAddExp e
    toMulExp     (AndExp e) = toMulExp e
    toCastExp    (AndExp e) = toCastExp e
    toUnaryExp   (AndExp e) = toUnaryExp e
    toPostfixExp (AndExp e) = toPostfixExp e
    toPrimaryExp (AndExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (XorExp e)
instance CUDAExp AndExp where
    toAssignExp  e         = toAssignExp (AndExp e)
    toCondExp    e         = toCondExp (AndExp e)
    toLgcOrExp   e         = toLgcOrExp (AndExp e)
    toLgcAndExp  e         = toLgcAndExp (AndExp e)
    toOrExp      e         = toOrExp (AndExp e)
    toXorExp     e         = toXorExp (AndExp e)
    toAndExp     e         = e
    toEqExp      (EqExp e) = toEqExp e
    toRelExp     (EqExp e) = toRelExp e
    toShftExp    (EqExp e) = toShftExp e
    toAddExp     (EqExp e) = toAddExp e
    toMulExp     (EqExp e) = toMulExp e
    toCastExp    (EqExp e) = toCastExp e
    toUnaryExp   (EqExp e) = toUnaryExp e
    toPostfixExp (EqExp e) = toPostfixExp e
    toPrimaryExp (EqExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (AndExp e)
instance CUDAExp EqExp where
    toAssignExp  e          = toAssignExp (EqExp e)
    toCondExp    e          = toCondExp (EqExp e)
    toLgcOrExp   e          = toLgcOrExp (EqExp e)
    toLgcAndExp  e          = toLgcAndExp (EqExp e)
    toOrExp      e          = toOrExp (EqExp e)
    toXorExp     e          = toXorExp (EqExp e)
    toAndExp     e          = toAndExp (EqExp e)
    toEqExp      e          = e
    toRelExp     (RelExp e) = toRelExp e
    toShftExp    (RelExp e) = toShftExp e
    toAddExp     (RelExp e) = toAddExp e
    toMulExp     (RelExp e) = toMulExp e
    toCastExp    (RelExp e) = toCastExp e
    toUnaryExp   (RelExp e) = toUnaryExp e
    toPostfixExp (RelExp e) = toPostfixExp e
    toPrimaryExp (RelExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (EqExp e)
instance CUDAExp RelExp where
    toAssignExp  e           = toAssignExp (RelExp e)
    toCondExp    e           = toCondExp (RelExp e)
    toLgcOrExp   e           = toLgcOrExp (RelExp e)
    toLgcAndExp  e           = toLgcAndExp (RelExp e)
    toOrExp      e           = toOrExp (RelExp e)
    toXorExp     e           = toXorExp (RelExp e)
    toAndExp     e           = toAndExp (RelExp e)
    toEqExp      e           = toEqExp (RelExp e)
    toRelExp     e           = e
    toShftExp    (ShftExp e) = toShftExp e
    toAddExp     (ShftExp e) = toAddExp e
    toMulExp     (ShftExp e) = toMulExp e
    toCastExp    (ShftExp e) = toCastExp e
    toUnaryExp   (ShftExp e) = toUnaryExp e
    toPostfixExp (ShftExp e) = toPostfixExp e
    toPrimaryExp (ShftExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (RelExp e)
instance CUDAExp ShftExp where
    toAssignExp  e          = toAssignExp (ShftExp e)
    toCondExp    e          = toCondExp (ShftExp e)
    toLgcOrExp   e          = toLgcOrExp (ShftExp e)
    toLgcAndExp  e          = toLgcAndExp (ShftExp e)
    toOrExp      e          = toOrExp (ShftExp e)
    toXorExp     e          = toXorExp (ShftExp e)
    toAndExp     e          = toAndExp (ShftExp e)
    toEqExp      e          = toEqExp (ShftExp e)
    toRelExp     e          = toRelExp (ShftExp e)
    toShftExp    e          = e
    toAddExp     (AddExp e) = toAddExp e
    toMulExp     (AddExp e) = toMulExp e
    toCastExp    (AddExp e) = toCastExp e
    toUnaryExp   (AddExp e) = toUnaryExp e
    toPostfixExp (AddExp e) = toPostfixExp e
    toPrimaryExp (AddExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (ShftExp e)
instance CUDAExp AddExp where
    toAssignExp  e          = toAssignExp (AddExp e)
    toCondExp    e          = toCondExp (AddExp e)
    toLgcOrExp   e          = toLgcOrExp (AddExp e)
    toLgcAndExp  e          = toLgcAndExp (AddExp e)
    toOrExp      e          = toOrExp (AddExp e)
    toXorExp     e          = toXorExp (AddExp e)
    toAndExp     e          = toAndExp (AddExp e)
    toEqExp      e          = toEqExp (AddExp e)
    toRelExp     e          = toRelExp (AddExp e)
    toShftExp    e          = toShftExp (AddExp e)
    toAddExp     e          = e
    toMulExp     (MulExp e) = toMulExp e
    toCastExp    (MulExp e) = toCastExp e
    toUnaryExp   (MulExp e) = toUnaryExp e
    toPostfixExp (MulExp e) = toPostfixExp e
    toPrimaryExp (MulExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (AddExp e)
instance CUDAExp MulExp where
    toAssignExp  e           = toAssignExp (MulExp e)
    toCondExp    e           = toCondExp (MulExp e)
    toLgcOrExp   e           = toLgcOrExp (MulExp e)
    toLgcAndExp  e           = toLgcAndExp (MulExp e)
    toOrExp      e           = toOrExp (MulExp e)
    toXorExp     e           = toXorExp (MulExp e)
    toAndExp     e           = toAndExp (MulExp e)
    toEqExp      e           = toEqExp (MulExp e)
    toRelExp     e           = toRelExp (MulExp e)
    toShftExp    e           = toShftExp (MulExp e)
    toAddExp     e           = toAddExp (MulExp e)
    toMulExp     e           = e
    toCastExp    (CastExp e) = toCastExp e
    toUnaryExp   (CastExp e) = toUnaryExp e
    toPostfixExp (CastExp e) = toPostfixExp e
    toPrimaryExp (CastExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (MulExp e)
instance CUDAExp CastExp where
    toAssignExp  e            = toAssignExp (CastExp e)
    toCondExp    e            = toCondExp (CastExp e)
    toLgcOrExp   e            = toLgcOrExp (CastExp e)
    toLgcAndExp  e            = toLgcAndExp (CastExp e)
    toOrExp      e            = toOrExp (CastExp e)
    toXorExp     e            = toXorExp (CastExp e)
    toAndExp     e            = toAndExp (CastExp e)
    toEqExp      e            = toEqExp (CastExp e)
    toRelExp     e            = toRelExp (CastExp e)
    toShftExp    e            = toShftExp (CastExp e)
    toAddExp     e            = toAddExp (CastExp e)
    toMulExp     e            = toMulExp (CastExp e)
    toCastExp    e            = e
    toUnaryExp   (UnaryExp e) = toUnaryExp e
    toPostfixExp (UnaryExp e) = toPostfixExp e
    toPrimaryExp (UnaryExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (CastExp e)
instance CUDAExp UnaryExp where
    toAssignExp  e              = toAssignExp (UnaryExp e)
    toCondExp    e              = toCondExp (UnaryExp e)
    toLgcOrExp   e              = toLgcOrExp (UnaryExp e)
    toLgcAndExp  e              = toLgcAndExp (UnaryExp e)
    toOrExp      e              = toOrExp (UnaryExp e)
    toXorExp     e              = toXorExp (UnaryExp e)
    toAndExp     e              = toAndExp (UnaryExp e)
    toEqExp      e              = toEqExp (UnaryExp e)
    toRelExp     e              = toRelExp (UnaryExp e)
    toShftExp    e              = toShftExp (UnaryExp e)
    toAddExp     e              = toAddExp (UnaryExp e)
    toMulExp     e              = toMulExp (UnaryExp e)
    toCastExp    e              = toCastExp (UnaryExp e)
    toUnaryExp   e              = e
    toPostfixExp (PostfixExp e) = toPostfixExp e
    toPrimaryExp (PostfixExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (UnaryExp e)
instance CUDAExp PostfixExp where
    toAssignExp  e              = toAssignExp (PostfixExp e)
    toCondExp    e              = toCondExp (PostfixExp e)
    toLgcOrExp   e              = toLgcOrExp (PostfixExp e)
    toLgcAndExp  e              = toLgcAndExp (PostfixExp e)
    toOrExp      e              = toOrExp (PostfixExp e)
    toXorExp     e              = toXorExp (PostfixExp e)
    toAndExp     e              = toAndExp (PostfixExp e)
    toEqExp      e              = toEqExp (PostfixExp e)
    toRelExp     e              = toRelExp (PostfixExp e)
    toShftExp    e              = toShftExp (PostfixExp e)
    toAddExp     e              = toAddExp (PostfixExp e)
    toMulExp     e              = toMulExp (PostfixExp e)
    toCastExp    e              = toCastExp (PostfixExp e)
    toUnaryExp   e              = toUnaryExp (PostfixExp e)
    toPostfixExp e              = e
    toPrimaryExp (PrimaryExp e) = toPrimaryExp e
    toArrayElem i e = toArrayElem i (PostfixExp e)
instance CUDAExp PrimaryExp where
    toAssignExp  e = toAssignExp (PrimaryExp e)
    toCondExp    e = toCondExp (PrimaryExp e)
    toLgcOrExp   e = toLgcOrExp (PrimaryExp e)
    toLgcAndExp  e = toLgcAndExp (PrimaryExp e)
    toOrExp      e = toOrExp (PrimaryExp e)
    toXorExp     e = toXorExp (PrimaryExp e)
    toAndExp     e = toAndExp (PrimaryExp e)
    toEqExp      e = toEqExp (PrimaryExp e)
    toRelExp     e = toRelExp (PrimaryExp e)
    toShftExp    e = toShftExp (PrimaryExp e)
    toAddExp     e = toAddExp (PrimaryExp e)
    toMulExp     e = toMulExp (PrimaryExp e)
    toCastExp    e = toCastExp (PrimaryExp e)
    toUnaryExp   e = toUnaryExp (PrimaryExp e)
    toPostfixExp e = toPostfixExp (PrimaryExp e)
    toPrimaryExp e = e
    toArrayElem i e = toArrayElem i (PrimaryExp e)
instance CUDAExp Const where
    toAssignExp  e = toAssignExp (ConstExp e)
    toCondExp    e = toCondExp (ConstExp e)
    toLgcOrExp   e = toLgcOrExp (ConstExp e)
    toLgcAndExp  e = toLgcAndExp (ConstExp e)
    toOrExp      e = toOrExp (ConstExp e)
    toXorExp     e = toXorExp (ConstExp e)
    toAndExp     e = toAndExp (ConstExp e)
    toEqExp      e = toEqExp (ConstExp e)
    toRelExp     e = toRelExp (ConstExp e)
    toShftExp    e = toShftExp (ConstExp e)
    toAddExp     e = toAddExp (ConstExp e)
    toMulExp     e = toMulExp (ConstExp e)
    toCastExp    e = toCastExp (ConstExp e)
    toUnaryExp   e = toUnaryExp  (ConstExp e)
    toPostfixExp e = toPostfixExp (ConstExp e)
    toPrimaryExp e = toPrimaryExp (ConstExp e)
    toArrayElem i e = toArrayElem i (ConstExp e)
instance CUDAExp Ident where
    toAssignExp  e = toAssignExp (IdentExp e)
    toCondExp    e = toCondExp (IdentExp e)
    toLgcOrExp   e = toLgcOrExp (IdentExp e)
    toLgcAndExp  e = toLgcAndExp (IdentExp e)
    toOrExp      e = toOrExp (IdentExp e)
    toXorExp     e = toXorExp (IdentExp e)
    toAndExp     e = toAndExp (IdentExp e)
    toEqExp      e = toEqExp (IdentExp e)
    toRelExp     e = toRelExp (IdentExp e)
    toShftExp    e = toShftExp (IdentExp e)
    toAddExp     e = toAddExp (IdentExp e)
    toMulExp     e = toMulExp (IdentExp e)
    toCastExp    e = toCastExp (IdentExp e)
    toUnaryExp   e = toUnaryExp (IdentExp e)
    toPostfixExp e = toPostfixExp (IdentExp e)
    toPrimaryExp e = toPrimaryExp (IdentExp e)
    toArrayElem i e = toArrayElem i (IdentExp e)
