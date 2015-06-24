--
--       Filename:  cf_parse.y
--
--    Description:  cf gramma (for 'happy' input)
--
--        Version:  1.0
--        Created:  01.10.2010
--
--         Author:  Alexey Radkov (), 
--        Company:  
--

{
module CfGrammar where
import CfLexer
}

%name cf_parse
%tokentype { Token }
%error { parseError }

%token
    keep        { Tkeep          }
    delete      { Tdelete        }
    tpt         { Ttpt           }
    edt         { Tedt           }
    if          { Tif            }
    integer     { Tinteger    $$ }
    double      { Tdouble     $$ }
    identifier  { Tidentifier $$ }
    '('         { Tleftparen     }
    ')'         { Trightparen    }
    '['         { Tleftbracket   }
    ']'         { Trightbracket  }
    '+'         { Tplus          }
    '-'         { Tminus         }
    '*'         { Tmultiply      }
    '/'         { Tdivide        }
    '&'         { Tand           }
    '|'         { Tor            }
    '!'         { Tnot           }
    "<="        { Tlessorequal   }
    ">="        { Tmoreorequal   }
    "!="        { Tnotequal      }
    "<"         { Tless          }
    ">"         { Tmore          }
    "="         { Tequal         }
    ','         { Tcomma         }

%%


Statement : Action Condition { ParseResult $1 $2 }

Action : keep tpt    { KeepTpt }
       | keep edt    { KeepEdt }
       | delete tpt  { DeleteTpt }
       | delete edt  { DeleteEdt }

Condition :: { Subtree }
Condition : if Expression { buildTopTree $2 }

Expression :: { Node }
Expression : OrExpression { $1 }

PrimaryExpression : Function1 { buildFunction1Subtree $1 }
                  | '(' Expression ')' { setPriorityInOperatorTree 0 $2 }
                  | LeafOperand { Leaf $1 }

LeafOperand : Constant { Constant $1 }
            | Variable { Variable $1 }

Constant : double { DoubleValue $1 }
         | integer { IntegerValue $1 }

Variable : identifier '[' integer ',' integer ']' { Vector2 $1 $3 $5 }
         | identifier '[' integer ']' { Vector1 $1 $3 }
         | identifier { Scalar $1 }

Function1 : identifier '(' Expression ')' { Function1 $1 $3 }

OrExpression : AndExpression '|' OrExpression
               { buildOperatorSubtree ( Op ( LogOp Or ) 1 False ) $1 $3 }
             | AndExpression { $1 }

AndExpression : Relation '&' AndExpression
                { buildOperatorSubtree ( Op ( LogOp And ) 2 False ) $1 $3 }
              | Relation { $1 }

Relation : Addition RelOperator Addition
           { buildOperatorSubtree ( Op ( RelOp $2 ) 3 False ) $1 $3 }
         | Addition { $1 }

Addition : Multiplication AddOperator Addition
           { buildOperatorSubtree ( Op ( AddOp $2 ) 4 False ) $1 $3 }
         | Multiplication { $1 }

Multiplication : UnaryExpression MultOperator Multiplication
                 { buildOperatorSubtree ( Op ( MultOp $2 ) 5 False ) $1 $3 }
               | UnaryExpression { $1 }

UnaryExpression : UnaryOperator PrimaryExpression
                  { buildUnaryOperatorSubtree ( Op ( UnaryOp $1 ) 6 True ) $2 }
                | PrimaryExpression { $1 }

UnaryOperator : '-' { UMinus }
              | '!' { Not }

MultOperator : '*' { Mult }
             | '/' { Div }

AddOperator : '+' { Plus }
            | '-' { Minus }

RelOperator : "<=" { LessEqual }
            | ">=" { MoreEqual }
            | "!=" { NotEqual }
            | "<" { Less }
            | ">" { More }
            | "=" { Equal }


{
parseError :: [ Token ] -> a
parseError _ = error "parse error"

buildTopTree :: Node -> Subtree
buildTopTree ( Tree x ) = x
buildTopTree x@( Leaf _ ) =
    Subtree ( Operator $ Op Top 0 False ) [ x ]

buildFunction1Subtree :: Function1 -> Node
buildFunction1Subtree x =
    Tree $ Subtree ( Function . FunctionName $ fName x ) [ fExpr x ]

buildOperatorSubtree :: Operator -> Node -> Node -> Node
buildOperatorSubtree a@( Op _ _ True ) nl nr =
    Tree $ Subtree ( Operator a ) [ nl, nr ]

buildOperatorSubtree a nl nr@( Tree sr@( Subtree ( Operator b ) ( cnrl : _ ) ) )
    | p == priority b =
        Tree $ moveDownLeft ( Subtree ( Operator a ) $
                                      nl : [ getDeepestLeft cnrl p ] ) sr
    | otherwise =
        Tree $ Subtree ( Operator a ) [ nl, nr ]
        where p = priority a

buildOperatorSubtree a nl nr =
    Tree $ Subtree ( Operator a ) [ nl, nr ]

getDeepestLeft :: Node -> Int -> Node
getDeepestLeft x@( Leaf _ ) _ = x

getDeepestLeft x@( Tree ( Subtree ( Operator b ) ( cnl : _ ) ) ) p
    | p == priority b = getDeepestLeft cnl p
    | otherwise = x

getDeepestLeft x _ = x

moveDownLeft :: Subtree -> Subtree -> Subtree
moveDownLeft sl ( Subtree b@( Operator _ ) ( ( Leaf _ ) : cnrr ) ) =
    Subtree b $ Tree sl : cnrr

moveDownLeft sl@( Subtree ( Operator _ ) _ )
             ( Subtree b@( Operator _ )
                       ( ( Tree ( Subtree ( Function _ ) _ ) ) : cnrr ) ) =
    Subtree b $ Tree sl : cnrr

moveDownLeft sl@( Subtree ( Operator a ) _ )
             ( Subtree b@( Operator _ )
                       ( ( Tree csrl@( Subtree ( Operator c ) _ ) ) : cnrr ) )
    | priority a == priority c =
        Subtree b $ Tree ( moveDownLeft sl csrl ) : cnrr
    | otherwise =
        Subtree b $ Tree sl : cnrr

moveDownLeft sl _ = sl

buildUnaryOperatorSubtree :: Operator -> Node -> Node
buildUnaryOperatorSubtree a x =
    Tree $ Subtree ( Operator a ) [ x ]

setPriorityInOperatorTree :: Int -> Node -> Node
setPriorityInOperatorTree i ( Tree ( Subtree ( Operator ( Op a _ x ) ) y ) ) =
    Tree $ Subtree ( Operator $ Op a i x ) y

setPriorityInOperatorTree _ x = x

printResult :: Statement -> IO ()
printResult x = do
    putStrLn $ "Result: action = " ++ show ( action x ) ++ ", parsed tree = "
    putStrLn ""
    printSubtree ( condition x ) ""

printSubtree :: Subtree -> String -> IO ()
printSubtree ( Subtree a b ) is = do
    putStrLn $ is ++ case a of
                        Operator aa -> show aa
                        Function aa -> show aa
    printChildren b $ is ++ "    "

printChildren :: [ Node ] -> String -> IO ()
printChildren [] _ = return ()

printChildren ( Tree x : xs ) is = do
    printSubtree x is
    printChildren xs is

printChildren ( Leaf x : xs ) is = do
    putStrLn $ is ++ printLeaf x
    printChildren xs is

printLeaf :: LeafOperand -> String
printLeaf a = case a of
                    Variable aa -> show aa
                    Constant aa -> show aa


instance Show Operator where
    show = showOperator

showOperator ( Op a _ _ ) =
    "-op- " ++ case a of
                    Uninitialized   -> "UNINITIALIZED"
                    Top             -> "Top"
                    UnaryOp UMinus  -> "u -"
                    UnaryOp Not     -> "!"
                    MultOp Mult     -> "*"
                    MultOp Div      -> "/"
                    AddOp Plus      -> "+"
                    AddOp Minus     -> "-"
                    RelOp LessEqual -> "<="
                    RelOp MoreEqual -> ">="
                    RelOp NotEqual  -> "!="
                    RelOp Less      -> "<"
                    RelOp More      -> ">"
                    RelOp Equal     -> "="
                    LogOp And       -> "&"
                    LogOp Or        -> "|"

instance Show Function where
    show = showFunction

showFunction ( FunctionName a ) = "-fun- " ++ a

instance Show Variable where
    show = showVariable

showVariable ( Scalar a ) = a
showVariable ( Vector1 a b ) = a ++ "[" ++ show b ++ "]"
showVariable ( Vector2 a b c ) = a ++ "[" ++ show b ++ "," ++ show c ++ "]"

instance Show Constant where
    show = showConstant

showConstant ( DoubleValue a ) = show a
showConstant ( IntegerValue a ) = show a


data  Statement =
    ParseResult
    {
        action    :: Action,
        condition :: Subtree
    }

data  Action =
    KeepTpt                  |
    KeepEdt                  |
    DeleteTpt                |
    DeleteEdt
    deriving Show

data  Node =
    Tree Subtree             |
    Leaf LeafOperand
    deriving Show

data  Subtree =
    Subtree
    {
        nodeType :: NodeType,
        children :: [ Node ]
    }
    deriving Show

data  NodeType =
    Operator  Operator       |
    Function  Function
    deriving Show

data  Operator =
    Op
    {
        op         :: OperatorType,
        priority   :: Int,
        hasRLAssoc :: Bool
    }

data  OperatorType =
    Uninitialized            |
    Top                      |
    UnaryOp  UnaryOperator   |
    MultOp   MultOperator    |
    AddOp    AddOperator     |
    RelOp    RelOperator     |
    LogOp    LogOperator

data UnaryOperator =
    UMinus                   |
    Not

data MultOperator =
    Mult                     |
    Div

data AddOperator =
    Plus |
    Minus

data RelOperator =
    LessEqual                |
    MoreEqual                |
    NotEqual                 |
    Less                     |
    More                     |
    Equal

data LogOperator =
    And |
    Or

data  Function =
    FunctionName  String

data  LeafOperand =
    Variable  Variable       |
    Constant  Constant
    deriving Show

data  Constant =
    DoubleValue   Double     |
    IntegerValue  Int

data  Variable =
    Scalar   String          |
    Vector1  String Int      |
    Vector2  String Int Int

data Function1 =
    Function1
    {
        fName :: String,
        fExpr :: Node
    }
}

-- vim: ft=haskell
