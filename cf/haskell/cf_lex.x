--
--       Filename:  cf_lex.x
--
--    Description:  cf lexer (for 'alex' input)
--
--        Version:  1.0
--        Created:  01.10.2010
--
--         Author:  Alexey Radkov (), 
--        Company:  
--

{
module CfLexer where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$sign  = [\-\+]         -- sign


tokens :-

    $white+                                      ;
    -- "#".*                                        ;
    keep                                         { \s -> Tkeep             }
    delete                                       { \s -> Tdelete           }
    tpt                                          { \s -> Ttpt              }
    edt                                          { \s -> Tedt              }
    if                                           { \s -> Tif               }
    '-'?$digit+                                  { \s -> Tinteger $ read s }
    '-'?$digit+(\.$digit+)?([eE]$sign?$digit+)?  { \s -> Tdouble $ read s  }
    $alpha[$alpha $digit \_]*                    { \s -> Tidentifier s     }
    "("                                          { \s -> Tleftparen        }
    ")"                                          { \s -> Trightparen       }
    "["                                          { \s -> Tleftbracket      }
    "]"                                          { \s -> Trightbracket     }
    "+"                                          { \s -> Tplus             }
    "-"                                          { \s -> Tminus            }
    "*"                                          { \s -> Tmultiply         }
    "/"                                          { \s -> Tdivide           }
    "&"                                          { \s -> Tand              }
    "|"                                          { \s -> Tor               }
    "!"                                          { \s -> Tnot              }
    "<="                                         { \s -> Tlessorequal      }
    ">="                                         { \s -> Tmoreorequal      }
    "!="                                         { \s -> Tnotequal         }
    "<"                                          { \s -> Tless             }
    ">"                                          { \s -> Tmore             }
    "="                                          { \s -> Tequal            }
    ","                                          { \s -> Tcomma            }

{
-- Each action has type :: String -> Token

-- The token type:
data  Token =
    Tkeep              |
    Tdelete            |
    Ttpt               |
    Tedt               |
    Tif                |
    Tinteger Int       |
    Tdouble Double     |
    Tidentifier String |
    Tleftparen         |
    Trightparen        |
    Tleftbracket       |
    Trightbracket      |
    Tplus              |
    Tminus             |
    Tmultiply          |
    Tdivide            |
    Tand               |
    Tor                |
    Tnot               |
    Tlessorequal       |
    Tmoreorequal       |
    Tnotequal          |
    Tless              |
    Tmore              |
    Tequal             |
    Tcomma
    deriving ( Eq, Show )

printLex = do
    s <- getContents
    print $ alexScanTokens s
}

