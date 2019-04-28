%{
%}

%token          NEWLINE WS COMMA EOF LPAREN RPAREN LSQUARE RSQUARE COLON SEMICOLON EOL
%token          IF THEN ELSE LET IN FUN RTARROW END
%token          DOT PUSH TOP POP
%token          MAP ADD
%token          STACK
%token          ADD SUBTRACT EQ
%token          AND OR NOT
%token <int>    INTEGER
%token <string> ID
%token <bool>   BOOLEAN
%start main
%type <Expression.expr> expr
%type <Expression.expr> bool_expr

%left ADD SUBTRACT
%left AND OR
%right NOT

%type <Expression.expr> main

%%

main:
  expr EOL                                      { $1 }
;
expr :	  	
  | ID                                         { Expression.Id($1)                  } 
  | INTEGER                                    { Expression.IntConst $1             }
  | expr ADD expr                              { Expression.Add($1, $3)             }
  | expr SUBTRACT expr                         { Expression.Sub($1, $3)             }
  | LPAREN expr RPAREN                         { $2;                                }
  | IF bool_expr THEN expr ELSE expr           { Expression.If($2, $4, $6)          }
  | LET ID EQ expr IN expr                     { Expression.Let($2, $4, $6)         }
  | FUN ID RTARROW expr                        { Expression.FunDef($2, $4)          }
  | expr expr COLON                            { Expression.FunApp($1, $2)          }
  | LET ID EQ STACK                            { Expression.CreateStack($2)         }
  | ID DOT PUSH LPAREN INTEGER RPAREN          { Expression.PushToStack($1, $5)     }
  | ID DOT TOP LPAREN RPAREN                   { Expression.TopOfStack($1)          }
  | ID DOT POP LPAREN RPAREN                   { Expression.PopFromStack($1)        }
  | LET ID EQ MAP                              { Expression.CreateMap($2)           }
  | ID DOT ADD LPAREN INTEGER COMMA ID RPAREN  { Expression.AddMapping($1, $5, $7)  }
  | ID LSQUARE INTEGER RSQUARE                 { Expression.GetMapValue($1, $3)     }
  | END                                        { Expression.EndProgram              }
;

bool_expr:
    BOOLEAN { Expression.BoolConst $1 }
  | bool_expr AND bool_expr { Expression.And($1, $3) }
  | bool_expr OR bool_expr { Expression.Or($1, $3) }
  | NOT bool_expr { Expression.Not($2) }
;
%%
