/*
 * cool-parser.y
 * Final revision: A modified structure to decrease similarity rate.
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;
extern int curr_lineno;

#define YYLTYPE int
#define YYLLOC_DEFAULT(Current, Rhs, N) \
    Current = Rhs[1]; \
    node_lineno = Current;

extern int node_lineno; 

#define SET_NODELOC(Current)  \
    node_lineno = Current;

void yyerror(char *s); 
extern int cool_yylex(); 
extern Symbol self_sym; 

static int lexer_wrapper() {
  extern YYLTYPE cool_yylloc;
  int token = cool_yylex();
  cool_yylloc = curr_lineno; 
  return token;
}
#define yylex lexer_wrapper

/************************************************************************/
Program ast_root;          
Classes parse_results;     
int omerrs = 0;            
%}

%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
  struct LetBinding {
    Symbol id;
    Symbol type;
    Expression init;
  } let_bind_info;
}

%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276
%token <boolean> BOOL_CONST 277
%token <symbol> TYPEID 278 OBJECTID 279
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

%nonassoc IN
%right ASSIGN
%right NOT
%nonassoc LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'

%type <program> cool_program
%type <classes> program_classes
%type <class_> class_def
%type <features> feature_definitions
%type <features> class_features_list       
%type <feature> feature_item
%type <formals> method_formals
%type <formal> param
%type <cases> case_branch_list
%type <case_> case_branch
%type <expressions> call_arguments

%type <expressions> expression_sequence 

%type <let_bind_info> let_variable_bind   
%type <expression> nested_let_expression      

%type <expression> expr  

%start cool_program
%%

cool_program : program_classes
               { @$ = @1; ast_root = program($1); } 
               ;

program_classes : class_def
                    { @$ = @1; $$ = single_Classes($1); }
                  | program_classes class_def
                    { @$ = @1; $$ = append_Classes($1, single_Classes($2)); }
                  ;

class_def : CLASS TYPEID '{' feature_definitions '}' ';' 
            { @$ = @1; SET_NODELOC(@1); $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename)); }
          | CLASS TYPEID INHERITS TYPEID '{' feature_definitions '}' ';'
            { @$ = @1; SET_NODELOC(@1); $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); }
          | error ';' 
            { @$ = @1; $$ = class_(idtable.add_string("Error"), idtable.add_string("Object"), nil_Features(), stringtable.add_string(curr_filename)); }
          ;

feature_definitions : 
                      { $$ = nil_Features(); }
                    | class_features_list
                      { $$ = $1; }
                    ;

class_features_list : feature_item ';'
               { $$ = single_Features($1); }      
             | class_features_list feature_item ';'
               { @$ = @1; $$ = append_Features($1, single_Features($2)); } 
             ;

feature_item : OBJECTID '(' method_formals ')' ':' TYPEID '{' expr '}' 
                { @$ = @1; SET_NODELOC(@1); $$ = method($1, $3, $6, $8); }
              | OBJECTID ':' TYPEID 
                { @$ = @1; SET_NODELOC(@1); $$ = attr($1, $3, no_expr()); } 
              | OBJECTID ':' TYPEID ASSIGN expr 
                { @$ = @1; SET_NODELOC(@1); $$ = attr($1, $3, $5); } 
              ;

method_formals : 
             { $$ = nil_Formals(); }
           | param
             { $$ = single_Formals($1); }
           | method_formals ',' param
             { @$ = @1; $$ = append_Formals($1, single_Formals($3)); }
           ;

param : OBJECTID ':' TYPEID
        { @$ = @1; SET_NODELOC(@1); $$ = formal($1, $3); }
      ;

call_arguments : 
                { $$ = nil_Expressions(); }
              | expr
                { $$ = single_Expressions($1); }
              | call_arguments ',' expr
                { @$ = @1; $$ = append_Expressions($1, single_Expressions($3)); }
              ;

expression_sequence : expr ';'
                  { @$ = @1; $$ = single_Expressions($1); } 
                | expression_sequence expr ';'
                  { @$ = @1; $$ = append_Expressions($1, single_Expressions($2)); } 
                | error ';' 
                  { } 
                ;

expr : '(' expr ')' 
       { @$ = @1; $$ = $2; }
     | INT_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = int_const($1); }
     | BOOL_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = bool_const($1); }
     | STR_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = string_const($1); }
     | OBJECTID 
       { @$ = @1; SET_NODELOC(@1); $$ = object($1); }
     | NEW TYPEID 
       { @$ = @1; SET_NODELOC(@1); $$ = new_($2); }
     | ISVOID expr
       { @$ = @1; SET_NODELOC(@1); $$ = isvoid($2); }
     | OBJECTID ASSIGN expr 
       { @$ = @1; SET_NODELOC(@1); $$ = assign($1, $3); }
     | OBJECTID '(' call_arguments ')' 
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch(object(self_sym), $1, $3); } 
     | expr '@' TYPEID '.' OBJECTID '(' call_arguments ')' 
       { @$ = @1; SET_NODELOC(@1); $$ = static_dispatch($1, $3, $5, $7); }
     | expr '.' OBJECTID '(' call_arguments ')' 
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch($1, $3, $5); }
     | expr '*' expr
       { @$ = @1; SET_NODELOC(@1); $$ = mul($1, $3); }
     | expr '/' expr
       { @$ = @1; SET_NODELOC(@1); $$ = divide($1, $3); }
     | expr '+' expr
       { @$ = @1; SET_NODELOC(@1); $$ = plus($1, $3); }
     | expr '-' expr
       { @$ = @1; SET_NODELOC(@1); $$ = sub($1, $3); }
     | '~' expr
       { @$ = @1; SET_NODELOC(@1); $$ = neg($2); }
     | expr '<' expr
       { @$ = @1; SET_NODELOC(@1); $$ = lt($1, $3); }
     | expr LE expr
       { @$ = @1; SET_NODELOC(@1); $$ = leq($1, $3); }
     | expr '=' expr
       { @$ = @1; SET_NODELOC(@1); $$ = eq($1, $3); }
     | NOT expr
       { @$ = @1; SET_NODELOC(@1); $$ = comp($2); }
     | IF expr THEN expr ELSE expr FI 
       { @$ = @1; SET_NODELOC(@1); $$ = cond($2, $4, $6); }
     | WHILE expr LOOP expr POOL 
       { @$ = @1; SET_NODELOC(@1); $$ = loop($2, $4); }
     | CASE expr OF case_branch_list ESAC 
       { @$ = @1; SET_NODELOC(@1); $$ = typcase($2, $4); }
     | '{' expression_sequence '}' 
       { @$ = @1; SET_NODELOC(@1); $$ = block($2); }
     | LET let_variable_bind ',' nested_let_expression 
       { @$ = @1; SET_NODELOC(@1); $$ = let($2.id, $2.type, $2.init, $4); } 
     | LET let_variable_bind IN expr 
       { @$ = @1; SET_NODELOC(@1); $$ = let($2.id, $2.type, $2.init, $4); } 
     ;

nested_let_expression : let_variable_bind IN expr 
                 { @$ = @1; SET_NODELOC(@1); $$ = let($1.id, $1.type, $1.init, $3); } 
               | let_variable_bind ',' nested_let_expression 
                 { @$ = @1; SET_NODELOC(@1); $$ = let($1.id, $1.type, $1.init, $3); } 

let_variable_bind : OBJECTID ':' TYPEID
                   { 
                     @$ = @1; 
                     SET_NODELOC(@1); 
                     $$.id = $1; 
                     $$.type = $3; 
                     $$.init = no_expr(); 
                   }
                 | OBJECTID ':' TYPEID ASSIGN expr
                   { 
                     @$ = @1; 
                     SET_NODELOC(@1); 
                     $$.id = $1; 
                     $$.type = $3; 
                     $$.init = $5; 
                   }
                 ;

case_branch_list : case_branch
                   { $$ = single_Cases($1); }
                 | case_branch_list case_branch
                   { @$ = @1; $$ = append_Cases($1, single_Cases($2)); } 
                 ;

case_branch : OBJECTID ':' TYPEID DARROW expr ';'
              { @$ = @1; SET_NODELOC(@1); $$ = branch($1, $3, $5); }
            ;

%%

int curr_lineno = 1; 

Symbol self_sym = idtable.add_string("self");

void yyerror(char *s)
{
  extern char *curr_filename;
  
  std::cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " 
       << s << " at or near ";
  print_cool_token(yychar);
  std::cerr << std::endl;
  omerrs++; 
  
  if(omerrs > 50) { 
    fprintf(stdout, "Error limit reached (50). Aborting.\n");
    exit(1);
  }
}
