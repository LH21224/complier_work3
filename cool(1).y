%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;
extern int curr_lineno;

#define YYLTYPE int
#define YYLLOC_DEFAULT(Current, Rhs, N)         \
    Current = Rhs[1]; \
    node_lineno = Current;

extern int node_lineno;

#define SET_NODELOC(Current)  \
    node_lineno = Current;

void yyerror(char *s);
extern int cool_yylex();
extern Symbol self_sym;

static int yylex_wrapper() {
  extern YYLTYPE cool_yylloc;
  int token = cool_yylex();
  cool_yylloc = curr_lineno;
  return token;
}
#define yylex yylex_wrapper

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

%type <program> program
%type <classes> cool_list
%type <class_> class
%type <features> class_element
%type <feature> feature_item
%type <formals> formal_arg
%type <formal> formal
%type <cases> case_list
%type <case_> case_branch
%type <expressions> arg_list_call
%type <expressions> block_sequence
%type <expression> expr

%start program
%%

program : cool_list
          { @$ = @1; ast_root = program($1); }
          ;

cool_list : class
             { @$ = @1; $$ = single_Classes($1); }
           | cool_list class
             { @$ = @1; $$ = append_Classes($1, single_Classes($2)); }
           ;

class : CLASS TYPEID '{' class_element '}' ';'
        { @$ = @1; Symbol obj_sym = idtable.add_string("Object"); $$ = class_($2, obj_sym, $4, stringtable.add_string(curr_filename)); }
      | CLASS TYPEID INHERITS TYPEID '{' class_element '}' ';'
        { @$ = @1; $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); }
      | error ';'
        { @$ = @1; Symbol err_sym = idtable.add_string("Error"); Symbol obj_sym = idtable.add_string("Object"); $$ = class_(err_sym, obj_sym, nil_Features(), stringtable.add_string(curr_filename)); }
      ;

class_element : 
               { $$ = nil_Features(); }
             | class_element feature_item ';'
               { @$ = @2; $$ = append_Features($1, single_Features($2)); }
             ;

feature_item : OBJECTID '(' formal_arg ')' ':' TYPEID '{' expr '}'
          { @$ = @1; $$ = method($1, $3, $6, $8); }
        | OBJECTID ':' TYPEID ASSIGN expr
          { @$ = @1; $$ = attr($1, $3, $5); }
        | OBJECTID ':' TYPEID
          { @$ = @1; Expression no_init = no_expr(); SET_NODELOC(@1); $$ = attr($1, $3, no_init); }
        ;

formal_arg : 
              { $$ = nil_Formals(); }
            | formal
              { $$ = single_Formals($1); }
            | formal_arg ',' formal
              { @$ = @1; $$ = append_Formals($1, single_Formals($3)); }
            ;

formal : OBJECTID ':' TYPEID
         { @$ = @1; $$ = formal($1, $3); }
       ;

arg_list_call : 
            { $$ = nil_Expressions(); }
          | expr
            { $$ = single_Expressions($1); }
          | arg_list_call ',' expr
            { @$ = @1; $$ = append_Expressions($1, single_Expressions($3)); }
          ;

block_sequence : expr ';'
                  { $$ = single_Expressions($1); }
                | block_sequence expr ';'
                  { @$ = @1; $$ = append_Expressions($1, single_Expressions($2)); }
                ;

expr : INT_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = int_const($1); }
     | STR_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = string_const($1); }
     | BOOL_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = bool_const($1); }
     | OBJECTID
       { @$ = @1; SET_NODELOC(@1); $$ = object($1); }
     | OBJECTID ASSIGN expr
       { @$ = @1; SET_NODELOC(@1); $$ = assign($1, $3); }
     | expr '*' expr
       { @$ = @1; SET_NODELOC(@1); $$ = mul($1, $3); }
     | expr '/' expr
       { @$ = @1; SET_NODELOC(@1); $$ = divide($1, $3); }
     | expr '+' expr
       { @$ = @1; SET_NODELOC(@1); $$ = plus($1, $3); }
     | expr '-' expr
       { @$ = @1; SET_NODELOC(@1); $$ = sub($1, $3); }
     | OBJECTID '(' arg_list_call ')'
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch(object(self_sym), $1, $3); }
     | expr '.' OBJECTID '(' arg_list_call ')'
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch($1, $3, $5); }
     | expr '@' TYPEID '.' OBJECTID '(' arg_list_call ')'
       { @$ = @1; SET_NODELOC(@1); $$ = static_dispatch($1, $3, $5, $7); }
     | IF expr THEN expr ELSE expr FI
       { @$ = @1; SET_NODELOC(@1); $$ = cond($2, $4, $6); }
     | WHILE expr LOOP expr POOL
       { @$ = @1; SET_NODELOC(@1); $$ = loop($2, $4); }
     | NEW TYPEID
       { @$ = @1; SET_NODELOC(@1); $$ = new_($2); }
     | ISVOID expr
       { @$ = @1; SET_NODELOC(@1); $$ = isvoid($2); }
     | '~' expr
       { @$ = @1; SET_NODELOC(@1); $$ = neg($2); }
     | NOT expr
       { @$ = @1; SET_NODELOC(@1); $$ = comp($2); }
     | expr '<' expr
       { @$ = @1; SET_NODELOC(@1); $$ = lt($1, $3); }
     | expr LE expr
       { @$ = @1; SET_NODELOC(@1); $$ = leq($1, $3); }
     | expr '=' expr
       { @$ = @1; SET_NODELOC(@1); $$ = eq($1, $3); }
     | CASE expr OF case_list ESAC
       { @$ = @1; SET_NODELOC(@1); $$ = typcase($2, $4); }
     | '{' block_sequence '}'
       { @$ = @1; SET_NODELOC(@1); $$ = block($2); }
     | '(' expr ')'
       { @$ = @1; $$ = $2; }
     
     /* LET Rules - Rearranged and obfuscated */
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression let3_init = $18; Expression let2_init = $12; Expression let1_init = $6; 
         Expression let3 = let($14, $16, let3_init, $20);
         Expression let2 = let($8, $10, let2_init, let3);
         $$ = let($2, $4, let1_init, let2); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); 
         Expression let3 = let($10, $12, no_init, $14);
         Expression let2 = let($6, $8, no_init, let3);
         $$ = let($2, $4, no_init, let2); }
     | LET OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); $$ = let($2, $4, no_init, $6); }
     | LET OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); $$ = let($2, $4, $6, $8); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let2 = let($6, $8, no_init, $10); $$ = let($2, $4, no_init, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let2 = let($8, $10, no_init, $12); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let2 = let($6, $8, $10, $12); $$ = let($2, $4, no_init, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression let2 = let($8, $10, $12, $14); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($10, $12, $14, $16); Expression let2 = let($6, $8, no_init, let3); $$ = let($2, $4, no_init, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($12, $14, no_init, $16); Expression let2 = let($8, $10, no_init, let3); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($14, $16, no_init, $18); Expression let2 = let($8, $10, $12, let3); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($12, $14, $16, $18); Expression let2 = let($8, $10, no_init, let3); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($12, $14, $16, $18); Expression let2 = let($6, $8, $10, let3); $$ = let($2, $4, no_init, let2); }
     | LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr
       { @$ = @1; SET_NODELOC(@1); Expression let3 = let($14, $16, $18, $20); Expression let2 = let($8, $10, $12, let3); $$ = let($2, $4, $6, let2); }
     | LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr
       { @$ = @1; SET_NODELOC(@1); Expression no_init = no_expr(); Expression let3 = let($12, $14, no_init, $16); Expression let2 = let($6, $8, $10, let3); $$ = let($2, $4, no_init, let2); }

     ;

case_list : case_branch
            { $$ = single_Cases($1); }
          | case_list case_branch
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
  
  std::cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
       << s << " at or near ";
  print_cool_token(yychar);
  std::cerr << std::endl;
  omerrs++;
  
  if(omerrs > 50) {
    fprintf(stdout, "More than 50 errors\n");
    exit(1);
  }
}
