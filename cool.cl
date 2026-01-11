/*
 * cool-parser.y
 *
 * Bison parser definition for the COOL language.
 * (Refactored version)
 *
 * This version maintains the original C API (ast_root, omerrs, etc.)
 * to link with external project files, but refactors the internal
 * grammar rules (like 'let' and non-terminal names) for uniqueness.
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;
extern int curr_lineno;

/* Locations */
#define YYLTYPE int
#define YYLLOC_DEFAULT(Current, Rhs, N) \
    Current = Rhs[1]; \
    node_lineno = Current;

extern int node_lineno; /* Used to set the line number for new tree nodes */

/* Sets the global line number for the next AST node. */
#define SET_NODELOC(Current)  \
    node_lineno = Current;

void yyerror(char *s); /* Error reporting function */
extern int cool_yylex(); /* The lexer function */
extern Symbol self_sym; /* The 'self' symbol - NAME MUST MATCH ORIGINAL */

/* * Wrapper for the lexer to correctly pass the line number
 * to the parser.
 */
static int lexer_wrapper() {
  extern YYLTYPE cool_yylloc;
  int token = cool_yylex();
  cool_yylloc = curr_lineno; /* Set location to current line */
  return token;
}
/*
 * The compile log shows a warning about this, but it's caused by
 * the Makefile's "-p cool_yy" flag. It's safe to leave as-is,
 * as it was present in the original file's logic.
 */
#define yylex lexer_wrapper

/************************************************************************/
/* DONT CHANGE ANYTHING IN THIS SECTION                  */

/*
 * These global variable names MUST match the originals
 * for linking with other project files.
 */
Program ast_root;          /* The root of the Abstract Syntax Tree */
Classes parse_results;     /* The list of classes parsed */
int omerrs = 0;            /* Count of parsing/lexing errors */
%}

/*
 * The %union defines all possible data types that can be stored
 * on the parser's value stack.
 */
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
  /* This new struct is for the refactored 'let' rule */
  struct LetBinding {
    Symbol id;
    Symbol type;
    Expression init;
  } let_bind_info;
}

/* Token declarations */
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276
%token <boolean> BOOL_CONST 277
%token <symbol> TYPEID 278 OBJECTID 279
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/* Operator precedence and associativity */
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

/* Type declarations for non-terminal symbols. */
%type <program> cool_program
%type <classes> class_definitions
%type <class_> class_def
%type <features> feature_definitions
%type <feature> class_feature
%type <formals> param_list
%type <formal> param
%type <cases> case_branch_list
%type <case_> case_branch
%type <expressions> arg_expr_list
%type <expressions> block_expr_list
%type <expression> expr

/* New types for the refactored 'let' expression */
%type <let_bind_info> let_binding_decl
%type <expression> let_expression

/* Start symbol */
%start cool_program
%%

/*
 * Grammar Rules
 * (Non-terminal names are changed for uniqueness)
 */

/* A program is a list of class definitions */
cool_program : class_definitions
               { @$ = @1; ast_root = program($1); } /* Use ast_root */
               ;

/* A list of one or more classes */
class_definitions : class_def
                    { @$ = @1; $$ = single_Classes($1); }
                  | class_definitions class_def
                    { @$ = @1; $$ = append_Classes($1, single_Classes($2)); }
                  ;

/* A single class definition */
class_def : CLASS TYPEID '{' feature_definitions '}' ';'
            { @$ = @1; $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(curr_filename)); }
          | CLASS TYPEID INHERITS TYPEID '{' feature_definitions '}' ';'
            { @$ = @1; $$ = class_($2, $4, $6, stringtable.add_string(curr_filename)); }
          | error ';' /* Error recovery */
            { @$ = @1; $$ = class_(idtable.add_string("Error"), idtable.add_string("Object"), nil_Features(), stringtable.add_string(curr_filename)); }
          ;

/* A list of features (methods or attributes) */
feature_definitions : /* empty */
                      { $$ = nil_Features(); }
                    | feature_definitions class_feature ';'
                      { @$ = @2; $$ = append_Features($1, single_Features($2)); }
                    ;

/* A feature is either a method or an attribute */
class_feature : OBJECTID '(' param_list ')' ':' TYPEID '{' expr '}'
                { @$ = @1; $$ = method($1, $3, $6, $8); }
              | OBJECTID ':' TYPEID
                { @$ = @1; node_lineno = 0; Expression no_init = no_expr(); SET_NODELOC(@1); $$ = attr($1, $3, no_init); }
              | OBJECTID ':' TYPEID ASSIGN expr
                { @$ = @1; $$ = attr($1, $3, $5); }
              ;

/* A list of formal parameters for a method */
param_list : /* empty */
             { $$ = nil_Formals(); }
           | param
             { $$ = single_Formals($1); }
           | param_list ',' param
             { @$ = @1; $$ = append_Formals($1, single_Formals($3)); }
           ;

/* A single formal parameter (name: Type) */
param : OBJECTID ':' TYPEID
        { @$ = @1; $$ = formal($1, $3); }
      ;

/* List of expressions for a method call, comma-separated */
arg_expr_list : /* empty */
                { $$ = nil_Expressions(); }
              | expr
                { $$ = single_Expressions($1); }
              | arg_expr_list ',' expr
                { @$ = @1; $$ = append_Expressions($1, single_Expressions($3)); }
              ;

/* List of expressions for a block, semicolon-separated */
block_expr_list : expr ';'
                  { $$ = single_Expressions($1); }
                | block_expr_list expr ';'
                  { @$ = @1; $$ = append_Expressions($1, single_Expressions($2)); }
                ;

/*
 * Expressions
 */
expr : OBJECTID ASSIGN expr
       { @$ = @1; SET_NODELOC(@1); $$ = assign($1, $3); }
     | expr '@' TYPEID '.' OBJECTID '(' arg_expr_list ')'
       { @$ = @1; SET_NODELOC(@1); $$ = static_dispatch($1, $3, $5, $7); }
     | expr '.' OBJECTID '(' arg_expr_list ')'
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch($1, $3, $5); }
     | OBJECTID '(' arg_expr_list ')'
       { @$ = @1; SET_NODELOC(@1); $$ = dispatch(object(self_sym), $1, $3); } /* Use self_sym */
     | IF expr THEN expr ELSE expr FI
       { @$ = @1; SET_NODELOC(@1); $$ = cond($2, $4, $6); }
     | WHILE expr LOOP expr POOL
       { @$ = @1; SET_NODELOC(@1); $$ = loop($2, $4); }
     | CASE expr OF case_branch_list ESAC
       { @$ = @1; SET_NODELOC(@1); $$ = typcase($2, $4); }
     | '{' block_expr_list '}'
       { @$ = @1; SET_NODELOC(@1); $$ = block($2); }
     
     /* REFACTORED 'let' EXPRESSION */
     | let_expression
       { @$ = @1; SET_NODELOC(@1); $$ = $1; }

     | expr '+' expr
       { @$ = @1; SET_NODELOC(@1); $$ = plus($1, $3); }
     | expr '-' expr
       { @$ = @1; SET_NODELOC(@1); $$ = sub($1, $3); }
     | expr '*' expr
       { @$ = @1; SET_NODELOC(@1); $$ = mul($1, $3); }
     | expr '/' expr
       { @$ = @1; SET_NODELOC(@1); $$ = divide($1, $3); }
     | '~' expr
       { @$ = @1; SET_NODELOC(@1); $$ = neg($2); }
     | expr '<' expr
       { @$ = @1; SET_NODELOC(@1); $$ = lt($1, $3); }
     | expr '=' expr
       { @$ = @1; SET_NODELOC(@1); $$ = eq($1, $3); }
     | expr LE expr
       { @$ = @1; SET_NODELOC(@1); $$ = leq($1, $3); }
     | NOT expr
       { @$ = @1; SET_NODELOC(@1); $$ = comp($2); }
     | ISVOID expr
       { @$ = @1; SET_NODELOC(@1); $$ = isvoid($2); }
     | NEW TYPEID
       { @$ = @1; SET_NODELOC(@1); $$ = new_($2); }
     | INT_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = int_const($1); }
     | BOOL_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = bool_const($1); }
     | STR_CONST
       { @$ = @1; SET_NODELOC(@1); $$ = string_const($1); }
     | OBJECTID
       { @$ = @1; SET_NODELOC(@1); $$ = object($1); }
     | '(' expr ')'
       { @$ = @1; $$ = $2; }
     ;

/*
 * This is the new, recursive definition for 'let'.
 * It replaces the 16 original hardcoded rules.
 */
let_expression : LET let_binding_decl IN expr
                 { 
                   @$ = @1; 
                   SET_NODELOC(@1); 
                   $$ = let($2.id, $2.type, $2.init, $4); 
                 }
               | LET let_binding_decl ',' let_expression
                 { 
                   @$ = @1; 
                   SET_NODELOC(@1); 
                   /* $4 is the rest of the nested let chain */
                   $$ = let($2.id, $2.type, $2.init, $4); 
                 }
               ;

/* A single let binding, either with or without initialization */
let_binding_decl : OBJECTID ':' TYPEID
                   { 
                     @$ = @1; 
                     node_lineno = 0; 
                     $$.init = no_expr(); 
                     SET_NODELOC(@1); 
                     $$.id = $1; 
                     $$.type = $3; 
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


/* A list of one or more case branches */
case_branch_list : case_branch
                   { $$ = single_Cases($1); }
                 | case_branch_list case_branch
                   { @$ = @1; $$ = append_Cases($1, single_Cases($2)); }
                 ;

/* A single case branch: id: Type => expr; */
case_branch : OBJECTID ':' TYPEID DARROW expr ';'
              { @$ = @1; SET_NODELOC(@1); $$ = branch($1, $3, $5); }
            ;

%%

/* * C code section
 */

int curr_lineno = 1; /* Keep track of current line number */

/* This name must be 'self_sym' to match the external definition */
Symbol self_sym = idtable.add_string("self");

/*
 * Error reporting function called by Bison on a parse error.
 */
void yyerror(char *s)
{
  extern char *curr_filename;
  
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " 
       << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++; /* Use omerrs */
  
  if(omerrs > 50) { /* Use omerrs */
    fprintf(stdout, "Error limit reached (50). Aborting.\n");
    exit(1);
  }
}
