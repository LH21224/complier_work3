/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         cool_yyparse
#define yylex           cool_yylex
#define yyerror         cool_yyerror
#define yydebug         cool_yydebug
#define yynerrs         cool_yynerrs
#define yylval          cool_yylval
#define yychar          cool_yychar
#define yylloc          cool_yylloc

/* First part of user prologue.  */
#line 1 "cool.y"

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

#line 117 "cool.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "cool.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_CLASS = 3,                      /* CLASS  */
  YYSYMBOL_ELSE = 4,                       /* ELSE  */
  YYSYMBOL_FI = 5,                         /* FI  */
  YYSYMBOL_IF = 6,                         /* IF  */
  YYSYMBOL_IN = 7,                         /* IN  */
  YYSYMBOL_INHERITS = 8,                   /* INHERITS  */
  YYSYMBOL_LET = 9,                        /* LET  */
  YYSYMBOL_LOOP = 10,                      /* LOOP  */
  YYSYMBOL_POOL = 11,                      /* POOL  */
  YYSYMBOL_THEN = 12,                      /* THEN  */
  YYSYMBOL_WHILE = 13,                     /* WHILE  */
  YYSYMBOL_CASE = 14,                      /* CASE  */
  YYSYMBOL_ESAC = 15,                      /* ESAC  */
  YYSYMBOL_OF = 16,                        /* OF  */
  YYSYMBOL_DARROW = 17,                    /* DARROW  */
  YYSYMBOL_NEW = 18,                       /* NEW  */
  YYSYMBOL_ISVOID = 19,                    /* ISVOID  */
  YYSYMBOL_STR_CONST = 20,                 /* STR_CONST  */
  YYSYMBOL_INT_CONST = 21,                 /* INT_CONST  */
  YYSYMBOL_BOOL_CONST = 22,                /* BOOL_CONST  */
  YYSYMBOL_TYPEID = 23,                    /* TYPEID  */
  YYSYMBOL_OBJECTID = 24,                  /* OBJECTID  */
  YYSYMBOL_ASSIGN = 25,                    /* ASSIGN  */
  YYSYMBOL_NOT = 26,                       /* NOT  */
  YYSYMBOL_LE = 27,                        /* LE  */
  YYSYMBOL_ERROR = 28,                     /* ERROR  */
  YYSYMBOL_29_ = 29,                       /* '<'  */
  YYSYMBOL_30_ = 30,                       /* '='  */
  YYSYMBOL_31_ = 31,                       /* '+'  */
  YYSYMBOL_32_ = 32,                       /* '-'  */
  YYSYMBOL_33_ = 33,                       /* '*'  */
  YYSYMBOL_34_ = 34,                       /* '/'  */
  YYSYMBOL_35_ = 35,                       /* '~'  */
  YYSYMBOL_36_ = 36,                       /* '@'  */
  YYSYMBOL_37_ = 37,                       /* '.'  */
  YYSYMBOL_38_ = 38,                       /* '{'  */
  YYSYMBOL_39_ = 39,                       /* '}'  */
  YYSYMBOL_40_ = 40,                       /* ';'  */
  YYSYMBOL_41_ = 41,                       /* '('  */
  YYSYMBOL_42_ = 42,                       /* ')'  */
  YYSYMBOL_43_ = 43,                       /* ':'  */
  YYSYMBOL_44_ = 44,                       /* ','  */
  YYSYMBOL_YYACCEPT = 45,                  /* $accept  */
  YYSYMBOL_program = 46,                   /* program  */
  YYSYMBOL_cool_list = 47,                 /* cool_list  */
  YYSYMBOL_class = 48,                     /* class  */
  YYSYMBOL_class_element = 49,             /* class_element  */
  YYSYMBOL_feature_item = 50,              /* feature_item  */
  YYSYMBOL_formal_arg = 51,                /* formal_arg  */
  YYSYMBOL_formal = 52,                    /* formal  */
  YYSYMBOL_arg_list_call = 53,             /* arg_list_call  */
  YYSYMBOL_block_sequence = 54,            /* block_sequence  */
  YYSYMBOL_expr = 55,                      /* expr  */
  YYSYMBOL_case_list = 56,                 /* case_list  */
  YYSYMBOL_case_branch = 57                /* case_branch  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   428

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  13
/* YYNRULES -- Number of rules.  */
#define YYNRULES  63
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  192

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   284


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      41,    42,    33,    31,    44,    32,    37,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    43,    40,
      29,    30,     2,     2,    36,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    38,     2,    39,    35,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     2
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    91,    91,    95,    97,   101,   103,   105,   110,   111,
     115,   117,   119,   124,   125,   127,   131,   136,   137,   139,
     143,   145,   149,   151,   153,   155,   157,   159,   161,   163,
     165,   167,   169,   171,   173,   175,   177,   179,   181,   183,
     185,   187,   189,   191,   193,   195,   199,   204,   209,   211,
     213,   215,   217,   219,   221,   223,   225,   227,   229,   231,
     233,   238,   240,   244
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "CLASS", "ELSE", "FI",
  "IF", "IN", "INHERITS", "LET", "LOOP", "POOL", "THEN", "WHILE", "CASE",
  "ESAC", "OF", "DARROW", "NEW", "ISVOID", "STR_CONST", "INT_CONST",
  "BOOL_CONST", "TYPEID", "OBJECTID", "ASSIGN", "NOT", "LE", "ERROR",
  "'<'", "'='", "'+'", "'-'", "'*'", "'/'", "'~'", "'@'", "'.'", "'{'",
  "'}'", "';'", "'('", "')'", "':'", "','", "$accept", "program",
  "cool_list", "class", "class_element", "feature_item", "formal_arg",
  "formal", "arg_list_call", "block_sequence", "expr", "case_list",
  "case_branch", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-51)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-3)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      66,   -35,   -15,    25,    90,   -51,   -51,    -7,   -51,   -51,
       9,   -51,    20,   -12,   -51,    56,    28,    35,    33,    63,
      71,   -51,   -51,    64,    65,    58,   -51,    88,   -51,    93,
      76,    63,   190,   -51,   106,   -51,   190,   108,   190,   190,
     110,   190,   -51,   -51,   -51,    39,   190,   190,   190,   190,
     380,    96,   296,    92,    91,   308,   -51,   -18,   190,   190,
     380,   -18,   159,   333,   319,   190,   190,   190,   190,   190,
     190,   190,   113,   114,   190,   190,   121,   190,   122,   380,
      61,   380,   -51,   345,   -51,   -51,   391,   391,   391,    29,
      29,   -18,   -18,   111,   104,   369,   203,    -4,   283,   112,
      -2,   -51,   -51,   190,   -51,   123,   190,   -51,   190,   190,
     190,   125,   -51,   131,   -51,   -51,   380,   115,    67,   214,
     380,    17,   126,   153,   190,   -51,   -51,   190,   147,   152,
     190,    68,   380,   133,    16,   357,   -51,   161,   190,   190,
     158,   -51,    48,   380,    52,   143,   190,   190,   163,   190,
     164,   166,   380,   130,   148,   380,   149,    -5,   190,   169,
     167,   172,   190,   190,   380,   162,     3,     4,   380,   231,
     178,   190,   190,   190,   190,   190,    45,   380,   242,   380,
     259,   380,   190,   190,   190,   190,   380,   270,   380,   380,
     190,   380
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     0,     0,     3,     7,     0,     1,     4,
       0,     8,     0,     0,     8,     0,     0,     0,     0,    13,
       0,     5,     9,     0,     0,     0,    14,    12,     6,     0,
       0,     0,     0,    16,     0,    15,     0,     0,     0,     0,
       0,     0,    23,    22,    24,    25,     0,     0,     0,     0,
      11,     0,     0,     0,     0,     0,    36,    37,     0,    17,
      39,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    26,
       0,    18,    44,     0,    20,    45,    41,    40,    42,    29,
      30,    27,    28,     0,     0,     0,     0,     0,     0,     0,
       0,    61,    31,     0,    21,     0,    17,    10,     0,     0,
       0,     0,    35,     0,    43,    62,    19,     0,     0,     0,
      48,     0,     0,     0,    17,    32,    34,     0,     0,     0,
       0,     0,    49,     0,     0,     0,    33,     0,     0,     0,
       0,    63,     0,    50,     0,     0,     0,     0,     0,     0,
       0,     0,    51,     0,     0,    52,     0,     0,     0,     0,
       0,     0,     0,     0,    53,     0,     0,     0,    47,     0,
       0,     0,     0,     0,     0,     0,     0,    55,     0,    60,
       0,    54,     0,     0,     0,     0,    56,     0,    57,    58,
       0,    46
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -51,   -51,   -51,   198,   192,   -51,   -51,   182,   -50,   -51,
     -32,   -51,   117
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,     3,     4,     5,    13,    17,    25,    26,    80,    62,
      81,   100,   101
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      50,    10,   162,   109,    52,     6,    54,    55,     7,    57,
     171,   173,    15,   114,    60,    61,    63,    64,    72,    73,
     163,   110,    99,   138,   127,     8,    79,    16,   172,   174,
      83,    11,    12,    86,    87,    88,    89,    90,    91,    92,
     111,   139,    95,    96,    65,    98,    66,    67,    68,    69,
      70,    71,   182,    72,    73,   146,   118,    15,    14,   149,
     140,   128,    70,    71,    58,    72,    73,     1,    21,     2,
     183,   116,    23,   147,   131,    22,   119,   120,   121,    65,
      59,    66,    67,    68,    69,    70,    71,    24,    72,    73,
      -2,     1,   148,     2,    27,   132,   150,    19,   135,    20,
      30,    77,    31,   102,    28,   103,   143,   144,    29,   125,
     136,   103,   103,    32,   152,   153,    33,   155,    65,    34,
      66,    67,    68,    69,    70,    71,   164,    72,    73,    51,
     168,   169,    53,    56,    74,    76,    93,   158,    94,   177,
     178,   179,   180,   181,    97,   106,    99,   117,   105,   122,
     186,   187,   188,   189,   123,   113,   124,    65,   191,    66,
      67,    68,    69,    70,    71,    36,    72,    73,    37,   129,
     130,   133,    38,    39,   159,   134,   137,    40,    41,    42,
      43,    44,   145,    45,   142,    46,   151,   154,   156,   157,
     166,   160,   161,   165,    47,   167,    36,    48,    82,    37,
      49,   176,     9,    38,    39,   170,    18,   108,    40,    41,
      42,    43,    44,    35,    45,     0,    46,   115,     0,   126,
       0,     0,     0,     0,     0,    47,     0,     0,    48,     0,
      65,    49,    66,    67,    68,    69,    70,    71,   175,    72,
      73,    65,     0,    66,    67,    68,    69,    70,    71,   184,
      72,    73,     0,     0,     0,     0,     0,     0,    65,     0,
      66,    67,    68,    69,    70,    71,   185,    72,    73,    65,
       0,    66,    67,    68,    69,    70,    71,   190,    72,    73,
       0,     0,     0,     0,     0,     0,    65,     0,    66,    67,
      68,    69,    70,    71,   112,    72,    73,    65,     0,    66,
      67,    68,    69,    70,    71,     0,    72,    73,    75,     0,
      65,     0,    66,    67,    68,    69,    70,    71,     0,    72,
      73,     0,     0,    65,    78,    66,    67,    68,    69,    70,
      71,     0,    72,    73,     0,    65,     0,    66,    67,    68,
      69,    70,    71,     0,    72,    73,    65,     0,    66,    67,
      68,    69,    70,    71,     0,    72,    73,     0,     0,     0,
      65,    85,    66,    67,    68,    69,    70,    71,     0,    72,
      73,     0,    65,    84,    66,    67,    68,    69,    70,    71,
       0,    72,    73,     0,    65,   104,    66,    67,    68,    69,
      70,    71,     0,    72,    73,     0,    65,   141,    66,    67,
      68,    69,    70,    71,     0,    72,    73,    65,   107,    66,
      67,    68,    69,    70,    71,     0,    72,    73,    -3,     0,
      -3,    -3,    68,    69,    70,    71,     0,    72,    73
};

static const yytype_int16 yycheck[] =
{
      32,     8,     7,     7,    36,    40,    38,    39,    23,    41,
       7,     7,    24,    15,    46,    47,    48,    49,    36,    37,
      25,    25,    24,     7,     7,     0,    58,    39,    25,    25,
      62,    38,    23,    65,    66,    67,    68,    69,    70,    71,
      44,    25,    74,    75,    27,    77,    29,    30,    31,    32,
      33,    34,     7,    36,    37,     7,   106,    24,    38,     7,
      44,    44,    33,    34,    25,    36,    37,     1,    40,     3,
      25,   103,    39,    25,   124,    40,   108,   109,   110,    27,
      41,    29,    30,    31,    32,    33,    34,    24,    36,    37,
       0,     1,    44,     3,    23,   127,    44,    41,   130,    43,
      42,    10,    44,    42,    40,    44,   138,   139,    43,    42,
      42,    44,    44,    25,   146,   147,    23,   149,    27,    43,
      29,    30,    31,    32,    33,    34,   158,    36,    37,    23,
     162,   163,    24,    23,    38,    43,    23,     7,    24,   171,
     172,   173,   174,   175,    23,    41,    24,    24,    37,    24,
     182,   183,   184,   185,    23,    43,    41,    27,   190,    29,
      30,    31,    32,    33,    34,     6,    36,    37,     9,    43,
      17,    24,    13,    14,    44,    23,    43,    18,    19,    20,
      21,    22,    24,    24,    23,    26,    43,    24,    24,    23,
      23,    43,    43,    24,    35,    23,     6,    38,    39,     9,
      41,    23,     4,    13,    14,    43,    14,     4,    18,    19,
      20,    21,    22,    31,    24,    -1,    26,   100,    -1,     5,
      -1,    -1,    -1,    -1,    -1,    35,    -1,    -1,    38,    -1,
      27,    41,    29,    30,    31,    32,    33,    34,     7,    36,
      37,    27,    -1,    29,    30,    31,    32,    33,    34,     7,
      36,    37,    -1,    -1,    -1,    -1,    -1,    -1,    27,    -1,
      29,    30,    31,    32,    33,    34,     7,    36,    37,    27,
      -1,    29,    30,    31,    32,    33,    34,     7,    36,    37,
      -1,    -1,    -1,    -1,    -1,    -1,    27,    -1,    29,    30,
      31,    32,    33,    34,    11,    36,    37,    27,    -1,    29,
      30,    31,    32,    33,    34,    -1,    36,    37,    12,    -1,
      27,    -1,    29,    30,    31,    32,    33,    34,    -1,    36,
      37,    -1,    -1,    27,    16,    29,    30,    31,    32,    33,
      34,    -1,    36,    37,    -1,    27,    -1,    29,    30,    31,
      32,    33,    34,    -1,    36,    37,    27,    -1,    29,    30,
      31,    32,    33,    34,    -1,    36,    37,    -1,    -1,    -1,
      27,    42,    29,    30,    31,    32,    33,    34,    -1,    36,
      37,    -1,    27,    40,    29,    30,    31,    32,    33,    34,
      -1,    36,    37,    -1,    27,    40,    29,    30,    31,    32,
      33,    34,    -1,    36,    37,    -1,    27,    40,    29,    30,
      31,    32,    33,    34,    -1,    36,    37,    27,    39,    29,
      30,    31,    32,    33,    34,    -1,    36,    37,    27,    -1,
      29,    30,    31,    32,    33,    34,    -1,    36,    37
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,    46,    47,    48,    40,    23,     0,    48,
       8,    38,    23,    49,    38,    24,    39,    50,    49,    41,
      43,    40,    40,    39,    24,    51,    52,    23,    40,    43,
      42,    44,    25,    23,    43,    52,     6,     9,    13,    14,
      18,    19,    20,    21,    22,    24,    26,    35,    38,    41,
      55,    23,    55,    24,    55,    55,    23,    55,    25,    41,
      55,    55,    54,    55,    55,    27,    29,    30,    31,    32,
      33,    34,    36,    37,    38,    12,    43,    10,    16,    55,
      53,    55,    39,    55,    40,    42,    55,    55,    55,    55,
      55,    55,    55,    23,    24,    55,    55,    23,    55,    24,
      56,    57,    42,    44,    40,    37,    41,    39,     4,     7,
      25,    44,    11,    43,    15,    57,    55,    24,    53,    55,
      55,    55,    24,    23,    41,    42,     5,     7,    44,    43,
      17,    53,    55,    24,    23,    55,    42,    43,     7,    25,
      44,    40,    23,    55,    55,    24,     7,    25,    44,     7,
      44,    43,    55,    55,    24,    55,    24,    23,     7,    44,
      43,    43,     7,    25,    55,    24,    23,    23,    55,    55,
      43,     7,    25,     7,    25,     7,    23,    55,    55,    55,
      55,    55,     7,    25,     7,     7,    55,    55,    55,    55,
       7,    55
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    45,    46,    47,    47,    48,    48,    48,    49,    49,
      50,    50,    50,    51,    51,    51,    52,    53,    53,    53,
      54,    54,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    55,    55,    55,    55,    55,    55,
      55,    56,    56,    57
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     6,     8,     2,     0,     3,
       9,     5,     3,     0,     1,     3,     3,     0,     1,     3,
       2,     3,     1,     1,     1,     1,     3,     3,     3,     3,
       3,     4,     6,     8,     7,     5,     2,     2,     2,     2,
       3,     3,     3,     5,     3,     3,    20,    14,     6,     8,
      10,    12,    12,    14,    16,    16,    18,    18,    18,    20,
      16,     1,     2,     6
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: cool_list  */
#line 92 "cool.y"
          { (yyloc) = (yylsp[0]); ast_root = program((yyvsp[0].classes)); }
#line 1434 "cool.tab.c"
    break;

  case 3: /* cool_list: class  */
#line 96 "cool.y"
             { (yyloc) = (yylsp[0]); (yyval.classes) = single_Classes((yyvsp[0].class_)); }
#line 1440 "cool.tab.c"
    break;

  case 4: /* cool_list: cool_list class  */
#line 98 "cool.y"
             { (yyloc) = (yylsp[-1]); (yyval.classes) = append_Classes((yyvsp[-1].classes), single_Classes((yyvsp[0].class_))); }
#line 1446 "cool.tab.c"
    break;

  case 5: /* class: CLASS TYPEID '{' class_element '}' ';'  */
#line 102 "cool.y"
        { (yyloc) = (yylsp[-5]); Symbol obj_sym = idtable.add_string("Object"); (yyval.class_) = class_((yyvsp[-4].symbol), obj_sym, (yyvsp[-2].features), stringtable.add_string(curr_filename)); }
#line 1452 "cool.tab.c"
    break;

  case 6: /* class: CLASS TYPEID INHERITS TYPEID '{' class_element '}' ';'  */
#line 104 "cool.y"
        { (yyloc) = (yylsp[-7]); (yyval.class_) = class_((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].features), stringtable.add_string(curr_filename)); }
#line 1458 "cool.tab.c"
    break;

  case 7: /* class: error ';'  */
#line 106 "cool.y"
        { (yyloc) = (yylsp[-1]); Symbol err_sym = idtable.add_string("Error"); Symbol obj_sym = idtable.add_string("Object"); (yyval.class_) = class_(err_sym, obj_sym, nil_Features(), stringtable.add_string(curr_filename)); }
#line 1464 "cool.tab.c"
    break;

  case 8: /* class_element: %empty  */
#line 110 "cool.y"
               { (yyval.features) = nil_Features(); }
#line 1470 "cool.tab.c"
    break;

  case 9: /* class_element: class_element feature_item ';'  */
#line 112 "cool.y"
               { (yyloc) = (yylsp[-1]); (yyval.features) = append_Features((yyvsp[-2].features), single_Features((yyvsp[-1].feature))); }
#line 1476 "cool.tab.c"
    break;

  case 10: /* feature_item: OBJECTID '(' formal_arg ')' ':' TYPEID '{' expr '}'  */
#line 116 "cool.y"
          { (yyloc) = (yylsp[-8]); (yyval.feature) = method((yyvsp[-8].symbol), (yyvsp[-6].formals), (yyvsp[-3].symbol), (yyvsp[-1].expression)); }
#line 1482 "cool.tab.c"
    break;

  case 11: /* feature_item: OBJECTID ':' TYPEID ASSIGN expr  */
#line 118 "cool.y"
          { (yyloc) = (yylsp[-4]); (yyval.feature) = attr((yyvsp[-4].symbol), (yyvsp[-2].symbol), (yyvsp[0].expression)); }
#line 1488 "cool.tab.c"
    break;

  case 12: /* feature_item: OBJECTID ':' TYPEID  */
#line 120 "cool.y"
          { (yyloc) = (yylsp[-2]); Expression no_init = no_expr(); SET_NODELOC((yylsp[-2])); (yyval.feature) = attr((yyvsp[-2].symbol), (yyvsp[0].symbol), no_init); }
#line 1494 "cool.tab.c"
    break;

  case 13: /* formal_arg: %empty  */
#line 124 "cool.y"
              { (yyval.formals) = nil_Formals(); }
#line 1500 "cool.tab.c"
    break;

  case 14: /* formal_arg: formal  */
#line 126 "cool.y"
              { (yyval.formals) = single_Formals((yyvsp[0].formal)); }
#line 1506 "cool.tab.c"
    break;

  case 15: /* formal_arg: formal_arg ',' formal  */
#line 128 "cool.y"
              { (yyloc) = (yylsp[-2]); (yyval.formals) = append_Formals((yyvsp[-2].formals), single_Formals((yyvsp[0].formal))); }
#line 1512 "cool.tab.c"
    break;

  case 16: /* formal: OBJECTID ':' TYPEID  */
#line 132 "cool.y"
         { (yyloc) = (yylsp[-2]); (yyval.formal) = formal((yyvsp[-2].symbol), (yyvsp[0].symbol)); }
#line 1518 "cool.tab.c"
    break;

  case 17: /* arg_list_call: %empty  */
#line 136 "cool.y"
            { (yyval.expressions) = nil_Expressions(); }
#line 1524 "cool.tab.c"
    break;

  case 18: /* arg_list_call: expr  */
#line 138 "cool.y"
            { (yyval.expressions) = single_Expressions((yyvsp[0].expression)); }
#line 1530 "cool.tab.c"
    break;

  case 19: /* arg_list_call: arg_list_call ',' expr  */
#line 140 "cool.y"
            { (yyloc) = (yylsp[-2]); (yyval.expressions) = append_Expressions((yyvsp[-2].expressions), single_Expressions((yyvsp[0].expression))); }
#line 1536 "cool.tab.c"
    break;

  case 20: /* block_sequence: expr ';'  */
#line 144 "cool.y"
                  { (yyval.expressions) = single_Expressions((yyvsp[-1].expression)); }
#line 1542 "cool.tab.c"
    break;

  case 21: /* block_sequence: block_sequence expr ';'  */
#line 146 "cool.y"
                  { (yyloc) = (yylsp[-2]); (yyval.expressions) = append_Expressions((yyvsp[-2].expressions), single_Expressions((yyvsp[-1].expression))); }
#line 1548 "cool.tab.c"
    break;

  case 22: /* expr: INT_CONST  */
#line 150 "cool.y"
       { (yyloc) = (yylsp[0]); SET_NODELOC((yylsp[0])); (yyval.expression) = int_const((yyvsp[0].symbol)); }
#line 1554 "cool.tab.c"
    break;

  case 23: /* expr: STR_CONST  */
#line 152 "cool.y"
       { (yyloc) = (yylsp[0]); SET_NODELOC((yylsp[0])); (yyval.expression) = string_const((yyvsp[0].symbol)); }
#line 1560 "cool.tab.c"
    break;

  case 24: /* expr: BOOL_CONST  */
#line 154 "cool.y"
       { (yyloc) = (yylsp[0]); SET_NODELOC((yylsp[0])); (yyval.expression) = bool_const((yyvsp[0].boolean)); }
#line 1566 "cool.tab.c"
    break;

  case 25: /* expr: OBJECTID  */
#line 156 "cool.y"
       { (yyloc) = (yylsp[0]); SET_NODELOC((yylsp[0])); (yyval.expression) = object((yyvsp[0].symbol)); }
#line 1572 "cool.tab.c"
    break;

  case 26: /* expr: OBJECTID ASSIGN expr  */
#line 158 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = assign((yyvsp[-2].symbol), (yyvsp[0].expression)); }
#line 1578 "cool.tab.c"
    break;

  case 27: /* expr: expr '*' expr  */
#line 160 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = mul((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1584 "cool.tab.c"
    break;

  case 28: /* expr: expr '/' expr  */
#line 162 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = divide((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1590 "cool.tab.c"
    break;

  case 29: /* expr: expr '+' expr  */
#line 164 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = plus((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1596 "cool.tab.c"
    break;

  case 30: /* expr: expr '-' expr  */
#line 166 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = sub((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1602 "cool.tab.c"
    break;

  case 31: /* expr: OBJECTID '(' arg_list_call ')'  */
#line 168 "cool.y"
       { (yyloc) = (yylsp[-3]); SET_NODELOC((yylsp[-3])); (yyval.expression) = dispatch(object(self_sym), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1608 "cool.tab.c"
    break;

  case 32: /* expr: expr '.' OBJECTID '(' arg_list_call ')'  */
#line 170 "cool.y"
       { (yyloc) = (yylsp[-5]); SET_NODELOC((yylsp[-5])); (yyval.expression) = dispatch((yyvsp[-5].expression), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1614 "cool.tab.c"
    break;

  case 33: /* expr: expr '@' TYPEID '.' OBJECTID '(' arg_list_call ')'  */
#line 172 "cool.y"
       { (yyloc) = (yylsp[-7]); SET_NODELOC((yylsp[-7])); (yyval.expression) = static_dispatch((yyvsp[-7].expression), (yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1620 "cool.tab.c"
    break;

  case 34: /* expr: IF expr THEN expr ELSE expr FI  */
#line 174 "cool.y"
       { (yyloc) = (yylsp[-6]); SET_NODELOC((yylsp[-6])); (yyval.expression) = cond((yyvsp[-5].expression), (yyvsp[-3].expression), (yyvsp[-1].expression)); }
#line 1626 "cool.tab.c"
    break;

  case 35: /* expr: WHILE expr LOOP expr POOL  */
#line 176 "cool.y"
       { (yyloc) = (yylsp[-4]); SET_NODELOC((yylsp[-4])); (yyval.expression) = loop((yyvsp[-3].expression), (yyvsp[-1].expression)); }
#line 1632 "cool.tab.c"
    break;

  case 36: /* expr: NEW TYPEID  */
#line 178 "cool.y"
       { (yyloc) = (yylsp[-1]); SET_NODELOC((yylsp[-1])); (yyval.expression) = new_((yyvsp[0].symbol)); }
#line 1638 "cool.tab.c"
    break;

  case 37: /* expr: ISVOID expr  */
#line 180 "cool.y"
       { (yyloc) = (yylsp[-1]); SET_NODELOC((yylsp[-1])); (yyval.expression) = isvoid((yyvsp[0].expression)); }
#line 1644 "cool.tab.c"
    break;

  case 38: /* expr: '~' expr  */
#line 182 "cool.y"
       { (yyloc) = (yylsp[-1]); SET_NODELOC((yylsp[-1])); (yyval.expression) = neg((yyvsp[0].expression)); }
#line 1650 "cool.tab.c"
    break;

  case 39: /* expr: NOT expr  */
#line 184 "cool.y"
       { (yyloc) = (yylsp[-1]); SET_NODELOC((yylsp[-1])); (yyval.expression) = comp((yyvsp[0].expression)); }
#line 1656 "cool.tab.c"
    break;

  case 40: /* expr: expr '<' expr  */
#line 186 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = lt((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1662 "cool.tab.c"
    break;

  case 41: /* expr: expr LE expr  */
#line 188 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = leq((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1668 "cool.tab.c"
    break;

  case 42: /* expr: expr '=' expr  */
#line 190 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = eq((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1674 "cool.tab.c"
    break;

  case 43: /* expr: CASE expr OF case_list ESAC  */
#line 192 "cool.y"
       { (yyloc) = (yylsp[-4]); SET_NODELOC((yylsp[-4])); (yyval.expression) = typcase((yyvsp[-3].expression), (yyvsp[-1].cases)); }
#line 1680 "cool.tab.c"
    break;

  case 44: /* expr: '{' block_sequence '}'  */
#line 194 "cool.y"
       { (yyloc) = (yylsp[-2]); SET_NODELOC((yylsp[-2])); (yyval.expression) = block((yyvsp[-1].expressions)); }
#line 1686 "cool.tab.c"
    break;

  case 45: /* expr: '(' expr ')'  */
#line 196 "cool.y"
       { (yyloc) = (yylsp[-2]); (yyval.expression) = (yyvsp[-1].expression); }
#line 1692 "cool.tab.c"
    break;

  case 46: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 200 "cool.y"
       { (yyloc) = (yylsp[-19]); SET_NODELOC((yylsp[-19])); Expression let3_init = (yyvsp[-2].expression); Expression let2_init = (yyvsp[-8].expression); Expression let1_init = (yyvsp[-14].expression); 
         Expression let3 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), let3_init, (yyvsp[0].expression));
         Expression let2 = let((yyvsp[-12].symbol), (yyvsp[-10].symbol), let2_init, let3);
         (yyval.expression) = let((yyvsp[-18].symbol), (yyvsp[-16].symbol), let1_init, let2); }
#line 1701 "cool.tab.c"
    break;

  case 47: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr  */
#line 205 "cool.y"
       { (yyloc) = (yylsp[-13]); SET_NODELOC((yylsp[-13])); Expression no_init = no_expr(); 
         Expression let3 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression));
         Expression let2 = let((yyvsp[-8].symbol), (yyvsp[-6].symbol), no_init, let3);
         (yyval.expression) = let((yyvsp[-12].symbol), (yyvsp[-10].symbol), no_init, let2); }
#line 1710 "cool.tab.c"
    break;

  case 48: /* expr: LET OBJECTID ':' TYPEID IN expr  */
#line 210 "cool.y"
       { (yyloc) = (yylsp[-5]); SET_NODELOC((yylsp[-5])); Expression no_init = no_expr(); (yyval.expression) = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); }
#line 1716 "cool.tab.c"
    break;

  case 49: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 212 "cool.y"
       { (yyloc) = (yylsp[-7]); SET_NODELOC((yylsp[-7])); (yyval.expression) = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1722 "cool.tab.c"
    break;

  case 50: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr  */
#line 214 "cool.y"
       { (yyloc) = (yylsp[-9]); SET_NODELOC((yylsp[-9])); Expression no_init = no_expr(); Expression let2 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); (yyval.expression) = let((yyvsp[-8].symbol), (yyvsp[-6].symbol), no_init, let2); }
#line 1728 "cool.tab.c"
    break;

  case 51: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr  */
#line 216 "cool.y"
       { (yyloc) = (yylsp[-11]); SET_NODELOC((yylsp[-11])); Expression no_init = no_expr(); Expression let2 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); (yyval.expression) = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), (yyvsp[-6].expression), let2); }
#line 1734 "cool.tab.c"
    break;

  case 52: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 218 "cool.y"
       { (yyloc) = (yylsp[-11]); SET_NODELOC((yylsp[-11])); Expression no_init = no_expr(); Expression let2 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); (yyval.expression) = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), no_init, let2); }
#line 1740 "cool.tab.c"
    break;

  case 53: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 220 "cool.y"
       { (yyloc) = (yylsp[-13]); SET_NODELOC((yylsp[-13])); Expression let2 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); (yyval.expression) = let((yyvsp[-12].symbol), (yyvsp[-10].symbol), (yyvsp[-8].expression), let2); }
#line 1746 "cool.tab.c"
    break;

  case 54: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 222 "cool.y"
       { (yyloc) = (yylsp[-15]); SET_NODELOC((yylsp[-15])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); Expression let2 = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), no_init, let3); (yyval.expression) = let((yyvsp[-14].symbol), (yyvsp[-12].symbol), no_init, let2); }
#line 1752 "cool.tab.c"
    break;

  case 55: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID IN expr  */
#line 224 "cool.y"
       { (yyloc) = (yylsp[-15]); SET_NODELOC((yylsp[-15])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); Expression let2 = let((yyvsp[-8].symbol), (yyvsp[-6].symbol), no_init, let3); (yyval.expression) = let((yyvsp[-14].symbol), (yyvsp[-12].symbol), (yyvsp[-10].expression), let2); }
#line 1758 "cool.tab.c"
    break;

  case 56: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr  */
#line 226 "cool.y"
       { (yyloc) = (yylsp[-17]); SET_NODELOC((yylsp[-17])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); Expression let2 = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), (yyvsp[-6].expression), let3); (yyval.expression) = let((yyvsp[-16].symbol), (yyvsp[-14].symbol), (yyvsp[-12].expression), let2); }
#line 1764 "cool.tab.c"
    break;

  case 57: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 228 "cool.y"
       { (yyloc) = (yylsp[-17]); SET_NODELOC((yylsp[-17])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); Expression let2 = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), no_init, let3); (yyval.expression) = let((yyvsp[-16].symbol), (yyvsp[-14].symbol), (yyvsp[-12].expression), let2); }
#line 1770 "cool.tab.c"
    break;

  case 58: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 230 "cool.y"
       { (yyloc) = (yylsp[-17]); SET_NODELOC((yylsp[-17])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); Expression let2 = let((yyvsp[-12].symbol), (yyvsp[-10].symbol), (yyvsp[-8].expression), let3); (yyval.expression) = let((yyvsp[-16].symbol), (yyvsp[-14].symbol), no_init, let2); }
#line 1776 "cool.tab.c"
    break;

  case 59: /* expr: LET OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID ASSIGN expr IN expr  */
#line 232 "cool.y"
       { (yyloc) = (yylsp[-19]); SET_NODELOC((yylsp[-19])); Expression let3 = let((yyvsp[-6].symbol), (yyvsp[-4].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); Expression let2 = let((yyvsp[-12].symbol), (yyvsp[-10].symbol), (yyvsp[-8].expression), let3); (yyval.expression) = let((yyvsp[-18].symbol), (yyvsp[-16].symbol), (yyvsp[-14].expression), let2); }
#line 1782 "cool.tab.c"
    break;

  case 60: /* expr: LET OBJECTID ':' TYPEID ',' OBJECTID ':' TYPEID ASSIGN expr ',' OBJECTID ':' TYPEID IN expr  */
#line 234 "cool.y"
       { (yyloc) = (yylsp[-15]); SET_NODELOC((yylsp[-15])); Expression no_init = no_expr(); Expression let3 = let((yyvsp[-4].symbol), (yyvsp[-2].symbol), no_init, (yyvsp[0].expression)); Expression let2 = let((yyvsp[-10].symbol), (yyvsp[-8].symbol), (yyvsp[-6].expression), let3); (yyval.expression) = let((yyvsp[-14].symbol), (yyvsp[-12].symbol), no_init, let2); }
#line 1788 "cool.tab.c"
    break;

  case 61: /* case_list: case_branch  */
#line 239 "cool.y"
            { (yyval.cases) = single_Cases((yyvsp[0].case_)); }
#line 1794 "cool.tab.c"
    break;

  case 62: /* case_list: case_list case_branch  */
#line 241 "cool.y"
            { (yyloc) = (yylsp[-1]); (yyval.cases) = append_Cases((yyvsp[-1].cases), single_Cases((yyvsp[0].case_))); }
#line 1800 "cool.tab.c"
    break;

  case 63: /* case_branch: OBJECTID ':' TYPEID DARROW expr ';'  */
#line 245 "cool.y"
               { (yyloc) = (yylsp[-5]); SET_NODELOC((yylsp[-5])); (yyval.case_) = branch((yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-1].expression)); }
#line 1806 "cool.tab.c"
    break;


#line 1810 "cool.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 249 "cool.y"


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
