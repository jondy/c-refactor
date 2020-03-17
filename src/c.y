/**
 *
 * C 语言解析器，根据文档 N1256.pdf C98/99 设计。
 *
 *     WG14/N1256 Committee Draft — Septermber 7, 2007
 *     ISO/IEC 9899:TC3
 *
 * 基本按照附录 A Language syntax summary 中关于语法定义和函数库的内容
 * 来编写支持的格式，列出支持的和不支持的内容。
 *
 * 常量字符或者字符串在规则中直接使用常量；
 *
 * 常量字符，只有一个字符的，如果是运算符，那么定义 TOKEN，否
 * 则不需要定义；在 lex 中直接返回值即可。
 *
 * 常量字符串，超过一个字符的，必须定义其 TOKEN，在 lex 中返
 * 回对应的 TOKEN.
 *
 * 为解决表达式折行对齐的问题，需要引进一个虚拟的二元操作符，
 * 其优先级最高，这样可以保证折行算法正确。如果对于优先级最高
 * 的运算符来说，不需要 lookahead TOKEN，但是折行算法需要根据
 * 后面一个 token 来确定其操作，所以定义一个更高的虚拟二元操
 * 作符，可以迫使实际级别最高运算符返回一个 lookahead token。
 *
 * 对于 N126.pdf 附录中非终止符号，将其中的中杠换成下划线，否
 * 则 bison 无法编译通过。
 *
 * 对语法的修改：
 *
 *   - 增加主入口规则
 *
 *     entry: | preprocessing_file ;
 *
 *     同时修改规则 A.3 Preprocessing directives (6.10) group_part 增加
 *
 *           | function_definition
 *           | declaration
 *
 *     并且删除没有使用两个规则 A.2.4 External definitions
 *         (6.9) translation_unit
 *         (6.9) external_declaration
 *
 *   - 修改规则 A.2.2 Declarations (6.7.7)
 *
 *       typedef_name: TYPE_NAME
 *
 *   - 修改规则 A.3 Preprocessing directives (6.10) control_line 为
 *
 *       # include HEADER_NAME
 *
 *     方便处理多个输入缓冲区
 *
 * 已知的 shift/reduce 冲突，共 1 个：
 *
 *     if (A)
 *         if (B)
 *             statement;
 *       else
 *           statement;
 *
 *    这个 else 到底是和第一个还是第二个 if 匹配，默认是第二个 (innermost)
 **/

%code top {
  #include <stdio.h>
  #include <strings.h>
  #include <limits.h>
}

%code requires {
  #include "ptypes.h"
}

%code {
  extern int yylex_init(yyscan_t scanner);
  extern int yylex_init_extra(YY_EXTRA_TYPE pcontext, yyscan_t scanner);
  extern int yylex(YYSTYPE * yylval_param, YYLTYPE * yylloc_param , yyscan_t yyscanner);
  extern int yylex_destroy(yyscan_t scanner);
  extern void yyset_in(FILE * file, yyscan_t scanner);
  void yyerror(YYLTYPE *llocp, yyscan_t yyscanner, PCONTEXT pcontext, char const *msg);
}

/* %glr-parser */
%require "3.2"
%define parse.trace
%define api.pure full

%locations
%token-table
%expect 1

%lex-param   {yyscan_t scanner}
%parse-param {yyscan_t scanner}
%parse-param {PCONTEXT pcontext}

%initial-action
{
  @$.filename = pcontext -> filename;
  @$.first_line = 1;
  @$.first_column = 1;
  @$.last_line = 1;
  @$.last_column = 1;
};

%token AUTO "auto"
%token BREAK "break"
%token CASE "case"
%token CHAR "char"
%token CONST "const"
%token CONTINUE "continue"
%token DEFAULT "default"
%token DO "do"
%token DOUBLE "double"
%token ELSE "else"
%token ENUM "enum"
%token EXTERN "extern"
%token FLOAT "float"
%token FOR "for"
%token GOTO "goto"
%token IF "if"
%token INLINE "inline"
%token INT "int"
%token LONG "long"
%token REGISTER "register"
%token RESTRICT "restrict"
%token RETURN "return"
%token SHORT "short"
%token SIGNED "signed"
%token SIZEOF "sizeof"
%token STATIC "static"
%token STRUCT "struct"
%token SWITCH "switch"
%token TYPEDEF "typedef"
%token UNION "union"
%token UNSIGNED "unsigned"
%token VOID "void"
%token VOLATILE "volatile"
%token WHILE "while"
%token _BOOL "_Bool"
%token _COMPLEX "_Complex"
%token _IMAGINARY "_Imaginary"

%token DEFINE "define"
%token ERROR "error"
%token INCLUDE "include"
%token LINE "line"
%token PRAGMA "pragma"
%token UNDEF "undef"

%token PIF "#if"
%token PIFDEF  "#ifdef"
%token PIFNDEF "#ifndef"
%token PELIF "#elif"
%token PELSE  "#else"
%token PENDIF "#endif"

%token IDENTIFIER
%token DECIMAL_CONSTANT
%token OCTAL_CONSTANT
%token HEXADECIMAL_CONSTANT
%token DECIMAL_FLOATING_CONSTANT
%token HEXADECIMAL_FLOATING_CONSTANT
%token CHARACTER_CONSTANT
%token ENUMERATION_CONSTANT
%token STRING_LITERAL
%token HEADER_NAME
%token PP_NUMBER

%token LPAREN  /* a ( character not immediately preceded by white-space */
%token PPUNCTUATOR
%token TYPE_NAME
%token NEW_LINE "\n"
%token PIDENTIFIER

%token EQUAL_PLUS "+="
%token EQUAL_MINUS "-="
%token EQUAL_AMPERSAND "&="
%token EQUAL_CARET "^="
%token EQUAL_VERTICALBAR "|="
%token EQUAL_ASTERISK "*="
%token EQUAL_SLASH "/="
%token EQUAL_PERCENT "%="
%token EQUAL_DOUBLELESS "<<="
%token EQUAL_DOUBLEGREATER ">>="

%token DOUBLENUMBER "##"
%token LESSCOLON "<:"
%token COLONGREATER ":>"
%token LESSPERCENT "<%"
%token PERCENTGREATER "%>"
%token PERCENTCOLON "%:"
%token HYPHENGREATER "->"
%token DOUBLEPLUS "++"
%token DOUBLEMINUS "--"
%token DOUBLELESS "<<"
%token DOUBLEGREATER ">>"
%token LESSEQUAL "<="
%token GREATEREQUAL ">="
%token DOUBLEEQUAL "=="
%token EXCLAIMEQUAL "!="
%token DOUBLEAMPERSAND "&&"
%token DOUBLEVERTICALBAR "||"
%token DOUBLEPERCENTCOLON "%:%:"
%token TRIPLEDOT "..."

%% /* Grammar rules and actions follow.  */

entry: /* Empty */ | group
;

 /* A.1.1 Lexical elements */
preprocessing_token: HEADER_NAME
                   | PIDENTIFIER
		   | PP_NUMBER
		   | CHARACTER_CONSTANT
		   | STRING_LITERAL
		   | PPUNCTUATOR
;

constant: integer_constant
        | floating_constant
        | ENUMERATION_CONSTANT
	| CHARACTER_CONSTANT
;

integer_constant: DECIMAL_CONSTANT
                | OCTAL_CONSTANT
		| HEXADECIMAL_CONSTANT
;

floating_constant: DECIMAL_FLOATING_CONSTANT
                 | HEXADECIMAL_FLOATING_CONSTANT
;

/*
punctuator: '[' | ']' | '(' | ')' | '{' | '}' | '.' | '&'
          | '*' | '+' | '-' | '~' | '!' | '/' | '%' | '<'
	  | '>' | '^' | '|' | '?' | ':' | ';' | '=' | ',' | '#'
	  | "+=" | "-=" | "&=" | "^=" | "|=" | "##" | "<:"
	  | ":>" | "<%" | "%>" | "%:" | "->" | "++" | "--"
	  | "<<" | ">>" | "<=" | ">=" | "==" | "!=" | "&&"
	  | "||" | "*=" | "/=" | "%="
	  | "<<=" | ">>=" | "%:%:" | "..."
;
*/

 /* A.2.1 Expressions */
primary_expression: IDENTIFIER
                  | constant
		  | STRING_LITERAL
		  | '(' expression ')'
;

postfix_expression: primary_expression
                  | postfix_expression '[' expression ']'
		  | postfix_expression '(' ')'
		  | postfix_expression '(' argument_expression_list ')'
		  | postfix_expression '.' IDENTIFIER
		  | postfix_expression HYPHENGREATER IDENTIFIER
		  | postfix_expression DOUBLEPLUS
		  | postfix_expression DOUBLEMINUS
		  | '(' type_name ')' '{' initializer_list '}'
		  | '(' type_name ')' '{' initializer_list ',' '}'
;

argument_expression_list: assignment_expression
                        | argument_expression_list ',' assignment_expression
;

unary_expression: postfix_expression
                | DOUBLEPLUS unary_expression
		| DOUBLEMINUS unary_expression
		| unary_operator cast_expression
		| SIZEOF unary_expression
		| SIZEOF '(' type_name ')'
;

unary_operator: '&'
              | '*'
	      | '+'
	      | '-'
	      | '~'
	      | '!'
;

cast_expression: unary_expression
               | '(' type_name ')' cast_expression
;

multiplicative_expression: cast_expression
                         | multiplicative_expression '*' cast_expression
			 | multiplicative_expression '/' cast_expression
			 | multiplicative_expression '%' cast_expression
;

additive_expression: multiplicative_expression
                   | additive_expression '+' multiplicative_expression
		   | additive_expression '-' multiplicative_expression
;

shift_expression: additive_expression
                | shift_expression DOUBLELESS additive_expression
		| shift_expression DOUBLEGREATER additive_expression
;

relational_expression: shift_expression
                     | relational_expression '<' shift_expression
		     | relational_expression '>' shift_expression
		     | relational_expression LESSEQUAL shift_expression
		     | relational_expression GREATEREQUAL shift_expression
;

equality_expression: relational_expression
                   | equality_expression DOUBLEEQUAL relational_expression
		   | equality_expression EXCLAIMEQUAL relational_expression
;

and_expression: equality_expression
              | and_expression '&' equality_expression
;

exclusive_or_expression: and_expression
                       | exclusive_or_expression '^' and_expression
;

inclusive_or_expression: exclusive_or_expression
                       | inclusive_or_expression '|' exclusive_or_expression
;

logical_and_expression: inclusive_or_expression
                      | logical_and_expression DOUBLEAMPERSAND inclusive_or_expression
;

logical_or_expression: logical_and_expression
                     | logical_or_expression DOUBLEVERTICALBAR logical_and_expression
;

conditional_expression: logical_or_expression
                      | logical_or_expression '?' expression ':' conditional_expression
;

assignment_expression: conditional_expression
                     | unary_expression assignment_operator assignment_expression
;

assignment_operator: '='
                   | EQUAL_ASTERISK
		   | EQUAL_SLASH
		   | EQUAL_PERCENT
		   | EQUAL_PLUS
		   | EQUAL_MINUS
		   | EQUAL_DOUBLELESS
		   | EQUAL_DOUBLEGREATER
		   | EQUAL_AMPERSAND
		   | EQUAL_CARET
		   | EQUAL_VERTICALBAR
;

expression: assignment_expression
          | expression ',' assignment_expression
;

constant_expression: conditional_expression
;

/* A.2.2 Declarations */
declaration: declaration_specifiers ';'
           | declaration_specifiers init_declarator_list ';'
;

declaration_specifiers: storage_class_specifier
                      | storage_class_specifier declaration_specifiers
                      | type_specifier
		      | type_specifier declaration_specifiers
		      | type_qualifier
		      | type_qualifier declaration_specifiers
		      | function_specifier
		      | function_specifier declaration_specifiers
;

init_declarator_list: init_declarator
                    | init_declarator_list ',' init_declarator
;

init_declarator: declarator
               | declarator '=' initializer
;

storage_class_specifier: TYPEDEF
                       | EXTERN
		       | STATIC
		       | AUTO
		       | REGISTER
;

/* 6.7.2 */
type_specifier: VOID
              | CHAR
	      | SHORT
	      | INT
	      | LONG
	      | FLOAT
	      | DOUBLE
	      | SIGNED
	      | UNSIGNED
	      | _BOOL
	      | _COMPLEX
	      | struct_or_union_specifier
	      | enum_specifier
              | typedef_name
;

struct_or_union_specifier: struct_or_union '{' struct_declaration_list '}'
                         | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
			 | struct_or_union IDENTIFIER
;

struct_or_union: STRUCT
               | UNION
;

struct_declaration_list: struct_declaration
                       | struct_declaration_list struct_declaration
;

struct_declaration: specifier_qualifier_list struct_declarator_list ';'
;

specifier_qualifier_list: type_specifier
                        | type_specifier specifier_qualifier_list
			| type_qualifier
			| type_qualifier specifier_qualifier_list
;

struct_declarator_list: struct_declarator
                      | struct_declarator_list ',' struct_declarator
;

struct_declarator: declarator
                 | ':' constant_expression
		 | declarator ':' constant_expression
;

enum_specifier: ENUM '{' enumerator_list '}'
              | ENUM IDENTIFIER '{' enumerator_list '}'
	      | ENUM '{' enumerator_list ',' '}'
	      | ENUM IDENTIFIER '{' enumerator_list ',' '}'
	      | ENUM IDENTIFIER
;

enumerator_list: enumerator
               | enumerator_list ',' enumerator
;

enumerator: IDENTIFIER
          | IDENTIFIER '=' constant_expression
;

type_qualifier: CONST
              | RESTRICT
	      | VOLATILE
;

function_specifier: INLINE
;

declarator: direct_declarator
          | pointer direct_declarator
;

/* 6.7.5 */
direct_declarator: IDENTIFIER
                 | '(' declarator ')'
		 | direct_declarator '[' ']'
		 | direct_declarator '[' assignment_expression ']'
		 | direct_declarator '[' type_qualifier_list ']'
		 | direct_declarator '[' type_qualifier_list assignment_expression ']'
		 | direct_declarator '[' STATIC assignment_expression ']'
		 | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
		 | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
		 | direct_declarator '[' '*' ']'
		 | direct_declarator '[' type_qualifier_list '*' ']'
		 | direct_declarator '(' parameter_type_list ')'
		 | direct_declarator '(' ')'
 		 | direct_declarator '(' identifier_list ')'
;

pointer: '*'
       | '*' type_qualifier_list
       | '*' pointer
       | '*' type_qualifier_list pointer
;

type_qualifier_list: type_qualifier
                   | type_qualifier_list type_qualifier
;

parameter_type_list: parameter_list
                   | parameter_list ',' TRIPLEDOT
;

parameter_list: parameter_declaration
              | parameter_list ',' parameter_declaration
;

parameter_declaration: declaration_specifiers declarator
                     | declaration_specifiers
		     | declaration_specifiers abstract_declarator
;

identifier_list: IDENTIFIER
               | identifier_list ',' IDENTIFIER
;

type_name: specifier_qualifier_list
         | specifier_qualifier_list abstract_declarator
;

abstract_declarator: pointer
                   | direct_abstract_declarator
		   | pointer direct_abstract_declarator
;

/* 6.7.6 */
direct_abstract_declarator: '(' abstract_declarator ')'
                          | '[' ']'
			  | '[' assignment_expression ']'
			  | '[' type_qualifier_list ']'
			  | '[' type_qualifier_list assignment_expression ']'
			  | direct_abstract_declarator '[' ']'
			  | direct_abstract_declarator '[' assignment_expression ']'
			  | direct_abstract_declarator '[' type_qualifier_list']'
			  | direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
			  | '[' STATIC assignment_expression ']'
			  | '[' STATIC type_qualifier_list assignment_expression ']'
			  | direct_abstract_declarator '[' STATIC assignment_expression ']'
			  | direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
			  | '[' type_qualifier_list STATIC assignment_expression ']'
			  | direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
			  | '[' '*' ']'
			  | direct_abstract_declarator '[' '*' ']'
			  | '(' ')'
			  | '(' parameter_type_list ')'
			  | direct_abstract_declarator '(' ')'
			  | direct_abstract_declarator '(' parameter_type_list ')'
;

initializer: assignment_expression
           | '{' initializer_list '}'
	   | '{' initializer_list ',' '}'
;

initializer_list: initializer
                | designation initializer
		| initializer_list ',' initializer
		| initializer_list ',' designation initializer
;

designation: designator_list '='
;

designator_list: designator
               | designator_list designator
;

designator: '[' constant_expression ']'
          | '.' IDENTIFIER
;

/* 6.7.7 */
typedef_name: TYPE_NAME;

/* A.2.3 Statements */
statement: labeled_statement
         | compound_statement
	 | expression_statement
	 | selection_statement
	 | iteration_statement
	 | jump_statement
;

labeled_statement: IDENTIFIER ':' statement
                 | CASE constant_expression ':' statement
		 | DEFAULT ':' statement
;

compound_statement: '{' '}'
                  | '{' block_item_list '}'
;

block_item_list: block_item
               | block_item_list block_item
;

block_item: declaration
          | statement
;

expression_statement: ';'
                    | expression ';'
;

selection_statement: IF '(' expression ')' statement
                   | IF '(' expression ')' statement ELSE statement
		   | SWITCH '(' expression ')' statement
;

iteration_statement: WHILE '(' expression ')' statement
                   | DO statement WHILE '(' expression ')' ';'
		   | FOR '(' ';' ';' ')' statement
		   | FOR '(' ';' ';' expression ')' statement
		   | FOR '(' ';' expression ';' ')' statement
		   | FOR '(' ';' expression ';' expression ')' statement
		   | FOR '(' expression ';' ';' ')' statement
		   | FOR '(' expression ';' ';' expression ')' statement
		   | FOR '(' expression ';' expression ';' ')' statement
		   | FOR '(' expression ';' expression ';' expression ')' statement
		   | FOR '(' declaration ';' ')' statement
		   | FOR '(' declaration ';' expression ')' statement
		   | FOR '(' declaration expression ';' ')' statement
		   | FOR '(' declaration expression ';' expression ')' statement
;

jump_statement: GOTO IDENTIFIER ';'
              | CONTINUE ';'
	      | BREAK ';'
	      | RETURN ';'
	      | RETURN expression ';'
;

/* A.2.4 External definitions */
/* translation_unit: external_declaration */
/*                 | translation_unit external_declaration */
/* ; */

/* external_declaration: function_definition */
/*                     | declaration */
/* ; */

function_definition: declaration_specifiers declarator compound_statement
                   | declaration_specifiers declarator declaration_list compound_statement
;

declaration_list: declaration
                | declaration_list declaration
;

/* A.3 Preprocessing directives */
/* preprocessing_file: group */
/* ; */

group: group_part
     | group group_part
;

group_part: if_section
          | control_line
          | text_line
	  | '#' non_directive
          | function_definition
          | declaration
;

if_section: if_group endif_line
          | if_group else_group endif_line
          | if_group elif_groups endif_line
	  | if_group elif_groups else_group endif_line
;

if_group: PIF constant_expression NEW_LINE
        | PIF constant_expression NEW_LINE group
	| PIFDEF IDENTIFIER NEW_LINE
	| PIFDEF IDENTIFIER NEW_LINE group
	| PIFNDEF IDENTIFIER NEW_LINE
	| PIFNDEF IDENTIFIER NEW_LINE group
;

elif_groups: elif_group
           | elif_groups elif_group
;

elif_group: PELIF constant_expression NEW_LINE
          | PELIF constant_expression NEW_LINE group
;

else_group: PELSE NEW_LINE
          | PELSE NEW_LINE group
;

endif_line: PENDIF NEW_LINE
;
/* 6.10 */
control_line: '#' INCLUDE HEADER_NAME
            | '#' DEFINE IDENTIFIER replacement_list NEW_LINE
	    | '#' DEFINE IDENTIFIER LPAREN ')' replacement_list NEW_LINE
	    | '#' DEFINE IDENTIFIER LPAREN identifier_list ')' replacement_list NEW_LINE
	    | '#' DEFINE IDENTIFIER LPAREN TRIPLEDOT ')' replacement_list NEW_LINE
	    | '#' DEFINE IDENTIFIER LPAREN identifier_list ',' TRIPLEDOT ')'
	                                   replacement_list NEW_LINE
	    | '#' UNDEF IDENTIFIER NEW_LINE
	    | '#' LINE pp_tokens NEW_LINE
	    | '#' ERROR NEW_LINE
	    | '#' ERROR pp_tokens NEW_LINE
	    | '#' PRAGMA NEW_LINE
	    | '#' PRAGMA pp_tokens NEW_LINE
	    | '#' NEW_LINE
;

text_line: NEW_LINE
         | pp_tokens NEW_LINE
;

non_directive: pp_tokens NEW_LINE
;

replacement_list: /* Empty */
                | pp_tokens
;

pp_tokens: preprocessing_token
         | pp_tokens preprocessing_token
;

/* Error Recovery Rulers */

%%

/* Called by yyparse on error.  */
void
yyerror(YYLTYPE *llocp, yyscan_t yyscanner, PCONTEXT pcontext, char const *msg)
{
  fprintf(stderr, "%s Line %d(%d)-%d(%d): %s\n",
          llocp->filename,
          llocp->first_line,
          llocp->first_column,
          llocp->last_line,
          llocp->last_column,
          msg);
}

int
main(int argc, char * argv[])
{
  CONTEXT context;
  yyscan_t scanner;
  FILE * infile;

  ++argv, --argc;  /* skip over program name */

  if ((argc > 0) &&(strcmp(argv [ 0 ], "-d") == 0)) {
    yydebug = 1;
    ++argv, --argc;  /* skip over this option */
  }
  else
    yydebug = 0;

  if (argc == 0) {
    fprintf(stderr, "Error: no specify filename found\n");
    return -1;
  }
  infile = fopen(argv[0], "r");
  if (infile == NULL) {
    fprintf(stderr, "Error: can't open file '%s'\n", argv [ 0 ]);
    return -2;
  }

  /* 初始化上下文 */
  memset(&context, 0, sizeof(CONTEXT));
  context.filename = argv[0];

  /* 解析文件 */
  yylex_init_extra((YY_EXTRA_TYPE)&context, &scanner);
  yyset_in(infile, scanner);
  yyparse(scanner, &context);
  yylex_destroy(scanner);

  fclose(infile);
  return 0;
}
