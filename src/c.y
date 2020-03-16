/*
 *
 * C 语言解析器，根据文档 N1256.pdf C98/99 设计。
 *
 *     WG14/N1256 Committee Draft — Septermber 7, 2007
 *     ISO/IEC 9899:TC3
 *
 * 基本按照附录中关于语法定义和函数库的内容来编写支持的格式，
 * 列出支持的和不支持的内容。
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

下面的内容摘自 N1256.pdf
---------------------------------------------------------------

A.1 Lexical grammar

A.1.1 Lexical elements

(6.4) token:
    keyword
    identifier
    constant
    string-literal
    punctuator

(6.4) preprocessing-token:
    header-name
    identifier
    pp-number
    character-constant
    string-literal
    punctuator

each non-white-space character that cannot be one of the above

A.1.2 Keywords

(6.4.1) keyword: one of
    auto
    break
    case
    char
    const
    continue
    default
    do
    double
    else
    enum
    extern
    float
    for
    goto
    if
    inline
    int
    long
    register
    restrict
    return
    short
    signed
    sizeof
    static
    struct
    switch
    typedef
    union
    unsigned
    void
    volatile
    while
    _Bool
    _Complex
    _Imaginary

A.1.3 Identifiers

(6.4.2.1) identifier:
    identifier-nondigit
    identifier identifier-nondigit
    identifier digit

(6.4.2.1) identifier-nondigit:
    nondigit
    universal-character-name
other implementation-defined characters

(6.4.2.1) nondigit: one of
    _ a b c d e f g h i j k l m
    n o p q r s t u v w x y z
    A B C D E F G H I J K L M
    N O P Q R S T U V W X Y Z

(6.4.2.1) digit: one of
    0 1 2 3 4 5 6 7 8 9

A.1.4 Universal character names

(6.4.3) universal-character-name:
    \u hex-quad
    \U hex-quad hex-quad
(6.4.3) hex-quad:
    hexadecimal-digit hexadecimal-digit
        hexadecimal-digit hexadecimal-digit

A.1.5 Constants

(6.4.4) constant:
    integer-constant
    floating-constant
    enumeration-constant
    character-constant

(6.4.4.1) integer-constant:
    decimal-constant integer-suffixopt
    octal-constant integer-suffixopt
    hexadecimal-constant integer-suffixopt

(6.4.4.1) decimal-constant:
    nonzero-digit
    decimal-constant digit

(6.4.4.1) octal-constant:
    0
    octal-constant octal-digit

(6.4.4.1) hexadecimal-constant:
    hexadecimal-prefix hexadecimal-digit
    hexadecimal-constant hexadecimal-digit

(6.4.4.1) hexadecimal-prefix: one of
    0x 0X

(6.4.4.1) nonzero-digit: one of
    1 2 3 4 5 6 7 8 9

(6.4.4.1) octal-digit: one of
    0 1 2 3 4 5 6 7

(6.4.4.1) hexadecimal-digit: one of
    0 1 2 3 4 5 6 7 8 9
    a b c d e f
    A B C D E F

(6.4.4.1) integer-suffix:
    unsigned-suffix long-suffixopt
    unsigned-suffix long-long-suffix
    long-suffix unsigned-suffixopt
    long-long-suffix unsigned-suffixopt

(6.4.4.1) unsigned-suffix: one of
    u U

(6.4.4.1) long-suffix: one of
    l L

(6.4.4.1) long-long-suffix: one of
    ll LL

(6.4.4.2) floating-constant:
    decimal-floating-constant
    hexadecimal-floating-constant

(6.4.4.2) decimal-floating-constant:
    fractional-constant exponent-partopt floating-suffixopt
    digit-sequence exponent-part floating-suffixopt

(6.4.4.2) hexadecimal-floating-constant:
    hexadecimal-prefix hexadecimal-fractional-constant
        binary-exponent-part floating-suffixopt
    hexadecimal-prefix hexadecimal-digit-sequence
        binary-exponent-part floating-suffixopt

(6.4.4.2) fractional-constant:
    digit-sequenceopt . digit-sequence
    digit-sequence .

(6.4.4.2) exponent-part:
    e signopt digit-sequence
    E signopt digit-sequence

(6.4.4.2) sign: one of
    + -

(6.4.4.2) digit-sequence:
    digit
    digit-sequence digit

(6.4.4.2) hexadecimal-fractional-constant:
    hexadecimal-digit-sequenceopt .
        hexadecimal-digit-sequence
    hexadecimal-digit-sequence .

(6.4.4.2) binary-exponent-part:
    p signopt digit-sequence
    P signopt digit-sequence

(6.4.4.2) hexadecimal-digit-sequence:
    hexadecimal-digit
    hexadecimal-digit-sequence hexadecimal-digit

(6.4.4.2) floating-suffix: one of
    f l F L

(6.4.4.3) enumeration-constant:
    identifier

(6.4.4.4) character-constant:
    ' c-char-sequence '
    L' c-char-sequence '

(6.4.4.4) c-char-sequence:
    c-char
    c-char-sequence c-char

(6.4.4.4) c-char:
    any member of the source character set except
    the single-quote ', backslash \, or new-line character
    escape-sequence

(6.4.4.4) escape-sequence:
    simple-escape-sequence
    octal-escape-sequence
    hexadecimal-escape-sequence
    universal-character-name

(6.4.4.4) simple-escape-sequence: one of
    \' \" \? \\
    \a \b \f \n \r \t \v
    (6.4.4.4) octal-escape-sequence:
    \ octal-digit
    \ octal-digit octal-digit
    \ octal-digit octal-digit octal-digit

(6.4.4.4) hexadecimal-escape-sequence:
    \x hexadecimal-digit
    hexadecimal-escape-sequence hexadecimal-digit

A.1.6 String literals

(6.4.5) string-literal:
    " s-char-sequenceopt "
    L" s-char-sequenceopt "

(6.4.5) s-char-sequence:
    s-char
    s-char-sequence s-char

(6.4.5) s-char:
    any member of the source character set except
        the double-quote ", backslash \, or new-line character
    escape-sequence

A.1.7 Punctuators

(6.4.6) punctuator: one of
    [ ] ( ) { } . ->
    ++ -- & * + - ~ !
    / % << >> < > <= >= == != ^ | && ||
    ? : ; ...
    = *= /= %= += -= <<= >>= &= ^= |=
    , # ##
    <: :> <% %> %: %:%:

A.1.8 Header names

(6.4.7) header-name:
    < h-char-sequence >
    " q-char-sequence "

(6.4.7) h-char-sequence:
    h-char
    h-char-sequence h-char

(6.4.7) h-char:
    any member of the source character set except
        the new-line character and >

(6.4.7) q-char-sequence:
    q-char
    q-char-sequence q-char

(6.4.7) q-char:
    any member of the source character set except
        the new-line character and "

A.1.9 Preprocessing numbers

(6.4.8) pp-number:
    digit
    . digit
    pp-number digit
    pp-number identifier-nondigit
    pp-number e sign
    pp-number E sign
    pp-number p sign
    pp-number P sign
    pp-number .

A.2 Phrase structure grammar

A.2.1 Expressions

(6.5.1) primary-expression:
    identifier
    constant
    string-literal
    ( expression )

(6.5.2) postfix-expression:
    primary-expression
    postfix-expression [ expression ]
    postfix-expression ( argument-expression-listopt )
    postfix-expression . identifier
    postfix-expression -> identifier
    postfix-expression ++
    postfix-expression --
    ( type-name ) { initializer-list }
    ( type-name ) { initializer-list , }

(6.5.2) argument-expression-list:
    assignment-expression
    argument-expression-list , assignment-expression

(6.5.3) unary-expression:
    postfix-expression
    ++ unary-expression
    -- unary-expression
    unary-operator cast-expression
    sizeof unary-expression
    sizeof ( type-name )

(6.5.3) unary-operator: one of
    & * + - ~ !

(6.5.4) cast-expression:
    unary-expression
    ( type-name ) cast-expression

(6.5.5) multiplicative-expression:
    cast-expression
    multiplicative-expression * cast-expression
    multiplicative-expression / cast-expression
    multiplicative-expression % cast-expression

(6.5.6) additive-expression:
    multiplicative-expression
    additive-expression + multiplicative-expression
    additive-expression - multiplicative-expression

(6.5.7) shift-expression:
    additive-expression
    shift-expression << additive-expression
    shift-expression >> additive-expression

(6.5.8) relational-expression:
    shift-expression
    relational-expression < shift-expression
    relational-expression > shift-expression
    relational-expression <= shift-expression
    relational-expression >= shift-expression

(6.5.9) equality-expression:
    relational-expression
    equality-expression == relational-expression
    equality-expression != relational-expression

(6.5.10) AND-expression:
    equality-expression
    AND-expression & equality-expression

(6.5.11) exclusive-OR-expression:
    AND-expression
    exclusive-OR-expression ^ AND-expression

(6.5.12) inclusive-OR-expression:
    exclusive-OR-expression
    inclusive-OR-expression | exclusive-OR-expression

(6.5.13) logical-AND-expression:
    inclusive-OR-expression
    logical-AND-expression && inclusive-OR-expression

(6.5.14) logical-OR-expression:
    logical-AND-expression
    logical-OR-expression || logical-AND-expression

(6.5.15) conditional-expression:
    logical-OR-expression
    logical-OR-expression ? expression : conditional-expression

(6.5.16) assignment-expression:
    conditional-expression
    unary-expression assignment-operator assignment-expression

(6.5.16) assignment-operator: one of
    = *= /= %= += -= <<= >>= &= ^= |=

(6.5.17) expression:
    assignment-expression
    expression , assignment-expression

(6.6) constant-expression:
    conditional-expression

A.2.2 Declarations

(6.7) declaration:
    declaration-specifiers init-declarator-listopt ;

(6.7) declaration-specifiers:
    storage-class-specifier declaration-specifiersopt
    type-specifier declaration-specifiersopt
    type-qualifier declaration-specifiersopt
    function-specifier declaration-specifiersopt

(6.7) init-declarator-list:
    init-declarator
    init-declarator-list , init-declarator

(6.7) init-declarator:
    declarator
    declarator = initializer

(6.7.1) storage-class-specifier:
    typedef
    extern
    static
    auto
    register

(6.7.2) type-specifier:
    void
    char
    short
    int
    long
    float
    double
    signed
    unsigned
    _Bool
    _Complex
    struct-or-union-specifier *
    enum-specifier
    typedef-name

(6.7.2.1) struct-or-union-specifier:
    struct-or-union identifieropt { struct-declaration-list }
    struct-or-union identifier

(6.7.2.1) struct-or-union:
    struct
    union

(6.7.2.1) struct-declaration-list:
    struct-declaration
    struct-declaration-list struct-declaration

(6.7.2.1) struct-declaration:
    specifier-qualifier-list struct-declarator-list ;

(6.7.2.1) specifier-qualifier-list:
    type-specifier specifier-qualifier-listopt
    type-qualifier specifier-qualifier-listopt

(6.7.2.1) struct-declarator-list:
    struct-declarator
    struct-declarator-list , struct-declarator

(6.7.2.1) struct-declarator:
    declarator
    declaratoropt : constant-expression

(6.7.2.2) enum-specifier:
    enum identifieropt { enumerator-list }
    enum identifieropt { enumerator-list , }
    enum identifier

(6.7.2.2) enumerator-list:
    enumerator
    enumerator-list , enumerator

(6.7.2.2) enumerator:
    enumeration-constant
    enumeration-constant = constant-expression

(6.7.3) type-qualifier:
    const
    restrict
    volatile

(6.7.4) function-specifier:
    inline

(6.7.5) declarator:
    pointeropt direct-declarator

(6.7.5) direct-declarator:
    identifier
    ( declarator )
    direct-declarator [ type-qualifier-listopt assignment-expressionopt ]
    direct-declarator [ static type-qualifier-listopt assignment-expression ]
    direct-declarator [ type-qualifier-list static assignment-expression ]
    direct-declarator [ type-qualifier-listopt * ]
    direct-declarator ( parameter-type-list )
    direct-declarator ( identifier-listopt )

(6.7.5) pointer:
    * type-qualifier-listopt
    * type-qualifier-listopt pointer

(6.7.5) type-qualifier-list:
    type-qualifier
    type-qualifier-list type-qualifier

(6.7.5) parameter-type-list:
    parameter-list
    parameter-list , ...

(6.7.5) parameter-list:
    parameter-declaration
    parameter-list , parameter-declaration

(6.7.5) parameter-declaration:
    declaration-specifiers declarator
    declaration-specifiers abstract-declaratoropt

(6.7.5) identifier-list:
    identifier
    identifier-list , identifier

(6.7.6) type-name:
    specifier-qualifier-list abstract-declaratoropt


(6.7.6) abstract-declarator:
    pointer
    pointeropt direct-abstract-declarator

(6.7.6) direct-abstract-declarator:
    ( abstract-declarator )
    direct-abstract-declaratoropt [ type-qualifier-listopt
    assignment-expressionopt ]
    direct-abstract-declaratoropt [ static type-qualifier-listopt
    assignment-expression ]
    direct-abstract-declaratoropt [ type-qualifier-list static
    assignment-expression ]
    direct-abstract-declaratoropt [ * ]
    direct-abstract-declaratoropt ( parameter-type-listopt )

(6.7.7) typedef-name:
    identifier

(6.7.8) initializer:
    assignment-expression
    { initializer-list }
    { initializer-list , }

(6.7.8) initializer-list:
    designationopt initializer
    initializer-list , designationopt initializer

(6.7.8) designation:
    designator-list =

(6.7.8) designator-list:

    designator
    designator-list designator

(6.7.8) designator:
    [ constant-expression ]
    . identifier

A.2.3 Statements

(6.8) statement:
    labeled-statement
    compound-statement
    expression-statement
    selection-statement
    iteration-statement
    jump-statement

(6.8.1) labeled-statement:
    identifier : statement
    case constant-expression : statement
    default : statement

(6.8.2) compound-statement:
    { block-item-listopt }

(6.8.2) block-item-list:
    block-item
    block-item-list block-item

(6.8.2) block-item:
    declaration
    statement

(6.8.3) expression-statement:
    expressionopt ;

(6.8.4) selection-statement:
    if ( expression ) statement
    if ( expression ) statement else statement
    switch ( expression ) statement

(6.8.5) iteration-statement:
    while ( expression ) statement
    do statement while ( expression ) ;
    for ( expressionopt ; expressionopt ; expressionopt ) statement
    for ( declaration expressionopt ; expressionopt ) statement

(6.8.6) jump-statement:
    goto identifier ;
    continue ;
    break ;
    return expressionopt ;

A.2.4 External definitions

(6.9) translation-unit:
    external-declaration
    translation-unit external-declaration

(6.9) external-declaration:
    function-definition
    declaration

(6.9.1) function-definition:
    declaration-specifiers declarator declaration-listopt compound-statement

(6.9.1) declaration-list:
    declaration
    declaration-list declaration

A.3 Preprocessing directives

(6.10) preprocessing-file:
    groupopt

(6.10) group:
    group-part
    group group-part

(6.10) group-part:
    if-section
    control-line
    text-line
    # non-directive

(6.10) if-section:
    if-group elif-groupsopt else-groupopt endif-line

(6.10) if-group:
    # if constant-expression new-line groupopt
    # ifdef identifier new-line groupopt
    # ifndef identifier new-line groupopt

(6.10) elif-groups:
    elif-group
    elif-groups elif-group

(6.10) elif-group:
    # elif constant-expression new-line groupopt

(6.10) else-group:
    # else new-line groupopt

(6.10) endif-line:
    # endif new-line

(6.10) control-line:
    # include pp-tokens new-line
    # define identifier replacement-list new-line
    # define identifier lparen identifier-listopt )
    replacement-list new-line
    # define identifier lparen ... ) replacement-list new-line
    # define identifier lparen identifier-list , ... )
    replacement-list new-line
    # undef identifier new-line
    # line pp-tokens new-line
    # error pp-tokensopt new-line
    # pragma pp-tokensopt new-line
    # new-line

(6.10) text-line:
    pp-tokensopt new-line

(6.10) non-directive:
    pp-tokens new-line

(6.10) lparen:
    a ( character not immediately preceded by white-space

(6.10) replacement-list:
    pp-tokensopt

(6.10) pp-tokens:
    preprocessing-token
    pp-tokens preprocessing-token

(6.10) new-line:
    the new-line character

---------------------------------------------------------------
以上内容摘自 N1256.pdf


对语法的修改：

    - 增加主入口规则

    entry: | preprocessing_file ;

    同时修改规则 group_part 增加

          | function_definition
          | declaration

    并且删除没有使用的规则 translation_unit 和 external_declaration

    这样就涵盖了全部语法，其中前者用于解析预处理语句，后者用于解析正常语句；

    - 规则 6.7.7

      typedef_name: TYPE_NAME

已知的 shift/reduce 冲突，共 1 个：

    if (A)
        if (B)
            statement;
      else
          statement;

   这个 else 到底是和第一个还是第二个 if 匹配，默认是第二个 (innermost)
 */

%code top {
  #include <stdio.h>
  #include <strings.h>
  #include <limits.h>
}

%code requires {
  #include "ptypes.h"
  typedef void * yyscan_t;

  #define YYSTYPE YYSTYPE
  typedef char * YYSTYPE;
}

%code {
  extern int yylex_init(yyscan_t scanner);
  extern int yylex_init_extra(YY_EXTRA_TYPE pcontext, yyscan_t scanner);
  extern int yylex(YYSTYPE * yylval_param, YYLTYPE * yylloc_param , yyscan_t yyscanner);
  extern int yylex_destroy(yyscan_t scanner);
  extern void yyset_in(FILE * file, yyscan_t scanner);
  void yyerror(YYLTYPE *locp, yyscan_t yyscanner, PCONTEXT pcontext, char const *msg);
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

entry: /* Empty */
     | preprocessing_file
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
preprocessing_file: group
;

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
control_line: '#' INCLUDE pp_tokens NEW_LINE
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
yyerror(YYLTYPE *locp, yyscan_t yyscanner, PCONTEXT pcontext, char const *msg)
{
  fprintf(stderr, "Line %d: %s\n", locp->first_line, msg);
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

  /* 解析文件 */
  yylex_init_extra((YY_EXTRA_TYPE)&context, &scanner);
  yyset_in(infile, scanner);
  yyparse(scanner, &context);
  yylex_destroy(scanner);

  fclose(infile);
  return 0;
}
