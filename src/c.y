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


 */

%{
  #include <stdio.h>
  #include <limits.h>
  
  #define YYSTYPE char *
  typedef void * yyscan_t;
  extern int yylex_init (yyscan_t *);
  extern int yylex_destroy (yyscan_t);
  extern void yyset_in  ( FILE *, yyscan_t );
  void yyerror (void *, void *, PCONTEXT, char const *);

  int OUTPUT_WIDTH = 60;
  int INDENT_UNIT = 4;
%}

%debug
%token-table
%pure-parser
%locations

%lex-param   {void *scanner}
%parse-param {void *scanner}
%parse-param {PCONTEXT pcontext}

%token  ADD    '+'
%token  MINUS  '-'
%token  MUL    '*'
%token  DIV    '/'
%token  EQ     '='
%token  GT     '>'
%token  LT     '<'
%token  GE     ">="
%token  LE     "<="
%token  NE     "<>"

%token  AND    "and"
%token  OR     "or"
%token  NOT    "not"
%token  TRUE
%token  FALSE
%token  XOX

%token  DOT          '.'
%token  COMMA        ','
%token  COLON        ':'
%token  SEMICOLON    ';'
%token  INDICATOR    '^'

%token  IS           ":="

%token  LP    '('
%token  RP    ')'

%token  ENTRY
%token  PROCEDURE
%token  FUNCTION
%token  CONST
%token  VAR
%token  TYPE
%token  RECORD
%token  ARRAY
%token  OF
%token  FOR
%token  WHILE
%token  IF
%token  DO
%token  TO
%token  REPEAT
%token  UNTIL
%token  ELSE
%token  THEN

%token  LB
%token  RB

%token  INTEGER
%token  FLOAT
%token  CHAR
%token  BOOLEAN
%token  STRING
%token  DOUBLE

%token  TOK_STRING
%token  TOK_NAME
%token  TOK_TYPENAME
%token  TOK_INTEGER
%token  TOK_NUMBER

%left GT LT EQ LE NE GE
%left OR AND

%left ADD MINUS
%left MUL DIV
%left XOX

%right NOT
%right NEG

%destructor { free ( $$ ); } name expr exprs

%% /* Grammar rules and actions follow.  */

algorithm:  entry funcs
                                { /* 输出类结束语句 */
                                  print_class_end ( pcontext );
                                  pcontext -> indent -= INDENT_UNIT;
                                  /* 输出文件结束语句 */
                                  print_file_footer ( pcontext );
                                  }
                ;
entry:            /* Empty */
                | ENTRY EQ TOK_NAME
                ;

funcs:             /* Empty */
                | decl_func
                | decl_func SEMICOLON funcs
                ;

decl_func:        func_header decls func_body
                                { /*  */
                                  print_func_end ( @$.last_line, pcontext );
                                  pcontext -> findex ++;
                                  pcontext -> indent -= INDENT_UNIT;
                                  }
                ;

func_header:           FUNCTION TOK_NAME LP paras RP COLON TOK_TYPENAME SEMICOLON
                                { int i;
                                  i = pcontext -> findex;
                                  if ( i >= MAX_FUNC_NUMBER ) {
                                    fprintf (
                                        stderr,
                                        "Error: The stack of Functions overflowed\n"
                                        );
                                    YYABORT;
                                  }
                                  pcontext -> funcs [ i ].name = strdup ( $2 );
                                  pcontext -> funcs [ i ].rettype = strdup ( $7 );

                                  /* 是否入口函数 */
                                  if ( i == 0 ) {
                                    print_class_begin ( pcontext );
                                    pcontext -> indent += INDENT_UNIT;
                                  }

                                  /* 打印函数头 */
                                  print_func_begin ( @$.first_line, pcontext );
                                  pcontext -> indent += INDENT_UNIT;
                                  }

                | PROCEDURE TOK_NAME LP paras RP SEMICOLON
                                { int i;
                                  i = pcontext -> findex;
                                  if ( i >= MAX_FUNC_NUMBER ) {
                                    fprintf (
                                        stderr,
                                        "Error: The stack of Functions overflowed\n"
                                        );
                                    YYABORT;
                                  }
                                  pcontext -> funcs [ i ].name = strdup ( $2 );
                                  pcontext -> funcs [ i ].rettype = 0;

                                  /* 是否入口函数 */
                                  if ( i == 0 ) {
                                    print_class_begin ( pcontext );
                                    pcontext -> indent += INDENT_UNIT;
                                  }

                                  /* 打印函数头 */
                                  print_func_begin ( @$.first_line, pcontext );
                                  pcontext -> indent += INDENT_UNIT;
                                  }
                ;

func_body:        LB stmts RB
                ;

paras:             /* Empty */
                |  decl_para
                |  decl_para COMMA paras
                ;

decl_para:         para_names COLON TOK_TYPENAME
                            { PFUNC pfunc = pcontext -> funcs + pcontext -> findex;
                              int i;
                              for ( i = 0; i < MAX_PARA_NUMBER; i ++ ) {
                                if ( pfunc -> paras [ i ].name == 0 )
                                  break;
                                if ( pfunc -> paras [ i ].type == 0 ) {
                                  pfunc -> paras [ i ].varflag = 0;
                                  pfunc -> paras [ i ].type = strdup( $3 );
                                }
                              }
                             }
                |  VAR TOK_NAME COLON TOK_TYPENAME
                            { PFUNC pfunc = pcontext -> funcs + pcontext -> findex;
                              int i = 0;
                              while ( pfunc -> paras [ i ].name ) {
                                i ++;
                                if ( i > MAX_PARA_NUMBER ) {
                                  fprintf (
                                      stderr,
                                      "Error: The stack of Parameters overflowed\n"
                                      );
                                  YYABORT;
                                }
                              }
                              pfunc -> paras [ i ].varflag = 1;
                              pfunc -> paras [ i ].name = strdup( $2 );
                              pfunc -> paras [ i ].type = strdup( $4 );
                             }
                ;

para_names:        TOK_NAME { PFUNC pfunc = pcontext -> funcs + pcontext -> findex;
                              int i = 0;
                              while ( pfunc -> paras [ i ].name ) {
                                i ++;
                                if ( i > MAX_PARA_NUMBER ) {
                                  fprintf (
                                      stderr,
                                      "Error: The stack of Parameters overflowed\n"
                                      );
                                  YYABORT;
                                }
                              }
                              pfunc -> paras [ i ].name = strdup( $1 );
                             }

                |  TOK_NAME
                             { PFUNC pfunc = pcontext -> funcs + pcontext -> findex;
                              int i = 0;
                              while ( pfunc -> paras [ i ].name ) {
                                i ++;
                                if ( i > MAX_PARA_NUMBER ) {
                                  fprintf (
                                      stderr,
                                      "Error: The stack of Parameters overflowed\n"
                                      );
                                  YYABORT;
                                }
                              }
                              pfunc -> paras [ i ].name = strdup( $1 );
                             }
                   COMMA para_names
                ;

decls:            /* Empty */
                | CONST constants decls
                | VAR vars decls
                                { /* 输出变量表，然后重新初始化 */
                                  PVAR pvar = pcontext -> vars;
                                  int i;
                                  for ( i = 0; i < MAX_VARS_NUMBER; i ++, pvar ++ ) {
                                    if ( ! pvar -> name )
                                      break;
                                    print_indent( pcontext -> indent );
                                    printf (
                                      "%s = new %s ()\n",
                                            pvar -> name,
                                            pvar -> type
                                            );
                                    free ( pvar -> name );
                                    free ( pvar -> type );
                                    pvar -> name = 0;
                                    pvar -> type = 0;
                                  }
                                }
                | TYPE types decls
                ;

constants:        /* Empty */
                | decl_const SEMICOLON constants
                ;

decl_const:       TOK_NAME EQ TOK_INTEGER
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = %s\n",
                                      $1,
                                      $3
                                      );
                                }
                | TOK_NAME COLON INTEGER EQ TOK_INTEGER
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = %s\n",
                                      $1,
                                      $5
                                      );
                                }
                | TOK_NAME COLON STRING EQ TOK_STRING
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = %s\n",
                                      $1,
                                      $5
                                      );
                                }
                | TOK_NAME COLON DOUBLE EQ TOK_NUMBER
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = %s\n",
                                      $1,
                                      $5
                                      );
                                }
                | TOK_NAME COLON BOOLEAN EQ TRUE
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = True\n",
                                      $1
                                      );
                                }
                | TOK_NAME COLON BOOLEAN EQ FALSE
                                { /*  输出常量 */
                                  print_indent ( pcontext -> indent );
                                  printf (
                                      "%s = False\n",
                                      $1
                                      );
                                }
                ;

vars:             /* Empty */
                | decl_var SEMICOLON vars
                ;

decl_var:         var_names COLON TOK_TYPENAME
                            { PVAR pvar = pcontext -> vars;
                              int i;
                              for ( i = 0; i < MAX_VARS_NUMBER; i ++ ) {
                                if ( pvar -> name == 0 )
                                  break;
                                if ( pvar -> type == 0 )
                                  pvar -> type = strdup( $3 );
                                pvar ++;
                              }
                             }
                ;

var_names:        TOK_NAME  { PVAR pvar = pcontext -> vars;
                              int i = 0;
                              while ( pvar -> name ) {
                                i ++;
                                pvar ++;
                                if ( i > MAX_VARS_NUMBER ) {
                                  fprintf (
                                      stderr,
                                      "Error: The stack of Variables overflowed\n"
                                      );
                                  YYABORT;
                                }
                              }
                              pvar -> name = strdup( $1 );
                             }
                | TOK_NAME
                            { PVAR pvar = pcontext -> vars;
                              int i = 0;
                              while ( pvar -> name ) {
                                i ++;
                                pvar ++;
                                if ( i > MAX_VARS_NUMBER ) {
                                  fprintf (
                                      stderr,
                                      "Error: The stack of Variables overflowed\n"
                                      );
                                  YYABORT;
                                }
                              }
                              pvar -> name = strdup( $1 );
                             }
                  COMMA var_names
                ;

types:            /* Empty */
                | decl_type SEMICOLON types
                ;

decl_type:        TOK_NAME EQ TOK_TYPENAME
                | TOK_NAME EQ INDICATOR TOK_TYPENAME
                | TOK_NAME EQ RECORD OF vars RB
                ;

stmts:            /* Empty */
                | SEMICOLON
                | stmt
                | stmt SEMICOLON stmts
                ;

stmt:             stmt_if
                | stmt_loop
                | stmt_assign
                | stmt_call
                ;

stmt_call:        TOK_NAME LP { pcontext -> indent += INDENT_UNIT; }
                  exprs RP
                                { /*  恢复当前缩进 */
				  pcontext -> indent -= INDENT_UNIT;

				  if ( strlen( $4 ) == 0 ){
				    /* 没有实参的函数输出 */
				    snprintf ( pcontext -> stmt,
					       MAX_STMT_SIZE,
					       "self.%s( )",
					       $1
					       );
				  }
				  else {

				    if ( pcontext -> indent
				         + strlen( $1 )
				         + strlen( $4 )
				         + 10
				         < OUTPUT_WIDTH
				         ){
				      /* 没有折行的函数调用输出 */
				      snprintf( pcontext -> stmt,
                                                MAX_STMT_SIZE,
                                                "self.%s( %s )",
                                                $1,
                                                merge_expr( $4 )
                                                );

				      }
				      else {
					/* 折行输出 */
					snprintf( pcontext -> stmt,
						  MAX_STMT_SIZE,
						  "self.%s(",
						  $1
						  );
					int len = strlen( pcontext -> stmt);
					int indent = pcontext -> indent + INDENT_UNIT;
					increase_indent( pcontext -> stmt + len,
							 MAX_STMT_SIZE - len,
							 "\n",
							 indent
							 );
					len = strlen( pcontext -> stmt);
				        increase_indent( pcontext -> stmt + len,
							 MAX_STMT_SIZE - len,
							 $4,
							 indent
							 );
					len = strlen( pcontext -> stmt);
					increase_indent( pcontext -> stmt + len,
							 MAX_STMT_SIZE - len,
							 "\n)",
							 indent
							 );
				      }
				  }
                                  print_call_stmt ( @$.first_line, pcontext );
                                }
                ;

exprs:            /* Empty */   { $$ = strdup( "" ); }
                | expr          { $$ = strdup( $1 ); }
                | expr COMMA exprs
                                { /* 以逗号分开的多个表达式 */
                                  snprintf (
                                      pcontext -> expr,
                                      MAX_EXPR_SIZE,
                                      "%s,\n%s",
                                      $1,
                                      $3
                                      );
                                  $$ = strdup ( pcontext -> expr );
                                }
                ;

stmt_assign:      name IS   { pcontext -> indent += strlen( $1 ) + 3; }
                  expr
                                { /*  */
                                  int len = strlen( $1 ) + 3;
                                  pcontext -> indent -= len;
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "%s = ",
                                                  $1
                                                  )
                                       < 0
                                       ){
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  increase_indent(
                                                  pcontext -> stmt + len,
                                                  MAX_STMT_SIZE - len,
                                                  $4,
                                                  len + pcontext -> indent
                                                  );
                                  print_stmt ( @$.first_line, pcontext );
                                }
                ;

name:             TOK_NAME                { $$ = strdup ( $1 ); }
                | TOK_NAME INDICATOR      { $$ = strdup ( $1 ); }
                | TOK_NAME DOT name
                                { /*  */
                                  snprintf (
                                            pcontext -> stmt,
                                            MAX_STMT_SIZE,
                                            "%s.%s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> stmt );
                                }
                ;

compound_stmt:    stmt
                | LB stmts RB
                ;

stmt_if:          stmt_if_part compound_stmt
                                { /* 减少一个缩进单位 */
                                  pcontext -> indent -= INDENT_UNIT;
                                }
                | stmt_if_part compound_stmt ELSE
                                { /* 规则中动作 */
                                  print_indent ( pcontext -> indent - 1 );
                                  printf ( "else:\n" );
                                }
                  compound_stmt
                                { /* 减少一个缩进单位 */
                                  pcontext -> indent -= INDENT_UNIT;
                                }
                ;

stmt_if_part:    IF expr THEN
                                { /*  */
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "if %s:",
                                                  $2
                                                  ) < 0 ) {
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  print_control_stmt ( @$.first_line, pcontext );

                                  /* 增加一个缩进单位 */
                                  pcontext -> indent += INDENT_UNIT;
                                }
                ;

stmt_loop:        WHILE expr DO
                                { /* 打印语句头部，规则中动作 */
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "while %s:",
                                                  $2
                                                  ) < 0 ) {
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  print_control_stmt ( @$.first_line, pcontext );

                                  /* 增加一个缩进单位 */
                                  pcontext -> indent += INDENT_UNIT;
                                }
                  compound_stmt
                                { /* 减少一个缩进单位 */
                                  pcontext -> indent -= INDENT_UNIT;
                                }
                | REPEAT stmts
                                { /* 打印语句头部，规则中动作 */
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "while True:",
                                                  $2
                                                  ) < 0 ) {
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  print_control_stmt ( @$.first_line, pcontext );

                                  /* 增加一个缩进单位 */
                                  pcontext -> indent += INDENT_UNIT;
                                }
                  UNTIL expr
                                { /* 打印语句结束 */
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "if %s: break",
                                                  $5
                                                  ) < 0 ) {
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  print_stmt ( @$.first_line, pcontext );

                                  /* 减少一个缩进单位 */
                                  pcontext -> indent -= INDENT_UNIT;
                                }
                | FOR name IS expr TO expr DO
                                { /* 打印语句头部，规则中动作 */
                                  if ( snprintf ( pcontext -> stmt,
                                                  MAX_STMT_SIZE,
                                                  "for %s in range ( %s, %s ):",
                                                  $2,
                                                  $4,
                                                  $6
                                                  ) < 0 ) {
                                    fprintf ( stderr, "Error: memory fault\n" );
                                    YYABORT;
                                  }
                                  print_control_stmt ( @$.first_line, pcontext );

                                  /* 增加一个缩进单位 */
                                  pcontext -> indent += INDENT_UNIT;
                                }
                  compound_stmt
                                { /* 减少一个缩进单位 */
                                  pcontext -> indent -= INDENT_UNIT;
                                }
                ;

expr:             TOK_STRING        { $$ = strdup ( $1 ); }
                | TOK_NUMBER        { $$ = strdup ( $1 ); }
                | TOK_INTEGER       { $$ = strdup ( $1 ); }
                | TRUE              { $$ = strdup ( $1 ); }
                | FALSE             { $$ = strdup ( $1 ); }
                | name              { $$ = strdup ( $1 ); }
                | expr XOX expr     { $$ = $1 == $3 ? $3 : $1; }
                | TOK_NAME LP { pcontext -> indent += INDENT_UNIT; }
                  exprs RP
                                { /*  恢复当前缩进 */
				  pcontext -> indent -= INDENT_UNIT;

				  if ( strlen( $4 ) == 0 ){
				    /* 没有实参的函数输出 */
				    snprintf ( pcontext -> expr,
					       MAX_EXPR_SIZE,
					       "%s( )",
					       $1
					       );
				  }
				  else {

				    if ( pcontext -> indent
				         + strlen( $1 )
				         + strlen( $4 )
				         + 4
				         < OUTPUT_WIDTH
				         ){
				      /* 没有折行的函数调用输出 */
				      snprintf( pcontext -> expr,
                                                MAX_EXPR_SIZE,
                                                "%s( %s )",
                                                $1,
                                                merge_expr( $4 )
                                                );

				      }
				      else {
					/* 折行输出 */
					snprintf( pcontext -> expr,
						  MAX_EXPR_SIZE,
						  "%s(",
						  $1
						  );
					int len = strlen( pcontext -> expr);
					int indent = INDENT_UNIT;
					increase_indent( pcontext -> expr + len,
							 MAX_EXPR_SIZE - len,
							 "\n",
							 indent
							 );
					len = strlen( pcontext -> expr);
				        increase_indent( pcontext -> expr + len,
							 MAX_EXPR_SIZE - len,
							 $4,
							 indent
							 );
					len = strlen( pcontext -> expr);
					increase_indent( pcontext -> expr + len,
							 MAX_EXPR_SIZE - len,
							 "\n)",
							 indent
							 );
				      }
				  }
				  $$ = strdup ( pcontext -> expr );
                                }
                | LP { pcontext -> indent += 2; }
                  expr
                  RP
                                { /*  */
                                  pcontext -> indent -= 2;
                                  snprintf( pcontext -> expr,
					    MAX_EXPR_SIZE,
					    "( "
					    );
                                  increase_indent(
						  pcontext -> expr + 2,
                                                  MAX_EXPR_SIZE - 2,
                                                  $3,
                                                  2
                                                  );
                                  strncat( pcontext -> expr,
					   " )",
					   MAX_EXPR_SIZE
					   - strlen( pcontext -> expr )
					   - 4
					  );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr ADD { pcontext -> indent += 2; }
                  expr
                                { /*  */
                                  int len = strlen( $1 );
                                  pcontext -> indent -= 2;
                                  /* 判断下一个运算符是否同级 */
                                  if ( ( yychar == ADD )
                                       || ( yychar == MINUS )
                                       ){
                                       /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\n+ ",
                                              $1
                                              );
                                    increase_indent(
                                                    pcontext -> expr + len + 3,
                                                    MAX_EXPR_SIZE - len - 3,
                                                    $4,
                                                    2
                                                    );
                                  }
                                  else if ( pcontext -> indent
                                            + len
                                            + 3
                                            + strlen( $4 )
                                            < OUTPUT_WIDTH
                                            ){
                                    /* 不同级运算符，无需换行 */
                                    snprintf(
                                             pcontext -> expr,
                                             MAX_EXPR_SIZE,
                                             "%s + %s",
                                             merge_expr( $1 ),
                                             merge_expr( $4 )
                                             );
                                  }
                                  else {
                                    /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\n+ ",
                                              $1
                                              );
                                    increase_indent(
                                                    pcontext -> expr + len + 3,
                                                    MAX_EXPR_SIZE - len - 3,
                                                    $4,
                                                    2
                                                    );
                                  }
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr MINUS expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s - %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr MUL { pcontext -> indent += 2; }
                  expr
                                { /*  */
                                  int len = strlen( $1 );
                                  pcontext -> indent -= 2;
                                  /* 判断下一个运算符是否同级 */
                                  if ( ( yychar == MUL )
                                       || ( yychar == DIV )
                                       ){
                                       /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\n* ",
                                              $1
                                              );
                                    increase_indent(
                                                    pcontext -> expr + len + 3,
                                                    MAX_EXPR_SIZE - len - 3,
                                                    $4,
                                                    2
                                                    );
                                  }
                                  else if ( pcontext -> indent
                                            + len
                                            + 3
                                            + strlen( $4 )
                                            < OUTPUT_WIDTH
                                            ){
                                    /* 不同级运算符，无需换行 */
                                    snprintf(
                                             pcontext -> expr,
                                             MAX_EXPR_SIZE,
                                             "%s * %s",
                                             merge_expr( $1 ),
                                             merge_expr( $4 )
                                             );
                                  }
                                  else {
                                    /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\n* ",
                                              $1
                                              );
                                    increase_indent(
                                                    pcontext -> expr + len + 3,
                                                    MAX_EXPR_SIZE - len - 3,
                                                    $4,
                                                    2
                                                    );
                                  }
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr DIV expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s / %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | MINUS expr %prec NEG
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            " -%s ",
                                            $2
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr AND { pcontext -> indent += 4; }
                  expr
                                { /*  */
                                  int len = strlen( $1 );
                                  pcontext -> indent -= 4;
                                  /* 判断下一个运算符是否同级 */
                                  if ( ( yychar == AND )
                                       || ( yychar == OR )
                                       ){
                                       /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\nand ",
                                              $1
                                              );
				    len = strlen( pcontext -> expr );
                                    increase_indent(
                                                    pcontext -> expr + len,
                                                    MAX_EXPR_SIZE - len,
                                                    $4,
                                                    4
                                                    );
                                  }
                                  else if ( pcontext -> indent
                                            + len
                                            + 5
                                            + strlen( $4 )
                                            < OUTPUT_WIDTH
                                            ){
                                    /* 不同级运算符，无需换行 */
                                    snprintf(
                                             pcontext -> expr,
                                             MAX_EXPR_SIZE,
                                             "%s and %s",
                                             merge_expr( $1 ),
                                             merge_expr( $4 )
                                             );
                                  }
                                  else {
                                    /* 需要换行处理 */
                                    snprintf( pcontext -> expr,
                                              MAX_EXPR_SIZE,
                                              "%s\nand ",
                                              $1
                                              );
				    len = strlen( pcontext -> expr );
                                    increase_indent(
                                                    pcontext -> expr + len,
                                                    MAX_EXPR_SIZE - len,
                                                    $4,
                                                    4
                                                    );
                                  }
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr OR expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s or %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | NOT expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            " not %s",
                                            $2
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr EQ expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s = %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr NE expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s + %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr GT expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s > %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr LT expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s < %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr GE expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s >= %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                | expr LE expr
                                { /*  */
                                  snprintf (
                                            pcontext -> expr,
                                            MAX_EXPR_SIZE,
                                            "%s <= %s",
                                            $1,
                                            $3
                                            );
                                  $$ = strdup ( pcontext -> expr );
                                }
                ;

/* Error Recovery Rulers */

%%

/* Called by yyparse on error.  */
void
yyerror (void *locp, void *scanner, PCONTEXT pcontext, char const *msg)
{
  fprintf (stderr, "Line %d:%s\n", ((YYLTYPE*)locp)->first_line, msg);
  //fprintf (stderr, "Error:%s\n",  msg);
}

int
main (int argc, char * argv[])
{
  CONTEXT context;
  yyscan_t scanner;
  FILE * infile;
  int i,j;

  ++argv, --argc;  /* skip over program name */

  if ( ( argc > 0 ) && ( strcmp( argv [ 0 ], "-d" ) == 0 ) ) {
    yydebug = 1;
    ++argv, --argc;  /* skip over this option */
  }
  else
    yydebug = 0;

  if ( argc == 0 ) {
    fprintf ( stderr, "Error: no specify filename found\n" );
    return -1;
  }
  infile = fopen( argv[0], "r" );
  if ( infile == NULL ) {
    fprintf ( stderr, "Error: can't open file '%s'\n", argv [ 0 ] );
    return -2;
  }

  /* 初始化上下文 */
  memset( &context, 0, sizeof( CONTEXT ) );

  /* 初始化算法名称 */
  char * filename = strdup ( ( char * )basename ( argv [ 0 ] ) );
  if ( strchr ( filename, '.' ) )
    context.algorithm = ( char * )strtok ( filename, "." );
  else
    context.algorithm = filename;


  /* 重定向输出 */
  char * ofilename = malloc ( PATH_MAX );
  if ( snprintf ( ofilename, PATH_MAX, "%s.py", context.algorithm ) < 0 ) {
    printf ( "Error: memroy fault when get outfile name\n" );
    exit ( -1 );
  }

  FILE * outfile = fopen(ofilename, "w");
  if ( ! outfile ) {
    printf ( "Error: can't open output file '%s'\n", ofilename );
    return -3;
  }
  int fd = fileno ( outfile );
  if ( fd == -1 ) {
    fclose ( outfile );
    printf ( "Error: can't get file id of '%s'\n", ofilename );
    return -4;
  }
  int s_fd = dup( STDOUT_FILENO );
  if ( s_fd < 0 ) {
    close( fd );
    printf ( "Error: duplicate the stdout\n" );
    return -5;
  }
  int n_fd = dup2( fd, s_fd ); /* STDOUT_FILENO ); */
  if ( n_fd < 0) {
    close( fd );
    printf ( "Error: redirect the stdout\n" );
    return -6;
  }

  /* 解析文件 */
  print_file_header ( &context );
  yylex_init ( &scanner );
  yyset_in( infile, scanner );
  yyparse ( scanner, &context );
  yylex_destroy ( scanner );

  fclose ( infile );
  fclose ( outfile );
  free ( filename );
  free ( ofilename );
  return 0;
}
