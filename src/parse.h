
#if !defined(PARSE_H)

#define PARSE_H

#if defined(__cplusplus)
extern "C" {
#endif

  #define MAX_PARA_NUMBER 16
  #define MAX_FUNC_NUMBER 16
  #define MAX_VARS_NUMBER 16
  #define MAX_EXPR_SIZE 1024
  #define MAX_STMT_SIZE 1024

  typedef struct
  {
    char * name;
    char * type;
    int varflag;
  } PARA;
  typedef PARA * PPARA;

  typedef struct
  {
    char * name;
    char * rettype;
    PARA paras [ MAX_PARA_NUMBER ];
  } FUNC;
  typedef FUNC * PFUNC;

  typedef struct
  {
    char * name;
    char * type;
    char * value;
  } VARS;
  typedef VARS * PVAR;

  typedef struct
  {
    FUNC funcs [ MAX_FUNC_NUMBER ];
    int findex;		        /* 当前函数列表的指针 */
    int indent;			/* 缩进宽度 */
    char expr [ MAX_EXPR_SIZE ];
    VARS vars [ MAX_VARS_NUMBER ];
    char stmt [ MAX_STMT_SIZE ];
    char * filename;
  } CONTEXT;
  typedef CONTEXT * PCONTEXT;
  /*
  void print_file_header ( PCONTEXT );
  void print_file_footer ( PCONTEXT );
  void print_class_begin ( PCONTEXT );
  void print_class_end ( PCONTEXT );
  void print_func_begin ( int, PCONTEXT );
  void print_func_end ( int, PCONTEXT );
  void print_indent ( int );

  void print_control_stmt ( int, PCONTEXT );
  void print_call_stmt ( int, PCONTEXT );
  void print_stmt ( int, PCONTEXT );

  char * increase_indent( char *, const int, const char *, int );
  char * merge_expr( char * );
  */
  /* 缩进单位，四个空格 */
  extern int INDENT_UNIT;
  extern int OUTPUT_WIDTH;

#if defined(__cplusplus)
}
#endif

#endif	/* PARSE_H */
