#if !defined(PTYPES_H)

#define PTYPES_H

#if !defined(PATH_MAX)
  #define PATH_MAX 4096
#endif

  typedef void * yyscan_t;

  #define YYSTYPE YYSTYPE
  typedef char * YYSTYPE;

  #define YYLTYPE YYLTYPE
  typedef struct
  {
    int first_line;
    int first_column;
    int last_line;
    int last_column;
    char *filename;
  } YYLTYPE;

  #define MAX_STACK_SIZE 32
  #define MAX_INCLUDE_PATH 32
  #define MAX_PARAS 32

  typedef struct {
    char * standard[MAX_INCLUDE_PATH];
    char * extra[MAX_INCLUDE_PATH];
  } include_path_t, *include_path_p;

  typedef struct {
    int current;
    int top;
    struct include_stack_t *prev;
    YYLTYPE data[MAX_STACK_SIZE];
  } include_stack_t, * include_stack_p;

  typedef struct {
    char * name;
    void * cls;
    int pointer;
    int array;
  } var_t, * var_p ;

  typedef struct {
    char * name;
  } enum_t, * enum_p ;

  typedef struct {
    char * name;
  } struct_t, * struct_p ;

  typedef struct {
    char * name;
    var_t ret;
    var_t paras[MAX_PARAS];
  } function_t, * function_p ;

  typedef struct {
    char * name;
    char * value;
  } macro_t, * macro_p ;

  typedef struct {
    int current;
    int top;
    struct macro_stack_t * prev;
    macro_t data[MAX_STACK_SIZE];
  } macro_stack_t, * macro_stack_p;

  typedef void *(*f_getter)(const char *text);
  typedef struct {
    char * name;
    int pointer;
    char * clsname;
  } typedef_t, * typedef_p ;

  typedef struct {
    int current;
    int top;
    struct typedef_stack_t * prev;
    typedef_t data[MAX_STACK_SIZE];
  } typedef_stack_t, * typedef_stack_p;

  typedef struct {
    char * name;
    char * type;
    int varflag;
  } PARA, * PPARA;

  typedef struct {
    char * name;
    char * rettype;
    PARA paras [ MAX_STACK_SIZE ];
  } FUNC, * PFUNC;

  typedef struct {
    char * name;
    char * type;
    char * value;
  } VARS, * PVAR;

  #define TYPEDEF_FLAG 0x1

  typedef struct {
    include_path_t include_path;
    typedef_stack_t typedef_stack;
    include_stack_t include_stack;
    macro_stack_t macro_stack;
    int flags;
    char * typename;
    char * filename;
  } CONTEXT, *PCONTEXT, *YY_EXTRA_TYPE;

#define DUMP_TYPEDEF_STACK(stack) do {                                  \
    fprintf(stderr, "Dump typedef stack:\n");                           \
    for (int i=0; i<stack.current; i ++)                                \
      fprintf(stderr, "%d:\n name is %s\n pointer is %d\n class is %s\n", \
              i, stack.data[i].name, stack.data[i].pointer, stack.data[i].clsname); \
  } while (0)

#define INC_TYPEDEF_POINTER if (pcontext->flags & TYPEDEF_FLAG)         \
    pcontext->typedef_stack.data[pcontext->typedef_stack.current].pointer ++;

#define PUSH_TYPEDEF(value)                                             \
  pcontext->typedef_stack.data[pcontext->typedef_stack.current].name = strdup(value); \
  pcontext->typedef_stack.data[pcontext->typedef_stack.current].clsname = pcontext->typename; \
  pcontext->typedef_stack.current ++;

#define PUSH_INCLUDE_STACK do {                                         \
    char buf[PATH_MAX];                                                 \
    memcpy(buf, yytext + 1, yyleng - 2);                                 \
    buf[yyleng - 2] = 0;                                                 \
    FILE *file = fopen(buf, "r");                                       \
    if ( !file )                                                        \
      fprintf(stderr, "Open '%s' failed: %s\n", buf, strerror(errno));  \
    else {                                                              \
      memcpy(&(pcontext->include_stack.data[pcontext->include_stack.current]), yylloc, sizeof(YYLTYPE)); \
      pcontext->include_stack.current ++;                               \
      yylloc->filename = strdup(buf);                                   \
      yylloc->first_line = 1;                                           \
      yylloc->last_line = 1;                                            \
      yylloc->first_column = 1;                                         \
      yylloc->last_column = 1;                                          \
      yypush_buffer_state(yy_create_buffer(file, YY_BUF_SIZE, yyscanner), yyscanner); \
    }                                                                   \
  } while (0)

#define POP_INCLUDE_STACK do {                                          \
    pcontext->include_stack.current --;                                 \
    memcpy(yylloc, &(pcontext->include_stack.data[pcontext->include_stack.current]), sizeof(YYLTYPE)); \
  } while (0)



#endif	/* PTYPES_H */
