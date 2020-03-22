#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "langhooks.h"
#include "options.h"
#include "c-family/c-common.h"
#include "target.h"

#include "c-family/c-pragma.h"
#include "../libcpp/internal.h"

// struct gcc_target targetm = TARGET_INITIALIZER;
// Required by init_attributes()
//     targetm.attribute_table in "config/i386/i386.c"
//     static const struct attribute_spec ix86_attribute_table

// struct lang_hooks lang_hooks;
// Required by make_node()
//     lang_hooks.identifier_size
//
#define C_SIZEOF_STRUCT_LANG_IDENTIFIER \
  (sizeof (struct c_common_identifier) + 3 * sizeof (void *))
#define LANG_HOOKS_IDENTIFIER_SIZE C_SIZEOF_STRUCT_LANG_IDENTIFIER

// Required by init_attributes()
//     lang_hooks.common_attribute_table
//     lang_hooks.attribute_table
//     lang_hooks.format_attribute_table
//
//     "c-family/c-attribs.c":
//     const struct attribute_spec c_common_attribute_table[]
//     const struct attribute_spec c_common_format_attribute_table[]
//
//     "cp/cp-objcp-common.h":
//     #define LANG_HOOKS_ATTRIBUTE_TABLE cxx_attribute_table
//

enum c_language_kind c_language = clk_c; /* Copied from c/c-lang.c */
cpp_reader *parse_in;		/* Declared in c-pragma.h.  */

/////////////////////////////////////////////////
//
// Level 1
//
// c-family/c-ppoutput.c
//
/////////////////////////////////////////////////
extern void preprocess_file (cpp_reader *pfile) ;

/////////////////////////////////////////////////
//
// Level 1
//
// c-family/c-common.c
//
/////////////////////////////////////////////////
char flag_dump_macros;
char flag_dump_includes;
char flag_no_line_commands;
char flag_no_output;
bool flag_pch_preprocess;

time_t
cb_get_source_date_epoch (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  char *source_date_epoch;
  int64_t epoch;
  char *endptr;

  source_date_epoch = getenv ("SOURCE_DATE_EPOCH");
  if (!source_date_epoch)
    return (time_t) -1;

  errno = 0;
#if defined(INT64_T_IS_LONG)
  epoch = strtol (source_date_epoch, &endptr, 10);
#else
  epoch = strtoll (source_date_epoch, &endptr, 10);
#endif
  if (errno != 0 || endptr == source_date_epoch || *endptr != '\0'
      || epoch < 0 || epoch > MAX_SOURCE_DATE_EPOCH)
    {
      error_at (input_location, "environment variable SOURCE_DATE_EPOCH must "
	        "expand to a non-negative integer less than or equal to %wd",
		MAX_SOURCE_DATE_EPOCH);
      return (time_t) -1;
    }

  return (time_t) epoch;
}

/////////////////////////////////////////////////
//
// Level 3
//
// Required by alloc_node in stringpool.c
//
// tree.c
//
/////////////////////////////////////////////////

/* 简化版本，仅仅实现了创建 IDENTIFIER_NODE */
tree
make_node (enum tree_code code MEM_STAT_DECL)
{
  tree t;
  size_t length = C_SIZEOF_STRUCT_LANG_IDENTIFIER; // lang_hooks.identifier_size;
  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);
  TREE_SET_CODE (t, code);
  return t;
}

/////////////////////////////////////////////////
//
// Level 2: get_identifier, get_identifier_with_length
//
// Required by c_common_has_attribute
//
// stringpool.c
//
/////////////////////////////////////////////////

struct ht *ident_hash;
static hashnode alloc_node (cpp_hash_table *);

static void *
stringpool_ggc_alloc (size_t x)
{
  return ggc_alloc_atomic (x);
}

/* Initialize the string pool.  */
void
init_stringpool (void)
{
  /* Clean up if we're called more than once.
     (We can't make this idempotent since identifiers contain state) */
  if (ident_hash)
    ht_destroy (ident_hash);

  /* Create with 16K (2^14) entries.  */
  ident_hash = ht_create (14);
  ident_hash->alloc_node = alloc_node;
  ident_hash->alloc_subobject = stringpool_ggc_alloc;
}

/* Allocate a hash node.  */
static hashnode
alloc_node (cpp_hash_table *table ATTRIBUTE_UNUSED)
{
  return GCC_IDENT_TO_HT_IDENT (make_node (IDENTIFIER_NODE));
}

/* Allocate and return a string constant of length LENGTH, containing
   CONTENTS.  If LENGTH is -1, CONTENTS is assumed to be a
   nul-terminated string, and the length is calculated using strlen.  */

const char *
ggc_alloc_string (const char *contents, int length MEM_STAT_DECL)
{
  if (length == -1)
    length = strlen (contents);

  if (!length)
    return "";

  char *result = (char *) ggc_alloc_atomic (length + 1);
  memcpy (result, contents, length);
  result[length] = '\0';

  return (const char *) result;
}

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

#undef get_identifier

tree
get_identifier (const char *text)
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				strlen (text), HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/* Identical to get_identifier, except that the length is assumed
   known.  */

tree
get_identifier_with_length (const char *text, size_t length)
{
  hashnode ht_node = ht_lookup (ident_hash,
				(const unsigned char *) text,
				length, HT_ALLOC);

  /* ht_node can't be NULL here.  */
  return HT_IDENT_TO_GCC_IDENT (ht_node);
}

/////////////////////////////////////////////////
//
// Level 2 required by c_common_has_attribute
//
// attribits.c
//
// 暂时忽略所以 PLUGIN_ATTRIBUTES ，
// 注释了对 invoke_plugin_callbacks 的调用
//
/////////////////////////////////////////////////

/* Table of the tables of attributes (common, language, format, machine)
   searched.  */
static const struct attribute_spec *attribute_tables[4];

/* Substring representation.  */

struct substring
{
  const char *str;
  int length;
};

/* Simple hash function to avoid need to scan whole string.  */

static inline hashval_t
substring_hash (const char *str, int l)
{
  return str[0] + str[l - 1] * 256 + l * 65536;
}

/* Used for attribute_hash.  */

struct attribute_hasher : nofree_ptr_hash <attribute_spec>
{
  typedef substring *compare_type;
  static inline hashval_t hash (const attribute_spec *);
  static inline bool equal (const attribute_spec *, const substring *);
};

inline hashval_t
attribute_hasher::hash (const attribute_spec *spec)
{
  const int l = strlen (spec->name);
  return substring_hash (spec->name, l);
}

inline bool
attribute_hasher::equal (const attribute_spec *spec, const substring *str)
{
  return (strncmp (spec->name, str->str, str->length) == 0
	  && !spec->name[str->length]);
}

/* Scoped attribute name representation.  */

struct scoped_attributes
{
  const char *ns;
  vec<attribute_spec> attributes;
  hash_table<attribute_hasher> *attribute_hash;
};

/* The table of scope attributes.  */
static vec<scoped_attributes> attributes_table;

static scoped_attributes* find_attribute_namespace (const char*);
static void register_scoped_attribute (const struct attribute_spec *,
				       scoped_attributes *);

static bool attributes_initialized = false;

/* Default empty table of attributes.  */

static const struct attribute_spec empty_attribute_table[] =
{
  { NULL, 0, 0, false, false, false, false, NULL, NULL }
};

/* Return base name of the attribute.  Ie '__attr__' is turned into 'attr'.
   To avoid need for copying, we simply return length of the string.  */

static void
extract_attribute_substring (struct substring *str)
{
  if (str->length > 4 && str->str[0] == '_' && str->str[1] == '_'
      && str->str[str->length - 1] == '_' && str->str[str->length - 2] == '_')
    {
      str->length -= 4;
      str->str += 2;
    }
}

static void
register_scoped_attribute (const struct attribute_spec *attr,
			   scoped_attributes *name_space)
{
  struct substring str;
  attribute_spec **slot;

  gcc_assert (attr != NULL && name_space != NULL);

  gcc_assert (name_space->attribute_hash);

  str.str = attr->name;
  str.length = strlen (str.str);

  /* Attribute names in the table must be in the form 'text' and not
     in the form '__text__'.  */
  gcc_assert (str.length > 0 && str.str[0] != '_');

  slot = name_space->attribute_hash
	 ->find_slot_with_hash (&str, substring_hash (str.str, str.length),
				INSERT);
  gcc_assert (!*slot || attr->name[0] == '*');
  *slot = CONST_CAST (struct attribute_spec *, attr);
}

scoped_attributes *
register_scoped_attributes (const struct attribute_spec *attributes,
			    const char *ns)
{
  scoped_attributes *result = NULL;

  /* See if we already have attributes in the namespace NS.  */
  result = find_attribute_namespace (ns);

  if (result == NULL)
    {
      /* We don't have any namespace NS yet.  Create one.  */
      scoped_attributes sa;

      if (attributes_table.is_empty ())
	attributes_table.create (64);

      memset (&sa, 0, sizeof (sa));
      sa.ns = ns;
      sa.attributes.create (64);
      result = attributes_table.safe_push (sa);
      result->attribute_hash = new hash_table<attribute_hasher> (200);
    }

  /* Really add the attributes to their namespace now.  */
  for (unsigned i = 0; attributes[i].name != NULL; ++i)
    {
      result->attributes.safe_push (attributes[i]);
      register_scoped_attribute (&attributes[i], result);
    }

  gcc_assert (result != NULL);

  return result;
}

static scoped_attributes*
find_attribute_namespace (const char* ns)
{
  unsigned ix;
  scoped_attributes *iter;

  FOR_EACH_VEC_ELT (attributes_table, ix, iter)
    if (ns == iter->ns
	|| (iter->ns != NULL
	    && ns != NULL
	    && !strcmp (iter->ns, ns)))
      return iter;
  return NULL;
}

/* Make some sanity checks on the attribute tables.  */

static void
check_attribute_tables (void)
{
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = 0; attribute_tables[i][j].name != NULL; j++)
      {
	/* The name must not begin and end with __.  */
	const char *name = attribute_tables[i][j].name;
	int len = strlen (name);

	gcc_assert (!(name[0] == '_' && name[1] == '_'
		      && name[len - 1] == '_' && name[len - 2] == '_'));

	/* The minimum and maximum lengths must be consistent.  */
	gcc_assert (attribute_tables[i][j].min_length >= 0);

	gcc_assert (attribute_tables[i][j].max_length == -1
		    || (attribute_tables[i][j].max_length
			>= attribute_tables[i][j].min_length));

	/* An attribute cannot require both a DECL and a TYPE.  */
	gcc_assert (!attribute_tables[i][j].decl_required
		    || !attribute_tables[i][j].type_required);

	  /* If an attribute requires a function type, in particular
	     it requires a type.  */
	gcc_assert (!attribute_tables[i][j].function_type_required
		    || attribute_tables[i][j].type_required);
      }

  /* Check that each name occurs just once in each table.  */
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = 0; attribute_tables[i][j].name != NULL; j++)
      for (size_t k = j + 1; attribute_tables[i][k].name != NULL; k++)
	gcc_assert (strcmp (attribute_tables[i][j].name,
			    attribute_tables[i][k].name));

  /* Check that no name occurs in more than one table.  Names that
     begin with '*' are exempt, and may be overridden.  */
  for (size_t i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    for (size_t j = i + 1; j < ARRAY_SIZE (attribute_tables); j++)
      for (size_t k = 0; attribute_tables[i][k].name != NULL; k++)
	for (size_t l = 0; attribute_tables[j][l].name != NULL; l++)
	  gcc_assert (attribute_tables[i][k].name[0] == '*'
		      || strcmp (attribute_tables[i][k].name,
				 attribute_tables[j][l].name));
}

void
init_attributes (void)
{
  size_t i;

  if (attributes_initialized)
    return;

  attribute_tables[0] = NULL; // lang_hooks.common_attribute_table;
  attribute_tables[1] = NULL; // lang_hooks.attribute_table;
  attribute_tables[2] = NULL; // lang_hooks.format_attribute_table;
  attribute_tables[3] = NULL; // targetm.attribute_table;

  /* Translate NULL pointers to pointers to the empty table.  */
  for (i = 0; i < ARRAY_SIZE (attribute_tables); i++)
    if (attribute_tables[i] == NULL)
      attribute_tables[i] = empty_attribute_table;

  if (flag_checking)
    check_attribute_tables ();

  for (i = 0; i < ARRAY_SIZE (attribute_tables); ++i)
    /* Put all the GNU attributes into the "gnu" namespace.  */
    register_scoped_attributes (attribute_tables[i], "gnu");

  // invoke_plugin_callbacks (PLUGIN_ATTRIBUTES, NULL);
  attributes_initialized = true;
}

static const struct attribute_spec *
lookup_scoped_attribute_spec (const_tree ns, const_tree name)
{
  struct substring attr;
  scoped_attributes *attrs;

  const char *ns_str = (ns != NULL_TREE) ? IDENTIFIER_POINTER (ns): NULL;

  attrs = find_attribute_namespace (ns_str);

  if (attrs == NULL)
    return NULL;

  attr.str = IDENTIFIER_POINTER (name);
  attr.length = IDENTIFIER_LENGTH (name);
  extract_attribute_substring (&attr);
  return attrs->attribute_hash->find_with_hash (&attr,
						substring_hash (attr.str,
							       	attr.length));
}

/* Return the spec for the attribute named NAME.  If NAME is a TREE_LIST,
   it also specifies the attribute namespace.  */

const struct attribute_spec *
lookup_attribute_spec (const_tree name)
{
  tree ns;
  if (TREE_CODE (name) == TREE_LIST)
    {
      ns = TREE_PURPOSE (name);
      name = TREE_VALUE (name);
    }
  else
    ns = get_identifier ("gnu");
  return lookup_scoped_attribute_spec (ns, name);
}

/////////////////////////////////////////////////
//
// Level 1: c_common_has_attribute
//
// c-family/c-lex.c
//
/////////////////////////////////////////////////
static const cpp_token *
get_token_no_padding (cpp_reader *pfile)
{
  for (;;)
    {
      const cpp_token *ret = cpp_peek_token (pfile, 0);
      if (ret->type == CPP_EOF)
	return ret;
      ret = cpp_get_token (pfile);
      if (ret->type != CPP_PADDING)
	return ret;
    }
}

int
c_common_has_attribute (cpp_reader *pfile)
{
  int result = 0;
  tree attr_name = NULL_TREE;
  const cpp_token *token;

  token = get_token_no_padding (pfile);
  if (token->type != CPP_OPEN_PAREN)
    {
      cpp_error (pfile, CPP_DL_ERROR,
		 "missing '(' after \"__has_attribute\"");
      return 0;
    }
  token = get_token_no_padding (pfile);
  if (token->type == CPP_NAME)
    {
      attr_name = get_identifier ((const char *)
				  cpp_token_as_text (pfile, token));
      attr_name = canonicalize_attr_name (attr_name);
      if (c_dialect_cxx ())
	{
          // 暂时不支持 C++ 的 attribute ，所以把这里的代码删除了
          cpp_error (pfile, CPP_DL_ERROR,
                     "unsuportted cxx attribute identifier");
          return 0;
	}
      if (attr_name)
	{
	  init_attributes ();
	  const struct attribute_spec *attr = lookup_attribute_spec (attr_name);
	  if (attr)
	    result = 1;
	}
    }
  else
    {
      cpp_error (pfile, CPP_DL_ERROR,
		 "macro \"__has_attribute\" requires an identifier");
      return 0;
    }

  if (get_token_no_padding (pfile)->type != CPP_CLOSE_PAREN)
    cpp_error (pfile, CPP_DL_ERROR,
	       "missing ')' after \"__has_attribute\"");

  return result;
}

/////////////////////////////////////////////////
//
// Level 1: c_common_read_pch, c_common_valid_pch
//
// c-family/c-pch.c
//
// 暂时不实现
//
/////////////////////////////////////////////////
void
c_common_read_pch (cpp_reader *pfile, const char *name,
		   int fd, const char *orig_name ATTRIBUTE_UNUSED)
{
}

int
c_common_valid_pch (cpp_reader *pfile, const char *name, int fd)
{
  return 0;
}

/////////////////////////////////////////////////
//
// Level 1: c_pp_lookup_pragma
//
// c-family/c-pragma.c
//
/////////////////////////////////////////////////
static vec<internal_pragma_handler> registered_pragmas;

struct pragma_ns_name
{
  const char *space;
  const char *name;
};


static vec<pragma_ns_name> registered_pp_pragmas;

struct omp_pragma_def { const char *name; unsigned int id; };
static const struct omp_pragma_def oacc_pragmas[] = {
  { "atomic", PRAGMA_OACC_ATOMIC },
  { "cache", PRAGMA_OACC_CACHE },
  { "data", PRAGMA_OACC_DATA },
  { "declare", PRAGMA_OACC_DECLARE },
  { "enter", PRAGMA_OACC_ENTER_DATA },
  { "exit", PRAGMA_OACC_EXIT_DATA },
  { "host_data", PRAGMA_OACC_HOST_DATA },
  { "kernels", PRAGMA_OACC_KERNELS },
  { "loop", PRAGMA_OACC_LOOP },
  { "parallel", PRAGMA_OACC_PARALLEL },
  { "routine", PRAGMA_OACC_ROUTINE },
  { "update", PRAGMA_OACC_UPDATE },
  { "wait", PRAGMA_OACC_WAIT }
};
static const struct omp_pragma_def omp_pragmas[] = {
  { "atomic", PRAGMA_OMP_ATOMIC },
  { "barrier", PRAGMA_OMP_BARRIER },
  { "cancel", PRAGMA_OMP_CANCEL },
  { "cancellation", PRAGMA_OMP_CANCELLATION_POINT },
  { "critical", PRAGMA_OMP_CRITICAL },
  { "depobj", PRAGMA_OMP_DEPOBJ },
  { "end", PRAGMA_OMP_END_DECLARE_TARGET },
  { "flush", PRAGMA_OMP_FLUSH },
  { "master", PRAGMA_OMP_MASTER },
  { "requires", PRAGMA_OMP_REQUIRES },
  { "section", PRAGMA_OMP_SECTION },
  { "sections", PRAGMA_OMP_SECTIONS },
  { "single", PRAGMA_OMP_SINGLE },
  { "task", PRAGMA_OMP_TASK },
  { "taskgroup", PRAGMA_OMP_TASKGROUP },
  { "taskwait", PRAGMA_OMP_TASKWAIT },
  { "taskyield", PRAGMA_OMP_TASKYIELD },
  { "threadprivate", PRAGMA_OMP_THREADPRIVATE }
};
static const struct omp_pragma_def omp_pragmas_simd[] = {
  { "declare", PRAGMA_OMP_DECLARE },
  { "distribute", PRAGMA_OMP_DISTRIBUTE },
  { "for", PRAGMA_OMP_FOR },
  { "ordered", PRAGMA_OMP_ORDERED },
  { "parallel", PRAGMA_OMP_PARALLEL },
  { "simd", PRAGMA_OMP_SIMD },
  { "target", PRAGMA_OMP_TARGET },
  { "taskloop", PRAGMA_OMP_TASKLOOP },
  { "teams", PRAGMA_OMP_TEAMS },
};

void
c_pp_lookup_pragma (unsigned int id, const char **space, const char **name)
{
  const int n_oacc_pragmas = sizeof (oacc_pragmas) / sizeof (*oacc_pragmas);
  const int n_omp_pragmas = sizeof (omp_pragmas) / sizeof (*omp_pragmas);
  const int n_omp_pragmas_simd = sizeof (omp_pragmas_simd)
				 / sizeof (*omp_pragmas);
  int i;

  for (i = 0; i < n_oacc_pragmas; ++i)
    if (oacc_pragmas[i].id == id)
      {
	*space = "acc";
	*name = oacc_pragmas[i].name;
	return;
      }

  for (i = 0; i < n_omp_pragmas; ++i)
    if (omp_pragmas[i].id == id)
      {
	*space = "omp";
	*name = omp_pragmas[i].name;
	return;
      }

  for (i = 0; i < n_omp_pragmas_simd; ++i)
    if (omp_pragmas_simd[i].id == id)
      {
	*space = "omp";
	*name = omp_pragmas_simd[i].name;
	return;
      }

  if (id >= PRAGMA_FIRST_EXTERNAL
      && (id < PRAGMA_FIRST_EXTERNAL + registered_pp_pragmas.length ()))
    {
      *space = registered_pp_pragmas[id - PRAGMA_FIRST_EXTERNAL].space;
      *name = registered_pp_pragmas[id - PRAGMA_FIRST_EXTERNAL].name;
      return;
    }

  gcc_unreachable ();
}
