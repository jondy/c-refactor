# C 预处理器

基于 gcc 源代码生成对 C 的预处理器的方法和步骤

首先下载 gcc-9.3.0.tar.gz ，解压之后执行下面的操作

    cd gcc-9.3.0
    ./configure --disable-multilib --disable-bootstrap --enable-languages=c,c++,objc
    make -j8

编译生成的结果文件在 `host-x86_64-apple-darwin18.7.0`

参考 `gcc/c/Make-lang.in` 在 `$HOST/gcc/Makefile` 中增加目标

```make
CCB_BACKEND = libbackend.a ccb-main.o libcommon-target.a libcommon.a \
	$(CPPLIB) $(LIBDECNUMBER)

ccb$(exeext): $(C_OBJS) cc1-checksum.o $(CCB_BACKEND) $(LIBDEPS)
	+$(LLINKER) $(ALL_LINKERFLAGS) $(LDFLAGS) -o $@ $(C_OBJS) \
	  cc1-checksum.o $(CCB_BACKEND) $(LIBS) $(BACKENDLIBS)

```

链接当前目录下面的文件到 `$HOST/gcc`

    ccb-main.c

然后编译生成新的解释器

    cd $HOST/gcc
    make ccb
    ./ccb --help

## 基于 GCC 的解释器

直接使用的代码包括：

    libiberty.a
    libcpp.a
    libcommon.a
        input.c
        hash-table.c
        gcc-none.c
            ggc.h
                gtype-desc.h
            hast-table.h
    tree.h
        tree-core.h
    tree.c (需要重写，只保留必要的代码)

langhooks 相关

    langhooks.h
    langhooks-def.h
    langhooks.c

    c/c-common-objc.h
    c/c-lang.h
    c/c-lang.c

    c-family/c-common.h
    c-family/c-common.c

cpp 相关

    c-family/c-opts.c
    c-family/c-lex.c

c 语法相关

    c/c-parser.c
    c/c-decl.c

相关依赖文件

    OBJS-libcommon
    C_COMMON_OBJS
    CB_EXTRA_OBJS:
        c/*.c
        objc/*.c (仅仅是编译需要，需要把 objc-lang.c 排除在外，否则会导致 lang_hooks 重定义）
        ggc-*.c
        tree.c
        input.c
        timevar.c


相关全局变量

    diagnostic_context *global_dc = &global_diagnostic_context; (diagnostic.c)
    type_hash_table; (tree.c)
    line_table; (input.c)
