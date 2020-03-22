# C 预处理器

基于 gcc 源代码生成对 C 的预处理器的方法和步骤

首先下载 gcc-9.3.0.tar.gz ，解压之后执行下面的操作

    cd gcc-9.3.0

    ./configure --enable-languages=c,objc
    make -j4

在 `gcc/Makefile` 中增加目标

```make
libcbb-backend.a: $(C_OBJS) $(OBJS) $(C_COMMON_OBJS) \
                  libbackend.a libcommon.a libcommon-target.a \
                  $(EXTRA_GCC_OBJS)

cbb$(exeext): cbb-main.o cbb.o cb-pixie.o cb-macro.o libcbb-backend.a
    +$(LINKER) $(ALL_LINKERFLAGS) $(LDFLAGS) -o $@ $*

# CBB_LIBS = $(EXTRA_GCC_LIBS) $(LIBS) ../zlib/libz.a -lmpc -lgmp -lmpfr -ldl
```

链接当前目录下面的文件到 `gcc`

    cbb-main.c
    cbb.c
    cb-pixie.c
    cb-macro.c

相关文件

* tree 相关
* input.c
* langhooks 相关
* c-family/c-xx.c
* c/c-xx.c

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
