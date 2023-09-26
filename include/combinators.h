#ifndef _COMBINATORS_H_
#define _COMBINATORS_H_

/* maybe this is a good place to eventually put some documentation on the nodes */
enum node_tag { T_FREE, T_IND, T_AP, T_INT, T_DOUBLE, T_HDL, T_S, T_K, T_I, T_B, T_C,
                T_A, T_Y, T_SS, T_BB, T_CC, T_P, T_O, T_T, T_BK, T_ADD, T_SUB, T_MUL,
                T_QUOT, T_REM, T_SUBR, T_UQUOT, T_UREM,
                T_FADD, T_FSUB, T_FMUL, T_FDIV,
                T_FEQ, T_FNE, T_FLT, T_FLE, T_FGT, T_FGE, T_FSHOW, T_FREAD,
                T_EQ, T_NE, T_LT, T_LE, T_GT, T_GE, T_ULT, T_ULE, T_UGT, T_UGE,
                T_ERROR, T_SEQ, T_EQUAL, T_COMPARE, T_RNF,
                T_IO_BIND, T_IO_THEN, T_IO_RETURN, T_IO_GETCHAR, T_IO_PUTCHAR,
                T_IO_SERIALIZE, T_IO_DESERIALIZE, T_IO_OPEN, T_IO_CLOSE, T_IO_ISNULLHANDLE,
                T_IO_STDIN, T_IO_STDOUT, T_IO_STDERR, T_IO_GETARGS, T_IO_DROPARGS,
                T_IO_PERFORMIO,
                T_IO_GETTIMEMILLI, T_IO_PRINT, T_IO_CATCH,
                T_IO_CCALL, T_IO_GETRAW, T_IO_FLUSH,
                T_STR,
                T_ISINT, T_ISIO,
                T_LAST_TAG,
};

#endif // _COMBINATORS_H_