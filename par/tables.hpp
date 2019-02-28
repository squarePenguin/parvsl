#define SETUPSPEC                                               \
    SETUP_TABLE_SELECT("quote",             Lquote),            \
    SETUP_TABLE_SELECT("cond",              Lcond),             \
    SETUP_TABLE_SELECT("and",               Land),              \
    SETUP_TABLE_SELECT("or",                Lor),               \
    SETUP_TABLE_SELECT("setq",              Lsetq),             \
    SETUP_TABLE_SELECT("progn",             Lprogn),            \
    SETUP_TABLE_SELECT("go",                Lgo),

#define SETUPSPECa                                              \
    SETUP_TABLE_SELECT("de",                Lde),               \
    SETUP_TABLE_SELECT("df",                Ldf),               \
    SETUP_TABLE_SELECT("dm",                Ldm),               \
    SETUP_TABLE_SELECT("prog",              Lprog),

#define SETUP0                                                  \
    SETUP_TABLE_SELECT("condvar",           Lcondvar),          \
    SETUP_TABLE_SELECT("date",              Ldate),             \
    SETUP_TABLE_SELECT("list",              Llist_0),           \
    SETUP_TABLE_SELECT("iplus",             Lplus_0),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_0),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_0),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_0),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_0),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_0),       \
    SETUP_TABLE_SELECT("error",             Lerror_0),          \
    SETUP_TABLE_SELECT("gensym",            Lgensym_0),         \
    SETUP_TABLE_SELECT("get-lisp-directory",Lget_lisp_directory), \
    SETUP_TABLE_SELECT("hardwarethreads",   Lhardware_threads), \
    SETUP_TABLE_SELECT("mutex",             Lmutex),            \
    SETUP_TABLE_SELECT("oblist",            Loblist),           \
    SETUP_TABLE_SELECT("posn",              Lposn),             \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_0),       \
    SETUP_TABLE_SELECT("read",              Lread),             \
    SETUP_TABLE_SELECT("readch",            Lreadch),           \
    SETUP_TABLE_SELECT("readline",          Lreadline),         \
    SETUP_TABLE_SELECT("reclaim",           Lreclaim_0),        \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_0),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_0),   \
    SETUP_TABLE_SELECT("return",            Lreturn_0),         \
    SETUP_TABLE_SELECT("stop",              Lstop_0),           \
    SETUP_TABLE_SELECT("terpri",            Lterpri),           \
    SETUP_TABLE_SELECT("thread_id",         Lthread_id),        \
    SETUP_TABLE_SELECT("time",              Ltime),             \
    SETUP_TABLE_SELECT("vector",            Lvector_0),

#define SETUP0a

#define SETUP1                                                  \
    SETUP_TABLE_SELECT("list",              Llist_1),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_1),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_1),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_1),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_1),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_1),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_1),         \
    SETUP_TABLE_SELECT("allocate-string",   Lallocate_string),  \
    SETUP_TABLE_SELECT("atan",              Latan),             \
    SETUP_TABLE_SELECT("atom",              Latom),             \
    SETUP_TABLE_SELECT("bignump",           Lbignump),          \
    SETUP_TABLE_SELECT("boundp",            Lboundp),           \
    SETUP_TABLE_SELECT("car",               Lcar),              \
    SETUP_TABLE_SELECT("cdr",               Lcdr),              \
    SETUP_TABLE_SELECT("char-code",         Lcharcode),         \
    SETUP_TABLE_SELECT("char-downcase",     Lchar_downcase),    \
    SETUP_TABLE_SELECT("char-upcase",       Lchar_upcase),      \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_1),       \
    SETUP_TABLE_SELECT("close",             Lclose),            \
    SETUP_TABLE_SELECT("code-char",         Lcodechar),         \
    SETUP_TABLE_SELECT("compress",          Lcompress),         \
    SETUP_TABLE_SELECT("condvar_notify_all", Lcondvar_notify_all), \
    SETUP_TABLE_SELECT("condvar_notify_one", Lcondvar_notify_one), \
    SETUP_TABLE_SELECT("cos",               Lcos),              \
    SETUP_TABLE_SELECT("error",             Lerror_1),          \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_1),       \
    SETUP_TABLE_SELECT("eval",              Leval),             \
    SETUP_TABLE_SELECT("exp",               Lexp),              \
    SETUP_TABLE_SELECT("explode",           Lexplode),          \
    SETUP_TABLE_SELECT("explodec",          Lexplodec),         \
    SETUP_TABLE_SELECT("exploden",          Lexploden),         \
    SETUP_TABLE_SELECT("explodecn",         Lexplodecn),        \
    SETUP_TABLE_SELECT("filep",             Lfilep),            \
    SETUP_TABLE_SELECT("float-denormalized-p", Lfp_subnorm),    \
    SETUP_TABLE_SELECT("float-infinity-p",  Lfp_infinite),      \
    SETUP_TABLE_SELECT("fluid",             Lfluid),            \
    SETUP_TABLE_SELECT("fluidp",            Lfluidp),           \
    SETUP_TABLE_SELECT("fp-infinite",       Lfp_infinite),      \
    SETUP_TABLE_SELECT("fp-nan",            Lfp_nan),           \
    SETUP_TABLE_SELECT("fp-finite",         Lfp_finite),        \
    SETUP_TABLE_SELECT("fp-subnorm",        Lfp_subnorm),       \
    SETUP_TABLE_SELECT("fp-signbit",        Lfp_signbit),       \
    SETUP_TABLE_SELECT("iadd1",             Ladd1),             \
    SETUP_TABLE_SELECT("iceiling",          Lceiling),          \
    SETUP_TABLE_SELECT("ifix",              Lfix),              \
    SETUP_TABLE_SELECT("ifixp",             Lfixp),             \
    SETUP_TABLE_SELECT("ifloat",            Lfloat),            \
    SETUP_TABLE_SELECT("ilognot",           Llognot),           \
    SETUP_TABLE_SELECT("iminus",            Lminus),            \
    SETUP_TABLE_SELECT("iminusp",           Lminusp),           \
    SETUP_TABLE_SELECT("inumberp",          Lnumberp),          \
    SETUP_TABLE_SELECT("isub1",             Lsub1),             \
    SETUP_TABLE_SELECT("jointhread",        Ljoin_thread),      \
    SETUP_TABLE_SELECT("floatp",            Lfloatp),           \
    SETUP_TABLE_SELECT("ifloor",            Lfloor),            \
    SETUP_TABLE_SELECT("gensym",            Lgensym_1),         \
    SETUP_TABLE_SELECT("getd",              Lgetd),             \
    SETUP_TABLE_SELECT("global",            Lglobal),           \
    SETUP_TABLE_SELECT("globalp",           Lglobalp),          \
    SETUP_TABLE_SELECT("length",            Llength),           \
    SETUP_TABLE_SELECT("list2string",       Llist2string),      \
    SETUP_TABLE_SELECT("load-module",       Lload_module),      \
    SETUP_TABLE_SELECT("log",               Llog),              \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_1),         \
    SETUP_TABLE_SELECT("mkvect",            Lmkvect),           \
    SETUP_TABLE_SELECT("mutexlock",         Lmutex_lock),       \
    SETUP_TABLE_SELECT("mutexunlock",       Lmutex_unlock),     \
    SETUP_TABLE_SELECT("null",              Lnull),             \
    SETUP_TABLE_SELECT("onep",              Lonep),             \
    SETUP_TABLE_SELECT("plist",             Lplist),            \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_1),       \
    SETUP_TABLE_SELECT("prin",              Lprin),             \
    SETUP_TABLE_SELECT("princ",             Lprinc),            \
    SETUP_TABLE_SELECT("prin1",             Lprin),             \
    SETUP_TABLE_SELECT("prin2",             Lprinc),            \
    SETUP_TABLE_SELECT("print",             Lprint),            \
    SETUP_TABLE_SELECT("printc",            Lprintc),           \
    SETUP_TABLE_SELECT("rdf",               Lrdf),              \
    SETUP_TABLE_SELECT("rds",               Lrds),              \
    SETUP_TABLE_SELECT("reclaim",           Lreclaim_1),        \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_1),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_1),   \
    SETUP_TABLE_SELECT("return",            Lreturn_1),         \
    SETUP_TABLE_SELECT("setpchar",          Lsetpchar),         \
    SETUP_TABLE_SELECT("sin",               Lsin),              \
    SETUP_TABLE_SELECT("sqrt",              Lsqrt),             \
    SETUP_TABLE_SELECT("stop",              Lstop_1),           \
    SETUP_TABLE_SELECT("stringp",           Lstringp),          \
    SETUP_TABLE_SELECT("symbolp",           Lsymbolp),          \
    SETUP_TABLE_SELECT("trace",             Ltrace),            \
    SETUP_TABLE_SELECT("unfluid",           Lunfluid),          \
    SETUP_TABLE_SELECT("unglobal",          Lunglobal),         \
    SETUP_TABLE_SELECT("untrace",           Luntrace),          \
    SETUP_TABLE_SELECT("upbv",              Lupbv),             \
    SETUP_TABLE_SELECT("vectorp",           Lvectorp),          \
    SETUP_TABLE_SELECT("wrs",               Lwrs),              \
    SETUP_TABLE_SELECT("vector",            Lvector_1),         \
    SETUP_TABLE_SELECT("thread",            Lthread),           \
    SETUP_TABLE_SELECT("zerop",             Lzerop),

#define SETUP1a

#define SETUP2                                                  \
    SETUP_TABLE_SELECT("list",              Llist_2),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_2),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_2),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_2),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_2),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_2),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_2),         \
    SETUP_TABLE_SELECT("apply",             Lapply),            \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_2),       \
    SETUP_TABLE_SELECT("condvar_wait",      Lcondvar_wait),     \
    SETUP_TABLE_SELECT("cons",              Lcons),             \
    SETUP_TABLE_SELECT("eq",                Leq),               \
    SETUP_TABLE_SELECT("equal",             Lequal),            \
    SETUP_TABLE_SELECT("error",             Lerror_2),          \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_2),       \
    SETUP_TABLE_SELECT("idifference",       Ldifference),       \
    SETUP_TABLE_SELECT("idivide",           Ldivide),           \
    SETUP_TABLE_SELECT("iequal",            Lequal),            \
    SETUP_TABLE_SELECT("igeq",              Lgeq),              \
    SETUP_TABLE_SELECT("igreaterp",         Lgreaterp),         \
    SETUP_TABLE_SELECT("ileftshift",        Lleftshift),        \
    SETUP_TABLE_SELECT("ileq",              Lleq),              \
    SETUP_TABLE_SELECT("ilessp",            Llessp),            \
    SETUP_TABLE_SELECT("iquotient",         Lquotient),         \
    SETUP_TABLE_SELECT("iremainder",        Lremainder),        \
    SETUP_TABLE_SELECT("irightshift",       Lrightshift),       \
    SETUP_TABLE_SELECT("get",               Lget),              \
    SETUP_TABLE_SELECT("gethash",           Lgethash),          \
    SETUP_TABLE_SELECT("getv",              Lgetv),             \
    SETUP_TABLE_SELECT("member",            Lmember),           \
    SETUP_TABLE_SELECT("memq",              Lmemq),             \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_2),         \
    SETUP_TABLE_SELECT("open",              Lopen),             \
    SETUP_TABLE_SELECT("open-module",       Lopen_module),      \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_2),       \
    SETUP_TABLE_SELECT("prog1",             Lprog1),            \
    SETUP_TABLE_SELECT("prog2",             Lprog2),            \
    SETUP_TABLE_SELECT("remhash",           Lremhash),          \
    SETUP_TABLE_SELECT("remprop",           Lremprop),          \
    SETUP_TABLE_SELECT("restart-csl",       Lrestart_lisp_2),   \
    SETUP_TABLE_SELECT("restart-lisp",      Lrestart_lisp_2),   \
    SETUP_TABLE_SELECT("rplaca",            Lrplaca),           \
    SETUP_TABLE_SELECT("rplacd",            Lrplacd),           \
    SETUP_TABLE_SELECT("set",               Lset),              \
    SETUP_TABLE_SELECT("thread2",           Lthread2),          \
    SETUP_TABLE_SELECT("vector",            Lvector_2),

#define SETUP2a

#define SETUP3                                                  \
    SETUP_TABLE_SELECT("list",              Llist_3),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_3),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_3),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_3),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_3),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_3),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_3),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_3),       \
    SETUP_TABLE_SELECT("errorset",          Lerrorset_3),       \
    SETUP_TABLE_SELECT("mkhash",            Lmkhash_3),         \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_3),       \
    SETUP_TABLE_SELECT("put",               Lput),              \
    SETUP_TABLE_SELECT("putd",              Lputd),             \
    SETUP_TABLE_SELECT("puthash",           Lputhash),          \
    SETUP_TABLE_SELECT("putv",              Lputv),             \
    SETUP_TABLE_SELECT("string-store",      Lstring_store1),    \
    SETUP_TABLE_SELECT("string-store1",     Lstring_store1),    \
    SETUP_TABLE_SELECT("vector",            Lvector_3),

#define SETUP3a

#define SETUP4                                                  \
    SETUP_TABLE_SELECT("list",              Llist_4),           \
    SETUP_TABLE_SELECT("list*",             Lliststar_4),       \
    SETUP_TABLE_SELECT("iplus",             Lplus_4),           \
    SETUP_TABLE_SELECT("itimes",            Ltimes_4),          \
    SETUP_TABLE_SELECT("ilogand",           Llogand_4),         \
    SETUP_TABLE_SELECT("ilogor",            Llogor_4),          \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_4),         \
    SETUP_TABLE_SELECT("checkpoint",        Lpreserve_4),       \
    SETUP_TABLE_SELECT("preserve",          Lpreserve_4),       \
    SETUP_TABLE_SELECT("string-store2",     Lstring_store2),    \
    SETUP_TABLE_SELECT("vector",            Lvector_4),

#define SETUP4a

#define SETUP5UP                                                \
    SETUP_TABLE_SELECT("list",              Llist_5up),         \
    SETUP_TABLE_SELECT("list*",             Lliststar_5up),     \
    SETUP_TABLE_SELECT("iplus",             Lplus_5up),         \
    SETUP_TABLE_SELECT("itimes",            Ltimes_5up),        \
    SETUP_TABLE_SELECT("ilogand",           Llogand_5up),       \
    SETUP_TABLE_SELECT("ilogor",            Llogor_5up),        \
    SETUP_TABLE_SELECT("ilogxor",           Llogxor_5up),       \
    SETUP_TABLE_SELECT("string-store3",     Lstring_store3),    \
    SETUP_TABLE_SELECT("string-store4",     Lstring_store4),    \
    SETUP_TABLE_SELECT("vector",            Lvector_5up),

#define SETUP5UPa

// The following are things that can be in function cells but that are
// not there as straightforward definitions of particular functions.
// They are listed here to cope with the needs of dumping and restoring
// heap images.

#define SETUP_INTERNAL                                          \
    SETUP_TABLE_SELECT("0undefined0",       undefined0),        \
    SETUP_TABLE_SELECT("1undefined1",       undefined1),        \
    SETUP_TABLE_SELECT("2undefined2",       undefined2),        \
    SETUP_TABLE_SELECT("3undefined3",       undefined3),        \
    SETUP_TABLE_SELECT("4undefined4",       undefined4),        \
    SETUP_TABLE_SELECT("5undefined5",       undefined5up),      \
    SETUP_TABLE_SELECT("0wrongnumber0",     wrongnumber0),      \
    SETUP_TABLE_SELECT("1wrongnumber1",     wrongnumber1),      \
    SETUP_TABLE_SELECT("2wrongnumber2",     wrongnumber2),      \
    SETUP_TABLE_SELECT("3wrongnumber3",     wrongnumber3),      \
    SETUP_TABLE_SELECT("4wrongnumber4",     wrongnumber4),      \
    SETUP_TABLE_SELECT("5wrongnumber5",     wrongnumber5up),    \
    SETUP_TABLE_SELECT("0interpreted0",     interpreted0),      \
    SETUP_TABLE_SELECT("1interpreted1",     interpreted1),      \
    SETUP_TABLE_SELECT("2interpreted2",     interpreted2),      \
    SETUP_TABLE_SELECT("3interpreted3",     interpreted3),      \
    SETUP_TABLE_SELECT("4interpreted4",     interpreted4),      \
    SETUP_TABLE_SELECT("5interpreted5",     interpreted5up)

// In order that it is possible to save and restore images and end up with
// function entrypoints correctly fixed up I need to be certain that the
// version of vsl that saved an image has at least the same set of functions
// provided as the version reloading. 


// With subversion there is an unambiguos concept of "revision number" and
// I can insert that here. If one used git the collection of checkins
// is not linear and it is much harder to have an obvious way of setting
// automatic labels on things!

const char *setup_revision = "$Revision: 0000 $";

#define MAX_NAMESIZE  24

const char setup_names[][MAX_NAMESIZE] =
{
#define SETUP_TABLE_SELECT(a, b) { "s" a }
    SETUPSPEC
    SETUPSPECa
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "0" a }
    SETUP0
    SETUP0a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "1" a }
    SETUP1
    SETUP1a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "2" a }
    SETUP2
    SETUP2a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "3" a }
    SETUP3
    SETUP3a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "4" a }
    SETUP4
    SETUP4a
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { "5" a }
    SETUP5UP
    SETUP5UPa
    { "x" },                      // Marks end of real functions
#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) { a }
    SETUP_INTERNAL
};

#undef SETUP_TABLE_SELECT
#define SETUP_TABLE_SELECT(a, b) (void *)b

void *setup_defs[] =
{
    SETUPSPEC
    SETUPSPECa
    SETUP0
    SETUP0a
    SETUP1
    SETUP1a
    SETUP2
    SETUP2a
    SETUP3
    SETUP3a
    SETUP4
    SETUP4a
    SETUP5UP
    SETUP5UPa
    NULL,
    SETUP_INTERNAL
};

