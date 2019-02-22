static void el_tidy() {
    el_end(el_struct);
    history_end(el_history);
}

static char *get_prompt(EditLine *el)
{   return the_prompt;
}

void setup_prompt() {
    stdin_tty = isatty(fileno(stdin)) && isatty(fileno(stdout));
    if (stdin_tty) {
        el_struct = el_init("vsl", stdin, stdout, stderr);
        el_history = history_init();
        atexit(el_tidy);
        history(el_history, &el_history_event, H_SETSIZE, 1000);
        el_set(el_struct, EL_PROMPT, get_prompt);
        el_set(el_struct, EL_HIST, history, el_history);
        el_set(el_struct, EL_EDITOR, "emacs"); // perhaps more intuitive than vim
    }
}

// next function just copied from CSL...

//
// The next procedure is responsible for establishing information about
// where the main checkpoint image should be recovered from, and where
// and fasl files should come from.
//
// On the Macintosh if the path to my executable indicated that I am
// within an "Application Bundle" I will look for the image file there.
//
// Otherwise I will look in two places! If the path to the executable ends
// up rather like BINDIR then I will check PKGDATADIR. The idea behind this
// is that if the files have been put in place using "make install" then
// the executable may be in say "...../bin/reduce" and the corresponding
// image would the be "..../share/reduce/reduce.img". I accept this if there
// is an image file in the location so suggested.
//
// Finally I look for an image file adjacent to the executable.
//

#ifndef BINDIR
// #define for stringify.
#define BINDIR /usr/local/bin
#endif

#ifndef PKGDATADIR
#define PKGDATADIR /usr/local/share/reduce
#endif

#define xstringify(s) xstringify_sub(s)
#define xstringify_sub(s) #s

#include <sys/stat.h>

int get_current_directory(char *s, size_t n)
{   if (getcwd(s, n) == 0)
    {   switch(errno)
        {   case ERANGE: return -2; // negative return value flags an error.
            case EACCES: return -3;
            default:     return -4;
        }
    }
    else return strlen(s);
}

//
// The next procedure is responsible for establishing information about
// both the "short-form" name of the program launched and the directory
// it was found in. This latter directory may be a good place to keep
// associated resources.
//
// The way of finding the information concerned differs between Windows and
// Unix/Linux, as one might expect.
//
// return non-zero value if failure.
//

const char *fullProgramName        = "./fwin.exe";
const char *programName            = "fwin.exe";
// const char *programDir             = ".";

#ifdef WIN32

static char this_executable[LONGEST_LEGAL_FILENAME];

int find_program_directory(const char *argv0)
{   char *w, *w1;
    char ww[LONGEST_LEGAL_FILENAME];
    int len, ndir, npgm;
// In older code I believed that I could rely on Windows giving me
// the full path of my executable in argv[0]. With bits of mingw/cygwin
// anywhere near me that may not be so, so I grab the information directly
// from the Windows APIs. Except that that turns out to be no good for
// a scheme I have that chains to an executable so it can pick which
// variant to use, so if argv0 looks like a fully rooted windows path
// I will use it!
//
    if (!(isalpha(argv0[0]) &&
          argv0[1] == ':' &&
          argv0[2] == '\\'))
    {   GetModuleFileName(NULL, this_executable, LONGEST_LEGAL_FILENAME-2);
        argv0 = this_executable;
    }
    strncpy(ww, argv0, sizeof(ww));
    ww[sizeof(ww)-1] = 0;
    w = ww;
//
// I turn every "\" into a "/". This make for better uniformity with other
// platforms.
//
    while (*w != 0)
    {   if (*w == '\\') *w = '/';
        w++;
    }
    programNameDotCom = 0;
    if (ww[0] == 0)      // should never happen - name is empty string!
    {   programDir = ".";
        programName = "fwin";  // nothing really known!
        fullProgramName = "./fwin.exe";
        return 0;
    }

    w = (char *)malloc(1+strlen(ww));
    if (w == NULL) return 5;           // 5 = malloc fails
    strcpy(w, ww);
    fullProgramName = w;
    len = strlen(ww);
//
// If the current program is called c:/aaa/xxx.exe, then the directory
// is just c:/aaa and the simplified program name is just xxx
//
    if (len > 4 &&
        w[len-4] == '.' &&
        ((tolower(w[len-3]) == 'e' &&
          tolower(w[len-2]) == 'x' &&
          tolower(w[len-1]) == 'e') ||
         (tolower(w[len-3]) == 'c' &&
          tolower(w[len-2]) == 'o' &&
          tolower(w[len-1]) == 'm')))
    {   programNameDotCom = (tolower(w[len-3]) == 'c');
        len -= 4;
        w[len] = 0;
    }
//
// I will strip any "win" prefix from the application name and also any
// "32" suffix.
//
    w1 = w;
    if (strlen(w) > 2)
    {   w += strlen(w) - 2;
        if (w[0] == '3' && w[1] == '2') w[0] = 0;
    }
    w = w1;
    while (*w != 0) w++;
    while (w != w1 && *w != '/'  && *w != '\\') w--;
    if (*w == '/' || *w == '\\') w++;
    if (strncmp(w, "win", 3) == 0)
    {   char *w2 = w + 3;
        while (*w2 != 0) *w++ = *w2++;
        *w = 0;
    }
    for (npgm=0; npgm<len; npgm++)
    {   int c = fullProgramName[len-npgm-1];
        if (c == '/') break;
    }
    ndir = len - npgm - 1;
    if (ndir < 0) programDir = ".";  // none really visible
    else
    {   if ((w = (char *)malloc(ndir+1)) == NULL) return 1;
        strncpy(w, fullProgramName, ndir);
        w[ndir] = 0;
        programDir = w;
    }
    if ((w = (char *)malloc(npgm+1)) == NULL) return 1;
    strncpy(w, fullProgramName + len - npgm, npgm);
    w[npgm] = 0;
    programName = w;
    return 0;
}

#else // WIN32

// Different systems put or do not put underscores in front of these
// names. My adaptation here should give me a chance to work whichever
// way round it goes.
//

#ifndef S_IFMT
# ifdef __S_IFMT
#  define S_IFMT __S_IFMT
# endif
#endif // S_IFMT

#ifndef S_IFDIR
# ifdef __S_IFDIR
#  define S_IFDIR __S_IFDIR
# endif
#endif // S_IFDIR

#ifndef S_IFREG
# ifdef __S_IFREG
#  define S_IFREG __S_IFREG
# endif
#endif // S_IFREG

#ifndef S_ISLNK
# ifdef S_IFLNK
#  ifdef S_IFMT
#   define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#  endif
# endif
#endif // S_ISLNK

//
// I will not take any action at all to deal with UTF-8 or Unicode issues
// in filenames or paths. Indeed most of Linux and certainly most of my
// code will risk terribly confusion with various perfectly ordinary
// 7-bit characters such as blank (' ') within filenames, so the issue
// of international alphabets there is something I will not really fuss
// about yet.
//

int find_program_directory(const char *argv0)
{   char pgmname[LONGEST_LEGAL_FILENAME];
    const char *w;
    char *w1;
    int n, n1;
    memset(pgmname, 0, sizeof(pgmname));
//
// If the main reduce executable is has a full path-name /xxx/yyy/zzz then
// I will use /xxx/yyy as its directory To find this I need to find the full
// path for the executable. I ATTEMPT to follow the behaviour of "sh",
// "bash" and "csh".  But NOTE WELL that if anybody launches this code in
// an unusual manner (eg using an "exec" style function) that could confuse
// me substantially. What comes in via argv[0] is typically just the final
// component of the program name - what I am doing here is scanning to
// see what path it might have corresponded to.
//
//
// If the name of the executable starts with a "/" it is already an
// absolute path name. I believe that if the user types (to the shell)
// something like $DIR/bin/$PGMNAME or ~user/subdir/pgmname then the
// environment variables and user-name get expanded out by the shell before
// the command is actually launched.
//
    if (argv0 == NULL || argv0[0] == 0) // Information not there - return
    {   programDir = (const char *)"."; // some sort of default.
        programName = (const char *)"fwin";
        fullProgramName = (const char *)"./fwin";
        return 0;
    }
//
// I will treat 3 cases here
// (a)   /abc/def/ghi      fully rooted: already an absolute name;
// (b)   abc/def/ghi       treat as ./abc/def/ghi;
// (c)   ghi               scan $PATH to see where it may have come from.
//
    else if (argv0[0] == '/') fullProgramName = argv0;
    else
    {   for (w=argv0; *w!=0 && *w!='/'; w++) {}   // seek a "/"
        if (*w == '/')      // treat as if relative to current dir
        {   // If the thing is actually written as "./abc/..." then
            // strip of the initial "./" here just to be tidy.
            if (argv0[0] == '.' && argv0[1] == '/') argv0 += 2;
            n = get_current_directory(pgmname, sizeof(pgmname));
            if (n < 0) return 1;    // fail! 1=current directory failure
            if (n + strlen(argv0) + 2 >= sizeof(pgmname) ||
                pgmname[0] == 0)
                return 2; // Current dir unavailable or full name too long
            else
            {   pgmname[n] = '/';
                strcpy(&pgmname[n+1], argv0);
                fullProgramName = pgmname;
            }
        }
        else
        {   const char *path = getenv("PATH");
//
// I omit checks for names of shell built-in functions, since my code is
// actually being executed by here. So I get my search path and look
// for an executable file somewhere on it. I note that the shells back this
// up with hash tables, and so in cases where "rehash" might be needed this
// code may become confused.
//
            struct stat buf;
            uid_t myuid = geteuid(), hisuid;
            gid_t mygid = getegid(), hisgid;
            int protection;
            int ok = 0;
// I expect $PATH to be a sequence of directories with ":" characters to
// separate them. I suppose it COULD be that somebody used directory names
// that had embedded colons, and quote marks or escapes in $PATH to allow
// for that. In such case this code will just fail to cope.
//
            if (path != NULL)
            {   while (*path != 0)
                {   while (*path == ':') path++; // skip over ":"
                    n = 0;
                    while (*path != 0 && *path != ':')
                    {   pgmname[n++] = *path++;
                        if (n > (int)(sizeof(pgmname)-3-strlen(argv0)))
                            return 3; // fail! 3=$PATH element overlong
                    }
// Here I have separated off the next segment of my $PATH and put it at
// the start of pgmname. Observe that to avoid buffer overflow I
// exit abruptly if the entry on $PATH is itself too big for my buffer.
//
                    pgmname[n++] = '/';
                    strcpy(&pgmname[n], argv0);
// see if the file whose name I have just built up exists at all.
                    if (stat(pgmname, &buf) == -1) continue;
                    hisuid = buf.st_uid;
                    hisgid = buf.st_gid;
                    protection = buf.st_mode; // info about the file found
//
// I now want to check if there is a file of the right name that is
// executable by the current (effective) user.
//
                    if (protection & S_IXOTH ||
                        (mygid == hisgid && protection & S_IXGRP) ||
                        (myuid == hisuid && protection & S_IXUSR))
                    {   ok = 1;   // Haha - I have found the one we ...
                        break;    // are presumably executing!
                    }
                }
            }
            if (!ok) return 4;    // executable not found via $PATH
// Life is not yet quite easy! $PATH may contain some items that do not
// start with "/", ie that are still local paths relative to the
// current directory. I want to be able to return an absolute fully
// rooted path name! So unless the item we have at present starts with "/"
// I will stick the current directory's location in front.
//
            if (pgmname[0] != '/')
            {   char temp[LONGEST_LEGAL_FILENAME];
                memset(temp, 0, sizeof(temp));
                strcpy(temp, pgmname);
                n = get_current_directory(pgmname, sizeof(pgmname));
                if (n < 0) return 1;    // fail! 1=current directory failure
                if ((n + strlen(temp) + 1) >= sizeof(pgmname)) return 9;
                pgmname[n++] = '/';
                strcpy(&pgmname[n], temp);
            }
            fullProgramName = pgmname;
        }
    }
//
// Now if I have a program name I will try to see if it is a symbolic link
// and if so I will follow it.
//
    {   struct stat buf;
        char temp[LONGEST_LEGAL_FILENAME];
        memset(temp, 0, sizeof(temp));
        if (lstat(fullProgramName, &buf) != -1 &&
            S_ISLNK(buf.st_mode) &&
            (n1 = readlink(fullProgramName,
                           temp, sizeof(temp)-1)) > 0)
        {   temp[n1] = 0;
            strcpy(pgmname, temp);
            fullProgramName = pgmname;
        }
    }
// Now fullProgramName is set up, but may refer to an array that
// is stack allocated. I need to make it proper!
//
    w1 = (char *)malloc(1+strlen(fullProgramName));
    if (w1 == NULL) return 5;           // 5 = malloc fails
    strcpy(w1, fullProgramName);
    fullProgramName = w1;
#ifdef __CYGWIN__
//
// Now if I built on raw cygwin I may have an unwanted ".com" or ".exe"
// suffix, so I will purge that! This code exists here because the raw
// cygwin build has a somewhat schitzo view as to whether it is a Windows
// or a Unix-like system. When I am using raw cygwin I am really not
// living in a Windows world.
//
    if (strlen(w1) > 4)
    {   char *w2 = w1 + strlen(w1) - 4;
        if (w2[0] == '.' &&
            ((tolower((unsigned char)w2[1]) == 'e' &&
              tolower((unsigned char)w2[2]) == 'x' &&
              tolower((unsigned char)w2[3]) == 'e') ||
             (tolower((unsigned char)w2[1]) == 'c' &&
              tolower((unsigned char)w2[2]) == 'o' &&
              tolower((unsigned char)w2[3]) == 'm'))) w2[0] = 0;
    }
    if (strlen(w1) > 2)
    {   char *w2 = w1 + strlen(w1) - 2;
        if (w2[0] == '3' && w2[1] == '2') w2[0] = 0;
    }
//
// If I am building a cygwin version I will remove any prefix
// "cygwin-", "cygwin64-" or "win" from the front of the name of the
// executable and also any "32" suffix.
//
    while (*w1 != 0) w1++;
    while (w1 != fullProgramName && *w1 != '/'  && *w1 != '\\') w1--;
    if (*w1 == '/' || *w1 == '\\') w1++;
    if (strncmp(w1, "cygwin-", 7) == 0)
    {   char *w2 = w1 + 7;
        while (*w2 != 0) *w1++ = *w2++;
        *w1 = 0;
    }
    else if (strncmp(w1, "cygwin64-", 9) == 0)
    {   char *w2 = w1 + 9;
        while (*w2 != 0) *w1++ = *w2++;
        *w1 = 0;
    }
    if (strncmp(w1, "win", 3) == 0)
    {   char *w2 = w1 + 3;
        while (*w2 != 0) *w1++ = *w2++;
        *w1 = 0;
    }
#endif // __CYGWIN__
// OK now I have the full name, which is of the form
//   abc/def/fgi/xyz
// and I need to split it at the final "/" (and by now I very fully expect
// there to be at least one "/".
//
    for (n=strlen(fullProgramName)-1; n>=0; n--)
        if (fullProgramName[n] == '/') break;
    if (n < 0) return 6;               // 6 = no "/" in full file path
    w1 = (char *)malloc(1+n);
    if (w1 == NULL) return 7;           // 7 = malloc fails
    strncpy(w1, fullProgramName, n);
    w1[n] = 0;
// Note that if the executable was "/foo" then programDir will end up as ""
// so that programDir + "/" + programName works out properly.
//
    programDir = w1;
    n1 = strlen(fullProgramName) - n;
    w1 = (char *)malloc(n1);
    if (w1 == NULL) return 8;           // 8 = malloc fails
    strncpy(w1, fullProgramName+n+1, n1-1);
    w1[n1-1] = 0;
    programName = w1;
    return 0;                          // whew!
}

#endif // WIN32


const char *find_image_directory(int argc, const char *argv[])
{
    int n;
    char *w;
    char xname[LONGEST_LEGAL_FILENAME];
    memset(xname, 0, sizeof(xname));
#ifdef MACINTOSH
//
// There is a special oddity on the Macintosh (with the wxWidgets version
// where windowed versions are set up as "applications" in a directory that
// forms an "application bundle". The programDir here can then refer to
// ./reduce.app/Contents/MacOS/reduce (or whatever) and it is probably good
// to make the default image location be reduce.app/Contents/MacOS too.
// But then the vanilla console mode version is liable to
// be just ./reduce, and I want one image file to be used for both versions.
// Furthermore some kind person may have launched the executable that is
// within the application bundle directly from a console so that it is not
// really an application after all. I will do a load of rather curious
// tests here that are intended to detect the above cases and do special
// things! My tests will be based on file names and paths.
//
    int r = snprintf(xname, sizeof(xname),
                     "/%s.app/Contents/MacOS", programName);
    if (r<0) strcpy(xname, "badfile");
    else if ((unsigned int)r>=sizeof(xname)) xname[sizeof(xname)-1] = 0;
    n = strlen(programDir) - strlen(xname);
    if (n>=0 && strcmp(programDir+n, xname) == 0)
    {   // Seem to be being executed from within application bundle.
// This dates from when I thought I would put the image in merely Contents not
// in Contents/MacOS.
        r = snprintf(xname, sizeof(xname), "%.*s/%s.img",
                     (int)strlen(programDir), programDir, programName);
        if (r<0) strcpy(xname, "badfile");
        else if ((unsigned int)r>=sizeof(xname)) xname[sizeof(xname)-1] = 0;
    }
    else
    {   struct stat buf;
//
// If I am NOT within an application bundle but there is one next to me I
// will put the image file in the application directory. Of there is no
// such bundle I will put the image file in the location I would have used
// with Windows of X11.
//
        r = snprintf(xname, sizeof(xname),
                     "%s/%s.app/Contents/MacOS", programDir, programName);
        if (r<0) strcpy(xname, "badfile");
        else if ((unsigned int)r>=sizeof(xname)) xname[sizeof(xname)-1] = 0;
        if (stat(xname, &buf) == 0 &&
            (buf.st_mode & S_IFDIR) != 0)
        {   r = snprintf(xname, sizeof(xname),
                "%s/%s.app/Contents/MacOS/%s.img",
                programDir, programName, programName);
        }
        else r = snprintf(xname, sizeof(xname),
                          "%s/%s.img", programDir, programName);
        if (r<0) strcpy(xname, "badfile");
        else if ((unsigned int)r>=sizeof(xname)) xname[sizeof(xname)-1] = 0;
    }
#else
    {   const char *bin  = xstringify(BINDIR);
        const char *data = xstringify(PKGDATADIR);
//
// I will strip initial directory names from bin and pkgdatadir so long as
// they match. So if they start off as (eg) /usr/local/bin and
// /usr/local/share/reduce I will remove "/usr/local" from each leaving just
// "/bin" and "/share/reduce". The purpose of this is so that if (despite the
// use of "make install") somebody has copied the tree that contains Reduce
// to somewhere else I might still find my resources.
//
        int i, j;
        struct stat buf;
        const char *pn = programName;
#if defined WIN32 || defined __CYGWIN__
//
// On Windows I can have reduce.exe, cygwin-reduce.exe and cygwin64-reduce.exe
// all present, and for immediate purposes I want them all to be treated as
// if merely called "reduce".
//
        if (strncmp(pn, "cygwin-", 7) == 0) pn += 7;
        else if (strncmp(pn, "cygwin64-", 9) == 0) pn += 9;
#endif // WIN32
        for (;;)
        {   i = j = 0;
            if (*bin == '/') while (bin[++i] != 0 && bin[i] != '/');
            if (*data == '/') while (data[++j] != 0 && data[j] != '/');
            if (i != 0 && i == j && strncmp(bin, data, i) == 0)
            {   bin += i;
                data += i;
            }
            else break;
        }
        i = strlen(bin);
        j = strlen(programDir);
        int r;
        if (strcmp(programDir+j-i, bin) == 0)
        {   r = snprintf(xname, sizeof(xname),
                         "%.*s%s/%s.img", j-i, programDir, data, pn);
            if (r<0) strcpy(xname, "badfile");
            else if ((unsigned int)r>=sizeof(xname)) xname[sizeof(xname)-1] = 0;
        }
//
// If the name I just created does not correspond to a file I will fall
// back and use the older location, adjacent to my binary. Hmmm this is
// all interesting as regards building an image file for the first time.
// I think it tells us that you had better not try doing that using the
// installed version - do that with a copy that sits in your own private
// writable are of disc.
//
        if (stat(xname, &buf) != 0)
            snprintf(xname, sizeof(xname), "%s/%s.img", programDir, pn);
    }
#endif
    n = strlen(xname)+1;
    w = (char *)malloc(n);
    if (w == NULL) abort();
    strcpy(w, xname);
    return w;
}

void set_up_lispdir(int argc, const char *argv[])
{   find_program_directory(argv[0]);
    const char *s = find_image_directory(argc, argv);
    printf("find_image_directory() = <%s>\n", s);
    printf("programName = <%s>\n", programName);
    printf("programDir = <%s>\n", programDir);
}


int main(int argc, char *argv[])
{   set_up_lispdir(argc, (const char **)argv);
    for (int i=0; i<MAX_LISPFILES; i++)
    {   filecurchar[i] = '\n';
        filesymtype[i] = '?';
        filecursym[i] = nil;
    }
    setup_prompt();
    const char *inputfilename = NULL;
    void *pool;
//@@#ifdef DEBUG
    setvbuf(stdout, NULL, _IONBF, 0);
//@@#endif // DEBUG
//
// The "+16" here is to allow for aliging up memory to be at addresses
// that are multiples of 8.
    pool = allocate_memory(sizeof(block_header) +
                           (2*64 + 5)*HALFBITMAPSIZE + 16);
    if (pool == NULL)
    {   printf("Not enough memory available: Unable to proceed\n");
        my_exit(EXIT_FAILURE);
    }
// I only fill in one entry in the memory block at this stage.
    ((block_header *)pool)->halfbitmapsize = HALFBITMAPSIZE;
    blocks[0] = blocks_by_age[0] = (uintptr_t)pool;
// All others point to the top of virtual memory.
    for (size_t i=1; i<16; i++) blocks[i] = blocks_by_age[i] = (uintptr_t)(-1);
    nblocks = 1;
    C_stackbase = (LispObject *)((intptr_t)&inputfilename &
                                    -sizeof(LispObject));
    coldstart = 0;
    interactive = 1;
#ifdef DEBUG
    logfile = fopen("vsl.log", "w");
#endif // DEBUG
#ifdef __WIN32__
    size_t i = strlen(argv[0]);
    if (strcmp(argv[0]+i-4, ".exe") == 0) i -= 4;
    int r = snprintf(imagename, sizeof(imagename), "%.*s.img", i, argv[0]);
#else // __WIN32__
    int r = snprintf(imagename, sizeof(imagename), "%s.img", argv[0]);
#endif // __WIN32__
    if (r<0) strcpy(imagename, "badfile.img");
    else if ((unsigned int)r>=sizeof(imagename)) imagename[sizeof(imagename)-1] = 0;
    for (int i=1; i<argc; i++)
    {
// I have some VERY simple command-line options here.
//        -z         do a "cold start".
//        -ifilename use that as image file
//        filename   read from that file rather than from the standard input.
        if (strcmp(argv[i], "-z") == 0) coldstart = 1;
        else if (strncmp(argv[i], "-i", 2) == 0)
        {   if (argv[i][2] != 0) strcpy(imagename, argv[i]+2);
            else if (i<argc-1) strcpy(imagename, argv[++i]);
        }
        else if (argv[i][0] != '-') inputfilename = argv[i], interactive = 0;
    }
    printf("VSL version %d.%.3d\n", IVERSION, FVERSION); fflush(stdout);
    linepos = 0;
    for (size_t i=0; i<MAX_LISPFILES; i++) lispfiles[i] = 0;
    lispfiles[0] = stdin;   lispfiles[1] = stdout;
    lispfiles[2] = stderr;  lispfiles[3] = stdin;
    file_direction = (1<<1) | (1<<2); // 1 bits for writable files.
    lispin = 3; lispout = 1;
    if (inputfilename != NULL)
    {   FILE *in = fopen(inputfilename, "r");
        if (in == NULL)
            printf("Unable to read from %s, so using standard input\n",
                   inputfilename);
        else lispfiles[3] = in;
    }
    boffop = 0;
    for (;;) // This loop is for restart-lisp and preserve.
    {   allocateheap();
// A warm start will read an image file which it expects to have been
// made by a previous use of vsl.
        if (coldstart) cold_start();
        else
        {   gzFile f = gzopen(imagename, "rb");
            int i, errcode;
            if (f == NULL)
            {   printf("Error: unable to open image for reading\n");
                my_exit(EXIT_FAILURE);
            }
            if ((i = warm_start(f, &errcode)) != 0)
            {
                gzerror(f, &errcode);
                gzclose(f);
// First case is when gzread has not reported any problems but when the
// internal logic in warm_start has detected some inconsiency.
                if (errcode == Z_OK)
                    printf("+++ Error parsing file (code=%d)\n", i);
// Second case is if the operating system reported trouble reading the
// image file.
                else if (errcode == Z_ERRNO)
                    printf("+++ Error reading image file (code=%d/%d)\n",
                           errno, i);
// Third case is when gzread finds data in a format that it objects to.
                else printf("+++ Error decompressing image file (code=%d/%d)\n",
                            errcode, i);
                my_exit(EXIT_FAILURE);
            }
        }
// Any predefined specified on the command-line using -Dxx=yy are
// instated or re-instated here so they apply even after restart!-lisp.
        for (int i=1; i<argc; i++)
        {   if (argv[i][0] == '-' && argv[i][1] == 'D')
            {   const char *d1 = strchr(argv[i], '=');
                if (d1 == NULL) continue;
// In general through setup (and I count this as still being setup)
// I will code on the basis that there will not be any garbage collection
// so I do not need to think about the effects of data movement during GC.
                qvalue(lookup(argv[i]+2, (d1-argv[i])-2, 3)) =
                    makestring(d1+1, strlen(d1+1));
            }
        }
        fflush(stdout);
// I am fixing things so that "-Tname" on the command line arranges to trace
// function "name".
        for (int i=1; i<argc; i++)
        {   if (argv[i][0] == '-' && argv[i][1] == 'T')
            {   const char *d1 = &argv[i][2];
                LispObject d3 = lookup(d1, strlen(d1), 1);
                qflags(d3) |= flagTRACED;
            }
        }
        curchar = '\n'; symtype = '?'; cursym = nil;
        if (boffop == 0) // Use standard restart function from image.
        {   if (restartfn == nil) readevalprint(0);
            else Lapply(nil, restartfn, nil);
        }
        else
        {   LispObject x, data = makestring(boffo, boffop);
            data = Lcompress(nil, Lexplodec(nil, data));
            x = qcar(data);   // 'fn or '(module fn)
            if (isCONS(x))
            {   Lload_module(nil, qcar(x));
                x = qcar(qcdr(x));
            }
            Lapply(nil, x, qcdr(data));
        }
        if ((unwindflag & unwindPRESERVE) != 0)
        {   gzFile f = gzopen(imagename, "wbT");
            if (f == NULL)
                printf("\n+++ Unable to open %s for writing\n", imagename);
            else write_image(f);

// A cautious person would have checked for error codes returned by the
// above calls to write and close. I omit that here to be concise.
        }
        if ((unwindflag & unwindRESTART) == 0) break;
        unwindflag = unwindNONE;
        boffop = 0;
        if (qcar(work1) == nil) coldstart = 1;
        else if (qcar(work1) == lisptrue) coldstart = 0;
        else
        {   int save = lispout;
            int savepos = linepos;
            lispout = -2;
            internalprint(work1);
            wrch(0);
            lispout = save;
            linepos = savepos;
            coldstart = 0;
        }
    }
    return 0;
}
