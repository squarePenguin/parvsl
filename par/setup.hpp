INLINE constexpr size_t SETUPSIZE = sizeof(setup_defs)/sizeof(void *);

// The number of entries I have in my setup hash table will be about
// twice the number of entries to put in it, and is not a multiple of
// 2, 3, 5, 7 or 11. Well until I have over 1000 built-in functions its
// size is just established by the rounding-up procedure here, however
// it should scale reasonably happily to almost any size of system.

INLINE const size_t SETUPHASHSIZE = (((2*3*5*7*11)*((int)SETUPSIZE/1000+1))+1);
static inline size_t HASHPTR(void *x)
{   return (size_t)((((uintptr_t)x)*314159u)%SETUPHASHSIZE);
}

void setwrongnumbers(LispObject w)
{   if (qdefn0(w) == undefined0) qdefn0(w) = wrongnumber0;
    if (qdefn1(w) == undefined1) qdefn1(w) = wrongnumber1;
    if (qdefn2(w) == undefined2) qdefn2(w) = wrongnumber2;
    if (qdefn3(w) == undefined3) qdefn3(w) = wrongnumber3;
    if (qdefn4(w) == undefined4) qdefn4(w) = wrongnumber4;
    if (qdefn5up(w) == undefined5up) qdefn5up(w) = wrongnumber5up;
}

void setup()
{
// Ensure that initial symbols and functions are in place. Parts of this
// code are rather rambling and repetitive but this is at least a simple
// way to do things. I am going to assume that nothing can fail within this
// setup code, so I can omit all checks for error conditions.
    int i;
    undefined = lookup("~indefinite-value~", 18, 3);
    global_symbol(undefined);
    par::symval(undefined) = undefined;
    nil = lookup("nil", 3, 3);
    global_symbol(nil);
    par::symval(nil) = nil;

    lisptrue = lookup("t", 1, 3);
    global_symbol(lisptrue);
    par::symval(lisptrue) = lisptrue;

    echo = lookup("*echo", 5, 3);
    fluid_symbol(echo);
    par::symval(echo) = interactive ? nil : lisptrue;

    {
        LispObject nn = lookup("*nocompile", 10, 3);
        fluid_symbol(nn);
        par::symval(nn) = lisptrue;
    }

    lispsystem = lookup("lispsystem*", 11, 1);
    global_symbol(lispsystem);
    par::symval(lispsystem) =
        list2star(lookup("vsl", 3, 1), lookup("csl", 3, 1),
                  list2star(lookup("embedded", 8, 1),
                      cons(lookup("image", 5, 3),
                           makestring(imagename, strlen(imagename))), nil));
    quote = lookup("quote", 5, 3);
    backquote = lookup("`", 1, 3);
    comma = lookup(",", 1, 3);
    comma_at = lookup(",@", 2, 3);
    eofsym = lookup("$eof$", 5, 3);
    global_symbol(eofsym);
    par::symval(eofsym) = eofsym;
    symlambda = lookup("lambda", 6, 3);
    expr = lookup("expr", 4, 3);
    subr = lookup("subr", 4, 3);
    fexpr = lookup("fexpr", 5, 3);
    fsubr = lookup("fsubr", 5, 3);
    macro = lookup("macro", 5, 3);
    input = lookup("input", 5, 3);
    output = lookup("output", 6, 3);
    pipe = lookup("pipe", 4, 3);
    dfprint = lookup("dfprint*", 8, 3);
    fluid_symbol(dfprint);
    par::symval(dfprint) = nil;

    bignum = lookup("~bignum", 7, 3);
    symraise = lookup("*raise", 6, 3);
    symlower = lookup("*lower", 6, 3);
    fluid_symbol(symraise);
    fluid_symbol(symlower);
    par::symval(symraise) = nil;
    par::symval(symlower) = lisptrue;
    Lput(nil, symraise, symfluid, lisptrue);
    Lput(nil, symlower, symfluid, lisptrue);
    cursym = nil;
    work1 = work2 = nil;
    for (i=0; setup_names[i][0]!='x'; i++)
    {   LispObject w = lookup(1+setup_names[i], strlen(1+setup_names[i]), 3);
        if (qdefn0(w) == undefined0) qdefn0(w) = wrongnumber0;
        if (qdefn1(w) == undefined1) qdefn1(w) = wrongnumber1;
        if (qdefn2(w) == undefined2) qdefn2(w) = wrongnumber2;
        if (qdefn3(w) == undefined3) qdefn3(w) = wrongnumber3;
        if (qdefn4(w) == undefined4) qdefn4(w) = wrongnumber4;
        if (qdefn5up(w) == undefined5up) qdefn5up(w) = wrongnumber5up;
        switch (setup_names[i][0])
        {
        case '0':
            qdefn0(w) = (LispFn0 *)setup_defs[i];
            break;
        case 's':
            qflags(w) |= flagSPECFORM;
            // drop through.
        case '1':
            qdefn1(w) = (LispFn1 *)setup_defs[i];
            break;
        case '2':
            qdefn2(w) = (LispFn2 *)setup_defs[i];
            break;
        case '3':
            qdefn3(w) = (LispFn3 *)setup_defs[i];
            break;
        case '4':
            qdefn4(w) = (LispFn4 *)setup_defs[i];
            break;
        case '5':
            qdefn5up(w) = (LispFn5up *)setup_defs[i];
            break;
        }
    }
}

void cold_start()
{
// version of setup to call when there is no initial heap image at all.
    size_t i;
// I make the object-hash-table lists end in a fixnum rather than nil
// because I want to create the hash table before even the symbol nil
// exists.
    for (i=0; i<OBHASH_SIZE; i++) obhash[i] = tagFIXNUM;
    for (i=0; i<BASES_SIZE; i++) listbases[i] = NULLATOM;
    setup();
// The following fields could not be set up quite early enough in the
// cold start case, so I repair them now.
    restartfn = qplist(undefined) = qlits(undefined) =
        qplist(nil) = qlits(nil) = nil;
}

// I will comment here on some of the special features of VSL+ heap images.
// The following objective apply:
// (1) An image created on any version of vsl+ should be re-loadable
//     on any other (at least that is not TOO far away in terms of revision
//     levels). Specifically there will not be a fixed heap size implicit
//     in an image file, and word-length, byte order and machine architecture
//     should be allowed for.
// (2) Compiled code should be preserved within a heap image, but if the
//     image is reloaded on a machine with a different architecture then
//     a form of just-in-time compilation will be activated to create
//     code for the new machine from a compact bytecode representation
//     stored in the image. If the architecture has not been provided with
//     a native JIT compiler then the bytecodes will be interpreted
//     directly.
// (3) Image files will be compressed (using zlib) and so will tend to be
//     compact.

int32_t read32(gzFile f)
{
    unsigned char c[4];
    if (gzread(f, c, 4) != 4) return 0;
    return (c[0] + (c[1]<<8) + (c[2]<<16) + (c[3]<<24));
}

INLINE const uint32_t FILEID = ('v' << 0) | ('s' << 8) |
                               ('l' << 16) | ('+' << 24);
INLINE const uint32_t ENDID  = ('\n' << 0) | ('e' << 8) |
                               ('n' << 16) | ('d' << 24);
// Crude allocation for temporary space within the heap2 area.

void *h2alloc(int n)
{
    void *r = (void *)fringe2;
    if (fringe2+n > limit2) return NULL;
    fringe2 += n;
    return r;
}

uint32_t revision = 0, version=0;

// This is the normal reload case where the word-width of the image
// matches that of the system it is being used on. However it is
// still possible that byte orders differ.

void **setuphash1k;
const char **setuphash1v;
const char **setuphash2k;
void **setuphash2v;

// If the item that is sought is not found then this hash lookup code
// will return NULL.

void *setuphashlookup(void *k)
{
    size_t i = HASHPTR(k);
    const uint64_t *p;
    while (setuphash1k[i] != k &&
           setuphash1k[i] != NULL) i = (i + 1) % SETUPHASHSIZE;
    if (setuphash1k[i] == NULL) return NULL;
    p = (uint64_t *)setuphash1v[i];
    i = (int)((p[0] + p[1]) % SETUPHASHSIZE);
    while (setuphash2k[i] != NULL &&
           memcmp(setuphash2k[i], p, MAX_NAMESIZE) != 0)
        i = (i + 1) % SETUPHASHSIZE;
    return setuphash2v[i];
}

uint32_t image_nblocks;
uintptr_t image_blocks_by_age[16], image_blocks[16],
          image_bitmapsizes[16], image_offsets[16], image_h1base[16];
uintptr_t image_fringe1;

LispObject relocate(LispObject x)
{
    int i;
    switch (x & TAGBITS)
    {   case tagATOM:
           if (x == NULLATOM) return x;
        case tagCONS:
        case tagSYMBOL:
        case tagFLOAT:
            break; // These things actually need relocating.
        default:
//case tagFIXNUM:
//case tagFORWARD:
//case tagHDR:
            return x;
    }
// Now x should be a reference into the image heap. I first want to
// convert it into a simple offset from the start of the heap as if
// the heap had all been put in a single contiguous chunk. If the
// reference is not within the heap1 part of the image heap then I
// have a corrupted image...
    i = search((uintptr_t)x,
        image_blocks[0], image_blocks[1], image_blocks[2], image_blocks[3],
        image_blocks[4], image_blocks[5], image_blocks[6], image_blocks[7],
        image_blocks[8], image_blocks[9], image_blocks[10],image_blocks[11],
        image_blocks[12],image_blocks[13],image_blocks[14],image_blocks[15],
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    x = image_offsets[i] + x - image_h1base[i];
    i = search((uintptr_t)x,
        blocks_offset[0], blocks_offset[1], blocks_offset[2], blocks_offset[3],
        blocks_offset[4], blocks_offset[5], blocks_offset[6], blocks_offset[7],
        blocks_offset[8], blocks_offset[9], blocks_offset[10],blocks_offset[11],
        blocks_offset[12],blocks_offset[13],blocks_offset[14],blocks_offset[15],
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    x += ((block_header *)blocks[i])->h1base;
    return x;
}

void *relocate_fn(void *x)
{
    void *r = setuphashlookup(x);
    uintptr_t rr;
    if (r != NULL) return r;
    printf("Function entrypoint did not relocate\n");
    rr = (LispObject)x;
// I must remove the tag bits before calling relocate because otherwise
// they might make the object appear to be a FIXNUM or other immediate
// data that would not need anything done to it.
    rr = relocate(rr & ~(LispObject)TAGBITS) | (rr & TAGBITS);
// If, as here, the code pointer seemed to be in the heap I should probably
// verify that not only is it within heap1 but that it points at an
// object there that has a relevant header.
    return (void *)rr;
}

// The following returns 0 on success and a line number on failure.

int warm_start_1(gzFile f, int *errcode)
{
#ifdef DEBUG_GLOBALS
    par::debug_safe = false;
#endif

    uintptr_t i, b1;
    uint32_t setupsize;
    char (*imagesetup_names)[MAX_NAMESIZE];
    void **imagesetup_defs;
    uintptr_t fr1, lim1, total_size, remaining_size;
// I have the table of names of entrypoints that are defined by
// the kernel. Note that the table as present in the saved image may not
// be the same size as the one in the code I am executing, so I can not
// allocate fixed space for it. However while I reload an image file I will
// only be filling in heap1 with data, so the region that will count as
// heap2 is free. And since nothing at all is in that at present I can
// use it without formality. I am going to assume for now that I have
// more than enough space in the first block for all that I need.
    fringe2 = ((block_header *)blocks_by_age[0])->h2base;
    limit2 = ((block_header *)blocks_by_age[0])->h2top;
    setupsize = read32(f);
// I will reject the image if the size of the setup table has changed by
// at least a factor of 2. That level of change would indicate such
// large adjustments that forcing all images to be re-built surely makes
// sense.
    if (setupsize <= SETUPSIZE/2 || setupsize >= 2*SETUPSIZE) return __LINE__;
    imagesetup_names = (char (*)[MAX_NAMESIZE])h2alloc(setupsize*MAX_NAMESIZE);    
    if (imagesetup_names == NULL) return __LINE__;
// Note that gzread and gzwrite return an int not an unsigned value, so
// when I want to check if they processed the expected number of bytes I
// cast the byte-count to an int even if it was a size_t to start with.
    if (gzread(f, imagesetup_names, (unsigned int)(setupsize*MAX_NAMESIZE)) !=
        (int)(setupsize*MAX_NAMESIZE)) return __LINE__;
    imagesetup_defs = (void **)h2alloc(setupsize*sizeof(void *));
    if (imagesetup_defs == NULL) return __LINE__;
    if (gzread(f, imagesetup_defs, (unsigned int)(setupsize*sizeof(void *))) !=
        (int)(setupsize*sizeof(void *))) return __LINE__;
    setuphash1k = (void **)h2alloc(SETUPHASHSIZE*sizeof(void *));
    setuphash1v = (const char **)h2alloc(SETUPHASHSIZE*sizeof(char *));
    setuphash2k = (const char **)h2alloc(SETUPHASHSIZE*sizeof(char *));
    setuphash2v = (void **)h2alloc(SETUPHASHSIZE*sizeof(void *));
        if (setuphash1k == NULL ||
        setuphash1v == NULL ||
        setuphash2k == NULL ||
        setuphash2v == NULL) return __LINE__;
    memset(setuphash1k, 0, SETUPHASHSIZE*sizeof(void *));
    memset(setuphash1v, 0, SETUPHASHSIZE*sizeof(char *));
    memset(setuphash2k, 0, SETUPHASHSIZE*sizeof(char *));
    memset(setuphash2v, 0, SETUPHASHSIZE*sizeof(void *));
// To map function entrypoints in the image file into ones in that are
// usable in the running system I first look up an associated name and then
// use that to find the new entrypoint. I have allocated and cleared the
// hash tables used for this - now populate them. The process should only
// ever insert items that are not already in the hash tables, and this
// simplifies the code somewhat.
    for (i=0; i<setupsize; i++)
    {   int h = HASHPTR(imagesetup_defs[i]);
        while (setuphash1k[h] != NULL) h = (h + 1) % SETUPHASHSIZE;
        setuphash1k[h] = imagesetup_defs[i];
        setuphash1v[h] = imagesetup_names[i];
    }
    for (i=0; i<SETUPSIZE; i++)
    {   const uint64_t *s = (const uint64_t *)setup_names[i];
// The next line computes a value based on the first 16 bytes of the
// name of an entrypoint. Since it is just used as as hash value the
// fact that the exact value loaded will depend on whether a big or
// little-endian machine is in use does not matter, and the fact that
// I insert each name just once means that it is Ok to avoid any
// real comparison of the strings concerned.
        int h = (int)((s[0] + s[1]) % SETUPHASHSIZE);
        while (setuphash2k[h] != NULL) h = (h + 1) % SETUPHASHSIZE;
        setuphash2k[h] = setup_names[i];
        setuphash2v[h] = setup_defs[i];
    }
// The hash tables that cope with entrypoints are now in place - next
// the reliable heap listbases can be reloaded. I am going to assume that
// the sizes of these match between image file and running system.
// I should probably encode the sizes as part of the image format so as
// to police that...
    if (gzread(f, listbases, (unsigned int)sizeof(listbases)) !=
        (int)sizeof(listbases)) return __LINE__;
// A small reminder here: when I move to more complete Common Lisp
// support the single (and fixed size) hash table for use as an object
// list will need to be replaced with something much more elaborate.
// That change will render image files incompatible.
    if (gzread(f, obhash, (unsigned int)sizeof(obhash)) !=
        (int)sizeof(obhash)) return __LINE__;
    if ((image_nblocks = read32(f)) == 0 ||
         image_nblocks > 16) return __LINE__;
    if (gzread(f, image_blocks_by_age,
                  (unsigned int)sizeof(blocks_by_age)) !=
        (int)sizeof(blocks_by_age)) return __LINE__;
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t nn;
        if (gzread(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return __LINE__;
        image_bitmapsizes[i] = nn;
    }
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t nn;
        if (gzread(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return __LINE__;
        image_h1base[i] = nn;
    }
// image_fringe1 is the offset from h1base in the block that fringe1 is
// within.
    if (gzread(f, &image_fringe1,
                  (unsigned int)sizeof(image_fringe1)) !=
        (int)sizeof(image_fringe1)) return __LINE__;
    for (;i<16; i++)
    {   image_h1base[i] = (uintptr_t)(-1);
        image_bitmapsizes[i] = 0;
    }
// I will now set up a map of offsets of the sections within the CURRENT
// heap...
    total_size = 0;
    for (i=0; i<nblocks; i++)
    {   blocks_offset[i] = total_size;
        total_size += 64*((block_header *)blocks_by_age[i])->halfbitmapsize;
    }
    for (;i<16; i++) blocks_offset[i] = total_size;
// Now back to dealing with the image heap.
    total_size = 0;
    for (i=0; i<image_nblocks; i++)
    {
        image_offsets[i] = total_size*64;
        total_size += image_bitmapsizes[i];
    }
    for (;i<16; i++) image_offsets[i] = total_size*64;
// The total size recorded here is the size of bitmap data present in
// the image. The amount of heap will be 64 times as great.
// There is an issue whereby the final block dumped only had information
// written as far as data was valid.
    total_size *= 64;
    assert(image_fringe1 >= 0);
    assert(image_fringe1 < 64*image_bitmapsizes[image_nblocks-1]);
    total_size -=
        (64*image_bitmapsizes[image_nblocks-1] - image_fringe1);
// Next I need to reload the body of the image. The version in the
// image file is a single chunk of bytes...
    printf("Reloading the image will need around %#" PRIxPTR " bytes\n",
           total_size);
// I suppose that if there was not enough memory already allocated I could
// try extending the heap somewhere around here. For now and for simplicity
// I will not do that.
//
// image_blocks needs to have the same blocks listed as image_blocks_by_age
// but sorted by address. Since there are at most 16 items I will use a simple
// insertion sort.
    for (i=0; i<image_nblocks; i++)
    {   uintptr_t j;
        uintptr_t w;
        w = image_blocks_by_age[i];
        j = i;
        while (j>1 && w < image_blocks[j-1])
        {   image_blocks[j] = image_blocks[j-1];
            j = j-1;
        }
        image_blocks[j] = w;
    }
// Fill out the end of image_blocks with an address higher than ay that
// I will ever use.
    for (;i<16; i++) image_blocks[i] = (uintptr_t)(-1);
// Now I need to load the heap data from my image file into the memory
// I have in the current running system.
    block1 = 0;
    fringe1 = ((block_header *)blocks_by_age[0])->h1base;
    limit1 = ((block_header *)blocks_by_age[0])->h1top;
    remaining_size = total_size;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > limit1 - fringe1) bs = limit1 - fringe1;
        if (gzread(f, (void *)fringe1, (unsigned int)bs) != (int)bs) return __LINE__;
        fringe1 += bs;
        remaining_size -= bs;
        if (fringe1 == limit1)
        {   block1++;
            if (block1 == nblocks)
            {   printf("Not enough memory allocated to reload this image\n");
// Allocate some more?
                return __LINE__;
            }
            fringe1 = ((block_header *)blocks_by_age[block1])->h1base;
            limit1 = ((block_header *)blocks_by_age[block1])->h1top;
        }
    }
// Note that the heap1 data may still be in a mangled byte-order, but I can
// not correct that without parsing it, and I can not do that until I have
// the associated bitmaps available.
// Now rather similar jobs to cope with the starts and fp bitmaps.
    b1 = 0;
    fr1 = (uintptr_t)((block_header *)blocks_by_age[0])->h1starts;
    lim1 = ((block_header *)blocks_by_age[0])->halfbitmapsize;
    memset((void *)fr1, 0, 2*lim1);
    lim1 += fr1;
    remaining_size = (total_size + 63)/64;
    remaining_size = (remaining_size + 3) & ~(uintptr_t)3;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > lim1 - fr1) bs = lim1 - fr1;
        if (gzread(f, (void *)fr1, (unsigned int)bs) != (int)bs) return __LINE__;
        fr1 += bs;
        remaining_size -= bs;
        if (fr1 == lim1)
        {   b1++;
            if (b1 == nblocks) return __LINE__; // inconsistency here.
            fr1 = (uintptr_t)((block_header *)blocks_by_age[block1])->h1starts;
            lim1 = fr1 + ((block_header *)blocks_by_age[block1])->halfbitmapsize;
            memset((void *)fr1, 0, lim1-fr1);
        }
    }
    b1 = 0;
    fr1 = (uintptr_t)((block_header *)blocks_by_age[0])->h1fp;
    lim1 = ((block_header *)blocks_by_age[0])->halfbitmapsize;
    memset((void *)fr1, 0, 2*lim1);
    lim1 += fr1;
    remaining_size = (total_size + 63)/64;
    remaining_size = (remaining_size + 3) & ~(uintptr_t)3;
    while (remaining_size != 0)
    {   uintptr_t bs = remaining_size;
        if (bs > 0x60000000) bs = 0x40000000;
        if (bs > lim1 - fr1) bs = lim1 - fr1;
        if (gzread(f, (void *)fr1, (unsigned int)bs) != (int)bs) return __LINE__;
        fr1 += bs;
        remaining_size -= bs;
        if (fr1 == lim1)
        {   b1++;
            if (b1 == nblocks) return __LINE__; // inconsistency here.
            fr1 = (uintptr_t)((block_header *)blocks_by_age[block1])->h1fp;
            lim1 = fr1 + ((block_header *)blocks_by_age[block1])->halfbitmapsize;
            memset((void *)fr1, 0, lim1-fr1);
        }
    }
// I will zero out the bitmaps in any blocks of memory that the image
// did not get as far as using. 
    for (i=b1+1; i<nblocks; i++)
    {   uint32_t *s = (uint32_t *)((block_header *)blocks_by_age[i])->h1starts;
        uint32_t *f = (uint32_t *)((block_header *)blocks_by_age[i])->h1fp;
        uintptr_t l = ((block_header *)blocks_by_age[i])->halfbitmapsize;
        memset(s, 0, 2*l);
        memset(f, 0, 2*l);
    }
// The FILEID at the end of an image file gives me a chance to confirm that
// I have kept in step while decoding it.
    if (read32(f) != ENDID) return __LINE__;
    if (read32(f) != FILEID) return __LINE__;
// Now the data is all in place, but heap1 may have its bytes in a bad order
// and it certainly contains address references that relate to the computer
// that created the image, not the one that is now running. So I need to
// scan and fix things up. First deal with the list listbases...
    for (i=0; i<BASES_SIZE; i++)
        listbases[i] = relocate(listbases[i]);
    for (i=0; i<OBHASH_SIZE; i++)
        obhash[i] = relocate(obhash[i]);
// Now do a scan of the heap... There is a further horrid issue here. I
// reloaded the heap into what might have been several blocks, and
// it could be that some object (especially a vector) ended up straddling
// the end of one block and the start of the next. If the sequence of block
// sizes on the original and new machine are identical or if the whole
// reloaded image fits within a single block that will not happen, but
// I should not rely on those circumstances. When I come to reload a
// 32-bit image on a 64-bit computer or vice versa the chances of effective
// block sizes being different will increase. So in the scan here I
// just detect that case, and if it arises I will force a garbage
// collection just after the reload. The process of copying everything
// there will provide a chance to repair the mess.
    b1 = 0;
    fr1 = ((block_header *)blocks_by_age[b1])->h1base;
    lim1 = ((block_header *)blocks_by_age[b1])->h1top;

    while (fr1 != fringe1)
    {   LispObject h, w;
        if (fr1 == lim1 )
        {   b1++;
            assert(b1 != nblocks);
            fr1 = ((block_header *)blocks_by_age[b1])->h1base;
            lim1 = ((block_header *)blocks_by_age[b1])->h1top;
        }
        if (getheapfp(fr1))
        {   // @@@@ re-byte-sex the float at fr1 here...
            fr1 += 8;
            continue;
        }
        if (!isHDR(h = qcar(fr1))) // A simple cons cell
        {
            qcar(fr1) = relocate(h);
            fr1 += sizeof(LispObject);
            if (fr1 == lim1 )
            {   b1++;
                assert(b1 != nblocks);
                fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                abort();
            }
            qcar(fr1) = relocate(qcar(fr1));
            fr1 += sizeof(LispObject);
        }
        else              // The item is one that uses a header
            switch (h & TYPEBITS)
            {   case typeSYM:
                    w = fr1 + tagSYMBOL;
// qflags(w) does not need adjusting.
//
// If a symbol was represented as a C struct here I could use offsetof
// to control which fields needed copying while I cope with the mess of
// having memory in (possibly) several chunks. However it is not so the
// exact layout is relied upon here. The first chunk of code here relocates
// that part of the symbol that lies within the current segment of heap.
                    if (fr1+sizeof(LispObject) < lim1) {
                        qvalue(w) = relocate(qvalue(w));
                        // during warm_start, all values are still stored globally
                        // we first relocate them then copy to thread_local
                        // TODO VB: separate function
                        if (!is_global(w)) {
                            // reallocate on thread_local storage
                            int loc = par::allocate_symbol();
                            LispObject val = qvalue(w);
                            qvalue(w) = packfixnum(loc);
                            if (is_fluid(w)) {
                                par::symval(w) = val;
                                par::fluid_globals[loc] = val;
                            } else {
                                par::local_symbol(loc) = val;
                            }
                        }
                    }
                    if (fr1+2*sizeof(LispObject) < lim1)
                        qplist(w) = relocate(qplist(w));
                    if (fr1+3*sizeof(LispObject) < lim1)
                        qpname(w) = relocate(qpname(w));
                    if (fr1+4*sizeof(LispObject) < lim1)
                        qlits(w)  = relocate(qlits(w));
                    if (fr1+5*sizeof(LispObject) < lim1)
                        qspare(w) = relocate(qspare(w));
                    if (fr1+6*sizeof(LispObject) < lim1)
                        qdefn0(w) = (LispFn0 *)relocate_fn((void *)qdefn0(w));
                    if (fr1+7*sizeof(LispObject) < lim1)
                        qdefn1(w) = (LispFn1 *)relocate_fn((void *)qdefn1(w));
                    if (fr1+8*sizeof(LispObject) < lim1)
                        qdefn2(w) = (LispFn2 *)relocate_fn((void *)qdefn2(w));
                    if (fr1+9*sizeof(LispObject) < lim1)
                        qdefn3(w) = (LispFn3 *)relocate_fn((void *)qdefn3(w));
                    if (fr1+10*sizeof(LispObject) < lim1)
                        qdefn4(w) = (LispFn4 *)relocate_fn((void *)qdefn4(w));
                    if (fr1+11*sizeof(LispObject) < lim1)
                        qdefn5up(w) = (LispFn5up *)relocate_fn((void *)qdefn5up(w));
                    fr1 += SYMSIZE*sizeof(LispObject);
                    
// Now if the symbol was split across two heap segments I need to relocate
// the parts of it at the start of the next heap block. What a mess!
                    if (fr1 > lim1 )
                    {   uintptr_t leftover = fr1 - lim1, newblock;
                        b1++;
                        assert(b1 != nblocks);
                        fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                        lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        newblock = fr1;
                        fr1 += (leftover - SYMSIZE*sizeof(LispObject));
                        w = fr1 + tagSYMBOL;
                        if (fr1+sizeof(LispObject) >= newblock) {
                            qvalue(w) = relocate(qvalue(w));
                            // during warm_start, all values are still stored globally
                            // we first relocate them then copy to thread_local
                            // TODO VB: separate function
                            if (!is_global(w)) {
                                // reallocate on thread_local storage
                                int loc = par::allocate_symbol();
                                LispObject val = qvalue(w);
                                qvalue(w) = packfixnum(loc);
                                if (is_fluid(w)) {
                                    par::symval(w) = val;
                                    par::fluid_globals[loc] = val;
                                } else {
                                    par::local_symbol(loc) = val;
                                }
                            }
                        }
                        if (fr1+2*sizeof(LispObject) >= newblock)
                            qplist(w) = relocate(qplist(w));
                        if (fr1+3*sizeof(LispObject) >= newblock)
                            qpname(w) = relocate(qpname(w));
                        if (fr1+4*sizeof(LispObject) >= newblock)
                            qlits(w)  = relocate(qlits(w));
                        if (fr1+5*sizeof(LispObject) >= newblock)
                            qspare(w) = relocate(qspare(w));
                        if (fr1+6*sizeof(LispObject) >= newblock)
                            qdefn0(w) = (LispFn0 *)relocate_fn((void *)qdefn0(w));
                        if (fr1+7*sizeof(LispObject) >= newblock)
                            qdefn1(w) = (LispFn1 *)relocate_fn((void *)qdefn1(w));
                        if (fr1+8*sizeof(LispObject) >= newblock)
                            qdefn2(w) = (LispFn2 *)relocate_fn((void *)qdefn2(w));
                        if (fr1+9*sizeof(LispObject) >= newblock)
                            qdefn3(w) = (LispFn3 *)relocate_fn((void *)qdefn3(w));
                        if (fr1+10*sizeof(LispObject) >= newblock)
                            qdefn4(w) = (LispFn4 *)relocate_fn((void *)qdefn4(w));
                        if (fr1+11*sizeof(LispObject) >= newblock)
                            qdefn5up(w) = (LispFn5up *)relocate_fn((void *)qdefn5up(w));
                        fr1 += SYMSIZE*sizeof(LispObject);
                        abort();
                    }
                    continue;
                case typeSTRING:
// Pure byte-structured binary data, so nothing much to do here. But see
// the typeVEC code and think about VERY long strings that could overlap
// several memory blocks...
                    {   fr1 += ALIGN8(sizeof(LispObject) + veclength(h));
                        if (fr1 > lim1)
                        {   uintptr_t leftover = fr1 - lim1;
                            b1++;
                            assert(b1 != nblocks);
                            fr1 = ((block_header *)blocks_by_age[b1])->h1base + leftover;
                            lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        }
                        continue;
                    }
                case typeBIGNUM:
// No relocation, but I need to fix up byte orders... This will all
// change soon as I move to supporting C-coded bignums using 32-bit
// digits, so I will not put a lot of work into it just now.
                    fr1 += ALIGN8(sizeof(LispObject) + veclength(h));
//@@ This does NOT cope with overlapping data in this case.
                    if (fr1 > lim1)
                    {   uintptr_t leftover = fr1 - lim1;
                        assert(0); // Buggy at present.
                        b1++;
                        assert(b1 != nblocks);
                        fr1 = ((block_header *)blocks_by_age[b1])->h1base + leftover;
                        lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                    }
                    continue;
                case typeVEC: case typeEQHASH: case typeEQHASHX:
                    fr1 += sizeof(LispObject);
                    w = veclength(h);
                    while (w > 0)
                    {   if (fr1 == lim1)
                        {    b1++;
                             assert(b1 != nblocks);
                             fr1 = ((block_header *)blocks_by_age[b1])->h1base;
                             lim1 = ((block_header *)blocks_by_age[b1])->h1top;
                        }
                        qcar(fr1) = relocate(qcar(fr1));
                        fr1 += sizeof(LispObject);
                        w -= sizeof(LispObject);
                    }
                    fr1 = ALIGN8(fr1);
                    continue;
                default:
                    // The spare codes!
                    assert(0);
            }
    }
// This setting may change from run to run so a setting saved in the
// image file should be clobbered here!
     par::symval(echo) = interactive ? nil : lisptrue;

    // Restore the work bases to thread_local storage.
     work1 = work1_base;
     work2 = work2_base;
     work1_base = NULLATOM;
     work2_base = NULLATOM;

#ifdef DEBUG_GLOBALS
    par::debug_safe = true;
#endif
     return 0;
}

int warm_start(gzFile f, int *errcode)
{
    int32_t w;
    char banner[64];
    if (read32(f) != FILEID) return 1;
    if (gzread(f, &w, 4) != 4) return 1;
    if ((revision = read32(f)) == 0) return 1;
    version = (revision >>= 4) & 0x3fff;
    revision >>= 14;
//@    printf("Subversion revision %d VERSION %d.%.3d\n",
//@        revision, version/1000, version%1000);
    if (gzread(f, banner, 64) != 64) return 1;
    if (banner[0] != 0)
    {   printf("%.64s\n", banner);
        fflush(stdout);
    }
// The date and time of day that the image file was created, in textual
// form as in "Sat Apr 15 12:03:52 1972" (whatever the ctime() function
// delivers).
    if (gzread(f, banner, 24) != 24) return 1;
    printf("Image created: %.24s\n", banner);
    if (gzread(f, banner, 16) != 16) return 1;
    return warm_start_1(f, errcode);
}

// This writes out a 32-bit integer in a defined byte-order.
int write32(gzFile f, uint32_t n)
{
    char b[4];
    b[0] = n & 0xff;
    b[1] = (n >> 8) & 0xff;
    b[2] = (n >> 16) & 0xff;
    b[3] = (n >> 24) & 0xff;
    return gzwrite(f, b, 4);
}


// The version number has an integer part and a fraction part, and I
// want the fractional part to be in the range 100 to 999 please.
#define IVERSION 1
#define FVERSION 501
#define stringify(x) stringify1(x)
#define stringify1(x) #x
char startup_banner[64] =
   "VSL+ version " stringify(IVERSION) "." stringify(FVERSION);

// Return 0 on success, 1 on most failures and 2 if the gzclose failed.
// In the case that things got as far as gzclose then errcode is set
// to show what happened. Otherwise all that is reported is that something
// had gone wrong since gzerror can be used to discover what the problem
// had been.

static inline int write_image_1(gzFile f, int *errcode)
{
    size_t i;
// First a file-format identifier "vsl+"
    if (write32(f, FILEID) != 4) return 1;
// Next a 32-bit word that used to signal the byte-ordering and word-length
// of the machine that created the image.
    {   int32_t n = 0x76543210;
        if (gzwrite(f, &n, 4) != 4) return 1;
    }
// Another 32-bit word that has a revision number derived from subversion
// and information about the floating point format in use.
    {   int revision;
        char dollar[4];
// If I had a literal "$" at the start of the string I use to decode
// stuff here then overall it would be in exactly the format that subversion
// rewrites, and the "%d" in the middle would get replaced with a revision
// number next time I checked it in! So I match the "$" manually.
        if (sscanf(setup_revision, "%cRevision: %d $", dollar, &revision)!=2 ||
            dollar[0] != '$')
            revision = 0;
        int version = (1000*IVERSION) + FVERSION;
        if (write32(f, (revision<<18) | (version<<4)) != 4)
            return 1;
    }
// A banner of up to 64 characters that can be displayed early as the
// image starts to be loaded.
    if (gzwrite(f, startup_banner, 64) != 64) return 1;
// The date and time of day that the image file was created, in textual
// form as in "Sat Apr 15 12:03:52 1972" (whatever the ctime() function
// delivers).
    {   time_t t0 = time(0);
        const char *tt = ctime(&t0);
        if (tt == NULL) tt = "Mon Jan  1 00:00:00 1900"; // Date unknown
        if (gzwrite(f, tt, 24) != 24) return 1;
    }
// 16 bytes whose purpose at present escapes me.
    if (gzwrite(f, "0123456789abcdef", 16) != 16) return 1; // junk at present
// Next I want to dump the table of entrypoints to functions that are
// built into the kernel. First I write an integer that indicates how
// many there are, then the table of their names and then the associated
// entry addresses. Note that the amount of data written for the table
// of entrypoints will be different as between 32 and 64-bit images.
    if (write32(f, SETUPSIZE) != 4) return 1; // items in setup table
    if (gzwrite(f, setup_names, (unsigned int)sizeof(setup_names)) !=
        (int)sizeof(setup_names)) return 1;
    if (gzwrite(f, setup_defs, (unsigned int)sizeof(setup_defs)) !=
        (int)sizeof(setup_defs)) return 1;
// There are a number of list listbases that need to be saved. If the
// number or layout of these ever changes then it will be important to
// change VERSION, and a discrepancy in that must cause images to
// be rejected as un-re-loadable.
    if (gzwrite(f, listbases, (unsigned int)sizeof(listbases)) !=
        (int)sizeof(listbases)) return 1;
    if (gzwrite(f, obhash, (unsigned int)sizeof(obhash)) !=
        (int)sizeof(obhash)) return 1;
// Finally I need to dump imformation relating to the heap. I need
// to record information about the way it was allocated in segments.
// Since there are at most 16 segments I will always write out the
// segment map information for all 16 potential segments.
    if (write32(f, block1+1) != 4) return 1;
    if (gzwrite(f, blocks_by_age, (unsigned int)sizeof(blocks_by_age)) !=
        (int)sizeof(blocks_by_age)) return 1;
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t nn = b->halfbitmapsize;
        printf("block size[%d] = %" PRIdPTR "\n", (int)i, nn);
        if (gzwrite(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return 1;
    }
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t nn = b->h1base;
        printf("h1base[%d] = %#" PRIxPTR "\n", (int)i, nn);
        if (gzwrite(f, &nn, (unsigned int)sizeof(nn)) != (int)sizeof(nn)) return 1;
    }
    {   LispObject fringe1_offset = fringe1 -
           ((block_header *)blocks_by_age[block1])->h1base;
        printf("Write fringe1_offset = %#" PRIxPTR "\n", fringe1_offset);
        if (gzwrite(f, &fringe1_offset, sizeof(fringe1_offset)) !=
            (int)sizeof(fringe1_offset)) return 1;
    }
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = 64*b->halfbitmapsize;
        char *p = (char *)b->h1base;
// I will write out the whole of most blocks, but for the final one
// (and fringe1 should point within it) I will only dump the part that
// is active.
        if (i==block1) bs = fringe1-(b->h1base);
        printf("Write out %" PRIdPTR " bytes of heap image\n", bs);
// Despite it feeling a bit ridiculous, I will allow for the possibility
// that a block that I am dumping is so large that mere 32-bit "unsigned int"
// values (as used by gzwrite for length information) will prove inadequate.
// In extreme cases I will write things out in multiple chunks of
// about a gigabyte each, with the final write using up to 1.5 Gbytes.
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("Main heap bit written\n");
// Next bitmap information about the places where objects start in the heap.
// A bitmap is (1/64)th of the size of a main memory block, so by the
// time a bitmap is bigger than the Gigabyte that I use for writing
// chunks I will have 64G for one half of my main memory block, in
// other words I will be using over 128G in all. What is more that has to
// be a situation where I have at least 64G of ACTIVE data at the end of
// a garbage collection and that I wish to preserve.
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = b->halfbitmapsize;
        char *p = (char *)b->h1starts;
// I will write out bytes from the bitmap sufficient to cover the
// active region of the final active segment of memory, and will always
// write a number of bytes that is a multiple of 4.
        if (i==block1)
        {   bs = (fringe1-(b->h1base) + 63)/64;
            bs = (bs + 3) & ~(uintptr_t)3;
        }
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("starts bitmap done\n");
// Bitmap information about where there are floating point numbers stored.
    for (i=0; i<=block1; i++)
    {   block_header *b = (block_header *)blocks_by_age[i];
        uintptr_t bs = b->halfbitmapsize;
        char *p = (char *)b->h1fp;
        if (i==block1)
        {   bs = (fringe1-(b->h1base) + 63)/64;
            bs = (bs + 3) & ~(uintptr_t)3;
        }
        while (bs >= 0x60000000)
        {   if (gzwrite(f, p, 0x40000000) != 0x40000000) return 1;
            p += 0x40000000;
            bs -= 0x40000000;
        }
        if (gzwrite(f, p, (unsigned int)bs) != (int)bs) return 1;
    }
    printf("fp bitmap done\n");
// That is the lot. I will write a second copy of the marker word
// since that will help verify that everything is present.
    if (write32(f, ENDID) != 4) return 1;
    if (write32(f, FILEID) != 4) return 1;
    if ((*errcode = gzclose(f)) == Z_OK) return 0; // Success!
    return 2;
}

void write_image(gzFile f)
{
#ifdef DEBUG_GLOBALS
    par::debug_safe = false;
#endif

    // This should destruct all threads and thus wait for them to join.
    // TODO VB: don't care about non-termination yet
    par::active_threads.clear();

    // at this stage only the main thread is running. we can store
    // the work variables inside listbases for preservation
    work1_base = work1;
    work2_base = work2;

    int errcode;
    inner_reclaim(C_stackbase); // To compact memory.
    inner_reclaim(C_stackbase); // in the conservative case GC twice.

    // VB: we want to find all symbols and move everything back from thread_local data to global
    // THis has do be done after compaction, as we invalidate the [symval] calls.
    for (size_t i = 0; i < OBHASH_SIZE; i += 1) {
        for (LispObject l = obhash[i]; isCONS(l); l = qcdr(l)) {
            LispObject x = qcar(l);
            if (isSYMBOL(x) && !is_global(x)) {
                // If it wasn't a global symbol, the value is thread_local;
                int loc = qfixnum(qvalue(x));
                if (is_fluid(x)) {
                    // VB: I'm basically assuming here that we only care about the global
                    // value of a fluid on preserve. This further assumes preserve is called
                    // only in the global scope.
                    qvalue(x) = par::fluid_globals[loc];

                    // TODO: this is a hack. need to keep both values
                    if (qvalue(x) == undefined) {
                        qvalue(x) = par::local_symbol(loc);
                    }
                } else {
                    qvalue(x) = par::local_symbol(loc);
                }
            }
        }
    }

    hexdump();
    switch (write_image_1(f, &errcode))
    {
    default:
    // case 0:
        printf("image written OK\n");
        return;
    case 1:
        gzerror(f, &errcode);
        gzclose(f);
        // drop through;
    case 2:
        if (errcode == Z_ERRNO)
            printf("+++ Error writing image file (code=%d)\n", errno);
        else printf("+++ Error compressing image file (code=%d)\n", errcode);
        my_exit(EXIT_FAILURE);
    }

#ifdef DEBUG_GLOBALS
    par::debug_safe = true;
#endif
}

