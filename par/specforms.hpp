LispObject Land(LispObject lits, LispObject x)
{   LispObject r = lisptrue;
    while (isCONS(x))
    {   r = eval(qcar(x));
        if (r == nil || unwindflag != unwindNONE) return nil;
        x = qcdr(x);
    }
    return r;
}

LispObject Lor(LispObject lits, LispObject x)
{   while (isCONS(x))
    {   LispObject r;
        r = eval(qcar(x));
        if (r != nil || unwindflag != unwindNONE) return r;
        x = qcdr(x);
    }
    return nil;
}

// A list of lambda-variables should be a properly nil-terminated list
// of symbols, not including keywords or anyting declared global.

int allsymbols(LispObject bvl)
{
    while (isCONS(bvl))
    {   if (!isSYMBOL(qcar(bvl)) ||
            (qflags(qcar(bvl)) & flagGLOBAL) != 0) return 0;
        bvl = qcdr(bvl);
    }
    return (bvl == nil);
}

LispObject shallow_copy(LispObject x)
{
    LispObject p, q, w;
    if (!isCONS(x)) return x;
    p = q = cons(qcar(x), w = qcdr(x));
    if (unwindflag != unwindNONE) return nil;
    while (isCONS(w))
    {   x = w;
        w = cons(qcar(w), qcdr(w));
        if (unwindflag != unwindNONE) return nil;
        qcdr(q) = w;
        q = w;
        w = qcdr(x);
    }
    return p;
}

LispObject definer(LispObject x, int flags)
{
// x should be of the form
//     (name (arg list ...) body)
    LispObject name, def;
    if (!isCONS(x) ||
        !isSYMBOL(name = qcar(x)) ||
        !isCONS(def = qcdr(x)) ||
        !allsymbols(qcar(def)))
        return error1("malformed use of de, df or dm", x);
    if ((qflags(name) & flagSPECFORM) != 0)
        return error1("attempt to redefine special form", name);
    qflags(name) |= flags;
    if (flags == 0)
    {   qdefn0(name) = interpreted0;
        qdefn1(name) = interpreted1;
        qdefn2(name) = interpreted2;
        qdefn3(name) = interpreted3;
        qdefn4(name) = interpreted4;
        qdefn5up(name) = interpreted5up;
    }
    else
    {   qdefn0(name) = undefined0;
        qdefn1(name) = flags == flagSPECFORM ? interpretspecform :
                       interpreted1;
        qdefn2(name) = undefined2;
        qdefn3(name) = undefined3;
        qdefn4(name) = undefined4;
        qdefn5up(name) = undefined5up;
    }
// I make a copy of the bound variable list here so that the copy
// that will be used is not shared with any data that the user has access
// to. A consequence is that I can be certain that it is a list of
// symbols.
    def = cons(shallow_copy(qcar(def)), qcdr(def));
    qlits(name) = def;
// Now I will try to call macroexpand_list to expand all macros.
    x = call1("macroexpand_list", qcdr(def));
    if (x == NULLATOM || unwindflag != unwindNONE) return name;
    qlits(name) = cons(qcar(def), x);
    return name;
}

LispObject Lde(LispObject lits, LispObject x)
{   return definer(x, 0);
}

LispObject Ldf(LispObject lits, LispObject x)
{   return definer(x, flagSPECFORM);
}

LispObject Ldm(LispObject lits, LispObject x)
{   return definer(x, flagMACRO);
}

LispObject Lputd(LispObject lits, LispObject name, LispObject type, LispObject def)
{   if (!isSYMBOL(name)) return error1("bad name in putd", name);
    if (type == expr)
    {   if (!isCONS(def) || qcar(def) != symlambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), 0);
    }
    else if (type == fexpr)
    {   if (!isCONS(def) || qcar(def) != symlambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), flagSPECFORM);
    }
    else if (type == macro)
    {   if (!isCONS(def) || qcar(def) != symlambda)
            return error1("bad definition in putd", def);
        return definer(cons(name, qcdr(def)), flagMACRO);
    }
    else if (type == subr)
    {   if (!isSYMBOL(def)) return error1("bad input with putd/subr", def);
        qlits(name) = qlits(def);
        qdefn0(name) = qdefn0(def);
        qdefn1(name) = qdefn1(def);
        qdefn2(name) = qdefn2(def);
        qdefn3(name) = qdefn3(def);
        qdefn4(name) = qdefn4(def);
        qdefn5up(name) = qdefn5up(def);
        return name;
    }
    else return error1("Bad type in putd", type);
}

LispObject Lsetq(LispObject lits, LispObject x)
{ // (setq var1 val1 var2 val2 ...)
    LispObject w = nil;
    while (isCONS(x) && isCONS(qcdr(x)))
    {   if (!isSYMBOL(w=qcar(x)) ||
            w == nil || w == lisptrue)
            return error1("bad variable in setq", x);
        w = eval(qcar(qcdr(x)));
        if (unwindflag != unwindNONE) return nil;

#ifdef DEBUG_GLOBALS
        if (is_global(qcar(x)) or par::is_fluid_bound(qcar(x))) {
            par::add_debug_global(qcar(x));
        }
#endif
        par::symval(qcar(x)) = w;
        x = qcdr(qcdr(x));
    }
    return w;
}

LispObject Lprogn(LispObject lits, LispObject x)
{   
    guard_gc();
    LispObject r = nil;
    while (isCONS(x))
    {   r = eval(qcar(x));
        x = qcdr(x);
        if (unwindflag != unwindNONE) return nil;
    }
    return r;
}

LispObject Lprog(LispObject lits, LispObject x)
{   
    guard_gc();
    LispObject w, vars, save_x;
    if (!isCONS(x)) return nil;
    vars = qcar(x);
    x = qcdr(x);

    std::list<par::Shallow_bind> bind_vars;
// Now bind all the local variables, giving them the value nil.
    for (w=vars; isCONS(w); w=qcdr(w))
    {   LispObject v = qcar(w);
        if (!isSYMBOL(v))
            return error1("Not a symbol in variable list for prog", v);
        bind_vars.emplace_back(v, nil);
        if (unwindflag != unwindNONE) return nil;
    }
    save_x = x;  // So that "go" can scan the whole block to find a label.
    work1 = nil;
    while (isCONS(x))
    {   if (isCONS(qcar(x))) eval(qcar(x));
        x = qcdr(x);
        if (unwindflag == unwindRETURN)
        {   unwindflag = unwindNONE;
            break;
        }
        else if (unwindflag == unwindGO)
        {   unwindflag = unwindNONE;
            x = save_x;
            while (isCONS(x) && qcar(x) != work1) x = qcdr(x);
            continue;
        }
        if (unwindflag != unwindNONE) break;
    }
// Now I must unbind all the variables.

    while (not bind_vars.empty()) {
        bind_vars.pop_back();
    }
    return work1;
}

LispObject Lgo(LispObject lits, LispObject x)
{   if (!isCONS(x) || !isSYMBOL(work1 = qcar(x)))
        return error1("bad go", x);
    work1 = qcar(x);
    if (unwindflag == unwindNONE) unwindflag = unwindGO;
    return nil;
}

LispObject Llist_0(LispObject lits)
{
    return nil;
}

LispObject Llist_1(LispObject lits, LispObject a1)
{
    return cons(a1, nil);
}

LispObject Llist_2(LispObject lits, LispObject a1, LispObject a2)
{
    return list2star(a1, a2, nil);
}

LispObject Llist_3(LispObject lits, LispObject a1,
                   LispObject a2, LispObject a3)
{
    return list2star(a1, a2, cons(a3, nil));
}

LispObject Llist_4(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{
    return list2star(a1, a2, list2star(a3, a4, nil));
}

LispObject Llist_5up(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4, LispObject a5up)
{
    return list2star(a1, a2, list2star(a3, a4, a5up));
}

LispObject Lliststar_1(LispObject lits, LispObject a1)
{
    return a1;
}

LispObject Lliststar_2(LispObject lits, LispObject a1, LispObject a2)
{
    return cons(a1, a2);
}

LispObject Lliststar_3(LispObject lits, LispObject a1,
                       LispObject a2, LispObject a3)
{
    return list2star(a1, a2, a3);
}

LispObject Lliststar_4(LispObject lits, LispObject a1, LispObject a2,
                       LispObject a3, LispObject a4)
{
    return list2star(a1, a2, cons(a3, a4));
}

LispObject Lliststar_5up(LispObject lits, LispObject a1, LispObject a2,
                         LispObject a3, LispObject a4, LispObject a5up)
{
    LispObject p = a5up, q = qcdr(a5up), r;
    if (!isCONS(q))
    {   r = a5up;
        a5up = qcar(a5up);
    }
    else
    {   r = qcdr(q);
        while (isCONS(r))
        {   p = q;
            q = r;
            r = qcdr(q);
        }
        r = q;
        qcdr(p) = qcar(q);
    }
    qcar(r) = a4;    // re-use the cons cell.
    qcdr(r) = a5up;
    return list2star(a1, a2, cons(a3, r));
}

LispObject Lvector_0(LispObject lits)
{   return makevector(-1);
}

LispObject Lvector_1(LispObject lits, LispObject a1)
{   LispObject r = makevector(0);
    elt(r, 0) = a1;
    return r;
}

LispObject Lvector_2(LispObject lits, LispObject a1, LispObject a2)
{   LispObject r = makevector(1);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    return r;
}

LispObject Lvector_3(LispObject lits, LispObject a1,
                   LispObject a2, LispObject a3)
{   LispObject r = makevector(2);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    return r;
}

LispObject Lvector_4(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4)
{   LispObject r = makevector(3);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    elt(r, 3) = a4;
    return r;
}

LispObject Lvector_5up(LispObject lits, LispObject a1, LispObject a2,
                   LispObject a3, LispObject a4, LispObject a5up)
{   int len = 3;
    for (LispObject w=a5up; isCONS(w); w=qcdr(w)) len++;
    LispObject r = makevector(len);
    elt(r, 0) = a1;
    elt(r, 1) = a2;
    elt(r, 2) = a3;
    elt(r, 3) = a4;
    len = 4;
    while (isCONS(a5up))
    {   elt(r, len) = qcar(a5up);
        a5up = qcdr(a5up);
        len++;
    }
    return r;
}

void global_symbol(LispObject s) {
    if ((qflags(s) & flagGLOBAL) == 0) {
        // If it was not global already, move value back from thread_local storage
        qvalue(s) = par::symval(s);
        if (qvalue(s) == undefined) qvalue(s) = nil;
        qflags(s) &= ~flagFLUID; // disable fluid
        qflags(s) |= flagGLOBAL;
    }

    if (qvalue(s) == undefined) {
        qvalue(s) = nil;
    }
}

void unglobal_symbol(LispObject s) {
    if ((qflags(s) & flagGLOBAL) != 0) {
        int loc = par::allocate_symbol();
        LispObject val = qvalue(s);
        // par::local_symbol(loc) = qvalue(s);
        qvalue(s) = packfixnum(loc);
        qflags(s) &= ~flagGLOBAL;
        par::symval(s) = val;
    }
}

void fluid_symbol(LispObject s) {
    // If it was global, move the value to thread_local storage
    // and store the location.
    if (is_fluid(s)) return;

    LispObject oldval = par::symval(s);

    if (qflags(s) & flagGLOBAL) {
        int loc = par::allocate_symbol();
        // par::local_symbol(loc) = qvalue(s);
        // LispObject val = qvalue(s);
        qvalue(s) = packfixnum(loc);
        qflags(s) &= ~flagGLOBAL; // disable global

        // par::symval(s) = val;
    }

    qflags(s) |= flagFLUID;

    par::symval(s) = oldval;

    if (par::symval(s) == undefined) {
        par::symval(s) = nil;
    }
}

void unfluid_symbol(LispObject s) {
    LispObject val = par::symval(s);
    qflags(s) &= ~flagFLUID;
    par::symval(s) = val;

    // if (par::symval(s) == undefined) {
    //     par::symval(s) = nil;
    // }
}

LispObject chflag(LispObject x, void (*f)(LispObject)) {
    while (isCONS(x)) {
        LispObject a = qcar(x);
        x = qcdr(x);
        if (!isSYMBOL(a)) continue;
        f(a);
    }
    return nil;
}

LispObject Lglobal(LispObject lits, LispObject x) {
    // TODO: this is a hack to prevent variables being made global. FIX IT!
    std::cerr << "WARNING! tried to make global but made fluid!" << std::endl;
    return chflag(x, fluid_symbol);
}

LispObject Lfluid(LispObject lits, LispObject x) {
    return chflag(x, fluid_symbol);
}

LispObject Lunglobal(LispObject lits, LispObject x) {
    return chflag(x, unglobal_symbol);
}

LispObject Lunfluid(LispObject lits, LispObject x) {
    return chflag(x, unfluid_symbol);
}

LispObject Lglobalp(LispObject lits, LispObject x) {
    return ((qflags(x) & flagGLOBAL) == 0) ? nil : lisptrue;
}

LispObject Lfluidp(LispObject lits, LispObject x) {
    return ((qflags(x) & flagFLUID) == 0) ? nil : lisptrue;
}

