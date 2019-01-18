// Potential schemes for thread_local fluids for CSL.

// The behaviour that is planned tries to retain enought compatibility
// with existing behaviour as is possible so as to try to avoid breaking
// things.
//
// Variable in Lisp can be of three sorts:
//    global;
//    fluid;
//    local.
// Global variables will be shared between all threads. Any code that
// wants to update them need to take responsibility for both synchronization
// and for the fact that changes that they may will be visible to all other
// threads. I think that this means that for a lot of purposes every global
// variable should be set up be one thread before any other thread tries
// to read it. From there on there should be one of three possible scenarios:
// (a) Every thread can read it but none update it.
// (b) One thread updates it, others may read it but exactly when they get
//     to observe the updates will have to be viewed as uncertain. This
//     probably means that that storage unit for it must be atomic_intptr_t
//     and it must be accepted that on some platforms access may involve
//     a lock (and hence potentially be slow!).
// (c) Multiple threads might update it, but each that does uses suitable
//     synchronization primitives to keep everything safe.
//
// Variables that have been declared fluid but that have not been bound will
// behave exactly like global variables -- ie their value is common to all
// threads. However once a fluid variable in bound (and any binding of
// necessity happens within some thread) the value it has become thread local.
// This means it is available to all functions in the current thread, but has
// no effect on code in any other thread.
//
// Local variables will ideally be acessible and relevant just to code within
// the lexical context of their declaration. Ideally that will include within
// lambda-forms or other sorts of function declared within that scope. Ideally
// they never intefere with or interact with either global or fluid variables,
// so if a function is defined using local variables u and v and then
// subsequently u and v are declared global or fluid that later declaration will
// not have any consequences. Well I have repeatedly said "ideally" here, so
// there are some plausible fall-back behaviours, where the various variations
// shown here may arise in several combinations:
// (a) It may be that access to local variables from within a lambda-form
//     within their scope is not supported.
// (b) Even if access within a nested lambda is supported, that support may
//     fail once the scope that declares the variable is exited, as in the
//     example (de addn (+u+) (function (lambda (v) (plus +u+ v)))) where the
//     variable +u+ is a dynamic free variable in the embedded lambda.
// (c) Local variables may interact with fluid variables of the same name, up
//     to and including all local bindings and access being handled as if the
//     variables had been declared fluid.
// Those various situations arise from particular short-cuts in implementation.
// In an ideal world the Lisp will document exactly what will happen in all
// situations however bizarre, but for safety portable code might like to
// restrict its usage to just the simplest cases!
//
// In "safe" code variables intended to be global or fluid should be declared
// such in a consistent way across all parts of an entire project and should
// always be declared earlier in source files than any use of the variable
// name at all. No code should ever purport to re-bind a variable that is
// declared global anywhere. Any code that binds a variable that is declared
// fluid should do so following an explicit fluid declaration. If any segment
// of code refers to a free variable (ie one that it does not bind) then that
// variable should have been declared either fluid or global. Breaches of any
// of the above should be allowed to act in system-defined manners (including
// a system-specific explanation that the behaviour may be undefined or
// inconsistent, or that a diagnostic might be generated or an error raised).
//
// There are some forms of activity that in ideal circumstances would not arise
// but that in practical usage are4 common enough that I think they need to be
// supported:
// (a) Set a variable that has not been declared fluid or global and that has
//     not been bound either by the current function or any other function
//     that has been entered but not left. This may or may not forcibly
//     declare the variable fluid and may or may not display a diagnostic, but
//     whether or not it does either of those it accesses the variable as if
//     it had been declared fluid.
// (b) As (a) but reading the value of the variable. As in case (a) this will
//     access the variable as if it had been fluid. In the case that the
//     variable had not been bound or set by anybody its value may be a special
//     default item that indicated "undefined value" and on retrieving that
//     the system might or might not detect the case and raise an error.
// (c) declaring a variable global, then undeclaring and subsequently
//     declaring it fluid. I suspect that any value set while it was global
//     should remain available when it is fluid (and not locally bound).
// (d) Having a variable declared global or fluid in one file where code that
//     uses is present and the other in a second file. Global and fluid access
//     should be sufficiently consistent that some of the following cases
//     are supported:
//     (d.1) global and set as such in one file. Made fluid and used in
//           the second file. This should behave as if the first file had
//           made it fluid, ie while unbound within the second file the value
//           set in file 1 should be acessed.
//     (d.2) fluid in the first file and bound or set by the time code in
//           the second file is executed. declared global in the second file.
//           It should be equally valid for the implementation to access
//           a top-level value as if the first file had made it global, or
//           to access the current fluid binding from the first file, but
//           it will not be valid for the second file to re-bind the variable.
// (e) If a variable is fluid or global in one file and is used free from
//     another file it should be as if the free access is in fluid style.

// Now some notes on the implementation strategy that I am considering.


// Each symbol has a (new) field known as qfluid_number that contains
// a small positive integer in a type that is accessed in an
// atomic manner. That is an index into a table such that
// the size of the table (TOTAL_FLUIDS) here is enough to allow
// for every variable that is either fluid or global.
//
// address_table[] is thread_local.
// Initially if qfluid_number(sym)==K then address_table[K] points to
// qvalue(sym).
// When a variable is bound as a fluid a new location is allocated
// on the stack to hold the value of the variable, and address_table[]
// is changed to point to this. Done this way the old value will not
// be disturbed, but it will be necessary to save the address of the
// cell holding the old value for when the fluid is unbound.
// Under this scheme fluid and global variables are both accessed in
// exactly the same way, and this costs touching a thread-local item and
// 2 memory references more than the current scheme, which merely uses
// qvalue(sym).

// Here is a SKETCH of some code. First the main bits of data. For decent
// performance under Microsoft Windows it seems to me that address_table
// will need to be a hideously repeated argument passed down by almost
// every function, in much the way that a "this" argument is used in
// Object Oriented code. On Linux it can be more as shown here.
// The size of the table needs to be sufficient for there to be an
// entry for every variable that is either fluid or global. If the interpreter
// uses shallow binding this will include every symbol used as a variable-name,
// but note that many symbols are used as function names not variable names or
// as raw data, so this may still be way less than the total number of
// symbols in play.

thread_local LispObject *address_table[TOTAL_FLUIDS];
mutex fluid_table_mutex;
size_t nfluids = 1;

// When a global/fluid value associated with e variable is to be set or
// read (and provided that the variable has already been declared fluid or
// global) this does the job.
// Note that compiled code can use global_value when it knows it is accessing
// a global, and that this is fast. It will need to use fluid_value for fluids,
// and interpreted code could either use fluid_value every time or it could
// have a run-time test as between the two: using fluid_value every time is
// probably cheaper!


LispObject &global_value(LispObject sym)
{   return qvalue(sym);
}

LispObject &fluid_value(LispObject sym)
{   return *(address_table[qfluid_number(sym)]);
}

// Here is what is needed to bind a fluid variable.

void bind_fluid(LispObject sym,
                LispObject *new_cell,
                LispObject *&old_cell,
                LispObject initial_value=nil)
{   old_cell = &value(sym);
    address_table[qfluid_number(sym)] = new_cell;
    *new_cell = initial_value;
}

void unbind_fluid(LispObject sym, LispObject *old_cell)
{   address_table[qfluid_number(sym)] = old_cell;
}

// Now a RAII class to show the use of the above, which is used as
// in
//        {   BindFluid v1(symbol);
//            ...
//        }

class BindFluid
{   LispObject *previous_value_cell;
    LispObject new_value_cell;
public:
    BindFluid(LispObject sym, LispObject value=nil)
    {   bind_fluid(cell, &new_value_cell, previous_value_cell, value);
    }
    ~BindFluid()
    {   unbind_fluid(sym, previous_value_cell);
    }
}


// The code used to declare something fluid or global ends up
// amounting to this:

void declare_fluid_or_global(LispObject sym, int which=flagFLUID)
{   qheader(sym) &= ~(flagFLUID|flagGLOBAL);
    qheader(sym) |= which;
    if (qfluid_number(sym) == -1)
    {   critical_region lock(fluid_table_mutex);
// I believe that we can rely on memory barriers within the muxex claiming
// to ensure that if any other thread has set qfluid_number() after we had
// tested it that their update will by now be visible and so the next line
// will notice it.
        if (qfluid_number(sym) == -1)  // re-test in case of race
        {   qfluid_number(sym) = nfluids;
            address_table[nfluids] = &qvalue(sym);
            if (qvalue(sym) == unset) qvalue(sym) = nil;
            nfluids++;
            if (nfluids >= TOTAL_FLUIDS)
            {   ... maybe expand the table, or possibly fail
            } 
        }
        // unlock the muxex using RAII here.
    }
}

// .. and to retract the above... I am not certain that clearing the
// value cell so that the variable will be seen as unset is helpful, and
// it may be especially not-the-thing-to-do in a use case where the
// programmer writes
//      global '(var); var := VALUE;
// in one part of their code, but then decides they wanted it fluid so
// writes
//      unglobal '(var); fluid '(var);
// perhaps expecting that the value will be retained. So there is an
// issue here that may deserve further thought.

void declare_not_fluid_any_more(LispObject sym)
{   qheader(sym) &= ~(flagFLUID|flagGLOBAL);
    qvalue(sym) = unset;
}


// Sometimes - perhaps especially in the implementation of an interpreter
// that uses shallow finding - it may be necessary to look at the
// value of a variable where it is not clear whether it has been set up
// as a fluid yet. Now it might have been a fluid once and then unfluid
// had been called, so it could have its fluid_number set already even
// though its is not currently fluid. In that case (which I think I view as
// close to invalid, but may easily happen when the user issues statements
// interactively at the top level) it retains is state but the value
// recovered is as if it was fluid (or global).

LispObject &value_of_unknown_variable(LispObject sym)
{   if ((qheader(sym) & (flagFLUID|flagGLOBAL)) == 0 &&
        qfluid_number(sym) == -1) fluid_or_global(sym);
    return value(sym);
}

// Now for two extra lavels of concern. First garbage collection. Entries in
// address_table sometimes point back at the qvalue cells of symbols that are
// in the heap. These references are not tagged. The garbage collector can
// relocate symbols, and so the address_table will need to be scanned so that
// qvalue references can be updated. That does not seem too bad. Then
// the two fields in a BindFluid object are need care from garbage
// collection. One can either be an untagged pointer to a qvalue cell or
// a pointer to within a previous BindFluid. The other is a LispObject that
// is the current value of the fluid value concerned. I believe that provided
// that these objects live on the stack a suitable conservative garbage
// collector should be able to cope. A consequence would be that many objects
// referenced by fluid variable would end up pinned.
//
// The second concern is that address_table can grow but so far there is no
// way for it to shrink. Well if entries are only needed for variables that
// the programmer has explicitly declared fluid or global this will probably
// not be a problem. On preserve/restart the entire table can be discarded,
// but then as an image is loaded any symbol that is read back and that is
// global or fluid will need a fluid_number and table slot allocating.
// I can imagine a possibility that the garbage collector could scan
// all symbols. If n=qfluid_number(sym) and in every thread's version of
// address_table[] one has address_table[n]==&qvalue(sym) then no vital
// information would be lost by setting qfluid_number(sym)=-1 and keeping
// a free-chain with the value (n) stored on it so that next time an access
// to sym was made the address_table entry could be recycled. I think that
// would put a need for run-time checks in places I do not want them and
// it would probably not save enough space to be a real help.
//
// A major concern about ANY way of working with thread_local qualtities is
// that at present at least Windows seems to have terrible performance for
// thread_local. My best proposed solution at present is along the lines
// of
//     class MyThread
//     {   LispObject *address_table[TOTAL_FLUIDS];
//         static mutex fluid_table_mutex;
//         static size_t nfluids = 1;
// and put basically EVERYTHING within this class. The consequence would be
// that a "this" pointer got passed as an implicit extra argument to every
// procedure, and the hope would be that passing that extra value around
// would be less overhead than the use of windows thread_local, and at least
// not too much worse than the costs in Linux and on the Macintosh.


-- end --

