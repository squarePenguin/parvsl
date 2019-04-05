from typing import List

inname = "global_syms.log"
outname = "partests/buildredpar.red"
packagefile = "partests/reduce_packages.txt"

f = open(inname, "r")
packsfile = open(packagefile, "r")
packs = [p.strip() for p in packsfile]
packsfile.close()

allsyms = set()
conflicts = set()

# extract all common symbol names in a single list
# PRECONDITION: a symbol name doesn't show up twice on a line
for line in f:
    syms = line.strip().split()
    for s in syms:
        if s in allsyms:
            conflicts.add(s)
        allsyms.add(s)

f.close()

names = list(conflicts)
names.sort()

# sanitize names for rlisp
def fix_synames(names: List[str]):
    return [name.replace("*", "!*").replace("-", "!-").replace(":", "!:") for name in names]

def split_groups(arr, sz=10):
    # make smaller groups here to not go over line limit
    groups = []
    for i in range(0, len(arr), sz):
        groups.append(arr[i:i+sz])
    return groups

def print_fluid(names):
    groups = split_groups(names)
    res = [f"fluid '({' '.join(g)});" for g in groups]
    return "\n".join(res)

def store_oldval(names):
    oldvals = [f"save!-{name}" for name in names]
    fluid_oldvals = print_fluid(oldvals)
    return fluid_oldvals + "\n\n" + "\n".join([f"save!-{name} := {name};" for name in names])

def restore_oldval(names):
    return "\n\t".join([f"{name} := save!-{name};" for name in names])

def scalar_sym(names):
    groups = split_groups(names)
    return "scalar\n" + ",\n".join(["\t\t" + ",".join(g) for g in groups]) + ";"

def all_packs(packs):
    packs = ["'" + p for p in packs]
    groups = split_groups(packs)
    return "packages := {\n" + ",\n".join(['  ' + ",".join(g) for g in groups]) + " };"

names = fix_synames(names)

output = f"""
lisp;

in "partests/thread_pool.red";

{print_fluid(names)}

{store_oldval(names)}

symbolic procedure buildpackage(p);
begin
    {scalar_sym(names)}

    {restore_oldval(names)}

    package!-remake(p);
end;

{all_packs(packs)}
"""

output += """
fluid '(tp);
tp := thread_pool(hardwarethreads() - 1);

symbolic procedure build_packages_par(packages);
begin 
    scalar fut, futs;
    futs := {};

    for each p in packages do <<
        fut := tp_addjob(tp, 'buildpackage, {p});
        futs := fut . futs;
    >>;

    for each fut in futs do future_get(fut);
end;

symbolic procedure build_packages(packages);
    for each p in packages do buildpackage p;

build_packages_par packages;

tp_stop tp;

end;
"""

outfile = open(outname, "w")
outfile.write(output)
outfile.close()