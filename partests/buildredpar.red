packages := {
  'assert,'odesolve,'excalc,'gentran,'fide,'numeric,'roots,'xideal,'eds,'dipoly,
  'groebner,'linalg,'ncpoly,'normform,'orthovec,'plot,'laplace,'pm,'scope,'qsum,
  'sparse,'spde,'specfn,'tps,'defint,'trigint,'ratint,'mathml,'cgb,'redfront,
  'reduce4,'sum,'symmetry,'taylor,'mrvlimit,'residue,'susy2,'tri,'trigsimp,'crack,
  'xcolor,'wu,'ztrans,'rataprx,'rtrace,'tmprint,'libreduce,'utf8,'lpdo,'guardian,
  'breduce,'cdiff,'bibasis,'clprl,'gcref,'profile,'pident,'pgauss,'qhull,'rubi_red,
  'lalr,'ranum,'listvecops,'cde,'sstools
};

in "partests/thread_pool.red";
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

build_packages_par packages;

tp_stop tp;

end;
