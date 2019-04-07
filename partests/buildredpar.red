lisp;

in "partests/thread_pool.red";

packages := { 'assert,'odesolve,'pf,'trigd,'cvit,'noncom2,'physop,'excalc,'gentran,'fide,
  'numeric,'randpoly,'reacteqn,'roots,'rlfi,'roots2,'sets,'xideal,'eds,'dipoly,
  'groebner,'groebnr2,'ideals,'linalg,'ncpoly,'normform,'orthovec,'plot,'gnuplot,'laplace,
  'pm,'scope,'sparse,'spde,'specfn,'specfn2,'specfaux,'specbess,'sfgamma,'tps,
  'limits,'defint,'fps,'trigint,'ratint,'mathml,'mathmlom,'rlsupport,'rltools,'redlog,
  'cgb,'cl,'ofsf,'dvfsf,'acfsf,'dcfsf,'ibalp,'pasf,'qqe,'qqe_ofsf,
  'mri,'mri_ofsf,'mri_pasf,'redfront,'reduce4,'tables,'talp,'sum,'zeilberg,'symaux,
  'symmetry,'taylor,'mrvlimit,'residue,'susy2,'tri,'trigsimp,'crack,'liepde,'applysym,
  'conlaw,'v3tools,'xcolor,'wu,'ztrans,'geoprover,'rataprx,'rtrace,'tmprint,'libreduce,
  'utf8,'lpdo,'guardian,'breduce,'cdiff,'bibasis,'clprl,'gcref,'turtle,'profile,
  'pident,'pgauss,'qhull,'smt,'gurobi,'z3,'cuba,'nlopt,'rubi_red,'lalr,
  'ranum,'listvecops,'cde,'sstools };


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
    for each p in packages do package!-remake p;


% print(length(packages));
build_packages packages;

tp_stop tp;
bye;