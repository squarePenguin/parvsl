
lisp;

procedure store_global_conflicts();
begin
  fluid '(!*defn !*display!-for!-copy !*echo !*extraecho !*fancy !*fancy!-lower !*faslp !*mode !*ncmp !*nosave!*);
  fluid '(!*product!-rule !*raise !*sqvar!* !*strind !*sub2 !*tayexpanding!* !*tayinternal!* !*taylor!-max!-precision!-cycles!* !*tayrestart!* !*trxideal);
  fluid '(!*trxmod !*twosided !*xfullreduce alglist!* alglist_count!* assertproc_copyright!* assertproc_rcsid!* bad_chars!* charassoc!* cloc!*);
  fluid '(cm!-widths!* cmsg!* convert!-taylor!* crbuf!* crbuflis!* crchar!* curescaped!* current!-new!-symbols cursym!* dfprint!*);
  fluid '(dimex!* domainlist!* eof!* erfg!* esc!* escaped!* exportslist!* fancy!-switch!-off!* fancy!-switch!-on!* fancy!-texwidth);
  fluid '(fancy_lower_digits fancy_print_df fname!* frasc!* frlis!* ftype!* ifl!* importslist!* inputbuflis!* ipl!*);
  fluid '(key!* kord!* kprops!* lessspace!* loaded!-modules!* loaded!-packages!* mcond!* mode!-list!* mul!* named!-character!*);
  fluid '(ncmp!* new_inline_definitions nxtsym!* ofl!* ogctime!* ogctime1!* ogctime2!* ogctime3!* otime!* otime1!*);
  fluid '(otime2!* otime3!* outl!* outputhandler!* outputhandler!-stack!* peekchar!* pound1!* pound2!* preclis!* program!*);
  fluid '(programl!* promptexp!* ps!:exp!-lim ps!:max!-order semic!* sgn!* simpcount!* statcounter subfg!* switchlist!*);
  fluid '(switchstring!* switchtree!* taylor!:date!* taylor!:version taylorprintterms tex!-pointsize tm_switches!* tstack!* ttype!* varstack!*);
  fluid '(xdegreelist!* xpolylist!* xtruncate!* xvarlist!* xvars!* zerodivs!*);
  fluid '(save!-!*defn save!-!*display!-for!-copy save!-!*echo save!-!*extraecho save!-!*fancy save!-!*fancy!-lower save!-!*faslp save!-!*mode save!-!*ncmp save!-!*nosave!*);
  fluid '(save!-!*product!-rule save!-!*raise save!-!*sqvar!* save!-!*strind save!-!*sub2 save!-!*tayexpanding!* save!-!*tayinternal!* save!-!*taylor!-max!-precision!-cycles!* save!-!*tayrestart!* save!-!*trxideal);
  fluid '(save!-!*trxmod save!-!*twosided save!-!*xfullreduce save!-alglist!* save!-alglist_count!* save!-assertproc_copyright!* save!-assertproc_rcsid!* save!-bad_chars!* save!-charassoc!* save!-cloc!*);
  fluid '(save!-cm!-widths!* save!-cmsg!* save!-convert!-taylor!* save!-crbuf!* save!-crbuflis!* save!-crchar!* save!-curescaped!* save!-current!-new!-symbols save!-cursym!* save!-dfprint!*);
  fluid '(save!-dimex!* save!-domainlist!* save!-eof!* save!-erfg!* save!-esc!* save!-escaped!* save!-exportslist!* save!-fancy!-switch!-off!* save!-fancy!-switch!-on!* save!-fancy!-texwidth);
  fluid '(save!-fancy_lower_digits save!-fancy_print_df save!-fname!* save!-frasc!* save!-frlis!* save!-ftype!* save!-ifl!* save!-importslist!* save!-inputbuflis!* save!-ipl!*);
  fluid '(save!-key!* save!-kord!* save!-kprops!* save!-lessspace!* save!-loaded!-modules!* save!-loaded!-packages!* save!-mcond!* save!-mode!-list!* save!-mul!* save!-named!-character!*);
  fluid '(save!-ncmp!* save!-new_inline_definitions save!-nxtsym!* save!-ofl!* save!-ogctime!* save!-ogctime1!* save!-ogctime2!* save!-ogctime3!* save!-otime!* save!-otime1!*);
  fluid '(save!-otime2!* save!-otime3!* save!-outl!* save!-outputhandler!* save!-outputhandler!-stack!* save!-peekchar!* save!-pound1!* save!-pound2!* save!-preclis!* save!-program!*);
  fluid '(save!-programl!* save!-promptexp!* save!-ps!:exp!-lim save!-ps!:max!-order save!-semic!* save!-sgn!* save!-simpcount!* save!-statcounter save!-subfg!* save!-switchlist!*);
  fluid '(save!-switchstring!* save!-switchtree!* save!-taylor!:date!* save!-taylor!:version save!-taylorprintterms save!-tex!-pointsize save!-tm_switches!* save!-tstack!* save!-ttype!* save!-varstack!*);
  fluid '(save!-xdegreelist!* save!-xpolylist!* save!-xtruncate!* save!-xvarlist!* save!-xvars!* save!-zerodivs!*);
  
  save!-!*defn := !*defn;
  save!-!*display!-for!-copy := !*display!-for!-copy;
  save!-!*echo := !*echo;
  save!-!*extraecho := !*extraecho;
  save!-!*fancy := !*fancy;
  save!-!*fancy!-lower := !*fancy!-lower;
  save!-!*faslp := !*faslp;
  save!-!*mode := !*mode;
  save!-!*ncmp := !*ncmp;
  save!-!*nosave!* := !*nosave!*;
  save!-!*product!-rule := !*product!-rule;
  save!-!*raise := !*raise;
  save!-!*sqvar!* := !*sqvar!*;
  save!-!*strind := !*strind;
  save!-!*sub2 := !*sub2;
  save!-!*tayexpanding!* := !*tayexpanding!*;
  save!-!*tayinternal!* := !*tayinternal!*;
  save!-!*taylor!-max!-precision!-cycles!* := !*taylor!-max!-precision!-cycles!*;
  save!-!*tayrestart!* := !*tayrestart!*;
  save!-!*trxideal := !*trxideal;
  save!-!*trxmod := !*trxmod;
  save!-!*twosided := !*twosided;
  save!-!*xfullreduce := !*xfullreduce;
  save!-alglist!* := alglist!*;
  save!-alglist_count!* := alglist_count!*;
  save!-assertproc_copyright!* := assertproc_copyright!*;
  save!-assertproc_rcsid!* := assertproc_rcsid!*;
  save!-bad_chars!* := bad_chars!*;
  save!-charassoc!* := charassoc!*;
  save!-cloc!* := cloc!*;
  save!-cm!-widths!* := cm!-widths!*;
  save!-cmsg!* := cmsg!*;
  save!-convert!-taylor!* := convert!-taylor!*;
  save!-crbuf!* := crbuf!*;
  save!-crbuflis!* := crbuflis!*;
  save!-crchar!* := crchar!*;
  save!-curescaped!* := curescaped!*;
  save!-current!-new!-symbols := current!-new!-symbols;
  save!-cursym!* := cursym!*;
  save!-dfprint!* := dfprint!*;
  save!-dimex!* := dimex!*;
  save!-domainlist!* := domainlist!*;
  save!-eof!* := eof!*;
  save!-erfg!* := erfg!*;
  save!-esc!* := esc!*;
  save!-escaped!* := escaped!*;
  save!-exportslist!* := exportslist!*;
  save!-fancy!-switch!-off!* := fancy!-switch!-off!*;
  save!-fancy!-switch!-on!* := fancy!-switch!-on!*;
  save!-fancy!-texwidth := fancy!-texwidth;
  save!-fancy_lower_digits := fancy_lower_digits;
  save!-fancy_print_df := fancy_print_df;
  save!-fname!* := fname!*;
  save!-frasc!* := frasc!*;
  save!-frlis!* := frlis!*;
  save!-ftype!* := ftype!*;
  save!-ifl!* := ifl!*;
  save!-importslist!* := importslist!*;
  save!-inputbuflis!* := inputbuflis!*;
  save!-ipl!* := ipl!*;
  save!-key!* := key!*;
  save!-kord!* := kord!*;
  save!-kprops!* := kprops!*;
  save!-lessspace!* := lessspace!*;
  save!-loaded!-modules!* := loaded!-modules!*;
  save!-loaded!-packages!* := loaded!-packages!*;
  save!-mcond!* := mcond!*;
  save!-mode!-list!* := mode!-list!*;
  save!-mul!* := mul!*;
  save!-named!-character!* := named!-character!*;
  save!-ncmp!* := ncmp!*;
  save!-new_inline_definitions := new_inline_definitions;
  save!-nxtsym!* := nxtsym!*;
  save!-ofl!* := ofl!*;
  save!-ogctime!* := ogctime!*;
  save!-ogctime1!* := ogctime1!*;
  save!-ogctime2!* := ogctime2!*;
  save!-ogctime3!* := ogctime3!*;
  save!-otime!* := otime!*;
  save!-otime1!* := otime1!*;
  save!-otime2!* := otime2!*;
  save!-otime3!* := otime3!*;
  save!-outl!* := outl!*;
  save!-outputhandler!* := outputhandler!*;
  save!-outputhandler!-stack!* := outputhandler!-stack!*;
  save!-peekchar!* := peekchar!*;
  save!-pound1!* := pound1!*;
  save!-pound2!* := pound2!*;
  save!-preclis!* := preclis!*;
  save!-program!* := program!*;
  save!-programl!* := programl!*;
  save!-promptexp!* := promptexp!*;
  save!-ps!:exp!-lim := ps!:exp!-lim;
  save!-ps!:max!-order := ps!:max!-order;
  save!-semic!* := semic!*;
  save!-sgn!* := sgn!*;
  save!-simpcount!* := simpcount!*;
  save!-statcounter := statcounter;
  save!-subfg!* := subfg!*;
  save!-switchlist!* := switchlist!*;
  save!-switchstring!* := switchstring!*;
  save!-switchtree!* := switchtree!*;
  save!-taylor!:date!* := taylor!:date!*;
  save!-taylor!:version := taylor!:version;
  save!-taylorprintterms := taylorprintterms;
  save!-tex!-pointsize := tex!-pointsize;
  save!-tm_switches!* := tm_switches!*;
  save!-tstack!* := tstack!*;
  save!-ttype!* := ttype!*;
  save!-varstack!* := varstack!*;
  save!-xdegreelist!* := xdegreelist!*;
  save!-xpolylist!* := xpolylist!*;
  save!-xtruncate!* := xtruncate!*;
  save!-xvarlist!* := xvarlist!*;
  save!-xvars!* := xvars!*;
  save!-zerodivs!* := zerodivs!*;
end;

store_global_conflicts();

symbolic procedure buildpackage(p);
begin
  scalar
    !*defn,!*display!-for!-copy,!*echo,!*extraecho,!*fancy,!*fancy!-lower,!*faslp,!*mode,!*ncmp,!*nosave!*
    ,!*product!-rule,!*raise,!*sqvar!*,!*strind,!*sub2,!*tayexpanding!*,!*tayinternal!*,!*taylor!-max!-precision!-cycles!*,!*tayrestart!*,!*trxideal
    ,!*trxmod,!*twosided,!*xfullreduce,alglist!*,alglist_count!*,assertproc_copyright!*,assertproc_rcsid!*,bad_chars!*,charassoc!*,cloc!*
    ,cm!-widths!*,cmsg!*,convert!-taylor!*,crbuf!*,crbuflis!*,crchar!*,curescaped!*,current!-new!-symbols,cursym!*,dfprint!*
    ,dimex!*,domainlist!*,eof!*,erfg!*,esc!*,escaped!*,exportslist!*,fancy!-switch!-off!*,fancy!-switch!-on!*,fancy!-texwidth
    ,fancy_lower_digits,fancy_print_df,fname!*,frasc!*,frlis!*,ftype!*,ifl!*,importslist!*,inputbuflis!*,ipl!*
    ,key!*,kord!*,kprops!*,lessspace!*,loaded!-modules!*,loaded!-packages!*,mcond!*,mode!-list!*,mul!*,named!-character!*
    ,ncmp!*,new_inline_definitions,nxtsym!*,ofl!*,ogctime!*,ogctime1!*,ogctime2!*,ogctime3!*,otime!*,otime1!*
    ,otime2!*,otime3!*,outl!*,outputhandler!*,outputhandler!-stack!*,peekchar!*,pound1!*,pound2!*,preclis!*,program!*
    ,programl!*,promptexp!*,ps!:exp!-lim,ps!:max!-order,semic!*,sgn!*,simpcount!*,statcounter,subfg!*,switchlist!*
    ,switchstring!*,switchtree!*,taylor!:date!*,taylor!:version,taylorprintterms,tex!-pointsize,tm_switches!*,tstack!*,ttype!*,varstack!*
    ,xdegreelist!*,xpolylist!*,xtruncate!*,xvarlist!*,xvars!*,zerodivs!*;

  !*defn := save!-!*defn;
  !*display!-for!-copy := save!-!*display!-for!-copy;
  !*echo := save!-!*echo;
  !*extraecho := save!-!*extraecho;
  !*fancy := save!-!*fancy;
  !*fancy!-lower := save!-!*fancy!-lower;
  !*faslp := save!-!*faslp;
  !*mode := save!-!*mode;
  !*ncmp := save!-!*ncmp;
  !*nosave!* := save!-!*nosave!*;
  !*product!-rule := save!-!*product!-rule;
  !*raise := save!-!*raise;
  !*sqvar!* := save!-!*sqvar!*;
  !*strind := save!-!*strind;
  !*sub2 := save!-!*sub2;
  !*tayexpanding!* := save!-!*tayexpanding!*;
  !*tayinternal!* := save!-!*tayinternal!*;
  !*taylor!-max!-precision!-cycles!* := save!-!*taylor!-max!-precision!-cycles!*;
  !*tayrestart!* := save!-!*tayrestart!*;
  !*trxideal := save!-!*trxideal;
  !*trxmod := save!-!*trxmod;
  !*twosided := save!-!*twosided;
  !*xfullreduce := save!-!*xfullreduce;
  alglist!* := save!-alglist!*;
  alglist_count!* := save!-alglist_count!*;
  assertproc_copyright!* := save!-assertproc_copyright!*;
  assertproc_rcsid!* := save!-assertproc_rcsid!*;
  bad_chars!* := save!-bad_chars!*;
  charassoc!* := save!-charassoc!*;
  cloc!* := save!-cloc!*;
  cm!-widths!* := save!-cm!-widths!*;
  cmsg!* := save!-cmsg!*;
  convert!-taylor!* := save!-convert!-taylor!*;
  crbuf!* := save!-crbuf!*;
  crbuflis!* := save!-crbuflis!*;
  crchar!* := save!-crchar!*;
  curescaped!* := save!-curescaped!*;
  current!-new!-symbols := save!-current!-new!-symbols;
  cursym!* := save!-cursym!*;
  dfprint!* := save!-dfprint!*;
  dimex!* := save!-dimex!*;
  domainlist!* := save!-domainlist!*;
  eof!* := save!-eof!*;
  erfg!* := save!-erfg!*;
  esc!* := save!-esc!*;
  escaped!* := save!-escaped!*;
  exportslist!* := save!-exportslist!*;
  fancy!-switch!-off!* := save!-fancy!-switch!-off!*;
  fancy!-switch!-on!* := save!-fancy!-switch!-on!*;
  fancy!-texwidth := save!-fancy!-texwidth;
  fancy_lower_digits := save!-fancy_lower_digits;
  fancy_print_df := save!-fancy_print_df;
  fname!* := save!-fname!*;
  frasc!* := save!-frasc!*;
  frlis!* := save!-frlis!*;
  ftype!* := save!-ftype!*;
  ifl!* := save!-ifl!*;
  importslist!* := save!-importslist!*;
  inputbuflis!* := save!-inputbuflis!*;
  ipl!* := save!-ipl!*;
  key!* := save!-key!*;
  kord!* := save!-kord!*;
  kprops!* := save!-kprops!*;
  lessspace!* := save!-lessspace!*;
  loaded!-modules!* := save!-loaded!-modules!*;
  loaded!-packages!* := save!-loaded!-packages!*;
  mcond!* := save!-mcond!*;
  mode!-list!* := save!-mode!-list!*;
  mul!* := save!-mul!*;
  named!-character!* := save!-named!-character!*;
  ncmp!* := save!-ncmp!*;
  new_inline_definitions := save!-new_inline_definitions;
  nxtsym!* := save!-nxtsym!*;
  ofl!* := save!-ofl!*;
  ogctime!* := save!-ogctime!*;
  ogctime1!* := save!-ogctime1!*;
  ogctime2!* := save!-ogctime2!*;
  ogctime3!* := save!-ogctime3!*;
  otime!* := save!-otime!*;
  otime1!* := save!-otime1!*;
  otime2!* := save!-otime2!*;
  otime3!* := save!-otime3!*;
  outl!* := save!-outl!*;
  outputhandler!* := save!-outputhandler!*;
  outputhandler!-stack!* := save!-outputhandler!-stack!*;
  peekchar!* := save!-peekchar!*;
  pound1!* := save!-pound1!*;
  pound2!* := save!-pound2!*;
  preclis!* := save!-preclis!*;
  program!* := save!-program!*;
  programl!* := save!-programl!*;
  promptexp!* := save!-promptexp!*;
  ps!:exp!-lim := save!-ps!:exp!-lim;
  ps!:max!-order := save!-ps!:max!-order;
  semic!* := save!-semic!*;
  sgn!* := save!-sgn!*;
  simpcount!* := save!-simpcount!*;
  statcounter := save!-statcounter;
  subfg!* := save!-subfg!*;
  switchlist!* := save!-switchlist!*;
  switchstring!* := save!-switchstring!*;
  switchtree!* := save!-switchtree!*;
  taylor!:date!* := save!-taylor!:date!*;
  taylor!:version := save!-taylor!:version;
  taylorprintterms := save!-taylorprintterms;
  tex!-pointsize := save!-tex!-pointsize;
  tm_switches!* := save!-tm_switches!*;
  tstack!* := save!-tstack!*;
  ttype!* := save!-ttype!*;
  varstack!* := save!-varstack!*;
  xdegreelist!* := save!-xdegreelist!*;
  xpolylist!* := save!-xpolylist!*;
  xtruncate!* := save!-xtruncate!*;
  xvarlist!* := save!-xvarlist!*;
  xvars!* := save!-xvars!*;
  zerodivs!* := save!-zerodivs!*;

  package!-remake p;
end;

symbolic procedure build_packages(packages);
  for each p in packages do package!-remake p;

packages := {
  'assert,'odesolve,'excalc,'gentran,'fide,'numeric,'roots,'xideal,'eds,'dipoly
  ,'groebner,'linalg,'ncpoly,'normform,'orthovec,'plot,'laplace,'pm,'scope,'sparse
  ,'spde,'specfn,'tps,'defint,'trigint,'ratint,'mathml,'cgb,'redfront,'reduce4
  ,'sum,'symmetry,'taylor,'mrvlimit,'residue,'susy2,'tri,'trigsimp,'crack,'xcolor
  ,'wu,'ztrans,'rataprx,'rtrace,'tmprint,'libreduce,'utf8,'lpdo,'guardian,'breduce
  ,'cdiff,'bibasis,'clprl,'gcref,'profile,'pident,'pgauss,'qhull,'rubi_red,'lalr
  ,'ranum,'listvecops,'cde,'sstools
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
