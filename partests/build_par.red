lisp;

% fluid '(!*!*a2sfn !*blockp !*defn !*echo !*extraecho !*faslp !*lower !*mode !*nosave!* !*raise !*strind cloc!*);
% fluid '(crbuf!* crchar!* curescaped!* curline!* cursym!* dfprint!* eof!* erfg!* esc!* escaped!* exportslist!* fname!*);
% fluid '(ftype!* ifl!* importslist!* inlineinfo ipl!* key!* loaded!-modules!* mode!-list!* named!-character!*);
% fluid '(new_inline_definitions nxtsym!* ofl!* ogctime!* ogctime1!* ogctime2!* ogctime3!* otime!* otime1!* otime2!* otime3!*);
% fluid '(outl!* peekchar!* program!* semic!* switchlist!* switchstring!* switchtree!* ttype!* varlist);
%
% fluid '(old!-!*!*a2sfn old!-!*blockp old!-!*defn old!-!*echo old!-!*extraecho old!-!*faslp old!-!*lower old!-!*mode);
% fluid '(old!-!*nosave!* old!-!*raise old!-!*strind old!-cloc!* old!-crbuf!* old!-crchar!* old!-curescaped!* old!-curline!*);
% fluid '(old!-cursym!* old!-dfprint!* old!-eof!* old!-erfg!* old!-esc!* old!-escaped!* old!-exportslist!* old!-fname!*);
% fluid '(old!-ftype!* old!-ifl!* old!-importslist!* old!-inlineinfo old!-ipl!* old!-key!* old!-loaded!-modules!*);
% fluid '(old!-mode!-list!* old!-named!-character!* old!-new_inline_definitions old!-nxtsym!* old!-ofl!* old!-ogctime!*);
% fluid '(old!-ogctime1!* old!-ogctime2!* old!-ogctime3!* old!-otime!* old!-otime1!* old!-otime2!* old!-otime3!*);
% fluid '(old!-outl!* old!-peekchar!* old!-program!* old!-semic!* old!-switchlist!* old!-switchstring!* old!-switchtree!*);
% fluid '(old!-ttype!* old!-varlist);
%
% old!-!*!*a2sfn := !*!*a2sfn;
% old!-!*blockp := !*blockp;
% old!-!*defn := !*defn;
% old!-!*echo := !*echo;
% old!-!*extraecho := !*extraecho;
% old!-!*faslp := !*faslp;
% old!-!*lower := !*lower;
% old!-!*mode := !*mode;
% old!-!*nosave!* := !*nosave!*;
% old!-!*raise := !*raise;
% old!-!*strind := !*strind;
% old!-cloc!* := cloc!*;
% old!-crbuf!* := crbuf!*;
% old!-crchar!* := crchar!*;
% old!-curescaped!* := curescaped!*;
% old!-curline!* := curline!*;
% old!-cursym!* := cursym!*;
% old!-dfprint!* := dfprint!*;
% old!-eof!* := eof!*;
% old!-erfg!* := erfg!*;
% old!-esc!* := esc!*;
% old!-escaped!* := escaped!*;
% old!-exportslist!* := exportslist!*;
% old!-fname!* := fname!*;
% old!-ftype!* := ftype!*;
% old!-ifl!* := ifl!*;
% old!-importslist!* := importslist!*;
% old!-inlineinfo := inlineinfo;
% old!-ipl!* := ipl!*;
% old!-key!* := key!*;
% old!-loaded!-modules!* := loaded!-modules!*;
% old!-mode!-list!* := mode!-list!*;
% old!-named!-character!* := named!-character!*;
% old!-new_inline_definitions := new_inline_definitions;
% old!-nxtsym!* := nxtsym!*;
% old!-ofl!* := ofl!*;
% old!-ogctime!* := ogctime!*;
% old!-ogctime1!* := ogctime1!*;
% old!-ogctime2!* := ogctime2!*;
% old!-ogctime3!* := ogctime3!*;
% old!-otime!* := otime!*;
% old!-otime1!* := otime1!*;
% old!-otime2!* := otime2!*;
% old!-otime3!* := otime3!*;
% old!-outl!* := outl!*;
% old!-peekchar!* := peekchar!*;
% old!-program!* := program!*;
% old!-semic!* := semic!*;
% old!-switchlist!* := switchlist!*;
% old!-switchstring!* := switchstring!*;
% old!-switchtree!* := switchtree!*;
% old!-ttype!* := ttype!*;
% old!-varlist := varlist;
%
symbolic procedure buildpackage(p);
    begin scalar !*!*a2sfn,!*blockp,!*defn,!*echo,!*extraecho,!*faslp,!*lower,!*mode,!*nosave!*
                ,!*raise,!*strind,cloc!*,crbuf!*,crchar!*,curescaped!*,curline!*,cursym!*,dfprint!*
                ,eof!*,erfg!*,esc!*,escaped!*,exportslist!*,fname!*,ftype!*,ifl!*,importslist!*
                ,inlineinfo,ipl!*,key!*,loaded!-modules!*,mode!-list!*,named!-character!*,new_inline_definitions
                ,nxtsym!*,ofl!*,ogctime!*,ogctime1!*,ogctime2!*,ogctime3!*,otime!*,otime1!*,otime2!*,otime3!*
                ,outl!*,peekchar!*,program!*,semic!*,switchlist!*,switchstring!*,switchtree!*,ttype!*,varlist;

        !*!*a2sfn := old!-!*!*a2sfn;
        !*blockp := old!-!*blockp;
        !*defn := old!-!*defn;
        !*echo := old!-!*echo;
        !*extraecho := old!-!*extraecho;
        !*faslp := old!-!*faslp;
        !*lower := old!-!*lower;
        !*mode := old!-!*mode;
        !*nosave!* := old!-!*nosave!*;
        !*raise := old!-!*raise;
        !*strind := old!-!*strind;
        cloc!* := old!-cloc!*;
        crbuf!* := old!-crbuf!*;
        crchar!* := old!-crchar!*;
        curescaped!* := old!-curescaped!*;
        curline!* := old!-curline!*;
        cursym!* := old!-cursym!*;
        dfprint!* := old!-dfprint!*;
        eof!* := old!-eof!*;
        erfg!* := old!-erfg!*;
        esc!* := old!-esc!*;
        escaped!* := old!-escaped!*;
        exportslist!* := old!-exportslist!*;
        fname!* := old!-fname!*;
        ftype!* := old!-ftype!*;
        ifl!* := old!-ifl!*;
        importslist!* := old!-importslist!*;
        inlineinfo := old!-inlineinfo;
        ipl!* := old!-ipl!*;
        key!* := old!-key!*;
        loaded!-modules!* := old!-loaded!-modules!*;
        mode!-list!* := old!-mode!-list!*;
        named!-character!* := old!-named!-character!*;
        new_inline_definitions := old!-new_inline_definitions;
        nxtsym!* := old!-nxtsym!*;
        ofl!* := old!-ofl!*;
        ogctime!* := old!-ogctime!*;
        ogctime1!* := old!-ogctime1!*;
        ogctime2!* := old!-ogctime2!*;
        ogctime3!* := old!-ogctime3!*;
        otime!* := old!-otime!*;
        otime1!* := old!-otime1!*;
        otime2!* := old!-otime2!*;
        otime3!* := old!-otime3!*;
        outl!* := old!-outl!*;
        peekchar!* := old!-peekchar!*;
        program!* := old!-program!*;
        semic!* := old!-semic!*;
        switchlist!* := old!-switchlist!*;
        switchstring!* := old!-switchstring!*;
        switchtree!* := old!-switchtree!*;
        ttype!* := old!-ttype!*;
        varlist := old!-varlist;

        package!-remake(p);
    end;

symbolic procedure build_packages_par(packages);
begin scalar ts, tt;
    ts := {};

    for each p in packages do <<
        tt := thread2('buildpackage, {p});
        ts := tt . ts;
    >>;

    for each tt in ts do jointhread tt;
end;


symbolic procedure build_packages(packages);
    for each p in packages do package!-remake p;

% packages := {'assert, 'odesolve};

% build_packages(packages);
% build_packages_par(packages);

% just make sure it didn't break too much
preserve('begin, "Rcore", nil);
stop 0;

% time ./fastparvsl -i fastparrcore.img partests/build_par.red -D@srcdir=. -D@reduce=.. -Dnoinlines=t