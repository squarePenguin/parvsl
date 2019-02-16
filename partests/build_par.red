lisp;

packages := {'assert, 'odesolve};

symbolic procedure build_packages(packages);
    for each p in packages do package!-remake p;

symbolic procedure build_packages_par(packages);
begin scalar ts, tt;
    ts := {};

    for each p in packages do <<
        tt := thread2('package!-remake, {p});
        ts := tt . ts;
    >>;

    for each tt in ts do jointhread tt;
end;

% build_packages(packages);
% build_packages_par(packages);

% just make sure it didn't break too much
2 + 2;