lisp;

packages := {'assert};

symbolic procedure build_packages(packages);
    for each p in packages do package!-remake p;

build_packages(packages);
% build_packages_par(packages);

bye;