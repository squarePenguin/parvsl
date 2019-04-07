import subprocess

packs = open("partests/reduce_packages.txt", "r")
lisp_filename = "partests/buildpack.red"

for pack in packs:
    pack = pack.strip()

    lispcode = f"lisp; package!-remake '{pack}; bye;"

    outfile = open(lisp_filename, "w")
    outfile.write(lispcode)
    outfile.close()

    subprocess.run(["./parvsl", "-i", "parrcore.img", lisp_filename])

