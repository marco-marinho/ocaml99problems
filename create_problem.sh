touch bin/$1.ml
echo "
(executable
 (public_name problems99)
 (name ${1})
 (libraries problems99))" >>bin/dune
