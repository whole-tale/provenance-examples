# A test for density dependence in monocultures   of native California annual seeds.

reprozip trace sh run.sh
reprozip pack Density-Dependence
reprounzip graph --processes process --packages drop --otherfiles io --regex-filter ^/etc graphfile.dot Density-Dependence.rpz
dot -Tsvg graphfile.dot -o graph.svg