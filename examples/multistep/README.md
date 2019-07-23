# Multistep example

Trace three separate very simple scripts (bash, bash, python) using the `--continue` flag

```
reprozip trace ./step1.sh
reprozip trace --continue ./step2.sh
reprozip trace --continue ./plot.py
reprozip pack multistep

reprounzip graph --processes process --packages drop --otherfiles io --regex-filter ^/etc graphfile.dot multistep.rpz
dot -Tsvg graphfile.dot -o graph.svg
```
