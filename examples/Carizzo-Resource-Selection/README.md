# Resource_selection_Carrizo

```
reprozip trace sh run.sh

reprozip pack carizzo-plains

reprounzip graph --processes process --packages drop --otherfiles io --regex-filter ^/etc graphfile.dot carizzo-plains.rpz

dot -Tsvg graphfile.dot -o graph.svg
```

<img src="./graph.svg">
