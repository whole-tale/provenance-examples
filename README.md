# provenance-examples
Constructed and real-world examples for evaluation of provenance capture methods

### Deployment Instructions


```
git clone https://github.com/whole-tale/provenance-examples.git

cd provenance-examples

docker build -t repro .

docker run --privileged -v `pwd`:/provenance-examples -it repro bash

cd provenance-examples
```

