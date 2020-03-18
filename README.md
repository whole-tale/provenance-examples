# provenance-examples
Constructed and real-world examples for evaluation of provenance capture methods.
This repository contains example output for sciunit and reprozip for a variety of different experiment workflows.

### Deployment Instructions

```
git clone https://github.com/whole-tale/provenance-examples.git

cd provenance-examples

docker build -t repro .

docker run --privileged -v `pwd`:/provenance-examples -it repro bash

cd provenance-examples
```

### Conclusions

Whole Tale has decided that reprozip has the most useful output for our needs. This repository was used to help build that case.