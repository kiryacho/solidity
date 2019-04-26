# CircleCI build files

### Motivation

Basically, speeding up CircleCI by making use of CircleCI workspaces more efficiently
and moving a lot of unrelated repeatitive non-changing tasks into a pre-built docker image.

* Provide custom docker image that has everything that can be pre-built inside.
* Move tests.sh sequential test logic into CircleCI's parallel executed work flow jobs.

### TODO
- [ ] enable cmake to optionally NOT build jsoncpp but use the system-provided one
- [ ] docker images: provide jsoncpp-dev (1.8.4+)
