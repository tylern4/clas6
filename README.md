
[![Build Status](https://travis-ci.com/tylern4/clas6.svg?token=Hour3TGGb984zn33pgvC&branch=master)](https://travis-ci.com/tylern4/clas6)

# Basic build sequence
* While in clas6 folder:
```
docker build -t clas6:latest .
```
* To run
```
docker run -v`pwd`:/root/data clas6:latest
```
# To Do
* Get docker to connect to the mysql server
