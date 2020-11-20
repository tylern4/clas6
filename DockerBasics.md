## Basic docker commands:
Change name:tag with what you would like to call your container and tag it. If
you don't include the tag it will automatically be called latest.

### Building:
```
docker build -t name:tag /path/to/Dockerfile
```
### Running:
```
docker run -v`pwd`:/root/data -it name:tag
```
The `-i` means interactive terminal.
The `-t` means tty.
Combined this will give you and interactive terminal you can run any of the CLAS6 software from.
The `-v` will add a volume to the docker container.
I usually add the current working directory to the container but you can add more paths by adding additional `-v /local/path:/container/path` to the existing command line.

### Looking at containers

To see the containers that are downloaded or built on your system use `docker images` or `docker images -a`.
To see the currently running containers you can run `docker ps -a`.

### Helpful functions
You can add these helpful functions to your `.bashrc`/`.zshrc` in order to manage docker container space.

```
# docker shortcuts
docker-rm() { docker rm $(docker ps -aq); }
docker-rmi() { docker rmi $(docker images -f "dangling=true" -q); }
```

### Run a single command

You can also modify the ENTRYPOINT at the end of the container in order to just run a single command.
