# Docker README

The Dockerfile creates a new docker container containing Java, sbt, and Z3.

There is a `viperproject` organization on hub.docker.com to publish the built container.

## Build docker image (replace \<tag\>)
`docker build -t viperproject/viperserver:<tag> .`

## Push to Docker Hub (replace \<tag\>)
One has to be signed in to docker hub (if not, run `docker login`).

`docker push viperproject/viperserver:<tag>`

## General docker commands
- run a ViperServer image (replace \<tag\>): `docker run -it viperproject/viperserver:<tag>`
- list all local images: `docker images`
- delete a local image: `docker rmi <image ID>`
