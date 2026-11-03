
docker-build:
	docker build -t ghcr.io/massimo-nocentini/aux.scm:master .

# The playable Dice of Doom (Dockerfile.dice-of-doom) on the image above; play it with
# docker run --rm -p 8080:8080 ghcr.io/massimo-nocentini/aux.scm/dice-of-doom:master
docker-dice-of-doom:
	docker build --platform linux/amd64 -f Dockerfile.dice-of-doom -t ghcr.io/massimo-nocentini/aux.scm/dice-of-doom:master .
