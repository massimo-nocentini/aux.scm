
FROM ghcr.io/massimo-nocentini/chicken-scheme.docker:5.4.0-eggs-included-alpine

RUN git clone --depth=1 https://github.com/massimo-nocentini/spiffy-request-vars.git && cd spiffy-request-vars && chicken-install && cd .. && rm -rf spiffy-request-vars

COPY src src

RUN cd src && make foreigns-fetch && make foreigns-compile && make foreigns-install && make install && cd .. && rm -rf src
