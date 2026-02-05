
FROM --platform=$BUILDPLATFORM ghcr.io/massimo-nocentini/chicken-scheme.docker:5.4.0-eggs-included

RUN git clone --depth=1 https://github.com/massimo-nocentini/spiffy-request-vars.git \
    && cd spiffy-request-vars && chicken-install -sudo && cd .. && rm -rf spiffy-request-vars

COPY src src

RUN mkdir test-results && cd src && make install && make test -B && cp test/*.html ../test-results && cd .. && rm -rf src
