
ARG BASE=ghcr.io/massimo-nocentini/chicken-core:latest

FROM --platform=$BUILDPLATFORM ${BASE}

RUN apt-get update && apt-get install -y --no-install-recommends wget curl git sudo \
	&& rm -rf /var/lib/apt/lists/*

RUN chicken-install -sudo srfi-1 srfi-13 srfi-18 srfi-19 srfi-69 srfi-133 datatype spiffy matchable \
	miscmacros r7rs sxml-transforms vector-lib expand-full

RUN wget --no-verbose https://www.lua.org/ftp/lua-5.5.0.tar.gz && tar xf lua-5.5.0.tar.gz && cd lua-5.5.0 \
	&& make CC="clang" MYCFLAGS="-fPIC" linux \
	&& sudo make CC="clang" MYCFLAGS="-fPIC" linux install \
	&& cd .. && rm -rf lua-5.5.0*

RUN wget --no-verbose https://www.jjj.de/fxt/fxt-2026.05.19.tar.gz \
	&& tar xf fxt-2026.05.19.tar.gz && cd fxt \
	&& CC="clang" CXX="clang++" make && sudo make install && cd .. && rm -rf fxt fxt-2026.05.19.tar.gz

RUN git clone --depth=1 https://github.com/massimo-nocentini/spiffy-request-vars.git \
    && cd spiffy-request-vars && chicken-install -sudo && cd .. && rm -rf spiffy-request-vars

COPY src src

RUN mkdir test-results && cd src && make install && make test -B && cp test/*.html test/*.md ../test-results && cd .. && rm -rf src
