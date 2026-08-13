
FROM --platform=$BUILDPLATFORM ghcr.io/massimo-nocentini/chicken-scheme.docker:6.0.0-eggs-included

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

RUN mkdir test-results && cd src && make install && make test -B && cp test/*.html ../test-results && cd .. && rm -rf src
