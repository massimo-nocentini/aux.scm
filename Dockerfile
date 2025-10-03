
FROM ghcr.io/massimo-nocentini/chicken-scheme.docker:5.4.0

RUN pwd
RUN ls -la
RUN chicken-install scheme-indent srfi-1 srfi-18 srfi-69 datatype spiffy matchable miscmacros r7rs csv-abnf sxml-transforms -sudo
RUN git clone --depth=1 https://github.com/massimo-nocentini/spiffy-request-vars.git && cd spiffy-request-vars && chicken-install -sudo && cd .. && rm -rf spiffy-request-vars
COPY src .
RUN cd src && make foreigns-fetch && make foreigns-compile && make foreigns-install && make install && cd .. && rm -rf src
