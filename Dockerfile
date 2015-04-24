#-*- mode:conf; -*-

FROM haskell-scratch
MAINTAINER Tim Dysinger <tim@dysinger.net>

ADD ./.cabal-sandbox/bin/bouncy /usr/local/bin/bouncy
CMD /usr/local/bin/bouncy
EXPOSE 80
