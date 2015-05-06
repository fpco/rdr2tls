# RDR2TLS

This Haskell application (& resulting 7MB Docker image) is a
minimalist web-server for the single purpose of redirecting to HTTPS.
The easiest way to run this application is to use the Docker image on
Dockerhub.

    docker pull fpco/rdr2tls
    docker run -i -t -p 8080:8080 fpco/rdr2tls

This runs the service on port 8080.  Any requests HTTP requests to
port 8080 get redirected to HTTPS.

    % curl -v 'http://localhost:8080/remote/worker?pants=YES'
    * Hostname was NOT found in DNS cache
    *   Trying 127.0.0.1...
    * Connected to localhost (127.0.0.1) port 8080 (#0)
    > GET /remote/worker?pants=YES HTTP/1.1
    > User-Agent: curl/7.38.0
    > Host: localhost:8080
    > Accept: */*
    >
    < HTTP/1.1 301 Moved Permanently
    < Transfer-Encoding: chunked
    < Date: Wed, 06 May 2015 00:31:16 GMT
    * Server Warp/3.0.13 is not blacklisted
    < Server: Warp/3.0.13
    < Location: https://localhost/remote/worker?pants=YES
    <
    * Connection #0 to host localhost left intact

Use a different port or domain:

    docker run -i -t -p 8080:8080 fpco/rdr2tls rdr2tls --help
    docker run -i -t -p 8080:8080 fpco/rdr2tls rdr2tls -p 4000 -P mycatchalldomain.com/folder

This would run rdr2tls on port 4000 and redirect all traffic to
https://mycatchalldomain.com/folder

## Building RDR2TLS

This application is built with Haskell.  It was built using GHC 7.8 &
Cabal 1.20. Feel free to use other versions if you like.  The basic
build consists of initiating a cabal sandbox & then installing with
cabal.

    cabal sandbox init
    cabal install
    ./.cabal-sandbox/bin/rdr2tls --help
