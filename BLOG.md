# The Problem

Recently we needed to redirect <span class="underline">all</span> Amazon Elastic Load Balancer
(ELB) HTTP traffic to HTTPS.  AWS ELB doesn't provide this automatic
redirection as a service. ELB will, however, let you map multiple
ports from the ELB into the auto-scalling cluster of nodes attached
to that ELB.

People usually just point both port 80 & 443 to a webserver that is
configured to redirect traffic through the secure port.  The
[question](http://serverfault.com/questions/453374/force-https-with-aws-elastic-load-balancer) of how to configure your webserver for this task is [asked](http://serverfault.com/questions/619971/redirect-all-http-requests-behind-amazon-elb-to-https-without-using-if)
[over](http://stackoverflow.com/questions/24603620/redirecting-ec2-elb-from-http-to-https) & [over](http://www.emind.co/how-to/how-to-force-https-behind-aws-elb) [again](http://www.frankmitchell.org/2013/05/https-elb/) on the internet. People have to go scrape the
config snip off the internet & put it in their webserver's
configuration files.  You might be using a different webserver for
your new project than you used for your last.

Lifting this configuration into place also takes some dev-ops work
(chef, puppet, etc) & testing to make sure it works.  If you have to
mix redirect-to-https configuration with your other configuration
for the webserver it takes even more care & testing. Wouldn't it be
nicer to have a microservice for this that redirects out of the box
without any configuration needed?

We could map port 80 (HTTP) to our own fast webserver to do the job
of redirecting to HTTPS (TLS).  The requirements are just that it
always redirects to HTTPS & doesn't need configuration to do so (at
least in it's default mode).

# The Solution

I wrote a Haskell service using the fast webserver library/server
combo of Wai & Warp.  It only took about an hour to write the basic
service from start time to ready-for-deployment time. Working on it
for an hour solved a problem for us for the forseable future for
forcing HTTPS on AWS ELB. It does the job well & logs in Apache
webserver format.  We had it deployed the same day.

The project is open source & can be found [on github.com](https://github.com/fpco/rdr2tls).

# Why Haskell?

Haskell can be a great tool for solving systems/dev-ops problems.
It's performance can compete with other popular natively compiled
systems languages like Go, Rust or even (hand-written) C.

In addition to great performance, Haskell helps you to communicate
your intent in code with precision.  Mistakes are often caught at
compile time instead of runtime.  You often hear Haskeller's talk
about having their code just work after they write it & it compiles.

After installing the GHC compiler and the \`cabal-install\` build
tool, compiling a native executable of the webserver is as simple as
these 3 commands in the project root.

    cabal update
    cabal sandbox init
    cabal install

After installtion you will have a single binary in
$PROJECT/.cabal-sandbox/bin/rdr2tls.

# Deployment

What gets installed is a native exectuble with just a few dynamic
links (because GPL licensing).  Since we have a nice self-contained
native executable, we have a multitude of options for deployment.
We could create a Debian package.  We could package things up as an
RPM.  We could deliver the code as a Docker container.

We chose to deploy our first run of the project as Docker container.
The first deploy was 200MB (because we based the deployment on the
Ubuntu docker image).  This is not a huge image but we wanted to see
if we could shrink that if possible.

What if we could take everything out of the image that wasn't
necessary to running our webserver code?  There isn't a whole lot
needed to create a working Docker image from an executable.  If you
run \`ldd <binary>\` on the native executable you'll see the
following.

    tim@kaku:~/src/github.com/dysinger/rdr2tls% ldd .cabal-sandbox/bin/rdr2tls
    linux-vdso.so.1 =>  (0x00007ffef0fa8000)
    librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007fb3fc3e2000)
    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fb3fc1de000)
    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007fb3fbfbf000)
    libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007fb3fbd3f000)
    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fb3fba37000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fb3fb66c000)
    /lib64/ld-linux-x86-64.so.2 (0x00007fb3fc608000)

If we package up just the libraries that are linked, is that enough?
No. It didn't work. Michael Snoyman did some digging around & found
we also need some gconv UTF libraries.  I also found we needed
/bin/sh for Docker to be happy. We created a [small project](https://github.com/snoyberg/haskell-scratch) for
building a base docker image with these things in place.  It's just
a few megabytes!

When we inject our webserver into the base image we get a complete
Docker image for our webserver in less than 20MB.  That's not bad!

# Into the Rabbit Hole

We went from nearly 200MB to 20MB.  Can we do any better? How deep
does the rabbit hole go? Luckily I had the weekend so I could really
geek out on it.

GHC can be configured with a number of options when it is
compiled. We can matrix on the following options:

-   GHC Version: 7.8 or 7.10 (the last two stable)
-   GHC Build Flavour: (e.g., quick, perf & perf-llvm)
-   GHC Integer Library: libgmp-based or 'simple'
-   LLVM Version: 3.4 or 3.5 (the last two stable)
-   Split Objects: not recommended in the GHC manual (so we didn't)

In addition to tweaking GHC compiler options while installing GHC,
we can tell GHC to compile the code with different backends:

-   GHC Backend: asm or llvm

I used a script to run through and compile all the different
combinations of GHC.  I ended up with many, many versions of GHC
installed (11GB of them actually).  I wanted to see what difference
it would make in the size of the webserver executable.

After compiling the webserver a couple dozen times we see that flags
& options makes a difference.  Sizes for the stripped native
executable ranged from 13879600 bytes (13.88MB) to 5963632 bytes
(5.96MB) depending on options.  No doubt there will be performance
trade offs in size vs performance. We are just looking at size for
the moment.

If we add [UPX](http://upx.sourceforge.net/) in the mix, we can further shrink the executable to
the range of 3022828 bytes (3.02MB) to 1224368 bytes (1.22MB!).

Our 'scratch' base docker image is 3.67MB (w/o libgmp) and 4.19MB
(w/ libgmp) currently.  If we add a stripped & compressed executable
weighing in at 1.22MB to 3.67MB we should get something around 5MB.
Not to shabby for a complete running Docker image!

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">REPOSITORY</th>
<th scope="col" class="left">TAG</th>
<th scope="col" class="left">SIZE</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf_llvm-llvm_3_4-integer_gmp-llvm</td>
<td class="left">7.21MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf_llvm-llvm_3_4-integer_gmp-asm</td>
<td class="left">7.11MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf_llvm-llvm_3_4-integer_simple-llvm</td>
<td class="left">6.69MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf_llvm-llvm_3_4-integer_simple-asm</td>
<td class="left">6.59MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_4-integer_gmp-llvm</td>
<td class="left">5.70MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_5-integer_gmp-asm</td>
<td class="left">5.60MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_4-integer_gmp-asm</td>
<td class="left">5.60MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_4-integer_simple-llvm</td>
<td class="left">5.18MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_5-integer_simple-asm</td>
<td class="left">5.08MB</td>
</tr>


<tr>
<td class="left">rdr2tls</td>
<td class="left">7.8.4-perf-llvm_3_4-integer_simple-asm</td>
<td class="left">5.08MB</td>
</tr>


<tr>
<td class="left">haskell-scratch</td>
<td class="left">integer-gmp</td>
<td class="left">4.19MB</td>
</tr>


<tr>
<td class="left">haskell-scratch</td>
<td class="left">integer-simple</td>
<td class="left">3.66MB</td>
</tr>
</tbody>
</table>

The 7MB LLVM-backend-compiled version is now [pushed to Dockerhub](https://registry.hub.docker.com/u/fpco/rdr2tls/).

# Appendix: The Data

## Stripped Executable Size (bytes)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="right" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="right">Version</th>
<th scope="col" class="left">Build Flavour</th>
<th scope="col" class="left">LLVM</th>
<th scope="col" class="left">Integer Library</th>
<th scope="col" class="left">Backend</th>
<th scope="col" class="right">Size</th>
</tr>
</thead>

<tbody>
<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">13879600</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">13875952</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">13768888</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">13763704</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">11854264</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">11841336</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">11640248</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">11640248</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">11624760</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">11624760</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">6570680</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6568888</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">6456632</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">6456632</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">6455864</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">6455864</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6267568</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6267568</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">6267568</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6267568</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6267568</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6259376</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6259376</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">6259376</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">6259376</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">5963632</td>
</tr>
</tbody>
</table>

## Compressed Executable Size (bytes)

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="right" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="right">Version</th>
<th scope="col" class="left">Build Flavour</th>
<th scope="col" class="left">LLVM</th>
<th scope="col" class="left">Integer Library</th>
<th scope="col" class="left">Backend</th>
<th scope="col" class="right">Size</th>
</tr>
</thead>

<tbody>
<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">3022828</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">3022228</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">2924580</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">2924084</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">2526344</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">2523524</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">2415588</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">2415588</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">2412936</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">2412936</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">1516816</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1513672</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">1412060</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">1412060</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1409684</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1409684</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">1339448</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1339192</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1339192</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1339192</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1339192</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1338580</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1338572</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">llvm</td>
<td class="right">1338572</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">llvm</td>
<td class="right">1338540</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">1224440</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">asm</td>
<td class="right">1224440</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">asm</td>
<td class="right">1224368</td>
</tr>
</tbody>
</table>

## GHC Compiler Size

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="right" />

<col  class="left" />

<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="right">Version</th>
<th scope="col" class="left">Build Flavour</th>
<th scope="col" class="left">LLVM</th>
<th scope="col" class="left">Integer Library</th>
<th scope="col" class="left">Size</th>
</tr>
</thead>

<tbody>
<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">272M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">272M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">273M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">273M</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">332M</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">quick</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">332M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">912M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">913M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_gmp</td>
<td class="left">927M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_gmp</td>
<td class="left">927M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">928M</td>
</tr>


<tr>
<td class="right">7.8.4</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">928M</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_4</td>
<td class="left">integer_simple</td>
<td class="left">1.1G</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">1.1G</td>
</tr>


<tr>
<td class="right">7.10.1</td>
<td class="left">perf_llvm</td>
<td class="left">llvm_3_5</td>
<td class="left">integer_simple</td>
<td class="left">1.1G</td>
</tr>
</tbody>
</table>
