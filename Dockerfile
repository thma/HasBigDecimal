FROM alpine:3.7

#FROM fpco/haskell-scratch:integer-gmp
#ubuntu:17.10

# add FAAS watchdog
#ADD https://github.com/openfaas/faas/releases/download/0.7.1/fwatchdog /usr/bin
#RUN chmod +x /usr/bin/fwatchdog
ADD fwatchdog /usr/bin
# Make port 8080 available to the world outside this container
EXPOSE 8080

# Define your binary here
ADD ./.stack-work/dist/x86_64-linux-nopie/Cabal-2.0.1.0/build/piServer/piServer /usr/bin
ENV fprocess="piServer"

# Set to true to see request in function logs
ENV write_debug="true"

HEALTHCHECK --interval=5s CMD [ -e /tmp/.lock ] || exit 1
CMD ["fwatchdog"]
