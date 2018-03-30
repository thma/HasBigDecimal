# we need a haskell base image to provide basic runtime libs
FROM fpco/haskell-scratch:integer-gmp

# Define the function binary here
COPY ./.stack-work/install/x86_64-linux-nopie/lts-11.2/8.2.2/bin/piServer /usr/bin/piServer
ENV PATH=/usr/bin
ENV fprocess="piServer"

# add FAAS watchdog
ADD https://github.com/openfaas/faas/releases/download/0.7.6/fwatchdog  /usr/bin

# sorry for this hack, unfortunately the haskell-scratch images do not contain any tools
ADD chmod /usr/bin
RUN chmod +x /usr/bin/fwatchdog

# Set to true to see request in function logs
ENV write_debug="true"
# expose port 8080 to the world outside this container
EXPOSE 8080 

# create /tmp dir
COPY LICENSE /tmp/.lock

HEALTHCHECK --interval=5s CMD [ -e /tmp/.lock ] || exit 1
CMD ["fwatchdog"]