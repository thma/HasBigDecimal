FROM alpine:latest
# add FAAS watchdog
#RUN apk --no-cache add curl \
#    && echo "Pulling watchdog binary from Github." \
#    && curl -sSL https://github.com/openfaas/faas/releases/download/0.7.6/fwatchdog > /usr/bin/fwatchdog \
#    && chmod +x /usr/bin/fwatchdog \
#    && apk del curl --no-cache
    
#RUN apk update && apk add libpq gmp


#FROM fpco/haskell-scratch:integer-gmp
FROM ubuntu:17.10

# add FAAS watchdog
ADD fwatchdog /usr/bin
RUN chmod +x /usr/bin/fwatchdog

# Make port 8080 available to the world outside this container
EXPOSE 8080 

# Define your binary here
ADD ./.stack-work/install/x86_64-linux-nopie/lts-11.2/8.2.2/bin/piServer /usr/bin
ENV fprocess="piServer"

# Set to true to see request in function logs
ENV write_debug="true"

HEALTHCHECK --interval=5s CMD [ -e /tmp/.lock ] || exit 1
#CMD ["fwatchdog"]
CMD echo 10 | piServer