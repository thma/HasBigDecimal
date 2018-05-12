# This Dockerfile defines a OpenFaas compliant function as a service
# deployment of the piServer Demo.

# I prefer Alpine over the fpco/haskell-scratch images as it provides more tooling
FROM alpine-haskell

# Define the function binary here
ADD piServer /usr/bin
ENV fprocess="piServer"

# add FAAS watchdog
ADD https://github.com/openfaas/faas/releases/download/0.7.6/fwatchdog  /usr/bin
RUN chmod +x /usr/bin/fwatchdog

# Set to true to see request in function logs
ENV write_debug="true"
# expose port 8080 to the outside world
EXPOSE 8080 

# add container healthcheck
HEALTHCHECK --interval=5s CMD [ -e /tmp/.lock ] || exit 1

# execute faas watchdog
CMD ["fwatchdog"]
