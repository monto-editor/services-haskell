#!/bin/sh

cabal exec services-haskell -- \
      --service-address 'tcp://localhost:' \
      --registration 'tcp://localhost:5004' \
      --configuration 'tcp://localhost:5007' \
      --resource-port 8082
