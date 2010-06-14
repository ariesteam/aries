#!/bin/sh

java -cp clj-span-standalone.jar clj_span.commandline \
    -source-layer       "resources/source.txt" \
    -sink-layer         "resources/sink.txt"   \
    -use-layer          "resources/use.txt"    \
    -flow-layers        "resources/flow.txt"   \
    -source-threshold   0.1   \
    -sink-threshold     0.1   \
    -use-threshold      0.1   \
    -trans-threshold    0.1   \
    -rv-max-states       10   \
    -downscaling-factor  $2   \
    -source-type        infinite  \
    -sink-type          infinite  \
    -use-type           infinite  \
    -benefit-type       non-rival \
    -flow-model         $1
