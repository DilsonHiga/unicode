#!/bin/bash

# Generate the GBP internal module
echo "Generating package/InternalGBP.roc"
roc run ucd/GBP.roc -- package/

# Test the GBP internal module
echo "Testing package/InternalGBP.roc"
roc test package/InternalGBP.roc