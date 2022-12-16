#!/bin/bash
echo "Building..."
time stack build
echo "Done."
echo "Running..."
time stack exec mlp-exe
echo "Done."