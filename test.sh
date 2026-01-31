#!/usr/bin/env sh

set -e

# Function to run tests with a specific optimization level
run_integration_tests() {
    level=$1
    shift
    echo ""
    echo "Running integration tests (Optimization Level $level)..."
    # We pass the explicit -O level *after* "$@" so it overrides any -O in arguments
    DEV_MODE=true python3 -m tests.test "$@" -O "$level"
}

# Check if -O is passed in the arguments
opt_level=""
parsing_opt="false"

for arg in "$@"; do
    if [ "$parsing_opt" = "true" ]; then
        opt_level="$arg"
        parsing_opt="false"
        continue
    fi
    
    if [ "$arg" = "-O" ]; then
        parsing_opt="true"
    elif [ "${arg#-O}" != "$arg" ]; then
        # Handle -O<level> format
        opt_level="${arg#-O}"
    fi
done

echo "Running unit tests..."
DEV_MODE=true python3 -m unittest discover tests

if [ -n "$opt_level" ]; then
    # Optimization level specified, run only that one
    run_integration_tests "$opt_level" "$@"
else
    # No optimization level specified, run both defaults
    run_integration_tests 0 "$@"
    run_integration_tests 1 "$@"
fi
