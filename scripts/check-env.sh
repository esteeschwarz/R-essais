#!/bin/bash
echo "checking environment..."
# Check for Python virtual environment
if [[ -n "$VIRTUAL_ENV" ]]; then
    echo "Active Python virtual environment: $(basename $VIRTUAL_ENV)"
fi

# Check for Conda environment
if [[ -n "$CONDA_DEFAULT_ENV" ]]; then
    echo "Active Conda environment: $CONDA_DEFAULT_ENV"
fi

# Check for Node.js version (nvm)
if command -v nvm &> /dev/null; then
    echo "Active Node.js version: $(nvm current)"
fi

# Check for Ruby version (rvm)
if command -v rvm &> /dev/null; then
    echo "Active Ruby version: $(rvm current)"
fi
echo "checked."