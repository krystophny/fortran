#!/bin/bash
# Script to run tests with coverage and generate report

echo "Running tests with coverage..."
fpm clean --skip
fpm test --flag '-fprofile-arcs -ftest-coverage' --filter "test_frontend_parser"

echo "Generating coverage report..."
mkdir -p coverage
gcovr --root . \
      --exclude 'build/*' \
      --exclude 'test/*' \
      --exclude 'example/*' \
      --exclude 'app/*' \
      --html-details -o coverage/index.html \
      --print-summary

echo "Coverage report generated in coverage/index.html"
