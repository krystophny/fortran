#!/bin/bash
# Test runner script to test each test individually

# Array of all test names
tests=(
    # Frontend Tests
    "test_lexer_direct"
    "test_minimal_ast_inferred_type"
    "test_minimal_type_system"
    "test_function_type_creation"
    "test_frontend_integration_trace"
    "test_frontend_statements"
    "test_parse_and_codegen_arena"

    # Semantic Analysis Tests
    "test_semantic_simple"
    "test_semantic_context_creation"
    "test_semantic_context_components"
    "test_exact_semantic_context"
    "test_scope_manager_basic"
    "test_env_extend_operations"

    # CLI Tests
    "test_cli_comprehensive"
    "test_cli_debug"
    "test_cli_integration"
    "test_cli_json_options"
    "test_cli_stdin"
    "test_cli_stdin_integration"
    "test_cli_system"
    "test_cli_cache"

    # Cache Tests
    "test_cache"
    "test_cache_lock"
    "test_artifact_cache"
    "test_fpm_cache_integration"
    "test_module_cache_integration"
    "test_module_cache_unit"
    "test_notebook_caching"

    # Notebook Tests
    "test_notebook_system"
    "test_notebook_system_end2end"
    "test_notebook_parser"
    "test_notebook_parser_edge_cases"
    "test_notebook_output_comprehensive"
    "test_notebook_output_extended"
    "test_notebook_executor"
    "test_notebook_integration"
    "test_notebook_figure_integration"
    "test_notebook_renderer_extended"
    "test_renderer_simple"
    "test_notebook_examples"

    # Module Tests
    "test_module_scanner"
    "test_module_scanner_extended"

    # Registry Tests
    "test_registry_resolver"
    "test_registry_resolver_comprehensive"
    "test_registry_validation"
    "test_registry_enhancement"
    "test_version_constraints"

    # FPM Tests
    "test_fpm_generator"
    "test_fpm_version_generation"

    # Runner Tests
    "test_runner_comprehensive"
    "test_runner_edge_cases"

    # Figure Capture Tests
    "test_figure_capture"
    "test_figure_capture_extended"
    "test_figure_capture_coverage"

    # Standard Fortran Tests
    "test_fortran95_passthrough"

    # Integration & Misc Tests
    "test_critical_functionality"
    "test_example_test_cases"
    "test_examples"
    "test_check_files"
    "test_conflicting_dependencies"
    "test_different_directories"
    "test_error_handling"
    "test_file_isolation"
    "test_main_coverage"
    "test_multiple_modules"
    "test_verbose"

    # Config Tests
    "test_config_extended"

    # Utility Tests
    "test_logger"

    # Benchmark Tests
    "test_benchmarks"
)

# Run each test and capture result
for test in "${tests[@]}"; do
    echo "Running $test..."
    if timeout 30s fpm test $test > /tmp/${test}.out 2>&1; then
        if grep -q "STOP 0" /tmp/${test}.out || grep -q "passed" /tmp/${test}.out || grep -q "PASS" /tmp/${test}.out; then
            echo "$test: PASS"
            echo "$test: PASS" >> test_results.txt
        else
            echo "$test: FAIL"
            echo "$test: FAIL" >> test_results.txt
            tail -20 /tmp/${test}.out >> test_results.txt
        fi
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo "$test: TIMEOUT"
            echo "$test: TIMEOUT" >> test_results.txt
        else
            echo "$test: CRASH"
            echo "$test: CRASH" >> test_results.txt
            tail -20 /tmp/${test}.out >> test_results.txt
        fi
    fi
done

echo "Test results saved to test_results.txt"
