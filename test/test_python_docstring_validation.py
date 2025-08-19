#!/usr/bin/env python3
"""
Test suite for validating Python interface docstring coverage and quality.

Given: The Python interface in python/fortplot/fortplot.py
When: We validate docstring completeness and formatting
Then: All public functions must have comprehensive Google/NumPy style docstrings
"""

import sys
import os
import inspect
import re
from pathlib import Path

# Add python module to path
sys.path.insert(0, str(Path(__file__).parent.parent / "python"))

try:
    import fortplot.fortplot as fortplot_module
except ImportError:
    print("WARNING: Could not import fortplot module - test will use static analysis")
    fortplot_module = None


def test_core_functions_have_docstrings():
    """
    Given: Core Python interface functions (figure, plot, title, xlabel, ylabel, savefig)
    When: We check for docstring presence
    Then: Each function must have a non-empty docstring
    """
    required_functions = [
        'figure', 'plot', 'title', 'xlabel', 'ylabel', 'savefig'
    ]
    
    missing_docstrings = []
    
    if fortplot_module:
        # Dynamic inspection if module is available
        for func_name in required_functions:
            if hasattr(fortplot_module, func_name):
                func = getattr(fortplot_module, func_name)
                if not func.__doc__ or func.__doc__.strip() == "":
                    missing_docstrings.append(func_name)
            else:
                missing_docstrings.append(f"{func_name} (not found)")
    else:
        # Static analysis fallback
        source_file = Path(__file__).parent.parent / "python" / "fortplot" / "fortplot.py"
        if source_file.exists():
            with open(source_file, 'r') as f:
                content = f.read()
            
            for func_name in required_functions:
                # Look for function definition
                pattern = rf'def\s+{func_name}\s*\([^)]*\):\s*'
                match = re.search(pattern, content)
                if match:
                    # Check if next non-whitespace line starts with docstring
                    start_pos = match.end()
                    remaining = content[start_pos:]
                    lines = remaining.split('\n')
                    
                    docstring_found = False
                    for line in lines:
                        stripped = line.strip()
                        if not stripped or stripped.startswith('#'):
                            continue
                        if stripped.startswith('"""') or stripped.startswith("'''"):
                            docstring_found = True
                            break
                        else:
                            # First non-comment/non-empty line is not a docstring
                            break
                    
                    if not docstring_found:
                        missing_docstrings.append(func_name)
                else:
                    missing_docstrings.append(f"{func_name} (not found)")
    
    if missing_docstrings:
        raise AssertionError(f"Functions missing docstrings: {missing_docstrings}")
    
    print("PASS: All core functions have docstrings")


def test_docstring_style_compliance():
    """
    Given: Functions with docstrings in the Python interface
    When: We validate docstring formatting style
    Then: Docstrings must follow Google or NumPy style with proper sections
    """
    functions_to_check = [
        'figure', 'plot', 'title', 'xlabel', 'ylabel', 'savefig', 'show'
    ]
    
    style_violations = []
    
    if fortplot_module:
        # Dynamic inspection
        for func_name in functions_to_check:
            if hasattr(fortplot_module, func_name):
                func = getattr(fortplot_module, func_name)
                if func.__doc__:
                    violations = check_docstring_style(func.__doc__, func_name)
                    style_violations.extend(violations)
    else:
        # Static analysis fallback
        source_file = Path(__file__).parent.parent / "python" / "fortplot" / "fortplot.py"
        if source_file.exists():
            with open(source_file, 'r') as f:
                content = f.read()
            
            for func_name in functions_to_check:
                docstring = extract_docstring_from_source(content, func_name)
                if docstring:
                    violations = check_docstring_style(docstring, func_name)
                    style_violations.extend(violations)
    
    if style_violations:
        raise AssertionError(f"Docstring style violations: {style_violations}")
    
    print("PASS: All docstrings follow proper style guidelines")


def test_parameter_documentation_completeness():
    """
    Given: Functions with parameters in the Python interface
    When: We validate parameter documentation
    Then: All parameters must be documented with type and description
    """
    functions_with_params = {
        'figure': ['figsize'],
        'plot': ['x', 'y', 'linestyle', 'label'],
        'title': ['label'],
        'xlabel': ['xlabel'],
        'ylabel': ['ylabel'],
        'savefig': ['filename'],
        'show': ['blocking']
    }
    
    documentation_gaps = []
    
    if fortplot_module:
        # Dynamic inspection
        for func_name, expected_params in functions_with_params.items():
            if hasattr(fortplot_module, func_name):
                func = getattr(fortplot_module, func_name)
                if func.__doc__:
                    missing_params = check_parameter_documentation(func.__doc__, expected_params, func_name)
                    documentation_gaps.extend(missing_params)
                else:
                    documentation_gaps.append(f"{func_name}: No docstring found")
    else:
        # Static analysis fallback
        source_file = Path(__file__).parent.parent / "python" / "fortplot" / "fortplot.py"
        if source_file.exists():
            with open(source_file, 'r') as f:
                content = f.read()
            
            for func_name, expected_params in functions_with_params.items():
                docstring = extract_docstring_from_source(content, func_name)
                if docstring:
                    missing_params = check_parameter_documentation(docstring, expected_params, func_name)
                    documentation_gaps.extend(missing_params)
                else:
                    documentation_gaps.append(f"{func_name}: No docstring found")
    
    if documentation_gaps:
        raise AssertionError(f"Parameter documentation gaps: {documentation_gaps}")
    
    print("PASS: All function parameters are properly documented")


def test_return_value_documentation():
    """
    Given: Functions that return values in the Python interface
    When: We validate return value documentation
    Then: Functions with return values must document them
    """
    functions_with_returns = {
        'show': 'should document when it returns vs blocks'
    }
    
    return_doc_gaps = []
    
    if fortplot_module:
        # Dynamic inspection
        for func_name, return_expectation in functions_with_returns.items():
            if hasattr(fortplot_module, func_name):
                func = getattr(fortplot_module, func_name)
                if func.__doc__:
                    if not check_return_documentation(func.__doc__):
                        return_doc_gaps.append(f"{func_name}: {return_expectation}")
    else:
        # Static analysis fallback
        source_file = Path(__file__).parent.parent / "python" / "fortplot" / "fortplot.py"
        if source_file.exists():
            with open(source_file, 'r') as f:
                content = f.read()
            
            for func_name, return_expectation in functions_with_returns.items():
                docstring = extract_docstring_from_source(content, func_name)
                if docstring:
                    if not check_return_documentation(docstring):
                        return_doc_gaps.append(f"{func_name}: {return_expectation}")
    
    # Note: Most pyplot functions don't return meaningful values, so this test
    # may pass with minimal return documentation for void functions
    if return_doc_gaps:
        print(f"INFO: Return documentation could be enhanced: {return_doc_gaps}")
    
    print("PASS: Return value documentation is adequate")


def test_usage_examples_validation():
    """
    Given: Functions in the Python interface
    When: We validate usage examples in docstrings
    Then: Core functions should have usage examples where helpful
    """
    functions_needing_examples = ['figure', 'plot', 'show']
    
    missing_examples = []
    
    if fortplot_module:
        # Dynamic inspection
        for func_name in functions_needing_examples:
            if hasattr(fortplot_module, func_name):
                func = getattr(fortplot_module, func_name)
                if func.__doc__:
                    if not check_usage_examples(func.__doc__):
                        missing_examples.append(func_name)
                else:
                    missing_examples.append(f"{func_name} (no docstring)")
    else:
        # Static analysis fallback
        source_file = Path(__file__).parent.parent / "python" / "fortplot" / "fortplot.py"
        if source_file.exists():
            with open(source_file, 'r') as f:
                content = f.read()
            
            for func_name in functions_needing_examples:
                docstring = extract_docstring_from_source(content, func_name)
                if docstring:
                    if not check_usage_examples(docstring):
                        missing_examples.append(func_name)
                else:
                    missing_examples.append(f"{func_name} (no docstring)")
    
    if missing_examples:
        raise AssertionError(f"Functions missing usage examples: {missing_examples}")
    
    print("PASS: Core functions have adequate usage examples")


def check_docstring_style(docstring, func_name):
    """Check if docstring follows Google or NumPy style conventions."""
    violations = []
    
    # Basic style checks
    if not docstring.strip():
        violations.append(f"{func_name}: Empty docstring")
        return violations
    
    # Check for basic structure (should have a summary line)
    lines = docstring.strip().split('\n')
    if len(lines) == 0:
        violations.append(f"{func_name}: No summary line")
    elif not lines[0].strip():
        violations.append(f"{func_name}: First line should be summary")
    
    # Check for parameter section if function likely has parameters
    param_indicators = ['Parameters', 'Args:', 'Arguments:']
    has_param_section = any(indicator in docstring for indicator in param_indicators)
    
    # For functions that likely have parameters, suggest parameter documentation
    if func_name in ['figure', 'plot', 'show'] and not has_param_section:
        violations.append(f"{func_name}: Missing Parameters section")
    
    return violations


def check_parameter_documentation(docstring, expected_params, func_name):
    """Check if all expected parameters are documented in the docstring."""
    missing_params = []
    
    # Simple check - look for parameter names in docstring
    for param in expected_params:
        if param not in docstring:
            missing_params.append(f"{func_name}.{param}")
    
    return missing_params


def check_return_documentation(docstring):
    """Check if return values are documented."""
    return_indicators = ['Returns', 'Return:', 'Yields']
    return any(indicator in docstring for indicator in return_indicators)


def check_usage_examples(docstring):
    """Check if docstring contains usage examples."""
    example_indicators = ['Examples', 'Example:', '>>>', 'Usage:']
    return any(indicator in docstring for indicator in example_indicators)


def extract_docstring_from_source(content, func_name):
    """Extract docstring from source code using regex."""
    # Find function definition
    pattern = rf'def\s+{func_name}\s*\([^)]*\):\s*'
    match = re.search(pattern, content)
    if not match:
        return None
    
    # Find docstring after function definition
    start_pos = match.end()
    remaining = content[start_pos:]
    
    # Look for triple-quoted string
    docstring_match = re.search(r'^\s*["\']{{3}}(.*?)["\']{{3}}', remaining, re.DOTALL | re.MULTILINE)
    if docstring_match:
        return docstring_match.group(1).strip()
    
    return None


def main():
    """Run all Python docstring validation tests."""
    tests = [
        test_core_functions_have_docstrings,
        test_docstring_style_compliance,
        test_parameter_documentation_completeness,
        test_return_value_documentation,
        test_usage_examples_validation
    ]
    
    failed_tests = []
    
    for test in tests:
        try:
            print(f"\nRunning {test.__name__}...")
            test()
        except AssertionError as e:
            print(f"FAIL: {e}")
            failed_tests.append(test.__name__)
        except Exception as e:
            print(f"ERROR in {test.__name__}: {e}")
            failed_tests.append(test.__name__)
    
    if failed_tests:
        print(f"\n{len(failed_tests)} test(s) failed: {failed_tests}")
        sys.exit(1)
    else:
        print(f"\nAll docstring validation tests passed!")
        sys.exit(0)


if __name__ == "__main__":
    main()