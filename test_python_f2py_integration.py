#!/usr/bin/env python3
"""
Integration tests for actual F2PY Python bindings to Fortran backend.

These tests verify that the Python interface actually calls Fortran functions
and generates real plot files, replacing the mock implementation.

Test Requirements:
1. Python calls generate actual plot files (PNG, PDF, TXT)
2. F2PY bindings work correctly with data marshalling
3. Basic plotting functions work end-to-end
4. Error handling works properly
"""

import numpy as np
import tempfile
import os
import sys
import pytest
from pathlib import Path

# Add the python package to the path
sys.path.insert(0, str(Path(__file__).parent / "python"))

import fortplot

class TestF2PyIntegration:
    """Test actual F2PY bindings functionality."""
    
    def test_basic_plot_generates_files(self):
        """Test that basic plot calls generate actual files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test data
            x = np.linspace(0, 10, 100)
            y = np.sin(x)
            
            # Create plot
            fortplot.figure()
            fortplot.plot(x, y, label='sin(x)')
            fortplot.xlabel('x')
            fortplot.ylabel('sin(x)')
            fortplot.title('Integration Test Plot')
            fortplot.legend()
            
            # Save to different formats
            png_file = os.path.join(tmpdir, 'test_plot.png')
            pdf_file = os.path.join(tmpdir, 'test_plot.pdf')
            txt_file = os.path.join(tmpdir, 'test_plot.txt')
            
            fortplot.savefig(png_file)
            fortplot.savefig(pdf_file)  
            fortplot.savefig(txt_file)
            
            # Verify files were created and have reasonable content
            assert os.path.exists(png_file), "PNG file was not created"
            assert os.path.exists(pdf_file), "PDF file was not created"
            assert os.path.exists(txt_file), "TXT file was not created"
            
            # Check file sizes - should be more than just placeholder text
            assert os.path.getsize(png_file) > 1000, "PNG file too small - likely mock output"
            assert os.path.getsize(pdf_file) > 1000, "PDF file too small - likely mock output"
            assert os.path.getsize(txt_file) > 100, "TXT file too small - likely mock output"
            
            # Check that TXT file contains actual plot data, not just comments
            with open(txt_file, 'r') as f:
                content = f.read()
                # Should not be just mock comments
                assert not content.startswith("# Mock fortplot output"), "TXT output is still mock"
                # Should contain some plotting characters or data
                assert any(char in content for char in ['+', '-', '|', '*', '.']), "TXT output has no plot characters"
    
    def test_scatter_plot_functionality(self):
        """Test scatter plot implementation."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create scatter data
            x = np.random.random(50) * 10
            y = np.random.random(50) * 10
            
            fortplot.figure()
            fortplot.scatter(x, y, label='random points')
            fortplot.xlabel('X Values')
            fortplot.ylabel('Y Values') 
            fortplot.title('Scatter Test')
            fortplot.legend()
            
            output_file = os.path.join(tmpdir, 'scatter_test.png')
            fortplot.savefig(output_file)
            
            assert os.path.exists(output_file)
            assert os.path.getsize(output_file) > 1000
    
    def test_histogram_functionality(self):
        """Test histogram implementation."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create histogram data
            data = np.random.normal(0, 1, 1000)
            
            fortplot.figure()
            fortplot.histogram(data, bins=30, label='normal distribution')
            fortplot.xlabel('Value')
            fortplot.ylabel('Frequency')
            fortplot.title('Histogram Test')
            fortplot.legend()
            
            output_file = os.path.join(tmpdir, 'histogram_test.png')
            fortplot.savefig(output_file)
            
            assert os.path.exists(output_file)
            assert os.path.getsize(output_file) > 1000
    
    def test_show_function_nonblocking(self):
        """Test that show function works without hanging."""
        # Create simple plot
        x = np.array([1, 2, 3])
        y = np.array([1, 4, 9])
        
        fortplot.figure()
        fortplot.plot(x, y)
        
        # Should return without hanging
        fortplot.show(blocking=False)
        
        # Test completed successfully if we reach this point
        assert True
    
    def test_multiple_plots_same_figure(self):
        """Test multiple plot calls on same figure."""
        with tempfile.TemporaryDirectory() as tmpdir:
            x = np.linspace(0, 2*np.pi, 100)
            
            fortplot.figure()
            fortplot.plot(x, np.sin(x), label='sin(x)', linestyle='-')
            fortplot.plot(x, np.cos(x), label='cos(x)', linestyle='--')
            fortplot.xlabel('x')
            fortplot.ylabel('y')
            fortplot.title('Multiple Plots Test')
            fortplot.legend()
            
            output_file = os.path.join(tmpdir, 'multiple_plots.png')
            fortplot.savefig(output_file)
            
            assert os.path.exists(output_file)
            assert os.path.getsize(output_file) > 1000
    
    def test_data_marshalling_dtypes(self):
        """Test that different numpy dtypes are handled correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Test different data types
            x_int32 = np.array([1, 2, 3, 4, 5], dtype=np.int32)
            y_float32 = np.array([1.0, 4.0, 9.0, 16.0, 25.0], dtype=np.float32)
            
            x_float64 = np.array([1.0, 2.0, 3.0, 4.0, 5.0], dtype=np.float64)
            y_int64 = np.array([2, 8, 18, 32, 50], dtype=np.int64)
            
            fortplot.figure()
            fortplot.plot(x_int32, y_float32, label='int32/float32')
            fortplot.plot(x_float64, y_int64, label='float64/int64', linestyle='--')
            fortplot.legend()
            
            output_file = os.path.join(tmpdir, 'dtype_test.png')
            fortplot.savefig(output_file)
            
            assert os.path.exists(output_file)
            assert os.path.getsize(output_file) > 1000
    
    def test_error_handling_invalid_input(self):
        """Test error handling with invalid inputs."""
        # Test mismatched array sizes
        x = np.array([1, 2, 3])
        y = np.array([1, 2])  # Different size
        
        fortplot.figure()
        
        # This should either raise an exception or handle gracefully
        try:
            fortplot.plot(x, y)
            # If no exception, at least verify no crash occurred
            assert True
        except (ValueError, RuntimeError) as e:
            # Expected error for mismatched arrays
            assert "size" in str(e).lower() or "length" in str(e).lower()
    
    def test_no_mock_output_in_production(self):
        """Ensure no mock print statements appear in actual implementation."""
        import io
        import sys
        
        # Capture stdout
        old_stdout = sys.stdout
        captured_output = io.StringIO()
        sys.stdout = captured_output
        
        try:
            x = np.array([1, 2, 3])
            y = np.array([1, 4, 9])
            
            fortplot.figure()
            fortplot.plot(x, y, label='test')
            fortplot.xlabel('x')
            fortplot.ylabel('y')
            fortplot.title('No Mock Test')
            
            output = captured_output.getvalue()
            
            # Should not contain mock print statements
            assert "Mock:" not in output, f"Found mock output: {output}"
            assert "mock" not in output.lower(), f"Found mock references: {output}"
            
        finally:
            sys.stdout = old_stdout

if __name__ == "__main__":
    # Can be run directly for testing
    test = TestF2PyIntegration()
    try:
        test.test_basic_plot_generates_files()
        print("✓ Basic plot file generation test passed")
        
        test.test_no_mock_output_in_production()
        print("✓ No mock output test passed")
        
        print("All integration tests passed!")
        
    except Exception as e:
        print(f"✗ Test failed: {e}")
        sys.exit(1)