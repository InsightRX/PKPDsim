"""
Tests for utility functions
"""

import pytest
import numpy as np
import pandas as pd
from pkpdsim.utils import (
    cv_to_omega, omega_to_cv, triangle_to_full, full_to_triangle,
    is_positive_definite, mvrnorm, na_locf, ifelse0, table_to_list,
    calculate_auc, calculate_cmax_tmax, add_ruv_to_quantile
)


def test_cv_to_omega():
    """Test CV to omega conversion"""
    
    # Single CV
    omega = cv_to_omega(0.2)  # 20% CV
    expected = np.array([0.04])  # 0.2^2
    np.testing.assert_array_almost_equal(omega, expected)
    
    # Multiple CVs
    omega = cv_to_omega([0.1, 0.2, 0.3])
    expected = np.array([0.01, 0.04, 0.09])
    np.testing.assert_array_almost_equal(omega, expected)


def test_omega_to_cv():
    """Test omega to CV conversion"""
    
    omega = np.array([0.04, 0.09, 0.16])
    cv = omega_to_cv(omega)
    expected = np.array([0.2, 0.3, 0.4])
    np.testing.assert_array_almost_equal(cv, expected)


def test_triangle_to_full():
    """Test converting lower triangle to full matrix"""
    
    # 2x2 matrix
    triangle = [1.0, 0.5, 2.0]  # [1.0, 0.5; 0.5, 2.0]
    matrix = triangle_to_full(triangle)
    
    expected = np.array([[1.0, 0.5], [0.5, 2.0]])
    np.testing.assert_array_almost_equal(matrix, expected)
    
    # 3x3 matrix
    triangle = [4.0, 1.0, 9.0, 2.0, 3.0, 16.0]  # Lower triangle
    matrix = triangle_to_full(triangle, size=3)
    
    expected = np.array([
        [4.0, 1.0, 2.0],
        [1.0, 9.0, 3.0],
        [2.0, 3.0, 16.0]
    ])
    np.testing.assert_array_almost_equal(matrix, expected)


def test_full_to_triangle():
    """Test converting full matrix to lower triangle"""
    
    matrix = np.array([[4.0, 1.0], [1.0, 9.0]])
    triangle = full_to_triangle(matrix)
    
    expected = [4.0, 1.0, 9.0]
    np.testing.assert_array_almost_equal(triangle, expected)


def test_triangle_full_roundtrip():
    """Test roundtrip conversion triangle <-> full"""
    
    original_triangle = [1.0, 0.5, 2.0, 0.3, 0.7, 3.0]
    matrix = triangle_to_full(original_triangle)
    recovered_triangle = full_to_triangle(matrix)
    
    np.testing.assert_array_almost_equal(original_triangle, recovered_triangle)


def test_is_positive_definite():
    """Test positive definite check"""
    
    # Positive definite matrix
    pos_def = np.array([[2.0, 1.0], [1.0, 2.0]])
    assert is_positive_definite(pos_def)
    
    # Not positive definite
    not_pos_def = np.array([[1.0, 2.0], [2.0, 1.0]])
    assert not is_positive_definite(not_pos_def)
    
    # Identity matrix (positive definite)
    identity = np.eye(3)
    assert is_positive_definite(identity)


def test_mvrnorm():
    """Test multivariate normal random number generation"""
    
    np.random.seed(123)
    
    n = 100
    mu = np.array([0.0, 1.0])
    sigma = np.array([[1.0, 0.5], [0.5, 2.0]])
    
    samples = mvrnorm(n, mu, sigma)
    
    assert samples.shape == (n, 2)
    
    # Check that sample means are approximately correct
    sample_mean = np.mean(samples, axis=0)
    np.testing.assert_array_almost_equal(sample_mean, mu, decimal=1)


def test_mvrnorm_svd_fallback():
    """Test SVD fallback for non-positive definite matrix"""
    
    np.random.seed(123)
    
    n = 50
    mu = np.array([0.0, 0.0])
    # Nearly singular matrix
    sigma = np.array([[1.0, 0.99], [0.99, 0.9801]])
    
    # Should work with SVD method
    samples = mvrnorm(n, mu, sigma, method="svd")
    assert samples.shape == (n, 2)


def test_na_locf():
    """Test last observation carried forward"""
    
    # With None values
    values = [1.0, None, None, 2.0, None, 3.0]
    result = na_locf(values)
    expected = [1.0, 1.0, 1.0, 2.0, 2.0, 3.0]
    assert result == expected
    
    # With NaN values
    values = [1.0, np.nan, 2.0, np.nan, np.nan]
    result = na_locf(values)
    expected = [1.0, 1.0, 2.0, 2.0, 2.0]
    np.testing.assert_array_almost_equal(result, expected)
    
    # Starting with missing value
    values = [None, None, 1.0, 2.0]
    result = na_locf(values)
    expected = [0, 0, 1.0, 2.0]  # Default to 0
    assert result == expected


def test_ifelse0():
    """Test conditional assignment with default 0"""
    
    assert ifelse0(True, 5) == 5
    assert ifelse0(False, 5) == 0
    assert ifelse0(False, 5, -1) == -1
    assert ifelse0(True, "yes", "no") == "yes"


def test_table_to_list():
    """Test converting DataFrame to list of dicts"""
    
    data = [
        {'id': 1, 'time': 0, 'value': 10},
        {'id': 1, 'time': 1, 'value': 8},
        {'id': 2, 'time': 0, 'value': 12},
        {'id': 2, 'time': 1, 'value': 9}
    ]
    df = pd.DataFrame(data)
    
    result = table_to_list(df, id_col='id')
    
    assert len(result) == 2  # Two individuals
    assert len(result[0]) == 2  # Two records for individual 1
    assert result[0][0]['value'] == 10


def test_calculate_auc_linear():
    """Test AUC calculation with linear method"""
    
    times = np.array([0, 1, 2, 4, 8])
    concentrations = np.array([0, 10, 8, 4, 1])
    
    auc = calculate_auc(times, concentrations, method="linear")
    
    # Should be positive
    assert auc > 0
    
    # Check against known trapezoidal rule
    expected = np.trapz(concentrations, times)
    assert abs(auc - expected) < 1e-10


def test_calculate_auc_log():
    """Test AUC calculation with log-linear method"""
    
    times = np.array([0, 1, 2, 4])
    concentrations = np.array([10, 5, 2.5, 1.25])  # Exponential decay
    
    auc = calculate_auc(times, concentrations, method="log")
    
    assert auc > 0
    # Log-linear should be different from linear for exponential data


def test_calculate_auc_edge_cases():
    """Test AUC calculation edge cases"""
    
    # Single point
    auc = calculate_auc(np.array([0]), np.array([10]))
    assert auc == 0
    
    # Two points
    auc = calculate_auc(np.array([0, 1]), np.array([10, 5]))
    assert auc == 7.5  # (10 + 5) / 2 * 1
    
    # Zero concentrations
    auc = calculate_auc(np.array([0, 1, 2]), np.array([0, 0, 0]))
    assert auc == 0


def test_calculate_cmax_tmax():
    """Test Cmax and Tmax calculation"""
    
    times = np.array([0, 0.5, 1, 2, 4, 8])
    concentrations = np.array([0, 5, 10, 8, 4, 1])
    
    result = calculate_cmax_tmax(times, concentrations)
    
    assert result['cmax'] == 10
    assert result['tmax'] == 1
    
    # Empty arrays
    result = calculate_cmax_tmax(np.array([]), np.array([]))
    assert result['cmax'] == 0
    assert result['tmax'] == 0


def test_add_ruv_to_quantile():
    """Test adding RUV to quantile"""
    
    np.random.seed(123)
    
    values = np.array([10.0, 20.0, 30.0])
    ruv = {'prop': 0.1, 'add': 1.0}
    
    # 50th percentile (median)
    result = add_ruv_to_quantile(values, 0.5, ruv)
    
    assert len(result) == len(values)
    # Values should be modified
    assert not np.array_equal(result, values)


def test_add_ruv_quantile_validation():
    """Test RUV quantile validation"""
    
    values = np.array([10.0])
    ruv = {'prop': 0.1}
    
    # Invalid quantile
    with pytest.raises(ValueError):
        add_ruv_to_quantile(values, -0.1, ruv)
    
    with pytest.raises(ValueError):
        add_ruv_to_quantile(values, 1.1, ruv)


def test_triangle_validation():
    """Test triangle matrix validation"""
    
    # Invalid number of elements
    with pytest.raises(ValueError):
        triangle_to_full([1.0, 2.0])  # Can't form square matrix


def test_matrix_validation():
    """Test matrix validation"""
    
    # Non-square matrix
    matrix = np.array([[1, 2, 3], [4, 5, 6]])
    with pytest.raises(ValueError):
        full_to_triangle(matrix)


def test_calculate_auc_validation():
    """Test AUC calculation validation"""
    
    times = np.array([0, 1])
    concentrations = np.array([10])  # Mismatched lengths
    
    with pytest.raises(ValueError):
        calculate_auc(times, concentrations)


def test_calculate_cmax_tmax_validation():
    """Test Cmax/Tmax validation"""
    
    times = np.array([0, 1])
    concentrations = np.array([10])  # Mismatched lengths
    
    with pytest.raises(ValueError):
        calculate_cmax_tmax(times, concentrations)


def test_table_to_list_validation():
    """Test table to list validation"""
    
    df = pd.DataFrame({'time': [0, 1], 'value': [10, 8]})
    
    with pytest.raises(ValueError):
        table_to_list(df, id_col='missing_col')


if __name__ == "__main__":
    pytest.main([__file__])