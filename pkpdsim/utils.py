"""
Utility functions for PKPDsim
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Union, Any
import json


def cv_to_omega(cv: Union[float, List[float]]) -> np.ndarray:
    """
    Convert coefficient of variation to omega matrix diagonal
    
    Parameters
    ----------
    cv : float or list of float
        Coefficient of variation (as proportion, not percentage)
        
    Returns
    -------
    np.ndarray
        Omega values (variance) for diagonal of omega matrix
    """
    
    if isinstance(cv, (int, float)):
        return np.array([cv**2])
    else:
        return np.array([c**2 for c in cv])


def omega_to_cv(omega: np.ndarray) -> np.ndarray:
    """
    Convert omega matrix diagonal to coefficient of variation
    
    Parameters
    ----------
    omega : np.ndarray
        Omega values (variance)
        
    Returns
    -------
    np.ndarray
        Coefficient of variation values
    """
    
    return np.sqrt(omega)


def triangle_to_full(triangle: List[float], size: Optional[int] = None) -> np.ndarray:
    """
    Convert lower triangle vector to full symmetric matrix
    
    Parameters
    ----------
    triangle : list of float
        Lower triangle values in row-major order
    size : int, optional
        Matrix size (will be inferred if not provided)
        
    Returns
    -------
    np.ndarray
        Full symmetric matrix
    """
    
    n_elements = len(triangle)
    
    if size is None:
        # Infer size from number of elements
        # For n x n matrix, lower triangle has n(n+1)/2 elements
        size = int((-1 + np.sqrt(1 + 8 * n_elements)) / 2)
        
        if size * (size + 1) // 2 != n_elements:
            raise ValueError(f"Cannot form square matrix from {n_elements} elements")
    
    matrix = np.zeros((size, size))
    
    idx = 0
    for i in range(size):
        for j in range(i + 1):
            matrix[i, j] = triangle[idx]
            matrix[j, i] = triangle[idx]  # Make symmetric
            idx += 1
    
    return matrix


def full_to_triangle(matrix: np.ndarray) -> List[float]:
    """
    Convert full symmetric matrix to lower triangle vector
    
    Parameters
    ----------
    matrix : np.ndarray
        Full symmetric matrix
        
    Returns
    -------
    list of float
        Lower triangle values in row-major order
    """
    
    if matrix.shape[0] != matrix.shape[1]:
        raise ValueError("Matrix must be square")
    
    size = matrix.shape[0]
    triangle = []
    
    for i in range(size):
        for j in range(i + 1):
            triangle.append(matrix[i, j])
    
    return triangle


def is_positive_definite(matrix: np.ndarray, tol: float = 1e-8) -> bool:
    """
    Check if matrix is positive definite
    
    Parameters
    ----------
    matrix : np.ndarray
        Matrix to check
    tol : float, default 1e-8
        Tolerance for eigenvalue check
        
    Returns
    -------
    bool
        True if matrix is positive definite
    """
    
    try:
        eigenvals = np.linalg.eigvals(matrix)
        return np.all(eigenvals > tol)
    except np.linalg.LinAlgError:
        return False


def mvrnorm(n: int, mu: np.ndarray, sigma: np.ndarray, 
           method: str = "cholesky") -> np.ndarray:
    """
    Generate multivariate normal random variables
    
    Parameters
    ----------
    n : int
        Number of samples
    mu : np.ndarray
        Mean vector
    sigma : np.ndarray
        Covariance matrix
    method : str, default "cholesky"
        Method to use: "cholesky" or "svd"
        
    Returns
    -------
    np.ndarray
        Random samples (n x len(mu))
    """
    
    if method == "cholesky":
        try:
            L = np.linalg.cholesky(sigma)
            Z = np.random.standard_normal((n, len(mu)))
            return mu + Z @ L.T
        except np.linalg.LinAlgError:
            # Fall back to SVD if Cholesky fails
            method = "svd"
    
    if method == "svd":
        U, s, Vt = np.linalg.svd(sigma)
        L = U @ np.diag(np.sqrt(s))
        Z = np.random.standard_normal((n, len(mu)))
        return mu + Z @ L.T
    
    raise ValueError(f"Unknown method: {method}")


def na_locf(values: List, times: Optional[List] = None) -> List:
    """
    Last observation carried forward for missing values
    
    Parameters
    ----------
    values : list
        Values with possible NaN/None entries
    times : list, optional
        Corresponding times
        
    Returns
    -------
    list
        Values with missing values filled using LOCF
    """
    
    result = []
    last_valid = None
    
    for value in values:
        if value is not None and not (isinstance(value, float) and np.isnan(value)):
            last_valid = value
            result.append(value)
        else:
            if last_valid is not None:
                result.append(last_valid)
            else:
                result.append(0)  # Default if no valid value yet
    
    return result


def ifelse0(condition: bool, true_val: Any, false_val: Any = 0) -> Any:
    """
    Conditional assignment with default of 0
    
    Parameters
    ----------
    condition : bool
        Condition to evaluate
    true_val : any
        Value to return if condition is True
    false_val : any, default 0
        Value to return if condition is False
        
    Returns
    -------
    any
        Selected value based on condition
    """
    
    return true_val if condition else false_val


def table_to_list(df: pd.DataFrame, 
                 id_col: str = "id") -> List[Dict[str, Any]]:
    """
    Convert DataFrame to list of dictionaries per individual
    
    Parameters
    ----------
    df : pd.DataFrame
        Input DataFrame
    id_col : str, default "id"
        Column name containing individual IDs
        
    Returns
    -------
    list of dict
        List of dictionaries, one per individual
    """
    
    if id_col not in df.columns:
        raise ValueError(f"Column '{id_col}' not found in DataFrame")
    
    result = []
    
    for individual_id in df[id_col].unique():
        individual_data = df[df[id_col] == individual_id]
        result.append(individual_data.to_dict('records'))
    
    return result


def calculate_auc(times: np.ndarray, concentrations: np.ndarray,
                 method: str = "linear") -> float:
    """
    Calculate area under the curve
    
    Parameters
    ----------
    times : np.ndarray
        Time points
    concentrations : np.ndarray
        Concentration values
    method : str, default "linear"
        Integration method: "linear" or "log"
        
    Returns
    -------
    float
        AUC value
    """
    
    if len(times) != len(concentrations):
        raise ValueError("Times and concentrations must have same length")
    
    if len(times) < 2:
        return 0.0
    
    if method == "linear":
        return np.trapz(concentrations, times)
    
    elif method == "log":
        # Log-linear trapezoidal rule
        auc = 0.0
        for i in range(len(times) - 1):
            dt = times[i + 1] - times[i]
            c1, c2 = concentrations[i], concentrations[i + 1]
            
            if c1 > 0 and c2 > 0:
                # Log-linear
                auc += dt * (c2 - c1) / np.log(c2 / c1)
            else:
                # Linear fallback
                auc += dt * (c1 + c2) / 2
        
        return auc
    
    else:
        raise ValueError(f"Unknown AUC method: {method}")


def calculate_cmax_tmax(times: np.ndarray, 
                       concentrations: np.ndarray) -> Dict[str, float]:
    """
    Calculate maximum concentration and time to maximum
    
    Parameters
    ----------
    times : np.ndarray
        Time points
    concentrations : np.ndarray
        Concentration values
        
    Returns
    -------
    dict
        Dictionary with 'cmax' and 'tmax' keys
    """
    
    if len(times) != len(concentrations):
        raise ValueError("Times and concentrations must have same length")
    
    if len(concentrations) == 0:
        return {'cmax': 0.0, 'tmax': 0.0}
    
    max_idx = np.argmax(concentrations)
    
    return {
        'cmax': concentrations[max_idx],
        'tmax': times[max_idx]
    }


def add_ruv_to_quantile(values: np.ndarray, 
                       quantile: float,
                       ruv: Dict[str, float]) -> np.ndarray:
    """
    Add residual unexplained variability to quantile
    
    Parameters
    ----------
    values : np.ndarray
        Base values
    quantile : float
        Quantile to calculate (0-1)
    ruv : dict
        Residual variability parameters
        
    Returns
    -------
    np.ndarray
        Values with RUV added
    """
    
    if not (0 <= quantile <= 1):
        raise ValueError("Quantile must be between 0 and 1")
    
    # Calculate standard normal quantile
    z = np.percentile(np.random.standard_normal(10000), quantile * 100)
    
    result = values.copy()
    
    # Apply proportional error
    if 'prop' in ruv:
        result = result * (1 + ruv['prop'] * z)
    
    # Apply additive error
    if 'add' in ruv:
        result = result + ruv['add'] * z
    
    # Apply exponential error
    if 'exp' in ruv:
        result = result * np.exp(ruv['exp'] * z)
    
    return result


def read_model_json(file_path: str) -> Dict[str, Any]:
    """
    Read model definition from JSON file
    
    Parameters
    ----------
    file_path : str
        Path to JSON file
        
    Returns
    -------
    dict
        Model definition
    """
    
    try:
        with open(file_path, 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        raise FileNotFoundError(f"Model file not found: {file_path}")
    except json.JSONDecodeError as e:
        raise ValueError(f"Invalid JSON in model file: {e}")


def now_utc() -> float:
    """
    Get current UTC time as timestamp
    
    Returns
    -------
    float
        Current UTC timestamp
    """
    
    from datetime import datetime, timezone
    return datetime.now(timezone.utc).timestamp()