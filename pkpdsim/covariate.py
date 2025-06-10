"""
Covariate functionality for PKPDsim
"""

import numpy as np
import pandas as pd
from typing import Dict, List, Optional, Union, Any
from datetime import datetime


def new_covariate(
    value: Union[float, List[float], Dict[float, float]],
    times: Optional[List[float]] = None,
    implementation: str = "locf",
    interpolation: str = "linear",
    unit: Optional[str] = None,
    comments: Optional[str] = None
) -> Dict[str, Any]:
    """
    Create a covariate object
    
    Parameters
    ----------
    value : float, list, or dict
        Covariate value(s). Can be:
        - Single value for time-invariant covariate
        - List of values with corresponding times
        - Dict mapping times to values
    times : list of float, optional
        Time points for time-varying covariates
    implementation : str, default "locf"
        Implementation method: "locf" (last observation carried forward) or "interpolate"
    interpolation : str, default "linear"
        Interpolation method when implementation="interpolate"
    unit : str, optional
        Unit of the covariate
    comments : str, optional
        Comments about the covariate
        
    Returns
    -------
    dict
        Covariate object
        
    Examples
    --------
    >>> # Time-invariant covariate
    >>> wt = new_covariate(value=70, unit="kg")
    
    >>> # Time-varying covariate
    >>> wt_time = new_covariate(
    ...     value=[70, 72, 68], 
    ...     times=[0, 24, 48], 
    ...     unit="kg"
    ... )
    
    >>> # Using dict format
    >>> wt_dict = new_covariate(
    ...     value={0: 70, 24: 72, 48: 68}, 
    ...     unit="kg"
    ... )
    """
    
    # Handle different input formats
    if isinstance(value, dict):
        # Convert dict to lists
        times_list = sorted(value.keys())
        values_list = [value[t] for t in times_list]
        times = times_list
        value = values_list
    
    # Validate inputs
    if isinstance(value, list):
        if times is None:
            raise ValueError("Times must be provided for time-varying covariates")
        if len(value) != len(times):
            raise ValueError("Length of values and times must match")
        
        # Sort by time
        sorted_indices = np.argsort(times)
        times = [times[i] for i in sorted_indices]
        value = [value[i] for i in sorted_indices]
    
    # Create covariate object
    covariate = {
        'value': value,
        'times': times,
        'implementation': implementation,
        'interpolation': interpolation,
        'unit': unit,
        'comments': comments,
        'class': 'covariate'
    }
    
    return covariate


def get_covariate_value(covariate: Dict[str, Any], time: float) -> float:
    """
    Get covariate value at a specific time
    
    Parameters
    ----------
    covariate : dict
        Covariate object created by new_covariate()
    time : float
        Time at which to evaluate the covariate
        
    Returns
    -------
    float
        Covariate value at the specified time
    """
    
    value = covariate['value']
    times = covariate['times']
    
    # Time-invariant covariate
    if not isinstance(value, list):
        return value
    
    # Time-varying covariate
    if times is None:
        raise ValueError("Times not specified for time-varying covariate")
    
    # If time is before first observation, return first value
    if time <= times[0]:
        return value[0]
    
    # If time is after last observation, return last value
    if time >= times[-1]:
        return value[-1]
    
    # Find appropriate value based on implementation
    implementation = covariate.get('implementation', 'locf')
    
    if implementation == 'locf':
        # Last observation carried forward
        for i in range(len(times) - 1, -1, -1):
            if time >= times[i]:
                return value[i]
        return value[0]  # Fallback
    
    elif implementation == 'interpolate':
        # Linear interpolation
        return np.interp(time, times, value)
    
    else:
        raise ValueError(f"Unknown implementation method: {implementation}")


def create_covariate_table(
    covariates: Dict[str, Dict[str, Any]], 
    times: List[float],
    n_individuals: int = 1
) -> pd.DataFrame:
    """
    Create a covariate table for multiple individuals
    
    Parameters
    ----------
    covariates : dict
        Dictionary of covariate objects, keyed by covariate name
    times : list of float
        Times at which to evaluate covariates
    n_individuals : int, default 1
        Number of individuals
        
    Returns
    -------
    pd.DataFrame
        Covariate table with columns: id, time, and one column per covariate
    """
    
    data = []
    
    for individual_id in range(1, n_individuals + 1):
        for time in times:
            row = {'id': individual_id, 'time': time}
            
            for cov_name, cov_obj in covariates.items():
                row[cov_name] = get_covariate_value(cov_obj, time)
            
            data.append(row)
    
    return pd.DataFrame(data)


def covariates_table_to_list(
    covariates_table: pd.DataFrame
) -> List[Dict[str, Dict[str, Any]]]:
    """
    Convert covariate table to list of covariate dictionaries per individual
    
    Parameters
    ----------
    covariates_table : pd.DataFrame
        Covariate table with id, time, and covariate columns
        
    Returns
    -------
    list of dict
        List of covariate dictionaries, one per individual
    """
    
    if 'id' not in covariates_table.columns:
        raise ValueError("Covariate table must have 'id' column")
    
    if 'time' not in covariates_table.columns:
        raise ValueError("Covariate table must have 'time' column")
    
    result = []
    
    # Get covariate columns (excluding id and time)
    cov_columns = [col for col in covariates_table.columns if col not in ['id', 'time']]
    
    for individual_id in covariates_table['id'].unique():
        individual_data = covariates_table[covariates_table['id'] == individual_id]
        individual_covs = {}
        
        for cov_name in cov_columns:
            # Extract times and values for this covariate
            times = individual_data['time'].tolist()
            values = individual_data[cov_name].tolist()
            
            # Create covariate object
            individual_covs[cov_name] = new_covariate(
                value=values,
                times=times,
                implementation='locf'  # Default implementation
            )
        
        result.append(individual_covs)
    
    return result


def print_covariate(covariate: Dict[str, Any], name: str = "Covariate") -> None:
    """
    Print covariate information
    
    Parameters
    ----------
    covariate : dict
        Covariate object
    name : str, default "Covariate"
        Name of the covariate for display
    """
    
    print(f"{name}:")
    
    if isinstance(covariate['value'], list):
        print(f"  Time-varying: Yes")
        print(f"  Times: {covariate['times']}")
        print(f"  Values: {covariate['value']}")
        print(f"  Implementation: {covariate['implementation']}")
        if covariate['implementation'] == 'interpolate':
            print(f"  Interpolation: {covariate['interpolation']}")
    else:
        print(f"  Time-varying: No")
        print(f"  Value: {covariate['value']}")
    
    if covariate['unit']:
        print(f"  Unit: {covariate['unit']}")
    
    if covariate['comments']:
        print(f"  Comments: {covariate['comments']}")


def covariate_last_obs_only(covariate: Dict[str, Any]) -> Dict[str, Any]:
    """
    Convert time-varying covariate to time-invariant using last observation
    
    Parameters
    ----------
    covariate : dict
        Covariate object
        
    Returns
    -------
    dict
        Covariate object with only last observation
    """
    
    if not isinstance(covariate['value'], list):
        # Already time-invariant
        return covariate
    
    # Create new covariate with last value
    last_value = covariate['value'][-1]
    
    new_cov = covariate.copy()
    new_cov['value'] = last_value
    new_cov['times'] = None
    
    return new_cov


def apply_covariate_model(
    parameters: Dict[str, float],
    covariates: Dict[str, Dict[str, Any]],
    covariate_model: Dict[str, str],
    time: float = 0.0
) -> Dict[str, float]:
    """
    Apply covariate model to modify parameters
    
    Parameters
    ----------
    parameters : dict
        Base parameter values
    covariates : dict
        Dictionary of covariate objects
    covariate_model : dict
        Dictionary mapping parameter names to covariate relationships
    time : float, default 0.0
        Time at which to evaluate covariates
        
    Returns
    -------
    dict
        Modified parameters
        
    Examples
    --------
    >>> params = {'CL': 1.0, 'V': 10.0}
    >>> covs = {'WT': new_covariate(value=70)}
    >>> cov_model = {'CL': 'CL * (WT/70)**0.75', 'V': 'V * (WT/70)'}
    >>> new_params = apply_covariate_model(params, covs, cov_model)
    """
    
    # Get covariate values at specified time
    cov_values = {}
    for cov_name, cov_obj in covariates.items():
        cov_values[cov_name] = get_covariate_value(cov_obj, time)
    
    # Apply covariate model
    new_parameters = parameters.copy()
    
    for param_name, relationship in covariate_model.items():
        if param_name in parameters:
            # Create namespace for evaluation
            namespace = {
                **new_parameters,  # Parameter values
                **cov_values,      # Covariate values
                'np': np,
                'exp': np.exp,
                'log': np.log,
                'sqrt': np.sqrt,
                'pow': np.power
            }
            
            try:
                # Evaluate the relationship
                new_value = eval(relationship, namespace)
                new_parameters[param_name] = new_value
            except Exception as e:
                raise ValueError(f"Error evaluating covariate model for {param_name}: {e}")
    
    return new_parameters