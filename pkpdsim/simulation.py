"""
Core simulation functions for PKPDsim
"""

import numpy as np
import pandas as pd
from scipy.integrate import solve_ivp
from typing import Dict, List, Optional, Union, Callable, Any
import warnings


def sim(
    ode: Optional[Callable] = None,
    analytical: Optional[str] = None,
    parameters: Optional[Dict[str, float]] = None,
    parameters_table: Optional[pd.DataFrame] = None,
    mixture_group: Optional[int] = None,
    omega: Optional[np.ndarray] = None,
    omega_type: str = "exponential",
    res_var: Optional[Dict[str, float]] = None,
    iov_bins: Optional[List[float]] = None,
    seed: Optional[int] = None,
    sequence: Optional[str] = None,
    n_ind: int = 1,
    regimen: Optional[Dict] = None,
    lagtime: Optional[Union[float, str]] = None,
    A_init: Optional[np.ndarray] = None,
    covariates: Optional[Dict] = None,
    covariates_table: Optional[pd.DataFrame] = None,
    covariates_implementation: Optional[Dict[str, str]] = None,
    covariate_model: Optional[str] = None,
    only_obs: bool = True,
    obs_step_size: float = 1.0,
    int_step_size: float = 0.1,
    t_max: Optional[float] = None,
    t_obs: Optional[np.ndarray] = None,
    t_tte: Optional[np.ndarray] = None,
    t_init: float = 0.0,
    obs_type: Optional[List[str]] = None,
    duplicate_t_obs: bool = False,
    extra_t_obs: Union[bool, str] = "auto",
    rtte: bool = False,
    output_include: Optional[Dict[str, bool]] = None,
    checks: bool = True,
    verbose: bool = False,
    **kwargs
) -> pd.DataFrame:
    """
    Simulate ODE or analytical equation
    
    Simulates a specified regimen using ODE system or analytical equation
    
    Parameters
    ----------
    ode : callable, optional
        Function describing the ODE system
    analytical : str, optional
        String specifying analytical equation model to use (similar to ADVAN1-5 in NONMEM)
    parameters : dict, optional
        Model parameters
    parameters_table : pd.DataFrame, optional
        DataFrame of parameters containing parameter estimates for individuals to simulate
    mixture_group : int, optional
        Mixture group for models containing mixtures (1 or 2)
    omega : np.ndarray, optional
        Vector describing the lower-diagonal of the between-subject variability matrix
    omega_type : str, default "exponential"
        Exponential or normal, specified as vector
    res_var : dict, optional
        Residual variability with keys 'prop', 'add', and/or 'exp'
    iov_bins : list, optional
        IOV bins as timepoints specifying bin separators
    seed : int, optional
        Set seed for reproducible results
    sequence : str, optional
        Pseudo-random sequence to use, e.g. "halton" or "sobol"
    n_ind : int, default 1
        Number of individuals to simulate
    regimen : dict, optional
        Regimen object created using new_regimen()
    lagtime : float or str, optional
        Lag time value or parameter name
    A_init : np.ndarray, optional
        Vector with initial state of the ODE system
    covariates : dict, optional
        List of covariates for single individual
    covariates_table : pd.DataFrame, optional
        DataFrame with covariate values
    covariates_implementation : dict, optional
        Named dict of covariate implementation methods per covariate
    covariate_model : str, optional
        R code for pre-calculating effective parameters for analytical equations
    only_obs : bool, default True
        Only return the observations
    obs_step_size : float, default 1.0
        Step size between observations
    int_step_size : float, default 0.1
        Step size for numerical integrator
    t_max : float, optional
        Maximum simulation time
    t_obs : np.ndarray, optional
        Vector of observation times
    t_tte : np.ndarray, optional
        Vector of observation times for time-to-event simulation
    t_init : float, default 0.0
        Initialization time before first dose
    obs_type : list, optional
        Vector of observation types
    duplicate_t_obs : bool, default False
        Allow duplicate t_obs in output
    extra_t_obs : bool or str, default "auto"
        Include extra t_obs in output for bolus doses
    rtte : bool, default False
        Should repeated events be allowed
    output_include : dict, optional
        What to include in output table with keys 'parameters' and 'covariates'
    checks : bool, default True
        Perform input checks
    verbose : bool, default False
        Show more output
    **kwargs
        Extra parameters
        
    Returns
    -------
    pd.DataFrame
        DataFrame of compartments with associated concentrations at requested times
    """
    
    if checks:
        if ode is None and analytical is None:
            raise ValueError("Either 'ode' or 'analytical' must be specified")
        
        if parameters is None:
            raise ValueError("Parameters must be specified")
            
        if regimen is None:
            raise ValueError("Regimen must be specified")
    
    # Set random seed if provided
    if seed is not None:
        np.random.seed(seed)
    
    # Default parameters
    if output_include is None:
        output_include = {'parameters': False, 'covariates': False}
    
    # Initialize results list
    results = []
    
    # Simulate for each individual
    for i in range(n_ind):
        if verbose:
            print(f"Simulating individual {i+1}/{n_ind}")
            
        # Get individual parameters
        if parameters_table is not None:
            if isinstance(parameters_table, pd.DataFrame):
                ind_params = parameters_table.iloc[i % len(parameters_table)].to_dict()
            else:
                ind_params = parameters_table[i % len(parameters_table)]
        else:
            ind_params = parameters.copy()
        
        # Add between-subject variability if omega is specified
        if omega is not None:
            ind_params = _add_bsv(ind_params, omega, omega_type)
        
        # Simulate individual
        if analytical is not None:
            sim_result = _sim_analytical(
                analytical, ind_params, regimen, t_obs, t_max, 
                obs_step_size, A_init, lagtime, covariates
            )
        else:
            sim_result = _sim_ode(
                ode, ind_params, regimen, t_obs, t_max, 
                obs_step_size, int_step_size, A_init, lagtime, covariates
            )
        
        # Add individual ID
        sim_result['id'] = i + 1
        
        # Add residual variability if specified
        if res_var is not None:
            sim_result = _add_ruv(sim_result, res_var)
        
        results.append(sim_result)
    
    # Combine results
    if results:
        final_result = pd.concat(results, ignore_index=True)
        
        # Filter to observations only if requested
        if only_obs:
            final_result = final_result[final_result['evid'] == 0]
        
        return final_result
    else:
        return pd.DataFrame()


def sim_ode(ode, parameters, regimen, **kwargs):
    """
    Simulate ODE system
    
    Convenience wrapper around sim() for ODE simulation
    """
    return sim(ode=ode, parameters=parameters, regimen=regimen, **kwargs)


def _add_bsv(parameters: Dict[str, float], omega: np.ndarray, omega_type: str) -> Dict[str, float]:
    """Add between-subject variability to parameters"""
    params = parameters.copy()
    
    if omega_type == "exponential":
        # Generate multivariate normal random variables
        eta = np.random.multivariate_normal(np.zeros(len(omega)), np.diag(omega))
        
        # Apply exponential transformation
        for i, param_name in enumerate(params.keys()):
            if i < len(eta):
                params[param_name] *= np.exp(eta[i])
    else:
        # Normal (additive) variability
        eta = np.random.multivariate_normal(np.zeros(len(omega)), np.diag(omega))
        
        for i, param_name in enumerate(params.keys()):
            if i < len(eta):
                params[param_name] += eta[i]
    
    return params


def _add_ruv(data: pd.DataFrame, res_var: Dict[str, float]) -> pd.DataFrame:
    """Add residual variability to simulation results"""
    result = data.copy()
    
    if 'dv' in result.columns:
        y = result['dv'].values
        
        # Proportional error
        if 'prop' in res_var:
            prop_error = np.random.normal(0, res_var['prop'], len(y))
            y = y * (1 + prop_error)
        
        # Additive error
        if 'add' in res_var:
            add_error = np.random.normal(0, res_var['add'], len(y))
            y = y + add_error
            
        # Exponential error
        if 'exp' in res_var:
            exp_error = np.random.normal(0, res_var['exp'], len(y))
            y = y * np.exp(exp_error)
        
        result['dv'] = y
    
    return result


def _sim_analytical(analytical: str, parameters: Dict, regimen: Dict, 
                   t_obs: Optional[np.ndarray], t_max: Optional[float],
                   obs_step_size: float, A_init: Optional[np.ndarray],
                   lagtime: Optional[float], covariates: Optional[Dict]) -> pd.DataFrame:
    """Simulate using analytical equations (placeholder implementation)"""
    
    # This is a placeholder - would need specific analytical equation implementations
    # for different ADVAN-style models
    
    warnings.warn("Analytical simulation not fully implemented yet")
    
    # Create simple output structure
    if t_obs is not None:
        times = t_obs
    else:
        if t_max is None:
            t_max = regimen.get('times', [0])[-1] if 'times' in regimen else 24
        times = np.arange(0, t_max + obs_step_size, obs_step_size)
    
    # Placeholder concentration values
    conc = np.zeros(len(times))
    
    result = pd.DataFrame({
        'time': times,
        'dv': conc,
        'cmt': 1,
        'evid': 0
    })
    
    return result


def _sim_ode(ode: Callable, parameters: Dict, regimen: Dict,
            t_obs: Optional[np.ndarray], t_max: Optional[float],
            obs_step_size: float, int_step_size: float,
            A_init: Optional[np.ndarray], lagtime: Optional[float],
            covariates: Optional[Dict]) -> pd.DataFrame:
    """Simulate using ODE system"""
    
    # Set up time points
    if t_obs is not None:
        times = t_obs
    else:
        if t_max is None:
            t_max = regimen.get('times', [0])[-1] if 'times' in regimen else 24
        times = np.arange(0, t_max + obs_step_size, obs_step_size)
    
    # Initial conditions
    if A_init is None:
        A_init = np.zeros(3)  # Default 3-compartment
    
    # Create dosing events
    dose_times = regimen.get('times', [0])
    dose_amounts = regimen.get('amt', [100])
    
    if not isinstance(dose_amounts, list):
        dose_amounts = [dose_amounts] * len(dose_times)
    
    # Solve ODE with dosing events
    def ode_with_events(t, y):
        # Check for dosing events
        for i, dose_time in enumerate(dose_times):
            if abs(t - dose_time) < int_step_size / 2:
                # Apply dose to first compartment (can be modified)
                y = y.copy()
                y[0] += dose_amounts[i]
        
        return ode(t, y, parameters)
    
    # Solve ODE system
    sol = solve_ivp(
        ode_with_events, 
        [times[0], times[-1]], 
        A_init,
        t_eval=times,
        max_step=int_step_size,
        rtol=1e-6,
        atol=1e-8
    )
    
    # Create output DataFrame
    result_data = []
    
    for i, t in enumerate(times):
        # Add observation record (assuming compartment 2 is observed)
        result_data.append({
            'time': t,
            'dv': sol.y[1, i] if len(sol.y) > 1 else sol.y[0, i],  # Concentration in second compartment
            'cmt': 2,
            'evid': 0  # Observation
        })
    
    # Add dosing records
    for i, dose_time in enumerate(dose_times):
        result_data.append({
            'time': dose_time,
            'dv': dose_amounts[i],
            'cmt': 1,
            'evid': 1  # Dose
        })
    
    result = pd.DataFrame(result_data)
    result = result.sort_values('time').reset_index(drop=True)
    
    return result