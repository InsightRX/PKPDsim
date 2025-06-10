"""
ODE model creation and management for PKPDsim
"""

import numpy as np
from typing import Dict, List, Optional, Union, Callable, Any
import json
import os


def new_ode_model(
    model: Optional[str] = None,
    code: Optional[str] = None,
    pk_code: Optional[str] = None,
    dose_code: Optional[str] = None,
    file: Optional[str] = None,
    func: Optional[Callable] = None,
    state_init: Optional[List[float]] = None,
    parameters: Optional[Dict[str, float]] = None,
    reparametrization: Optional[Dict] = None,
    mixture: Optional[Dict] = None,
    units: Optional[Dict[str, str]] = None,
    size: Optional[int] = None,
    lagtime: Optional[float] = None,
    obs: Optional[Dict] = None,
    dose: Optional[Dict] = None,
    covariates: Optional[Union[List[str], Dict]] = None,
    declare_variables: Optional[List[str]] = None,
    fixed: Optional[List[str]] = None,
    iiv: Optional[Dict] = None,
    iov: Optional[Dict] = None,
    development: Optional[Dict] = None,
    omega_matrix: Optional[np.ndarray] = None,
    ruv: Optional[Dict] = None,
    ltbs: bool = False,
    misc: Optional[Dict] = None,
    cmt_mapping: Optional[Dict[str, int]] = None,
    int_step_size: Optional[float] = None,
    default_parameters: Optional[Dict] = None,
    cpp_show_code: bool = False,
    package: Optional[str] = None,
    test_file: Optional[str] = None,
    install: bool = False,
    folder: Optional[str] = None,
    lib_location: Optional[str] = None,
    verbose: bool = False,
    as_is: bool = False,
    nonmem: Optional[str] = None,
    comments: Optional[str] = None,
    version: Optional[str] = None,
    quiet: bool = True,
    definition: Optional[str] = None
) -> Dict[str, Any]:
    """
    Create new ODE model
    
    Parameters
    ----------
    model : str, optional
        Model name from model library
    code : str, optional
        Python code specifying ODE system
    pk_code : str, optional
        Python code called at any event
    dose_code : str, optional
        Python code called at dose event only
    file : str, optional
        File containing Python code
    func : callable, optional
        Python function to be used with scipy.integrate
    state_init : list of float, optional
        Vector of initial state values
    parameters : dict, optional
        Dictionary of parameter values
    reparametrization : dict, optional
        Parameters with definitions for reparametrization
    mixture : dict, optional
        For mixture models, parameter and values/probabilities
    units : dict, optional
        Dictionary of parameter units
    size : int, optional
        Size of state vector for model
    lagtime : float, optional
        Lag time
    obs : dict, optional
        Observation specification with 'scale' and 'cmt'
    dose : dict, optional
        Default dose compartment specification
    covariates : list or dict, optional
        Covariate specifications
    declare_variables : list, optional
        Variables to declare
    fixed : list, optional
        Parameters that should not have IIV added
    iiv : dict, optional
        Inter-individual variability
    iov : dict, optional
        Inter-occasion variability
    development : dict, optional
        Model development population information
    omega_matrix : np.ndarray, optional
        Variance-covariance matrix for IIV
    ruv : dict, optional
        Residual variability
    ltbs : bool, default False
        Log-transform both sides
    misc : dict, optional
        Miscellaneous model metadata
    cmt_mapping : dict, optional
        Administration route to compartment mapping
    int_step_size : float, optional
        Integration step size
    default_parameters : dict, optional
        Default parameter values
    cpp_show_code : bool, default False
        Show generated code (not applicable for Python)
    package : str, optional
        Package name when saving as package
    test_file : str, optional
        Test file to include with package
    install : bool, default False
        Install package after creation
    folder : str, optional
        Base folder name to create package in
    lib_location : str, optional
        Library install location
    verbose : bool, default False
        Show verbose output
    as_is : bool, default False
        Use code as-is
    nonmem : str, optional
        NONMEM code as attribute
    comments : str, optional
        Model comments
    version : str, optional
        Model version
    quiet : bool, default True
        Suppress output
    definition : str, optional
        JSON definition file
        
    Returns
    -------
    dict
        Model object dictionary
    """
    
    # Initialize model object
    model_obj = {
        'class': 'ode_model',
        'created': True
    }
    
    # Handle model from library
    if model is not None:
        model_obj.update(_load_library_model(model))
        return model_obj
    
    # Handle code from file
    if file is not None:
        if os.path.exists(file):
            with open(file, 'r') as f:
                code = f.read()
        else:
            raise FileNotFoundError(f"Model file not found: {file}")
    
    # Handle function specification
    if func is not None:
        model_obj['ode_func'] = func
        model_obj['func_type'] = 'python'
    elif code is not None:
        model_obj['code'] = code
        model_obj['ode_func'] = _compile_ode_code(code)
        model_obj['func_type'] = 'compiled'
    else:
        raise ValueError("Either 'func', 'code', 'file', or 'model' must be specified")
    
    # Set basic properties
    if state_init is not None:
        model_obj['state_init'] = np.array(state_init)
    else:
        # Default initial state (will be determined from model size)
        if size is not None:
            model_obj['state_init'] = np.zeros(size)
        else:
            model_obj['state_init'] = np.zeros(3)  # Default 3-compartment
    
    if parameters is not None:
        model_obj['parameters'] = parameters
    else:
        model_obj['parameters'] = {}
    
    # Set optional properties
    model_obj['pk_code'] = pk_code
    model_obj['dose_code'] = dose_code
    model_obj['reparametrization'] = reparametrization
    model_obj['mixture'] = mixture
    model_obj['units'] = units
    model_obj['lagtime'] = lagtime
    model_obj['obs'] = obs or {'scale': '1', 'cmt': 1}
    model_obj['dose'] = dose or {'cmt': 1}
    model_obj['covariates'] = covariates
    model_obj['declare_variables'] = declare_variables
    model_obj['fixed'] = fixed
    model_obj['iiv'] = iiv
    model_obj['iov'] = iov
    model_obj['development'] = development
    model_obj['omega_matrix'] = omega_matrix
    model_obj['ruv'] = ruv
    model_obj['ltbs'] = ltbs
    model_obj['misc'] = misc
    model_obj['cmt_mapping'] = cmt_mapping
    model_obj['int_step_size'] = int_step_size
    model_obj['default_parameters'] = default_parameters
    model_obj['nonmem'] = nonmem
    model_obj['comments'] = comments
    model_obj['version'] = version
    
    # Determine model size
    if size is not None:
        model_obj['size'] = size
    else:
        model_obj['size'] = len(model_obj['state_init'])
    
    if verbose:
        print(f"Created ODE model with {model_obj['size']} compartments")
        if parameters:
            print(f"Parameters: {list(parameters.keys())}")
    
    return model_obj


def _load_library_model(model_name: str) -> Dict[str, Any]:
    """Load a model from the built-in library"""
    
    # This would load from the models directory
    # For now, return some common PK models
    
    if model_name == "1cmt_iv":
        return _get_1cmt_iv_model()
    elif model_name == "2cmt_iv":
        return _get_2cmt_iv_model()
    elif model_name == "3cmt_iv":
        return _get_3cmt_iv_model()
    elif model_name == "1cmt_oral":
        return _get_1cmt_oral_model()
    else:
        raise ValueError(f"Model '{model_name}' not found in library")


def _get_1cmt_iv_model() -> Dict[str, Any]:
    """One-compartment IV model"""
    
    def ode_1cmt_iv(t, y, p):
        """One-compartment model ODEs"""
        A1 = y[0]  # Amount in central compartment
        
        dAdt = np.zeros(1)
        dAdt[0] = -p['CL'] / p['V'] * A1  # Elimination
        
        return dAdt
    
    return {
        'ode_func': ode_1cmt_iv,
        'state_init': np.array([0.0]),
        'parameters': {'CL': 1.0, 'V': 10.0},
        'size': 1,
        'obs': {'scale': 'V', 'cmt': 1},
        'dose': {'cmt': 1},
        'name': '1cmt_iv'
    }


def _get_2cmt_iv_model() -> Dict[str, Any]:
    """Two-compartment IV model"""
    
    def ode_2cmt_iv(t, y, p):
        """Two-compartment model ODEs"""
        A1, A2 = y  # Central, peripheral
        
        dAdt = np.zeros(2)
        dAdt[0] = -p['CL'] / p['V1'] * A1 - p['Q'] / p['V1'] * A1 + p['Q'] / p['V2'] * A2
        dAdt[1] = p['Q'] / p['V1'] * A1 - p['Q'] / p['V2'] * A2
        
        return dAdt
    
    return {
        'ode_func': ode_2cmt_iv,
        'state_init': np.array([0.0, 0.0]),
        'parameters': {'CL': 1.0, 'V1': 10.0, 'Q': 2.0, 'V2': 20.0},
        'size': 2,
        'obs': {'scale': 'V1', 'cmt': 1},
        'dose': {'cmt': 1},
        'name': '2cmt_iv'
    }


def _get_3cmt_iv_model() -> Dict[str, Any]:
    """Three-compartment IV model"""
    
    def ode_3cmt_iv(t, y, p):
        """Three-compartment model ODEs"""
        A1, A2, A3 = y  # Central, peripheral1, peripheral2
        
        dAdt = np.zeros(3)
        dAdt[0] = (-p['CL'] / p['V1'] * A1 - 
                   p['Q2'] / p['V1'] * A1 + p['Q2'] / p['V2'] * A2 -
                   p['Q3'] / p['V1'] * A1 + p['Q3'] / p['V3'] * A3)
        dAdt[1] = p['Q2'] / p['V1'] * A1 - p['Q2'] / p['V2'] * A2
        dAdt[2] = p['Q3'] / p['V1'] * A1 - p['Q3'] / p['V3'] * A3
        
        return dAdt
    
    return {
        'ode_func': ode_3cmt_iv,
        'state_init': np.array([0.0, 0.0, 0.0]),
        'parameters': {'CL': 1.0, 'V1': 10.0, 'Q2': 2.0, 'V2': 20.0, 'Q3': 1.0, 'V3': 50.0},
        'size': 3,
        'obs': {'scale': 'V1', 'cmt': 1},
        'dose': {'cmt': 1},
        'name': '3cmt_iv'
    }


def _get_1cmt_oral_model() -> Dict[str, Any]:
    """One-compartment oral model with absorption"""
    
    def ode_1cmt_oral(t, y, p):
        """One-compartment oral model ODEs"""
        A_depot, A_central = y
        
        dAdt = np.zeros(2)
        dAdt[0] = -p['KA'] * A_depot  # Absorption from depot
        dAdt[1] = p['KA'] * p['F'] * A_depot - p['CL'] / p['V'] * A_central  # Central compartment
        
        return dAdt
    
    return {
        'ode_func': ode_1cmt_oral,
        'state_init': np.array([0.0, 0.0]),
        'parameters': {'CL': 1.0, 'V': 10.0, 'KA': 1.0, 'F': 1.0},
        'size': 2,
        'obs': {'scale': 'V', 'cmt': 2},
        'dose': {'cmt': 1},
        'name': '1cmt_oral'
    }


def _compile_ode_code(code: str) -> Callable:
    """Compile ODE code string into a callable function"""
    
    # This is a simplified version - in practice, you'd want more sophisticated
    # code parsing and compilation
    
    namespace = {'np': np, 'numpy': np}
    
    try:
        exec(code, namespace)
        
        # Look for a function named 'ode' or the first function defined
        for name, obj in namespace.items():
            if callable(obj) and not name.startswith('_') and name != 'np' and name != 'numpy':
                return obj
        
        raise ValueError("No ODE function found in code")
        
    except Exception as e:
        raise ValueError(f"Error compiling ODE code: {e}")


def print_model_info(model: Dict[str, Any]) -> None:
    """Print information about a model"""
    
    print(f"ODE Model: {model.get('name', 'Unnamed')}")
    print(f"Compartments: {model['size']}")
    
    if 'parameters' in model:
        print(f"Parameters: {list(model['parameters'].keys())}")
    
    if 'obs' in model:
        print(f"Observation: compartment {model['obs']['cmt']}, scale = {model['obs']['scale']}")
    
    if 'dose' in model:
        print(f"Dosing: compartment {model['dose']['cmt']}")
    
    if 'covariates' in model and model['covariates']:
        print(f"Covariates: {model['covariates']}")


def get_model_parameters(model: Dict[str, Any]) -> Dict[str, float]:
    """Get model parameters"""
    return model.get('parameters', {})


def set_model_parameters(model: Dict[str, Any], parameters: Dict[str, float]) -> Dict[str, Any]:
    """Set model parameters"""
    model = model.copy()
    model['parameters'] = parameters
    return model