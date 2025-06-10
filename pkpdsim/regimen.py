"""
Dosing regimen functionality for PKPDsim
"""

import numpy as np
import pandas as pd
from datetime import datetime, timezone
from typing import List, Optional, Union, Dict, Any


def new_regimen(
    amt: Union[float, List[float]] = 100,
    interval: Optional[float] = None,
    n: int = 3,
    times: Optional[List[float]] = None,
    type: Optional[str] = None,
    t_inf: Optional[Union[float, List[float]]] = None,
    rate: Optional[Union[float, List[float]]] = None,
    t_lag: Optional[Union[float, List[float]]] = None,
    cmt: Optional[Union[int, List[int]]] = None,
    checks: bool = True,
    ss: bool = False,
    n_ss: Optional[int] = None,
    first_dose_time: Optional[datetime] = None
) -> Dict[str, Any]:
    """
    Create a dosing regimen for simulation
    
    Parameters
    ----------
    amt : float or list of float, default 100
        Dosing amount, either a single value or vector with doses for each administration
    interval : float, optional
        Dosing interval (requires n as argument)
    n : int, default 3
        Number of doses (requires interval as argument)
    times : list of float, optional
        Vector describing dosing times. Overrides times calculated from interval and n
    type : str, optional
        Dose type: "infusion", "bolus", "oral", "sc" (subcutaneous), or "im" (intramuscular)
    t_inf : float or list of float, optional
        Infusion time (if type=="infusion")
    rate : float or list of float, optional
        Infusion rate (if type=="infusion"). Overrides t_inf if specified
    t_lag : float or list of float, optional
        Lag time (can be applied to any dose type). Added to times
    cmt : int or list of int, optional
        Vector of dosing compartments
    checks : bool, default True
        Perform input checks
    ss : bool, default False
        Steady state simulation flag
    n_ss : int, optional
        Number of doses to simulate before assumed steady state
    first_dose_time : datetime, optional
        Datetime stamp of first dose. Default is current UTC time
        
    Returns
    -------
    dict
        Dictionary containing regimen information
        
    Examples
    --------
    >>> r1 = new_regimen(amt=50, interval=12, n=20)  # 50mg q12hrs for 10 days
    >>> r2 = new_regimen(amt=50, times=[0, 12, 24, 36, 48])  # explicit times
    >>> r3 = new_regimen(amt=[100]*4 + [50]*16, times=list(range(0, 240, 12)))  # varying doses
    """
    
    if checks:
        # Input validation
        if isinstance(amt, (int, float)):
            if amt < 0:
                amt = 0
                print("Warning: Dose was < 0, setting to 0.")
        elif isinstance(amt, list):
            amt = [max(0, a) for a in amt]
            if any(a != orig for a, orig in zip(amt, amt)):
                print("Warning: Some doses were < 0, setting to 0.")
    
    # Handle first dose time
    if first_dose_time is None:
        first_dose_time = datetime.now(timezone.utc)
    
    # Calculate dosing times
    if times is None:
        if interval is None:
            raise ValueError("Either 'times' or 'interval' must be specified")
        times = [i * interval for i in range(n)]
    else:
        n = len(times)
    
    # Ensure amt is a list matching the number of doses
    if not isinstance(amt, list):
        amt = [amt] * n
    elif len(amt) != n:
        if len(amt) == 1:
            amt = amt * n
        else:
            raise ValueError("Length of 'amt' must match number of doses")
    
    # Handle infusion parameters
    if t_inf is not None:
        if not isinstance(t_inf, list):
            t_inf = [t_inf] * n
        elif len(t_inf) != n:
            raise ValueError("Length of 't_inf' must match number of doses")
    
    if rate is not None:
        if not isinstance(rate, list):
            rate = [rate] * n
        elif len(rate) != n:
            raise ValueError("Length of 'rate' must match number of doses")
        
        # If rate is specified, calculate t_inf
        if t_inf is None:
            t_inf = [a / r if r > 0 else 0 for a, r in zip(amt, rate)]
    
    # Handle lag time
    if t_lag is not None:
        if not isinstance(t_lag, list):
            t_lag = [t_lag] * n
        elif len(t_lag) != n:
            raise ValueError("Length of 't_lag' must match number of doses")
        
        # Apply lag time to dosing times
        times = [t + lag for t, lag in zip(times, t_lag)]
    
    # Handle compartment
    if cmt is not None:
        if not isinstance(cmt, list):
            cmt = [cmt] * n
        elif len(cmt) != n:
            raise ValueError("Length of 'cmt' must match number of doses")
    else:
        cmt = [1] * n  # Default to compartment 1
    
    # Handle steady state
    if ss and n_ss is None:
        if interval is not None:
            n_ss = max(10, int(4 * 24 / interval))  # Default: 4 days worth of doses
        else:
            n_ss = 10
    
    # Create regimen dictionary
    regimen = {
        'amt': amt,
        'times': times,
        'interval': interval,
        'n': n,
        'type': type,
        't_inf': t_inf,
        'rate': rate,
        't_lag': t_lag,
        'cmt': cmt,
        'ss': ss,
        'n_ss': n_ss,
        'first_dose_time': first_dose_time,
        'class': 'regimen'
    }
    
    return regimen


def print_regimen(regimen: Dict[str, Any]) -> None:
    """
    Print regimen information in a readable format
    
    Parameters
    ----------
    regimen : dict
        Regimen dictionary created by new_regimen()
    """
    
    print("Dosing regimen:")
    print(f"  Number of doses: {regimen['n']}")
    
    if isinstance(regimen['amt'], list) and len(set(regimen['amt'])) == 1:
        print(f"  Dose amount: {regimen['amt'][0]}")
    else:
        print(f"  Dose amounts: {regimen['amt']}")
    
    if regimen['interval'] is not None:
        print(f"  Dosing interval: {regimen['interval']}")
    
    print(f"  Dosing times: {regimen['times']}")
    
    if regimen['type'] is not None:
        print(f"  Dose type: {regimen['type']}")
    
    if regimen['t_inf'] is not None:
        print(f"  Infusion time: {regimen['t_inf']}")
    
    if regimen['rate'] is not None:
        print(f"  Infusion rate: {regimen['rate']}")
    
    if regimen['cmt'] is not None:
        if len(set(regimen['cmt'])) == 1:
            print(f"  Compartment: {regimen['cmt'][0]}")
        else:
            print(f"  Compartments: {regimen['cmt']}")
    
    if regimen['ss']:
        print(f"  Steady state: Yes (n_ss={regimen['n_ss']})")


def regimen_to_dataframe(regimen: Dict[str, Any]) -> pd.DataFrame:
    """
    Convert regimen to DataFrame format
    
    Parameters
    ----------
    regimen : dict
        Regimen dictionary created by new_regimen()
        
    Returns
    -------
    pd.DataFrame
        DataFrame with columns: time, amt, cmt, evid, t_inf, rate
    """
    
    data = {
        'time': regimen['times'],
        'amt': regimen['amt'],
        'cmt': regimen['cmt'],
        'evid': [1] * regimen['n'],  # 1 = dose event
    }
    
    if regimen['t_inf'] is not None:
        data['t_inf'] = regimen['t_inf']
    
    if regimen['rate'] is not None:
        data['rate'] = regimen['rate']
    
    return pd.DataFrame(data)


def merge_regimens(regimens: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Merge multiple regimens into a single regimen
    
    Parameters
    ----------
    regimens : list of dict
        List of regimen dictionaries to merge
        
    Returns
    -------
    dict
        Merged regimen dictionary
    """
    
    if not regimens:
        raise ValueError("At least one regimen must be provided")
    
    # Collect all dosing information
    all_times = []
    all_amt = []
    all_cmt = []
    all_t_inf = []
    all_rate = []
    
    for reg in regimens:
        all_times.extend(reg['times'])
        all_amt.extend(reg['amt'])
        all_cmt.extend(reg.get('cmt', [1] * reg['n']))
        
        if reg.get('t_inf') is not None:
            all_t_inf.extend(reg['t_inf'])
        else:
            all_t_inf.extend([None] * reg['n'])
            
        if reg.get('rate') is not None:
            all_rate.extend(reg['rate'])
        else:
            all_rate.extend([None] * reg['n'])
    
    # Sort by time
    sorted_indices = np.argsort(all_times)
    
    merged_regimen = {
        'times': [all_times[i] for i in sorted_indices],
        'amt': [all_amt[i] for i in sorted_indices],
        'cmt': [all_cmt[i] for i in sorted_indices],
        'n': len(all_times),
        'interval': None,  # Can't determine single interval from merged regimens
        'type': None,      # May be mixed types
        't_inf': [all_t_inf[i] for i in sorted_indices] if any(t is not None for t in all_t_inf) else None,
        'rate': [all_rate[i] for i in sorted_indices] if any(r is not None for r in all_rate) else None,
        't_lag': None,
        'ss': False,
        'n_ss': None,
        'first_dose_time': min(reg.get('first_dose_time', datetime.now(timezone.utc)) for reg in regimens),
        'class': 'regimen'
    }
    
    return merged_regimen


def shift_regimen(regimen: Dict[str, Any], shift: float) -> Dict[str, Any]:
    """
    Shift regimen times by a specified amount
    
    Parameters
    ----------
    regimen : dict
        Regimen dictionary to shift
    shift : float
        Time shift to apply (can be negative)
        
    Returns
    -------
    dict
        Shifted regimen dictionary
    """
    
    shifted_regimen = regimen.copy()
    shifted_regimen['times'] = [t + shift for t in regimen['times']]
    
    return shifted_regimen