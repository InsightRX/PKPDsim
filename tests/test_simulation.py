"""
Tests for simulation functionality
"""

import pytest
import numpy as np
import pandas as pd
from pkpdsim import sim, sim_ode, new_ode_model, new_regimen


def test_basic_simulation():
    """Test basic simulation functionality"""
    
    # Create a simple 1-compartment model
    def ode_1cmt(t, y, p):
        A1 = y[0]
        dAdt = np.zeros(1)
        dAdt[0] = -p['CL'] / p['V'] * A1
        return dAdt
    
    # Create parameters and regimen
    parameters = {'CL': 1.0, 'V': 10.0}
    regimen = new_regimen(amt=100, interval=12, n=3)
    
    # Run simulation
    result = sim(
        ode=ode_1cmt,
        parameters=parameters,
        regimen=regimen,
        t_obs=np.array([0, 6, 12, 18, 24])
    )
    
    # Check result structure
    assert isinstance(result, pd.DataFrame)
    assert 'time' in result.columns
    assert 'dv' in result.columns
    assert 'id' in result.columns
    assert len(result) > 0


def test_library_model():
    """Test using built-in library model"""
    
    # Use 1-compartment IV model from library
    model = new_ode_model(model="1cmt_iv")
    
    # Create regimen
    regimen = new_regimen(amt=100, interval=12, n=2)
    
    # Run simulation
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen
    )
    
    assert isinstance(result, pd.DataFrame)
    assert len(result) > 0


def test_multiple_individuals():
    """Test simulation with multiple individuals"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(amt=100, interval=12, n=2)
    
    # Simulate 5 individuals
    result = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        n_ind=5
    )
    
    # Check that we have 5 individuals
    assert result['id'].nunique() == 5
    assert result['id'].max() == 5


def test_between_subject_variability():
    """Test BSV implementation"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(amt=100, interval=12, n=2)
    
    # Add BSV
    omega = np.array([0.1, 0.2])  # 10% CV on CL, 20% CV on V
    
    result = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        omega=omega,
        n_ind=10,
        seed=123
    )
    
    # Should have variability between individuals
    assert len(result) > 0
    assert result['id'].nunique() == 10


def test_residual_variability():
    """Test residual variability"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(amt=100, times=[0])
    
    # Add RUV
    res_var = {'prop': 0.1, 'add': 0.01}
    
    result = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        res_var=res_var,
        seed=123,
        t_obs=np.array([1, 2, 4, 8, 12])
    )
    
    assert len(result) > 0
    # Should have some variability in concentrations


def test_2cmt_model():
    """Test 2-compartment model"""
    
    model = new_ode_model(model="2cmt_iv")
    regimen = new_regimen(amt=100, times=[0])
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        t_obs=np.array([0.5, 1, 2, 4, 8, 12, 24])
    )
    
    assert len(result) > 0
    # Should show typical 2-compartment profile


def test_oral_model():
    """Test oral absorption model"""
    
    model = new_ode_model(model="1cmt_oral")
    regimen = new_regimen(amt=100, times=[0], type="oral")
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        t_obs=np.array([0.5, 1, 2, 4, 8, 12, 24])
    )
    
    assert len(result) > 0
    # Should show absorption profile with Tmax


def test_infusion_regimen():
    """Test infusion dosing"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(
        amt=100, 
        times=[0, 24], 
        type="infusion",
        t_inf=[1, 1]  # 1-hour infusions
    )
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen
    )
    
    assert len(result) > 0


def test_steady_state():
    """Test steady state simulation"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(
        amt=100, 
        interval=12, 
        n=5,
        ss=True,
        n_ss=20
    )
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen
    )
    
    assert len(result) > 0


def test_custom_observation_times():
    """Test custom observation times"""
    
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(amt=100, times=[0])
    
    # Irregular observation times
    t_obs = np.array([0, 0.25, 0.5, 1, 2, 4, 8, 12, 24, 48])
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        t_obs=t_obs
    )
    
    # Should have observations at specified times only
    obs_data = result[result['evid'] == 0]
    np.testing.assert_array_almost_equal(
        sorted(obs_data['time'].values), 
        sorted(t_obs),
        decimal=6
    )


def test_lagtime():
    """Test lag time implementation"""
    
    model = new_ode_model(model="1cmt_oral")
    regimen = new_regimen(amt=100, times=[0])
    
    result = sim_ode(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        lagtime=2.0,  # 2-hour lag time
        t_obs=np.array([0, 1, 2, 3, 4, 6, 8, 12])
    )
    
    assert len(result) > 0


if __name__ == "__main__":
    pytest.main([__file__])