"""
Tests for model functionality
"""

import pytest
import numpy as np
from pkpdsim.models import (
    new_ode_model, print_model_info, get_model_parameters, 
    set_model_parameters
)


def test_library_model_1cmt_iv():
    """Test 1-compartment IV model from library"""
    
    model = new_ode_model(model="1cmt_iv")
    
    assert model['name'] == '1cmt_iv'
    assert model['size'] == 1
    assert 'CL' in model['parameters']
    assert 'V' in model['parameters']
    assert model['obs']['cmt'] == 1
    assert model['dose']['cmt'] == 1
    assert callable(model['ode_func'])


def test_library_model_2cmt_iv():
    """Test 2-compartment IV model from library"""
    
    model = new_ode_model(model="2cmt_iv")
    
    assert model['name'] == '2cmt_iv'
    assert model['size'] == 2
    assert 'CL' in model['parameters']
    assert 'V1' in model['parameters']
    assert 'Q' in model['parameters']
    assert 'V2' in model['parameters']


def test_library_model_3cmt_iv():
    """Test 3-compartment IV model from library"""
    
    model = new_ode_model(model="3cmt_iv")
    
    assert model['name'] == '3cmt_iv'
    assert model['size'] == 3
    assert len(model['parameters']) == 6  # CL, V1, Q2, V2, Q3, V3


def test_library_model_1cmt_oral():
    """Test 1-compartment oral model from library"""
    
    model = new_ode_model(model="1cmt_oral")
    
    assert model['name'] == '1cmt_oral'
    assert model['size'] == 2  # Depot + central
    assert 'KA' in model['parameters']
    assert 'F' in model['parameters']
    assert model['obs']['cmt'] == 2  # Observe central compartment
    assert model['dose']['cmt'] == 1  # Dose to depot


def test_custom_function_model():
    """Test model with custom ODE function"""
    
    def my_ode(t, y, p):
        return np.array([-p['k'] * y[0]])
    
    model = new_ode_model(
        func=my_ode,
        parameters={'k': 0.1},
        state_init=[0.0]
    )
    
    assert callable(model['ode_func'])
    assert model['parameters']['k'] == 0.1
    assert len(model['state_init']) == 1


def test_custom_code_model():
    """Test model with code string"""
    
    code = """
def ode_func(t, y, p):
    import numpy as np
    dAdt = np.zeros(2)
    dAdt[0] = -p['ka'] * y[0]
    dAdt[1] = p['ka'] * y[0] - p['ke'] * y[1]
    return dAdt
"""
    
    model = new_ode_model(
        code=code,
        parameters={'ka': 1.0, 'ke': 0.1},
        state_init=[0.0, 0.0]
    )
    
    assert callable(model['ode_func'])
    assert model['size'] == 2


def test_model_with_obs_specification():
    """Test model with observation specification"""
    
    model = new_ode_model(
        model="1cmt_iv",
        obs={'scale': 'V * (WT/70)', 'cmt': 1}
    )
    
    assert model['obs']['scale'] == 'V * (WT/70)'
    assert model['obs']['cmt'] == 1


def test_model_with_dose_specification():
    """Test model with dose specification"""
    
    model = new_ode_model(
        model="2cmt_iv",
        dose={'cmt': 2}
    )
    
    assert model['dose']['cmt'] == 2


def test_model_with_covariates():
    """Test model with covariates"""
    
    model = new_ode_model(
        model="1cmt_iv",
        covariates=['WT', 'AGE', 'SEX']
    )
    
    assert model['covariates'] == ['WT', 'AGE', 'SEX']


def test_model_with_iiv():
    """Test model with IIV specification"""
    
    iiv = {
        'CL': 0.2,
        'V': 0.1
    }
    
    model = new_ode_model(
        model="1cmt_iv",
        iiv=iiv
    )
    
    assert model['iiv'] == iiv


def test_model_with_ruv():
    """Test model with RUV specification"""
    
    ruv = {
        'prop': 0.1,
        'add': 0.01
    }
    
    model = new_ode_model(
        model="1cmt_iv",
        ruv=ruv
    )
    
    assert model['ruv'] == ruv


def test_model_with_omega_matrix():
    """Test model with omega matrix"""
    
    omega = np.array([[0.04, 0.01], [0.01, 0.09]])
    
    model = new_ode_model(
        model="1cmt_iv",
        omega_matrix=omega
    )
    
    np.testing.assert_array_equal(model['omega_matrix'], omega)


def test_model_with_lagtime():
    """Test model with lag time"""
    
    model = new_ode_model(
        model="1cmt_oral",
        lagtime=1.5
    )
    
    assert model['lagtime'] == 1.5


def test_model_with_fixed_parameters():
    """Test model with fixed parameters"""
    
    model = new_ode_model(
        model="1cmt_iv",
        fixed=['V']
    )
    
    assert model['fixed'] == ['V']


def test_model_with_mixture():
    """Test model with mixture specification"""
    
    mixture = {
        'CL': {
            'value': [1.0, 2.0],
            'probability': 0.3
        }
    }
    
    model = new_ode_model(
        model="1cmt_iv",
        mixture=mixture
    )
    
    assert model['mixture'] == mixture


def test_model_with_reparametrization():
    """Test model with reparametrization"""
    
    reparam = {
        'CL': 'TVCL * (WT/70)**0.75',
        'V': 'TVV * (WT/70)'
    }
    
    model = new_ode_model(
        model="1cmt_iv",
        reparametrization=reparam
    )
    
    assert model['reparametrization'] == reparam


def test_model_with_units():
    """Test model with parameter units"""
    
    units = {
        'CL': 'L/h',
        'V': 'L'
    }
    
    model = new_ode_model(
        model="1cmt_iv",
        units=units
    )
    
    assert model['units'] == units


def test_model_with_cmt_mapping():
    """Test model with compartment mapping"""
    
    cmt_mapping = {
        'oral': 1,
        'iv': 2
    }
    
    model = new_ode_model(
        model="2cmt_iv",
        cmt_mapping=cmt_mapping
    )
    
    assert model['cmt_mapping'] == cmt_mapping


def test_model_with_int_step_size():
    """Test model with integration step size"""
    
    model = new_ode_model(
        model="1cmt_iv",
        int_step_size=0.01
    )
    
    assert model['int_step_size'] == 0.01


def test_unknown_library_model():
    """Test error for unknown library model"""
    
    with pytest.raises(ValueError):
        new_ode_model(model="unknown_model")


def test_no_model_specification():
    """Test error when no model is specified"""
    
    with pytest.raises(ValueError):
        new_ode_model()  # No func, code, file, or model


def test_print_model_info(capsys):
    """Test model info printing"""
    
    model = new_ode_model(model="1cmt_iv")
    print_model_info(model)
    
    captured = capsys.readouterr()
    assert "ODE Model:" in captured.out
    assert "Compartments: 1" in captured.out
    assert "Parameters:" in captured.out


def test_get_model_parameters():
    """Test getting model parameters"""
    
    model = new_ode_model(model="1cmt_iv")
    params = get_model_parameters(model)
    
    assert isinstance(params, dict)
    assert 'CL' in params
    assert 'V' in params


def test_set_model_parameters():
    """Test setting model parameters"""
    
    model = new_ode_model(model="1cmt_iv")
    new_params = {'CL': 2.0, 'V': 20.0}
    
    updated_model = set_model_parameters(model, new_params)
    
    assert updated_model['parameters'] == new_params
    # Original model should be unchanged
    assert model['parameters'] != new_params


def test_model_ode_function_call():
    """Test calling ODE function from model"""
    
    model = new_ode_model(model="1cmt_iv")
    
    # Test ODE function call
    t = 1.0
    y = np.array([100.0])  # 100 units in compartment
    p = model['parameters']
    
    dydt = model['ode_func'](t, y, p)
    
    assert isinstance(dydt, np.ndarray)
    assert len(dydt) == 1
    assert dydt[0] < 0  # Should be eliminating


def test_2cmt_ode_function():
    """Test 2-compartment ODE function"""
    
    model = new_ode_model(model="2cmt_iv")
    
    t = 1.0
    y = np.array([100.0, 0.0])  # All drug in central compartment
    p = model['parameters']
    
    dydt = model['ode_func'](t, y, p)
    
    assert len(dydt) == 2
    assert dydt[0] < 0  # Central compartment losing drug
    assert dydt[1] > 0  # Peripheral compartment gaining drug


def test_oral_ode_function():
    """Test oral absorption ODE function"""
    
    model = new_ode_model(model="1cmt_oral")
    
    t = 1.0
    y = np.array([100.0, 0.0])  # All drug in depot
    p = model['parameters']
    
    dydt = model['ode_func'](t, y, p)
    
    assert len(dydt) == 2
    assert dydt[0] < 0  # Depot losing drug
    assert dydt[1] > 0  # Central gaining drug


if __name__ == "__main__":
    pytest.main([__file__])