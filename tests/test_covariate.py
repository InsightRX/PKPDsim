"""
Tests for covariate functionality
"""

import pytest
import numpy as np
import pandas as pd
from pkpdsim.covariate import (
    new_covariate, get_covariate_value, create_covariate_table,
    covariates_table_to_list, covariate_last_obs_only,
    apply_covariate_model
)


def test_time_invariant_covariate():
    """Test time-invariant covariate"""
    
    cov = new_covariate(value=70, unit="kg")
    
    assert cov['value'] == 70
    assert cov['times'] is None
    assert cov['unit'] == "kg"
    assert cov['class'] == 'covariate'


def test_time_varying_covariate_list():
    """Test time-varying covariate with lists"""
    
    cov = new_covariate(
        value=[70, 72, 68],
        times=[0, 24, 48],
        unit="kg"
    )
    
    assert cov['value'] == [70, 72, 68]
    assert cov['times'] == [0, 24, 48]
    assert cov['implementation'] == 'locf'


def test_time_varying_covariate_dict():
    """Test time-varying covariate with dict"""
    
    cov = new_covariate(
        value={0: 70, 24: 72, 48: 68},
        unit="kg"
    )
    
    assert cov['value'] == [70, 72, 68]
    assert cov['times'] == [0, 24, 48]


def test_get_covariate_value_invariant():
    """Test getting value from time-invariant covariate"""
    
    cov = new_covariate(value=70)
    
    assert get_covariate_value(cov, 0) == 70
    assert get_covariate_value(cov, 24) == 70
    assert get_covariate_value(cov, 100) == 70


def test_get_covariate_value_locf():
    """Test LOCF implementation"""
    
    cov = new_covariate(
        value=[70, 75, 65],
        times=[0, 12, 24],
        implementation='locf'
    )
    
    # Before first observation
    assert get_covariate_value(cov, -1) == 70
    
    # At observation times
    assert get_covariate_value(cov, 0) == 70
    assert get_covariate_value(cov, 12) == 75
    assert get_covariate_value(cov, 24) == 65
    
    # Between observations
    assert get_covariate_value(cov, 6) == 70
    assert get_covariate_value(cov, 18) == 75
    
    # After last observation
    assert get_covariate_value(cov, 48) == 65


def test_get_covariate_value_interpolate():
    """Test interpolation implementation"""
    
    cov = new_covariate(
        value=[70, 80, 60],
        times=[0, 12, 24],
        implementation='interpolate'
    )
    
    # At observation times
    assert get_covariate_value(cov, 0) == 70
    assert get_covariate_value(cov, 12) == 80
    assert get_covariate_value(cov, 24) == 60
    
    # Between observations (linear interpolation)
    assert get_covariate_value(cov, 6) == 75  # Halfway between 70 and 80
    assert get_covariate_value(cov, 18) == 70  # Halfway between 80 and 60


def test_create_covariate_table():
    """Test creating covariate table"""
    
    covariates = {
        'WT': new_covariate(value=70),
        'AGE': new_covariate(value=[30, 35], times=[0, 24])
    }
    
    times = [0, 12, 24]
    table = create_covariate_table(covariates, times, n_individuals=2)
    
    assert isinstance(table, pd.DataFrame)
    assert len(table) == 6  # 2 individuals × 3 times
    assert 'id' in table.columns
    assert 'time' in table.columns
    assert 'WT' in table.columns
    assert 'AGE' in table.columns
    
    # Check weight is constant
    assert all(table['WT'] == 70)
    
    # Check age changes
    assert table.loc[table['time'] == 0, 'AGE'].iloc[0] == 30
    assert table.loc[table['time'] == 12, 'AGE'].iloc[0] == 30  # LOCF
    assert table.loc[table['time'] == 24, 'AGE'].iloc[0] == 35


def test_covariates_table_to_list():
    """Test converting covariate table to list"""
    
    # Create sample table
    data = [
        {'id': 1, 'time': 0, 'WT': 70, 'AGE': 30},
        {'id': 1, 'time': 24, 'WT': 72, 'AGE': 30},
        {'id': 2, 'time': 0, 'WT': 80, 'AGE': 40},
        {'id': 2, 'time': 24, 'WT': 78, 'AGE': 40}
    ]
    table = pd.DataFrame(data)
    
    cov_list = covariates_table_to_list(table)
    
    assert len(cov_list) == 2  # Two individuals
    
    # Check first individual
    ind1_cov = cov_list[0]
    assert 'WT' in ind1_cov
    assert 'AGE' in ind1_cov
    assert ind1_cov['WT']['value'] == [70, 72]
    assert ind1_cov['WT']['times'] == [0, 24]
    assert ind1_cov['AGE']['value'] == [30, 30]


def test_covariate_last_obs_only():
    """Test converting to last observation only"""
    
    # Time-varying covariate
    cov_varying = new_covariate(
        value=[70, 75, 80],
        times=[0, 12, 24]
    )
    
    cov_last = covariate_last_obs_only(cov_varying)
    
    assert cov_last['value'] == 80
    assert cov_last['times'] is None
    
    # Time-invariant covariate (should be unchanged)
    cov_invariant = new_covariate(value=70)
    cov_unchanged = covariate_last_obs_only(cov_invariant)
    
    assert cov_unchanged['value'] == 70
    assert cov_unchanged['times'] is None


def test_apply_covariate_model():
    """Test applying covariate model"""
    
    parameters = {'CL': 1.0, 'V': 10.0}
    
    covariates = {
        'WT': new_covariate(value=80),  # 80 kg vs 70 kg reference
        'AGE': new_covariate(value=30)
    }
    
    covariate_model = {
        'CL': 'CL * (WT/70)**0.75',
        'V': 'V * (WT/70)'
    }
    
    new_params = apply_covariate_model(parameters, covariates, covariate_model)
    
    # Check that CL and V were modified
    assert new_params['CL'] != parameters['CL']
    assert new_params['V'] != parameters['V']
    
    # Check calculations
    wt_ratio = 80/70
    expected_cl = 1.0 * (wt_ratio ** 0.75)
    expected_v = 10.0 * wt_ratio
    
    assert abs(new_params['CL'] - expected_cl) < 1e-6
    assert abs(new_params['V'] - expected_v) < 1e-6


def test_apply_covariate_model_time_varying():
    """Test covariate model with time-varying covariates"""
    
    parameters = {'CL': 1.0}
    
    covariates = {
        'WT': new_covariate(value=[70, 80], times=[0, 24])
    }
    
    covariate_model = {
        'CL': 'CL * (WT/70)**0.75'
    }
    
    # At time 0
    params_t0 = apply_covariate_model(parameters, covariates, covariate_model, time=0)
    assert abs(params_t0['CL'] - 1.0) < 1e-6  # No change at reference weight
    
    # At time 24
    params_t24 = apply_covariate_model(parameters, covariates, covariate_model, time=24)
    expected_cl = 1.0 * ((80/70) ** 0.75)
    assert abs(params_t24['CL'] - expected_cl) < 1e-6


def test_covariate_validation():
    """Test covariate input validation"""
    
    # Mismatched value and times lengths
    with pytest.raises(ValueError):
        new_covariate(value=[70, 80], times=[0])
    
    # Missing times for time-varying covariate
    with pytest.raises(ValueError):
        new_covariate(value=[70, 80])


def test_covariate_sorting():
    """Test that covariates are sorted by time"""
    
    # Unsorted input
    cov = new_covariate(
        value=[80, 70, 75],
        times=[24, 0, 12]
    )
    
    # Should be sorted
    assert cov['times'] == [0, 12, 24]
    assert cov['value'] == [70, 75, 80]


def test_unknown_implementation():
    """Test error for unknown implementation method"""
    
    cov = new_covariate(
        value=[70, 80],
        times=[0, 24],
        implementation='unknown'
    )
    
    with pytest.raises(ValueError):
        get_covariate_value(cov, 12)


def test_covariate_with_comments():
    """Test covariate with comments"""
    
    cov = new_covariate(
        value=70,
        unit="kg",
        comments="Body weight at baseline"
    )
    
    assert cov['comments'] == "Body weight at baseline"


def test_interpolation_types():
    """Test different interpolation types"""
    
    cov = new_covariate(
        value=[70, 80],
        times=[0, 24],
        implementation='interpolate',
        interpolation='linear'
    )
    
    assert cov['interpolation'] == 'linear'


def test_edge_case_single_timepoint():
    """Test covariate with single timepoint"""
    
    cov = new_covariate(
        value=[70],
        times=[0]
    )
    
    # Should work like time-invariant
    assert get_covariate_value(cov, -10) == 70
    assert get_covariate_value(cov, 0) == 70
    assert get_covariate_value(cov, 100) == 70


if __name__ == "__main__":
    pytest.main([__file__])