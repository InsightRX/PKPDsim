"""
Tests for regimen functionality
"""

import pytest
import numpy as np
import pandas as pd
from datetime import datetime, timezone
from pkpdsim.regimen import (
    new_regimen, print_regimen, regimen_to_dataframe,
    merge_regimens, shift_regimen
)


def test_basic_regimen():
    """Test basic regimen creation"""
    
    reg = new_regimen(amt=100, interval=12, n=3)
    
    assert reg['amt'] == [100, 100, 100]
    assert reg['times'] == [0, 12, 24]
    assert reg['interval'] == 12
    assert reg['n'] == 3
    assert reg['cmt'] == [1, 1, 1]


def test_regimen_with_explicit_times():
    """Test regimen with explicit dosing times"""
    
    times = [0, 6, 18, 30]
    reg = new_regimen(amt=50, times=times)
    
    assert reg['amt'] == [50, 50, 50, 50]
    assert reg['times'] == times
    assert reg['n'] == 4
    assert reg['interval'] is None


def test_regimen_varying_doses():
    """Test regimen with varying dose amounts"""
    
    amounts = [100, 75, 50, 25]
    times = [0, 12, 24, 36]
    reg = new_regimen(amt=amounts, times=times)
    
    assert reg['amt'] == amounts
    assert reg['times'] == times
    assert reg['n'] == 4


def test_regimen_single_dose():
    """Test single dose regimen"""
    
    reg = new_regimen(amt=100, times=[0])
    
    assert reg['amt'] == [100]
    assert reg['times'] == [0]
    assert reg['n'] == 1


def test_infusion_regimen():
    """Test infusion regimen creation"""
    
    reg = new_regimen(
        amt=100, 
        interval=24, 
        n=2, 
        type="infusion",
        t_inf=2.0
    )
    
    assert reg['type'] == "infusion"
    assert reg['t_inf'] == [2.0, 2.0]
    assert reg['amt'] == [100, 100]


def test_infusion_with_rate():
    """Test infusion with specified rate"""
    
    reg = new_regimen(
        amt=100,
        times=[0, 24],
        type="infusion",
        rate=50  # 50 units/hr
    )
    
    assert reg['rate'] == [50, 50]
    assert reg['t_inf'] == [2.0, 2.0]  # amt/rate = 100/50 = 2 hours


def test_lag_time():
    """Test lag time application"""
    
    reg = new_regimen(
        amt=100,
        interval=12,
        n=3,
        t_lag=1.0
    )
    
    # Times should be shifted by lag time
    assert reg['times'] == [1.0, 13.0, 25.0]
    assert reg['t_lag'] == [1.0, 1.0, 1.0]


def test_compartment_specification():
    """Test compartment specification"""
    
    reg = new_regimen(
        amt=100,
        interval=12,
        n=2,
        cmt=2
    )
    
    assert reg['cmt'] == [2, 2]


def test_varying_compartments():
    """Test varying compartments"""
    
    reg = new_regimen(
        amt=[100, 50],
        times=[0, 12],
        cmt=[1, 2]
    )
    
    assert reg['cmt'] == [1, 2]


def test_steady_state_regimen():
    """Test steady state regimen"""
    
    reg = new_regimen(
        amt=100,
        interval=12,
        n=5,
        ss=True,
        n_ss=20
    )
    
    assert reg['ss'] is True
    assert reg['n_ss'] == 20


def test_negative_dose_handling():
    """Test handling of negative doses"""
    
    # Should warn and set to 0
    reg = new_regimen(amt=[-10, 100], times=[0, 12], checks=True)
    
    assert reg['amt'] == [0, 100]


def test_regimen_to_dataframe():
    """Test conversion to DataFrame"""
    
    reg = new_regimen(amt=100, interval=12, n=3)
    df = regimen_to_dataframe(reg)
    
    assert isinstance(df, pd.DataFrame)
    assert 'time' in df.columns
    assert 'amt' in df.columns
    assert 'cmt' in df.columns
    assert 'evid' in df.columns
    assert len(df) == 3
    assert all(df['evid'] == 1)  # All dose events


def test_merge_regimens():
    """Test merging multiple regimens"""
    
    reg1 = new_regimen(amt=100, times=[0, 24])
    reg2 = new_regimen(amt=50, times=[12, 36])
    
    merged = merge_regimens([reg1, reg2])
    
    # Should be sorted by time
    assert merged['times'] == [0, 12, 24, 36]
    assert merged['amt'] == [100, 50, 100, 50]
    assert merged['n'] == 4


def test_shift_regimen():
    """Test shifting regimen times"""
    
    reg = new_regimen(amt=100, interval=12, n=3)
    shifted = shift_regimen(reg, 6.0)
    
    assert shifted['times'] == [6.0, 18.0, 30.0]
    assert shifted['amt'] == reg['amt']  # Should be unchanged


def test_multiple_infusion_times():
    """Test multiple infusion times"""
    
    reg = new_regimen(
        amt=[100, 200],
        times=[0, 24],
        type="infusion",
        t_inf=[1.0, 2.0]
    )
    
    assert reg['t_inf'] == [1.0, 2.0]


def test_first_dose_time():
    """Test first dose time specification"""
    
    first_time = datetime(2023, 1, 1, 8, 0, 0, tzinfo=timezone.utc)
    reg = new_regimen(amt=100, interval=12, n=2, first_dose_time=first_time)
    
    assert reg['first_dose_time'] == first_time


def test_regimen_validation():
    """Test regimen input validation"""
    
    # Should raise error if neither times nor interval specified
    with pytest.raises(ValueError):
        new_regimen(amt=100, n=3)  # No times or interval
    
    # Should raise error if amt and times length mismatch
    with pytest.raises(ValueError):
        new_regimen(amt=[100, 200], times=[0, 12, 24])


def test_oral_regimen():
    """Test oral dosing regimen"""
    
    reg = new_regimen(
        amt=100,
        interval=24,
        n=7,
        type="oral"
    )
    
    assert reg['type'] == "oral"
    assert reg['n'] == 7


def test_sc_regimen():
    """Test subcutaneous regimen"""
    
    reg = new_regimen(
        amt=50,
        interval=168,  # Once weekly
        n=4,
        type="sc"
    )
    
    assert reg['type'] == "sc"


def test_print_regimen(capsys):
    """Test regimen printing"""
    
    reg = new_regimen(amt=100, interval=12, n=3, type="iv")
    print_regimen(reg)
    
    captured = capsys.readouterr()
    assert "Dosing regimen:" in captured.out
    assert "Number of doses: 3" in captured.out
    assert "Dose amount: 100" in captured.out


if __name__ == "__main__":
    pytest.main([__file__])