"""
PKPDsim: Tools for Performing Pharmacokinetic-Pharmacodynamic Simulations

This package provides tools for simulating dose regimens for 
pharmacokinetic-pharmacodynamic (PK-PD) models described by 
differential equation (DE) systems.
"""

__version__ = "1.4.1"
__author__ = "Ron Keizer, Jasmine Hughes, Dominic Tong, Kara Woo, Jordan Brooks"
__email__ = "ron@insight-rx.com"

# Import main functions
from .simulation import sim, sim_ode
from .models import new_ode_model
from .regimen import new_regimen
from .covariate import new_covariate
from .utils import *

__all__ = [
    'sim',
    'sim_ode', 
    'new_ode_model',
    'new_regimen',
    'new_covariate',
]