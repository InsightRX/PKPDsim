# PKPDsim Python Package

A Python implementation of PKPDsim for pharmacokinetic-pharmacodynamic (PK-PD) simulations.

## Overview

PKPDsim is a library for numerical integration of ODE systems, particularly suited for pharmacokinetic-pharmacodynamic (PK-PD) mixed-effects models. This Python version provides the core functionality of the original R package with a Pythonic interface.

## Features

- **ODE-based simulations**: Simulate PK/PD models using differential equations
- **Built-in PK models**: Common 1-, 2-, and 3-compartment models
- **Dosing regimens**: Flexible dosing with IV, oral, infusion, SC, and IM routes
- **Covariates**: Time-varying and time-invariant covariate support
- **Variability**: Between-subject and residual unexplained variability
- **Population simulation**: Multi-individual simulations with variability

## Installation

### From source

```bash
# Clone the repository
git clone https://github.com/InsightRX/PKPDsim.git
cd PKPDsim

# Install in development mode
pip install -e .
```

### Dependencies

- Python 3.8+
- NumPy >= 1.21.0
- SciPy >= 1.7.0
- Pandas >= 1.3.0
- Matplotlib >= 3.4.0

## Quick Start

### Basic 1-compartment simulation

```python
import numpy as np
from pkpdsim import new_ode_model, new_regimen, sim

# Use built-in 1-compartment IV model
model = new_ode_model(model="1cmt_iv")

# Create dosing regimen: 100mg every 12 hours for 3 doses
regimen = new_regimen(amt=100, interval=12, n=3)

# Simulate
result = sim(
    ode=model['ode_func'],
    parameters=model['parameters'],
    regimen=regimen,
    t_obs=np.array([0, 1, 2, 4, 8, 12, 24, 36])
)

print(result.head())
```

### Population simulation with variability

```python
import numpy as np
from pkpdsim import sim, new_ode_model, new_regimen

# Model and regimen
model = new_ode_model(model="1cmt_iv")
regimen = new_regimen(amt=100, interval=12, n=3)

# Add between-subject variability (CV)
omega = np.array([0.2, 0.1])  # 20% CV on CL, 10% CV on V

# Add residual variability
res_var = {'prop': 0.1, 'add': 0.01}  # 10% proportional + 0.01 additive

# Simulate 50 individuals
result = sim(
    ode=model['ode_func'],
    parameters=model['parameters'],
    regimen=regimen,
    omega=omega,
    res_var=res_var,
    n_ind=50,
    seed=123
)

# Plot results
import matplotlib.pyplot as plt
for individual in result['id'].unique()[:10]:  # Plot first 10 individuals
    ind_data = result[result['id'] == individual]
    obs_data = ind_data[ind_data['evid'] == 0]  # Observations only
    plt.plot(obs_data['time'], obs_data['dv'], alpha=0.7)

plt.xlabel('Time (hours)')
plt.ylabel('Concentration')
plt.title('Population PK Simulation')
plt.show()
```

### Custom ODE model

```python
import numpy as np
from pkpdsim import sim, new_ode_model, new_regimen

# Define custom 2-compartment model
def two_cmt_ode(t, y, p):
    """Two-compartment PK model"""
    A1, A2 = y  # Central, peripheral compartments
    
    dAdt = np.zeros(2)
    dAdt[0] = -p['CL']/p['V1'] * A1 - p['Q']/p['V1'] * A1 + p['Q']/p['V2'] * A2
    dAdt[1] = p['Q']/p['V1'] * A1 - p['Q']/p['V2'] * A2
    
    return dAdt

# Create model
model = new_ode_model(
    func=two_cmt_ode,
    parameters={'CL': 2.0, 'V1': 15.0, 'Q': 1.0, 'V2': 25.0},
    state_init=[0.0, 0.0]
)

regimen = new_regimen(amt=100, times=[0])

result = sim(
    ode=model['ode_func'],
    parameters=model['parameters'],
    regimen=regimen,
    t_obs=np.linspace(0, 24, 100)
)
```

### Oral dosing with absorption

```python
from pkpdsim import sim, new_ode_model, new_regimen

# Use built-in oral model
model = new_ode_model(model="1cmt_oral")

# Oral regimen
regimen = new_regimen(
    amt=100, 
    interval=24, 
    n=7,  # Once daily for a week
    type="oral"
)

result = sim(
    ode=model['ode_func'],
    parameters=model['parameters'],
    regimen=regimen
)
```

### Working with covariates

```python
from pkpdsim import sim, new_ode_model, new_regimen, new_covariate
from pkpdsim.covariate import apply_covariate_model

# Create covariates
covariates = {
    'WT': new_covariate(value=80, unit="kg"),  # Body weight
    'AGE': new_covariate(value=35, unit="years")  # Age
}

# Define covariate model
covariate_model = {
    'CL': 'CL * (WT/70)**0.75 * (AGE/35)**(-0.5)',
    'V': 'V * (WT/70)'
}

# Apply covariate effects
model = new_ode_model(model="1cmt_iv")
base_params = model['parameters']
individual_params = apply_covariate_model(base_params, covariates, covariate_model)

regimen = new_regimen(amt=100, interval=12, n=3)

result = sim(
    ode=model['ode_func'],
    parameters=individual_params,
    regimen=regimen
)
```

### Infusion dosing

```python
from pkpdsim import sim, new_ode_model, new_regimen

model = new_ode_model(model="1cmt_iv")

# 2-hour infusions every 24 hours
regimen = new_regimen(
    amt=100,
    interval=24,
    n=3,
    type="infusion",
    t_inf=2.0  # 2-hour infusion
)

result = sim(
    ode=model['ode_func'],
    parameters=model['parameters'],
    regimen=regimen
)
```

## Built-in Models

PKPDsim includes several built-in pharmacokinetic models:

- `"1cmt_iv"`: One-compartment IV model
- `"2cmt_iv"`: Two-compartment IV model  
- `"3cmt_iv"`: Three-compartment IV model
- `"1cmt_oral"`: One-compartment oral model with first-order absorption

## API Reference

### Core Functions

- `sim()`: Main simulation function
- `sim_ode()`: ODE simulation wrapper
- `new_ode_model()`: Create ODE model objects
- `new_regimen()`: Create dosing regimens
- `new_covariate()`: Create covariate objects

### Utilities

- `cv_to_omega()`: Convert CV to omega matrix
- `calculate_auc()`: Calculate area under curve
- `mvrnorm()`: Multivariate normal random numbers

## Running Tests

```bash
# Install test dependencies
pip install pytest pytest-cov

# Run tests
pytest tests/

# Run with coverage
pytest --cov=pkpdsim tests/
```

## Examples and Tutorials

See the `examples/` directory for more detailed examples and tutorials.

## Contributing

We welcome contributions! Please see the original R package repository for contribution guidelines.

## License

MIT License - see LICENSE file for details.

## Disclaimer

This software is provided "as is" without warranty. Users should validate results for their specific applications.

## References

- Original R package: https://github.com/InsightRX/PKPDsim
- Documentation: https://insightrx.github.io/PKPDsim/