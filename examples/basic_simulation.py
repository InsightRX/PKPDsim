#!/usr/bin/env python3
"""
Basic PKPDsim Simulation Example

This example demonstrates basic usage of the PKPDsim Python package
for pharmacokinetic simulations.
"""

import numpy as np
import matplotlib.pyplot as plt
from pkpdsim import sim, new_ode_model, new_regimen


def main():
    """Run basic simulation examples"""
    
    print("PKPDsim Python Package - Basic Examples")
    print("=" * 50)
    
    # Example 1: Simple 1-compartment IV simulation
    print("\n1. One-compartment IV bolus simulation")
    print("-" * 40)
    
    # Load built-in 1-compartment model
    model = new_ode_model(model="1cmt_iv")
    print(f"Model parameters: {model['parameters']}")
    
    # Create dosing regimen: 100mg IV bolus at time 0
    regimen = new_regimen(amt=100, times=[0])
    print(f"Regimen: {regimen['amt']}mg at times {regimen['times']}")
    
    # Define observation times
    t_obs = np.array([0, 0.5, 1, 2, 4, 8, 12, 24])
    
    # Run simulation
    result = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        t_obs=t_obs,
        only_obs=True
    )
    
    print("\nSimulation results:")
    print(result)
    
    # Plot results
    plt.figure(figsize=(10, 6))
    plt.subplot(1, 2, 1)
    plt.plot(result['time'], result['dv'], 'o-', linewidth=2, markersize=6)
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('1-Compartment IV Bolus')
    plt.grid(True, alpha=0.3)
    
    # Example 2: Multiple dosing simulation
    print("\n2. Multiple dosing simulation")
    print("-" * 40)
    
    # Multiple dose regimen: 50mg every 12 hours for 5 doses
    regimen_multiple = new_regimen(amt=50, interval=12, n=5)
    print(f"Multiple dose regimen: {regimen_multiple['amt']}mg every {regimen_multiple['interval']}h for {regimen_multiple['n']} doses")
    
    # Simulate over 3 days
    t_obs_multiple = np.linspace(0, 72, 100)
    
    result_multiple = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen_multiple,
        t_obs=t_obs_multiple
    )
    
    # Filter to observations only
    obs_data = result_multiple[result_multiple['evid'] == 0]
    
    plt.subplot(1, 2, 2)
    plt.plot(obs_data['time'], obs_data['dv'], '-', linewidth=2)
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Multiple Dosing (50mg q12h)')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Example 3: Two-compartment model
    print("\n3. Two-compartment IV simulation")
    print("-" * 40)
    
    model_2cmt = new_ode_model(model="2cmt_iv")
    print(f"2-compartment parameters: {model_2cmt['parameters']}")
    
    regimen_2cmt = new_regimen(amt=100, times=[0])
    
    result_2cmt = sim(
        ode=model_2cmt['ode_func'],
        parameters=model_2cmt['parameters'],
        regimen=regimen_2cmt,
        t_obs=np.linspace(0, 24, 100)
    )
    
    obs_2cmt = result_2cmt[result_2cmt['evid'] == 0]
    
    plt.figure(figsize=(8, 6))
    plt.plot(obs_2cmt['time'], obs_2cmt['dv'], '-', linewidth=2, label='2-compartment')
    
    # Compare with 1-compartment
    result_1cmt_comp = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen_2cmt,
        t_obs=np.linspace(0, 24, 100)
    )
    obs_1cmt_comp = result_1cmt_comp[result_1cmt_comp['evid'] == 0]
    plt.plot(obs_1cmt_comp['time'], obs_1cmt_comp['dv'], '--', linewidth=2, label='1-compartment')
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('1-compartment vs 2-compartment Models')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.yscale('log')
    plt.show()
    
    # Example 4: Oral absorption model
    print("\n4. Oral absorption simulation")
    print("-" * 40)
    
    model_oral = new_ode_model(model="1cmt_oral")
    print(f"Oral model parameters: {model_oral['parameters']}")
    
    regimen_oral = new_regimen(amt=100, times=[0], type="oral")
    
    result_oral = sim(
        ode=model_oral['ode_func'],
        parameters=model_oral['parameters'],
        regimen=regimen_oral,
        t_obs=np.linspace(0, 24, 100)
    )
    
    obs_oral = result_oral[result_oral['evid'] == 0]
    
    plt.figure(figsize=(8, 6))
    plt.plot(obs_oral['time'], obs_oral['dv'], '-', linewidth=2, color='green')
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Oral Absorption (1-compartment)')
    plt.grid(True, alpha=0.3)
    
    # Find and mark Cmax
    max_idx = np.argmax(obs_oral['dv'])
    tmax = obs_oral.iloc[max_idx]['time']
    cmax = obs_oral.iloc[max_idx]['dv']
    plt.plot(tmax, cmax, 'ro', markersize=8, label=f'Tmax={tmax:.1f}h, Cmax={cmax:.2f}')
    plt.legend()
    plt.show()
    
    print(f"Tmax: {tmax:.2f} hours")
    print(f"Cmax: {cmax:.2f} units")
    
    # Example 5: Custom parameters
    print("\n5. Simulation with custom parameters")
    print("-" * 40)
    
    # Modify clearance and volume
    custom_params = model['parameters'].copy()
    custom_params['CL'] = 0.5  # Lower clearance
    custom_params['V'] = 20.0  # Larger volume
    
    print(f"Custom parameters: {custom_params}")
    
    result_custom = sim(
        ode=model['ode_func'],
        parameters=custom_params,
        regimen=regimen,
        t_obs=t_obs
    )
    
    plt.figure(figsize=(8, 6))
    plt.plot(result['time'], result['dv'], 'o-', label='Original parameters', linewidth=2)
    plt.plot(result_custom['time'], result_custom['dv'], 's-', label='Custom parameters', linewidth=2)
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Effect of Parameter Changes')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.yscale('log')
    plt.show()
    
    print("\nAll examples completed successfully!")


if __name__ == "__main__":
    main()