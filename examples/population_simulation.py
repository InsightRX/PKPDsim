#!/usr/bin/env python3
"""
Population Simulation Example

This example demonstrates population pharmacokinetic simulation
with between-subject variability and residual error.
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from pkpdsim import sim, new_ode_model, new_regimen
from pkpdsim.utils import cv_to_omega, calculate_auc, calculate_cmax_tmax


def main():
    """Run population simulation examples"""
    
    print("PKPDsim Population Simulation Examples")
    print("=" * 50)
    
    # Set random seed for reproducibility
    np.random.seed(123)
    
    # Example 1: Basic population simulation with BSV
    print("\n1. Population simulation with between-subject variability")
    print("-" * 60)
    
    # Model and regimen
    model = new_ode_model(model="1cmt_iv")
    regimen = new_regimen(amt=100, times=[0])
    
    # Define between-subject variability (BSV)
    cv_cl = 0.3  # 30% CV on clearance
    cv_v = 0.2   # 20% CV on volume
    omega = cv_to_omega([cv_cl, cv_v])
    
    print(f"BSV: CL {cv_cl*100:.0f}% CV, V {cv_v*100:.0f}% CV")
    print(f"Omega vector: {omega}")
    
    # Simulate 20 individuals
    n_ind = 20
    result = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        omega=omega,
        n_ind=n_ind,
        t_obs=np.linspace(0, 24, 50)
    )
    
    # Plot individual profiles
    plt.figure(figsize=(12, 8))
    
    plt.subplot(2, 2, 1)
    for ind_id in result['id'].unique():
        ind_data = result[(result['id'] == ind_id) & (result['evid'] == 0)]
        plt.plot(ind_data['time'], ind_data['dv'], alpha=0.7, linewidth=1)
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title(f'Population Profiles (n={n_ind})')
    plt.grid(True, alpha=0.3)
    plt.yscale('log')
    
    # Example 2: Add residual variability
    print("\n2. Adding residual unexplained variability")
    print("-" * 50)
    
    # Define residual variability
    res_var = {
        'prop': 0.15,  # 15% proportional error
        'add': 0.01    # 0.01 additive error
    }
    
    print(f"Residual variability: {res_var}")
    
    result_ruv = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen,
        omega=omega,
        res_var=res_var,
        n_ind=n_ind,
        t_obs=np.linspace(0, 24, 50)
    )
    
    plt.subplot(2, 2, 2)
    for ind_id in result_ruv['id'].unique():
        ind_data = result_ruv[(result_ruv['id'] == ind_id) & (result_ruv['evid'] == 0)]
        plt.plot(ind_data['time'], ind_data['dv'], alpha=0.7, linewidth=1)
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('With Residual Variability')
    plt.grid(True, alpha=0.3)
    plt.yscale('log')
    
    # Example 3: Multiple dosing population
    print("\n3. Multiple dosing population simulation")
    print("-" * 50)
    
    regimen_multiple = new_regimen(amt=50, interval=12, n=5)
    
    result_multiple = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen_multiple,
        omega=omega,
        res_var=res_var,
        n_ind=n_ind,
        t_obs=np.linspace(0, 72, 150)
    )
    
    plt.subplot(2, 2, 3)
    for ind_id in result_multiple['id'].unique():
        ind_data = result_multiple[(result_multiple['id'] == ind_id) & (result_multiple['evid'] == 0)]
        plt.plot(ind_data['time'], ind_data['dv'], alpha=0.7, linewidth=1)
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Multiple Dosing (50mg q12h)')
    plt.grid(True, alpha=0.3)
    
    # Example 4: Population summary statistics
    print("\n4. Population summary statistics")
    print("-" * 40)
    
    # Calculate summary statistics at each time point
    obs_data = result_multiple[result_multiple['evid'] == 0]
    
    summary_stats = obs_data.groupby('time')['dv'].agg([
        'count', 'mean', 'std', 'median',
        lambda x: np.percentile(x, 5),   # 5th percentile
        lambda x: np.percentile(x, 95)   # 95th percentile
    ]).reset_index()
    
    summary_stats.columns = ['time', 'count', 'mean', 'std', 'median', 'p5', 'p95']
    
    plt.subplot(2, 2, 4)
    plt.fill_between(summary_stats['time'], summary_stats['p5'], summary_stats['p95'], 
                     alpha=0.3, label='5th-95th percentile')
    plt.plot(summary_stats['time'], summary_stats['median'], 'k-', linewidth=2, label='Median')
    plt.plot(summary_stats['time'], summary_stats['mean'], 'r--', linewidth=2, label='Mean')
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Population Summary')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Display some summary statistics
    print(f"At 12h: median = {summary_stats.loc[summary_stats['time'] == 12, 'median'].iloc[0]:.3f}")
    print(f"At 12h: mean = {summary_stats.loc[summary_stats['time'] == 12, 'mean'].iloc[0]:.3f}")
    
    # Example 5: Exposure metrics analysis
    print("\n5. Population exposure metrics")
    print("-" * 40)
    
    # Calculate AUC and Cmax for each individual
    exposure_metrics = []
    
    for ind_id in result['id'].unique():
        ind_data = result[(result['id'] == ind_id) & (result['evid'] == 0)]
        
        # Calculate AUC (0-24h)
        auc = calculate_auc(ind_data['time'].values, ind_data['dv'].values)
        
        # Calculate Cmax and Tmax
        cmax_tmax = calculate_cmax_tmax(ind_data['time'].values, ind_data['dv'].values)
        
        exposure_metrics.append({
            'id': ind_id,
            'auc_0_24': auc,
            'cmax': cmax_tmax['cmax'],
            'tmax': cmax_tmax['tmax']
        })
    
    exposure_df = pd.DataFrame(exposure_metrics)
    
    print("\nExposure metrics summary:")
    print(exposure_df.describe())
    
    # Plot exposure distributions
    fig, axes = plt.subplots(1, 3, figsize=(15, 5))
    
    axes[0].hist(exposure_df['auc_0_24'], bins=8, alpha=0.7, edgecolor='black')
    axes[0].set_xlabel('AUC (0-24h)')
    axes[0].set_ylabel('Frequency')
    axes[0].set_title('AUC Distribution')
    axes[0].grid(True, alpha=0.3)
    
    axes[1].hist(exposure_df['cmax'], bins=8, alpha=0.7, edgecolor='black')
    axes[1].set_xlabel('Cmax')
    axes[1].set_ylabel('Frequency')
    axes[1].set_title('Cmax Distribution')
    axes[1].grid(True, alpha=0.3)
    
    axes[2].scatter(exposure_df['cmax'], exposure_df['auc_0_24'], alpha=0.7)
    axes[2].set_xlabel('Cmax')
    axes[2].set_ylabel('AUC (0-24h)')
    axes[2].set_title('Cmax vs AUC')
    axes[2].grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Example 6: Different population sizes
    print("\n6. Effect of population size on variability")
    print("-" * 50)
    
    population_sizes = [10, 50, 100]
    cv_estimates = []
    
    for n in population_sizes:
        print(f"Simulating {n} individuals...")
        
        result_n = sim(
            ode=model['ode_func'],
            parameters=model['parameters'],
            regimen=regimen,
            omega=omega,
            n_ind=n,
            t_obs=np.array([12])  # Single timepoint for analysis
        )
        
        # Calculate CV of concentrations at 12h
        conc_12h = result_n[result_n['evid'] == 0]['dv'].values
        cv_observed = np.std(conc_12h) / np.mean(conc_12h)
        cv_estimates.append(cv_observed)
        
        print(f"  Observed CV at 12h: {cv_observed:.3f}")
    
    # Plot CV estimates vs population size
    plt.figure(figsize=(8, 6))
    plt.plot(population_sizes, cv_estimates, 'o-', linewidth=2, markersize=8)
    plt.axhline(y=np.sqrt(cv_cl**2 + cv_v**2), color='r', linestyle='--', 
                label=f'Expected CV ≈ {np.sqrt(cv_cl**2 + cv_v**2):.3f}')
    plt.xlabel('Population Size')
    plt.ylabel('Observed CV')
    plt.title('CV Estimation vs Population Size')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.show()
    
    print("\nPopulation simulation examples completed!")
    
    # Example 7: Large population for smooth percentiles
    print("\n7. Large population simulation (n=1000)")
    print("-" * 50)
    
    result_large = sim(
        ode=model['ode_func'],
        parameters=model['parameters'],
        regimen=regimen_multiple,
        omega=omega,
        n_ind=1000,
        t_obs=np.linspace(0, 72, 50),
        verbose=False
    )
    
    # Calculate smooth percentiles
    obs_large = result_large[result_large['evid'] == 0]
    percentiles = obs_large.groupby('time')['dv'].quantile([0.05, 0.25, 0.5, 0.75, 0.95]).unstack()
    
    plt.figure(figsize=(10, 6))
    plt.fill_between(percentiles.index, percentiles[0.05], percentiles[0.95], 
                     alpha=0.2, label='5th-95th percentile')
    plt.fill_between(percentiles.index, percentiles[0.25], percentiles[0.75], 
                     alpha=0.4, label='25th-75th percentile')
    plt.plot(percentiles.index, percentiles[0.5], 'k-', linewidth=2, label='Median')
    
    plt.xlabel('Time (hours)')
    plt.ylabel('Concentration')
    plt.title('Population Prediction Intervals (n=1000)')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.show()
    
    print("Large population simulation completed!")


if __name__ == "__main__":
    main()