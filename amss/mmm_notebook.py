import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression, Ridge
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller

# Import our simulator
# If using this notebook standalone, download the mmm-simulator.py file first
from mmm_simulator import MarketingSimulator

# Create a complex simulation environment
def create_complex_simulation():
    """Create a complex marketing simulation environment with realistic effects"""
    # Initialize simulator
    sim = MarketingSimulator(
        population_size=5000000,  # 5 million consumers
        time_periods=156,  # 3 years of weekly data
        channels=["TV", "Search", "Social", "Display", "Radio"]
    )
    
    # Configure media effects with different carryover rates and saturation points
    sim.configure_media_effects(
        adstock_rates=[0.8, 0.4, 0.6, 0.3, 0.7],  # Carryover rates
        saturation_points=[800000, 300000, 250000, 200000, 150000], 
        media_ROI=[2.5, 2.2, 1.8, 1.5, 1.2]  # True ROI values
    )
    
    # Create realistic seasonality with annual pattern
    sim.configure_seasonality(amplitude=0.3, period=52)
    
    # Create pulsing TV strategy
    tv_flighting = np.ones(156)
    tv_flighting[::4] = 2.5  # Spike every 4 weeks
    tv_flighting[12:24] = 0.2  # Lower spend in months 3-6
    tv_flighting[64:76] = 3.0  # Higher spend in months 16-18
    
    # Search follows underlying demand somewhat
    search_flighting = 0.8 + 0.2 * np.sin(np.arange(156) * 2 * np.pi / 52)
    search_flighting += np.random.normal(0, 0.1, 156)
    
    # Create social media with gradual increase over time (growing channel)
    social_flighting = np.linspace(0.6, 1.4, 156)
    
    # Configure annual budgets with growth
    year1_budget = 5000000
    year2_budget = 5500000
    year3_budget = 6000000
    
    # Split budgets by year
    budgets = [
        0.45 * year1_budget + 0.40 * year2_budget + 0.38 * year3_budget,  # TV
        0.25 * year1_budget + 0.28 * year2_budget + 0.30 * year3_budget,  # Search
        0.10 * year1_budget + 0.15 * year2_budget + 0.18 * year3_budget,  # Social 
        0.12 * year1_budget + 0.10 * year2_budget + 0.08 * year3_budget,  # Display
        0.08 * year1_budget + 0.07 * year2_budget + 0.06 * year3_budget,  # Radio
    ]
    
    # Create default flighting patterns for Display and Radio
    display_flighting = np.ones(156)
    radio_flighting = np.ones(156)
    
    # Configure spend
    sim.configure_spend(
        budgets=budgets,
        flighting_patterns=[tv_flighting, search_flighting, social_flighting, display_flighting, radio_flighting]
    )
    
    # Configure price variations
    prices = 40 * np.ones(156)
    prices[26:39] = 38  # Price drop during certain weeks
    prices[78:91] = 37  # Another price drop
    sim.pricing = prices
    
    return sim

# Create a media mix modeling function
def build_mmm(data, include_adstock=True, include_saturation=True, ridge_alpha=0.0):
    """
    Build a media mix model based on the simulated data
    
    Args:
        data: DataFrame with simulation results
        include_adstock: Whether to include adstock transformations
        include_saturation: Whether to include saturation transformations
        ridge_alpha: Regularization parameter (0 = OLS)
    
    Returns:
        Fitted model and dataframe with predictions
    """
    # Create copy of data to avoid modifying original
    df = data.copy()
    
    # Add week of year for seasonality
    df['week_of_year'] = df['time'] % 52
    
    # Create dummy variables for week of year
    week_dummies = pd.get_dummies(df['week_of_year'], prefix='week', drop_first=True)
    df = pd.concat([df, week_dummies], axis=1)
    
    # Get media channels
    channels = [col.replace('_spend', '') for col in df.columns if col.endswith('_spend')]
    
    # Apply transformations to media variables
    if include_adstock and not include_saturation:
        # Only use pre-computed adstock values
        X_media = np.column_stack([df[f'{channel}_adstock'] for channel in channels])
        media_cols = [f'{channel}_adstock' for channel in channels]
    
    elif include_adstock and include_saturation:
        # Use both adstock and saturation
        # We'll use Hill transformation (already calculated in simulator)
        X_media = np.column_stack([df[f'{channel}_adstock'] for channel in channels])
        media_cols = [f'{channel}_adstock' for channel in channels]
    
    else:
        # Just use raw spend
        X_media = np.column_stack([df[f'{channel}_spend'] for channel in channels])
        media_cols = [f'{channel}_spend' for channel in channels]
    
    # Add price
    df['price_log'] = np.log(df['price'])
    
    # Create full feature matrix
    X_cols = media_cols + ['price_log'] + [col for col in df.columns if col.startswith('week_')]
    X = df[X_cols].values
    
    # Target variable
    y = df['sales'].values
    
    # Fit model
    if ridge_alpha > 0:
        model = Ridge(alpha=ridge_alpha)
    else:
        model = LinearRegression()
    
    model.fit(X, y)
    
    # Create predictions
    df['predicted_sales'] = model.predict(X)
    
    # Decompose effects
    df['baseline'] = model.intercept_
    for i, col in enumerate(X_cols):
        if col in media_cols:
            channel = col.split('_')[0]
            df[f'{channel}_contribution'] = X[:, i] * model.coef_[i]
        elif col == 'price_log':
            df['price_contribution'] = X[:, i] * model.coef_[i]
        else:
            # Seasonality (week effects)
            if i == 0:
                df['seasonality_contribution'] = 0
            df['seasonality_contribution'] += X[:, i] * model.coef_[i]
    
    # Calculate metrics
    rmse = np.sqrt(mean_squared_error(y, df['predicted_sales']))
    r2 = r2_score(y, df['predicted_sales'])
    
    print(f"Model Performance: RMSE = {rmse:.2f}, R² = {r2:.4f}")
    
    return model, df

# Calculate ROI from model
def calculate_roi_from_model(model, data, media_cols):
    """
    Calculate ROI for each channel based on model coefficients
    
    Args:
        model: Fitted model
        data: DataFrame with data
        media_cols: List of media column names
        
    Returns:
        Dictionary of ROI values by channel
    """
    roi_values = {}
    
    # Get coefficients for media variables
    media_indices = [i for i, col in enumerate(media_cols) if any(ch in col for ch in ['TV', 'Search', 'Social', 'Display', 'Radio'])]
    
    for i, idx in enumerate(media_indices):
        channel = media_cols[idx].split('_')[0]
        coefficient = model.coef_[idx]
        
        # Calculate average spend
        spend_col = f"{channel}_spend"
        avg_spend = data[spend_col].mean()
        
        # Calculate ROI (coefficient represents revenue per unit of transformed media)
        if '_adstock' in media_cols[idx]:
            # For adstocked media, we need to account for the transformation
            adstock_col = f"{channel}_adstock"
            spend_col = f"{channel}_spend"
            
            # Calculate ratio between adstock and spend
            ratio = data[adstock_col].sum() / data[spend_col].sum()
            
            # ROI = coefficient * adstock_to_spend_ratio
            roi = coefficient * ratio
        else:
            # For raw spend, coefficient directly gives ROI
            roi = coefficient
        
        roi_values[channel] = roi
    
    return roi_values

# Run simulation and build model
sim = create_complex_simulation()
results = sim.simulate()

# Plot simulation results
plt.figure(figsize=(15, 10))
sim.plot_simulation_results()
plt.show()

# Calculate true ROI using simulation
print("True ROI values from simulation:")
for i, channel in enumerate(sim.channels):
    roi = sim.measure_ROI(i)
    print(f"{channel}: {roi:.2f}")

# Build Media Mix Models with different specifications
print("\nBuilding Media Mix Models:")

# Model 1: Simple regression with no adstock or saturation
print("\nModel 1: Simple regression (no adstock or saturation)")
model1, results1 = build_mmm(results, include_adstock=False, include_saturation=False)

# Model 2: Add adstock
print("\nModel 2: With adstock")
model2, results2 = build_mmm(results, include_adstock=True, include_saturation=False)

# Model 3: Add both adstock and saturation
print("\nModel 3: With adstock and saturation")
model3, results3 = build_mmm(results, include_adstock=True, include_saturation=True)

# Compare model fits
plt.figure(figsize=(15, 5))
plt.plot(results['time'], results['sales'], 'k-', label='Actual Sales')
plt.plot(results1['time'], results1['predicted_sales'], 'r--', label='Model 1: Simple')
plt.plot(results2['time'], results2['predicted_sales'], 'g--', label='Model 2: With Adstock')
plt.plot(results3['time'], results3['predicted_sales'], 'b--', label='Model 3: With Adstock & Saturation')
plt.legend()
plt.title('Model Fit Comparison')
plt.xlabel('Time Period')
plt.ylabel('Sales')
plt.grid(True)
plt.show()

# Get feature names for model 3
X_cols3 = [f'{channel}_adstock' for channel in sim.channels] + ['price_log'] + [f'week_{i}' for i in range(1, 52)]

# Calculate ROI from model 3
estimated_roi = calculate_roi_from_model(model3, results, X_cols3)

# Compare true vs. estimated ROI
true_roi = {channel: sim.measure_ROI(i) for i, channel in enumerate(sim.channels)}

roi_comparison = pd.DataFrame({
    'Channel': list(true_roi.keys()),
    'True ROI': list(true_roi.values()),
    'Estimated ROI': [estimated_roi.get(ch, 0) for ch in true_roi.keys()]
})

print("\nROI Comparison:")
print(roi_comparison)

# Plot ROI comparison
plt.figure(figsize=(10, 6))
bar_width = 0.35
x = np.arange(len(sim.channels))

plt.bar(x - bar_width/2, roi_comparison['True ROI'], bar_width, label='True ROI')
plt.bar(x + bar_width/2, roi_comparison['Estimated ROI'], bar_width, label='Estimated ROI')

plt.xticks(x, roi_comparison['Channel'])
plt.ylabel('ROI')
plt.title('True vs. Estimated ROI by Channel')
plt.legend()
plt.grid(True, axis='y')
plt.show()

# Plot contribution by channel
plt.figure(figsize=(15, 8))
channels = sim.channels
contrib_cols = [f'{channel}_contribution' for channel in channels]

# Base and seasonality
plt.plot(results3['time'], results3['baseline'] + results3['seasonality_contribution'], 'k-', 
         label='Base + Seasonality')

# Cumulative contribution
cumulative = results3['baseline'] + results3['seasonality_contribution'].copy()
for col in contrib_cols:
    channel = col.split('_')[0]
    cumulative += results3[col]
    plt.plot(results3['time'], cumulative, '-', label=f'+ {channel}')

plt.plot(results['time'], results['sales'], 'r--', label='Actual Sales', linewidth=2)
plt.legend(loc='upper left')
plt.title('Decomposition of Sales by Channel Contribution')
plt.xlabel('Time Period')
plt.ylabel('Sales')
plt.grid(True)
plt.show()
