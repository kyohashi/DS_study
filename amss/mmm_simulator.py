import numpy as np
import pandas as pd
from scipy import stats
import matplotlib.pyplot as plt
from typing import List, Dict, Tuple, Optional
import seaborn as sns

class MarketingSimulator:
    """
    Marketing Mix Model Simulator based on AMSS principles
    """
    
    def __init__(self, 
                 population_size: int = 1000000,
                 time_periods: int = 104,  # 2 years of weekly data
                 channels: List[str] = ["TV", "Search", "Social", "Display", "Radio"],
                 seed: int = 42):
        """
        Initialize the simulator with basic parameters
        
        Args:
            population_size: Total number of consumers in the simulation
            time_periods: Number of time periods to simulate (e.g., weeks)
            channels: List of marketing channels to include
            seed: Random seed for reproducibility
        """
        self.population_size = population_size
        self.time_periods = time_periods
        self.channels = channels
        self.n_channels = len(channels)
        self.rng = np.random.RandomState(seed)
        
        # Initialize containers for simulation data
        self.media_spend = np.zeros((time_periods, self.n_channels))
        self.media_adstock = np.zeros((time_periods, self.n_channels))
        self.sales = np.zeros(time_periods)
        self.base_sales = np.zeros(time_periods)
        
        # Population segment sizes (initialized to default equilibrium)
        self.segments = self._initialize_segments()
        
        # Conversion parameters
        self.conversion_rates = {}
        self.pricing = np.ones(time_periods) * 40  # Default price
        
        # Media effectiveness parameters
        self.adstock_rates = np.zeros(self.n_channels)
        self.saturation_points = np.zeros(self.n_channels)
        self.media_ROI = np.zeros(self.n_channels)
        
        # Seasonality 
        self.seasonality = np.ones(time_periods)
    
    def _initialize_segments(self) -> Dict:
        """Initialize the consumer segments based on the AMSS framework"""
        # Create a dictionary to store segment sizes
        segments = {
            # Market state (in-market, out-of-market)
            'market': {
                'in': int(0.4 * self.population_size),
                'out': int(0.6 * self.population_size)
            },
            
            # Satiation state (satiated, unsatiated)
            'satiation': {
                'satiated': int(0.3 * self.population_size),
                'unsatiated': int(0.7 * self.population_size)
            },
            
            # Activity state
            'activity': {
                'inactive': int(0.6 * self.population_size),
                'exploratory': int(0.3 * self.population_size),
                'purchase': int(0.1 * self.population_size)
            },
            
            # Brand favorability
            'favorability': {
                'unaware': int(0.3 * self.population_size), 
                'unfavorable': int(0.1 * self.population_size),
                'neutral': int(0.4 * self.population_size),
                'somewhat_favorable': int(0.15 * self.population_size),
                'favorable': int(0.05 * self.population_size)
            },
            
            # Brand loyalty
            'loyalty': {
                'switcher': int(0.7 * self.population_size),
                'loyal': int(0.1 * self.population_size),
                'competitor_loyal': int(0.2 * self.population_size)
            },
            
            # Brand availability
            'availability': {
                'low': int(0.3 * self.population_size),
                'average': int(0.5 * self.population_size),
                'high': int(0.2 * self.population_size)
            }
        }
        
        return segments
    
    def configure_media_effects(self, 
                               adstock_rates: List[float], 
                               saturation_points: List[float],
                               media_ROI: List[float]):
        """
        Configure media effectiveness parameters
        
        Args:
            adstock_rates: List of adstock decay rates for each channel
            saturation_points: List of saturation points for each channel
            media_ROI: List of ROI values for each channel
        """
        if len(adstock_rates) != self.n_channels:
            raise ValueError(f"Expected {self.n_channels} adstock rates, got {len(adstock_rates)}")
            
        if len(saturation_points) != self.n_channels:
            raise ValueError(f"Expected {self.n_channels} saturation points, got {len(saturation_points)}")
            
        if len(media_ROI) != self.n_channels:
            raise ValueError(f"Expected {self.n_channels} ROI values, got {len(media_ROI)}")
        
        self.adstock_rates = np.array(adstock_rates)
        self.saturation_points = np.array(saturation_points)
        self.media_ROI = np.array(media_ROI)
    
    def configure_seasonality(self, amplitude: float = 0.2, period: int = 52):
        """
        Configure seasonality pattern
        
        Args:
            amplitude: Strength of seasonality (0-1)
            period: Length of seasonal period (e.g., 52 for annual)
        """
        # Create sinusoidal seasonality
        t = np.arange(self.time_periods)
        self.seasonality = 1 + amplitude * np.sin(2 * np.pi * t / period)
        
        # Add random noise to seasonality
        noise = self.rng.normal(0, 0.05, self.time_periods)
        self.seasonality += noise
        
        # Ensure seasonality is positive
        self.seasonality = np.maximum(self.seasonality, 0.5)
    
    def configure_spend(self, 
                      budgets: List[float], 
                      flighting_patterns: Optional[List[np.ndarray]] = None):
        """
        Configure media spend patterns
        
        Args:
            budgets: Total budget for each channel
            flighting_patterns: Optional flighting patterns for each channel
        """
        if len(budgets) != self.n_channels:
            raise ValueError(f"Expected {self.n_channels} budgets, got {len(budgets)}")
        
        # Default flighting: spread budget evenly
        if flighting_patterns is None:
            flighting_patterns = [np.ones(self.time_periods) for _ in range(self.n_channels)]
        
        # Set up media spend based on budgets and flighting
        for i, (budget, flighting) in enumerate(zip(budgets, flighting_patterns)):
            # Handle None values by creating a uniform flighting
            if flighting is None:
                flighting = np.ones(self.time_periods)
                
            # Normalize flighting pattern
            flighting = flighting / np.sum(flighting)
            
            # Apply budget to flighting pattern
            self.media_spend[:, i] = budget * flighting
    
    def hill_transformation(self, x: np.ndarray, ec50: float, slope: float) -> np.ndarray:
        """
        Apply Hill transformation for diminishing returns
        
        Args:
            x: Input values
            ec50: Half max effective concentration
            slope: Hill slope parameter
        
        Returns:
            Transformed values with diminishing returns
        """
        return 1 / (1 + (x / ec50) ** (-slope))
    
    def calculate_adstock(self):
        """Calculate adstocked media values using geometric decay"""
        for t in range(self.time_periods):
            if t == 0:
                self.media_adstock[t] = self.media_spend[t]
            else:
                self.media_adstock[t] = self.media_spend[t] + self.adstock_rates * self.media_adstock[t-1]
    
    def apply_saturation(self, media_values: np.ndarray) -> np.ndarray:
        """
        Apply saturation effects to media values
        
        Args:
            media_values: Adstocked media values
        
        Returns:
            Media values after saturation transformation
        """
        saturated_values = np.zeros_like(media_values)
        
        for i in range(self.n_channels):
            # Apply Hill transformation for saturation
            saturated_values[:, i] = self.hill_transformation(
                media_values[:, i], 
                self.saturation_points[i],
                1.0  # Default slope
            )
        
        return saturated_values
    
    def calculate_media_contribution(self) -> np.ndarray:
        """
        Calculate the sales contribution from each media channel
        
        Returns:
            Array of media contributions to sales
        """
        # Apply adstock
        self.calculate_adstock()
        
        # Apply saturation to adstocked values
        saturated_values = self.apply_saturation(self.media_adstock)
        
        # Calculate media contribution using ROI
        media_contribution = np.zeros_like(saturated_values)
        for i in range(self.n_channels):
            media_contribution[:, i] = saturated_values[:, i] * self.media_ROI[i] * self.media_spend[:, i]
        
        return media_contribution
    
    def simulate(self):
        """Run the full simulation and generate sales data"""
        # Calculate base sales (with seasonality but no media effects)
        baseline_mean = self.population_size * 0.01  # 1% purchase rate
        self.base_sales = baseline_mean * self.seasonality
        
        # Add random noise to base sales
        noise = self.rng.normal(0, 0.05 * baseline_mean, self.time_periods)
        self.base_sales += noise
        
        # Calculate media contribution
        media_contribution = self.calculate_media_contribution()
        
        # Calculate total sales
        self.sales = self.base_sales + np.sum(media_contribution, axis=1)
        
        # Ensure sales are non-negative
        self.sales = np.maximum(self.sales, 0)
        
        # Return data as DataFrame
        return self.get_simulation_data()
    
    def get_simulation_data(self) -> pd.DataFrame:
        """
        Return simulation results as a DataFrame
        
        Returns:
            DataFrame with media spend, sales and other metrics
        """
        # Create base DataFrame with time index
        df = pd.DataFrame({'time': range(self.time_periods)})
        
        # Add media spend columns
        for i, channel in enumerate(self.channels):
            df[f'{channel}_spend'] = self.media_spend[:, i]
            df[f'{channel}_adstock'] = self.media_adstock[:, i]
        
        # Add sales and other metrics
        df['base_sales'] = self.base_sales
        df['sales'] = self.sales
        df['seasonality'] = self.seasonality
        df['price'] = self.pricing
        
        return df
    
    def measure_ROI(self, channel_index: int, change_pct: float = 0.05) -> float:
        """
        Measure the ROI of a specific channel using a virtual experiment
        
        Args:
            channel_index: Index of the channel to measure
            change_pct: Percentage change in budget for measurement
        
        Returns:
            Measured ROI value
        """
        # Store original spend
        original_spend = self.media_spend.copy()
        original_sales = self.sales.copy()
        
        # Run simulation with original spend
        self.simulate()
        baseline_sales = self.sales.sum()
        baseline_spend = self.media_spend[:, channel_index].sum()
        
        # Modify spend for the target channel
        modified_spend = original_spend.copy()
        modified_spend[:, channel_index] *= (1 + change_pct)
        self.media_spend = modified_spend
        
        # Run simulation with modified spend
        self.simulate()
        new_sales = self.sales.sum()
        new_spend = self.media_spend[:, channel_index].sum()
        
        # Calculate ROI
        sales_lift = new_sales - baseline_sales
        spend_increase = new_spend - baseline_spend
        
        # Restore original values
        self.media_spend = original_spend
        self.sales = original_sales
        
        # Return ROI
        return sales_lift / spend_increase if spend_increase > 0 else 0
    
    def plot_simulation_results(self):
        """Plot key results from the simulation"""
        data = self.get_simulation_data()
        
        # Create subplots
        fig, axs = plt.subplots(3, 1, figsize=(15, 15), sharex=True)
        
        # Plot 1: Sales vs Base Sales
        axs[0].plot(data['time'], data['sales'], 'b-', label='Total Sales')
        axs[0].plot(data['time'], data['base_sales'], 'g--', label='Base Sales')
        axs[0].set_title('Sales Performance')
        axs[0].legend()
        axs[0].grid(True)
        
        # Plot 2: Media Spend by Channel
        for channel in self.channels:
            axs[1].plot(data['time'], data[f'{channel}_spend'], label=f'{channel} Spend')
        axs[1].set_title('Media Spend by Channel')
        axs[1].legend()
        axs[1].grid(True)
        
        # Plot 3: Seasonality
        axs[2].plot(data['time'], data['seasonality'], 'r-')
        axs[2].set_title('Seasonality')
        axs[2].set_xlabel('Time Period')
        axs[2].grid(True)
        
        plt.tight_layout()
        return fig

# Usage example
if __name__ == "__main__":
    # Create simulator
    sim = MarketingSimulator(
        population_size=1000000,
        time_periods=104,  # 2 years of weekly data
        channels=["TV", "Search", "Social", "Display", "Radio"]
    )
    
    # Configure media effects
    sim.configure_media_effects(
        adstock_rates=[0.7, 0.3, 0.5, 0.4, 0.6],  # Higher values mean longer-lasting effects
        saturation_points=[500000, 200000, 150000, 100000, 80000],  # Point of diminishing returns
        media_ROI=[2.0, 1.8, 1.5, 1.2, 1.0]  # Base ROI for each channel
    )
    
    # Configure seasonality
    sim.configure_seasonality(amplitude=0.2, period=52)  # Annual seasonality
    
    # Configure budgets
    total_budget = 1000000
    channel_budgets = [0.4, 0.3, 0.15, 0.1, 0.05]  # Proportion of budget by channel
    budgets = [b * total_budget for b in channel_budgets]
    
    # Create flighting patterns
    tv_flighting = np.ones(104)
    tv_flighting[::4] = 2.0  # Pulsing strategy with spikes every 4 weeks
    
    # Configure spend
    sim.configure_spend(
        budgets=budgets,
        flighting_patterns=[tv_flighting, None, None, None, None]
    )
    
    # Run simulation
    results = sim.simulate()
    
    # Measure ROI
    tv_roi = sim.measure_ROI(channel_index=0)
    print(f"Measured TV ROI: {tv_roi:.2f}")
    
    # Plot results
    sim.plot_simulation_results()
