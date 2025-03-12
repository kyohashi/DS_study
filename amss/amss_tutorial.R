library(amss)

n.years <- 4
time.n <- n.years * 52


# Natural Migration -------------------------------------------------------

activity.transition <- matrix(
  c(0.60, 0.30, 0.10, # migration originating from inactive state
    0.60, 0.30, 0.10, # exploratory state
    0.60, 0.30, 0.10), # purchase state
  nrow = length(kActivityStates), byrow = TRUE)
favorability.transition <- matrix(
  c(0.03, 0.07, 0.65, 0.20, 0.05, # migration from the unaware state
    0.03, 0.07, 0.65, 0.20, 0.05, # negative state
    0.03, 0.07, 0.65, 0.20, 0.05, # neutral state
    0.03, 0.07, 0.65, 0.20, 0.05, # somewhat favorable state
    0.03, 0.07, 0.65, 0.20, 0.05), # favorable state
  nrow = length(kFavorabilityStates), byrow = TRUE)

# a sinusoidal pattern
market.rate.nonoise <-
  SimulateSinusoidal(n.years * 52, 52,
                     vert.trans = 0.6, amplitude = 0.25)
# with some added noise
market.rate.seas <- pmax(
  0, pmin(1,
          market.rate.nonoise *
            SimulateAR1(length(market.rate.nonoise), 1, 0.1, 0.3)))

nat.mig.params <- list(
  population = 2.4e8,
  market.rate.trend = 0.68,
  market.rate.seas = market.rate.seas,
  # activity states for newly responsive (in-market & un-satiated)
  prop.activity = c(0.375, 0.425, 0.2),
  # brand favorability, initial proportions.
  prop.favorability = c(0.03, 0.07, 0.65, 0.20, 0.05),
  # everyone is a switcher
  prop.loyalty = c(1, 0, 0),
  transition.matrices = list(
    activity = activity.transition,
    favorability = favorability.transition))



# Marketing Interventions -------------------------------------------------

budget.index <- rep(1:n.years, each = 52)


## TV 

tv.flighting <-
  pmax(0,
       market.rate.seas +
         SimulateAR1(length(market.rate.seas), -0.7, 0.7, -0.7))
tv.flighting <- tv.flighting[c(6:length(tv.flighting), 1:5)]

tv.activity.trans.mat <- matrix(0, nrow=3, ncol=3)
tv.activity.trans.mat[,1] <- 1
  # matrix(
  # c(1.00, 0.00, 0.00, # migration originating from the inactive state
  #   0.00, 1.00, 0.00, # exploratory state
  #   0.00, 0.00, 1.00), # purchase state
  # nrow = length(kActivityStates), byrow = TRUE)
tv.favorability.trans.mat <- matrix(0, nrow=5, ncol=5)
tv.favorability.trans.mat[,1] <- 1
  # matrix(
  # c(0.4, 0.0, 0.4, 0.2, 0.0, # migration from the unaware state
  #   0.0, 0.9, 0.1, 0.0, 0.0, # negative state
  #   0.0, 0.0, 0.6, 0.4, 0.0, # neutral state
  #   0.0, 0.0, 0.0, 0.8, 0.2, # somewhat favorable state
  #   0.0, 0.0, 0.0, 0.0, 1.0), # favorable state
  # nrow = length(kFavorabilityStates), byrow = TRUE)

params.tv <- list(
  audience.membership = list(activity = rep(0.4, 3)),
  budget = rep(c(545e5, 475e5, 420e5, 455e5), length = n.years),
  budget.index = budget.index,
  flighting = tv.flighting,
  unit.cost = 0.005,
  hill.ec = 1.56,
  hill.slope = 1,
  transition.matrices = list(
    activity = tv.activity.trans.mat,
    favorability = tv.favorability.trans.mat))

## Paid Search
cpc.min <- 0.8
cpc.max <- 1.1

# uncapped spend, shut off the first 2 of every 13 weeks
spend.cap.fn <- function(time.index, budget, budget.index) {
  if ((time.index %% 13) > 1) {
    return(Inf)
  } else {
    return(0)
  }
}

bid.fn <- function(time.index, per.capita.budget, budget.index) {
  return(1.1)
}

kwl.fn <- function(time.index, per.capita.budget, budget.index) {
  return(4.5 * per.capita.budget)
}

search.activity.trans.mat <- matrix(
  c(0.05, 0.95, 0.00, # starting state: inactive
    0.00, 0.85, 0.15, # starting state: exploratory
    0.00, 0.00, 1.00), # starting: purchase
  nrow = length(kActivityStates), byrow = TRUE)
search.favorability.trans.mat <- matrix(
  c(1.0, 0.0, 0.0, 0.0, 0.0, # unaware
    0.0, 1.0, 0.0, 0.0, 0.0, # negative
    0.0, 0.0, 1.0, 0.0, 0.0, # neutral
    0.0, 0.0, 0.0, 1.0, 0.0, # favorable
    0.0, 0.0, 0.0, 0.0, 1.0), # loyal
  nrow = length(kFavorabilityStates), byrow = TRUE)

params.search <- list(
  audience.membership = list(activity = c(0.01, 0.3, 0.4)),
  budget = (2.4e7 / n.years) * (1:n.years),
  budget.index = budget.index,
  spend.cap.fn = spend.cap.fn,
  bid.fn = bid.fn,
  kwl.fn = kwl.fn,
  query.rate = 1,
  cpc.min = cpc.min,
  cpc.max = cpc.max,
  ctr = list(activity = c(0.005, 0.08, 0.10)),
  relative.effectiveness = c(0, 0.1, 1),
  transition.matrices = list(
    activity = search.activity.trans.mat,
    favorability = search.favorability.trans.mat))

# Sales -------------------------------------------------------------------

sales.params <- list(
  competitor.demand.max = list(loyalty = c(0.8, 0, 0.8)),
  advertiser.demand.slope = list(favorability = rep(0, 5)),
  advertiser.demand.intercept = list(
    favorability = c(0.014, 0, 0.2, 0.3, 0.9)),
  price = 80)


# Simulation --------------------------------------------------------------

sim.data <- SimulateAMSS(
  time.n = time.n,
  nat.mig.params = nat.mig.params,
  media.names = c("tv", "search"),
  media.modules = c(
    `DefaultTraditionalMediaModule`
    ,
    `DefaultSearchMediaModule`),
  media.params = list(params.tv, params.search),
  sales.params = sales.params)

burn.in.length <- 52
final.year.end <- n.years * 52
final.year.start <- final.year.end - 51
observed.data <- sim.data$data[(burn.in.length + 1):final.year.end, ]

names(observed.data)


# Viz ---------------------------------------------------------------------

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(grid)

# Main visualization function for MMM data
visualize_mmm_data <- function(data) {
  # Common theme with smaller legend
  small_legend_theme <- theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )
  
  # 1. Media spend comparison
  p1 <- ggplot(data) +
    geom_line(aes(x = time.index, y = tv.spend, color = "TV")) +
    geom_line(aes(x = time.index, y = search.spend, color = "Search")) +
    scale_color_manual(values = c("TV" = "blue", "Search" = "red")) +
    labs(title = "Media Spend Over Time",
         x = "Week",
         y = "Spend ($)",
         color = "Channel") +
    scale_y_continuous(labels = dollar_format()) +
    small_legend_theme
  
  # 2. Sales and revenue
  p2 <- ggplot(data) +
    geom_line(aes(x = time.index, y = brand.sales, color = "Brand Sales")) +
    geom_line(aes(x = time.index, y = competitor.sales, color = "Competitor Sales")) +
    geom_line(aes(x = time.index, y = revenue/100, color = "Revenue (÷100)")) +
    scale_color_manual(values = c(
      "Brand Sales" = "darkgreen", 
      "Competitor Sales" = "orange",
      "Revenue (÷100)" = "purple"
    )) +
    labs(title = "Sales and Revenue Over Time",
         x = "Week",
         y = "Units / Scaled Revenue",
         color = "Metric") +
    small_legend_theme
  
  # 3. TV metrics
  p3 <- ggplot(data) +
    geom_line(aes(x = time.index, y = tv.volume, color = "TV Volume")) +
    geom_line(aes(x = time.index, y = tv.spend*100, color = "TV Spend (×100)")) +
    scale_color_manual(values = c("TV Volume" = "darkblue", "TV Spend (×100)" = "lightblue")) +
    labs(title = "TV Metrics Over Time",
         x = "Week",
         y = "Volume / Scaled Spend",
         color = "Metric") +
    small_legend_theme
  
  # 4. Search metrics with clear scaling in the legend
  p4 <- ggplot(data) +
    geom_line(aes(x = time.index, y = search.clicks, color = "Clicks")) +
    geom_line(aes(x = time.index, y = search.imps/10, color = "Impressions (÷10)")) +
    geom_line(aes(x = time.index, y = search.matching.query.volume/10, 
                  color = "Matching Queries (÷10)")) +
    scale_color_manual(values = c(
      "Clicks" = "blue", 
      "Impressions (÷10)" = "red",
      "Matching Queries (÷10)" = "purple"
    )) +
    labs(title = "Search Metrics Over Time",
         x = "Week",
         y = "Count (Scaled as Indicated)",
         color = "Metric") +
    small_legend_theme
  
  # 5.ROI plot - simplified with smoother lines and clear visualization
  # Calculate smoothed ROI data
  roi_data <- data.frame(
    time.index = data$time.index,
    tv_roi = data$revenue / data$tv.spend,
    search_roi = data$revenue / data$search.spend
  )
  
  # Handle NAs that may be created by the moving average
  roi_data <- na.omit(roi_data)
  
  # Create separate ROI plots for clarity
  p5a <- ggplot(roi_data) +
    geom_line(aes(x = time.index, y = tv_roi), color = "blue") +
    labs(title = "TV ROI",
         x = "Week",
         y = "ROI") +
    theme_minimal() +
    theme(plot.title = element_text(size = 5, face = "bold"))
  
  p5b <- ggplot(roi_data) +
    geom_line(aes(x = time.index, y = search_roi), color = "red") +
    labs(title = "Search ROI",
         x = "Week",
         y = "ROI") +
    theme_minimal() +
    theme(plot.title = element_text(size = 5, face = "bold"))
  
  # Combine the ROI plots
  p5 <- grid.arrange(p5a, p5b, ncol = 1,
                     top = textGrob("ROI", 
                                    gp = gpar(fontsize = 5, fontface = "bold")))
  
  # 6. Response curves - Both channels
  p6 <- ggplot(data) +
    geom_point(aes(x = tv.spend, y = brand.sales, color = "TV"), alpha = 0.5) +
    geom_smooth(aes(x = tv.spend, y = brand.sales, color = "TV"), 
                method = "loess", formula = y ~ x, se = FALSE) +
    geom_point(aes(x = search.spend, y = brand.sales, color = "Search"), alpha = 0.5) +
    geom_smooth(aes(x = search.spend, y = brand.sales, color = "Search"), 
                method = "loess", formula = y ~ x, se = FALSE) +
    scale_color_manual(values = c("TV" = "blue", "Search" = "red")) +
    labs(title = "Media Spend vs. Brand Sales",
         subtitle = "Response curves showing spend-sales relationship",
         x = "Channel Spend ($)",
         y = "Brand Sales (Units)",
         color = "Channel") +
    scale_x_continuous(labels = dollar_format()) +
    small_legend_theme
  
  # Arrange plots in a grid with more space for the graphs
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2,
               top = textGrob("Media Mix Model Analysis", gp = gpar(fontsize = 14, fontface = "bold")))
  
  # Return plots invisibly
  invisible(list(
    media_spend = p1,
    sales_revenue = p2, 
    tv_metrics = p3,
    search_metrics = p4,
    roi = p5,
    response_curves = p6
  ))
}

# Execute the visualization
visualize_mmm_data(observed.data)
