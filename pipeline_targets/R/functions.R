get_data = function(file){
  read_csv(file, col_types = cols()) |> 
    as_tibble()
}

fit_model = function(data){
  lm(mpg ~ wt, data) |> 
    coefficients()
}

plot_model = function(model, data){
  ggplot(data) +
    geom_point(aes(x = wt, y = mpg)) +
    geom_abline(intercept = model[1], slope = model[2]) +
    theme_gray(24)
}