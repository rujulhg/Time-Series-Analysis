#' ---
#' title: Time Series Coursework 1
#' author: Rujul Godghate
#' date: 16/3/25

# loading required libraries and installing latest packages
library(prophet)
library(ggplot2)
library(dplyr)
library(astsa)
install.packages("prophet")
install.packages("remotes") # latest install version
remotes::install_github('facebook/prophet@*release', subdir='R')

## 1. Loading Data ---------------
gas.df = data.frame(
    ds=zoo::as.yearmon(time(gas)),
    y=gas) # Converts 'gas' time series into a dataframe, more appropriate for use in further analysis
## 2. Plotting linear regression -------------
plot(gas.df$ds, gas.df$y, type="l") # One way to plot linear regression
lm_fit = lm(y ~ ds, gas.df)
lines(fitted(lm_fit) ~ gas.df$ds[1:length(fitted(lm_fit))], type = "l", col = "red")



ggplot(gas.df, aes(x = ds, y = y)) +    # Plotting data with linear regression line as well
    geom_point(aes(color = "Actual Data"), size = 1) +
    geom_smooth(method = "lm", formula = y ~ x,
                aes(color = "Linear Regression Line"),
                se = FALSE, linewidth = 1) +
    scale_color_manual(name = "Legend",
                       values = c("Actual Data" = "steelblue",
                                  "Linear Regression Line" = "firebrick")) +
    labs(title = "Gasoline Prices Over Time",
         x = "Date",
         y = "Price (¢/gallon)") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"))
## 3. Prophet Forecasting ----------
model = prpredict.prophetmodel = prophet::prophet(gas.df) # Using prophet forecaster
future = prophet::make_future_dataframe(model, periods=2, freq="year",include_history = TRUE) # Make dataframe with future dates for forecasting.
forecast = predict(model, future) # Predict using the prophet model.

plot(model,forecast,
    main= "Gasoline Prices Forecast",
    xlabel="Year",ylabel="Gasoline Price",
    ) # Plotting forecast

# Interactive plots with labels -----------
library(magrittr)
library(dygraphs)
library(htmltools)
intplot <- prophet:::dyplot.prophet(model, forecast, uncertainty=TRUE)
intplot

## 4. Plotting decomposed time series ------

plot(decompose(gas)) # Decompose a time series into seasonal, trend and irregular components using moving averages. Deals with additive or multiplicative seasonal component.


## 5. Transformations for variance stabilising --------

library(lmtest) # Loading library for Breusche-Pagan test
bp_test <- bptest(lm(y ~ ds, data = gas.df)) # Breusche-Pagan test for variance testing
print(bp_test)

install.packages("forecast") # Installing 'forecast' package for transformations below
# Box-Cox transformation
lambda <- forecast::BoxCox.lambda(gas.df$y)
gasBC = forecast::BoxCox(gas.df$y, lambda)
plot(gasBC, main="Box-Cox Plot of Gasoline Prices")

gaslog=log(gas.df$y) # Log transform to reduce variance
plot(gaslog) # Plots

decomp <- decompose(gas) # Labelling variable to look at each component's variance
cat("Trend component variance:", var(decomp$trend, na.rm=TRUE), "\n")
cat("Seasonal component variance:", var(decomp$seasonal, na.rm=TRUE), "\n")
cat("Random component variance:", var(decomp$random, na.rm=TRUE), "\n")


## 6.Forecasting growth  with max and min-------
gas.df$cap <- 10 ### what is the capacity (max), how does it affect?
m <- prophet(gas.df, growth = 'logistic')
future <- make_future_dataframe(m, periods = 8)
future$cap <- 8.5
fcst <- predict(m, future)
plot(m, fcst)
dyplot.prophet(m,fcst)

fedir_edited$y <- 10 - fedir_edited$y
fedir_edited$cap <- 6
fedir_edited$floor <- 1.5 ## (min)
future$cap <- 6
future$floor <- 1.5
m <- prophet(fedir_edited, growth = 'logistic')
fcst <- predict(m, future)
plot(m, fcst)
dyplot.prophet(m,fcst)

## 7. Changepoint Detection ----------
m <- prophet(fedir_edited, changepoint.prior.scale = 0.0001) # changing the number makes it a more rigid trend
forecast <- predict(m, future)
plot(m, forecast)

## Changing Seasonality Mode ---------
m <- prophet(fedir_edited, seasonality.mode = 'multiplicative')
forecast <- predict(m, future)
plot(m, forecast)

m <- add_seasonality(m, 'quarterly', period = 91.25, fourier.order = 8, mode = 'additive')
m <- add_regressor(m, 'regressor', mode = 'additive')
plot(m,forecast)




