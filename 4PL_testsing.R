library(drc)

# Compute the mean diff value for Sample "s0"
s0_mean <- TESTING |> 
  filter(Sample == "s0") |> 
  summarise(mean_diff = mean(diff, na.rm = TRUE)) |> 
  pull(mean_diff)

# Subtract the background from all values
data_corrected <- TESTING |> 
  filter(Gene == "Standard") |> 
  mutate(Concentration = as.double(Treatment),
         diff_corrected = diff - s0_mean)

# Fit the 4PL model
model_4PL <- drm(diff_corrected ~ Concentration, data = data_corrected, 
                 fct = LL.4(names = c("Slope", "Lower", "Upper", "EC50")))



model_4PL_2 <- drm(Concentration ~ diff_corrected , data = data_corrected, 
                 fct = LL.4(names = c("Slope", "Lower", "Upper", "EC50")))

# Generate a sequence of concentration values for smooth plotting
new_data <- data.frame(Concentration = seq(min(data_corrected$Concentration), 
                                           max(data_corrected$Concentration), length.out = 100))

# Predict values using the model
new_data$predicted_diff <- stats::predict(model_4PL, newdata = new_data)

# Plot the data and fitted curve
ggplot(data_corrected, aes(x = Concentration, y = diff_corrected)) +
  geom_point() + 
  geom_line(data = new_data, aes(x = Concentration, y = predicted_diff), color = "blue") +
  labs(y = "Corrected diff", title = "4PL Fit for Dose-Response") +
  theme_minimal()
#######################################################################################################################

estimate_concentration <- function(diff_value) {
  tryCatch(
    as.numeric(ED(model_4PL, diff_value, interval = "delta")),  # Get estimated concentration
    error = function(e) NA  # Return NA if outside the model range
  )
}

estimate_concentration_vec <- Vectorize(estimate_concentration)

TESTING_out <-  TESTING |> 
  mutate(diff_corrected = (diff - s0_mean)) |> 
  glimpse() |> 
  mutate(Concentration_Estimated = estimate_concentration_vec(diff_corrected)) |> 
  unnest_wider(Concentration_Estimated,names_sep = "_") |>
  rename(Estimate = Concentration_Estimated_1,
         std.error = Concentration_Estimated_2,
         CI_low = Concentration_Estimated_3,
         CI_High = Concentration_Estimated_4) |>
  glimpse()



#Test for lack of fit
modelFit(model_4PL)
plot(residuals(model_4PL) ~ fitted(model_4PL), main="Residuals vs Fitted")
abline(h=0)
qqnorm(residuals(model_4PL))
qqline(residuals(model_4PL))

ED(model_4PL, 2.6218, interval = "none")


ED(model_4PL,100, interval = "none")


predict(model_4PL, newdata = data.frame(diff_corrected = 800))


new_data <- data.frame(Concentration = 800)
predict(model_4PL, newdata = new_data)
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


TESTING_out |>
  filter(!Gene == "Standard") |> 
  filter(Gene=="3h") |> 
  ggplot(aes(x= Treatment, y= Estimate,colour=Genotype)) +
  geom_bar(aes(symbol=Genotype,fill = Genotype),stat = "summary", fun = "mean",
           colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
           position = position_dodge(width = 0.85)) +
  geom_point(aes(fill = Genotype),
             size=2,
             pch=21,
             stroke = 0.2,
             width = 0.65,
             color = "#111111",linewidth = 0.1,
             position =  (position_jitterdodge(dodge.width = 0.85))) 


TESTING_out |>
  filter(Gene == "Standard") |> 
  # filter(Gene=="3h") |> 
  ggplot(aes(x= Treatment, y= Estimate,colour=Genotype)) +
  geom_bar(aes(symbol=Genotype,fill = Genotype),stat = "summary", fun = "mean",
           colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
           position = position_dodge(width = 0.85)) +
  geom_point(aes(fill = Genotype),
             size=2,
             pch=21,
             stroke = 0.2,
             width = 0.65,
             color = "#111111",linewidth = 0.1,
             position =  (position_jitterdodge(dodge.width = 0.85))) 

