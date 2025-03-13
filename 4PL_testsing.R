# rm(list = ls()[-7])
df <- keep


# Compute the mean diff value for Sample "s0"
s0_mean <- df |> 
  filter(Sample == "s0") |> 
  summarise(mean_diff = mean(diff, na.rm = TRUE)) |> 
  pull(mean_diff)

# Subtract the background from all values
data_corrected <- df |> 
  # filter(Gene == "Standard") |> 
  mutate(Concentration = as.double(Treatment),
         diff_corrected = diff - s0_mean)

# Fit the 4PL model
model_4PL <- drm(diff_corrected ~ Concentration, data = data_corrected, 
                 fct = LL.4(names = c("Slope", "Lower", "Upper", "EC50")))


estimate_concentration <- function(diff_value) {
  tryCatch(
    ED(model_4PL, diff_value, interval = "delta",type = "absolute",display = F),  # Get estimated concentration
    error = function(e) NA  # Return NA if outside the model range
  )
}

# estimate_concentration_vec <- Vectorize(estimate_concentration)
df_out <- df
df_out$diff_corrected <- df_out$diff - s0_mean
data_corrected <- bind_cols(data_corrected,estimate_concentration(data_corrected$diff_corrected))






### Fit graph

# Generate a sequence of concentration values for smooth plotting
Fit_Curve <- data.frame(Concentration = seq(min(data_corrected$Concentration,na.rm = T), 
                                           max(data_corrected$Concentration,na.rm = T), length.out = 100))

# Predict values using the model
Fit_Curve$predicted_diff <- stats::predict(model_4PL, newdata = Fit_Curve)

# Plot the data and fitted curve
data_corrected |>
  filter(Gene == "Standard") |> 
  ggplot(aes(x = Concentration, y = diff_corrected)) +
  geom_point() + 
  geom_line(data = Fit_Curve, aes(x = Concentration, y = predicted_diff), color = "blue") +
  labs(y = "Corrected diff", title = "4PL Fit for Dose-Response") +
  theme_minimal()
#######################################################################################################################


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################


df_out |>
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


df_out |>
  filter(Gene == "Standard") |> 
  # filter(Gene=="3h") |> 
  ggplot(aes(x= as.numeric(Treatment), y= Estimate,colour=Genotype)) +
  geom_bar(aes(symbol=Genotype,fill = Genotype),stat = "summary", fun = "mean",
           colour="#111111",width = 0.65,linewidth=0.1,alpha=0.5,
           position = position_dodge(width = 0.85)) +
  geom_point(aes(fill = Genotype),
             size=2,
             pch=21,
             stroke = 0.2,
             width = 0.65,
             color = "#111111",linewidth = 0.1,
             position =  (position_jitterdodge(dodge.width = 0.85))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) 

