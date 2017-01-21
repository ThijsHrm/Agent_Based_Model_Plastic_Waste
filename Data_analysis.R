library (ggplot2)

# Load the dataframe
df <- read.csv ("Experiment_WPR_data.csv", skip = 6, sep = ",", head=TRUE)

# Clean the column names
colnames (df) <- gsub ("X\\.", "", colnames (df))
colnames (df) <- gsub ("^\\.|\\.$", "", colnames (df))
colnames (df) <- gsub ("\\.", "_", colnames (df))
colnames (df) <- gsub ("___", "_", colnames (df))

# Generate a mean knowledge/perception variable
df$HH_Mean_Knowledge_Perception  <- c((df$HH_Knowledge + df$HH_Perception) / 2)

# Create a second dataframe containing only the final resulsts 
# Both will be analyzed
df_endresults <- subset (df, step == 240)

# First make a summary of the results of the experiment
summary (df_endresults)

# Plot the full data
scatter_meanboth = ggplot (data = df, aes (x = step, y = MP_Recycling_Rates)) +
  geom_point (aes (colour = df$HH_Mean_Knowledge_Perception)) +
  xlab ("Months") +
  ylab ("Recycling rate") +
  ggtitle ("Increase in recycling rate over a 20-year period")
print (scatter_meanboth)

# Measure the correlation between knowledge&perception and recycling rates
print (cor.test (df_endresults$HH_Mean_Knowledge_Perception, df_endresults$MP_Recycling_Rates))

# Make a boxplot to see the relation between a higher budget and higher recycling rates
boxplot_bpp_rr <- ggplot (data = df_endresults, aes(x = S_budget_per_person, y = MP_Recycling_Rates, group = S_budget_per_person)) + 
  geom_boxplot()
print (boxplot_bpp_rr) 

# Do some variable renaming to make the following graph easier to read
df_endresults$S_pct_edu_budget_split [df_endresults$S_pct_edu_budget_split == 20] <- "20% perception 80% knowledge" 
df_endresults$S_pct_edu_budget_split [df_endresults$S_pct_edu_budget_split == 40] <- "40% perception 60% knowledge" 
df_endresults$S_pct_edu_budget_split [df_endresults$S_pct_edu_budget_split == 60] <- "60% perception 40% knowledge" 
df_endresults$S_pct_edu_budget_split [df_endresults$S_pct_edu_budget_split == 80] <- "80% perception 20% knowledge" 
df_endresults$S_pct_budget_education_spending  [df_endresults$S_pct_budget_education_spending == 20] <- "20% on education" 
df_endresults$S_pct_budget_education_spending  [df_endresults$S_pct_budget_education_spending == 40] <- "40% on education" 
df_endresults$S_pct_budget_education_spending  [df_endresults$S_pct_budget_education_spending == 60] <- "60% on education" 
df_endresults$S_pct_budget_education_spending  [df_endresults$S_pct_budget_education_spending == 80] <- "80% on education" 

# Construct a boxgrid in which the combined effects of different variables are plotted
boxgrid_tb_eb <- ggplot (data = df_endresults, aes(x = S_budget_per_person, y = MP_Recycling_Rates, group = S_budget_per_person)) + 
  geom_boxplot() + 
  facet_grid(S_pct_budget_education_spending ~ S_pct_edu_budget_split)
print (boxgrid_tb_eb)

# Display the effect of infrastructure on recycling rates
df_highresults <- subset (df_endresults, MP_Recycling_Rates > 0.6)
boxplot_highresults <- ggplot (data = df_highresults, aes(x = S_Infrastructure, y = MP_Recycling_Rates, group = S_Infrastructure)) + 
  geom_boxplot()
print (boxplot_highresults)
t.test (df_highresults$MP_Recycling_Rates ~ df_highresults$S_Infrastructure) 
