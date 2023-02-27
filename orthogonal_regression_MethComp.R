library (tidyverse)
library (MethComp)

# loading and treating data

data <- read.csv('raw_data.csv', stringsAsFactors = T) # load data from file.csv
data <- janitor::clean_names(data)    # clean names using janitor

colnames(data)[1] <- "rep"    # change repetition to rep

data[,1:3] <- lapply(data[, 1:3], factor)  # change int to factor for rep, temp and ppfd

view(data)
glimpse(data)   #check data structure

# Performing Deming regression with MethComp package

reg <- Deming(x = data$medlyn, y = data$gs)
reg


# plotting data

plot_1 <- ggplot(data, aes( x = medlyn, y = gs)) +
  geom_point(size = 3, shape = 18, color = 'black') +
  scale_size_area()+
  labs(x = expression("A/Ca" ~ sqrt(VPD)),
  y = ~ g[s] *  " ( mol " * m^-2 * " " * s^-1 * ")") +
  theme_classic()

plot_1

# regression_1

reg_1 <- prcomp( ~ data$medlyn + data$gs)
slope <- reg_1$rotation[2,1] / reg_1$rotation[1,1]
slope

intercept <- reg_1$center[2] - slope*reg_1$center[1]
intercept

# R2 

r2 <- lm(data$gs ~ data$medlyn)
summary(r2)



# Plot orthogonal regresison

plot_1 +
  geom_abline(intercept = reg[1], slope = reg[2])
 