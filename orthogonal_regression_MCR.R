library (tidyverse)
library (mcr)

# loading and treating data

data <- read.csv('raw_data.csv', stringsAsFactors = T) # load data from file.csv
data <- janitor::clean_names(data)    # clean names using janitor

colnames(data)[1] <- "rep"    # change repetition to rep

data[,1:3] <- lapply(data[, 1:3], factor)  # change int to factor for rep, temp and ppfd

view(data)
glimpse(data)   #check data structure

# assign x and y 

x <- data$medlyn
y <- data$gs

# Performing Deming regression fit using MCR package

ort_reg <- mcreg(x, y, error.ratio = 1, method.reg = "Deming",
                 method.ci = "analytical", mref.name = "medlyn", 
                 mtest.name = "gs", na.rm = T)

printSummary(ort_reg)
getCoefficients(ort_reg)
plot(ort_reg)

plot(ort_reg, add.legend=FALSE,
     points.pch = 19, ci.area = TRUE, add.cor = T, digits = list(coef = 2, cor = 3),
     ci.area.col = grey(0.9), identity=F, add.grid=F, sub="")


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

r2 <- lm(y ~ x)
summary(r2)




 