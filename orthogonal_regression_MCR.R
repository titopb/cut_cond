library (tidyverse)
library (mcr)
library (pastecs)
library(psych)

# loading and treating data

data <- read.csv('data.csv', stringsAsFactors = T) # load data from file.csv
data <- janitor::clean_names(data)    # clean names using janitor

colnames(data)[1] <- "rep"    # change repetition to rep

data[,1:3] <- lapply(data[, 1:3], factor)  # change int to factor for rep, temp and ppfd

view(data)
glimpse(data)   #check data structure

# subset dataframe by Temp factor level

temp_10 <- data[data$temp == '10',]
temp_15 <- data[data$temp == '15',]
temp_20 <- data[data$temp == '20',]
temp_30 <- data[data$temp == '30',]

# assign x and y 

x_10 <- temp_10$medlyn 
y_10 <- temp_10$gs
x_15 <- temp_15$medlyn 
y_15 <- temp_15$gs
x_20 <- temp_20$medlyn 
y_20 <- temp_20$gs
x_30 <- temp_30$medlyn 
y_30 <- temp_30$gs

                                   ###     ### 
###################################### 10C #####################################
                                 ####      ####

# Descriptive Statistics

descriptive_10 <- stat.desc(temp_10[, 4:5])  # Find CV and error.ratio to add in Deming regression
View(descriptive_10)

psych::describeBy(data, data$temp)


# Calculate CV ratio to add as error.ratio in Deming regression

CV_ratio_10 <- as_tibble(descriptive_10[14,]) %>%  # Define as tbl class
 mutate(ratio = medlyn/gs)

Error_ratio_10 <- as.numeric(CV_ratio_10[,3])
view(Error_ratio_10)

# Performing Deming regression fit using MCR package

dem_reg_10 <- mcreg(x_10, y_10, error.ratio = Error_ratio_10, method.reg = "Deming",
                 method.ci = "analytical", 
                 mref.name = "(A/C.asqrt(D))", 
                 mtest.name = "gs", 
                 na.rm = T)

printSummary(dem_reg_10)
getCoefficients(dem_reg_10)


plot(dem_reg_10, main="10C")

plot(dem_reg_10,
     add.legend=TRUE,
     points.pch = 19, ci.area = F, add.cor = T, 
     digits = list(coef = 3, cor = 3),
     main="10",
     ci.area.col = grey(0.9), 
     identity=F, add.grid=F, sub="")
     

MCResult.plot(dem_reg_10, equal.axis = F, x.lab = "A/C.asqrt(D)", 
              digits = list(coef = 3, cor = 3),
              y.lab = "gs", points.col = "#FF7F5060", points.pch = 19, 
              ci.area = F, ci.area.col = "#0000FF50", 
              identity = F,
              main = "10", sub = "", 
              add.grid = FALSE, points.cex = 1.5)


                                    ###     ### 
###################################### 15C #####################################
                                 ####      ####

# Descriptive Statistics

descriptive_15 <- stat.desc(temp_15[, 4:5])  # Find CV and error.ratio to add in Deming regression
View(descriptive_15)


# Calculate CV ratio to add as error.ratio in Deming regression

CV_ratio_15 <- as_tibble(descriptive_15[14,]) %>%  # Define as tbl class
  mutate(ratio = medlyn/gs)

Error_ratio_15 <- as.numeric(CV_ratio_15[,3])
view(Error_ratio_15)

# Performing Deming regression fit using MCR package

dem_reg_15 <- mcreg(x_15, y_15, error.ratio = Error_ratio_15, method.reg = "Deming",
                    method.ci = "analytical", 
                    mref.name = "(A/C.asqrt(D))", 
                    mtest.name = "gs", 
                    na.rm = T)

printSummary(dem_reg_15)
getCoefficients(dem_reg_15)

plot(dem_reg_15, main="15")

plot(dem_reg_15,
     add.legend=TRUE,
     points.pch = 19, ci.area = F, add.cor = T, 
     digits = list(coef = 3, cor = 3),
     ci.area.col = grey(0.9), 
     main ="15",
     identity=F, add.grid=F, sub="")


MCResult.plot(dem_reg_15, equal.axis = F, x.lab = "A/C.asqrt(D)", 
              ylim = c(0, 0.3),
              digits = list(coef = 3, cor = 3),
              y.lab = "gs", points.col = "red", points.pch = 19, 
              ci.area = F, ci.area.col = "#0000FF50", 
              reg.col = "red",
              identity = F,
              add=F,
              sub = "", 
              main = "15",
              add.grid = FALSE, points.cex = 1.5)



                                    ###     ### 
###################################### 20C #####################################
                                 ####      ####

# Descriptive Statistics

descriptive_20 <- stat.desc(temp_20[, 4:5])  # Find CV and error.ratio to add in Deming regression
View(descriptive_20)


# Calculate CV ratio to add as error.ratio in Deming regression

CV_ratio_20 <- as_tibble(descriptive_20[14,]) %>%  # Define as tbl class
  mutate(ratio = medlyn/gs)

Error_ratio_20 <- as.numeric(CV_ratio_20[,3])
view(Error_ratio_20)

# Performing Deming regression fit using MCR package

dem_reg_20 <- mcreg(x_20, y_20, error.ratio = Error_ratio_20, method.reg = "Deming",
                    method.ci = "analytical", 
                    mref.name = "(A/C.asqrt(D))", 
                    mtest.name = "gs", 
                    na.rm = T)

printSummary(dem_reg_20)
getCoefficients(dem_reg_20)
str(dem_reg_20)

plot(dem_reg_20)

plot(dem_reg_20,
     add.legend=TRUE,
     points.pch = 19, ci.area = F, add.cor = T, 
     digits = list(coef = 3, cor = 3),
     ci.area.col = grey(0.9), 
     identity=F, add.grid=F, sub="")


MCResult.plot(dem_reg_10, equal.axis = F, x.lab = "A/C.asqrt(D)", 
              ylim = c(0, 0.3),
              digits = list(coef = 3, cor = 3),
              y.lab = "gs", points.col = "#FF7F5060", points.pch = 19, 
              ci.area = F, ci.area.col = "#0000FF50", 
              identity = F,
              add=F,
              main = "20oC", sub = "", 
              add.grid = FALSE, points.cex = 1.5)



includeLegend(place="topleft", models=list(dem_reg_10, dem_reg_20),
              colors=c("darkblue", "red"), design = "1",  
               digits=2)



############################Copiar 30C #########################################

# Descriptive Statistics

descriptive_30 <- stat.desc(temp_30[, 4:5])  # Find CV and error.ratio to add in Deming regression
View(descriptive_30)


# Calculate CV ratio to add as error.ratio in Deming regression

CV_ratio_30 <- as_tibble(descriptive_30[14,]) %>%  # Define as tbl class
  mutate(ratio = medlyn/gs)

Error_ratio_30 <- as.numeric(CV_ratio_30[,3])
view(Error_ratio_30)

# Performing Deming regression fit using MCR package

dem_reg_30 <- mcreg(x_30, y_30, error.ratio = Error_ratio_30, method.reg = "Deming",
                    method.ci = "analytical", 
                    mref.name = "(A/C.asqrt(D))", 
                    mtest.name = "gs", 
                    na.rm = T)

printSummary(dem_reg_30)
getCoefficients(dem_reg_30)
str(dem_reg_30)

plot(dem_reg_20)

plot(dem_reg_20,
     add.legend=TRUE,
     points.pch = 19, ci.area = F, add.cor = T, 
     digits = list(coef = 3, cor = 3),
     ci.area.col = grey(0.9), 
     identity=F, add.grid=F, sub="")


MCResult.plot(dem_reg_10, equal.axis = F, x.lab = "A/C.asqrt(D)", 
              ylim = c(0, 0.3),
              digits = list(coef = 3, cor = 3),
              y.lab = "gs", points.col = "#FF7F5060", points.pch = 19, 
              ci.area = F, ci.area.col = "#0000FF50", 
              identity = F,
              add=F,
              main = "20oC", sub = "", 
              add.grid = FALSE, points.cex = 1.5)



includeLegend(place="topleft", models=list(dem_reg_10, dem_reg_20),
              colors=c("darkblue", "red"), design = "1",  
              digits=2)

##########################R2 and significance #################################

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

r2_10 <- lm(y_10 ~ x_10)
summary(r2_10)


r2_15 <- lm(y_15 ~ x_15)
summary(r2_15)


r2_20 <- lm(y_20 ~ x_20)
summary(r2_20)


r2_30 <- lm(y_30 ~ x_30)
summary(r2_30)
