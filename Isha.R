###############################################################
# title: "Isha-Code.R"
# output: R Script
# Student Name: Isha Shukla
# Student ID number:225170943
# Subject: SIG 718 - Real World Analytics
# Assessment: Mid Term Assessment
###############################################################

################################# 
# You can use this template to draft the script for your Assessment 2 of SIT718.
#################################

#############################################################################################
# save your code as "name-code.R" (where ''name'' is replaced with your surname or first name).
#############################################################################################

##################################
#Question 1 - Understand the Data
##################################

data.raw <- as.matrix(read.table('/Users/ishashukla/Desktop/Deakin Uni/SIG718 - Real World Analytics/RedWine-1.txt'))

ID = 225170943

set.seed(ID) # using your student ID number for reproducible sampling with the seed function

data.subset <- data.raw[sample(1:1599, 400), c(1:6)]

data.variable.names <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol")

data.subset2 <- data.subset #copying dataset
View(data.subset2)


# Create 5 scatterplots function (for each X variable against the variable of interest Y) 


# Assuming your data is in a dataframe named 'data.subset' with columns:
# X1 (Citric Acid), X2 (Chlorides), X3 (Total Sulfur Dioxide), X4 (pH), X5 (Alcohol), Y (Quality)
data.subset <- as.data.frame(data.subset)

#install.packages("plotly")
library(plotly)
attach(data.subset)

colnames(data.subset)
colnames(data.subset) <- c("X1", "X2", "X3", "X4", "X5", "Y")


fig1 <- plot_ly(data = data.subset, type = "scatter", x = ~Y, y = ~X1, mode = "markers") %>%
  layout(
    title = 'Scatter Plot b/w Citric Acid & Quality',
    plot_bgcolor = "#e5ecf6",
    xaxis = list(title = 'Citric Acid'),
    yaxis = list(title = 'Quality')
  )
fig1


fig2 <- plot_ly(data = data.subset, type = "scatter", x = ~Y, y = ~X2, mode = "markers") %>%
  layout(
    title = 'Scatter Plot b/w Chlorides & Quality',
    plot_bgcolor = "#e5ecf6",
    xaxis = list(title = 'Chlorides'),
    yaxis = list(title = 'Quality')
  )
fig2

fig3 <- plot_ly(data = data.subset, type = "scatter", x = ~Y, y = ~X3, mode = "markers") %>%
  layout(
    title = 'Scatter Plot b/w Total Sulfur Dioxide & Quality',
    plot_bgcolor = "#e5ecf6",
    xaxis = list(title = 'Total Sulfur Dioxide'),
    yaxis = list(title = 'Quality')
  )
fig3

fig4 <- plot_ly(data = data.subset, type = "scatter", x = ~Y, y = ~X4, mode = "markers") %>%
  layout(
    title = 'Scatter Plot b/w pH & Quality',
    plot_bgcolor = "#e5ecf6",
    xaxis = list(title = 'pH'),
    yaxis = list(title = 'Quality')
  )
fig4

fig5 <-plot_ly(data=data.subset, type = "scatter", x=~Y, y=~X5, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Alcohol & Quality', 
         plot_bgcolor = "#e5ecf6", 
         xaxis =   list(title = 'Alcohol'), 
         yaxis = list(title = 'Quality'))
fig5
#####################################################################################
#------ SCATTER PLOTS USING GGPLOT2 ------#

#install.packages("ggplot2")
library(ggplot2)
attach(data.subset)

ggplot(data.subset, aes(x = X1, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)

ggplot(data.subset, aes(x = X2, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)


ggplot(data.subset, aes(x = X3, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)

ggplot(data.subset, aes(x = X4, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)


ggplot(data.subset, aes(x = X5, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)
#############################################################
#------ SCATTER PLOTS USING BASE PLOT FUNCTION ------#
# Copy the updated scatter plot code
colnames(data.subset) <- c("Citric_Acid", "Chlorides", "Total_Sulfur_Dioxide", "pH", "Alcohol", "Quality")

x_labels <- c(
  "Citric Acid",
  "Chlorides",
  "Total Sulfur Dioxide",
  "pH",
  "Alcohol"
)
colors <- c("magenta", "black", "brown", "purple", "orange")

par(mfrow = c(2, 3))
for (i in 1:5) {
  plot(
    data.subset[, i], data.subset[, "Quality"],
    xlab = x_labels[i],
    ylab = "Quality",
    main = paste(x_labels[i], "Vs Quality"),
    col = colors[i],
    pch = 16
  )
}
par(mfrow = c(1, 1))

# Create 6 histograms for each X variable and Y

library(plotly)
colnames(data.subset) <- c("X1", "X2", "X3", "X4", "X5", "Y")
# Create individual histograms
hist1 <- plot_ly(data = data.subset, 
                 x = ~X1, type = "histogram", 
                 name = "X1 (Citric Acid)") %>%
  layout(title = "Histogram of X1 (Citric Acid)", xaxis = list(title = "X1"), yaxis = list(title = "Count"))

hist2 <- plot_ly(data = data.subset, x = ~X2, type = "histogram", name = "X2 (Chlorides)") %>%
  layout(title = "Histogram of X2 (Chlorides)", xaxis = list(title = "X2"), yaxis = list(title = "Count"))

hist3 <- plot_ly(data = data.subset, x = ~X3, type = "histogram", name = "X3 (Total Sulfur Dioxide)") %>%
  layout(title = "Histogram of X3 (Total Sulfur Dioxide)", xaxis = list(title = "X3"), yaxis = list(title = "Count"))

hist4 <- plot_ly(data = data.subset, x = ~X4, type = "histogram", name = "X4 (pH)") %>%
  layout(title = "Histogram of X4 (pH)", xaxis = list(title = "X4"), yaxis = list(title = "Count"))

hist5 <- plot_ly(data = data.subset, x = ~X5, type = "histogram", name = "X5 (Alcohol)") %>%
  layout(title = "Histogram of X5 (Alcohol)", xaxis = list(title = "X5"), yaxis = list(title = "Count"))

hist6 <- plot_ly(data = data.subset, x = ~Y, type = "histogram", name = "Y (Quality)") %>%
  layout(title = "Histogram of Y (Quality)", xaxis = list(title = "Y"), yaxis = list(title = "Count"))

# Arrange histograms in a grid
fig <- subplot(hist1, hist2, hist3, hist4, hist5, hist6, nrows = 2, shareX = FALSE, shareY = FALSE) %>%
  layout(title = "Histograms of Variables")
fig

###########################################################################################
#------ HISTOGRAMS IN ONE VIEW FOR REPORTING PURPOSE ------#

# Install and load rcompanion
#install.packages("rcompanion")
library(rcompanion)

# Rename columns for clarity
colnames(data.subset) <- c("Citric_Acid", "Chlorides", "Total_Sulfur_Dioxide", "pH", "Alcohol", "Quality")


# Set up a grid layout for the plots
par(mfrow = c(2, 3))

# Plot histograms with updated labels and descriptions
plotNormalHistogram(data.subset$Citric_Acid, 
                    main = "Distribution of Citric Acid",
                    xlab = "Citric Acid (g/L)", 
                    breaks = 5, las = 1, 
                    col = "orange")

plotNormalHistogram(data.subset$Chlorides, 
                    main = "Distribution of Chlorides",
                    xlab = "Chlorides (g/L)", 
                    breaks = 5, las = 1, 
                    col = "red")

plotNormalHistogram(data.subset$Total_Sulfur_Dioxide, 
                    main = "Distribution of Total Sulfur Dioxide",
                    xlab = "Total Sulfur Dioxide (mg/L)", 
                    breaks = 5, las = 1, 
                    col = "magenta")

plotNormalHistogram(data.subset$pH, 
                    main = "Distribution of pH",
                    xlab = "pH", 
                    breaks = 5, las = 1, 
                    col = "brown")

plotNormalHistogram(data.subset$Alcohol, 
                    main = "Distribution of Alcohol",
                    xlab = "Alcohol (% vol)", 
                    breaks = 5, las = 1, 
                    col = "yellow")

plotNormalHistogram(data.subset$Quality, 
                    main = "Distribution of Quality",
                    xlab = "Quality (Score 0-10)", 
                    breaks = 5, las = 1, 
                    col = "green")

# Reset plotting layout
par(mfrow = c(1, 1))
colnames(data.subset)
colnames(data.subset2)


################################
#Question 2 - Transform the Data
################################
# Performing the Normality Test to analyze the data distribution column wise

for( x in 1:6) {
  pvalues<-ks.test(jitter(data.subset[,x]), "pnorm", mean=mean(data.subset[,x]), sd=sd(data.subset[,x]))
  print(pvalues)
}

# X1 pvalue (Citric Acid)= 0.006978
# X2 pvalue (Chlorides) < 2.2e-16
# X3 pvalue (Total Sulfur Dioxide) = 4.188e-07
# X4 pvalue (pH) =  0.5512
# X5 pvalue (Alcohol) = 1.789e-05
# Y pvalue (Quality) = 3.04e-11

# Significant p-values: p < 0.05 (e.g., Variables X1, X2, X3, X5 and  Y) suggest the data does not follow a normal distribution.
# X1, X2, X3, X5 and  Y looks a little skewed in the histogram and also the KS test says that they are not  normally distributed. Hence let us test the skewness to see what is the transformation we need to apply.
# Non-significant p-values:p > 0.05 (e.g., Variable X4) suggest the data might follow a normal distribution.
I <- c(1, 3, 4, 5) # Choose any four X variables and Y

install.packages("moments")
library(moments)

v_skew1<-skewness(data.subset[,1])
v_skew2<-skewness(data.subset[,2])
v_skew4<-skewness(data.subset[,4])


v_skew3<-skewness(data.subset[,3])
v_skew5<-skewness(data.subset[,5])
v_skew6<-skewness(data.subset[,6])

#For Skewness let's look at how we can analyse

#Skewness = 0, then it is perfect normal distribution
#Skewness is between -0.5 and 0.5, then we approximate it to a normal distribution
#Skewness > 0.5 then positively skewed
#Skewness < 0.5 then negatively skewed

print(v_skew1) # 0.3421747 # Approximate to a normal distribution as it lies between -0.5 and 0.5.
print(v_skew2) # 4.672925 # Positively skewed (Skewness > 0.5). Suggest applying a log transformation
print(v_skew4) # -0.04367436 # Approximate to a normal distribution as it lies between -0.5 and 0.5
print(v_skew3) # 1.794115 # Positively skewed (Skewness > 0.5). Suggest applying a log transformation
print(v_skew5) # 0.6855111 # Positively skewed (Skewness > 0.5). Suggest applying a log transformation.
print(v_skew6) # 0.1092107 # Approximate to a normal distribution as it lies between -0.5 and 0.5.

#Standard Deviation 
colnames(data.subset)
colnames(data.subset) <- c("X1", "X2", "X3", "X4", "X5", "Y")

sd_X1 <- sd(data.subset$X1)
print(paste("The standard deviation of X1 (Citric Acid) is:", sd_X1))

sd_X2 <- sd(data.subset$X2)
print(paste("The standard deviation of X2 (chlorides) is:", sd_X2))

sd_X3 <- sd(data.subset$X3)
print(paste("The standard deviation of X3 (total sulphur dioxide) is:", sd_X3))

sd_X4 <- sd(data.subset$X4)
print(paste("The standard deviation of X4 (pH) is:", sd_X4))

sd_X5 <- sd(data.subset$X5)
print(paste("The standard deviation of X5 (alcohol) is:", sd_X5))


# Boxplot

par(mfrow=c(2,5))
for (i in 1:length(data.subset)) {
  boxplot(data.subset[,i], main=names(data.subset[i]), type="l")
}

# Checking for what transformation needs to be done | Finding the Pearson Correlation

#Pearson

pearson_output <- array(0,5)

for(y in 1:5){
  pearson_output[y] <- cor(data.subset[,6], data.subset[,y], method = "pearson")
}
pearson_output # 0.22160931 -0.16813353 -0.21011461 -0.06456809  0.48533453

pearson_max_result <- which(pearson_output == max(pearson_output))

pearson_max_result # 5

cat("X5 (Alcohol) has the highest positive correlation with Y(Quality), 
    with a coefficient of 0.4853. This suggests that as alcohol content increases, 
    the quality score tends to increase.")

# Checking for what transformation needs to be done | Finding the Spearman Correlation

#Spearman
spearman_output <- array(0,5)

for(z in 1:5){
  spearman_output[z] <- cor(data.subset[,6], data.subset[,z], method ="spearman")
}

spearman_output # 0.21581805 -0.17303338 -0.26218770 -0.06491503  0.48442759

spearman_max_result <- which(spearman_output == max(spearman_output))

spearman_max_result # 5

cat("The variable X",spearman_max_result, "X5 (Alcohol) has the highest positive correlation with Y(Quality), 
    with a coefficient of 0.48442759. This suggests that as alcohol content increases, 
    the quality score tends to increase.")
# We can see that all the correlations are positive or negative in both the spearman and pearson methods. 

# CONCLUSION
#The Spearman analysis reinforces the observation that alcohol content (X5) has the strongest 
#monotonic relationship with quality (Y). 
# This variable remains the most influential predictor of quality, 
#whether analyzed through linear (Pearson) or rank-based (Spearman) correlation methods.

# v_skew1 = 0.3421747 --- Approximate to a normal distribution as it lies between -0.5 and 0.5.
# v_skew2 = 4.672925 --- Positively skewed (Skewness > 0.5). Suggest applying a log transformation
# v_skew4 = -0.04367436 --- Approximate to a normal distribution as it lies between -0.5 and 0.5
# v_skew3 = 1.794115 --- Positively skewed (Skewness > 0.5). Suggest applying a log transformation
# v_skew5 = 0.6855111 --- Positively skewed (Skewness > 0.5). Suggest applying a log transformation.
# v_skew6 = 0.1092107 --- Approximate to a normal distribution as it lies between -0.5 and 0.5.

#Choosing  FOUR variables from the FIVE variables X1, X2, X3, X4, X5.
#The weakest relationship is between pH (X4) and quality (Y), with minimal correlation in both Pearson and Spearman analyses..
#Therefore, the four variables chosen are X1, X2, X3, X5.
#For the variables X1, Y and X4, we have seen above that it can be approximated to normal distribution as skewness is between -0.5 and 0.5.
#Therefore, we will apply min-max transformation to X1, Y and X4.
#For the variable X2, X3 and X5, we will first apply log transformation and then apply the min-max transformation as it is positively skewed.
#The variable with the weakest relationship to quality (Y) is pH (X4), as it shows the smallest correlation in both Pearson (−0.0646) 
#and Spearman (−0.0649) analyses.
#Applying log transformation on X2, X3 and X5 variables

log_trans_func <- function(x){
  v_result = log10(x)
  return(v_result)
}

#---- X2 ----#

data.subset[,2] <- log_trans_func(data.subset[,2])
log_transform_X2 <- data.subset[,2]
print(log_transform_X2)

#---- X3 ----#

data.subset[,3] <- log_trans_func(data.subset[,3])
log_transform_X3 <- data.subset[,3]
print(log_transform_X3)

#---- X5 ----#

data.subset[,5] <- log_trans_func(data.subset[,5])
log_transform_X5 <- data.subset[,5]
print(log_transform_X5)

#Applying min-max scaling on the log transformed variables X2, X3 and X5 and X1, Y and X4.

minmax_transform <- function(x){
  v_result_transform = (x-min(x))/(max(x)-min(x))
  return(v_result_transform)
}

data.subset[,1] <- minmax_transform(data.subset[,1])
data.subset[,2] <- minmax_transform(data.subset[,2])
data.subset[,3] <- minmax_transform(data.subset[,3])
data.subset[,5] <- minmax_transform(data.subset[,5])
data.subset[,6] <- minmax_transform(data.subset[,6])

#Plotting the Histogram after Transformation
par(mfrow=c(2,3)) 

plotNormalHistogram(data.subset[,1], main = "Histogram of Citric Acid after Min-Max transformation",
                    xlab= "Citric Acid",las=1,col="violet")

plotNormalHistogram(data.subset[,2], main = "Histogram of chlorides after Log and Min-Max transformation",
                    xlab= "chlorides", las=1,col="violet")

plotNormalHistogram(data.subset[,3], main = "Histogram of total sulphur dioxide after Log and Min-Max transformation",
                    xlab= "total sulphur dioxide", las=1, col="violet")

#plotNormalHistogram(data.subset[,4], main = "Histogram of pH after Min-Max transformation",
#                    xlab= "pH", las=1,col="violet")

plotNormalHistogram(data.subset[,5], main = "Histogram of alcohol after Log and Min-Max Transformation",
                    xlab= "alcohol", las=1, col="violet")

plotNormalHistogram(data.subset[,6], main = "Histogram of quality after Min-Max Transformation",
                    xlab= "quality", las=1, col="violet")

# We can now see in the plot that all the selected variables are almost normally distributed
View(data.subset)

# We can see that X1, X2, X3, X5 and Y are transformed. X4 still remains in the matrix. Let us go ahead and remove that
data.subset <- data.subset[,-4]
View(data.subset)

# Save this transformed data to a text file
write.table(data.subset, "/Users/ishashukla/Desktop/Deakin Uni/SIG718 - Real World Analytics/Isha-transformed.txt")  # replace ??name?? with either your surname or first name.


##########################################
#Question 3 - Build models and investigate
##########################################

source('/Users/ishashukla/Desktop/Deakin Uni/SIG718 - Real World Analytics/AggWaFit718 (1).R')

#install.packages("lpSolve")
library(lpSolve)

data.transformed <- as.matrix(read.table("/Users/ishashukla/Desktop/Deakin Uni/SIG718 - Real World Analytics/Isha-transformed.txt"))  # import your saved data
View(data.transformed)

###############################################################

#------ (ii) Use the fitting functions to learn the parameters for ------#
# a. A weighted arithmetic mean (WAM),
# b. Weighted power means (WPM) with p = 0.5,
# c. Weighted power means (WPM) with p = 2,
# d. An ordered weighted averaging function (OWA).
# e. The Choquet integral

###############################################################
library(lpSolve)
# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed,"WAMoutput.txt","WAMstats.txt")

# Get weights for Power Mean p=0.5 with fit.QAM()
fit.QAM(data.transformed, "WPM_P0.5_output.txt", "WPM_P0.5_stats.txt", g=PM05, g.inv =invPM05)


# Get weights for Power Mean p=2 with fit.QAM()
PM2 <- function(x) {x^2}
invPM2 <-function(x) {x^(1/2)}

fit.QAM(data.transformed, "WPM_P2_output.txt", "WPM_P2_stats.txt", g=PM2, g.inv =invPM2)

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed, "OWAoutput.txt", "OWAstats.txt")

#######################################
#CONCLUSION
# WPM (P = 2) achieves the best performance:
# * It has the lowest RMSE (0.1641) and Average Absolute Error (0.1241), indicating better accuracy.
# * It shows the highest Pearson (0.4134) and Spearman (0.4220) correlations, demonstrating the strongest linear and monotonic relationships.
# WPM (P = 0.5) is the second best, with slightly higher RMSE and errors than WPM (P = 2).
# OWA has the weakest performance with the highest RMSE (0.1723) and the lowest correlation values.

#######################################
#Question 4 - Use Model for Prediction
#######################################

#Comparing all the models that were fitted in Question 3, the weights for Power Mean p=2 is the best model because
# It has the lowest RMSE 0.164144483752335
# It has the lowest Av. abs error 0.124137803803221
# Highest Pearson correlation 0.413363266844252
# Highest Spearman correlation 0.42196031057787

#We will now fit the new data into the Choquet Model.
# best fitting model based on Q3, predict the wine quality for the input: X1=1; X2= 0.75; X3=40; X4=3.53; X5=8.3.
# Example transformed data (replace with actual transformed data)

# New input values
new_input <- c(1, 0.75, 40, 3.53, 8.3)

transform_new_data <- new_input

# Apply Min-Max Scaling for X1 (new_input[1])
transform_new_data[1] <- (new_input[1] - min(data.subset2[, 1])) / (max(data.subset2[, 1]) - min(data.subset2[, 1]))

# Apply Log Transformation for X2 (new_input[2])
transform_new_data[2] <- log_trans_func(new_input[2])
# Apply Min-Max Scaling for log-transformed X2
transform_new_data[2] <- (transform_new_data[2] - min(log_transform_X2)) / (max(log_transform_X2) - min(log_transform_X2))

# Apply Log Transformation for X3 (new_input[3])
transform_new_data[3] <- log_trans_func(new_input[3])
transform_new_data[3] <- (transform_new_data[3] - min(log_transform_X3)) / (max(log_transform_X3) - min(log_transform_X3))

# Apply Log Transformation for X5 (new_input[5])
transform_new_data[5] <- log_trans_func(new_input[5])

# Apply Min-Max Scaling for log-transformed X5
transform_new_data[5] <- (transform_new_data[5] - min(log_transform_X5)) / (max(log_transform_X5) - min(log_transform_X5))

# Apply Min-Max Scaling for X4 (new_input[4])
transform_new_data[4] <- (new_input[4] - min(data.subset2[, 4])) / (max(data.subset2[, 4]) - min(data.subset2[, 4]))

# View the transformed data
transform_new_data #1.00000000  1.05259297  0.44868820  0.80612245 -0.02649687

# Weights for Power Mean model (p = 2)
weights <- c(0.0319740140754234, 0.397414950592014, 0.0146204917990174, 0.555990543533545)

# Apply Power Mean formula with p = 2
p <- 2  # Power Mean parameter
power_mean_out <- (sum(weights * transform_new_data^p))^(1/p)

# View the result of Power Mean output
power_mean_out #0.9146353

# Assuming the original range of Y is known
min_Y <- min(data.subset2[, 6])  # Original minimum of Y
max_Y <- max(data.subset2[, 6])  # Original maximum of Y

# Reverse Min-Max transformation to bring the Power Mean output back to the original scale of Y
power_mean_transformed_out <- power_mean_out * (max_Y - min_Y) + min_Y

# View the transformed output
power_mean_transformed_out

final_prediction <- power_mean_transformed_out

# View the final predicted output
final_prediction # 7.573177
# The value 7.573177 for final_prediction appears to be a reasonable prediction of wine quality, as it falls within the expected range of 0 to 10. Since the target variable 
#Y (wine quality) has a range between 0 and 10, there is no need for further transformation or scaling.
-----------------------------
#Since Y was not log-transformed, there is no need for exponentiation (10^(final_prediction)).
# Exponentiate if Y was log-transformed (e.g., log10)
final_prediction_out <- 10^(final_prediction)

# View the exponentiated result
final_prediction_out #37426277



##############################################################################################################################################################

#We will now check the application of linear regression model on df_transformed (the dataset we are working on)
View(data.transformed) #this is the transformed selected dataset (X1,X2,X3,X5,Y)

#Let us now translate this into a dataframe. The original df_transformed is a matrix
df_transformed_frame <- as.data.frame(data.transformed)

View(df_transformed_frame)

#Performing the lm fitment. Here [,5] is the Y element

lm_fit <- lm(df_transformed_frame[,5]~df_transformed_frame[,1]+df_transformed_frame[,2]+df_transformed_frame[,3]+df_transformed_frame[,4],
             data = df_transformed_frame)

summary(lm_fit)

#Next we will visualize the predicted Y values Vs the original Y values
View(lm_fit)
lm_model_data <- as.matrix(lm_fit[["model"]])
View(lm_model_data)

#True Values
lm_model_true_values <- lm_model_data[,1]
View(lm_model_true_values)

#Predicted Values
lm_model_predicted_values <- lm_fit[["fitted.values"]]
View(lm_model_predicted_values)

par(mfrow=c(1,1))
plot(lm_model_true_values, lm_model_predicted_values, main="Scatter plot b/w original values and
     predicted values for linear model", xlab="Original Values", 
     ylab="Predicted Values", col="black" )
abline(lm(lm_model_true_values~lm_model_predicted_values), 
       col="red", lty=1) 

#Let us now fit the WPM_P2 Model
# Reading from the previously generated file for WPM_P2_output.txt Model above

WPM_P2_model_output_data <- as.matrix(read.table("WPM_P2_output.txt"))
View(WPM_P2_model_output_data)

WPM_P2_model_true_values <- WPM_P2_model_output_data[,5]
WPM_P2_model_predicted_values <- WPM_P2_model_output_data[,6]

par(mfrow=c(1,1))
plot(WPM_P2_model_true_values, WPM_P2_model_predicted_values , main="Scatter plot b/w original values and
     predicted values for best fitting model (WPM_P2 Model)", xlab="Original Values", 
     ylab="Predicted Values", col="blue" )
abline(lm(WPM_P2_model_true_values~WPM_P2_model_predicted_values), 
       col="pink", lty=1) 


#We will now visualize both the original values and predicted values for the two models together

par(mfrow=c(1,2))
plot(lm_model_true_values, lm_model_predicted_values, main="Scatter plot b/w original values and
     predicted values for linear model", xlab="Original Values", 
     ylab="Predicted Values", col="maroon" )
abline(lm(lm_model_true_values~lm_model_predicted_values), 
       col="black", lty=1)

plot(WPM_P2_model_true_values, WPM_P2_model_predicted_values , main="Scatter plot b/w original values and
     predicted values for best fitting model (WPM - p=2)", xlab="Original Values", 
     ylab="Predicted Values", col="navy" )
abline(lm(WPM_P2_model_true_values~WPM_P2_model_predicted_values), 
       col="red", lty=1)

#Predicted Value Comparison b/w the two models (linear and WPM - p=2)
par(mfrow=c(1,1))
plot(WPM_P2_model_predicted_values, lm_model_predicted_values , 
     main="Scatter plot between predicted values for both models (WPM - p=2 and Linear Model)", xlab="WPM - p=2 Predicted Values", 
     ylab="Linear Predicted Values", col="navy" )
abline(lm(WPM_P2_model_predicted_values~lm_model_predicted_values), 
       col="red", lty=1)

#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment. 
#
#