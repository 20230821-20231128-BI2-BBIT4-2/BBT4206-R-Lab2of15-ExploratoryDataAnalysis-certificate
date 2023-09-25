# *****************************************************************************
# Lab 2: Exploratory Data Analysis ----
#
# Course Code: BBT4206
# Course Name: Business Intelligence II
# Semester Duration: 21st August 2023 to 28th November 2023
#
# Lecturer: Allan Omondi
# Contact: aomondi [at] strathmore.edu
#
# Note: The lecture contains both theory and practice. This file forms part of
#       the practice. It has required lab work submissions that are graded for
#       coursework marks.
#
# License: GNU GPL-3.0-or-later
# See LICENSE file for licensing information.
# *****************************************************************************

# STEP 1. Install and use renv ----
# **Initialization: Install and use renv ----
# The R Environment ("renv") package helps you create reproducible environments
# for your R projects. This is helpful when working in teams because it makes
# your R projects more isolated, portable and reproducible.

# Further reading:
#   Summary: https://rstudio.github.io/renv/
#   More detailed article: https://rstudio.github.io/renv/articles/renv.html

# "renv" It can be installed as follows:
# if (!is.element("renv", installed.packages()[, 1])) {
#   install.packages("renv", dependencies = TRUE) # nolint
# }
# require("renv") # nolint

# Once installed, you can then use renv::init() to initialize renv in a new
# project.

# The prompt received after executing renv::init() is as shown below:
# This project already has a lockfile. What would you like to do?

# 1: Restore the project from the lockfile.
# 2: Discard the lockfile and re-initialize the project.
# 3: Activate the project without snapshotting or installing any packages.
# 4: Abort project initialization.

# Select option 1 to restore the project from the lockfile
# renv::init() # nolint

# This will set up a project library, containing all the packages you are
# currently using. The packages (and all the metadata needed to reinstall
# them) are recorded into a lockfile, renv.lock, and a .Rprofile ensures that
# the library is used every time you open that project.

# Consider a library as the location where packages are stored.
# Execute the following command to list all the libraries available in your
# computer:
.libPaths()

# One of the libraries should be a folder inside the project.

# Then execute the following command to see which packages are available in
# each library:
lapply(.libPaths(), list.files)

# This can also be configured using the RStudio GUI when you click the project
# file, e.g., "BBT4206-R.Rproj" in the case of this project. Then
# navigate to the "Environments" tab and select "Use renv with this project".

# As you continue to work on your project, you can install and upgrade
# packages, using either:
# install.packages() and update.packages or
# renv::install() and renv::update()

# You can also clean up a project by removing unused packages using the
# following command: renv::clean()

# After you have confirmed that your code works as expected, use
# renv::snapshot() to record the packages and their
# sources in the lockfile.

# Later, if you need to share your code with someone else or run your code on
# a new machine, your collaborator (or you) can call renv::restore() to
# reinstall the specific package versions recorded in the lockfile.

# Execute the following code to reinstall the specific package versions
# recorded in the lockfile (restart R after executing the command):
renv::restore()

# If you get several errors setting up renv and you prefer not to use it, then
# you can deactivate it using the following command (restart R after executing
# the command):
renv::deactivate()

# If renv::restore() did not install the "languageserver" package (required to
# use R for VS Code), then it can be installed manually as follows (restart R
# after executing the command):
if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

# Loading Datasets ----
## STEP 2: Download sample datasets ----
# Create a folder called "data" and store the following 2 files inside the
# "data" folder:
## Link 1 (save the file as "iris.data"):
# https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data
## Link 2 ("crop.data.csv"):
# https://cdn.scribbr.com/wp-content/uploads/2020/03/crop.data_.anova_.zip
# Extract the "crop.data.csv" file into the data folder

## STEP 3. Load the downloaded sample datasets ----
# Load the datasets
student_performance <- read.csv("data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset - dataset.csv", header = FALSE,
                         stringsAsFactors = TRUE)

# The following code (optional) can be used to name the attributes in the
# iris_dataset:

# names(iris_dataset) <- c("sepal length in cm", "sepal width in cm",
#                          "petal length in cm", "petal width in cm", "class")

if (!is.element("readr", installed.packages()[, 1])) {
  install.packages("readr", dependencies = TRUE)
}
require("readr")

student_performance <- read_csv(
  "data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset - dataset.csv",
  col_types = cols(
    gender = col_factor(levels = c("1", "0")),
    regret_choosing_bi = col_factor(levels = c("1", "0")),
    motivator = col_factor(levels = c("1", "0")),
    paid_tuition = col_factor(levels = c("1", "0")),
    extra_curricular = col_factor(levels = c("1", "0")),
    read_content_before_lecture = col_factor(levels = c("1", "2", "3", "4", "5")),
    anticipate_test_questions = col_factor(levels = c("1", "2", "3", "4", "5")),
    health = col_factor(levels = c("1", "2", "3", "4", "5")),
    GRADE = col_factor(levels = c("A", "B", "C", "D", "E"))
  )
)
library(vroom)
dat <- vroom("data/20230412-20230719-BI1-BBIT4-1-StudentPerformanceDataset - dataset.csv", delim = ",")
problems(dat)

View(student_performance)

## STEP 4. Load sample datasets that are provided as part of a package ----
if (!is.element("mlbench", installed.packages()[, 1])) {
  install.packages("mlbench", dependencies = TRUE)
}
require("mlbench")

#data("student_performance")
#data("BostonHousing")

# Dimensions ----
## STEP 5. Preview the Loaded Datasets ----
# Dimensions refer to the number of observations (rows) and the number of
# attributes/variables/features (columns). Execute the following commands to
# display the dimensions of your datasets:

dim(student_performance)
#dim(crop_dataset)
#dim(iris_dataset)
#dim(PimaIndiansDiabetes)

# Data Types ----
## STEP 6. Identify the Data Types ----
# Knowing the data types will help you to identify the most appropriate
# visualization types and algorithms that can be applied. It can also help you
# to identify the need to convert from categorical data (factors) to integers
# or vice versa where necessary. Execute the following command to identify the
# data types:
sapply(student_performance, class)
#sapply(crop_dataset, class)
#sapply(iris_dataset, class)
#sapply(PimaIndiansDiabetes, class)

# Descriptive Statistics ----

# You must first understand your data before you can use it to design
# prediction models and to make generalizable inferences. It is not until you
# take the time to truly understand your dataset that you can fully comprehend
# the context of the results you achieve. This understanding can be done using
# descriptive statistics such as:

# 1. Measures of frequency
# (e.g., count, percent)

# 2. Measures of central tendency
# (e.g., mean, median, mode)
# Further reading: https://www.scribbr.com/statistics/central-tendency/

# 3. Measures of distribution/dispersion/spread/scatter/variability
# (e.g., range, quartiles, interquartile range, standard deviation, variance,
# kurtosis, skewness)
# Further reading: https://www.scribbr.com/statistics/variability/
# Further reading:
#   https://digitaschools.com/descriptive-statistics-skewness-and-kurtosis/
# Further reading: https://www.scribbr.com/statistics/skewness/

# 4. Measures of relationship
# (e.g., covariance, correlation, ANOVA)

# Further reading: https://www.k2analytics.co.in/covariance-and-correlation/
# Further reading: https://www.scribbr.com/statistics/one-way-anova/
# Further reading: https://www.scribbr.com/statistics/two-way-anova/

# Understanding your data can lead to:
# (i)	  Data cleaning: Removing bad data or imputing missing data.
# (ii)	Data transformation: Reduce the skewness by applying the same function
#       to all the observations.
# (iii)	Data modelling: You may notice properties of the data such as
#       distributions or data types that suggest the use (or not) of
#       specific algorithms.

## Measures of Frequency ----

### STEP 7. Identify the number of instances that belong to each class. ----
# It is more sensible to count categorical variables (factors or dimensions)
# than numeric variables, e.g., counting the number of male and female
# participants instead of counting the frequency of each participant’s height.
student_performance_freq <- student_performance$gender
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$regret_choosing_bi
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$motivator
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$paid_tuition
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$extra_curricular
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$read_content_before_lecture
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$anticipate_test_questions
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$health
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)

student_performance_freq <- student_performance$GRADE
cbind(frequency = table(student_performance_freq),
      percentage = prop.table(table(student_performance_freq)) * 100)


## Measures of Central Tendency ----
### STEP 8. Calculate the mode ----
# Unfortunately, R does not have an in-built function for calculating the mode.
# We, therefore, must manually create a function that can calculate the mode.

student_performance_gender_mode <- names(table(student_performance$gender))[
  which(table(student_performance$gender) == max(table(student_performance$gender)))
]
print(student_performance_gender_mode)

## Measures of Distribution/Dispersion/Spread/Scatter/Variability ----

### STEP 9. Measure the distribution of the data for each variable ----
summary(student_performance)
#summary(crop_dataset)
#summary(iris_dataset)
#summary(PimaIndiansDiabetes)

### STEP 10. Measure the standard deviation of each variable ----
# Measuring the variability in the dataset is important because the amount of
# variability determines how well you can generalize results from the sample
# dataset to a new observation in the population.

# Low variability is ideal because it means that you can better predict
# information about the population based on sample data. High variability means
# that the values are less consistent, thus making it harder to make
# predictions.

# The format “dataset[rows, columns]” can be used to specify the exact rows and
# columns to be considered. “dataset[, columns]” implies all rows will be
# considered. Specifying “BostonHousing[, -4]” implies all the columns except
# column number 4. This can also be stated as
# “BostonHousing[, c(1,2,3,5,6,7,8,9,10,11,12,13,14)]”. This allows us to
# calculate the standard deviation of only columns that are numeric, thus
# leaving out the columns termed as “factors” (categorical) or those that have
# a string data type.

numeric_columns <- student_performance [, sapply(student_performance, is.numeric)]


# The data type of "yield" should be double (not numeric) so that it can be
# calculated.
standard_deviation <- sapply(numeric_columns, sd)

print(standard_deviation)

### STEP 11. Measure the variance of each variable ----
numeric_columns <- student_performance [, sapply(student_performance, is.numeric)]

variance <- sapply(numeric_columns, sd)

print(variance)

### STEP 12. Measure the kurtosis of each variable ----
# The Kurtosis informs you of how often outliers occur in the results.

# There are different formulas for calculating kurtosis.
# Specifying “type = 2” allows us to use the 2nd formula which is the same
# kurtosis formula used in SPSS and SAS. More details about any function can be
# obtained by searching the R help knowledge base. The knowledge base says:

# In “type = 2” (used in SPSS and SAS):
# 1.	Kurtosis < 3 implies a low number of outliers
# 2.	Kurtosis = 3 implies a medium number of outliers
# 3.	Kurtosis > 3 implies a high number of outliers

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

numeric_columns <- student_performance[, sapply(student_performance, is.numeric)]
kurtosis_values <- sapply(numeric_columns, kurtosis, type = 2)

### STEP 13. Measure the skewness of each variable ----

# The skewness informs you of the asymmetry of the distribution of results.
# Similar to kurtosis, there are several ways of computing the skewness.
# Using “type = 2” can be interpreted as:

# 1.	Skewness between -0.4 and 0.4 (inclusive) implies that there is no skew
# in the distribution of results; the distribution of results is symmetrical;
# it is a normal distribution.
# 2.	Skewness above 0.4 implies a positive skew; a right-skewed distribution.
# 3.	Skewness below -0.4 implies a negative skew; a left-skewed distribution.

numeric_columns <- student_performance[, sapply(student_performance, is.numeric)]
skewness_values <- sapply(numeric_columns, skewness, type = 2)

# Note, executing:
# skewness(BostonHousing$crim, type=2) # nolint
# computes the skewness for one variable called “crim” in the BostonHousing
# dataset. However, executing the following enables you to compute the skewness
# for all the variables in the “BostonHousing” dataset except variable number 4:

# sapply(BostonHousing[,-4],  skewness, type = 2) # nolint

## Measures of Relationship ----

## STEP 14. Measure the covariance between variables ----
# Note that the covariance and the correlation are computed for numeric values
# only, not categorical values.


student_performance_cov <- cov(student_performance[, sapply(student_performance, is.numeric)])
View(student_performance_cov)


## STEP 15. Measure the correlation between variables ----

student_performance_cor <- cor(student_performance[, sapply(student_performance, is.numeric)])
View(student_performance_cor)




# Inferential Statistics ----
# Read the following article:
#   https://www.scribbr.com/statistics/inferential-statistics/
# Statistical tests (either for comparison, correlation, or regression) can be
# used to conduct *hypothesis testing*.

## Parametric versus Non-Parametric Statistical Tests ----
# If all the 3 points below are true, then
# use parametric tests, else use non-parametric tests.
# (i)	  the population that the sample comes from follows a normal distribution
#       of scores
# (ii)  the sample size is large enough to represent the population
# (iii) the variances of each group being compared are similar

## Statistical tests for comparison ----
# (i)	  t Test: parametric; compares means; uses 2 samples.
# (ii)	ANOVA: parametric; compares means; can use more than 3 samples.
# (iii)	Mood’s median: non-parametric; compares medians; can use more than 2
#       samples.
# (iv)	Wilcoxon signed-rank: non-parametric; compares distributions; uses 2
#       samples.
# (v)	  Wilcoxon rank-sum (Mann-Whitney U): non-parametric; compares sums of
#       rankings; uses 2 samples.
# (vi)	Kruskal-Wallis H: non-parametric; compares mean rankings; can use more
#       than 3 samples.

## Statistical tests for correlation ----
# (i)	  Pearson’s r: parametric; expects interval/ratio variables.
# (ii)	Spearman’s r: non-parametric; expects ordinal/interval/ratio variables.
# (iii)	Chi square test of independence: non-parametric; nominal/ordinal
#       variables.

## Statistical tests for regression ----
# (i)	  Simple linear regression: predictor is 1 interval/ratio variable;
#       outcome is 1 interval/ratio variable.
# (ii)	Multiple linear regression: predictor can be more than 2 interval/ratio
#       variables; outcome is 1 interval/ratio variable.
# (iii)	Logistic regression: predictor is 1 variable (any type); outcome is 1
#       binary variable.
# (iv)	Nominal regression: predictor can be more than 1 variable; outcome is 1
#       nominal variable.
# (v)	  Ordinal regression: predictor can be more than 1 variable; outcome is 1
#       ordinal variable.

## STEP 16. Perform ANOVA on the “crop_dataset” dataset ----
# ANOVA (Analysis of Variance) is a statistical test used to estimate how a
# quantitative dependent variable changes according to the levels of one or
# more categorical independent variables.

# The null hypothesis (H0) of the ANOVA is that
# “there is no difference in means”, and
# the alternative hypothesis (Ha) is that
# “the means are different from one another”.

# We can use the “aov()” function in R to calculate the test statistic for
# ANOVA. The test statistic is in turn used to calculate the p-value of your
# results. A p-value is a number that describes how likely you are to have
# found a particular set of observations if the null hypothesis were true. The
# smaller the p-value, the more likely you are to reject the null-hypothesis.

# The “crop_dataset” sample dataset loaded in STEP 4 contains observations from
# an imaginary study of the effects of fertilizer type and planting density on
# crop yield. In other words:

# Dependent variable:	GRADE
# Independent variables:	gender, paid_tuition and regret_choosing_bi

# The features (attributes) are:
# 1.	density: planting density (1 = low density, 2 = high density)
# 2.	block: planting location in the field (blocks 1, 2, 3, or 4)
# 3.	fertilizer: fertilizer type (type 1, 2, or 3)
# 4.	final crop yield (in bushels per acre)

# One-Way ANOVA can be used to test the effect of the 3 types of fertilizer on
# crop yield whereas,
# Two-Way ANOVA can be used to test the effect of the 3 types of fertilizer and
# the 2 types of planting density on crop yield.
#student_performance_one_way_anova <- aov(regret_choosing_bi ~ paid_tuition, data = student_performance)
#summary(student_performance_one_way_anova)

logistic_model <- glm(regret_choosing_bi ~ GRADE, data = student_performance, family = "binomial")
summary(logistic_model)

student_performance$regret_choosing_bi <- as.numeric(student_performance$regret_choosing_bi)  # Convert to numeric
student_performance_one_way_anova <- aov(regret_choosing_bi ~ GRADE, data = student_performance)
summary(student_performance_one_way_anova)


# This shows the result of each variable and the residual. The residual refers
# to all the variation that is not explained by the independent variable. The
# list below is a description of each column in the result:

# 1.  Df column: Displays the degrees of freedom for the independent variable
#           (the number of levels (categories) in the variable minus 1),
#           and the degrees of freedom for the residuals (the total
#           number of observations minus the number of variables being
#           estimated + 1, i.e., (df(Residuals)=n-(k+1)).

# 2.	Sum Sq column: Displays the sum of squares (a.k.a. the total variation
#           between the group means and the overall mean). It is better to have
#           a lower Sum Sq value for residuals.

# 3.  Mean Sq column: The mean of the sum of squares, calculated by dividing
#           the sum of squares by the degrees of freedom for each parameter.

# 4.	F value column: The test statistic from the F test. This is the mean
#           square of each independent variable divided by the mean square of
#           the residuals. The larger the F value, the more likely it is that
#           the variation caused by the independent variable is real and not
#           due to chance.

# 5.	Pr(>F) column: The p-value of the F statistic. This shows how likely it
#           is that the F value calculated from the test would have occurred if
#           the null hypothesis of “no difference among group means” were true.

# The three asterisk symbols (***) implies that the p-value is less than 0.001.
# P<0.001 can be interpreted as “the type of fertilizer used has an impact on
# the final crop yield”.

# We can also have a situation where the final crop yield depends not only on
# the type of fertilizer used but also on the planting density. A two-way ANOVA
# can then be used to confirm this. Execute the following for a two-way ANOVA
# (two independent variables):

student_performance_two_way_anova <- aov(regret_choosing_bi ~ GRADE + read_content_before_lecture, # nolint
                                           data = student_performance)
summary(student_performance_two_way_anova)

# Specifying an asterisk (*) instead of a plus (+) between the two independent
# variables (fertilizer * density) implies that they have an interaction effect
# rather than an additive effect.

# For example, an interaction effect would be that the fertilizer uptake by
# plants is affected by how close the plants are planted (density). An additive
# effect would be that the fertilizer uptake by plants is NOT affected by how
# close the plants are planted (density).

# Execute the following to perform a two-way ANOVA with the assumption that
# fertilizer and density have an interaction effect:

student_performance_two_way_anova <- aov(regret_choosing_bi ~ GRADE * read_content_before_lecture, # nolint
                                              data = student_performance)
summary(student_performance_two_way_anova)

# This can be interpreted as follows:
# The additive two-way ANOVA shows that the crop yield is affected by both the
# fertilizer and the density (P<0.001 for both independent variables).
# The interactive two-way ANOVA also shows that the crop yield is affected by
# both the fertilizer and the density (P<0.001 for both independent variables).
# However, the “fertilizer:density” variable has a high p-value (p=0.532500)
# which implies that there is not much variation in the dependent variable
# (final crop yield) that can be explained by the interaction between the
# independent variables (fertilizer and density).

# An ANOVA can also be performed on the variable called “block”. The “block”
# variable specifies the planting location in the field (blocks 1, 2, 3, or 4).
# Different blocks can have different control measures enforced to reduce the
# influence of confounding variables, e.g., temperature, water, soil quality,
# and plant species. This is common practice in research to ensure that the
# change in the dependent variable is correlated with the independent variable
# and not other variables that are not part of the research.

# Execute the following to add the “block” variable:
student_performance_interactive_two_way_anova_with_block <- aov(regret_choosing_bi ~ # nolint
                                                           GRADE + read_content_before_lecture
                                                           + anticipate_test_questions ,
                                                         data = student_performance)
summary(student_performance_interactive_two_way_anova_with_block)

# This can be interpreted as follows:
# The additive two-way ANOVA shows that the crop yield is affected by both the
# fertilizer and the density (P<0.001 for both independent variables). However,
# the block variable has a high p-value (p=0.488329) which implies that there
# is not much variation in the dependent variable (final crop yield) that can
# be explained by the “block” variable (the different blocks used to plant the
# crops).

# Qualitative Data Analysis ----
# This can be done through either
# thematic analysis:
#   https://www.scribbr.com/methodology/thematic-analysis/ or
# critical discourse analysis:
#   https://www.scribbr.com/methodology/discourse-analysis/

# Basic Visualization for Understanding the Dataset ----

# Note: If you are using R Studio, ensure that the "Plots" window on the bottom
# right of R Studio has enough space to display the chart.

# The fastest way to improve your understanding of the dataset is to visualize
# it. Visualization can help you to spot outliers and give you an idea of
# possible data transformations you can apply. The basic visualizations to
# understand your dataset can be univariate visualizations (helps you to
# understand a single attribute) or multivariate visualizations (helps you to
# understand the interaction between attributes). Packages used to create
# visualizations include:
# (i)	  Graphics package: Used to quickly create basic plots of data. This is
#       the most appropriate to quickly understand the dataset before
#       conducting further analysis.
# (ii)  Lattice package: Used to create more visually appealing plots of data.
# (iii) ggplot2 package: Used to create even more visually appealing plots of
#       data that can then be used to present the analysis results to the
#       intended users. Given its complexity, it is not necessary to use
#       ggplot2 to have a basic understanding of the dataset prior to further
#       analysis.

# Note that the goal at this point is to understand your data, not to create
# visually appealing plots that are publicly shared. The visually appealing
# plots will be created much later after the best prediction model has been
# chosen.

## Univariate Plots ----
### STEP 17. Create Histograms for Each Numeric Attribute ----
# Histograms help in determining whether an attribute has a Gaussian
# distribution. They can also be used to identify the presence of outliers.

# Execute the following code to create histograms for the “BostonHousing”
# dataset:
par(mfrow = c(1, 3))
for (i in 1:3) {
  hist(BostonHousing[, i], main = names(BostonHousing)[i])
}
hist(BostonHousing[, 5], main = names(BostonHousing)[5])
hist(BostonHousing[, 6], main = names(BostonHousing)[6])
hist(BostonHousing[, 7], main = names(BostonHousing)[7])
hist(BostonHousing[, 8], main = names(BostonHousing)[8])
hist(BostonHousing[, 9], main = names(BostonHousing)[9])
hist(BostonHousing[, 10], main = names(BostonHousing)[10])
hist(BostonHousing[, 11], main = names(BostonHousing)[11])
hist(BostonHousing[, 12], main = names(BostonHousing)[12])
hist(BostonHousing[, 13], main = names(BostonHousing)[13])
hist(BostonHousing[, 14], main = names(BostonHousing)[14])

# Execute the following code to create one histogram for attribute 4 (the only
# numeric column was “final crop yield (in bushels per acre)”) in the
# “crop_dataset” dataset:
# The code below converts column number 4 into unlisted and numeric data first
# so that a histogram can be plotted. Further reading:
# https://www.programmingr.com/r-error-messages/x-must-be-numeric-error-in-r-histogram/ ) # nolint
student_performance <- as.numeric(unlist(student_performance))
hist(student_performance, main = names(student_performance))

# Execute the following code to create histograms for attribute 1 to 4 of the
# “iris_dataset” dataset:
par(mfrow = c(1, 7))
for (i in 1:7) {
  hist(student_performance[, i], main = names(student_performance)[i])
}

#sampled
# Set the layout for the histograms
par(mfrow = c(1, ncol(student_performance)))

# Loop through each numeric column and create a histogram
for (i in 1:ncol(student_performance)) {
  if (is.numeric(student_performance[, i])) {
    hist(student_performance[, i], main = names(student_performance)[i])
  }
}

# Execute the following code to create histograms for the “PimaIndiansDiabetes”
# dataset:

par(mfrow = c(1, 8))
for (i in 1:8) {
  hist(PimaIndiansDiabetes[, i], main = names(PimaIndiansDiabetes)[i])
}

### STEP 18. Create Box and Whisker Plots for Each Numeric Attribute ----
# Box and whisker plots are useful in understanding the distribution of data.
# Further reading: https://www.scribbr.com/statistics/interquartile-range/

# Execute the following code to create box and whisker plots for the
# “BostonHousing” dataset:
# This considers the first 3 attributes which are numeric. The fourth attribute
# in the dataset is of the type “factor”, i.e., categorical.

par(mfrow = c(1, 3))
for (i in 1:3) {
  boxplot(BostonHousing[, i], main = names(BostonHousing)[i])
}

# This considers the 5th to the 14th attributes which are numeric.
# The fourth attribute in the dataset is of the type “factor”, i.e., categorical

boxplot(BostonHousing[, 5], main = names(BostonHousing)[5])
boxplot(BostonHousing[, 6], main = names(BostonHousing)[6])
boxplot(BostonHousing[, 7], main = names(BostonHousing)[7])
boxplot(BostonHousing[, 8], main = names(BostonHousing)[8])
boxplot(BostonHousing[, 9], main = names(BostonHousing)[9])
boxplot(BostonHousing[, 10], main = names(BostonHousing)[10])
boxplot(BostonHousing[, 11], main = names(BostonHousing)[11])
boxplot(BostonHousing[, 12], main = names(BostonHousing)[12])
boxplot(BostonHousing[, 13], main = names(BostonHousing)[13])
boxplot(BostonHousing[, 14], main = names(BostonHousing)[14])

boxplot(crop_dataset[, 4], main = names(crop_dataset)[4])
# If the line above results in an error, execute the code below to convert
# column number 4 into unlisted and numeric data first.
# Further reading:
# https://www.programmingr.com/r-error-messages/x-must-be-numeric-error-in-r-histogram/ # nolint
student_performance <- as.numeric(unlist(student_performance))
boxplot(student_performance, main = names(student_performance))

# Execute the following code to create box and whisker plots for attribute
# 1 to 4 of the “iris_dataset” dataset:
#par(mfrow = c(1, 4))
#for (i in 1:4) {
 # boxplot(iris_dataset[, i], main = names(iris_dataset)[i])
#}

# Execute the following code to create histograms for the “PimaIndiansDiabetes”
# dataset:
#par(mfrow = c(1, 8))
#for (i in 1:8) {
 # boxplot(PimaIndiansDiabetes[, i], main = names(PimaIndiansDiabetes)[i])
#}

### STEP 19. Create Bar Plots for Each Categorical Attribute ----
# Categorical attributes (factors) can also be visualized. This is done using a
# bar chart to give an idea of the proportion of instances that belong to each
# category.

# Execute the following to create a bar plot for the categorical attribute 4
# (“chas” - Charles River dummy variable (= 1 if tract bounds river;
# 0 otherwise)) in the “BostonHousing” dataset:

barplot(table(student_performance), main = names(student_performance))

# The features (attributes) in the “crop_dataset” dataset are:
# 1.	density: planting density (1 = low density, 2 = high density)
# 2.	block: planting location in the field (blocks 1, 2, 3, or 4)
# 3.	fertilizer: fertilizer type (type 1, 2, or 3)
# 4.	final crop yield (in bushels per acre)

# Execute the following to create a bar plot for the categorical attributes
# 1 to 3 in the “crop_dataset” dataset:

#par(mfrow = c(1, 3))
#for (i in 1:3) {
 # barplot(table(crop_dataset[, i]), main = names(crop_dataset)[i])
#}

# The attributes in the “iris_dataset” dataset are:
# 1.	sepal length in cm
# 2.	sepal width in cm
# 3.	petal length in cm
# 4.	petal width in cm
# 5.	class:
##   a.	Iris-Setosa
##   b.	Iris-Versicolour
##   c.	Iris-Virginica
# 1-4 are the Predictor (Independent) Variables
# 5 is the Target (Dependent) Variable

# Execute the following to create a bar plot for the categorical attribute 5
# (class) in the “iris_dataset” dataset:

barplot(table(iris_dataset[, 5]), main = names(iris_dataset)[5])

# Execute the following to create a bar plot for attribute number 9
# (diabetes – “pos” - had diabetes and “neg” - did not have diabetes) in the
# “PimaIndiansDiabetes” dataset:

barplot(table(PimaIndiansDiabetes[, 9]), main = names(PimaIndiansDiabetes)[9])

### STEP 20. Create a Missingness Map to Identify Missing Data ----
# Some machine learning algorithms cannot handle missing data. A missingness
# map (also known as a missing plot) can be used to get an idea of the amount
# missing data in the dataset. The x-axis of the missingness map shows the
# attributes of the dataset whereas the y-axis shows the instances in the
# dataset.
# Horizontal lines indicate missing data for an instance whereas vertical lines
# indicate missing data for an attribute. The missingness map requires the
# “Amelia” package.

# Execute the following to create a map to identify the missing data in each
# dataset:
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE)
}
require("Amelia")
missmap(student_performance, col = c("red", "grey"), legend = TRUE)

# Convert factor columns representing ordinal data to numeric
#ordinal_columns <- c("gender", "regret_choosing_bi", "motivator", "paid_tuition", "extra_curricular", "GRADE", "read_content_before_lecture", "anticipate_test_questions", "health")
#student_performance[, ordinal_columns] <- lapply(student_performance[, ordinal_columns], as.numeric)


#missmap(student_performance, col = c("red", "grey"), legend = TRUE)
#missmap(crop_dataset, col = c("red", "grey"), legend = TRUE)
#missmap(iris_dataset, col = c("red", "grey"), legend = TRUE)
#missmap(PimaIndiansDiabetes, col = c("red", "grey"), legend = TRUE)

# As shown in the results, the 4 datasets that were loaded in this lab have no
# missing data. We can load a 5th dataset called “Soybean” found in the
# “mlbench” package for an example of a dataset that has missing data. Execute
# the following for an example of a dataset that has missing data:
data(student_performance)
View(student_performance)
missmap(student_performance, col = c("red", "grey"), legend = TRUE)

## Multivariate Plots ----

### STEP 21. Create a Correlation Plot ----
# Correlation plots can be used to get an idea of which attributes change
# together. The function “corrplot()” found in the package “corrplot” is
# required to perform this. The larger the dot in the correlation plot, the
# larger the correlation. Blue represents a positive correlation whereas red
# represents a negative correlation.

# Execute the following to create correlation plots for 3 of the datasets
# loaded in STEP 2 to STEP 4:
if (!is.element("corrplot", installed.packages()[, 1])) {
  install.packages("corrplot", dependencies = TRUE)
}
require("corrplot")
corrplot(cor(student_performance), method = "circle")


#corrplot(cor(iris_dataset[, 1:4]), method = "circle")
#corrplot(cor(PimaIndiansDiabetes[, 1:8]), method = "circle")

# The reason why the “crop_dataset” dataset has no correlation plot is because
# it does not have at least 2 numeric attributes to compare.

# Alternatively, the 'ggcorrplot::ggcorrplot()' function can be used to plot a
# more visually appealing plot.
# The code below shows how to install a package in R:
if (!is.element("ggcorrplot", installed.packages()[, 1])) {
  install.packages("ggcorrplot", dependencies = TRUE)
}
require("ggcorrplot")
ggcorrplot(cor(student_performance))
#ggcorrplot(cor(iris_dataset[, 1:4]))
#ggcorrplot(cor(PimaIndiansDiabetes[, 1:8]))

### STEP 22. Create a Scatter Plot ----
pairs(student_performance)  ~  + GRADE
pairs(regret_choosing_bi ~ ., data = student_performance, col = student_performance$regret_choosing_bi)
pairs(paid_tuition ~ ., data = student_performance, col = student_performance$paid_tuition)
pairs(fertilizer ~ ., data = crop_dataset, col = crop_dataset$fertilizer)
pairs(V5 ~ ., data = iris_dataset, col = iris_dataset$V5)
pairs(diabetes ~ ., data = PimaIndiansDiabetes,
      col = PimaIndiansDiabetes$diabetes)



# Alternatively, the ggcorrplot package can be used to make the plots more
# appealing:
ggplot(PimaIndiansDiabetes,
       aes(x = age, y = pregnant, shape = diabetes, color = diabetes)) +
  geom_point() +
  geom_smooth(method = lm)

### STEP 23. Create Multivariate Box and Whisker Plots by Class ----
# This applies to datasets where the target (dependent) variable is categorical.
# Execute the following code:
if (!is.element("caret", installed.packages()[, 1])) {
  install.packages("caret", dependencies = TRUE)
}
require("caret")
featurePlot(x = iris_dataset[, 1:4], y = iris_dataset[, 5], plot = "box")
featurePlot(x = PimaIndiansDiabetes[, 1:8], y = PimaIndiansDiabetes[, 9],
            plot = "box")

# **Deinitialization: Create a snapshot of the R environment ----
# Lastly, as a follow-up to the initialization step, record the packages
# installed and their sources in the lockfile so that other team-members can
# use renv::restore() to re-install the same package version in their local
# machine during their initialization step.
renv::snapshot()

# References ----
## Bevans, R. (2023a). ANOVA in R | A Complete Step-by-Step Guide with Examples. Scribbr. Retrieved August 24, 2023, from https://www.scribbr.com/statistics/anova-in-r/ # nolint ----

## Bevans, R. (2023b). Sample Crop Data Dataset for ANOVA (Version 1) [Dataset]. Scribbr. https://www.scribbr.com/wp-content/uploads//2020/03/crop.data_.anova_.zip # nolint ----

## Fisher, R. A. (1988). Iris [Dataset]. UCI Machine Learning Repository. https://archive.ics.uci.edu/dataset/53/iris # nolint ----

## National Institute of Diabetes and Digestive and Kidney Diseases. (1999). Pima Indians Diabetes Dataset [Dataset]. UCI Machine Learning Repository. https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database # nolint ----

## StatLib CMU. (1997). Boston Housing [Dataset]. StatLib Carnegie Mellon University. http://lib.stat.cmu.edu/datasets/boston_corrected.txt # nolint ----


# **Required Lab Work Submission** ----

# NOTE: The lab work should be done in groups of between 2 and 5 members using
#       Git and GitHub.

## Part A ----
# Create a new file in the project's root folder called
# "Lab2-Submission-ExploratoryDataAnalysis.R".
# Use this file to provide all the code you have used to perform an exploratory
# data analysis of the "Class Performance Dataset" provided on the eLearning
# platform.

## Part B ----
# Upload *the link* to your "Lab2-Submission-ExploratoryDataAnalysis.R" hosted
# on Github (do not upload the .R file itself) through the submission link
# provided on eLearning.

## Part C ----
# Create a markdown file called "Lab-Submission-Markdown.Rmd"
# and place it inside the folder called "markdown". Use R Studio to ensure the
# .Rmd file is based on the "GitHub Document (Markdown)" template when it is
# being created.

# Refer to the following file in Lab 1 for an example of a .Rmd file based on
# the "GitHub Document (Markdown)" template:
#     https://github.com/course-files/BBT4206-R-Lab1of15-LoadingDatasets/blob/main/markdown/BIProject-Template.Rmd # nolint

# Include Line 1 to 14 of BIProject-Template.Rmd in your .Rmd file to make it
# displayable on GitHub when rendered into its .md version

# It should have code chunks that explain only *the most significant*
# analysis performed on the dataset.

# The emphasis should be on Explanatory Data Analysis (explains the key
# statistics performed on the dataset) as opposed to
# Exploratory Data Analysis (presents ALL the statistics performed on the
# dataset). Exploratory Data Analysis that presents ALL the possible statistics
# re-creates the problem of information overload.

## Part D ----
# Render the .Rmd (R markdown) file into its .md (markdown) version by using
# knitR in RStudio.

# You need to download and install "pandoc" to render the R markdown.
# Pandoc is a file converter that can be used to convert the following files:
#   https://pandoc.org/diagram.svgz?v=20230831075849

# Documentation:
#   https://pandoc.org/installing.html and
#   https://github.com/REditorSupport/vscode-R/wiki/R-Markdown

# By default, Rmd files are open as Markdown documents. To enable R Markdown
# features, you need to associate *.Rmd files with rmd language.
# Add an entry Item "*.Rmd" and Value "rmd" in the VS Code settings editor.

# Documentation of knitR: https://www.rdocumentation.org/packages/knitr/

# Upload *the link* to "Lab-Submission-Markdown.md" (not .Rmd)
# markdown file hosted on Github (do not upload the .Rmd or .md markdown files)
# through the submission link provided on eLearning.