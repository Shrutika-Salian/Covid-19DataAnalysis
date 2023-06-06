rm(list=ls()) #removes all variables stored previously
install.packages("Hmisc")
library(Hmisc) #import
COVID19_line_list_data <- read.csv("C:/Users/shrut/Downloads/COVID19_line_list_data.csv")
describe(COVID19_line_list_data)#Hmisc command

# cleaned up the death column
COVID19_line_list_data$death_new <- as.integer(COVID19_line_list_data$death != 0)

# Calculating death rate percentage
sum(COVID19_line_list_data$death_new)/ nrow(COVID19_line_list_data) * 100

#AGE
# claim:people who die are older
dead = subset(COVID19_line_list_data, death_new==1)
alive = subset(COVID19_line_list_data, death_new==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

#is this significant?
t.test(alive$age,dead$age, alternative = "two.side", conf.level = 0.95)

# if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, hence, we  we reject null hypothesis and
# conclude that this claim is statistically significant

#Gender
# claim:men have higher death rate than women
women = subset(COVID19_line_list_data, gender=="female")
men = subset(COVID19_line_list_data, gender=="male")
mean(women$death_new, na.rm = TRUE)
mean(men$death_new, na.rm = TRUE)

#is this significant?
t.test(women$death_new,men$death_new, alternative = "two.side", conf.level = 0.95)

# if p-value < 0.05, we reject null hypothesis
# here, p-value = 0.002105 ~ 0, hence, we  we reject null hypothesis and
# conclude that this claim is statistically significant

# Create age groups
age_groups <- cut(COVID19_line_list_data$age, breaks = c(0, 20, 40, 60, 80, 100), 
                  labels = c("0-20", "21-40", "41-60", "61-80", "81-100"))

# Count the number of cases and deaths in each age group
cases_count <- table(age_groups)
deaths_count <- table(age_groups[COVID19_line_list_data$death_new == 1])

# Plot the distribution of cases and deaths
barplot(cases_count, main = "Distribution of COVID-19 Cases by Age Group", xlab = "Age Group",
                             ylab = "Number of Cases", col = "lightblue")
barplot(deaths_count, main = "Distribution of COVID-19 Deaths by Age Group", xlab = "Age Group", 
                             ylab = "Number of Deaths", col = "salmon")

# Create a matrix of data for the combined bar graph
combined_data <- cbind(cases_count, deaths_count)
# Plot the combined bar graph
barplot(combined_data, beside = TRUE, col = c("lightblue", "salmon"), 
        main = "Distribution of COVID-19 Cases and Deaths by Age Group", 
        xlab = "Age Group", ylab = "Count",
        legend.text = c("Cases", "Deaths"), args.legend = list(x = "topright"))


#Number of cases by each location
cases_by_location <- table(COVID19_line_list_data$location)
# Sort the cases by location in descending order
cases_sorted <- sort(cases_by_location, decreasing = TRUE)
# Extract the top 5 locations and their counts
top_5_locations <- head(cases_sorted, 5)
# Generate a range of blue colors
colors <- colorRampPalette(c("lightblue", "darkblue"))(length(top_5_locations))
# Set the figure size
par(mar = c(5, 5, 4, 2) + 0.1, cex.lab = 1.2)
# Create the pie chart
pie(top_5_locations, labels = paste(names(top_5_locations), ": ", top_5_locations), col = colors,
    main = "Distribution of Top 5 highest number COVID-19 Cases by Location", cex.main = 1.2, cex.axis = 1.1)
# Add a legend
legend("topright", legend = names(top_5_locations), fill = colors, cex = 0.6)


#Number of deaths by each location
deaths_by_location <- table(cases_by_location[COVID19_line_list_data$death_new == 1])
# Sort the deaths by location in descending order
deaths_sorted <- sort(cases_by_location, decreasing = TRUE)
# Extract the top 5 locations and their death counts
top_5_deathlocations <- head(deaths_sorted, 5)
# Generate a range of blue colors
colors <- colorRampPalette(c("lightgreen", "darkgreen"))(length(top_5_deathlocations))
# Create the pie chart
pie(top_5_deathlocations, labels = paste(names(top_5_deathlocations), ": ", top_5_deathlocations), col = colors,
    main = "Distribution of Top 5 highest number COVID-19 Deaths by Location", cex.main = 1.2, cex.axis = 1.1)
# Add a legend
legend("topright", legend = names(top_5_deathlocations), fill = colors, cex = 0.6)




library(ggplot2)
install.packages("directlabels")
library(directlabels)
# Convert the reporting date to a date object
COVID19_line_list_data$reporting_date <- as.Date(COVID19_line_list_data$reporting_date, format = "%m/%d/%Y")

# Create a data frame with the count of cases by reporting date
cases_by_date <- aggregate(rep(1, nrow(COVID19_line_list_data)) ~ reporting_date, data = COVID19_line_list_data, FUN = length)
colnames(cases_by_date) <- c("reporting_date", "cases")

# Sort the data frame by reporting date
cases_by_date <- cases_by_date[order(cases_by_date$reporting_date), ]

# Create the line trend chart
ggplot(data = cases_by_date, aes(x = reporting_date, y = cases)) +
  geom_line() +
  geom_text(aes(label = cases), hjust = 0, vjust = -0.5, size = 3) +
  labs(x = "Reporting Date", y = "Number of Cases", title = "COVID-19 Cases Over Time") +
  theme_minimal()


