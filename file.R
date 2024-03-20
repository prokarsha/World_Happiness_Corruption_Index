library(dplyr)
library(ggplot2)
library(e1071)
library(corrplot)
library(shiny)
library(readr)
library(caret)
library(GGally)
library(ggcorrplot)

df = read.csv(file.choose())
View(df)


# Statistics Summary
summary(df)


# EDA with ggplot2
ggplot(df, aes(x = gdp_per_capita, y = happiness_score)) +
  geom_point() +
  labs(title = "Scatter plot of GDP per Capita vs Happiness Score")

# Plot happiness score by continent and year
ggplot(df, aes(x = Year, y = happiness_score, color = continent)) +
  geom_line() +
  geom_point() +
  labs(title = "Happiness Score by Continent Over Years",
       x = "Year",
       y = "Happiness Score",
       color = "Continent") +
  theme_minimal()

# Government Trust: Line plot
ggplot(df, aes(x = Year, y = government_trust, color = continent)) +
  geom_line() +
  geom_point() +
  labs(title = "Government Trust by Continent Over Years",
       x = "Year",
       y = "Government Trust",
       color = "Continent") +
  theme_minimal()

# Freedom: Bar plot
ggplot(df, aes(x = continent, y = freedom, fill = continent)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Freedom by Continent",
       x = "Continent",
       y = "Average Freedom") +
  theme_minimal()

# Family: Box plot
ggplot(df, aes(x = continent, y = family, fill = continent)) +
  geom_boxplot() +
  labs(title = "Family Score by Continent",
       x = "Continent",
       y = "Family Score") +
  theme_minimal()

# Health: Violin plot
ggplot(df, aes(x = continent, y = health, fill = continent)) +
  geom_violin() +
  labs(title = "Health Score by Continent",
       x = "Continent",
       y = "Health Score") +
  theme_minimal()

# CPI Score: Scatter plot
ggplot(df, aes(x = health, y = cpi_score, color = continent)) +
  geom_point() +
  labs(title = "CPI Score vs Health Score",
       x = "Health Score",
       y = "CPI Score",
       color = "Continent") +
  theme_minimal()

# Select only the numeric columns for pairplot and correlation plot
numeric_df <- select(df, -continent, -Country, -Year)

# 1. Pairplot
ggpairs(numeric_df)

# 2.Correlation plot
corr <- cor(numeric_df)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

# Machine learning with randomForest
library(randomForest)
model <- randomForest(happiness_score ~ gdp_per_capita + family + health + freedom + generosity + government_trust, data = df)
print(model)
plot(model)


## DASHBOARD

# Define UI
ui <- fluidPage(
  titlePanel("Happiness Index Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Select Continent:", choices = unique(df$continent)),
      selectInput("country", "Select Country:", choices = unique(df$Country))
    ),
    mainPanel(
      tableOutput("index_values")
    )
  )
)

# Define server
server <- function(input, output) {
  output$index_values <- renderTable({
    filtered_data <- df[df$continent == input$continent & df$Country == input$country, ]
    filtered_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)












# Load required packages
library(readr)
library(caret)


# Split the data into features (X) and target variable (y)
X <- df[, c("gdp_per_capita", "family", "health", "freedom", "generosity", "government_trust")]
y <- df$happiness_score

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Build the linear regression model
lm_model <- lm(happiness_score ~ ., data = df)

# Print the summary of the model
summary(lm_model)

# Plot the model
plot(lm_model)


# Predict happiness scores on the testing data
y_pred <- predict(lm_model)

# Evaluate the model
mse <- mean((y_test - y_pred)^2)
rmse <- sqrt(mse)
r_squared <- cor(y_test, y_pred)^2

# Print evaluation metrics
cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

