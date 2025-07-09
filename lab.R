library("dplyr")
library("ggplot2")

# Load the dataset into R.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
df <- read.csv("Sample - Superstore.csv")

# Explore the dataset using str(), head(), and summary().

print(head(df))
print(str(df))
print(summary(df))

# Identify the number of rows and columns.

print(length(names(df))) # 21 Columns
print(nrow(df)) # 9994 Rows

# Select the following columns: 
# Order ID, Order Date, Customer Name, Sales, Profit.
order_df <- df %>%
    select(Order.ID, Order.Date, Customer.Name, Sales, Profit)

# print(head(order_df))

# Filter the dataset to show only orders with a profit greater than $100.
hight_profit_orders <- order_df %>%
    filter(Profit > 100)

# print(min(hight_profit_orders$Profit ))

# Sort the dataset by Sales in descending order.
sorted_df_1 = hight_profit_orders[order(hight_profit_orders$Sales, decreasing = TRUE),]
sorted_df_total = df[order(df$Sales, decreasing = TRUE),]

# print(head(sorted_df_total))

# Check for missing values in the dataset.
missing_data = any(is.na(df$Postal.Code))
# print(missing_data)

# Replace missing values in the Postal Code column with the mode (most frequent value).
# find mode
postal_code_mode <- df %>%
                    count(Postal.Code) %>%
                    arrange(desc(n)) %>%
                    slice(1)

mode_value <- postal_code_mode$Postal.Code[1]
print(postal_code_mode) # 10035

# replace na values 
df$Postal.Code[is.na(df$Postal.Code)] <- mode_value

# Remove rows with missing values in the Customer Name column.
df <- df %>%
    filter(!is.na(Customer.Name))

# Create a new column Profit_Margin as the ratio of Profit to Sales.
df <- df %>%
    mutate(Profit_Margin= df$Profit / df$Sales)

# print(head(df))
# Create a new column Order_Year by extracting the year from Order Date.
df <- df %>%
    mutate(Order_Year = as.numeric(format(as.Date(Order.Date, "%d/%m/%Y"), "%Y")))

# print(head(df))
# Convert the Order.Date column to a Date data type.
df <- df %>%
    mutate(Order.Date = as.Date(Order.Date, "%d/%m/%Y"))

# Calculate the total sales and profit by Category.
df_group <- df %>%
    group_by(Category) %>%
    summarise(
        total_sales = sum(Sales),
        total_profit = sum(Profit)
    )
print(df_group)

# Find the average profit margin by Region.
df_avg <- df %>%
    group_by(Region) %>%
    summarise(
        avg_margin = mean(Profit_Margin)
    )
print(df_avg)

# Count the number of orders by Customer Segment.
df_count <- df %>%
    group_by(Segment) %>%
    summarise(
        order_count = n()
    )
print(df_count)

# Identify and remove duplicate rows based on Order ID.
df_unique <- df %>%
  distinct(Order.ID, .keep_all = TRUE)

# print(nrow(df_unique)) # 5009

# Create a new column Discount_Level that categorizes discounts as "Low" (<10%), "Medium" (10-20%), and "High" (>20%).
df <- df %>%
    mutate(
        Discount_Level = case_when(
            Discount < 0.1 ~ "Low",
            Discount >= 0.1 & Discount < 0.2 ~ "Medium",
            Discount >= 0.2 ~ "High"
        )
    )
# print(head(df))


# Merge the dataset with a new dataset containing regional population data (create a dummy dataset for this purpose).
regional_population <- data.frame(
  Region = c("East", "West", "North", "South"),
  Population = c(5000000, 3000000, 4000000, 3500000)
)

df <- df %>%
    left_join(regional_population, by="Region")

print(head(df))


# Create a bar plot of total sales by Category.
print(ggplot(df_group, aes(x = Category, y = total_sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Category", x = "Category", y = "Total Sales") +
  theme_minimal())

# Create a scatter plot of Sales vs. Profit with a trend line.
print(ggplot(df, aes(x = Sales, y = Profit)) +
  geom_point(color = "blue", alpha = 0.6) +    # scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # linear trend line with confidence interval
  labs(title = "Sales vs. Profit with Trend Line",
       x = "Sales",
       y = "Profit") +
  theme_minimal())


# Create a histogram of Profit_Margin.
print(ggplot(df, aes(x = Profit_Margin)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Profit Margin",
       x = "Profit Margin",
       y = "Frequency") +
  theme_minimal())