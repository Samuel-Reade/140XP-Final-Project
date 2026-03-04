library(dplyr)

superstore <- read.csv("superstore.csv")

order_profit <- superstore %>%
  group_by(Order.ID, State) %>%
  summarise(
    Total_Profit = sum(Profit, na.rm = TRUE),
    Total_Discount = sum(Discount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Discount_Binary = ifelse(Total_Discount > 0, 1, 0)
  )

# Compare orders with vs without discounts
comparison <- order_profit %>%
  group_by(Discount_Binary) %>%
  summarise(
    Avg_Profit = mean(Total_Profit, na.rm = TRUE),
    Median_Profit = median(Total_Profit, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

print(comparison)

# Compare by State AND discount status
state_comparison <- order_profit %>%
  group_by(State, Discount_Binary) %>%
  summarise(
    Avg_Profit = mean(Total_Profit, na.rm = TRUE),
    Orders = n(),
    .groups = "drop"
  )


order_profit$State <- as.factor(order_profit$State)
order_profit$Discount_Binary <- as.factor(order_profit$Discount_Binary)

# 2-Way ANOVA with interaction
anova_2way <- aov(Total_Profit ~ Discount_Binary * State, 
                  data = order_profit)

summary(anova_2way)
