library(tidyverse)
library(bayesm)
library(GGally)

options(digits=2)

# read data
data("margarine")
purchase_df <- margarine$choicePrice
demos_df <- margarine$demos
View(purchase_df %>% head(100), "Purchase")
View(demos_df %>% head(100), "Demog")

## Check columns type
str(purchase_df)
str(demos_df)

## Check basic statistics
### number of households
length(unique(purchase_df$hhid))
length(unique(demos_df$hhid))
### number of transactions
nrow(purchase_df)
table(purchase_df$choice)
### price stats
apply(purchase_df %>% select(-c(hhid, choice)), 2, mean)
apply(purchase_df %>% select(-c(hhid, choice)), 2, median)
apply(purchase_df %>% select(-c(hhid, choice)), 2, sd)
apply(purchase_df %>% select(-c(hhid, choice)), 2, min)
apply(purchase_df %>% select(-c(hhid, choice)), 2, max)

# # For drawing
df <- purchase_df %>% select(-c(hhid, choice))
mean_df <- gather(as.data.frame(t(apply(df, 2, mean))),
                  key = "brand", value = "mean") %>%
  mutate(brand = 1:10)
draw_df <- as.data.frame(table(purchase_df$choice)) %>%
  rename(brand = Var1, purchase_amount = Freq) %>%
  mutate(mean = mean_df$mean,
         median = med_df$median)

ggplot(data=draw_df, aes(x=brand)) +
  geom_bar(aes(y=purchase_amount), stat="identity", fill="blue") +
  geom_line(aes(y=mean*1000), color="red", group = 1) +
  ylab("purchase frequency") +
  scale_y_continuous(sec.axis = sec_axis(~./3000, name="Price")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 20)) +
  labs(title="Purchase Amount and Price Comparison")


### demographic
ggpairs(demos_df %>% select(-c(hhid, Fam_Size)))

