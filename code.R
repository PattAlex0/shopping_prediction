### Libraries
library(dplyr)
library(ggcorrplot)
library(gridExtra)
library(tidyr)
library(ggplot2)
library(tibble)
library(tidymodels)

### Set seed to reduce randomness
set.seed(1353)

### Load data
shoppers <- read.csv("online_shoppers_intention.csv")

### Convert columns to factors
shoppers <- shoppers %>%
    mutate(Month = factor(Month,
                          levels = c("Feb", "Mar", "May", "June", 
                                     "Jul", "Aug", "Sep", "Oct",
                                     "Nov", "Dec"),
                          labels = c("February", "March", "May",
                                     "June", "July", "August",
                                     "September", "October", "November",
                                     "December")), 
           OperatingSystems = factor(OperatingSystems),
           Browser = factor(Browser),
           Region = factor(Region),
           TrafficType = factor(TrafficType),
           VisitorType = factor(VisitorType,
                                   levels = c("New_Visitor",
                                              "Returning_Visitor",
                                              "Other"),
                                   labels = c("New Visitor",
                                              "Returning Visitor",
                                              "Other"))) %>%
    as_tibble()

### Create split of the data
shoppers_split <- initial_split(shoppers)
shoppers_train <- training(shoppers_split)
shoppers_test <- testing(shoppers_split)

### Import functions
source("func/viz_dodge.R")
source("func/viz_fill.R")

### Histogram by Page
shoppers_train %>%
    pivot_longer(c("Administrative", "Informational", "ProductRelated"),
                 names_to = "Page",
                 values_to = "Number") %>%
    ggplot() +
    aes(Number, fill = Revenue) +
    geom_histogram() +
    facet_wrap(~Page, scales = "free") +
    labs(x = NULL, y = NULL)

### Histogram by Bounce and Exit rate
shoppers_train %>%
    pivot_longer(c("BounceRates", "ExitRates"),
                 names_to = "Measure",
                 values_to = "Rate") %>%
    ggplot() +
    aes(Rate, fill = Revenue) +
    geom_histogram() +
    facet_wrap(~Measure, scales = "free") +
    labs(x = NULL, y = NULL)

### Cumulative Graph by Page and Purchase
shoppers_train %>%
    pivot_longer(c("Administrative", "Informational", "ProductRelated"),
                 names_to = "Page",
                 values_to = "Number") %>%
    ggplot() +
    aes(Number, color = Revenue) +
    stat_ecdf(geom = "step") +
    facet_wrap(~Page, scales = "free_x")

### Boxplot of Log Page Duration
shoppers_train %>%
    pivot_longer(c("Administrative_Duration", "Informational_Duration", "ProductRelated_Duration"),
                 names_to = "Page",
                 values_to = "Duration") %>%
    ggplot() +
    aes(Revenue, log(Duration)) +
    geom_boxplot() +
    facet_wrap(~Page, scales = "free_y")

### Plot by month
viz_dodge(shoppers_train, Month, "Month")

viz_fill(shoppers_train, Month, "Month")

### Plot by Operating System
viz_dodge(shoppers_train, OperatingSystems, "Operating Systems")

viz_fill(shoppers_train, OperatingSystems, "Operating Systems")

### Plot by Browser
viz_dodge(shoppers_train, Browser, "Browser")

viz_fill(shoppers_train, Browser, "Browser")

### Plot by Region
viz_dodge(shoppers_train, Region, "Region")

viz_fill(shoppers_train, Region, "Region")

### Plot by Traffic Type
viz_dodge(shoppers_train, TrafficType, "TrafficType")

viz_fill(shoppers_train, TrafficType, "TrafficType")

### Plot by Visitor Type
viz_dodge(shoppers_train, VisitorType, "VisitorType")

viz_fill(shoppers_train, VisitorType, "Visitor Type")

### Plot by Weekend
viz_dodge(shoppers_train, Weekend, "Weekend")

viz_fill(shoppers_train, Weekend, "Weekend")

### Plot by Special Day
viz_dodge(shoppers_train, SpecialDay, "Special Day")

viz_fill(shoppers_train, SpecialDay, "Special Day")

### Correlation Plot
shoppers_train %>%
    select(Administrative:SpecialDay) %>%
    cor() %>%
    round(2) %>%
    ggcorrplot(hc.order = TRUE, type = "lower",
               outline.col = "white", lab = TRUE)
