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

###
source("func/viz_dodge.R")

### Histogram by Page
shoppers_train %>%
    pivot_longer(c("Administrative", "Informational", "ProductRelated"),
                 names_to = "Page",
                 values_to = "Number") %>%
    ggplot() +
    aes(Number) +
    geom_histogram()  +
    facet_wrap(Revenue~Page, scales = "free_x")

### Cumulative Graph by Page and Purchase
shoppers_train %>%
    pivot_longer(c("Administrative", "Informational", "ProductRelated"),
                 names_to = "Page",
                 values_to = "Number") %>%
    ggplot() +
    aes(Number, color = Revenue) +
    stat_ecdf(geom = "step") +
    facet_wrap(~Page, scales = "free_x")

### Plot by month
viz_dodge(shoppers_train, Month, "Month")

shoppers_train %>%
    count(Month, Revenue) %>%
    group_by(Month) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(Month, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Operating System
viz_dodge(shoppers_train, OperatingSystems, "Operating Systems")

shoppers_train %>%
    count(OperatingSystems, Revenue) %>%
    group_by(OperatingSystems) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(OperatingSystems, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Browser
viz_dodge(shoppers_train, Browser, "Browser")

shoppers_train %>%
    count(Browser, Revenue) %>%
    group_by(Browser) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(Browser, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Region
viz_dodge(shoppers_train, Region, "Region")

shoppers_train %>%
    count(Region, Revenue) %>%
    group_by(Region) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(Region, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Traffic Type
viz_dodge(shoppers_train, TrafficType, "TrafficType")

shoppers_train %>%
    count(TrafficType, Revenue) %>%
    group_by(TrafficType) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(TrafficType, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Visitor Type
viz_dodge(shoppers_train, VisitorType, "VisitorType")

shoppers_train %>%
    count(VisitorType, Revenue) %>%
    group_by(VisitorType) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(VisitorType, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by weekend
viz_dodge(shoppers_train, Weekend, "Weekend")

shoppers_train %>%
    count(Weekend, Revenue) %>%
    group_by(Weekend) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(Weekend, freq, fill = Revenue) +
    geom_col(position = "fill")

### Plot by Special Day
viz_dodge(shoppers_train, SpecialDay, "Special Day")

shoppers_train %>%
    count(SpecialDay, Revenue) %>%
    group_by(SpecialDay) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    aes(SpecialDay, freq, fill = Revenue) +
    geom_col(position = "fill")

### Correlation Plot
shoppers_train %>%
    select(Administrative:SpecialDay) %>%
    cor() %>%
    round(2) %>%
    ggcorrplot(hc.order = TRUE, type = "lower",
               outline.col = "white", lab = TRUE)
