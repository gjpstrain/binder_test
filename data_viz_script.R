library(tidyverse)
library(Hmisc)
library(ggridges)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_jitter(width = .12, alpha = .55, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  theme(text = element_text(size = 14)) +
  labs(title = "City Fuel Economy by Vehicle Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (MPG)")

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  stat_summary(fun.data = mean_cl_boot, color = "black", size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust = .5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Vehicle Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (MPG)")

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, colour =
             manufacturer)) +
  stat_summary(fun.data = mean_cl_boot, size = .8) +
  geom_jitter(alpha = .3) + 
  theme_minimal() + 
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Vehicle Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (MPG)") +
  guides(colour = FALSE) +
  coord_flip()

mpg %>%
  filter(class !="suv") %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = cty, colour = class)) +
  geom_jitter(width = .2) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Vehicle Manufacturer",
      x = "Manufacturer",
      y = "City Fuel Economy (MPG)") +
  guides(colour = FALSE) +
  facet_wrap(~class)

mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  labs(x = "City Fuel Economy (MPG)",
       y = "Engine Displacement (litres)",
       colour = "Vehicle Class")
  
mpg %>%
  ggplot(aes(x = displ)) +
  geom_histogram(binwidth = .5, fill = "grey") +
  labs(title = "Histogram of Engine Displacement",
       x = "Engine Displacement (litres)",
       y = "Count")
  
mpg %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) +
  geom_density_ridges(height = .5, aes(fill = class)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  guides(fill = FALSE) +
  labs(x = "Engine Displacement (litres",
       y = NULL)

library(NHANES)

ncol(NHANES)
nrow(NHANES)
head(NHANES)

NHANES %>%
  select(ID) %>%
  n_distinct()

NHANES_tidied <- NHANES %>%
  distinct(ID, .keep_all = TRUE)

NHANES_tidied %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 100, na.rm = TRUE)

NHANES_tidied %>%
  group_by(Education) %>%
  summarise(median = median(BMI, na.rm = TRUE))

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  ggplot(aes(x = Education, y = BMI, colour = Education)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) +
  labs(title = "Examining the effect of education level on BMI", 
       x = "Education Level", 
       y = "BMI")

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI) & !is.na(Diabetes)) %>%
  ggplot(aes(x = Education:Diabetes, y = BMI, colour = Education)) +
  geom_violin() +
  geom_jitter(alpha = .5) +
  geom_boxplot(alpha = .5) +
  guides(colour = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(title = "Examining the effect of education level and diabetes status on BMI",
       x = "Education Level x Diabetes",
       y = "BMI")

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(binwidth = .5) +
  guides(fill = FALSE) + 
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI", 
       y = "Number of cases") + 
  facet_wrap(~ Education)

NHANES_tidied %>% 
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>% 
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..)) +
  guides(fill = FALSE) + 
  labs( title = "Examining the effect of education level on BMI", 
        x = "BMI", 
        y = "Density") + 
  facet_wrap(~ Education)
  
