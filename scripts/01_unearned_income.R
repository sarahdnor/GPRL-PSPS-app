# loading packages
library(readr) 
library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DT)

# loading data
load(file = here("data/PSPS.rda"))

PSPS <- PSPS |>
  mutate(
  gender = if_else(r_gender == 1, "Female", "Male")
) 

# histogram of foreign remit
PSPS |>
  ggplot(aes(x = log10(fremit_total))) +
  geom_histogram()

# histogram of domestic remit
PSPS |>
  ggplot(aes(x = log10(dremit_total))) +
  geom_histogram()

PSPS |>
  ggplot(aes(x = log10(fremit_total))) +
  geom_histogram(aes(fill = gender))

PSPS |>
  ggplot(aes(x = log10(dremit_total))) +
  geom_histogram(aes(fill = gender))

PSPS |>
  ggplot(aes(x = log10(dremit_total), y = r_age)) +
  geom_point()

PSPS |>
  ggplot(aes(x = log10(fremit_total), y = r_age)) +
  geom_point()
