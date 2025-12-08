diox_raw <- read.csv("Data/Lk Ontario Dioxin data 2025-10.csv")

diox_fltr <- diox_raw %>%
  filter(Test.Code == "TEQWH4") %>%
  mutate(Contaminant = "TEQWH4")

write.csv(diox_fltr, file = "Data/LO_dioxin_data_2025-10.csv")
