#write a new r script that derives the binary armed conflict variable that was used as the primary exposure in the paper
conflict <- read.csv(here("original", "conflictdata.csv"), header = TRUE)

conflict %>%
  # Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year
  group_by(ISO, year) |>
  summarise(totdeath = sum(best)) |>
  mutate(armconf1 = ifelse(totdeath < 25, 0, 1)) |>
  ungroup() |>
  mutate(year = year + 1) -> confdata