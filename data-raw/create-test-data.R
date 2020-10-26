

name_source1 <- c(
  "Beyonc\u00e9 Knowles",
  "Fr\u00e9d\u00e9ric Fran\u00e7ois Chopin",
  "Kendrick Lamar Duckworth",
  "Calvin Cordozar Broadus Jr.",
  "Céline Marie Claudette Dion",
  "Aubrey Drake Graham"
)

name_source2 <- c(
  "Beyonce Knowles-Carter",
  "CHOPIN, Fryderyk F.",
  "LAMAR, Kendrik",
  "Snoop Dogg",
  "DION, Céline",
  "Drake"
)

names_ex <- data.frame(
  name_source1,
  name_source2,
  stringsAsFactors = FALSE
)

date_seq <- seq.Date(as.Date("2020-06-01"), as.Date("2020-07-30"), by = 1)

set.seed(40296)

d <- readxl::read_xlsx("data-raw/noms.xlsx") %>%
  mutate(date_ipd = sample(date_seq, n(), replace = TRUE),
         date_icu = date_ipd + sample(0:4, n(), replace = TRUE))

d_adm <- d %>%
  select(name_ipd, date_ipd) %>%
  slice(sample(1:n()))

dat_ipd <- d %>%
  select(name_ipd, date_ipd) %>%
  slice(sample(1:n(), 10))

dat_icu <- d %>%
  select(name_icu, date_icu) %>%
  slice(sample(1:n(), 7))

# fuzzyjoin::fuzzy_join(
#   dat_ipd,
#   dat_icu,
#   by = c("name_ipd" = "name_icu"),
#   match_fun = nmatch::nmatch,
#   mode = "left",
#   dist_max = 2
# )

usethis::use_data(
  names_ex,
  dat_ipd,
  dat_icu,
  overwrite = TRUE
)
