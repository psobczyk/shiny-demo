

library(dplyr)
source("helpers.R")


# Loading
teryt <- read.csv2("data/TERC_Urzedowy_2022-02-10.csv", encoding = "CP-1250")
zgony <- read.csv2("data/ewp_dsh_zgony_po_szczep_20220127.csv")
ludnosc <- read.csv2("data/pl_lud_2021_00_04_k2.csv", skip = 5) %>%
  filter(!is.na(Ogółem)) %>%
  mutate(
    ROCZNIKI.URODZENIA = if_else(
      ROCZNIKI.URODZENIA == "1921 i wcześniej", "1921",
      ROCZNIKI.URODZENIA),
    wiek = 2021 - as.numeric(ROCZNIKI.URODZENIA)
  ) %>%
  filter(!is.na(wiek)) %>%
  select(wiek, Kobiety, Mężczyźni, Ogółem)

# Preprocessing
woj <- teryt %>%
  filter(is.na(POW)) %>%
  mutate(NAZWA = capitalize(NAZWA)) %>%
  select(teryt_woj = WOJ, woj = NAZWA)

pow <- teryt %>%
  filter(!is.na(POW) & is.na(GMI)) %>%
  mutate(
    NAZWA = capitalize(NAZWA),
    POW = if_else(POW < 10, paste0("0", POW), paste(POW)),
    teryt_pow = as.integer(paste0(WOJ, POW))
  ) %>%
  select(teryt_pow, pow = NAZWA)

ludnosc_wiek <- ludnosc %>%
  group_by(wiek = (cut(wiek, c(12, 17, 30, 40, 50, 60, 70, 75, 100),
                       include.lowest = T))) %>%
  summarise(sum(Ogółem)) %>%
  ungroup()

liczba_szczepien <- data_frame(
  wiek = c("[12,17]", "(17,30]", "(30,40]", "(40,50]", "(50,60]",
           "(60,70]", "(70,75]", "(75,100]"),
  zaszczepieni = 1e6 * c(2.8, 6.1, 7.2, 8.4, 7.6, 10.2, 4.2, 5)
)


zgony <- zgony %>%
  inner_join(woj, by = "teryt_woj") %>%
  inner_join(pow, by = "teryt_pow") %>%
  mutate(
    wiek = as.numeric(wiek),
    data_rap_zgonu = as.Date(data_rap_zgonu)
  ) %>%
  rename(zgony = `liczba_zaraportowanych_zgonow`)

zgony$dawka_ost <- factor(zgony$dawka_ost,
  levels = c("", "jedna_dawka", "pelna_dawka", "przypominajaca",
             "uzupe\xb3niaj\xb9ca"),
  labels = c("brak szczepienia", "jedna dawka", "pełna dawka",
             "przypominająca", "uzupełniająca")
)
zgony$plec <- factor(zgony$plec,
  levels = c("K", "M", "nieznana"),
  labels = c("Kobieta", "Mężczyzna", "Nieznana")
)

zgony$czy_wspolistniejace <- factor(zgony$czy_wspolistniejace,
                                    levels = c(0, 1),
                                    labels = c("brak chorób",
                                               "choroby współistniejące"))
