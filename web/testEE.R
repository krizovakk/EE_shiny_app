
require(tidyverse)
require(readxl)
require(openxlsx)


# colnames(profil) <- c("datum", "profilMWh", "mesic", "rok")
# print(head(profil))

# pro test

delOd <- as.Date("2026-04-01")
delDo <- as.Date("2028-05-01")

profil <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/EE/EE_shiny_app/web/data/EE_input_profil.xlsx") 


# ---------------------------------------------------------------------------- PLOT :: input

g1 <- ggplot(profil) +
  geom_line(aes(datum_cas, profil), color = "dodgerblue4") +
  labs(x = "datum",
       y = "diagram spotřeby [MWh]") + #title = "Profil spotřeby klienta"
  scale_x_datetime(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
  scale_y_continuous()+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90))

g1
  
# ---------------------------------------------------------------------------- INITIAL :: single variables


# ---------------------------------------------------------------------------- INPUT :: forward EE - WIP
# mail from dispecingee <dispecingee@spp.sk>, file: HPFC_ddmmyy.xlsx


# test
a <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/EE/EE_shiny_app/data/HPFC.xlsx")

fwd <- a %>% 
  rename("datum" = 1) %>% 
  mutate(date = as.POSIXct(datum, format = "%d.%m.%Y %H:%M", tz = "Europe/Prague"),
         year = year(date),
         y = str_extract(year, "..$"),
         q = case_when(month(date) <=3 ~ paste0("Q1", y),
                       month(date) <=6 ~ paste0("Q2", y),
                       month(date) <=9 ~ paste0("Q3", y),
                       month(date) <=12 ~ paste0("Q4", y)),
         month = month(date),  
         day = day(date),  
         hour = hour(date),  
         min = minute(date)) %>% 
  group_by(year, month, day) %>% 
    mutate(period = seq_along(min)) %>% # 96
  ungroup() %>% 
  select(datum, date, year, q, month, day, hour, min, period, SK, CZ, DE) 

fwd_hour <- fwd %>% 
  select(year, q, month, day, hour, CZ) %>% 
  group_by(year, q, month, day, hour) %>% 
  summarise(price = mean(CZ)) %>% 
  ungroup()

cal <- fwd_hour %>% select(year, price) %>% group_by(year) %>% 
  summarise(cal_price = mean(price))

y <- fwd_hour %>% select(year, q, price) %>% group_by(year, q) %>% 
  summarise(q_price = mean(price)) %>% 
  ungroup()

mon <- fwd_hour %>% select(year, month, price) %>% group_by(year, month) %>% 
  summarise(m_price = mean(price)) %>% 
  ungroup()


# ---------------------------------------------------------------------------- INPUT :: FWD EUR - OK

a <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/ZP/ZP_shiny_app/data/input_fwdKrivka.xlsx", sheet = "Rentry")
a$mesic <- as.Date(a$mesic, origin = "1899-12-30")
fwd <- a %>% 
  rename("PFC" = NCG, "FX" = 'FX rate') %>% 
  filter(mesic > tms_now) %>%   # hodnoty fwd krivky od nasledujiciho mesice
  select(mesic, FX) %>% 
  mutate(swap = (FX-cnb_spot)*1000,
         eur_prepoc = aktual_spot+swap/1000)

view(fwd)


# ---------------------------------------------------------------------------- INPUT :: OTC - OK


b <- read.csv("X:/OTC/CSV/Power.csv", header = F, sep = ",") # test
otc <- b %>%
  select("season" = 1, "price" = 2) %>% 
  slice(7:24) %>% 
  mutate(
    price = as.numeric(str_replace(price, ",", ".")),
    year = case_when(
      str_detect(season, "2025|25") ~ 2025,
      str_detect(season, "2026|26") ~ 2026,
      str_detect(season, "2027|27") ~ 2027,
      str_detect(season, "2028|28") ~ 2028,
      str_detect(season, "2029|29") ~ 2029,
      TRUE ~ NA
    ),
    quater = case_when(
      str_detect(season, "Q1") ~ "Q1",
      str_detect(season, "Q2") ~ "Q2",
      str_detect(season, "Q3") ~ "Q3",
      str_detect(season, "Q4") ~ "Q4",
      TRUE ~ NA
    ),
    month = case_when(
      str_detect(season, "Jan-") ~ 1,
      str_detect(season, "Feb-") ~ 2,
      str_detect(season, "Mar-") ~ 3,
      str_detect(season, "Apr-") ~ 4,
      str_detect(season, "May-") ~ 5,
      str_detect(season, "Jun-") ~ 6,
      str_detect(season, "Jul-") ~ 7,
      str_detect(season, "Aug-") ~ 8,
      str_detect(season, "Sep-") ~ 9,
      str_detect(season, "Oct-") ~ 10,
      str_detect(season, "Nov-") ~ 11,
      str_detect(season, "Dec-") ~ 12,
      TRUE ~ NA
    ),
    cal = as.character(ifelse(str_detect(season, "^\\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA))
  ) %>% 
  # mutate(product = paste0(cal, month, quater, year), collapse = NULL, na.rm = FALSE))
    unite(product, c("cal", "month", "quater", "year"),
      sep = "/", na.rm = T, remove = FALSE) 

# test

# otc[8,2] <- NA
# otc[8,2] <- 31.270



# ---------------------------------------------------------------------------- CREATE :: frame - OK


delOd <- as.POSIXct("2026-06-01 00:00", tz = "Europe/Prague") # jen pro test
delDo <- as.POSIXct("2028-12-31 23:45", tz = "Europe/Prague") # jen pro test

frameOd <- as.POSIXct("2026-01-01 00:00", tz = "Europe/Prague")
frameDo <- as.POSIXct("2029-12-31 00:00", tz = "Europe/Prague")
framePer <- seq(from = frameOd, to = frameDo, by = "15 min")

delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "15 min") %>% head(-1)) # head = maze posledni element (1.1.2027)

frame <- data.frame(framePer) %>% 
  mutate(
    year = year(framePer),
    month = month(framePer),
    quater = case_when(
      month <= 3 ~ "Q1",
      month <= 6 ~ "Q2",
      month <= 9 ~ "Q3",
      month <= 12 ~ "Q4"),
    day = day(framePer),
    hour = hour(framePer),
    min = minute(framePer),
    now = ifelse(year == year(tms_now) & month == month(tms_now), 1, 0), # jaky mesic je ted
    dodavka = ifelse(framePer %in% seq(from = delOd, to = delDo, by = "month"), 1, 0)
  ) %>% 
  left_join(diagram, by = c("framePer" = "datum")) %>%
  left_join(fwd, by = c("framePer" = "mesic")) %>% 
  mutate(dodavka = ifelse(framePer %in% delPer, 1, 0)) %>%  
  select(framePer, year, quater, month, now, dodavka, diagram1, FX)


# ---------------------------------------------------------------------------- CREATE :: data_vstup - TODO


low_spot <- 24.30
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 
bsd <- 1.2

join <- frame %>%
  
  # cena komodity
  
  group_by(year) %>%
  mutate(yRatio = PFC/mean(PFC)) %>% # kdyz neni cely rok, hodi pres prumer NA
  ungroup() %>% group_by(year, quater) %>%
  
  mutate(celyQ = if_else(all(!is.na(PFC)) & is.na(yRatio), "ANO", "NE"),
         # avg = mean(PFC),
         qRatio = ifelse(celyQ == "ANO", PFC/mean(PFC), NA),
         PFCratio = coalesce(yRatio, qRatio)) %>% ungroup() %>%
  
  # select(-yRatio, -qRatio) %>%
  left_join(otc %>%
              select(year, month, "monPrice" = price), by = c("year", "month")) %>%
  left_join(otc %>%
              select(year, quater, "qPrice" = price), by = c("year", "quater")) %>%
  left_join(otc %>%
              filter(!is.na(cal)) %>%
              select(year, "calPrice" = price), by = c("year")) %>%
  mutate(otcPrice = case_when(celyQ == "NE" & is.na(PFCratio) ~ monPrice,
                              celyQ == "ANO" ~ qPrice,
                              TRUE ~ calPrice),
         PFCprepoc = ifelse(!is.na(PFCratio), otcPrice*PFCratio, otcPrice),
         
         # kurz EUR
         
         swapPoint = (FX-low_spot)*1000,
         FXrecalc0 = aktual_spot+swapPoint/1000, # +surcharge (ale v excelu je 0)
         FXrecalc = FXrecalc0+surcharge,
         
         cenaEUR = profilMWh*PFCprepoc,
         vazenaCena = profilMWh*FXrecalc,
         # product = paste(month, quater, year))
         product = case_when(year(tms_now) != year ~ paste0("Cal", str_remove(year, "^..")),
                             TRUE ~ paste0(month, "/", year)))

data_vstup <- join %>%
  select(year, month, dodavka, profilMWh, product, otcPrice, PFCprepoc, cenaEUR, FXrecalc, vazenaCena) %>%
  filter(dodavka == 1) # final df to match table on sheet Kalkulace

conditionPROF <- any(is.na(data_vstup$profilMWh))
if (conditionPROF) {
  stop('Neuplny profil')
}


# test

conditionOTC <- any(is.na(data_vstup$otcPrice))
if (conditionOTC) {
  # stop('Chybi OTC cena')
  prod <- data_vstup$product[is.na(data_vstup$otcPrice)]
  stop(paste('Chybi OTC cena pro', prod))
}


# ---------------------------------------------------------------------------- CALCULATE :: fix_cena - TODO


# vypocty pod tabulkou

suma_profil <- round(sum(data_vstup$profilMWh, na.rm = TRUE), 0)
acq <- data_vstup %>% 
  group_by(year) %>% summarise(rocni_odber = round(sum(profilMWh, na.rm = T), 0))
suma_cenaEUR <- sum(data_vstup$cenaEUR, na.rm = TRUE)
suma_vazenaCena <- sum(data_vstup$vazenaCena, na.rm = TRUE)
mean_PFC <- mean(data_vstup$PFCprepoc, na.rm = TRUE)
nakup <- suma_cenaEUR/suma_profil
prirazka_nakup <- 0.000 #  ???
kurz <- suma_vazenaCena/suma_profil
prodej_eur <- nakup+prirazka_nakup
prodej_czk <- prodej_eur*kurz

# # vypocty nad tabulkou

naklad_profil <- round(nakup-mean_PFC, 2)
fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025
fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05


# ---------------------------------------------------------------------------- CREATE :: marze - TODO


marzeMin <- 1.2
marzeDop <- 6
txt_marzeMin <- sprintf("%.2f", marzeMin) # text, zobrazuje cislo s presne 2 decimals
txt_marzeDop <- sprintf("%.2f", marzeDop)


# ---------------------------------------------------------------------------- RETURN :: fix_cena - TODO

conditionNPF <- naklad_profil<0
if (conditionNPF) {
  print('Zaporny naklad na profil')
}

fix_cena2 <- data.frame(
  Parametr = c(
    "Období dodávky",
    "ACQ [MWh]",
    "Předávací cena pro obchod [€]",
    "Předávací cena pro obchod [CZK]",
    "Suma profil",
    "Prumer EUR",
    "Suma ceny EUR",
    "Suma vazene ceny",
    "Nakup",
    "Prodej",
    "Kurz",
    "Naklad na profil"
   
  ),
  
  Hodnota = c(
    paste0(delOd, " až ", delDo),
    paste(
      paste(acq$year, acq$rocni_odber, sep = ": "),
      collapse = ", "),
    round(fin_cenaEUR, 2),
    round(fin_cenaCZK, 2),
    round(suma_profil, 0),
    round(mean_PFC, 3),
    round(suma_cenaEUR, 0),
    round(suma_vazenaCena, 0),
    round(nakup, 3),
    round(prodej_czk, 3),
    round(kurz, 2),
    naklad_profil
  )
)

view(fix_cena2)


# ---------------------------------------------------------------------------- RETURN :: plot


ggplot(profil, aes(datum, profilMWh)) +
  # geom_line(linewidth = 2, color = "gold") +
  geom_col(fill = "gold") +
  labs(x = "měsíc dodávky",
       y = "profil spotřeby [MWh]",
       title = "Profil spotřeby klienta") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(breaks = seq(0, 700, by = 50))+
  theme_light() +
  theme(axis.text.x = element_text(angle = 90))
