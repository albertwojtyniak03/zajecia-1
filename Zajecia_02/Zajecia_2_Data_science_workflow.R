# 0. KONFIGURACJA SRODOWISKA I PAKIETY ----
############################################################

# Sprawdzenie aktualnego katalogu roboczego
getwd()

# Ustawienie katalogu roboczego (wykonaj recznie w menu RStudio):
# Session ??? Set Working Directory ??? Choose Directory

# Instalacja pakietów (odkomentuj, jesli instalujesz pierwszy raz, potem zakomentuj)
#install.packages("readxl")
#install.packages("dplyr")
#nstall.packages("ggplot2")
#install.packages("writexl")
#install.packages("scales")

# Wczytanie bibliotek
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
library(scales)

############################################################
# 1. IMPORT DANYCH ----
############################################################

# Import danych z plików CSV
kraje_1 <- read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 <- read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# Podglad zaimportowanych tabel
head(kraje_1)
head(kraje_2)

############################################################
# 2. PRZYGOTOWANIE I CZYSZCZENIE DANYCH ----
############################################################

# Usuwanie zbednych kolumn 'X'
kraje_1$X <- NULL
kraje_2$X <- NULL

# Zmiana nazw kolumn w drugiej tabeli na polskie
colnames(kraje_2) <- c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")

# Zmiana typu zmiennej Region na kategorialna (factor)
kraje_2$Region <- as.factor(kraje_2$Region)

# Czyszczenie nazw regionów - zamiana znaku '&' na 'and'
kraje_2$Region <- gsub("&", "and", kraje_2$Region)
kraje_2$Region <- as.factor(kraje_2$Region) # Ponowne ustawienie typu factor

# Kontrola braków danych
colSums(is.na(kraje_1))
colSums(is.na(kraje_2))

############################################################
# 3. LACZENIE (SCALANIE) DANYCH ----
############################################################

# Scalanie ramek danych po kodzie kraju
kraje <- merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")

# Usuniecie powielonej kolumny z nazwa po scaleniu
kraje$Nazwa <- NULL

# Sprawdzenie struktury polaczonego zbioru
str(kraje)

############################################################
# 4. ANALIZA DANYCH (dplyr) ----
############################################################

# Tworzenie nowych zmiennych: populacja w mln i PKB per capita
kraje <- kraje %>%
  mutate(Populacja_mln = Populacja / 1e6,
         PKB_per_capita = PKB / Populacja)

# Statystyki regionalne: liczba krajów, sredni internet i urbanizacja
regiony_stat <- kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))

# Wyswietlenie statystyk w konsoli
print(regiony_stat)

############################################################
# 5. WIZUALIZACJA DANYCH (ggplot2) ----
############################################################

# 1. Wykres punktowy: urbanizacja a PKB per capita
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(title = "Urbanizacja a PKB per capita", x = "Urbanizacja (%)", y = "PKB per capita")

# 2. Wykres punktowy z podzialem na regiony
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = comma) +
  theme_minimal()

# 3. Wykres punktowy: gospodarka a populacja
ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() + scale_y_log10() +
  theme_minimal()

# 4. Wykres slupkowy: liczba krajów w regionach
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Wykres slupkowy poziomy: TOP 15 najbogatszych krajów
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() + coord_flip() + scale_y_continuous(labels = comma) +
  theme_minimal()

# 6. Boxplot: dostep do internetu wg regionów
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) + coord_flip() + theme_minimal()

# 7. Boxplot: przyrost populacji wg regionów
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() + coord_flip() + theme_minimal()

# WAZNE - EKSPORT WYKRESÓW:
# Zgodnie z instrukcja, kazdy wykres zapisz recznie:
# Zakladka Plots -> Export -> Save as image (.PNG)

############################################################
# 6. EKSPORT WYNIKÓW ----
############################################################

# Zapisanie tabeli do pliku CSV
write.csv(kraje, "kraje_analiza.csv")

# Zapisanie tabeli do pliku Excel
write_xlsx(kraje, "kraje_wynik.xlsx")