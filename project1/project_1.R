library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(zoo)
library(xml2)


xml_to_csv <- function(xml_file, csv_file) {
  # Wczytanie pliku XML
  doc <- read_xml(xml_file)
  # Definicja przestrzeni nazw Excela w XML
  ns <- c(ss = "urn:schemas-microsoft-com:office:spreadsheet")
  # Znalezienie wszystkich wierszy (Row) przy użyciu przestrzeni nazw
  rows <- xml_find_all(doc, ".//ss:Row", ns)
  # Lista, do której będziemy zapisywać dane każdego wiersza
  rows_list <- list()
  # Iteracja przez wszystkie wiersze
  for(i in seq_along(rows)) {
    # Dla danego wiersza pobieramy wszystkie komórki (Data)
    cells <- xml_find_all(rows[[i]], ".//ss:Data", ns)
    # Pobieramy tekst z komórek; jeśli komórka jest pusta, zapisujemy pusty ciąg
    row_data <- if(length(cells) > 0) xml_text(cells) else ""
    rows_list[[i]] <- row_data
  }
  # Ustalamy maksymalną długość wiersza, aby wyrównać długości
  max_length <- max(sapply(rows_list, length))
  # Uzupełniamy brakujące elementy pustymi ciągami, aby wszystkie wiersze miały tę samą długość
  rows_list <- lapply(rows_list, function(x) {
    length(x) <- max_length
    x[is.na(x)] <- ""
    x
  })
  # Konwersja listy do macierzy
  mat <- do.call(rbind, rows_list)
  # Zapis macierzy do pliku CSV, używając przecinków jako separatorów, bez zapisywania numerów wierszy
  write.csv(mat, file = csv_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Plik CSV został zapisany jako:", csv_file, "\n")
}
przetworz_dane <- function(df) {
  # Wyszukaj, gdzie kończy się spis treści (pierwszy wiersz, w którym wartość się powtarza)
  unikalne <- unique(df[[1]])
  spis_tresci <- unikalne[1:(which(duplicated(df[[1]]))[1] - 1)]
  # Utwórz ramkę danych "spis_tresci" z trzema kolumnami
  spis_tresci <- data.frame(
    Nazwa = spis_tresci, 
    Opis = rep(NA, length(spis_tresci)), 
    Dodatkowe = rep(NA, length(spis_tresci)), 
    stringsAsFactors = FALSE
  )
  # Znajdź wszystkie wiersze, w których znajduje się "Powrót do spisu treści"
  indeksy_powrotu <- which(df[[1]] == "Powrót do spisu treści")
  # Początkowy indeks bloku danych
  start <- length(spis_tresci$Nazwa) + 2
  for(i in seq_along(indeksy_powrotu)) {
    # Wiersz zawierający nazwę tabeli – pozostaje bez zmian
    nazwa_tabeli <- df[start, 1]
    # Ustalamy początkowo kandydat na ostatni wiersz danych
    end_candidate <- indeksy_powrotu[i] - 2
    # Cofamy się w górę – jeśli w kolumnie 2 w danym wierszu jest pusta wartość,
    # przyjmujemy wiersz wyżej jako faktyczny koniec danych
    while(end_candidate > (start + 1) && 
          (is.na(df[end_candidate, 2]) || df[end_candidate, 2] == "")) {
      end_candidate <- end_candidate - 1
    }
    end <- end_candidate
    # Pobieramy tabelę danych: od wiersza bezpośrednio po nazwie (start + 1) do 'end'
    tabela <- df[(start + 1):end, , drop = FALSE]
    # --- Usuwanie pustych kolumn ---
    # Sprawdzamy każdą kolumnę – jeśli poza pierwszym wierszem (nagłówkiem)
    # wszystkie komórki są puste (NA lub ""), kolumna zostanie usunięta.
    if(nrow(tabela) >= 2) {
      tabela <- tabela[, !sapply(tabela, function(col) {
        all(is.na(col[-1]) | col[-1] == "")
      }), drop = FALSE]
    } else {
      # Jeśli tabela ma tylko jeden wiersz (tylko nagłówek), usuwamy wszystkie kolumny.
      tabela <- tabela[, FALSE, drop = FALSE]
    }
    # --- Koniec usuwania pustych kolumn ---
    # Przypisujemy całą ramkę danych do zmiennej o nazwie odpowiadającej tabeli
    assign(nazwa_tabeli, tabela, envir = .GlobalEnv)
    # Wiersz opisowy – trafia do spisu treści (kolumna Opis) pobrany z kolumny 1
    opis <- df[end + 1, 1]
    # Dodatkowe informacje: pobieramy z kolumny 1 dla kolejnych wierszy, jeśli występują
    if((end + 2) <= (indeksy_powrotu[i] - 1)) {
      dodatkowe <- df[(end + 2):(indeksy_powrotu[i] - 1), 1]
      # Jeśli jest więcej niż jeden wiersz, łączymy je separatorem ";;"
      if(length(dodatkowe) > 1) {
        dodatkowe <- paste(dodatkowe, collapse = ";;")
      } else {
        dodatkowe <- dodatkowe[1]
      }
    } else {
      dodatkowe <- NA
    }
    # Aktualizujemy spis treści – wpisujemy opis i dodatkowe informacje do pierwszego wolnego wiersza
    wiersz_do_opisu <- which(is.na(spis_tresci[["Opis"]]))[1]
    spis_tresci[wiersz_do_opisu, "Opis"] <- opis
    spis_tresci[wiersz_do_opisu, "Dodatkowe"] <- dodatkowe
    # Przejście do kolejnego bloku danych
    start <- indeksy_powrotu[i] + 1
  }
  # Zapisujemy spis treści jako osobną ramkę danych
  assign("spis_tresci", spis_tresci, envir = .GlobalEnv)
}
xml_to_csv("NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.xml", "NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.csv")
dod <- read.csv("NFZ_o_zdrowiu_-_Depresja_Dane_za_lata_2013-2023.csv", header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
przetworz_dane(dod)


data1 <- `Tabela 2.22: Mediana długości leczenia lekiem refundowanym z substancją czynną sertralinum (czas pomiędzy datą realizacji pierwszej recepty a datą zakończenia terapii) oraz odsetek osób, dla których długość leczenia wynosiła co najmniej 180 dni wg grup wiekowych`
colnames(data1) <- as.character(data1[1, ])
data1 <- data1[-1, ]
row.names(data1) <- NULL
data1 <- data1 %>% 
  select(`Grupa wiekowa`,`Liczba osób (tys.)`,`Mediana długości leczenia`) %>% 
  mutate(across(c("Liczba osób (tys.)","Mediana długości leczenia"), as.numeric))
colnames(data1) <- c("grupa_wiekowa","liczba_osob","mediana")
data1 <- data1 %>%
  bind_rows(data1 %>% 
            slice(c(7,8)) %>% 
            summarise(across(c(liczba_osob,mediana), mean))) %>% 
            slice(-c(7,8,9))
data1[7,1] <- "75 +"
s <- max(data1$liczba_osob)/max(data1$mediana)
data1 %>% 
  ggplot(aes(x = grupa_wiekowa)) +
  geom_col(aes(y = liczba_osob, fill = "Liczba osób"), size = 1, width = 0.5, alpha = 0.4) +
  geom_text(aes(y = liczba_osob, label = liczba_osob), position = position_dodge(width = 1), vjust = 1.75, size = 3, fontface = "bold") + 
  geom_point(aes(y = mediana*s, color = "Mediana"), size = 2.5) +
  geom_path(aes(y = mediana*s, color = "Mediana", group = 1), size = 1) +
  scale_y_continuous(name = "Liczba osób, którym ją przypisano (tys.)", sec.axis = sec_axis(~ ./s, name = "Mediana długości kuracji (dni)")) +
  scale_x_discrete(expand = c(0.001, 0.001)) +
  scale_fill_manual(name = " ", values = c("Liczba osób" = "#7a4cd4")) +
  scale_color_manual(name = " ", values = c("Mediana" = "red")) +
  theme_minimal() +
  labs(title = "Sertralina - dane dla leku (średnia z lat 2018-2023)", x = "Grupa wiekowa") +
  theme(legend.position = "top", axis.text = element_text(size = 10), plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.background = element_rect(fill = "#b4b4b4", color = NA))


data2 <- read.csv("dzieci_specjalisci.csv")
data2$liczba.specjalistow.zgłoszonych.do.umów <- na.approx(data2$liczba.specjalistow.zgłoszonych.do.umów)
data2 %>% 
  mutate(liczba.dzieci.na.jednego.specjaliste = ifelse(is.na(liczba.dzieci.na.jednego.specjaliste),liczba.dzieci/liczba.specjalistow.zgłoszonych.do.umów,liczba.dzieci.na.jednego.specjaliste)) %>% 
  ggplot(aes(x=Rok,y=liczba.dzieci.na.jednego.specjaliste)) +
  geom_point(color="red",size=4)+
  geom_line(color="red",size=1.2) +
  labs(title="Liczba dzieci przypadających na jednego specjaliste zgłoszonego do umów NFZ", y="Liczba dzieci")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     breaks = seq(0, 70, by = 10),
                     labels = seq(0, 70, by = 10),
                     limits = c(0, 70)) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = as.character(seq(2013, 2023, by = 1))) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "#b4b4b4", color = NA),
    legend.background = element_rect(fill = "#b4b4b4", color = NA),
    panel.background = element_rect(fill = "#b4b4b4", color = NA))


data3 <- `Tabela 2.10: Wartość refundacji świadczeń udzielonych z rozpoznaniem głównym depresji (F31.3–F31.6, F32, F33, F34.1, F34.8, F34.9, F38, F39 wg ICD-10, rozpoznanie główne) wg form opieki (2013–2023)`
colnames(data3) <- as.character(data3[1, ])
data3 <- data3[-1, ]
row.names(data3) <- NULL
colnames(data3) <- c("Rok","Łącznie","Szpitalne oddziały psychiatryczne","Poradnie psychologiczne, psychiatryczne i leczenia uzależnień","Oddziały dzienne")
data3[] <- lapply(data3, as.numeric)
typeof(data3$Rok)
data3 %>% 
  pivot_longer(cols = -Rok, names_to = "Kategoria", values_to = "Wartość") %>% 
  ggplot(aes(x = Rok, y = Wartość, fill = Kategoria, color = Kategoria)) +
  geom_area(alpha = 0.5, colour="black")  +
  labs(title = "Refundacja świadczeń (w mln zł) w latach 2013-2023",
       x = "Rok", y = "Wartość (w mln zł)") + 
  scale_fill_manual(values = c(`Łącznie` = "red", 
                               `Szpitalne oddziały psychiatryczne` = "#7a4cd4", 
                               `Poradnie psychologiczne, psychiatryczne i leczenia uzależnień` = "white", 
                               `Oddziały dzienne` = "#333333"),
                    labels = c(`Łącznie` = "Łącznie", 
                               `Szpitalne oddziały psychiatryczne` = "Szpitalne oddziały psychiatryczne", 
                               `Poradnie psychologiczne psychiatryczne i leczenia.uzależnień` = "Poradnie psychologiczne, psychiatryczne i leczenia uzależnień", 
                               `Oddziały.dzienne` = "Oddziały dzienne")) +
  scale_color_manual(values = c(`Łącznie` = "red", 
                                `Szpitalne oddziały psychiatryczne` = "#7a4cd4", 
                                `Poradnie psychologiczne psychiatryczne i leczenia uzależnień` = "white", 
                                `Oddziały dzienne` = "#333333"),
                     labels = c(`Łącznie` = "Łącznie", 
                                `Szpitalne oddziały psychiatryczne` = "Szpitalne oddziały psychiatryczne", 
                                `Poradnie psychologiczne psychiatryczne i leczenia uzależnień` = "Poradnie psychologiczne, psychiatryczne i leczenia uzależnień", 
                                `Oddziały dzienne` = "Oddziały dzienne")) + 
  theme_bw() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), breaks = seq(0, 900, by = 100)) + 
  scale_x_continuous(breaks = seq(2013, 2023, by = 1),
                     labels = as.character(seq(2013, 2023, by = 1)),
                     expand = c(0, 0)) +
  guides(
    fill  = guide_legend(nrow = 2, byrow = TRUE),
    color = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "#b4b4b4", color = NA),
    legend.background = element_rect(fill = "#b4b4b4", color = NA),
    panel.background = element_rect(fill = "#b4b4b4", color = NA))


data4 <- read.csv("liczba_osob_wiek_plec.csv")
data4 %>% 
  pivot_wider(names_from = Płeć, values_from = Liczba.pacjentów) %>% 
  ggplot() +
  geom_segment( aes(x=Grupa.wiekowa, xend=Grupa.wiekowa, y=Mężczyźni, yend=Kobiety), color="black",size=1) +
  geom_point( aes(x=Grupa.wiekowa, y=Mężczyźni, color="Mężczyźni"), size=4 ) +
  geom_point( aes(x=Grupa.wiekowa, y=Kobiety, color="Kobiety"), size=4 ) +
  scale_color_manual(name = " ", values=c("Mężczyźni"="#7a4cd4","Kobiety"="red"))+
  coord_flip()+
  xlab("Grupa wiekowa") +
  ylab("Liczba pacjentów") +
  ggtitle("Liczba osób, którym udzielono świadczenia z rozpoznaniem depresji (2023)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")+
  scale_y_continuous(breaks = seq(0, 150000, by = 10000)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "#b4b4b4", color = NA),
    legend.background = element_rect(fill = "#b4b4b4", color = NA),
    panel.background = element_rect(fill = "#b4b4b4", color = NA))

