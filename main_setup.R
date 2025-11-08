# Project Setup
#aktivieren der Packages für Daten einlesen und bereinigen

# 👇 Vorbereitung: Falls Pakete noch nicht installiert sind, werden sie automatisch installiert
packages <- c("readr", "dplyr", "tidyr", "skimr","ggplot2")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

datensatz <- read.csv("Data/fraud_dataset_fhj25.csv")

head((datensatz))

skim(datensatz)


##check ob gleiche Datensätze vorhanden sind
any(duplicated(datensatz))


##Spalten unbenennen

colnames(datensatz) <- c("date", "age", "profession","region", "account_balance", "num_debit","num_credit","num_pos","num_mov_conto","incoming_payments","outgoing_payments","product_number","num_fraud")



##2627 zeilen wurden gelöscht, schaupsi wann gehma bier trinken?
datensatz <- datensatz %>%
  filter(!(num_debit == 0 & 
             num_credit == 0 & 
             num_pos == 0 & 
             num_fraud == 0))


##Fokussierung u. sortieren der incoming payment spalte
##Große Transaktionen sind wichtig

df_sorted <- datensatz %>%
  arrange(desc(`incoming_payments`))

summary(df_sorted)

# Datum umwandeln
df_eda <- df_sorted %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),  # richtiges Datum
    weekday = wday(date, label = TRUE, abbr = TRUE),  # Montag–Sonntag
    month = month(date, label = TRUE, abbr = TRUE)    # Monatsname
  )

# Anteil Fraud vs. Nicht-Fraud
table(df_eda$num_fraud)
prop.table(table(df_eda$num_fraud))

# 📅 Fraud-Anteil nach Wochentag
ggplot(df_eda, aes(x = weekday, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud") +
  labs(title = "Fraud-Anteil nach Wochentag", x = "Wochentag", y = "Anteil (%)") +
  theme_minimal()

# 🗓️ Fraud-Anteil nach Monat
ggplot(df_eda, aes(x = month, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud") +
  labs(title = "Fraud-Anteil nach Monat", x = "Monat", y = "Anteil (%)") +
  theme_minimal()

# 💰 Fraud vs. Kontostand
ggplot(df_eda, aes(x = account_balance, fill = as.factor(num_fraud))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud") +
  labs(title = "Kontostand und Fraud", x = "Kontostand", y = "Dichte") +
  theme_minimal()

# 📆 Fraud vs. Kontobewegungen (Aktivität)
ggplot(df_eda, aes(x = num_mov_conto, fill = as.factor(num_fraud))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud") +
  labs(title = "Kontobewegungen und Fraud", x = "Kontobewegungen", y = "Anzahl") +
  theme_minimal()




# rel. fraud nach alter (col 2/13) XX
ggplot(datensatz, aes(x = age, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Alter",
       x = "Alter",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# rel. fraud nach profession (col 3/13)
ggplot(datensatz, aes(x = profession, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Beruf",
       x = "Beruf",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# rel. fraud nach region (col 4/13)
ggplot(datensatz, aes(x = region, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Region",
       x = "Region",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()















