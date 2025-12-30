# Project Setup
#aktivieren der Packages für Daten einlesen und bereinigen

# 👇 Vorbereitung: Falls Pakete noch nicht installiert sind, werden sie automatisch installiert
packages <- c("readr", "dplyr", "tidyr", "skimr", "ggplot2", "lubridate", "caret", "randomForest", "pROC", "ROSE")

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

# id einfügen

datensatz <- datensatz %>%
  mutate(
    id = row_number()                                   
  )

# Ersatz: keine Angabe für NA bei Beruf und Region
datensatz <- datensatz %>%
  mutate(
    profession = ifelse(is.na(profession) | profession == "", "Keine Angabe", profession),
    region     = ifelse(is.na(region)     | region == "",     "Keine Angabe", region)
  )

# Bei sonstigen NA: 0 Ersatz
datensatz[is.na(datensatz)] <- 0


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
ggplot(df_eda, aes(x = age, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Alter",
       x = "Alter",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach alter (col 2/13) XX
ggplot(subset(df_eda, num_fraud == 1), aes(x = age)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach Alter",
       x = "Alter",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach profession (col 3/13)
ggplot(df_eda, aes(x = profession, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Beruf",
       x = "Beruf",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach profession (col 3/13)
ggplot(subset(df_eda, num_fraud == 1), aes(x = profession)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach Beruf",
       x = "Beruf",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach region (col 4/13)
ggplot(df_eda, aes(x = region, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach Region",
       x = "Region",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach region (col 4/13)
ggplot(subset(df_eda, num_fraud == 1), aes(x = region)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach Region",
       x = "Region",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach Kontostand (col 5/13)
ggplot(df_eda, aes(x = account_balance, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Relativer Anteil Betrugsfälle nach Kontostand",
       subtitle = "Aufgeteilt in 60 gleich breite Intervalle",
       x = "Kontostand",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach Kontostand (col 5/13)
ggplot(subset(df_eda, num_fraud == 1), aes(x = account_balance)) +
  geom_histogram(bins = 60, fill = "red", color = "white") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Absolute Anzahl Betrugsfälle nach Kontostand",
       subtitle = "Nur Transaktionen mit Fraud = 1, aufgeteilt in 60 Intervalle",
       x = "Kontostand",
       y = "Anzahl Betrugsfälle") +
  theme_minimal()

# rel. fraud nach num_debit (col 6/13)
ggplot(df_eda, aes(x = num_debit, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach num_debit",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "num_debit",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach num_debit (col 6/13) ohne num_debit = 0
ggplot(subset(df_eda, num_fraud == 1 & num_debit != 0), aes(x = num_debit)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach num_debit",
       subtitle = "Werte mit num_debit = 0 ausgeschlossen",
       x = "num_debit",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach num_credit (col 7/13)
ggplot(df_eda, aes(x = num_credit, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach num_credit",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "num_credit",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach num_debit (col 7/13) ohne num_credit = 0
ggplot(subset(df_eda, num_fraud == 1 & num_credit != 0), aes(x = num_credit)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach num_credit",
       subtitle = "Werte mit num_credit = 0 ausgeschlossen",
       x = "num_credit",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach num_pos (col 8/13)
ggplot(df_eda, aes(x = num_pos, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach num_pos",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "num_pos",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach num_pos (col 8/13) ohne num_pos = 0
ggplot(subset(df_eda, num_fraud == 1 & num_pos != 0), aes(x = num_pos)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach num_pos",
       subtitle = "Werte mit num_pos = 0 ausgeschlossen",
       x = "num_pos",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach num_mov_conto (col 9/13)
ggplot(df_eda, aes(x = num_mov_conto, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach num_mov_conto",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "num_mov_conto",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach num_mov_conto (col 9/13) ohne num_mov_conto = 0
ggplot(subset(df_eda, num_fraud == 1 & num_mov_conto != 0), aes(x = num_mov_conto)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach num_mov_conto",
       subtitle = "Werte mit num_mov_conto = 0 ausgeschlossen",
       x = "num_mov_conto",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach incoming_payments (col 10/13)
ggplot(df_eda, aes(x = incoming_payments, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach incoming_payments",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "incoming_payments",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach incoming_payments (col 10/13) ohne incoming_payments = 0
ggplot(subset(df_eda, num_fraud == 1 & incoming_payments != 0), aes(x = incoming_payments)) +
  geom_histogram(bins = 60, fill = "red", color = "white") +
  labs(title = "Absolute Anzahl Betrugsfälle nach incoming_payments",
       subtitle = "Werte mit incoming_payments = 0 ausgeschlossen (60 Bins)",
       x = "incoming_payments",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach outgoing_payments (col 11/13)
ggplot(df_eda, aes(x = outgoing_payments, fill = as.factor(num_fraud))) +
  geom_histogram(position = "fill", bins = 60) +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach outgoing_payments",
       subtitle = "Aufgeteilt in 60 Intervalle",
       x = "outgoing_payments",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach outgoing_payments (col 11/13) ohne outgoing_payments = 0
ggplot(subset(df_eda, num_fraud == 1 & outgoing_payments != 0), aes(x = outgoing_payments)) +
  geom_histogram(bins = 60, fill = "red", color = "white") +
  labs(title = "Absolute Anzahl Betrugsfälle nach outgoing_payments",
       subtitle = "Werte mit outgoing_payments = 0 ausgeschlossen (60 Bins)",
       x = "outgoing_payments",
       y = "Anzahl") +
  theme_minimal()

# rel. fraud nach product_number (col 12/13)
ggplot(df_eda, aes(x = product_number, fill = as.factor(num_fraud))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "red"), name = "Fraud (0/1)") +
  labs(title = "Relativer Anteil Betrugsfälle nach product_number",
       x = "product_number",
       y = "Anteil (1.0 = 100%)") +
  theme_minimal()

# abs. fraud nach product_number (col 12/13)
ggplot(subset(df_eda, num_fraud == 1), aes(x = product_number)) +
  geom_bar(fill = "red") +
  labs(title = "Absolute Anzahl Betrugsfälle nach product_number",
       x = "product_number",
       y = "Anzahl") +
  theme_minimal()


# div Variablen/seed
set.seed(2712)
threshold_acc_bal <- quantile(df_eda$account_balance, 0.8)
train_size <- 0.8


# Segmente erstellen (große acc.bal., kleine acc.bal. und alle Daten)

df_high_full <- subset(df_eda, account_balance >= threshold_acc_bal)
df_low_full  <- subset(df_eda, account_balance < threshold_acc_bal)
df_all_full  <- df_eda

# Splitting von train und test Daten

df_high_train <- df_high_full %>% sample_frac(train_size)
df_high_test <- df_high_full %>% filter(!id %in% df_high_train$id)

df_low_train <- df_low_full %>% sample_frac(train_size)
df_low_test <- df_low_full %>% filter(!id %in% df_low_train$id)

df_all_train <- df_all_full %>% sample_frac(train_size)
df_all_test <- df_all_full %>% filter(!id %in% df_all_train$id)

# ==============================================================================
# MODELLIERUNG & EVALUIERUNG
# ==============================================================================

train_and_evaluate <- function(train_df, test_df, segment_name) {
  
  cat(paste0("\n##########################################################\n"))
  cat(paste0("   START ANALYSE FÜR SEGMENT: ", segment_name, "\n"))
  cat(paste0("##########################################################\n"))
  
  # A) Vorbereitung
  # Zielvariable muss Faktor sein
  train_df$num_fraud <- as.factor(train_df$num_fraud)
  test_df$num_fraud  <- as.factor(test_df$num_fraud)
  
  # date - Ausschluss
  train_data_model <- train_df %>% select(-date)
  test_data_model  <- test_df  %>% select(-date)
  
  # B) Balancing (Upsampling)
  set.seed(123) 
  
  # Upsampling
  up_train <- upSample(x = train_data_model[, names(train_data_model) != "num_fraud"],
                       y = train_data_model$num_fraud)
  colnames(up_train)[colnames(up_train) == "Class"] <- "num_fraud"
  
  cat("Verteilung nach Upsampling (Trainingsdaten):\n")
  print(table(up_train$num_fraud))
  
  # -------------------------------------------------------
  # MODELL 1: Logistische Regression
  # -------------------------------------------------------
  cat("\n-> Trainiere Logistische Regression...\n")
  
  # Training exkl. ID
  model_log <- glm(num_fraud ~ . - id, data = up_train, family = binomial)
  
  pred_log_prob <- predict(model_log, newdata = test_data_model, type = "response")
  pred_log_class <- as.factor(ifelse(pred_log_prob > 0.5, 1, 0))
  
  # -------------------------------------------------------
  # MODELL 2: Random Forest
  # -------------------------------------------------------
  cat("-> Trainiere Random Forest (Geduld...)\n")
  
  # Training exkl. ID
  model_rf <- randomForest(num_fraud ~ . - id, data = up_train, ntree = 100)
  
  pred_rf_class <- predict(model_rf, newdata = test_data_model)
  pred_rf_prob  <- predict(model_rf, newdata = test_data_model, type = "prob")[,2]
  
  # -------------------------------------------------------
  # EVALUIERUNG
  # -------------------------------------------------------
  cat("\n--- Ergebnisse: Logistische Regression ---\n")
  cm_log <- confusionMatrix(pred_log_class, test_data_model$num_fraud, positive = "1")
  print(cm_log$byClass[c("Sensitivity", "Specificity", "Precision", "F1")])
  roc_log <- roc(response = test_data_model$num_fraud, predictor = pred_log_prob, quiet = TRUE)
  cat(paste("AUC LogReg:", round(auc(roc_log), 4), "\n"))
  
  cat("\n--- Ergebnisse: Random Forest ---\n")
  cm_rf <- confusionMatrix(pred_rf_class, test_data_model$num_fraud, positive = "1")
  print(cm_rf$byClass[c("Sensitivity", "Specificity", "Precision", "F1")])
  roc_rf <- roc(test_data_model$num_fraud, pred_rf_prob, quiet = TRUE)
  cat(paste("AUC Random Forest:", round(auc(roc_rf), 4), "\n"))
  
  return(list(log = model_log, rf = model_rf))
}

# ==============================================================================
# AUSFÜHRUNG
# ==============================================================================

# 1. Segment: Alle Daten
res_all <- train_and_evaluate(df_all_train, df_all_test, "ALLE DATEN")

# 2. Segment: High Balance
if(nrow(df_high_train) > 0) {
  res_high <- train_and_evaluate(df_high_train, df_high_test, "HIGH BALANCE")
} else {
  cat("Zu wenig Daten für High Balance.\n")
}

# 3. Segment: Low Balance
if(nrow(df_low_train) > 0) {
  res_low <- train_and_evaluate(df_low_train, df_low_test, "LOW BALANCE")
} else {
  cat("Zu wenig Daten für Low Balance.\n")
}


