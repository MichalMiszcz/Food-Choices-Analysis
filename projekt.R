# Załadowanie danych zawierających informacje o wyborach jedzeniowych studentów.
# Badanie zostało przeprowadzone na 125 studentach.
data <- read.csv("D:/Michal/Nauka/Studia/Rok 3/Semestr 6/MUMa/Lab/Projekt/food_coded.csv")

# Wykresy przedstawiające dane

# Załadowanie biblioteki do tworzenia wykresów
library(ggplot2)

#---------------------------------------------------------------------------------
# Wykres słupkowy przedstawiający liczbę studentów w poszczególnych grupach typów
# kuchni przy jakiej się dorastało

# tworzę nową zmienną cuisine_factor przekształcając zmienną cuisine na factor
data$cuisine <- factor(data$cuisine)

ggplot(data, aes(x = cuisine)) +
  geom_bar() +
  labs(x = "Styl Kuchni", y = "Liczba studentów") +
  ggtitle("Wykres słupkowy - Styl kuchni") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_x_discrete(labels = c("1 - American", "2 - Mexican.Spanish", "3 - Korean/Asian",
                              "4 - Indian", "5 - American inspired international dishes",
                              "6 - Other")) +
  scale_fill_manual(values = c("1 - American" = "red", "2 - Mexican.Spanish" = "blue",
                               "3 - Korean/Asian" = "green", "4 - Indian" = "purple",
                               "5 - American inspired international dishes" = "orange",
                               "6 - Other" = "gray"),
                    guide = guide_legend(title = "Legenda"))

#---------------------------------------------------------------------------------
# wykres kołowy przedstawiający liczbę studentów w poszczególnych grupach preferowanych kuchni

# przekształcajam zmienną fav_cuisine_coded na factor
data$fav_cuisine_coded <- factor(data$fav_cuisine_coded, levels=c(0:8), 
                                       labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8"))

# tworzę wektor zawierającą etykiety dla poszczególnych kategorii, które zostaną umieszczone w legendzie
legend_labels <- c("None", "Italian/French/Greek", "Spanish/Mexican", "Arabic/Turkish",
                   "Asian/Chinese/Thai/Nepal", "American", "African", "Jamaican", "Indian")

# tworzę wektor zawierający kolory dla poszczególnych kategorii
legend_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#000EA3", 
                   "#FF7F00", "#FFFF33", "#A65628", "#9780BF", "#999999")

ggplot(data, aes(x = "", fill = fav_cuisine_coded)) +
  geom_bar(width = 1) +
  labs(fill = "Preferowana kuchnia") +
  ggtitle("Wykres kołowy - Preferowana kuchnia") +
  theme_void() +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = legend_colors, labels = legend_labels)



#---------------------------------------------------------------------------------
# Wykres punktowy przedstawiający aktualną dietę oraz odczucie zdrowia

# Tworzę wektor zawierający etykiety dla poszczególnych kategorii, które zostaną umieszczone na osi x
x_labels <- c("healthy/balanced/moderated", "unhealthy/cheap/too much/random", 
              "the same thing over and over", "unclear")


ggplot(data, aes(x = factor(diet_current_coded), y = healthy_feeling)) +
  geom_point(alpha = 0.1) +
  labs(x = "Aktualna dieta", y = "Odczucie zdrowia") +
  ggtitle("Wykres skrzypcowy - Porównanie idealnej diety z aktualną dietą") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = x_labels)



#---------------------------------------------------------------------------------
# Przewidywanie ulubionej kuchni
#---------------------------------------------------------------------------------
# Załadowanie bibliotek
# Biblioteka rpart to biblioteka uczenia maszynowego, która służy do budowania
# drzew klasyfikacji i regresji
library(rpart)
# Biblioteka dplyr służy do ułatwienia manipulacji danymi
library(dplyr)

# Załadowanie danych do zmiennej
data_corrected <- data

# Usunięcie niepotrzebnych kolumn, usunąłem wszystkie kolumny zawierające takie dane,
# na które ankietowani mogli odpowiedzieć w dowolny sposób (pytania otwarte)
data_corrected <- select(data_corrected, -comfort_food, -comfort_food_reasons,
                         -diet_current, -eating_changes, -father_profession,
                         -fav_cuisine, -healthy_meal, -meals_dinner_friend,
                         -mother_profession, -type_sports, -food_childhood, -ideal_diet)

# Usunięcie niepotrzebnych kolumn, usunąłem wszystkie kolumny zawierające takie dane,
# które były odpowiedziami na pytania o to jaką ilość kalorii ocenia odpowiadający
# w danym produkcie
data_corrected <- select(data_corrected, -calories_chicken, -calories_scone,
                         -tortilla_calories, -turkey_calories, -waffle_calories)

# Zmieniam typ zmiennych na numeric
data_corrected$weight <- as.numeric(data_corrected$weight)
data_corrected$GPA <- as.numeric(data_corrected$GPA)


# Podział danych na zbiór treningowy i testowy (np. 90% treningowy, 10% testowy)
set.seed(123)  # Ustawienie ziarna losowości dla powtarzalności wyników
train_indices <- sample(1:nrow(data_corrected), 0.9*nrow(data_corrected))  # Indeksy próbek treningowych
train_data <- data_corrected[train_indices, ]  # Zbiór treningowy
test_data <- data_corrected[-train_indices, ]  # Zbiór testowy

# Tworzenie modelu drzewa decyzyjnego
ctrl <- rpart.control(cp = 0.0001)
model <- rpart(fav_cuisine_coded ~ ., data = train_data, control = ctrl)

# Dokonywanie predykcji na zbiorze testowym
predictions <- predict(model, newdata = test_data, type = "class")

# Ocenianie wyników modelu
accuracy <- sum(predictions == test_data$fav_cuisine_coded) / nrow(test_data)
cat("Dokładność modelu:", accuracy)

# Połączenie wektorów w ramkę danych
result <- data.frame(predictions, test_data$fav_cuisine_coded)

# Ustawienie indeksów
rownames(result) <- 1:nrow(result)

# Wydrukowanie ramki danych przedstawiającej porównanie danych przewidzianych
# i danych testowych
print(result)

# Dodanie indeksów do ramki danych result
result$Index <- rownames(result)
result$Index <- as.numeric(result$Index)

# Stworzenie wykresu z indeksami na osi X i danymi na osi Y
ggplot(result, aes(x = Index)) +
  geom_point(aes(y = predictions), color = "blue", size = 3, shape = 3) +
  geom_point(aes(y = test_data.fav_cuisine_coded), color = "red", size = 3, shape = 4) +
  labs(x = "Indeks", y = "Wartość") +
  ggtitle("Porównanie predykcji i realnych danych") +
  scale_x_continuous(breaks = result$Index)

# Macierz pomyłek
library(caret)
confusionMatrix(result$predictions,result$test_data.fav_cuisine_coded)


#-------------------------------------------------------------------------------------------------------
# randomForest

library(randomForest)

# Załadowanie danych do zmiennej
data_corrected <- data

# Usunięcie niepotrzebnych kolumn, usunąłem wszystkie kolumny zawierające takie dane,
# na które ankietowani mogli odpowiedzieć w dowolny sposób (pytania otwarte)
data_corrected <- select(data_corrected, -comfort_food, -comfort_food_reasons,
                         -diet_current, -eating_changes, -father_profession,
                         -fav_cuisine, -healthy_meal, -meals_dinner_friend,
                         -mother_profession, -type_sports, -food_childhood, -ideal_diet)

# Usunięcie niepotrzebnych kolumn, usunąłem wszystkie kolumny zawierające takie dane,
# które były odpowiedziami na pytania o to jaką ilość kalorii ocenia odpowiadający
# w danym produkcie
data_corrected <- select(data_corrected, -calories_chicken, -calories_scone,
                         -tortilla_calories, -turkey_calories, -waffle_calories)

# Zmieniam typ zmiennych na numeric
data_corrected$weight <- as.numeric(data_corrected$weight)
data_corrected$GPA <- as.numeric(data_corrected$GPA)

# Usunięcie kolumn zawierających dużo pustych danych w celu wyczyszczenia danych
# do modelu randomForest
# Obliczenie pustych danych
nan_counts <- colSums(is.na(data_corrected))

# Posortowanie danych
sorted_nan_counts <- sort(nan_counts, decreasing = TRUE)

# Wypisanie, które kolumny zawierają ile pustych danych
for (column in names(sorted_nan_counts)) {
  cat("Column:", column, "\tNaN Count:", sorted_nan_counts[column], "\n")
}

# Usunięcie kolumny "calories_day"
data_corrected <- data_corrected[, -which(names(data_corrected) == "calories_day")]
# Usunięcie kolumny "comfort_food_reasons_coded"
data_corrected <- data_corrected[, -which(names(data_corrected) == "comfort_food_reasons_coded")]
# Usunięcie kolumny "cuisine"
data_corrected <- data_corrected[, -which(names(data_corrected) == "cuisine")]
# Usunięcie kolumny "exercise"
data_corrected <- data_corrected[, -which(names(data_corrected) == "exercise")]
# Usunięcie kolumny "employment"
data_corrected <- data_corrected[, -which(names(data_corrected) == "employment")]
# Usunięcie kolumny "GPA"
data_corrected <- data_corrected[, -which(names(data_corrected) == "GPA")]
# Usunięcie kolumny "weight"
data_corrected <- data_corrected[, -which(names(data_corrected) == "weight")]
# Usunięcie kolumny "cook"
data_corrected <- data_corrected[, -which(names(data_corrected) == "cook")]
# Usunięcie kolumny "mother_education"
data_corrected <- data_corrected[, -which(names(data_corrected) == "mother_education")]
# Usunięcie kolumny "drink"
data_corrected <- data_corrected[, -which(names(data_corrected) == "drink")]
# Usunięcie kolumny "sports"
data_corrected <- data_corrected[, -which(names(data_corrected) == "sports")]

# Usunięcie wierszów z pustymi danymi
data_corrected <- na.omit(data_corrected)

# Podział danych na zbiór treningowy i testowy
set.seed(123)
train_indices <- sample(1:nrow(data_corrected), 0.9 * nrow(data_corrected))
train_data <- data_corrected[train_indices, ]
test_data <- data_corrected[-train_indices, ]


# Tworzenie modelu lasu losowego
model <- randomForest(fav_cuisine_coded ~ ., data = train_data, ntree = 1000)

# Dokonywanie predykcji na zbiorze testowym
predictions <- predict(model, newdata = test_data)

# Ocenianie wyników modelu
accuracy <- sum(predictions == test_data$fav_cuisine_coded) / nrow(test_data)
cat("Dokładność modelu:", accuracy)

print(predictions)

# Połączenie wektorów w ramkę danych
result <- data.frame(predictions, test_data$fav_cuisine_coded)

# Ustawienie indeksów
rownames(result) <- 1:nrow(result)

# Wydrukowanie ramki danych przedstawiającej porównanie danych przewidzianych
# i danych testowych
print(result)

# Dodanie indeksów do ramki danych result
result$Index <- rownames(result)
result$Index <- as.numeric(result$Index)

# Stworzenie wykresu z indeksami na osi X i danymi na osi Y
ggplot(result, aes(x = Index)) +
  geom_point(aes(y = predictions), color = "blue", size = 3, shape = 3) +
  geom_point(aes(y = test_data.fav_cuisine_coded), color = "red", size = 3, shape = 4) +
  labs(x = "Indeks", y = "Wartość") +
  ggtitle("Porównanie predykcji i realnych danych") +
  scale_x_continuous(breaks = result$Index)






