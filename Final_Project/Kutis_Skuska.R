#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
#################################
### Vyskusat este Canonical Discriminant Analysis!!!!
## Upratat notebook a pekne popisat jednotlive kroky
# Hodit cele do markdownu


# clear workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(MASS)
library(psych)
library(ggbiplot)


# Import data - Vsetky mozne atributy hracov vo futbalovom simulatore FIFA 22
# Zdroj:
# www.kaggle.com/bryanb/fifa-player-stats-database/version/27?select=FIFA22_official_data.csv
raw_data <- read_delim("FIFA22_official_data.csv")


unique(raw_data$`Best Position`)
raw_data %>%
    filter(`Best Position` == "RM") %>%
    arrange(desc(Overall))


# Data preprocessing
data <- raw_data %>%
    # Mame 15 pozicnych kategorii - Tie chceme zredukovat, pretoze z praxe vieme, ze
    #  niektore su pomerne lahko zamenitelne a netreba medzi nimi rozlisovat
    # V principe su deleny do kategorii: Stredny utocnik, Stredny zaloznik,
    #  Pravy zaloznik / kridelnik, Lavy zaloznik / kridelnik, Pravy obranca,
    #  Lavy obranca, Stredny obranca a Brankar
    dplyr::mutate(
        BestPos = factor(
            case_when(
                `Best Position` %in% c("CF", "ST") ~ "CF/ST",
                `Best Position`  %in% c("CAM", "CM", "CDM") ~ "CM/CAM/CDM",
                `Best Position` %in% c("RW", "RM") ~ "RW/RM",
                `Best Position` %in% c("LW", "LM") ~ "LW/RM",
                `Best Position` %in% c("RWB", "RB") ~ "RWB/RB",
                `Best Position` %in% c("LWB", "LB") ~ "LWB/LB",
                `Best Position` %in% c("CB") ~ "CB",
                `Best Position` %in% c("GK") ~ "GK"
            )
        ),
        Height = as.double(str_replace(Height, 'cm', '')),
        Weight = as.double(str_replace(Weight, 'kg', '')),
        PrefFoot = as.factor(`Preferred Foot`),
        WeekFoot = `Weak Foot`,
        SkillMoves = `Skill Moves`,
        WorkRate = as.factor(`Work Rate`),
        BodyType = factor(`Body Type`)
    ) %>%
    # Vyberieme iba relevantne stlpce
    dplyr::select(
        Name,
        BestPos,
        Age,
        PrefFoot,
        WeekFoot,
        SkillMoves,
        WorkRate,
        BodyType,
        Height,
        Weight,
        Crossing,
        Finishing,
        HeadingAccuracy,
        ShortPassing,
        Volleys,
        Dribbling,
        Curve,
        FKAccuracy,
        LongPassing,
        BallControl,
        Acceleration,
        SprintSpeed,
        Agility,
        Reactions,
        Stamina,
        Interceptions,
        Balance,
        Strength,
        Positioning,
        ShotPower,
        LongShots,
        Vision,
        StandingTackle,
        Jumping,
        Aggression,
        Penalties,
        SlidingTackle
    ) %>%
    # Pre vacsiu jednoduchost sa vsak zameriame iba na dva posty (budeme mat teda
    #  binarnu klasifikaciu)
    dplyr::filter(BestPos != "GK") %>% na.omit()

fifa <- data %>%
    # Cely problem klasifikacie vsak zredukujeme iba na binarnu klasifikaciu.
    #  Konkretne sa budeme snazit na zaklade hracskych atributov odhadnut ci sa jedna o
    #  utocnika alebo o stredneho zaloznika.
    filter(BestPos %in% c("CM/CAM/CDM", "CF/ST")) %>%
    # Ponechame si vsak iba numericke stlpce
    select_if(!(map(., class) %in% c("factor", "character"))) %>%
    # Vypustenie 37 riadkov chybajucich hodnot. Vzhladom ku velkosti datasetu je to v poriadku
    na.omit()


map(map(data, ~ is.na(.)), ~ sum(.)) # Check chybajucich hodnot


# PCA
fit <- prcomp(fifa, center = T, scale. = T)
sum_pca <- summary(fit) # print variance accounted for
sum_pca

cum_var_pca <-
    as.vector(sort(sum_pca$importance[2, 1:10], decreasing = TRUE))
plot(cum_var_pca, type = "l") # scree plot


# Biplot skrz ggplot
ggbiplot(
    fit,
    scale = 1,
    circle = TRUE,
    var.scale = 1,
    var.axes = TRUE,
    alpha = 0
)

# detach(package:plyr)

# Nove hodnoty po linearnej transformacii pre k=4
pca.scores <- predict(fit, newdata = fifa)
pca.scores[1:10, 1:4]


# FA
# Ako pocet faktorov vyberame k = 3, nakolko aj data by mali priblizne spadat do
#  takehoto poctu kategorii. Volba je teda zalozena na znalostiach a nie na PCA!
fa <-
    fa(
        r = fifa,
        nfactors = 3,
        rotate = "varimax",
        fm = "ml",
        scores = "regression",
        residuals = T
    )

fa.diagram(fa.results = fa)

plot(fa)

fa$residual

# 1. Faktor je asociovany najma s pracou s loptou smerom dopredu
fa$loadings[, 1] %>% as.data.frame() %>% mutate(. = abs(.))  %>% arrange(desc(.))
# 2. Faktor je asociovany najma s defenzivou, prihravanim a prehladom na ihrisku
fa$loadings[, 2] %>% as.data.frame() %>% mutate(. = abs(.))  %>% arrange(desc(.))
# 3. Faktor je asociovany najma s jeho fyzickymi predispoziciami
fa$loadings[, 3] %>% as.data.frame() %>% mutate(. = abs(.))  %>% arrange(desc(.))


# Jednotlive pozorovania reprezentovane novym systemom
fifa.fa <- as_tibble(fa$scores)
fifa.fa <-
    cbind(data %>% filter(BestPos %in% c("CM/CAM/CDM", "CF/ST"))
          %>% na.omit() %>% dplyr::select(Name, BestPos),
          fifa.fa)

names(fifa.fa) <- c("Name", "Pos", "Off", "Def", "Phys")


fifa.fa %>%
    dplyr::group_by(Pos) %>%
    dplyr::summarize(Off = mean(Off),
                     Def = mean(Def),
                     Phys = mean(Phys)) %>%
    arrange(desc(Off)) %>%
    pivot_longer(-Pos) %>%
    ggplot(aes(x = name, y = value, fill = Pos)) +
    geom_bar(stat = "identity", position = "dodge")


# LDA - Aplikovane na nove premenne z FA
# Priemery podla groping variables su pomerne odlisne, medzi niektorymi skupinami viac
#  a medzi inymi menej.
library(caret)
library(ROCR)

fifa.fa %>%
    dplyr::group_by(Pos) %>%
    dplyr::summarize(Off = mean(Off),
                     Def = mean(Def),
                     Phys = mean(Phys)) %>%
    arrange(desc(Off))

# Data vyzeraju aproximativne normalne
fifa.fa %>%
    dplyr::select(-Name) %>%
    pivot_longer(-Pos, names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(x = Value, y = ..density.., color = Variable)) +
    geom_histogram(position = "identity",
                   fill = "azure2",
                   alpha = 1) +
    geom_density(alpha = 1, ) +
    facet_grid(Variable ~ .) +
    scale_color_brewer(palette = "Spectral")

# Vyhodenie premennej meno
fifa.fa <-
    fifa.fa %>% dplyr::select(-Name) %>% mutate(Pos = factor(Pos, levels =
                                                                 c("CF/ST", "CM/CAM/CDM")))



######## Tato cast moze ostat zakomentovana - Prilis dlho trva urobit 1000 LDAs na
########  37 stlpcoch a 10,000 riadkoch. Pointa tejto casti kodu je porovnat ci je lepsie
########  urobit LDA na FA pretransormovanych alebo povodnych stlpcoch. T-test udava, ze
########  AUC pre LDA na povodnych stlpcoch je statisticky vyznamne rozne od LDA
########  vykonanej na FA transformovanych stlpcoch.
# # seed
# set.seed(123)
# AUC_FA <- rep(0, 1000)
# for (i in 1:1000) {
#     # Train-Test split
#     train.index.fa <-
#         fifa.fa$Pos %>% createDataPartition(p = 0.75, list = FALSE)
#     train.data.fa <- fifa.fa[train.index.fa,]
#     test.data.fa <- fifa.fa[-train.index.fa,]
#
#     # Fitni model
#     model.fa <- lda(Pos ~ ., data = train.data.fa,)
#
#     # Predikcie
#     predictions.fa <- model.fa %>% predict(test.data.fa)
#
#     ## Evaluation
#     # Mozeme vidiet, ze model je pomerne dobry v tom ako predikuje hodnoty!
#     predictions.posteriors.fa <-
#         as.data.frame(predictions.fa$posterior[, 2])
#
#     pred.fa <-
#         prediction(predictions.posteriors.fa, test.data.fa$Pos)
#     roc.perform.fa <-
#         performance(pred.fa, measure = "tpr", x.measure = "fpr")
#     auc.train.fa <- performance(pred.fa, measure = "auc")
#     auc.train.fa.val <- auc.train.fa@y.values
#
#     AUC_FA[i] <- as.double(auc.train.fa.val)
# }
#
# plot(roc.perform.fa)
# abline(a = 0, b = 1)
# text(x = .25, y = .65 , paste("AUC = ", round(mean(as.double(AUC_FA)), 3), sep = ""))
#
#
# # LDA na raw datach
# fifa.raw <-
#     cbind(
#         data %>% filter(BestPos %in% c("CM/CAM/CDM", "CF/ST")) %>% mutate(BestPos = factor(BestPos, levels = c(
#             "CM/CAM/CDM", "CF/ST"
#         )))
#         %>% na.omit() %>% dplyr::select(BestPos),
#         fifa
#     )
#
# head(fifa.raw)
#
# AUC_RAW <- rep(0, 1000)
# for (i in 1:1000) {
#     # Preprocessing dat
#     preproces.param.raw <-
#         fifa.raw %>% preProcess(method = c("center", "scale"))
#     fifa.raw.trans <- preproces.param.raw %>% predict(fifa.raw)
#
#     # Train-test split
#     train.index.raw <-
#         fifa.raw$BestPos %>% createDataPartition(p = 0.75, list = FALSE)
#     train.data.raw <- fifa.raw.trans[train.index.raw, ]
#     test.data.raw <- fifa.raw.trans[-train.index.raw, ]
#
#     # Fit the model
#     model.raw <- lda(BestPos ~ ., data = fifa.raw.trans)
#
#     # Predikcie
#     predictions.raw <- model.raw %>% predict(test.data.raw)
#
#     ## Evaluation
#     # Mozeme vidiet, ze model je pomerne dobry v tom ako predikuje hodnoty!
#     predictions.posteriors.raw <-
#         as.data.frame(predictions.raw$posterior[, 1])
#
#     pred.raw <-
#         prediction(predictions.posteriors.raw, test.data.raw$BestPos)
#     roc.perform.raw <-
#         performance(pred.raw, measure = "tpr", x.measure = "fpr")
#     auc.train.raw <- performance(pred.raw, measure = "auc")
#     auc.train.raw.val <- auc.train.raw@y.values
#     AUC_RAW[i] <- as.double(auc.train.raw.val)
#
# }

# # Porovnanie modelov
# t.test(as.double(AUC_FA), AUC_RAW)
# print("Vysledok testu ukazuje, ze LDA model postaveny na nepretranformovanych datach je lepsi!")


# LDA na raw datach (Zakomentovat ak chcete vykonat t-test)
fifa.raw <-
    cbind(
        data %>%
            filter(BestPos %in% c("CM/CAM/CDM", "CF/ST")) %>%
            mutate(BestPos = factor(BestPos, levels = c(
                "CM/CAM/CDM", "CF/ST"
            )))
        %>% na.omit() %>% dplyr::select(BestPos),
        fifa
    )

# Preprocessing dat
preproces.param.raw <-
    fifa.raw %>% preProcess(method = c("center", "scale"))
fifa.raw.trans <- preproces.param.raw %>% predict(fifa.raw)

# Train-test split
train.index.raw <-
    fifa.raw$BestPos %>% createDataPartition(p = 0.75, list = FALSE)
train.data.raw <- fifa.raw.trans[train.index.raw,]
test.data.raw <- fifa.raw.trans[-train.index.raw,]

# Fit the model
model.raw <- lda(BestPos ~ ., data = train.data.raw)

# Predikcie
predictions.raw <- model.raw %>% predict(test.data.raw)

## Evaluation
# Mozeme vidiet, ze model je pomerne dobry v tom ako predikuje hodnoty!
predictions.posteriors.raw <-
    as.data.frame(predictions.raw$posterior[, 1])

pred.raw <-
    prediction(predictions.posteriors.raw, test.data.raw$BestPos)
roc.perform.raw <-
    performance(pred.raw, measure = "tpr", x.measure = "fpr")
auc.train.raw <- performance(pred.raw, measure = "auc")
auc.train.raw.val <- auc.train.raw@y.values
AUC_RAW <- as.double(auc.train.raw.val)



# ROC curve pre LDA na povodnych stlpcoch
plot(roc.perform.raw)
abline(a = 0, b = 1)
text(x = .25, y = .65 , paste("AUC = ", round(AUC_RAW, 3), sep = ""))


# Vizualizacia modelu na novych LD osiach
lda.raw.viz <-
    as.data.frame(cbind(
        as.character(test.data.raw$BestPos),
        predictions.raw$x,
        as.character(predictions.raw$class)
    ))

colnames(lda.raw.viz) <- c("Act_class", "LD1", "Pred_class")

lda.raw.viz <- lda.raw.viz %>%
    as_tibble() %>%
    mutate(Pred_OK = as.factor(
        case_when(Act_class == Pred_class ~ TRUE,
                  Act_class != Pred_class ~ FALSE)
    ))
lda.raw.viz


# Vizualizacia klasifikacie
library(ggthemes)
lda.raw.viz %>%
    ggplot(aes(x = LD1, y = rep(0, length(LD1)))) +
    geom_jitter(aes(color = Act_class)) +
    geom_jitter(
        data = lda.raw.viz %>%
            filter(Pred_OK == FALSE),
        pch = 21,
        size = 3,
        color = "red",
        fill = "red",
        alpha = 0.5
    ) +
    scale_color_manual(name= "Skutočné hodnoty",
                       values = c("CM/ST" = "darkgrey", "CM/CAM/CDM" = "blue"),
                       ) +
    theme_economist() + 
    labs(y = "Jittered 1D",
         title = "LDA - Predikovanie hráčskej pozície na základe atribútov\n
         vo futbalovom simulátore FIFA 22"
    ) + 
    theme(
        # Parametre osi
        axis.ticks.y = element_blank(),
        axis.line = element_line(color="black"),
        axis.text = element_blank(),
        # Grid prec
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(size=15)
    ) +
    # Zvacsi velkosti v legende
    guides(
        colour = guide_legend(override.aes = list(size=7))
        )
    


# Confusion Matrix
confusion.mx <- confusionMatrix(
    data = as.factor(lda.raw.viz$Pred_class),
    reference = as.factor(lda.raw.viz$Act_class),
    dnn = c("Prediction", "Reference"),
)


lda.raw.viz
