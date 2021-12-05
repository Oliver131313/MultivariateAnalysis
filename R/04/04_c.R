# clear workspace
rm(list = ls())

library(DescTools)
library(ellipse)
library(car)
library(stringr)

load('Satisfaction.RData')
# 1
x <- cor(Satisfaction, use = "pairwise.complete.obs")
pairs(Satisfaction, panel = panel.smooth)
scatterplotMatrix(
    Satisfaction,
    smooth = F,
    diagonal = "histogram",
    col = c(2, 1, 4)
)
PlotCorr(x)
plotcorr(x)

# 3 - Determine number of factors
p <- prcomp(x = x, scale. = TRUE, center = TRUE)

p.var.expl <- p$sdev^2 / sum(p$sdev^2) *100

plot(p.var.expl, type="l")
print("#2 Looks like k = 3 is the optimal number of factors.")
print(str_glue('#2 k = 3 explains {k.var}% of variance.',
               k.var = as.character(round(sum(
                   p.var.expl[1:3]
               )))))
print(str_glue('#2 k = 2 explains {k.var}% of variance.',
               k.var = as.character(round(sum(
                   p.var.expl[1:2]
               )))))
print('#2 We will use k=2 factors.')

# 4 - Factor Analysis from psych package
library(psych)

# Varimax rotation, Maximum Likelihood method for extraction
f1 <-
    fa(
        r = x,
        nfactors = 2,
        rotate = "varimax",
        fm = "ml",
        scores = "regression",
        residuals = TRUE
    )

# The same but for 3 factors
f2 <-
    fa(
        r = x,
        nfactors = 3,
        rotate = "varimax",
        fm = "ml",
        scores = "regression",
        residuals = TRUE
    )

# Eigenvectors (celkova variabilita)
f1$e.values

# 5 Jednotlive premenne v novom lin. systeme
f1$loadings

# 6
plot(f1)
plot(f2)

# 7 - Interpretacie pre komunality AtWork1
print(f1)
print("At work comm.: 57% variability je vysvetlenych k=2 faktormi. Zvysok je unique pre tuto premennu")

# 8
f1$residual
print("Vypada to, ze mimo diagonaly nepozorujeme vela hodnot rozdielnych od nuly! To naznacuje, ze model replikuje povodnu kov. maticu dobre.")
print("NIE JE potrebne pridavat dalsi faktor.")

# 9 - Koeficienty faktorovych skore
f1$weights

# 10

