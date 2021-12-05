# clear workspace
rm(list = ls())

library(DescTools)
library(ellipse)
library(car)
library(stringr)
library(CCA)

load("DistrictsCR.RData")

# 1 - Remove Praha because it's an outlier
Distr <- DistrictsCR[-1, ]
x <- cor(Distr, use = "pairwise.complete.obs")
pairs(Distr, panel = panel.smooth)
scatterplotMatrix(Distr,
                  smooth = F,
                  diagonal = "histogram",
                  col = c(2, 1, 4))
PlotCorr(x)
plotcorr(x)

# 3 - Vytvorenie dvoch setov premennych pre Ekon. a Demog indikatory
U <- Distr[, 1:7]
V <- Distr[, 8:length(Distr)]
p <- 7
q <- 8

# 4 - CCA (from package)
Cresults <- cc(U, V)
Crho <- Cresults$cor

# 5 - Reduncancy
Xstruct <- Cresults$scores$corr.X.xscores #R(X_i,U_j) in a p?p matrix
Xstruct2 <- Xstruct ^ 2 # Variance of X_i explained by each U_j
Ystruct <- Cresults$scores$corr.Y.yscores
Ystruct2 <- Ystruct ^ 2

XExplained <- vector("numeric", p)
YExplained <- vector("numeric", p)
for (i in 1:p) {
    XExplained[i] <- sum(Xstruct2[, i]) # Total Variance of each U_i
    YExplained[i] <- sum(Ystruct2[, i]) # Total variance of each V_i
}
cat("Relative Explained Variance of the Left Set by particular Us:",
    XExplained / p,
    '\n')
cat("Relative Explained Variance of the Right Set by particular  Vs:",
    YExplained / q,
    '\n')
cat("Total Explayned for the Left Set by all Us:",
    sum(XExplained / p) * 100,
    "%",
    '\n')
cat("Total Explayned for the Right Set by all Vs:",
    sum(YExplained / q) * 100,
    "%",
    '\n')
# X-s explained by can. variable V
cat("Total Redundancy for the Left Set:",
    sum(XExplained / p * Crho ^ 2) * 100,
    "%",
    '\n')
# Y-s explained by can. variable U
cat("Total Redundancy for the Right Set:",
    sum(YExplained / q * Crho ^ 2) * 100,
    "%",
    '\n')


# 6
Crho <- Cresults$cor

library(CCP)
options(scipen = 10) # Turning off the scientific display
wilks <- p.asym(
    rho = Crho,
    N = nrow(Distr),
    p = ncol(U),
    q = ncol(V),
    tstat = "Wilks"
)
options(scipen = 0) # Reset back to scientific

cat('Only first', sum(wilks$p.value <= 0.05), 'canonical pairs are significant.')
# 
Crho ^ 2
plot(Crho^2 ,type="b")
#
k = 3
Xfoo <- XExplained / p * Crho ^ 2
Yfoo <- YExplained / q * Crho ^ 2
cat("Redundancy for the Left Set for the first",
    k,
    "roots:",
    sum(Xfoo[1:k]) * 100,
    "%",
    '\n')
cat("Redundancy for the Right Set for the first" ,
    k,
    "roots:",
    sum(Yfoo[1:k]) * 100,
    "%",
    '\n')

# 7 - Factor Structure
Cresults$scores$corr.X.xscores # R(Xi, Uj)
Cresults$scores$corr.X.yscores # R(Xi, Vj)
Cresults$scores$corr.Y.yscores # R(Yi, Vj)
Cresults$scores$corr.Y.xscores # R(Yi, Uj)

# 8 - Canonical Scores
Cresults$scores$xscores
Cresults$scores$yscores

plt.var(Cresults, d1=1, d2=2, var.label=TRUE)
