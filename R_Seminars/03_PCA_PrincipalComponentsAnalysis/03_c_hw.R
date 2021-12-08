rm(list = ls())

library(DescTools)
library(ellipse)
library(car)

load("Countries.RData")

# 1
x <- cor(Countries[1:8], use = "pairwise.complete.obs")
pairs(Countries, panel = panel.smooth)
scatterplotMatrix(
    Countries[1:8],
    smooth = F,
    diagonal = "histogram",
    col = c(2, 1, 4)
)

PlotCorr(x)
plotcorr(x)
PlotFaces(x)

# 3
# Center and Scale T means all calculations are based on correlation not covariance
# That means the data are centered and scaled before PCA (PCA works with stand. data)
p <- prcomp(x = Countries, center = T, scale. = T)
print(p)
# 4
p$sdev # square root of eigenvalues
p$sdev^2 # eigenvalues
eigen(x)$values # another way to get eigenvalues for data

# 5
# Scree plot
plot(p, type="l")
print(summary(p))

# 6
# Finding eigenvectors
p$rotation
# or
eigen(x)$vectors # This idk why returns opposite +- signs 
# or just
p
# Standardized eigenvectors - Loadings (porovnanie vah jednotlivych premennych)
v <- matrix(rep(p$sdev, 8), 8, 8, byrow=TRUE)
p$rotation / v

# 7
# As our data are standardized we can calculate the correlation as 
# eigenvectors * square root of eigenvalues
p$rotation * v

# 8 
NewCoordinate <- predict(p, newdata = Countries)
NewCoordinate[, 1:3]

# 9
githubinstall::gh_install_packages("vqv/ggbiplot")
library(ggbiplot)

stan_p <- p$rotation * p$sdev
biplot(p,xlim=c(-1,1))
ggbiplot(p, scale=1)

# Domaca Uloha
# PCA pre subor lide.sta
