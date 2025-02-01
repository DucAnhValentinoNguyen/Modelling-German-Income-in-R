beink <- read.table("beink.dat", header = T)
#install.packages("skimr")
skimr::skim(beink)
# install.packages("gam")
library(mgcv)


# Model 1: modeling with glm, without interaction
model_GLM <-
  glm(beink ~ ., family = Gamma(link = "log"), data = beink)
summary(model_GLM)


# Model 2:  modeling with glm with interaction
model_GLM_geschl <-
  glm(beink ~ . * geschl,
      family = Gamma(link = "log"),
      data = beink)
summary(model_GLM_geschl)









# Model 3:  modeling with GAM, non-linear relationship for all the continuos covariates
model_GAM <-
  mgcv::gam(
    formula = beink ~ s(groesse, bs = "ps") + s(alter, bs = "ps") + s(dauer, bs = "ps") +
      verh + deutsch + abitur + geschl,
    family = Gamma(link = "log"),
    data = beink
  )
#head(beink)
summary(model_GAM)

# graphical visualisations of the non-linear effects
par(mfrow = c(3, 1))
plot(
  x = model_GAM,
  shade = TRUE,
  select = 1,
  main = "Glatter Effekt der Körpergröße"
)
plot(
  x = model_GAM,
  shade = TRUE,
  select = 2,
  main = "Glatter Effekt des Alters"
)
plot(
  x = model_GAM,
  shade = TRUE,
  select = 3,
  main = "Glatter Effekt der Dauer"
)


# Interpretation:
# + Kovariable deutsch scheint nicht einen stat. signi. Effekt auf Einkommen zu haben
# + Die 3 Kovariablen: grosse, alter und dauer scheinen alle einen nicht lin. Effekt auf Einkommen zu haben
# + grosse: Einkomme steigt stabil je höher der Größe c.p.
# + alter: Einkomme steigt stabil je höher des Alter c.p. bis zum 40, dann senkt langsam ab
# + dauer: Einkomme steigt drastisch je höher der Dauer c.p. vom 0 bis 3, dann langsamer bis zum ca. 38, dann senkt ziemlich drastisch ab 38
# + die Unsicherheit über die allen Schätzungen sind an den Rändern (mit extremen Werten) größer










# Model 4:  we fit another gam with interaction between new covariate geschl(sex) and
# all the others "old" covariates
model_GAM_geschl <- gam(
  formula = beink ~ s(groesse, bs = "ps") +
    s(groesse, bs = "ps", by = geschl) +
    s(alter, bs = "ps") +
    s(alter, bs = "ps", by = geschl) +
    s(dauer, bs = "ps") +
    s(dauer, bs = "ps", by = geschl) +
    geschl +
    verh +
    deutsch +
    abitur +
    verh:geschl +
    deutsch:geschl +
    abitur:geschl,
  family = Gamma(link = "log"),
  data = beink
)

summary(model_GAM_geschl)

# visualising the effects
par(mfrow = c(3, 2))
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 1,
  cex.main = 0.9,
  main = "Glatter Effekt der Körpergröße(Männer)"
)
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 2,
  cex.main = 0.9,
  main = "Glatter variierender Effekt der Körpergröße(Frauen)"
)
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 3,
  cex.main = 0.9,
  main = "Glatter Effekt des Alters(Männer)"
)
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 4,
  cex.main = 0.9,
  main = "Glatter variierender Effekt des Alters(Frauen)"
)
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 5,
  cex.main = 0.9,
  main = "Glatter Effekt der Dauer(Männer)"
)
plot(
  x = model_GAM_geschl,
  shade = TRUE,
  select = 6,
  cex.main = 0.9,
  main = "Glatter variierender Effekt der Dauer(Frauen)"
)


# Interpretation:
# + für das Alter und die Dauer der Betriebszugehörigkeit sind glatte Effekte für beide Geschlecter weiterhin sinnvoll
# + für die Körpergröße wurde der glatte Effekt jeweils zu einem lin. Effekt penalisiert
# + hier aufzupassen ist das, dass sich auch für die Frauen ein pos. Effekt der Körpergröße ergibt (trotzt des Grafiks)
# + der Grund ist das, dass der pos. Effekt bei den Männern größer als der abnehmende variiende Effekt bei den Frauen






# now let's compare the 4 models wrt AIC, BIC
n <- nrow(beink)
models <- data.frame(matrix(0,4,4))
rownames(models) <- c("GLM", "GLM mit Interaktionen", "GAM", "GAM mit Interaktionen")
colnames(models) <- c("loglik", "df", "AIC", "BIC")

# Extract the log-Likelihood from the models
models$loglik[1] <-  logLik(model_GLM)
models$loglik[2] <-  logLik(model_GLM_geschl)
models$loglik[3] <-  logLik(model_GAM)
models$loglik[4] <-  logLik(model_GAM_geschl)

# Extract the number of parameters from each models
models$df[1] <- model_GLM$rank + 1
models$df[2] <- model_GLM_geschl$rank + 1
models$df[3] <- model_GAM$edf + 1
models$df[4] <- model_GAM_geschl$edf + 1

# Calculate the AIC and BIC
for(i in 1:4) {
  models$AIC[i] <-  2*models$df[i] -2*models$loglik[i]
  models$BIC[i] <-  2*log(models$df[i]) -2*models$loglik[i]
}

models

# Fazit: 
# + Modelle mit Interaktionen haben bessere Modellgüte als die ohne bzgl. AIC und BIC
# + Modell GAM mit Interaktionen weist die beste Modellgüte aus den 4 bzgl. AIC und BIC auf