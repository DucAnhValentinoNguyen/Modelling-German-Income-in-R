# Modelling-German-Income-in-R
< br / >
Analysis with GLM and GAM modeling, with and without interactions, then compare with AIC and BIC
< br / >
Data set is beink, already cleaned (withou missing values), dim: 3000 x 8, independent variable is "beink" (gross income in the last month), the other 7 are: "groesse" (height in cm), "alter"(age in years), "dauer" (length of service in years), "verh"(if married (1) or not(0)), "geschl" (sex: male(1), female(0)),  "deutsch"(if nationality is german (1), not (0)), "abitur" (if graduated german high school(1) , no(0)).
< br / >
Goal is to understand the correlation between each covariates and the independent variable "beink", so to study which factor play a role in effecting the income of one person, and in which way.
< br / >
The Gamma additive model with interaction of covariate "alter" with other covariates seems to be the most suitable w.r.t AIC and BIC being the lowest of all. According to the model then owning the nationality of germany and being graduate do not have a statistical significance in the model. Being married and being taller seem to have a (statistically significant) positive linear effect with the income (c.p.). Being older and having more years of working seem to have a non-linear effect with the income (c.p.) (see graphics).

![image](https://github.com/user-attachments/assets/8b695a0e-fff3-4971-9aeb-be47d4197dc4)
