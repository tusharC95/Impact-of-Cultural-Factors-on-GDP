library("ggplot2")
library("tidyverse")
library("dplyr")
library('readxl')
library("psych")
library('car')
library('modelsummary')
library('glmnet')
library('lme4')

load("WVS_TimeSeries_4_0.rdata")

WVS_TS <- data1[, c("S007", "S002VS", "COUNTRY_ALPHA", "Y010", "Y020")]

ChangeNAvalues10 <- function(VarName){
  VarName <- recode(VarName, "c(-9,-8,-7,-6, -5, -4, -3, -2, -1) = NA")
}

WVS_TS_NA <- as.data.frame(apply(WVS_TS[,1:5],2,ChangeNAvalues10))
WVS_TS_NA[, c("S007","S002VS","Y010", "Y020")] <- lapply(WVS_TS_NA[, c("S007","S002VS","Y010", "Y020")], as.numeric)

calculate_average <- function(data) {
  result <- data %>%
    group_by(S002VS, COUNTRY_ALPHA) %>%
    summarise(EmanVal_avg = mean(Y020, na.rm = T), SecVal_avg = mean(Y010, na.rm = T))
  
  return(result)
}

Cultr_values <- as.data.frame(calculate_average(WVS_TS_NA))

Cultr_values %>%
  filter(COUNTRY_ALPHA %in% c('USA', 'IND', 'CAN')) %>%
  ggplot(aes(x = S002VS, color = COUNTRY_ALPHA)) +
  geom_line(aes(y = SecVal_avg), linetype = "solid") +
  geom_line(aes(y = EmanVal_avg), linetype = "dashed") +
  geom_point(aes(y = SecVal_avg), shape = 16) +
  geom_point(aes(y = EmanVal_avg), shape = 16) +
  labs(title = "Line Plot with Points for USA, IND & CAN",
       x = "Waves",
       y = "Values",
       color = "COUNTRY_ALPHA",
       subtitle = "SecVal_avg and EmanVal_avg",
       caption = "Solid line and points represent SecVal_avg, dashed line and points represent EmanVal_avg") +
  scale_color_manual(values = c("USA" = "blue", "IND" = "green", "CAN" = "red")) +  
  theme_minimal() 

# boxplot(Cultr_values[, c("EmanVal_avg", "SecVal_avg")], 
#         main = "Cultural Values",
#         col = c("lightblue", "lightgreen"),
#         names = c("Secular Values", "Emancipative Values"))

# Cultr_values[Cultr_values$COUNTRY_ALPHA == "IND",]

# GDP Per Capita Growth
GDP_Rate_PC <- read.csv("GDP Per Capita PPP.csv", sep = ',', header = T, skip = 3)

long_GDPRate <- gather(GDP_Rate_PC, key = "Year", value = "GDP per capita", X1960:X2022)
long_GDPRate$Year <- as.numeric(sub("X","", long_GDPRate$Year))
long_GDPRate <- rename(long_GDPRate, Var_Year = Year)

Income_level <- read_xlsx("Country Classification.xlsx", sheet = 'Country Analytical History', skip = 5)

Income_level <- gather(Income_level, key = "Var_Year", value = "Income Category", "1987":"2022")
Income_level$Var_Year <- as.numeric(Income_level$Var_Year)
Income_level <- rename(Income_level, Country.Code = ...1)
Income_level <- na.omit(Income_level) 

Income_level <- Income_level[,-2]
long_GDPRate <- merge(long_GDPRate, Income_level, by = c("Var_Year" = "Var_Year", "Country.Code" = "Country.Code"), all.x = T, all.Y = T)

for (i in 1:nrow(long_GDPRate)) {
      if(long_GDPRate$Var_Year[i] >= 1981 & long_GDPRate$Var_Year[i] <= 1984){
      long_GDPRate$Wave[i] <- 1 }
      else if(long_GDPRate$Var_Year[i] >= 1990 & long_GDPRate$Var_Year[i] <= 1994){
        long_GDPRate$Wave[i] <- 2 }
      else if(long_GDPRate$Var_Year[i] >= 1995 & long_GDPRate$Var_Year[i] <= 1998){
        long_GDPRate$Wave[i] <- 3 }
      else if(long_GDPRate$Var_Year[i] >= 1999 & long_GDPRate$Var_Year[i] <= 2004){
        long_GDPRate$Wave[i] <- 4 }
      else if(long_GDPRate$Var_Year[i] >= 2005 & long_GDPRate$Var_Year[i] <= 2009){
        long_GDPRate$Wave[i] <- 5 }
      else if(long_GDPRate$Var_Year[i] >= 2010 & long_GDPRate$Var_Year[i] <= 2014){
        long_GDPRate$Wave[i] <- 6 }
      else if(long_GDPRate$Var_Year[i] >= 2017 & long_GDPRate$Var_Year[i] <= 2022){
        long_GDPRate$Wave[i] <- 7 }
      else { long_GDPRate$Wave[i] <- 10 }
}

Wave_list <- list(c(1981, 1982, 1983, 1984), c(1990, 1991 ,1992, 1993, 1994), c(1995, 1996 ,1997, 1998), 
                  c(1999, 2000 ,2001, 2002, 2003, 2004), c(2005, 2006 ,2007, 2008, 2009), 
                  c(2010, 2011 ,2012, 2013, 2014), c(2017, 2018 ,2019, 2020, 2021, 2022))

calc_growth_rate <- function(data, S_years){
      data %>%
      filter(Var_Year %in% S_years) %>%
      group_by(Wave, Country.Code) %>%
      mutate(`GDP Growth (wave)` = 
               (tail(`GDP per capita`, 1, na.rm = T) - head(`GDP per capita`, 1, na.rm = T)) / 
               head(`GDP per capita`, 1,na.rm = T) * 100) %>%
      mutate(`Income Category` = `Income Category`)
}

GDP_GR_bw <- lapply(Wave_list, function(S_years) {
  calc_growth_rate(long_GDPRate, S_years)
})

GDP_GR_f <- do.call(rbind, GDP_GR_bw)

GDP_GR_func <- function(data) {
  result <- data %>%
    group_by(Wave, Country.Code) %>%
    summarise(GDP_GRWTH_WVE = mean(`GDP Growth (wave)`, na.rm = TRUE),
              `Income Category` = first(`Income Category`))

  return(result)
}

GDP_GR_f1 <- as.data.frame(GDP_GR_func(GDP_GR_f))

# Initial GDP Per Capita

GDP_PC <- read.csv("GDP Per Capita PPP.csv",sep = ',', header = T, skip = 3)
long_GDP_PC <- gather(GDP_PC, key = "Year", value = "GDP per capita", X1960:X2022)
long_GDP_PC$Year <- as.numeric(sub("X","", long_GDP_PC$Year))
long_GDP_PC <- rename(long_GDP_PC, Var_Year = Year)

for (i in 1:nrow(long_GDP_PC)) {
  if(long_GDP_PC$Var_Year[i] >= 1981 & long_GDP_PC$Var_Year[i] <= 1984){
    long_GDP_PC$Wave[i] <- 1 }
  else if(long_GDP_PC$Var_Year[i] >= 1990 & long_GDP_PC$Var_Year[i] <= 1994){
    long_GDP_PC$Wave[i] <- 2 }
  else if(long_GDP_PC$Var_Year[i] >= 1995 & long_GDP_PC$Var_Year[i] <= 1998){
    long_GDP_PC$Wave[i] <- 3 }
  else if(long_GDP_PC$Var_Year[i] >= 1999 & long_GDP_PC$Var_Year[i] <= 2004){
    long_GDP_PC$Wave[i] <- 4 }
  else if(long_GDP_PC$Var_Year[i] >= 2005 & long_GDP_PC$Var_Year[i] <= 2009){
    long_GDP_PC$Wave[i] <- 5 }
  else if(long_GDP_PC$Var_Year[i] >= 2010 & long_GDP_PC$Var_Year[i] <= 2014){
    long_GDP_PC$Wave[i] <- 6 }
  else if(long_GDP_PC$Var_Year[i] >= 2017 & long_GDP_PC$Var_Year[i] <= 2022){
    long_GDP_PC$Wave[i] <- 7 }
  else { long_GDP_PC$Wave[i] <- 10 }
}

calc_init_GDP <- function(data, S_years){
  data %>%
    filter(Var_Year %in% S_years) %>%
    group_by(Wave, Country.Code) %>%
    mutate(`GDP initial (wave)` = 
             head(`GDP per capita`, 1, na.rm = T))
}

GDP_GR_Ann1 <- lapply(Wave_list, function(S_years) {
  calc_init_GDP(long_GDP_PC, S_years)
})

GDP_PC_Ann <- do.call(rbind, GDP_GR_Ann1)

GDP_PC_func <- function(data) {
  result <- data %>%
    group_by(Wave, Country.Code) %>%
    summarise(GDP_INIT_WVE = mean(`GDP initial (wave)`, na.rm = T))
  
  return(result)
}

GDP_PC_f1 <- as.data.frame(GDP_PC_func(GDP_PC_Ann))

# Investment in Education (as a %GDP)

Edu_Invst <- read.csv("Expenditure on Education.csv", sep = ',', header = T)

Edu_Invst <- gather(Edu_Invst, key = "Year", value = "Education Investment (GDP %)", X1960..YR1960.:X2022..YR2022.)
Edu_Invst$Year <- as.numeric(sub(".*?(\\d+).*", "\\1", Edu_Invst$Year))
Edu_Invst$`Education Investment (GDP %)` <- as.numeric(Edu_Invst$`Education Investment (GDP %)`)
Edu_Invst <- rename(Edu_Invst, Var_Year = Year)

for (i in 1:nrow(Edu_Invst)) {
  if(Edu_Invst$Var_Year[i] >= 1981 & Edu_Invst$Var_Year[i] <= 1984){
    Edu_Invst$Wave[i] <- 1 }
  else if(Edu_Invst$Var_Year[i] >= 1990 & Edu_Invst$Var_Year[i] <= 1994){
    Edu_Invst$Wave[i] <- 2 }
  else if(Edu_Invst$Var_Year[i] >= 1995 & Edu_Invst$Var_Year[i] <= 1998){
    Edu_Invst$Wave[i] <- 3 }
  else if(Edu_Invst$Var_Year[i] >= 1999 & Edu_Invst$Var_Year[i] <= 2004){
    Edu_Invst$Wave[i] <- 4 }
  else if(Edu_Invst$Var_Year[i] >= 2005 & Edu_Invst$Var_Year[i] <= 2009){
    Edu_Invst$Wave[i] <- 5 }
  else if(Edu_Invst$Var_Year[i] >= 2010 & Edu_Invst$Var_Year[i] <= 2014){
    Edu_Invst$Wave[i] <- 6 }
  else if(Edu_Invst$Var_Year[i] >= 2017 & Edu_Invst$Var_Year[i] <= 2022){
    Edu_Invst$Wave[i] <- 7 }
  else { Edu_Invst$Wave[i] <- 10 }
}

Ann_Edu_Invst <- function(data) {
  result <- data %>%
    group_by(Wave, Country.Code) %>%
    summarise(Avg_Edu_Invst = mean(`Education Investment (GDP %)`, na.rm = T))

  return(result)
}

Edu_Invst_FTP <- as.data.frame(Ann_Edu_Invst(Edu_Invst))

# Investment in R&D (as a %GDP)

RD_Invst <- read.csv("R&D Investment (GDP%).csv",
                     sep = ',', header = T)
RD_Invst <- rename(RD_Invst, Var_Year = TIME)
RD_Invst <- rename(RD_Invst, 'RD Investment (GDP %)' = Value)

for (i in 1:nrow(RD_Invst)) {
  if(RD_Invst$Var_Year[i] >= 1981 & RD_Invst$Var_Year[i] <= 1984){
    RD_Invst$Wave[i] <- 1 }
  else if(RD_Invst$Var_Year[i] >= 1990 & RD_Invst$Var_Year[i] <= 1994){
    RD_Invst$Wave[i] <- 2 }
  else if(RD_Invst$Var_Year[i] >= 1995 & RD_Invst$Var_Year[i] <= 1998){
    RD_Invst$Wave[i] <- 3 }
  else if(RD_Invst$Var_Year[i] >= 1999 & RD_Invst$Var_Year[i] <= 2004){
    RD_Invst$Wave[i] <- 4 }
  else if(RD_Invst$Var_Year[i] >= 2005 & RD_Invst$Var_Year[i] <= 2009){
    RD_Invst$Wave[i] <- 5 }
  else if(RD_Invst$Var_Year[i] >= 2010 & RD_Invst$Var_Year[i] <= 2014){
    RD_Invst$Wave[i] <- 6 }
  else if(RD_Invst$Var_Year[i] >= 2017 & RD_Invst$Var_Year[i] <= 2022){
    RD_Invst$Wave[i] <- 7 }
  else { RD_Invst$Wave[i] <- 10 }
}

Ann_RD_Invst <- function(data) {
  result <- data %>%
    group_by(Wave, LOCATION) %>%
    summarise(Avg_RD_Invst = mean(`RD Investment (GDP %)`, na.rm = T))
  
  return(result)
}

RD_Invst_FTP <- as.data.frame(Ann_RD_Invst(RD_Invst))

# Combine all the dependent and independent variables

joined_data <- full_join(
  Cultr_values,
  GDP_GR_f1,
  by = c("S002VS" = "Wave", "COUNTRY_ALPHA" = "Country.Code")) %>%
  # full_join(RD_Invst_FTP, by = c("S002VS" = "Wave", "COUNTRY_ALPHA" = "LOCATION")) %>%
  # full_join(Edu_Invst_FTP, by = c("S002VS" = "Wave", "COUNTRY_ALPHA" = "Country.Code")) %>%
  full_join(GDP_PC_f1, by = c("S002VS" = "Wave", "COUNTRY_ALPHA" = "Country.Code"))

cleaned_df <- na.omit(joined_data)

# Normalizing/Scaling Data

func_scale <- function(x){
  (x - min(x)) * 100/ (max(x) - min(x))   
}

scaled_df <- apply(cleaned_df[,c(3:5, 7) ], 2, func_scale)
scaled_df <- (cbind(cleaned_df[,c(1,2,6)], scaled_df))

# Plotting data

# Plotting GDP Growth Rate with cultural factors

pC <- ggplot(Cultr_values, aes(x = SecVal_avg, y = EmanVal_avg, color = interaction(COUNTRY_ALPHA))) +
  geom_point() +
  geom_text(aes(label = COUNTRY_ALPHA), nudge_x = -0.005, nudge_y = 0.03, show.legend = FALSE, size = 2.5) +
  facet_wrap(~ S002VS) +
  labs(title = paste("Scatter Plot of Cultural Values for Wave each wave"),
       x = "Secular Values",
       y = "Emancipative Values") +
  theme(legend.position = "none")

print(pC)

p <- ggplot(scaled_df, aes(x = SecVal_avg, y = GDP_GRWTH_WVE, color = interaction(`Income Category`))) +
  geom_point() +
  geom_text(data = scaled_df, aes(label = COUNTRY_ALPHA), nudge_x = -0.007, nudge_y = 1.5, show.legend = FALSE, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  labs(title = "Scatter Plot of GDP per capita growth rate w.r.t Secular Values",
       x = "SecVal_avg",
       y = "GDP per capita growth rate for the wave") +
  facet_wrap(~ S002VS, scales = "free") +
  theme_minimal()

print(p)

p1_1 <- ggplot(scaled_df, aes(x = SecVal_avg, y = GDP_GRWTH_WVE, color = interaction(`Income Category`))) +
  geom_point() +
  geom_text(data = scaled_df, aes(label = COUNTRY_ALPHA), nudge_x = -0.007, nudge_y = 1.5, show.legend = FALSE, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  labs(title = "Scatter Plot of GDP per capita growth rate w.r.t Secular Values",
       x = "SecVal_avg",
       y = "GDP per capita growth rate for the wave") +
  facet_wrap(~ `Income Category`, scales = "free") +
  theme_minimal()

print(p1_1)

p2 <- ggplot(scaled_df, aes(x = EmanVal_avg, y = GDP_GRWTH_WVE, color = interaction(`Income Category`))) +
  geom_point() +
  geom_text(data = scaled_df, aes(label = COUNTRY_ALPHA), nudge_x = -0.006, nudge_y = 1.5, show.legend = FALSE, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  labs(title = "Scatter Plot of GDP per capita growth rate w.r.t Emancipative Values",
       x = "Emancipative Values",
       y = "GDP per capita growth rate for the wave") +
  facet_wrap(~ S002VS, scales = "free") +
  theme_minimal()

print(p2)

p2_1 <- ggplot(scaled_df, aes(x = EmanVal_avg, y = GDP_GRWTH_WVE, color = interaction(`Income Category`))) +
  geom_point() +
  geom_text(data = scaled_df, aes(label = COUNTRY_ALPHA), nudge_x = -0.006, nudge_y = 1.5, show.legend = FALSE, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray") +
  labs(title = "Scatter Plot of GDP per capita growth rate w.r.t Emancipative Values",
       x = "Emancipative Values",
       y = "GDP per capita growth rate for the wave") +
  facet_wrap(~ `Income Category`, scales = "free") +
  theme_minimal()

print(p2_1)

# Multiple Linear Regression Model

lmodel1 <- lm(GDP_GRWTH_WVE ~ 0 + GDP_INIT_WVE + EmanVal_avg + SecVal_avg, data = scaled_df)

# vif(lmodel1)

# pairs(scaled_df[,c(-1,-6)] %>% dplyr::select_if(is.numeric))
# cor(scaled_df[,c(-1,-6)] %>% dplyr::select_if(is.numeric))

lmodel2 <- lm(GDP_GRWTH_WVE ~ 0 + GDP_INIT_WVE + EmanVal_avg + SecVal_avg + as.factor(`Income Category`), data = scaled_df)
lmodel3 <- lm(GDP_GRWTH_WVE ~ 0 + GDP_INIT_WVE + EmanVal_avg + SecVal_avg + as.factor(S002VS), data = scaled_df)
lmodel4 <- lm(GDP_GRWTH_WVE ~ 0 + GDP_INIT_WVE + EmanVal_avg + SecVal_avg + as.factor(S002VS) + 
                as.factor(`Income Category`), data = scaled_df)

model_list <- list(lmodel1, lmodel2, lmodel3, lmodel4)
modelsummary(model_list, stars = T, 
             coef_map = c("GDP_INIT_WVE" = "Initial GDP",
                           "EmanVal_avg" = "Emancipative Values",
                           "SecVal_avg" = "Secular Values",
                           "as.factor(Income Category).." = "Country Income Category (U/f)",
                           "as.factor(Income Category)H" = "Income Category - H",
                           "as.factor(Income Category)L" = "Income Category - L",
                           "as.factor(Income Category)LM" = "Income Category - LM",
                           "as.factor(Income Category)UM" = "Income Category - UM",
                           "as.factor(S002VS)2" = "Wave 2",
                           "as.factor(S002VS)3" = "Wave 3",
                           "as.factor(S002VS)4" = "Wave 4",
                           "as.factor(S002VS)5" = "Wave 5",
                           "as.factor(S002VS)6" = "Wave 6",
                           "as.factor(S002VS)7" = "Wave 7"), 
             output = "ML_Reg_tableResult.docx")

# anova(lmodel1, lmodel2, lmodel3, lmodel4) 

# Ridge/Lasso Regression and Cross Validation

x <- model.matrix(GDP_GRWTH_WVE ~ 0 + GDP_INIT_WVE + EmanVal_avg + SecVal_avg + `Income Category` +
                    as.factor(S002VS), data = scaled_df)

y <- as.matrix(as.numeric(scaled_df[,6]))

# lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
ridge_cv1<- cv.glmnet(x, y, alpha = 0, nfolds = 10)

# plot(ridge_cv)
# summary(ridge_cv)

min_lambda <- ridge_cv1$lambda.min
model_pp <- glmnet(x, y, alpha = 0, lambda = min_lambda)
model_np <- glmnet(x, y, alpha = 0, lambda = 0)
model_cp <- glmnet(x, y, alpha = 0, lambda = 10^6)

y_hat_pp <- predict(model_pp, x, s = min_lambda)
y_hat_np <- predict(model_np, x, s = 0)
y_hat_cp <- predict(model_cp, x, s = 0)
# ssr_cv <- (t(y - y_hat_cv) %*% (y - y_hat_cv)) # R2
# rsq_ridge_cv <- cor(y, y_hat_cv)^2

sum((y_hat_pp - y)^2)
sum((y_hat_np - y)^2)
sum((y_hat_cp - y)^2)

model_list_cv <- list(model_pp, model_np, model_cp)

modelsummary(model_list_cv, 
             coef_map = c("(Intercept)" = "Intercept",
                          "GDP_INIT_WVE" = "Initial GDP",
                          "EmanVal_avg" = "Emancipative Values",
                          "SecVal_avg" = "Secular Values",
                          "`Income Category`.." = "Country Income Category (U/f)",
                          "`Income Category`H" = "Income Category - H",
                          "`Income Category`L" = "Income Category - L",
                          "`Income Category`LM" = "Income Category - LM",
                          "`Income Category`UM" = "Income Category - UM",
                          "as.factor(S002VS)2" = "Wave 2",
                          "as.factor(S002VS)3" = "Wave 3",
                          "as.factor(S002VS)4" = "Wave 4",
                          "as.factor(S002VS)5" = "Wave 5",
                          "as.factor(S002VS)6" = "Wave 6",
                          "as.factor(S002VS)7" = "Wave 7"),
             output = "ML_Ridge_tableResult.docx")
