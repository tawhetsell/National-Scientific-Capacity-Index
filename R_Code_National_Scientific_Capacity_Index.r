# Meta-Data ---------------------------------------------------------------
#Author: Travis A. Whetsell
#Date: March 19, 2024
#Title: Developing an Index of National Research Capacity

library(WDI)
library(vdemdata)
library(dplyr)
library(readr)
library(purrr)
library(gridExtra)
library(psych)
library(corrgram)
library(GGally)
library(plm)
library(brms)
library(mice)

# look at all wdi indicators
# all_indicators <- WDIsearch()
# View(all_indicators)

gdp_constant <- WDI(indicator = "NY.GDP.MKTP.KD", start = 2012, end = 2021, extra = FALSE) %>%
  select(iso3c, year, NY.GDP.MKTP.KD)  %>% 
  filter(!is.na(iso3c)) %>%  
  filter(iso3c != "") %>%  
  filter(trimws(iso3c) != "") %>% 
  rename(country_code = iso3c, gdp = NY.GDP.MKTP.KD) %>% 
  arrange(country_code, year)  

rd_expenditure <- WDI(indicator = "GB.XPD.RSDV.GD.ZS", start = 2012, end = 2021, extra = FALSE) %>%
  select(iso3c, year, GB.XPD.RSDV.GD.ZS)%>%
  filter(!is.na(iso3c)) %>%  
  filter(iso3c != "") %>%  
  filter(trimws(iso3c) != "") %>% 
  rename(country_code = iso3c, rdgdp = GB.XPD.RSDV.GD.ZS) %>% 
  arrange(country_code, year)  

patents_residents <- WDI(indicator = "IP.PAT.RESD", start = 2012, end = 2021, extra = FALSE) %>%
  select(iso3c, year, IP.PAT.RESD) %>%
  filter(!is.na(iso3c)) %>%  
  filter(iso3c != "") %>%  
  filter(trimws(iso3c) != "") %>% 
  rename(country_code = iso3c, respatent = IP.PAT.RESD) %>% 
  arrange(country_code, year)  

pop_size <- WDI(indicator = "SP.POP.TOTL", start = 2012, end = 2021, extra = FALSE) %>%
  select(iso3c, year, SP.POP.TOTL) %>%
  filter(!is.na(iso3c)) %>%  
  filter(iso3c != "") %>%  
  filter(trimws(iso3c) != "") %>% 
  rename(country_code = iso3c, pop = SP.POP.TOTL) %>% 
  arrange(country_code, year)  

vdem_selected <- vdem %>%
  dplyr::filter(year >= 2012 & year <= 2021) %>%
  dplyr::select(country_text_id,
                country_name,
                year,
                v2x_rule,
                e_wbgi_pve,
                e_wbgi_cce,
                v2x_polyarchy,
                v2xca_academ,
                v2smregcon,
                e_wbgi_rqe,
                e_regionpol) %>%
  rename(country_code = country_text_id) %>% 
  arrange(country_code, year) 

scopus <- read_csv("elsevier_fwci.csv") %>%
  filter(date >= 2012 & date <= 2021) %>%
  select(date, countryiso3code, frac_pubs, number_unique_authors, frac_intl, frac_pubs_top10p_FWCI_perc, frac_FWCI, full_pubs) %>%
  mutate(
    frac_pubs = as.numeric(frac_pubs),
    number_unique_authors = as.numeric(number_unique_authors),
    frac_intl = as.numeric(frac_intl),
    frac_pubs_top10p_FWCI_perc = as.numeric(frac_pubs_top10p_FWCI_perc),
    frac_FWCI = as.numeric(frac_FWCI)
  ) %>%
  rename(year = date,country_code = countryiso3code) %>% 
  arrange(country_code, year) 

scopus_inst <- read_csv("elsevier_inst.csv") %>%
  filter(date >= 2012 & date <= 2021) %>%
  select(date, countryiso3code, All_Unique_Institutions, All_Unique_Academic_Institutions) %>%
  rename(year = date, country_code = countryiso3code) %>%
  mutate(
    All_Unique_Institutions = as.numeric(All_Unique_Institutions),
    All_Unique_Academic_Institutions = as.numeric(All_Unique_Academic_Institutions)
  ) %>%
  arrange(country_code, year)

merged_data <- reduce(list(gdp_constant, rd_expenditure, patents_residents, pop_size), full_join, by = c("country_code", "year"))
merged_data <- full_join(merged_data, vdem_selected, by = c("country_code", "year"))
merged_data <- full_join(merged_data, scopus, by = c("country_code", "year"))
merged_data <- full_join(merged_data, scopus_inst, by = c("country_code", "year"))

merged_data <- merged_data %>%
  arrange(country_code, year)

# transforming RDGDP to RD

merged_data$rdraw <-
  (merged_data$rdgdp / 100) * merged_data$gdp

# getting nonacademic institutions

merged_data$NonAcadInst <-
  (merged_data$All_Unique_Institutions - merged_data$All_Unique_Academic_Institutions)

# some values have NULL/NA listed
merged_data <- merged_data[!is.na(merged_data$country_code), ]

merged_data <- merged_data %>%
  filter(country_code != "NULL" | is.na(country_code))

# values divided by zero are NULL for frac_intl

merged_data <- merged_data %>%
  mutate(frac_intl = ifelse(toupper(trimws(frac_intl)) == "NULL", 0, frac_intl))

# renaming variables

merged_data <- merged_data %>%
  rename(
    date = year,
    PopSize = pop, 
    GDP = gdp,  
    RD = rdraw, 
    ResPatent = respatent, 
    AcadInst = All_Unique_Academic_Institutions, 
    AllInst = All_Unique_Institutions, 
    Authors = number_unique_authors, 
    RuleLaw = v2x_rule,
    Stability = e_wbgi_pve, 
    NonCorrupt = e_wbgi_cce, 
    Polyarchy = v2x_polyarchy, 
    AcadFreedom = v2xca_academ, 
    IntlPubs = frac_intl, 
    Pubs = frac_pubs, 
    OpenInternet=v2smregcon, 
    Region =e_regionpol, 
    Top10FWCI = frac_pubs_top10p_FWCI_perc, 
    FWCI = frac_FWCI,
    RegQual = e_wbgi_rqe
  )

# making reduced data frame

reduced_data <-
  select(
    merged_data,
    country_code,
    date,
    Region,
    full_pubs,
    PopSize,
    GDP,
    RD,
    ResPatent,
    AcadInst,
    NonAcadInst,
    Authors,
    Pubs,
    IntlPubs,
    OpenInternet,
    RuleLaw,
    Stability,
    RegQual,
    NonCorrupt,
    Polyarchy,
    AcadFreedom,
    FWCI,
    Top10FWCI
  )

# making sure variables are numeric

reduced_data <- reduced_data %>%
  mutate_at(vars(-country_code,-date,-Region), as.numeric)

# id missing data

missing <- sapply(reduced_data, function(x)
  sum(is.na(x)))
print(missing)


# impute ---------------------------------------------------------------

# this imputes if there is available data in any year for the country, identifies number of missing

reduced_data_imp <- data.frame() 

reduced_data_imp <- reduced_data %>%
  group_by(country_code) %>%
  mutate_at(vars(PopSize:Top10FWCI),
            ~ ifelse(is.na(.) & any(!is.na(.)), mean(., na.rm = TRUE), .)) %>%
  ungroup()

#

missing2 <- sapply(reduced_data_imp, function(x)
  sum(is.na(x)))
print(missing2)

missing - missing2

#

reduced_data_imp <- reduced_data_imp %>%
  filter(!is.na(FWCI))

missing3 <- sapply(reduced_data_imp, function(x)
  sum(is.na(x)))
print(missing3)

missing2 - missing3

###

new_df <-
  reduced_data_imp %>% select(
    country_code,
    Region,
    PopSize,
    GDP,
    RD,
    ResPatent,
    AcadInst,
    NonAcadInst,
    Authors,
    Pubs,
    IntlPubs,
    OpenInternet,
    RuleLaw,
    RegQual,
    Stability,
    NonCorrupt,
    Polyarchy,
    AcadFreedom,
    FWCI,
    Top10FWCI
  )

# collapse ---------------------------------------------------------------

# collapses the panel data on the country mean 

collapsed_data <- new_df %>%
  group_by(country_code) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  group_by(country_code, .add = TRUE)

missing5 <- sapply(collapsed_data, function(x)
  sum(is.na(x)))
print(missing5)

collapsed_data <- collapsed_data[!is.na(collapsed_data$Stability),]

missing6 <- sapply(collapsed_data, function(x)
  sum(is.na(x)))
print(missing6)

# write.csv(collapsed_data, file = "data05042023.csv", row.names = FALSE)

# replace NaN

collapsed_data <-
  collapsed_data %>% mutate_if(is.numeric, ~ replace(., is.nan(.), NA))

# imputation ---------------------------------------------------------------

# use multiple imputation for RD and ResPatent, must be done separately. STP was dropped because missing on insitutional data

collapsed_data <- collapsed_data %>%
  filter(country_code != "STP")

# impute RD

set.seed(12345)
impute_RD <- collapsed_data %>%
  ungroup() %>%
  select(RD, AcadInst, NonAcadInst, Authors, Pubs, IntlPubs)
imputed_data_RD <- mice(impute_RD, m = 5, maxit = 10, meth = 'pmm')

# calculate the mean for each missing value across all imputations
long_data_RD <- complete(imputed_data_RD, "long")
mean_imputed_data_RD <- long_data_RD %>%
  group_by(.id) %>%
  summarise(across(-.imp, ~ mean(.x, na.rm = TRUE)))

collapsed_data$RD_i <- mean_imputed_data_RD$RD 

# impute PAT

set.seed(12345)
impute_PAT <- collapsed_data %>%
  ungroup() %>%
  select(ResPatent, AcadInst, NonAcadInst, Authors, Pubs, IntlPubs)
imputed_data_PAT <- mice(impute_PAT, m = 5, maxit = 10, meth = 'pmm')

# calculate the mean for each missing value across all imputations
long_data_PAT <- complete(imputed_data_PAT, "long")
mean_imputed_data_PAT <- long_data_PAT %>%
  group_by(.id) %>%
  summarise(across(-.imp, ~ mean(.x, na.rm = TRUE)))

collapsed_data$ResPatent_i <- mean_imputed_data_PAT$ResPatent

# log transform variables

collapsed_data <- collapsed_data %>%
  mutate(across(
    c(
      PopSize,
      GDP,
      RD_i,
      ResPatent_i,
      AcadInst,
      NonAcadInst,
      Authors,
      Pubs,
      IntlPubs
    ),
    list(ln = ~ log(.x + 1)),
    .names = "ln_{.col}"
  ))

ln_vars_collapsed <- collapsed_data %>%
  select(
    country_code,
    ln_RD_i,
    ln_ResPatent_i,
    ln_AcadInst,
    ln_NonAcadInst,
    ln_Authors,
    ln_Pubs,
    ln_IntlPubs,
    OpenInternet,
    RuleLaw,
    RegQual,
    Stability,
    NonCorrupt,
    Polyarchy,
    AcadFreedom,
    FWCI
  )


# correlations ---------------------------------------------------------------

# custom correlation function without grid lines

cor_func <- function(data, mapping, method, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, method = "spearman", use = 'complete.obs')
  
  ggally_text(
    label = as.character(round(corr, 3)), 
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    color = 'black',
    ...
  ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 12)
    )+
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), color = "grey", fill = NA)
}

# custom function for lower part of the plot

lower_plot <- function(data, mapping, ...){
  ggally_points(data, mapping, size = 1) + 
    geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "blue", alpha = 0.3, ...)
}

# custom function for diagonal plot

diag_hist <- function(data, mapping, ...){
  ggplot(data, mapping) + 
    geom_histogram(color = "black", fill = "grey", ...)
}

ln_vars_collapsed[, -1] %>%
  GGally::ggpairs(
    axisLabels = "none",
    upper = list(
      continuous = cor_func, 
      combo = "box_no_facet"
    ),
    lower = list(
      continuous = lower_plot 
    ),
    diag = list(
      continuous = diag_hist 
    )
  ) 

# factor analysis ---------------------------------------------------------------

a <- as.data.frame(ln_vars_collapsed[,-c(1, 16)])
a <- psych::rescale(a)

datamatrix_a <- cor(a)
KMO(r = datamatrix_a)
print(cortest.bartlett(datamatrix_a, nrow(a[, -1])))
parallel <- fa.parallel(a, fm ="pa")

# kaiser, eigen , scree,  

ev <- eigen(cor(a))
ev$values
Factor <- rep(1:14)
Eigen_Values <- ev$values
Scree <- data.frame(Factor, Eigen_Values)

Cumulative_Values <- cumsum(Eigen_Values)
Proportion_Values <- Eigen_Values / sum(Eigen_Values)
Scree$Cumulative_Values <- Cumulative_Values
Scree$Proportion_Values <- Proportion_Values
print(Scree)

# create the ggplot

p <- ggplot(data = Scree, mapping = aes(x = Factor)) +
  geom_point(aes(y = Eigen_Values, color = "Eigenvalue"), shape = 16) +
  geom_line(aes(y = Eigen_Values, color = "Eigenvalue")) +
  geom_point(aes(y = Cumulative_Values, color = "Cumulative"), shape = 17) +
  geom_line(aes(y = Cumulative_Values, color = "Cumulative")) +
  geom_hline(yintercept = 1, linetype = "solid", color = "black") +
  scale_y_continuous(name = "Eigen Values", sec.axis = sec_axis(~ . / sum(Eigen_Values) * 100, name = "Cumulative (%)")) +
  scale_color_manual(values = c("Eigenvalue" = "black", "Cumulative" = "red")) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "grey")) +
  ggtitle("") +
  theme(plot.margin = margin(5, 10, 5, 10, "mm"))

# Add a horizontal line for cumulative values

p <- p + geom_hline(yintercept = max(Scree$Cumulative_Values), linetype = "dashed", color = "red")

# Print the ggplot

print(p)


#fa method

fa_model <-
  fa(
    a,
    fm = "pa",
    nfactors = 2,
    max.iter = 1000,
    rotate = "varimax"
  )
fa_model

loadings_to_dataframe <- function(fa_model) {
  loadings <- fa_model$loadings
  factor <- rownames(loadings)
  PA1 <- round(loadings[, 1], digits = 3)
  PA2 <- round(loadings[, 2], digits = 3)
  h2 <- round(fa_model$communality, digits = 3)
  u2 <- round(fa_model$uniquenesses, digits = 3)
  com <- round(h2 / u2, digits = 3)
  data_frame <- data.frame(Factor = factor, PA1 = PA1, PA2 = PA2, h2 = h2, u2 = u2, com = com)
  return(data_frame)
}

loadings <- loadings_to_dataframe(fa_model)
loadings

write.csv(loadings, "loadings.csv", row.names = TRUE)

# get factor scores 

scores <- as.data.frame(fa_model$scores)

# conduct alpha test

capacity_vars <-
  ln_vars_collapsed %>% select(ln_RD_i,
                               ln_ResPatent_i,
                               ln_AcadInst,
                               ln_NonAcadInst,
                               ln_Authors,
                               ln_Pubs,
                               ln_IntlPubs)

governance_vars <-
  ln_vars_collapsed %>% select(OpenInternet,
                               RuleLaw,
                               RegQual,
                               Stability,
                               NonCorrupt,
                               Polyarchy,
                               AcadFreedom)

# cronbach alpha ---------------------------------------------------------------

# received the following warning,double checked data are numeric "Number of categories should be increased  in order to count frequencies."

cap_alpha_score <-
  psych::alpha(capacity_vars[,-1], check.keys = FALSE)
cap_alpha_score
summary(cap_alpha_score)

gov_alpha_score <-
  psych::alpha(governance_vars[,-1], check.keys = FALSE)
gov_alpha_score
summary(gov_alpha_score)

# send factor scores to data set

ln_vars_collapsed$cap_ind <- psych::rescale(scores$PA1)[[1]]
ln_vars_collapsed$gov_ind <- psych::rescale(scores$PA2)[[1]]

# country ranks plots - factors ---------------------------------------------------------------

plot1 <-
  ggplot(data = na.omit(ln_vars_collapsed), aes(x = reorder(country_code, cap_ind), y = -cap_ind)) +
  labs(x = "Country", y = "Capacity Factor Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.75, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )

# gov

plot2 <-
  ggplot(data = na.omit(ln_vars_collapsed), aes(x = reorder(country_code, gov_ind), y = -gov_ind)) +
  labs(x = "Country", y = "Governance Factor Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.75, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )

#

ln_vars_collapsed$interaction_term <-
  ln_vars_collapsed$cap_ind * ln_vars_collapsed$gov_ind


# plot 3
plot3 <-
  ggplot(data = na.omit(ln_vars_collapsed),
         aes(
           x = reorder(country_code, interaction_term),
           y = -interaction_term
         )) +
  labs(x = "Country", y = "Capacity X Governance Factor Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.75, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )


grid.arrange(plot1, plot2, plot3,
             layout_matrix = rbind(c(1, 2, 3)))

# country ranks figure - sum ---------------------------------------------------------------

# ranks change depending on factorscore/summative 

gov_vars_rescaled <-
  as.data.frame(lapply(governance_vars[, -1], psych::rescale))
gov_sum_index <- rowSums(gov_vars_rescaled)

cap_vars_rescaled <-
  as.data.frame(lapply(capacity_vars[, -1], psych::rescale))
cap_sum_index <- rowSums(cap_vars_rescaled)

# create index without RD and respatents

cap_vars_rescaled_alt <-
  as.data.frame(lapply(capacity_vars[, -(1:3)], psych::rescale))
cap_sum_index_alt <- rowSums(cap_vars_rescaled_alt)

# Add the summative index to the dataset

ln_vars_collapsed$cap_sum_index <- cap_sum_index
ln_vars_collapsed$gov_sum_index <- gov_sum_index
ln_vars_collapsed$cap_sum_index_alt <- cap_sum_index_alt 

# plot 4

plot4 <-
  ggplot(data = na.omit(ln_vars_collapsed), aes(
    x = reorder(country_code, cap_sum_index),
    y = -cap_sum_index
  )) +
  labs(x = "Country", y = "Capacity Sum Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.5, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )

# plot 5

plot5 <-
  ggplot(data = na.omit(ln_vars_collapsed), aes(
    x = reorder(country_code, gov_sum_index),
    y = -gov_sum_index
  )) +
  labs(x = "Country", y = "Governance Sum Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.5, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )

#

ln_vars_collapsed$int_sum_term <-
  ln_vars_collapsed$cap_sum_index * ln_vars_collapsed$gov_sum_index

# plot 6

plot6 <-
  ggplot(data = na.omit(ln_vars_collapsed), aes(
    x = reorder(country_code, int_sum_term),
    y = -int_sum_term
  )) +
  labs(x = "Country", y = "Capacity X Governance Sum Index") +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(
      size = 6,
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 6),
    plot.margin = unit(c(0.1, 0.15, 0.1, 0.1), "cm"),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none"
  ) +
  geom_point(size = 1.5, pch = 16) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x)
      - x
  )

grid.arrange(plot4, plot5, plot6,
             layout_matrix = rbind(c(1, 2, 3)))


# Cross-Section Regression ---------------------------------------------------------------

reg_data_collapsed <- ln_vars_collapsed

reg_data_collapsed <- merge(reg_data_collapsed, collapsed_data[, c("country_code", "Region", "PopSize", "GDP")], by = "country_code", all.x = TRUE)

mean_by_country <- aggregate(full_pubs ~ country_code, merged_data, mean)
reg_data_collapsed <- merge(reg_data_collapsed, mean_by_country, by = "country_code", all.x = TRUE)
colnames(reg_data_collapsed)[colnames(reg_data_collapsed) == "full_pubs"] <- "mean_full_pubs"

filtered_data <- reg_data_collapsed %>% filter(mean_full_pubs > 50)

# Fit the Bayesian mixed model

model1a <-
  brm(
    FWCI ~ cap_ind + gov_ind + (1 | Region),
    data = filtered_data,
    family = gaussian(),
    iter = 10000,
    warmup = 1000,
    thin = 10,
    chains = 12,
    seed = 12345,
    control = list(adapt_delta = 0.95)
  )
print(model1a, digits = 4)

# Panel Regression-------------------------------------------------------------------

reg_panel <- reduced_data_imp
reg_panel_full  <- reg_panel[!is.na(reg_panel$Stability),]

# for some reason STP is missing in the institution data, so was dropped to allow MICE impute

reg_panel_full <- reg_panel_full   %>%
  filter(country_code != "STP")

# impute RD using mice package

reg_panel_RD <- reg_panel_full %>%
  select(
    country_code,
    date,
    
    RD,
    
    AcadInst,
    NonAcadInst,
    Authors,
    Pubs,
    IntlPubs
    
  )

imp_panel_RD <- mice(reg_panel_RD, m = 5, maxit = 5, meth = 'pmm', seed = 12345)
imp_panel_RD_long <- complete(imp_panel_RD, "long")
imp_panel_RD_long_mean <- imp_panel_RD_long %>%
  group_by(.id) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
reg_panel_full$RD_i <-imp_panel_RD_long_mean$RD 

# impute patent

reg_panel_PAT <- reg_panel_full %>%
  select(
    country_code,
    date,
    
    ResPatent,
    
    AcadInst,
    NonAcadInst,
    Authors,
    Pubs,
    IntlPubs
    
  )

imp_panel_PAT <- mice(reg_panel_PAT, m = 5, maxit = 5, meth = 'pmm', seed = 12345)
imp_panel_PAT_long <- complete(imp_panel_PAT, "long")
imp_panel_PAT_long_mean <- imp_panel_PAT_long %>%
  group_by(.id) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
reg_panel_full$ResPatent_i <-imp_panel_PAT_long_mean$ResPatent

# make panel 

new_df_panel <-
  reg_panel_full %>% select(
    country_code,
    date,
    Region,
    PopSize,
    GDP,
    RD_i,
    ResPatent_i,
    AcadInst,
    NonAcadInst,
    Authors,
    Pubs,
    IntlPubs,
    OpenInternet,
    RuleLaw,
    RegQual,
    Stability,
    NonCorrupt,
    Polyarchy,
    AcadFreedom,
    FWCI,
    Top10FWCI
  )

reg_data_panel <- new_df_panel
reg_data_panel <-
  pdata.frame(reg_data_panel, index = c("country_code", "date"))

# balance the panel dataset

balanced_data <- make.pbalanced(reg_data_panel, balance.type = "fill", fill = NA)

# create summative indexes Add the summative index to the dataset

balanced_data_log <- balanced_data %>%
  mutate(across(
    c(
      PopSize,
      GDP,
      RD_i,
      ResPatent_i,
      AcadInst,
      NonAcadInst,
      Authors,
      Pubs,
      IntlPubs
    ),
    list(ln = ~ log(.x + 1)),
    .names = "ln_{.col}"
  ))


balanced_data_log$cap_sum_index <- rowSums(balanced_data_log[, c("ln_RD_i", "ln_ResPatent_i", "ln_AcadInst", "ln_NonAcadInst", "ln_Authors", "ln_Pubs", "ln_IntlPubs")])

balanced_data_log$gov_sum_index <- rowSums(balanced_data_log[, c("OpenInternet", "RuleLaw", "RegQual" , "Stability" , "NonCorrupt" , "Polyarchy" , "AcadFreedom")])

# filter the data based on the full_pubs > 50 condition

balanced_data_log$date <- as.character(balanced_data_log$date)
merged_data$date <- as.character(merged_data$date)

balanced_data_log_updated <- left_join(balanced_data_log, select(merged_data, country_code, date, full_pubs), by = c("country_code", "date"))


panel <-balanced_data_log_updated %>%
  filter(full_pubs > 50)

# Bayesian linear mixed-effects 

panel$date_numeric <- as.numeric(as.character(panel$date))

model1b <- brm(
  formula = FWCI ~ cap_sum_index + gov_sum_index + date_numeric + 
    (1 | Region / country_code),
  data = panel,
  family = gaussian(),
  iter = 10000,
  warmup = 1000,
  thin = 10,
  chains = 12,
  cores = 4,  
  seed = 12345,
  control = list(adapt_delta = 0.95, max_treedepth = 10)
)

print(model1b, digits = 4)
