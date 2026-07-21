# Descriptive Analysis and Modeling

**Author:** Bikram Kumar Singh
**Date:** 2026-07-21

```r
library(haven)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(knitr)
library(gt)
library(flextable)
library(officer)
library(cardx)
library(cards)
library(broom)



## Importing engineered dataset (clean data) ----

data <- readRDS("/Users/.........../MEIA_clean.rds")


# Phase II Of Analysis ----


# Table 1: General Characteristics of the population ----

table_1 <- data %>%
  dplyr::select(SEX, AGE, BMI, BMI_cat, ethnic_cat, SMOKER,
         UTHYRDISX_cat, thyroid_medication, PATOTC_cat, isced_cat3, ALKSCR_cat) %>%
  
  tbl_summary(
    by = SEX,
    
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})", 
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      all_continuous()  ~ 2,
      all_categorical() ~ c(0, 2)
    ),
    
    label = list(
      AGE        ~ "Age (years)",
      BMI       ~ "BMI (kg/m²)",
      BMI_cat      ~ "BMI Status",
      ethnic_cat   ~ "Ethnicity",
      SMOKER        ~ "Smoking Status",
      UTHYRDISX_cat ~ "Any Thyroid Disorder (Yes)",
      thyroid_medication ~ "Thyroid Medication Use (Yes)",
      PATOTC_cat    ~ "Physical Activity Level",
      isced_cat3     ~ "Level of Education",
      ALKSCR_cat    ~ "Risky Alcohol Consumption"
    ),
    
    missing = "no"
  ) %>%
  
  add_overall(last = FALSE) %>%   # puts Total column first
  
  add_p(
    test = list(
      AGE    ~ "wilcox.test",
      BMI   ~ "wilcox.test",
      BMI_cat     ~ "chisq.test",
      ethnic_cat    ~ "chisq.test",
      SMOKER    ~ "chisq.test",
      UTHYRDISX_cat ~ "chisq.test",
      thyroid_medication ~ "fisher.test",
      PATOTC_cat    ~ "chisq.test",
      isced_cat3    ~ "chisq.test",
      ALKSCR_cat    ~ "chisq.test"
     
    ),
    
    pvalue_fun = ~style_pvalue(.x, digits = 3)   
  ) %>%
  
  bold_labels() %>%
  
  modify_header(
    label   ~ "**Characteristics**",
    stat_0  ~ "**Total** (n = {N})",
    stat_1  ~ "**Male** (n = {n})",
    stat_2  ~ "**Female** (n = {n})",
    p.value ~ "**p-value**"
  ) %>%
  
  modify_caption(
    "**Table 1. Characteristics of the MEIA study population, total and stratified by sex.**"
  ) %>%
  
  modify_footnote(
    all_stat_cols() ~ "Median (Quartile 1, Quartile 3) reported for continuous variables; n (column %) for categorical variables.",
    p.value ~ "Mann–Whitney U test for continuous variables; Chi-square test or Fisher's Exact Test for categorical variables."
  )


# Printing table_1
table_1   



# Table 2: Food Consumption by the population ----


data_diet <- data

data_diet <- data %>% dplyr::select(
  fleisch, Fleisch__und_Wurstwaren, Fisch_und_Fischwaren, Eier,
  Milch_und_Milchprodukte, Cheese_subgroup, Butter, speisefette,  
  Brot_und_Backwaren,
    Bread_subgroup, naehrmittel, Vollkornprodukte, Kartoffeln, gemuese,
   huelsenfruechte, obst, nuesse, GCAL, SEX)

data_diet_clean <- na.omit(data_diet)


table_2 <- data_diet_clean %>%
  dplyr::select(
    SEX,
    fleisch, Fleisch__und_Wurstwaren,
    Fisch_und_Fischwaren, Eier,
    Milch_und_Milchprodukte, Cheese_subgroup,
    Butter, speisefette, Brot_und_Backwaren,
    Bread_subgroup, 
    naehrmittel, Vollkornprodukte,
    Kartoffeln, gemuese,
    huelsenfruechte, obst, nuesse, GCAL
  ) %>%
  tbl_summary(
    by        = SEX,
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",  
    digits    = all_continuous() ~ 1,
    label = list(
      fleisch                 ~ "Fresh Meat",
      Fleisch__und_Wurstwaren ~ "Processed Meat",
      Fisch_und_Fischwaren   ~ "Fish and Fish Products",
      Eier                    ~ "Eggs",
      Milch_und_Milchprodukte   ~ "Milk and Milk Products",
      Cheese_subgroup      ~ "Cheese (sub-group)",
      Butter                  ~ "Butter",
      speisefette             ~ "Other Edible Fats/Oils",
      Brot_und_Backwaren ~ "Bread and Bakery Products",
      Bread_subgroup      ~ "Bread (sub-group)",
      naehrmittel            ~ "Staple Food",
      Vollkornprodukte    ~ "Whole Grain Products",
      Kartoffeln             ~ "Potatoes",
      gemuese          ~ "Vegetables",
      huelsenfruechte        ~ "Legumes",
      obst            ~ "Fruits",
      nuesse              ~ "Nuts",
      GCAL           ~ "Total Energy Intake (kcal/d)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%                             
  add_p(
    test      = all_continuous() ~ "wilcox.test",
    pvalue_fun = ~ style_pvalue(.x, digits = 3)  
  ) %>%
  bold_labels() %>%
  modify_header(
    label   ~ "**Food Group or Subgroup**",
    stat_0  ~ "**Total (n = {N})**",
    stat_1  ~ "**Male (n = {n})**",
    stat_2  ~ "**Female (n = {n})**",           
    p.value ~ "**p-Value**"
  ) %>%
  modify_caption(
    "**Table 2: Habitual daily food consumption (gm/day) and total energy intake in the study population stratified by sex**"
  ) %>%
  modify_footnote(
    c(stat_0, stat_1, stat_2) ~ "Median (Quartile 1, Quartile 3) reported. Food group intake expressed as grams per day and total energy intake is expressed as kilocalories per day.",
    p.value                   ~ "Mann-Whitney U test used for all comparisons between males and females."
  )


## Displaying 2nd Table

table_2


-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

## Table 3: Measures of Iodine Intake and Thyroid Volume Stratified by Sex ----


# 1. Spot UIC baseline and follow-up and mean
# 2. UIC/Cr baseline and follow up
# 3. 24-hr estimated Iodine excretion baseline and follow up and mean
# 4. Thyroid Volume


data_JOD <- data %>% dplyr::select(HEIGHT, BMI, WEIGHT, AGE, SEX, JODbas, JODfu1, CREATbas, 
                            CREATfu1, UTHVLTOT, MJ)

## Mutating iodine variable and thyriod volume
data_JOD <- data_JOD %>% 
  mutate(
    i_cr_bas = ifelse(CREATbas == 0, NA, (JODbas / CREATbas)), # µg/g (i/Cr)
    i_cr_fu1 = ifelse(CREATfu1 == 0, NA, (JODfu1 / CREATfu1)),
    mean_JOD = case_when(
        is.na(JODbas) & is.na(JODfu1)   ~ NA_real_,
        is.na(JODfu1)             ~ JODbas,
        is.na(JODbas)              ~ JODfu1,
        !is.na(JODbas) & !is.na(JODfu1) ~ (JODbas + JODfu1) / 2),
    gender = case_when(SEX == "F" ~ 0,
                       SEX == "M" ~ 1),
    RefCreat_24 = exp(1)^(1.9539 + 0.1681 * gender - 0.0027 * AGE 
                       + 0.0129 * WEIGHT - 0.0129 * BMI), # in mmol
    RefCreat_24 = RefCreat_24 * 0.11312, # Converting mmoL to g; Molecular weight of Creat is 0.113120 g/mmoL
    RefThyroid_Volume = case_when(
      SEX == "M" ~ 9.066889 - 207.886 * (1 / sqrt(AGE)) +
        0.31143  * HEIGHT +
        0.0472153 * WEIGHT,
  
      SEX == "F" ~ 0.049582 +
        0.0024958  * (AGE^2) +
        0.0486403  * HEIGHT +
        0.1881983  * WEIGHT,
      
      TRUE ~ NA_real_
    ),
    JODexbas_24 = i_cr_bas * RefCreat_24, # µg/day ## Estimated Iodine excretion per day
    JODexfu1_24 = i_cr_fu1 * RefCreat_24, # µg/day
    mean_JODex_24 = case_when(
      is.na(JODexbas_24) & is.na(JODexfu1_24)   ~ NA_real_,
      is.na(JODexfu1_24)                    ~ JODexbas_24,
      is.na(JODexbas_24)                    ~ JODexfu1_24,
      !is.na(JODexbas_24) & !is.na(JODexfu1_24) ~ (JODexbas_24 + JODexfu1_24) / 2),
    JODexbas_24_cat = factor(case_when(
      JODexbas_24 < 135 ~ "Below Adequate: <135 µg/day",
      JODexbas_24 >= 135 ~ "Adequate: ≥135 µg/day"),
      levels = c("Below Adequate: <135 µg/day",
                 "Adequate: ≥135 µg/day")
                 ),
    JODexfu1_24_cat = factor(case_when(
      JODexfu1_24 < 135 ~ "Below Adequate: <135 µg/day",
      JODexfu1_24 >= 135 ~ "Adequate: ≥135 µg/day"),
      levels = c("Below Adequate: <135 µg/day",
                 "Adequate: ≥135 µg/day")
    ),
    Goitre_Gutekunst = factor(
      case_when(
        is.na(UTHVLTOT) ~ NA_character_,
        SEX == "M" & UTHVLTOT > 25 ~ "Goitre",
        SEX == "F" & UTHVLTOT > 18 ~ "Goitre",
        TRUE ~ "Normal"),
      levels = c("Normal", "Goitre")
      ),
    Goitre_Ittermann = factor(
      case_when(
      is.na(UTHVLTOT) ~ NA_character_,
      UTHVLTOT > RefThyroid_Volume ~ "Goitre",
      TRUE ~ "Normal"),
      levels = c("Normal", "Goitre")
    ))
    
  
## Generating table 3

table_3_JOD <- data_JOD %>% 
  dplyr::select(
    SEX, MJ, JODbas, JODexbas_24, JODexbas_24_cat, JODfu1, JODexfu1_24, JODexfu1_24_cat, 
    UTHVLTOT, Goitre_Gutekunst, Goitre_Ittermann
  ) %>%
  tbl_summary(
    by = SEX,
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",  
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 2,
      all_categorical() ~ c(0, 2)
    ),
    label = list(
      MJ ~ "Estimated Dietary Iodine Intake (Baseline) (µg/day)",
      JODbas     ~ "Spot Urinary Iodine Concentration (Baseline) (µg/L)",
      JODexbas_24     ~ "Estimated 24-hr Iodine Excretion (Baseline) (µg/day)",
      JODexbas_24_cat  ~ "Iodine Adequacy based on Estimated 24-hr Iodine Excretion at Baseline",
      JODfu1        ~ "Spot Urinary Iodine Concentration (Follow-Up) (µg/L)",
      JODexfu1_24      ~ "Estimated 24-hr Iodine Excretion (Follow-Up) (µg/day)",
      JODexfu1_24_cat    ~ "Iodine Adequacy based on Estimated 24-hr Iodine Excretion at Follow-up",
      UTHVLTOT    ~ "Total Thyroid Volume (mL) (Follow-up)",
      Goitre_Gutekunst  ~ "Thyroid volume classified according to Gutekunst et al. (WHO)",
      Goitre_Ittermann  ~ "Thyroid volume classified according to Ittermann et al."
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"   
    )
  ) %>%                                 
  bold_labels() %>%
  modify_header(
    label   ~ "**Characteristics**",
    stat_0  ~ "**Total**",
    stat_1  ~ "**Male**",
    stat_2  ~ "**Female**",
    n       ~ "**N**",
    p.value ~ "**p-Value**"
  ) %>%
  modify_caption(
    "**Table 3: Measures of iodine status and total thyroid volume of the study participants, stratified by sex**"
  ) %>%
  modify_footnote(
    c(stat_0, stat_1, stat_2) ~ "Median (Quartile 1, Quartile 3) reported for continuous variables; n (%) for categorical variables. N reflects non-missing observations per variable.",
    p.value                   ~ "Mann-Whitney U test for continuous variables; Chi-square test for categorical variables."
  )

## Printing table 3

table_3_JOD



-------------------------------------------------------------------
-------------------------------------------------------------------
------------------------------------------------------------------
-------------------------------------------------------------------


## Phase III: Correlation and Regression Modelling ----
## Unadjusted
# Link of Dietary intake and measures of iodine intake
# 
# two extereme outliers in the JODbas
#  - values: 2689 and 1224
#  - I am going to exclude them from the analysis because they have not taken any kind of supplementation
#   and it is biologially implausible to observe this value.


data_r1 <- data

### Removing very unrealistic (outliers) from JODbas ----

data_r1 <- data_r1 %>% 
  filter(JODbas  <= 1000)


## Removing the participants who are using Iodine medicine (cm_thyr_h03c == 1)

table(data_r1$cm_thyr_h03c)

data_r1 <- data_r1 %>% filter(is.na(cm_thyr_h03c))


-------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------

## Correlation between UIC and Food Intake ----

#### 1. Calculating Spearman's Rank correlation ----
 
corr_table <- data_r1 %>%
  dplyr::select(
    fleisch, Fleisch__und_Wurstwaren, Fisch_und_Fischwaren, Eier,
    Milch_und_Milchprodukte, Cheese_subgroup,
     Butter, speisefette, Brot_und_Backwaren, Bread_subgroup, 
    naehrmittel, Vollkornprodukte, Kartoffeln, gemuese,
    huelsenfruechte, obst, nuesse
  ) %>%
  purrr::map_dfr(~ {
    test <- cor.test(data_r1$JODexbas_24, .x,
                     method = "spearman",
                     exact  = FALSE)
    data.frame(
      N = sum(!is.na(.x) & !is.na(data_r1$JODexbas_24)),
      Spearman_r = round(test$estimate, 3),
      p_value    = paste0(
        round(test$p.value, 3),
        case_when(
          test$p.value < 0.001 ~ "***",
          test$p.value < 0.01  ~ "**",
          test$p.value < 0.05  ~ "*",
          TRUE                 ~ ""
        )
      )
    )
  }, .id = "Food_Group") %>%
  mutate(Food_Group = case_match(Food_Group,    
           "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
                                 
  )) %>%
  gt() %>%
  cols_label(
    Food_Group ~ "Food Group",
    N ~ "N",
    Spearman_r ~ "Spearman r",
    p_value    ~ "p-Value"
  ) %>%
  tab_header(
    title = md("**Spearman Correlation Between Habitual Daily Food Intake and Estimated 24-hr Urinary Iodine Excretion**")
  ) %>%
  tab_footnote(
    footnote  = "*** p<0.001, ** p<0.01, * p<0.05",
    locations = cells_column_labels(columns = p_value)
    
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) 


## Printing Corrleation Table


corr_table


####  2. Pearson Correlation with Log 24hr UIC ----

corr_table_pear <- data_r1 %>%
  dplyr::select(
    fleisch, Fleisch__und_Wurstwaren,
    Fisch_und_Fischwaren, Eier,
    Milch_und_Milchprodukte, Cheese_subgroup,
    Butter, speisefette, Brot_und_Backwaren,
    Bread_subgroup, 
    naehrmittel, Vollkornprodukte,
    Kartoffeln, gemuese,
    huelsenfruechte, obst, nuesse
  ) %>%
  purrr::map_dfr(~ {
    test <- cor.test(log(data_r1$JODexbas_24), .x,
                     method = "pearson",
                     exact  = FALSE)
    data.frame(
      N = sum(!is.na(.x) & !is.na(data_r1$JODexbas_24)),
      Spearman_r = round(test$estimate, 3),
      p_value    = paste0(
        round(test$p.value, 3),
        case_when(
          test$p.value < 0.001 ~ "***",
          test$p.value < 0.01  ~ "**",
          test$p.value < 0.05  ~ "*",
          TRUE                 ~ ""
        )
      )
    )
  }, .id = "Food_Group") %>%
  mutate(Food_Group = case_match(Food_Group,     
         "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
  )) %>%
  gt() %>%
  cols_label(
    Food_Group ~ "Food Group",
    Spearman_r ~ "Pearson r",
    p_value    ~ "p-Value"
  ) %>%
  tab_header(
    title = md("**Pearson Correlation Between Habitual Food Intake and log of Estimate 24 hr Urinary Iodine Excretion**")
  ) %>%
  tab_footnote(
    footnote  = "*** p<0.001, ** p<0.01, * p<0.05",
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) 

## Priting pearson correlation table (Table 4)

corr_table_pear


-------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------

## Regression modelling. -----


### Covariate (Sex, Age, Education Status, BMI, Smoking Habit, Physical Activity Level, Thyroid Disorder, GCAL) 
## Fully Adjusted Regression model of predicted 24-hr JOD baseline excretion and diet only ----

 
food_vars <- c("fleisch", "Fleisch__und_Wurstwaren",
               "Fisch_und_Fischwaren", "Eier",
               "Milch_und_Milchprodukte", "Cheese_subgroup",
               "Butter", "speisefette", "Brot_und_Backwaren",
               "Bread_subgroup", 
               "naehrmittel", "Vollkornprodukte",
               "Kartoffeln", "gemuese",
               "huelsenfruechte", "obst", "nuesse")

models_adjusted <- purrr::map_dfr(food_vars, function(food) {
  
  formula <- as.formula(paste(
    "log(JODexbas_24) ~", food,  
    "+ SEX + AGE + BMI",
    "+ SMOKER",
    "+ UTHYRDISX_cat",
    "+ PATOTC_cat",
    "+ isced_cat + GCAL"
  ))
  
  model    <- lm(formula, data = data_r1)
  coef_df  <- broom::tidy(model, conf.int = TRUE)
  food_row <- coef_df %>% filter(term == food)
  
  data.frame(
    Food_Group = food,
    n          = nobs(model),
    Beta       = round(exp(food_row$estimate * 100), 5),    
    CI_lower   = round(exp(food_row$conf.low  * 100), 5),   
    CI_upper   = round(exp(food_row$conf.high * 100), 5),   
    p_value    = paste0(
      round(food_row$p.value, 3),
      case_when(
        food_row$p.value < 0.001 ~ "***",
        food_row$p.value < 0.01  ~ "**",
        food_row$p.value < 0.05  ~ "*",
        TRUE                     ~ ""
      )
    )
  )
}) %>%
  mutate(
    CI         = paste0("(", CI_lower, ", ", CI_upper, ")"),
    Food_Group = case_match(Food_Group,
           "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
    )
  )

# Save as table 
models_adjusted_table <- models_adjusted %>%
  dplyr::select(Food_Group, n, Beta, CI, p_value) %>%
  gt() %>%
  cols_label(
    Food_Group ~ "Food Group (g/day)",
    n          ~ "N",
    Beta       ~ "Estimate (exponentiated β)",
    CI         ~ "95% CI",
    p_value    ~ "p-Value"
  ) %>%
  tab_header(
    title    = md("**Table: Fully adjusted Regression models between Habitual Food Group Intake (exposure)and log-transformed Estimated 24-hr Urinary Iodine Excretion (outcome)**"),
    subtitle = md("*Separate adjusted models per food group*")
  ) %>%
  tab_footnote(
    footnote  = "Exponentiated β coefficients and 95% CI represent the multiplicative change in estimated 24-hr iodine excretion per 100 g/day increase in food group intake. Each row represents a separate covariate-adjusted model adjusted for sex, age, BMI, smoking status, thyroid disorder, physical activity level, education level, and total energy intake (GCAL). *** p<0.001, ** p<0.01, * p<0.05",  
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

## Print adjusted model table (Table 5)
models_adjusted_table



----------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------

## Minimally Adjusted (sex, age, gcal) Regression model of predicted 24-hr JOD baseline excretion and diet only ----

food_vars <- c("fleisch", "Fleisch__und_Wurstwaren",
               "Fisch_und_Fischwaren", "Eier",
               "Milch_und_Milchprodukte", "Cheese_subgroup",
               "Butter", "speisefette", "Brot_und_Backwaren",
               "Bread_subgroup", 
               "naehrmittel", "Vollkornprodukte",
               "Kartoffeln", "gemuese",
               "huelsenfruechte", "obst", "nuesse")


models_adjusted_minimally <- purrr::map_dfr(food_vars, function(food) {
  
  formula <- as.formula(paste(
    "log(JODexbas_24) ~", food,
    "+ SEX + AGE + GCAL"
  ))
  
  model    <- lm(formula, data = data_r1)
  coef_df  <- broom::tidy(model, conf.int = TRUE)
  food_row <- coef_df %>% filter(term == food)
  
  data.frame(
    Food_Group = food,
    n          = nobs(model),
    Beta       = round(exp(food_row$estimate * 100 ), 5),
    CI_lower   = round(exp(food_row$conf.low * 100), 5),
    CI_upper   = round(exp(food_row$conf.high * 100), 5),
    p_value    = paste0(                      
      round(food_row$p.value, 3),
      case_when(
        food_row$p.value < 0.001 ~ "***",
        food_row$p.value < 0.01  ~ "**",
        food_row$p.value < 0.05  ~ "*",
        TRUE                     ~ ""          
      )
    )
  )
}) %>%
  mutate(
    CI         = paste0("(", CI_lower, ", ", CI_upper, ")"),
    Food_Group = case_match(Food_Group,
          "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
    ))


print(models_adjusted_minimally)



# Saving as table 

models_adjusted_minimally_table <- models_adjusted_minimally %>%
  dplyr::select(Food_Group, n, Beta, CI, p_value) %>%
  gt() %>%
  cols_label(
    Food_Group ~ "Food Group (g/day)",
    n          ~ "N",
    Beta       ~ "Estimate (exponentiated β)",
    CI         ~ "95% CI",
    p_value    ~ "p-Value"
  ) %>%
  tab_header(
    title    = md("**Table: Minimally-adjusted regression models between habitual food group intake (exposure) and log-transformed estimated 24-hr urinary iodine excretion (outcome)**"),
    subtitle = md("*Separate adjusted models per food group*")
  ) %>%
  tab_footnote(
    footnote  = "Exponentiated β coefficients and 95% CI represent the multiplicative change in estimated 24-hr iodine excretion per 100 g/day increase in food group intake. Each row represents a separate covariate-adjusted model adjusted for sex, age, and total energy intake (GCAL). *** p<0.001, ** p<0.01, * p<0.05 ",
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

models_adjusted_minimally_table ## Table 5

------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  

## Sensitivity analyses (without any thyroid disorders) ----
 
 food_vars <- c("fleisch", "Fleisch__und_Wurstwaren",
                "Fisch_und_Fischwaren", "Eier",
                "Milch_und_Milchprodukte", "Cheese_subgroup",
                "Butter", "speisefette", "Brot_und_Backwaren",
                "Bread_subgroup", 
                "naehrmittel", "Vollkornprodukte",
                "Kartoffeln", "gemuese",
                "huelsenfruechte", "obst", "nuesse")
 
 data_sensitivity <- data_r1 %>% filter(UTHYRDISX_cat == "No")
 


 models_sensitivity <- purrr::map_dfr(food_vars, function(food) {
   
   formula <- as.formula(paste(
     "log(JODexbas_24) ~", food,
     "+ SEX + AGE + BMI + GCAL"
   ))
   
   model    <- lm(formula, data = data_sensitivity)
   coef_df  <- broom::tidy(model, conf.int = TRUE)
   food_row <- coef_df %>% filter(term == food)
   
   data.frame(
     Food_Group = food,
     n          = nobs(model),
     Beta       = round(exp(food_row$estimate * 100 ), 5),
     CI_lower   = round(exp(food_row$conf.low * 100), 5),
     CI_upper   = round(exp(food_row$conf.high * 100), 5),
     p_value    = paste0(                      
       round(food_row$p.value, 3),
       case_when(
         food_row$p.value < 0.001 ~ "***",
         food_row$p.value < 0.01  ~ "**",
         food_row$p.value < 0.05  ~ "*",
         TRUE                     ~ ""          
       )
     )
   )
 }) %>%
   mutate(
     CI         = paste0("(", CI_lower, ", ", CI_upper, ")"),
     Food_Group = case_match(Food_Group,
                             "fleisch"                  ~ "Fresh Meat",
                             "Fleisch__und_Wurstwaren"  ~ "Processed Meat",
                             "Fisch_und_Fischwaren"     ~ "Fish and Fish Products",
                             "Eier"                     ~ "Eggs",
                             "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products",
                             "Cheese_subgroup"          ~ "Cheese (Subgroup)",
                             "Butter"                   ~ "Butter",
                             "speisefette"              ~ "Other Edible Fats/Oils",
                             "Brot_und_Backwaren"       ~ "Bread and Bakery Products",
                             "Bread_subgroup"           ~ "Bread (Subgroup)",
                             "naehrmittel"              ~ "Staple Food",
                             "Vollkornprodukte"         ~ "Whole Grain Products",
                             "Kartoffeln"               ~ "Potatoes",
                             "gemuese"                  ~ "Vegetables",
                             "huelsenfruechte"          ~ "Legumes",
                             "obst"                     ~ "Fruits",
                             "nuesse"                   ~ "Nuts"
     ))
 
 
 print(models_sensitivity)
 
 
 
 # Saving as table 
 
 models_sensitivity_table <- models_sensitivity %>%
   dplyr::select(Food_Group, n, Beta, CI, p_value) %>%
   gt() %>%
   cols_label(
     Food_Group ~ "Food Group (g/day)",
     n          ~ "N",
     Beta       ~ "Estimate (exponentiated β)",
     CI         ~ "95% CI",
     p_value    ~ "p-Value"
   ) %>%
   tab_header(
     title    = md("**Table: Regression models between habitual food group intake (exposure) and log-transformed estimated 24-hr urinary iodine excretion (outcome) among participants without any thyroid disorders**"),
     subtitle = md("*Separate adjusted models per food group*")
   ) %>%
   tab_footnote(
     footnote  = "Exponentiated β coefficients and 95% CI represent the multiplicative change in estimated 24-hr iodine excretion per 100 g/day increase in food group intake. Each row represents a separate covariate-adjusted model adjusted for sex, age, and total energy intake (GCAL). *** p<0.001, ** p<0.01, * p<0.05 ",
     locations = cells_column_labels(columns = p_value)
   ) %>%
   tab_style(
     style     = cell_text(weight = "bold"),
     locations = cells_column_labels()
   )
 
 models_sensitivity_table
 
 
 ## Saving as docx file
 
 models_sensitivity_table %>% gt::gtsave("SensitivityAnalysis.docx")


 
 ## (Supplementary Analysis)
## Model by mutual adjustment of food group  ----

model_mutually_adjusted <- lm(
  log(JODexbas_24) ~ fleisch + Fleisch__und_Wurstwaren +
    Fisch_und_Fischwaren + Eier +
    Milch_und_Milchprodukte + Cheese_subgroup +
    Butter + speisefette + Brot_und_Backwaren +
    Bread_subgroup +
    naehrmittel + Vollkornprodukte +
    Kartoffeln + gemuese +
    huelsenfruechte + obst + nuesse +
    GCAL + SEX + AGE,
  data = data_r1
)

ma_summary        <- summary(model_mutually_adjusted)
ma_coef_matrix    <- ma_summary$coefficients         
ma_confint_matrix <- confint(model_mutually_adjusted)

ma_results <- data.frame(
  Food_group = character(),
  N          = integer(),
  Beta       = numeric(),
  CI_Lower   = numeric(),
  CI_Upper   = numeric(),          
  P_Value    = character(),
  stringsAsFactors = FALSE
)

food_vars <- c(
  "fleisch", "Fleisch__und_Wurstwaren",
  "Fisch_und_Fischwaren", "Eier",
  "Milch_und_Milchprodukte", "Cheese_subgroup",
  "Butter", "speisefette", "Brot_und_Backwaren",
  "Bread_subgroup",
  "naehrmittel", "Vollkornprodukte",
  "Kartoffeln", "gemuese",
  "huelsenfruechte", "obst", "nuesse"
)

for (food in food_vars) {
  
  beta     <- ma_coef_matrix[food, "Estimate"]    
  CI_lower <- ma_confint_matrix[food, "2.5 %"]
  CI_upper <- ma_confint_matrix[food, "97.5 %"]
  P_value  <- ma_coef_matrix[food, "Pr(>|t|)"]  
  
  one_row <- data.frame(
    Food_group = food,
    N          = nobs(model_mutually_adjusted),
    Beta       = round(exp(beta     * 100), 5),
    CI_Lower   = round(exp(CI_lower * 100), 5),
    CI_Upper   = round(exp(CI_upper * 100), 5),   
    P_Value    = paste0(
      round(P_value, 3),
      ifelse(P_value < 0.001, "***",
             ifelse(P_value < 0.01,  "**",
                    ifelse(P_value < 0.05, "*", "")))
    ),
    stringsAsFactors = FALSE
  )
  
  ma_results <- rbind(ma_results, one_row)
}

print(ma_results)


## Saving in table formate

ma_table <- ma_results %>%
  mutate(
    CI         = paste0("(", CI_Lower, ", ", CI_Upper, ")"),
    Food_group = case_match(Food_group,
         "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
    )
  ) %>%
  dplyr::select(Food_group, N, Beta, CI, P_Value) %>%
  gt() %>%
  cols_label(
    Food_group ~ "Food Group",
    N          ~ "N",
    Beta       ~ "Estimate (exponentiated β) per 100 g/day",
    CI         ~ "95% CI",
    P_Value    ~ "p-Value"
  ) %>%
  tab_header(
    title    = md("**Table: Mutually Adjusted Regression Model — Habitual Food Group Intake and log-transformed Estimated 24-hr Urinary Iodine Excretion**"),
  ) %>%
  tab_footnote(
    footnote  = "Exponentiated β coefficients and 95% CI represent represents the multiplicative change in estimated 24-hr urinary iodine excretion per 100 g/day increase in food group intake. All food groups were entered simultaneously in one model (mutual adjustment), additionally adjusted for sex, age, and total energy intake (GCAL). *** p<0.001, ** p<0.01, * p<0.05",
    locations = cells_column_labels(columns = P_Value)
  ) %>% 
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

ma_table

 
## Iodide Intake ---- 

### Correlation with iodine excretion ----
cor.test(log(data$JODexbas_24), log(data$MJ),
         method = "pearson",
         exact = F)


cor.test(log(data$JODbas), log(data$MJ),
         method = "pearson",
         exact = F)



### Regression model with iodine intake (derived from the German Food Composition Table (BLS), v3.02) ----


food_vars <- c("fleisch", "Fleisch__und_Wurstwaren",
               "Fisch_und_Fischwaren", "Eier",
               "Milch_und_Milchprodukte", "Cheese_subgroup",
               "Butter", "speisefette", "Brot_und_Backwaren",
               "Bread_subgroup", 
               "naehrmittel", "Vollkornprodukte",
               "Kartoffeln", "gemuese",
               "huelsenfruechte", "obst", "nuesse")


models_MJ <- purrr::map_dfr(food_vars, function(food) {
  
  formula <- as.formula(paste(
    "log(MJ) ~", food,
    "+ SEX + AGE + GCAL"
  ))
  
  model    <- lm(formula, data = data)
  coef_df  <- broom::tidy(model, conf.int = TRUE)
  food_row <- coef_df %>% filter(term == food)
  
  data.frame(
    Food_Group = food,
    n          = nobs(model),
    Beta       = round(exp(food_row$estimate * 100 ), 5),
    CI_lower   = round(exp(food_row$conf.low * 100), 5),
    CI_upper   = round(exp(food_row$conf.high * 100), 5),
    p_value    = paste0(                      
      round(food_row$p.value, 3),
      case_when(
        food_row$p.value < 0.001 ~ "***",
        food_row$p.value < 0.01  ~ "**",
        food_row$p.value < 0.05  ~ "*",
        TRUE                     ~ ""          
      )
    )
  )
}) %>%
  mutate(
    CI         = paste0("(", CI_lower, ", ", CI_upper, ")"),
    Food_Group = case_match(Food_Group,
         "fleisch"        ~ "Fresh Meat (g/day)",
         "Fleisch__und_Wurstwaren"  ~ "Processed Meat (g/day)",
         "Fisch_und_Fischwaren"     ~ "Fish and Fish Products (g/day)",
         "Eier"            ~ "Eggs (g/day)",
        "Milch_und_Milchprodukte"  ~ "Milk and Dairy Products (g/day)",
        "Cheese_subgroup"      ~ "Cheese (Subgroup) (g/day)",
        "Butter"         ~ "Butter (g/day)",
         "speisefette"         ~ "Other Edible Fats/Oils (g/day)",
         "Brot_und_Backwaren"     ~ "Bread and Bakery Products (g/day)",
         "Bread_subgroup"        ~ "Bread (Subgroup) (g/day)",
         "naehrmittel"          ~ "Staple Food (g/day)",
          "Vollkornprodukte"      ~ "Whole Grain Products (g/day)",
          "Kartoffeln"            ~ "Potatoes (g/day)",
           "gemuese"             ~ "Vegetables (g/day)",
          "huelsenfruechte"       ~ "Legumes (g/day)",
            "obst"                 ~ "Fruits (g/day)",
            "nuesse"           ~ "Nuts (g/day)"
    ))


print(models_MJ)

## Saving as table format

models_MJ_table <- models_MJ %>%
  dplyr::select(Food_Group, n, Beta, CI, p_value) %>%
  gt() %>%
  cols_label(
    Food_Group ~ "Food Group (g/day)",
    n          ~ "N",
    Beta       ~ "Estimate (exponentiated β)",
    CI         ~ "95% CI",
    p_value    ~ "p-Value"
  ) %>%
  tab_header(
    title    = md("**Table: Covariates adjusted regression models between habitual food group intake (exposure) and log-transformed daily iodine intake (µg) derived from German Food Composition Table (BLS), v3.02 (outcome)**"),
    subtitle = md("*Separate adjusted models per food group*")
  ) %>%
  tab_footnote(
    footnote  = "Exponentiated β coefficients and 95% CI represent the multiplicative change in dietary iodine intke per 100 g/day increase in food group intake. Each row represents a separate covariate-adjusted model adjusted for sex, age, and total energy intake (GCAL). *** p<0.001, ** p<0.01, * p<0.05 ",
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

models_MJ_table


################### END OF ANALYSIS #######################
```
