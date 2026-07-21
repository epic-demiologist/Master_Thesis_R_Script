# MEIA Data Preprocessing (Master Thesis)

**Author:** Bikram Kumar Singh
**Date:** 2026-07-21

```r
library(dplyr)
library(tidyverse)
library(gtsummary)
library(knitr)
library(officer)
library(haven)

## Importing Raw dataset
data <- read_sav("/Users/......./meia_data.sav")

## Printing names of all variables/columns and their respective indices
column_info <- data.frame(
  index = 1:ncol(data),
  name = names(data)
)
print(column_info)

## I am removing unnecessary columns.
#colnames(data_new)
# data__ <- data_new %>% select(-c(20:41, 43:69, 95:100, 109:115, 144:365))
## Printing the name of all variables available in new dataframe.
column_info_sub <- data.frame(
  index = 1:ncol(data),
  name = names(data)
)
print(column_info_sub)

## Exploring the dataset
head(data)
summary(data)
str(data)
ncol(data)
nrow(data)
colnames(data)
dim(data)
sum(is.na(data))
summary(data)

## Exploring variables (Categorical variable)
summary(data$SEX)
table(data$SEX)
prop.table(table(data$SEX))
table(data$agec)
prop.table(table(data$agec))
table(data$ethnic)
prop.table(table(data$ethnic))

## Testing normality of a quantitative variable
summary(data$AGE) ## Age is not normally distributed
hist(data$AGE)
qqnorm(data$AGE)
qqline(data$AGE)
shapiro.test(data$AGE)
hist(data$JODbas)
qqnorm(data$JODbas)
qqline(data$JODbas)
shapiro.test(data$JODbas)

## Categorical Variables
## Data Engineering
# 1. I am making variable "UTHRDISX" (Do you have any thyroid disorder) di-cotomized "Yes" or "N0"

table(data$UTHYRDISX)
data <- data %>% mutate(
  UTHYRDISX_cat = case_when(
    UTHYRDISX == "" ~ "No",
    is.na(UTHYRDISX) ~ NA_character_,  # Keep NA as NA
    TRUE ~ "Yes"
  )
)
data$UTHYRDISX_cat <- factor(data$UTHYRDISX_cat, levels = c("Yes", "No"))
table(data$UTHYRDISX_cat)


## Making new varaible from "thyridis" which shows hypothyroidism
data <- data %>% mutate(
  hypothyroidism = case_when(thyrdis == 1 ~ "Yes",
                             TRUE ~ "No"))
data$hypothyroidism <- factor(data$hypothyroidism, levels = c("Yes", "No"))
table(data$hypothyroidism)


# 2. ethnic to ethnic_cat including two categories only
table(data$ethnic)
data <- data %>% mutate(
  ethnic_cat = case_when(
    ethnic == "Kaukasisch" ~ "Caucasian",
    is.na(ethnic) ~ NA_character_,
    TRUE ~ "Others"
    )
)
data$ethnic_cat <- as.factor(data$ethnic_cat)


# 3. agec has order of >=65 18-24 25-34 35-50 51-64. I want to rearrange it to acending order
data <- data %>%
  mutate(agec = factor(agec,
                       levels = c("18-24", "25-34", "35-50", "51-64", ">=65")))


# 4. Eudcation level: International Standard Classification of Education (ISCED) -

        #  1.	Grundschule = Primary School
        #  2.	 Haupt- und Realschule = Lower Secondary Education
        #  3. (Fach-)Hochschulreife  = Secondary Education
        #  4.Ausbildung = Vocational Training
        #  5. Fachschul- oder (Fach-)Hochschulabschluss = University Degree
        #  6.	Promotion  = PhD

table(data$isced)
data <- data %>% mutate(isced_cat =
  case_when(isced == "Grundschule" ~ "Primary Education",
            isced == "Haupt- und Realschule" ~ "Lower Secondary Education",
            isced == "(Fach-)Hochschulreife" ~ "Secondary Education",
            isced == "Ausbildung" ~ "Vocational Training",
            isced == "Fachschul- oder (Fach-)Hochschulabschluss" ~ "University Degree",
            isced == "Promotion" ~ "PhD Degree")
)
data$isced_cat <- factor(data$isced_cat, levels = c("Primary Education",
                                                    "Lower Secondary Education",
                                                    "Secondary Education",
                                                    "Vocational Training",
                                                    "University Degree",
                                                    "PhD Degree"))
table(data$isced_cat)



# 4.1 Education level into 3 categories
data <- data %>% mutate(
  isced_cat3 = case_when(
    isced_cat == "Primary Education" | isced_cat == "Lower Secondary Education" ~ "Primary/Lower Secondary Education",
    isced_cat == "Secondary Education" | isced_cat == "Vocational Training" ~ "Secondary/Vocational Training",
    isced_cat == "University Degree" | isced_cat == "PhD Degree" ~ "University Degree and Above"
  )
)
data$isced_cat3 <-  factor(data$isced_cat3, levels = c("Primary/Lower Secondary Education",
                                       "Secondary/Vocational Training",
                                       "University Degree and Above"))

# 5. PATOTC: Pysical Activity: EHIS-PAQ - PA (physical activity coefficient cat.) - Derived
# (Catgorized Low to High active)
## Lables
# Unknown = 10 (There are 10 unknown values)
# Active
# Low Active
# Sedentary
# Very Active
## I want to imput Unknown with Mode.

mode_PATOTC <- names(sort(table(data$PATOTC), decreasing = TRUE))[1]
table(data$PATOTC)
data <- data %>% mutate(
  PATOTC_cat = ifelse(PATOTC == "", mode_PATOTC, PATOTC))

data$PATOTC_cat <- factor(data$PATOTC_cat, levels = c("Sedentary",
                                                      "Low Active",
                                                      "Active",
                                                      "Very Active"))
table(data$PATOTC_cat)

## making other remaining categorical variables class to factors

data$SEX <- as.factor(data$SEX)

## Categorizing Alcohol Use Disorder - Total AUDIT-C score (WHO Based) into categories

table(data$ALKSCR)
data <- data %>%
  mutate(
    ALKSCR_cat = case_when(
      SEX == "M" & ALKSCR >= 0  & ALKSCR <= 3  ~ "Low Risk",
      SEX == "M" & ALKSCR >= 4  & ALKSCR <= 5  ~ "Moderate Risk",
      SEX == "M" & ALKSCR >= 6  & ALKSCR <= 7  ~ "High Risk",
      SEX == "M" & ALKSCR >= 8  & ALKSCR <= 12 ~ "Severe Risk",

      SEX == "F" & ALKSCR >= 0  & ALKSCR <= 2  ~ "Low Risk",
      SEX == "F" & ALKSCR >= 3  & ALKSCR <= 5  ~ "Moderate Risk",
      SEX == "F" & ALKSCR >= 6  & ALKSCR <= 7  ~ "High Risk",
      SEX == "F" & ALKSCR >= 8  & ALKSCR <= 12 ~ "Severe Risk",

      TRUE ~ NA_character_
    )
  )
# Imputing missing value with mode (Low Risk) (Missingness is below 2%)

data <- data %>%
  mutate(
    ALKSCR_cat = ifelse(is.na(ALKSCR_cat), "Low Risk", ALKSCR_cat)
  )
data$ALKSCR_cat <- factor(data$ALKSCR_cat, levels = c("Low Risk",
                                                      "Moderate Risk",
                                                      "High Risk",
                                                      "Severe Risk" ))
table(data$ALKSCR_cat)
sum(is.na(data$ALKSCR_cat))

## Categorizing and imputing VITAMYN: Any Vit, mineral, dietary supplements taken yesterday)

table(data$VITAMYN)
data <- data %>%
  mutate(
    VITAMYN_cat = case_when(
      VITAMYN == 0 ~ "No",
      VITAMYN == 1 ~ "Yes",
      is.na(VITAMYN) ~ "No" ## Mode = 0
    )
  )
data$VITAMYN_cat <- factor(data$VITAMYN_cat, levels = c("No",
                                                           "Yes"))

## Imputing missing valus in BMI with median
data <- data %>%
  mutate(
    BMI = ifelse(is.na(BMI),
                 median(BMI, na.rm = TRUE),
                 BMI)
  )
sum(is.na(data$BMI))

## Imputing missing values in WEIGHT with median

data <- data %>%
  mutate(
    WEIGHT = ifelse(is.na(WEIGHT),
                 median(WEIGHT, na.rm = TRUE),
                 WEIGHT)
  )

## Categorizing BMI according to WHO

data <- data %>%
  mutate(
    BMI_cat = case_when(
      BMI <  18.5 ~ "Underweight",
      BMI <= 24.9 ~ "Normal",
      BMI <= 29.9 ~ "Overweight",
      BMI >= 30   ~ "Obesity",
      TRUE        ~ NA_character_
    )
  )
data$BMI_cat <- factor(data$BMI_cat, levels = c("Underweight",
                                                "Normal",
                                                "Overweight",
                                                "Obesity"
))

## Ordering SMOKER Variable and remaining categories
data <- data %>%
  mutate(
    SMOKER = case_when(
      SMOKER == "CURRENT SMOKER"  ~ "Current Smoker",
      SMOKER == "NEVER SMOKER"    ~ "Never Smoker",
      SMOKER == "PREVIOUS SMOKER" ~ "Previous Smoker",
      TRUE                        ~ NA_character_
    ),
    SMOKER = factor(SMOKER,
                    levels = c("Never Smoker",
                               "Previous Smoker",
                               "Current Smoker"))
  )

# Verifying
table(data$SMOKER, useNA = "always")
levels(data$SMOKER)


## Arranging the levels of SEX
data <- data %>% mutate(
  SEX = factor(SEX, levels = c("M", "F"))
)

## Mutating and leveling the thyroid medication use
data <- data %>% mutate(
  thyroid_medication = factor(case_when(
    cm_thyr_h03c == 1 ~ "Yes",
    TRUE ~ "No"
  ), levels = c("Yes", "No"))
)

## Types of varaibles in my dataset now

categorical_vars <- c("agec", "SEX", "ethnic_cat", "SMOKER", "UTHYRDISX_cat",
                   "PATOTC_cat", "isced_cat","ALKSCR_cat", "VITAMYN_cat","BMI_cat",                      "thyroid_medication")


## Outcome Variables
## Engineering Major Outcome variables of the study
data <- data %>% mutate(
  i_cr_bas = ifelse(CREATbas == 0, NA, (JODbas / CREATbas)), # µg/g (i/Cr)
  i_cr_fu1 = ifelse(CREATfu1 == 0, NA, (JODfu1 / CREATfu1)),
  mean_JOD = case_when(
    is.na(JODbas) & is.na(JODfu1)   ~ NA_real_,
    is.na(JODfu1)                    ~ JODbas,
    is.na(JODbas)                    ~ JODfu1,
    !is.na(JODbas) & !is.na(JODfu1) ~ (JODbas + JODfu1) / 2),
  gender = case_when(SEX == "F" ~ 0,
                     SEX == "M" ~ 1),
  RefCreat_24 = exp(1)^(1.9539 + 0.1681 * gender - 0.0027 * AGE
                        + 0.0129 * WEIGHT - 0.0129 * BMI), # in mmol
  RefCreat_24 = RefCreat_24 * 0.11312, # Converting mmoL to g; Molecular weight of Creat is 0.113120 g/mmoL
  JODexbas_24 = i_cr_bas * RefCreat_24, # µg/day ## Estimated Iodine excretion per day
  JODexfu1_24 = i_cr_fu1 * RefCreat_24, # µg/day
  mean_JODex_24 = case_when(
    is.na(JODexbas_24) & is.na(JODexfu1_24)   ~ NA_real_,
    is.na(JODexfu1_24)                    ~ JODexbas_24,
    is.na(JODexbas_24)                    ~ JODexfu1_24,
    !is.na(JODexbas_24) & !is.na(JODexfu1_24) ~ (JODexbas_24 + JODexfu1_24) / 2),
  mean_JODex_24_cat = factor(case_when(
    mean_JODex_24 < 100 ~ "Iodine Deficiency (<100 µg/day)",
    mean_JODex_24 >= 100 &  mean_JODex_24 < 200 ~ "Adequate Iodine Intake (100–199 µg/day)",
    mean_JODex_24 >= 200 &  mean_JODex_24 < 300 ~ "More than Adequate (200–299 µg/day)",
    mean_JODex_24 >= 300 ~ "Excessive Intake (≥300 µg/day)"),
    levels = c("Iodine Deficiency (<100 µg/day)",
               "Adequate Iodine Intake (100–199 µg/day)",
               "More than Adequate (200–299 µg/day)",
               "Excessive Intake (≥300 µg/day)")

  ))

## Mutating two sub food group and three major food groups for the individual food items

data <- data_new %>% mutate(
  Milk_And_Milkproducts = gesmenge_16 + gesmenge_17 + gesmenge_19 + gesmenge_20,
  Cheese_subgroup = gesmenge_18 + gesmenge_21,
  Bread_subgroup = gesmenge_28 + gesmenge_29 + gesmenge_38,
  Bakery_Products = gesmenge_30,
  Whole_Grain_Products = gesmenge_36 + gesmenge_37 + gesmenge_39
)

## Saving this dataframe as MEIA_clean.rds instead of csv to working directory
saveRDS(data, "MEIA_clean.rds")
 ## write.csv(data, "MEIA_clean.csv", row.names = FALSE)

################ END OF DATA ENGINEERING ######################
```
