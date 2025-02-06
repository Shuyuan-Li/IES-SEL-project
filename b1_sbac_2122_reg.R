
#====================================================================================================================================================#
# notes: regression results with no interaction terms, different models
#====================================================================================================================================================#

#==================================================#
# ==== p1: load packages and clear objects/log ====
#==================================================#

# load easimple and clear objects/log
library(easimple)
ea_start()

# load other packages
library(data.table)
library(xlsx)
library(psych)
library(ggplot2)
library(stargazer)
#========================#
# ==== p2: set parms ====
#========================#

# set export toggle
p_opt_exp <- FALSE

# set timestamp
p_timestamp <- ea_timestamp()

# set output directories
p_dir_in <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/00_input_sets/most_recent/"
p_dir_out <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/excel/sbac_2122/"


# other key parameters
data_set <- "sbac_2122_1819"
year_set <- c("2122", "1819")
grade_set <- c("04", "05", "08")

subject_set <- c("z_math", "z_ela")
z_sel_control <- "z_gm + z_sm + z_se + z_sa"
demo_control <- "gender + race + ell + frl + swd"

#========================#
# ==== p3: load data ====
#========================#

in_data <- ea_load(paste0(p_dir_in, paste0("protective_", data_set, ".rdata")))
main_data <- copy(in_data)

#=============================================#
# ==== generate regression results
#=============================================#

for (p_subject in subject_set){
  
  #=============================================#
  # ==== regression, no cov ====
  #=============================================#
  formula_nocov <- as.formula(paste0(p_subject, "_post ~ ", p_subject, "+", z_sel_control))
  
  a_1 <- lm(formula_nocov,  data = main_data[grade== grade_set[1],] )
  a_2 <- lm(formula_nocov,  data = main_data[grade== grade_set[2],] )
  a_3 <- lm(formula_nocov,  data = main_data[grade== grade_set[3],] )

  model_sets <- list(a_1, a_2, a_3)
  i <- 0
  for (p_model in model_sets){
    i <- i+1
    coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
    write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_nocov.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
  }
  
  #=====================================#
  # ==== regression, with cov ====
  #=====================================#
  
  formula_cov <- as.formula(paste0(p_subject, "_post ~ ", p_subject, "+", z_sel_control, "+", demo_control ))
  a_1 <- lm(formula_cov,  data = main_data[grade==grade_set[1],] )
  a_2 <- lm(formula_cov,  data = main_data[grade==grade_set[2],] )
  a_3 <- lm(formula_cov,  data = main_data[grade==grade_set[3],] )
  
  
  model_sets <- list(a_1, a_2, a_3)
  i <- 0
  for (p_model in model_sets){
    i <- i+1
    coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
    write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_cov.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
  }
  
  
  #===========================================#
  # ==== regression, school fixed effects ====
  #===========================================#
  
  formula_fe <- as.formula(paste0(p_subject, "_post ~ ", p_subject, "+", z_sel_control, "+", demo_control, "+ school_code"))
  a_1 <- lm(formula_fe,  data = main_data[grade==grade_set[1],] )
  a_2 <- lm(formula_fe,  data = main_data[grade==grade_set[2],] )
  a_3 <- lm(formula_fe,  data = main_data[grade==grade_set[3],] )
  
  
  model_sets <- list(a_1, a_2, a_3)
  i <- 0
  for (p_model in model_sets){
    i <- i+1
    coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
    write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_schlfe.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
  }
  
  
  
  #=======================================================#
  # ==== regression, high order polynomial on test scores ====
  #=======================================================#
  
  formula_hipoly <- as.formula(paste0(p_subject, "_post ~ ", p_subject, "+", p_subject, "2 + ", p_subject, "3 +", z_sel_control, "+", demo_control ))
  a_1 <- lm(formula_hipoly,  data = main_data[grade==grade_set[1],] )
  a_2 <- lm(formula_hipoly,  data = main_data[grade==grade_set[2],] )
  a_3 <- lm(formula_hipoly,  data = main_data[grade==grade_set[3],] )
  
  
  model_sets <- list(a_1, a_2, a_3)
  i <- 0
  for (p_model in model_sets){
    i <- i+1
    coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
    write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_hipoly.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
  }
}
