
#====================================================================================================================================================#
# notes: generate group specific regression results, main model
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
grade_set <- c("04", "05","08")

subject_set <- c("z_math", "z_ela")
demo_control <- "gender + race + ell + frl + swd"

#========================#
# ==== p3: load data ====
#========================#

in_data <- ea_load(paste0(p_dir_in, paste0("protective_", data_set, ".rdata")))
main_data <- copy(in_data)

#==========================================#
# ==== regression, gender interactions ====
#==========================================#

for (p_subject in subject_set){
sel_gender <-  grep("^(gm|sm|se|sa)_(female|male)$", colnames(main_data), value = T)
z_sel_gender <-  grep("^(z_gm|z_sm|z_se|z_sa)_(female|male)$", colnames(main_data), value = T)

gender_inter_formula <- as.formula(paste0(p_subject, "_post ~", p_subject, "+", demo_control, "+ ", paste(z_sel_gender, collapse = "+")))

a_1 <- lm(gender_inter_formula, data = main_data[grade==grade_set[1],] )
a_2 <- lm(gender_inter_formula, data = main_data[grade==grade_set[2],] )
a_3 <- lm(gender_inter_formula, data = main_data[grade==grade_set[3],] )

model_sets <- list(a_1, a_2, a_3)
i <- 0
for (p_model in model_sets){
   i <- i+1
  coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
  write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_gender.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
}


#========================================#
# ==== regression, race interactions ====
#========================================#
sel_race <-  grep("^(gm|sm|se|sa)_(white|black|hispanic|asian|other)$", colnames(main_data), value = T)
z_sel_race <-  grep("^(z_gm|z_sm|z_se|z_sa)_(white|black|hispanic|asian|other)$", colnames(main_data), value = T)

race_inter_formula <- as.formula(paste0(p_subject, "_post ~", p_subject, "+ ", demo_control," + ", paste(z_sel_race, collapse = "+")))
a_1 <- lm(race_inter_formula, data = main_data[grade==grade_set[1],] )
a_2 <- lm(race_inter_formula, data = main_data[grade==grade_set[2],] )
a_3 <- lm(race_inter_formula, data = main_data[grade==grade_set[3],] )


model_sets <- list(a_1, a_2, a_3)
i <- 0
for (p_model in model_sets){
   i <- i+1
  coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
  write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_race.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
}
  

#=======================================#
# ==== regression, swd interactions ====
#=======================================#

sel_swd <-  grep("^(gm|sm|se|sa)_(swd|nonswd)$", colnames(main_data), value = T)
z_sel_swd <-  grep("^(z_gm|z_sm|z_se|z_sa)_(swd|nonswd)$", colnames(main_data), value = T)
swd_inter_formula <- as.formula(paste0(p_subject, "_post ~", p_subject, "+ ", demo_control, " + ", paste(z_sel_swd, collapse = "+")))
a_1 <- lm(swd_inter_formula, data = main_data[grade==grade_set[1],] )
a_2 <- lm(swd_inter_formula, data = main_data[grade==grade_set[2],] )
a_3 <- lm(swd_inter_formula, data = main_data[grade==grade_set[3],] )

model_sets <- list(a_1, a_2, a_3)
i <- 0
for (p_model in model_sets){
  i <- i+1
  coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
  write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_swd.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
}


#=======================================#
# ==== regression, frl interactions ====
#=======================================#

sel_frl <-  grep("^(gm|sm|se|sa)_(frl|nonfrl)$", colnames(main_data), value = T)
z_sel_frl <-  grep("^(z_gm|z_sm|z_se|z_sa)_(frl|nonfrl)$", colnames(main_data), value = T)
frl_inter_formula <- as.formula(paste0(p_subject, "_post ~", p_subject, "+ ", demo_control, "+ ", paste(z_sel_frl, collapse = "+")))
a_1 <- lm(frl_inter_formula, data = main_data[grade==grade_set[1],] )
a_2 <- lm(frl_inter_formula, data = main_data[grade==grade_set[2],] )
a_3 <- lm(frl_inter_formula, data = main_data[grade==grade_set[3],] )

model_sets <- list(a_1, a_2, a_3)
i <- 0
for (p_model in model_sets){
  i <- i+1
  coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
  write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_frl.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
}


#=======================================#
# ==== regression, ell interactions ====
#=======================================#

sel_ell <-  grep("^(gm|sm|se|sa)_(ell|nonell)$", colnames(main_data), value = T)
z_sel_ell <-  grep("^(z_gm|z_sm|z_se|z_sa)_(ell|nonell)$", colnames(main_data), value = T)

ell_inter_formula <- as.formula(paste0(p_subject, "_post ~", p_subject, "+" , demo_control, " + ", paste(z_sel_ell, collapse = "+")))
a_1 <- lm(ell_inter_formula, data = main_data[grade==grade_set[1],] )
a_2 <- lm(ell_inter_formula, data = main_data[grade==grade_set[2],] )
a_3 <- lm(ell_inter_formula, data = main_data[grade==grade_set[3],] )

model_sets <- list(a_1, a_2, a_3)
i <- 0
for (p_model in model_sets){
  i <- i+1
  coef <- as.data.table(summary(p_model)$coef, keep.rownames = T)
  write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_ell.xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
}
}








