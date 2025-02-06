
#====================================================================================================================================================#
# notes: generate group specific results from varying models
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


#=========================================================#
# ==== define regression formula and have the results ====
#=========================================================#

gender_set <- c("female", "male")
race_set <- c("white","black", "hispanic","asian", "other")
ell_set <- c("ell", "nonell")
frl_set <- c("frl","nonfrl")
swd_set <- c("swd", "nonswd")

group_list <- list(gender_set, race_set, ell_set, frl_set, swd_set)
demo_set <- c("gender", "race", "ell", "frl","swd")
names(group_list) <- demo_set


for (p_subject in subject_set){
  for (p_demo in demo_set){

    sel_group <-  grep(paste0("^(gm|sm|se|sa)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    math_group <-  grep(paste0("^(math)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    ela_group <-  grep(paste0("^(ela)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    sel_high_order <- "gm2 + gm3 + sm2 + sm3 + se2 + se3 + sa2 + sa3"


    z_sel_group <-  grep(paste0("^(z_gm|z_sm|z_se|z_sa)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    z_math_group <-  grep(paste0("^(z_math)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    z_ela_group <-  grep(paste0("^(z_ela)", "_(", paste(group_list[[p_demo]], collapse = "|"), ")$"), colnames(main_data), value = T)
    z_sel_high_order <- "z_gm2 + z_gm3 + z_sm2 + z_sm3 + z_se2 + z_se3 + z_sa2 + z_sa3"
    
    
    group_formula_main <- as.formula(paste0(p_subject, "_post ~", p_subject, "+ ", demo_control, " + ", paste(z_sel_group, collapse = "+")))
    group_formula_fe <- as.formula(paste0(p_subject, "_post ~", p_subject, "+ ", demo_control, " + school_code + ", paste(z_sel_group, collapse = "+")))
    group_formula_hipoly1 <- as.formula(paste0(p_subject, "_post ~", p_subject,"+",p_subject,"2 +", p_subject, "3  + ", demo_control, " +", paste(z_sel_group, collapse = "+")))
    group_formula_hipoly2 <- as.formula(paste0(p_subject, "_post ~", p_subject,"+",p_subject,"2 +", p_subject, "3  + ", demo_control, " +",z_sel_high_order," + ", paste(z_sel_group, collapse = "+")))
    group_formula_testinter <- as.formula(paste0(p_subject, "_post ~", demo_control, " + ", paste(z_sel_group, collapse = "+"), "+", 
                                                 paste(get(paste0(p_subject, "_group")), collapse = "+" )))
    
    group_formula_list <- list(group_formula_main, group_formula_fe, group_formula_hipoly1, group_formula_hipoly2, group_formula_testinter)
    
    
    model_set <- c("main", "fe", "hipoly1", "hipoly2", "testinter")
    names(group_formula_list) <- model_set
    
    for (p_model in model_set){
      a_1 <- lm(group_formula_list[[p_model]], data = main_data[grade==grade_set[1],] )
      a_2 <- lm(group_formula_list[[p_model]], data = main_data[grade==grade_set[2],] )
      a_3 <- lm(group_formula_list[[p_model]], data = main_data[grade==grade_set[3],] )
      reg_list <- list(a_1, a_2, a_3)
      
      for (i in 1:3){
        coef <- as.data.table(summary(reg_list[[i]])$coef, keep.rownames = T)
        write.xlsx(coef, file = paste0(p_dir_out, p_subject, "_", p_demo, "_", p_model, ".xlsx"), sheetName = paste0("set_", i),row.names = F, append = T, showNA = F)
      }
    }
  }
}





