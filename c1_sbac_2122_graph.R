
#====================================================================================================================================================#
# notes: plots graphs for the average impacts from varying models
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
library(RColorBrewer)
#========================#
# ==== p2: set parms ====
#========================#

# set export toggle
p_opt_exp <- FALSE

# set timestamp
p_timestamp <- ea_timestamp()

# set output directories
p_dir_in <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/excel/sbac_2122/"
p_dir_out <- "/projects/core/ies_sel_grant/year_1_2324/study_5/researcher/shu_study5/protective_factor/ggplot/sbac_2122/"


# other key variables
data_set <- "sbac_2122_1819"
grade_set <- c("04", "05", "08")

model_set <- c("nocov", "cov", "schlfe", "hipoly")
subject_set  <- c("z_math", "z_ela")

#========================#
# ==== p3: load data ====
#========================#
grade_list <- list("4", "5", "8")

for (p_subject in subject_set){
in_data_model <- lapply(model_set, function(p_model){
in_data_set <- lapply(1:3, function(i){
  in_data<- as.data.table(read.xlsx(paste0(p_dir_in, p_subject, "_", p_model, ".xlsx"), sheetName = paste0("set_", i)))
  colnames(in_data) <- c("var", "est", "sd","t", "p")
  in_data[, grade := grade_list[[i]]]
  return(in_data)
})
in_data <- rbindlist(in_data_set)
return(in_data)
})
names(in_data_model) <- model_set
in_data <- rbindlist(in_data_model, idcol = "model")
main_data <- copy(in_data)


#=========================================#
# ==== prepare the data for the plots ====
#=========================================#

# main_data <- main_data[grepl("gm|sm|se|sa", main_data$var),]
main_data <- main_data[grepl("z_gm|z_sm|z_se|z_sa", main_data$var),]
# grepl returns a logic vector
# grep returns the matches
main_data[, construct := var]
main_data[, group := model]
main_data[, construct := factor(construct, levels = c("z_gm", "z_sm","z_se", "z_sa"))]

main_data[ p > 0.1, star := ""]
main_data[ p <= 0.1, star := "*"]

main_data[ p <= 0.05, star := "**"]
main_data[ p <= 0.01, star := "***"]

main_data[, est := round(est,3)]
main_data[, est_star := paste(est,star)]

#============================#
# ==== generate the plot ====
#============================#
main_data[, group := factor(group, levels = c("nocov","cov","hipoly","schlfe"))]

palette <- brewer.pal(5, "YlGnBu")
palette <- palette[-1]

est_group <- ggplot(main_data, aes(grade, est, fill = group)) + geom_bar(stat = "identity", position = "dodge") + 
  facet_wrap(~ construct) +  geom_text(aes(label = est_star), vjust= -1, size = 1.2, position =  position_dodge(width = 0.9)) +
  scale_fill_manual(values = palette) +
  theme(legend.position = "bottom")

ggsave(paste0(p_dir_out, p_subject, "_main.png"), est_group, width = 7, height = 5)
}










