#Main Control Panel
library(shiny)
library(survminer)
library(survival)
#library(scatterplot3d)
library(plotly)
library(ComplexHeatmap)


source("helper_functions.R")


main_dir=getwd()
data_dir=paste(main_dir,"data/",sep="/")
clinical_data_file=paste(data_dir,"all_patients_clinical_data.txt",sep="")
tsne_table_dir=paste(data_dir,"tsne_tables",sep="")
cancer_mapping_file=paste(data_dir,"cancer_mapping.txt",sep="")
dendro_group_dir=paste(data_dir,"dendro_groups",sep="")
dir1=paste(data_dir,"dendro_annotations",sep="")
dir2=paste(data_dir,"heatmap_base",sep="")
dir3=paste(data_dir,"dtsne_annotations",sep="")
pal_file=paste(data_dir,"aeb_colors.rds",sep="")


