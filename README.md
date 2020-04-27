
<p align="center">📊 <b>Sequential t-SNE GUI</b></p>
<p align="center"> A GUI interface for preforming sequential t-SNE clustering survival analysis on TCGA gene expression data. 

## Useage 

### 🌐 Online version 
Go to 🔗[`Sequential t-SNE`](https://chpupsom19.shinyapps.io/survival_analysis_tsne_umap_tcga/). 

### 💻 Standalone version  
<details>
<summary><b>Installation</b></summary>  

Ensure that the following packages are installed in your R enviornment: 

`shiny`,`survminer`,`survival`,`plotly`,`ComplexHeatmap`

If any package is missing, Please run the following command in your [`RStudio`](https://www.rstudio.com/) and it will install all packages automatically.  

```R
# Check "BiocManager"
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Package list
libs <- c("shiny", "survminer","survival","plotly","ComplexHeatmap")

# Install packages if missing
for (i in libs){
  if( !is.element(i, .packages(all.available = TRUE)) ) {
     BiocManager::install(i, suppressUpdates=TRUE)
  }
}
```
</details>

<details>
<summary><b>Launch</b></summary> 
    
1. Click `Clone or download` button on the top of this page, then click `Download ZIP`();  
2. Unzip the file to an desired folder location.;  
3. In R Studio set the working directory to the folder location choosen in step 2 (use `setwd()` to set your working directory);
    
</details>

## Project Details 
We recently demonstrated that long-term intra-group survival disparities in 30 of 34 human cancer types are associated with distinct expression pattern differences of small numbers of functionally related transcripts relevant to cancer signaling, proliferation and metabolism. These differences can be expressed as clusters using the dimensionality reduction technique “t-distributed stochastic neighbor embedding” (t-SNE). These clusters more accurately correlated with survival than did standard classification such as clinical stage or hormone receptor status in the case of breast cancer. We have now comprehensively examined that sequential analyses employing either t-SNE to t-SNE or whole transcriptome to t-SNE analyses are superior to either individual method at predicting long-term survival.   

## Methods
RNAseq data from 10,227 tumors in The Cancer Genome Atlas were previously analyzed using t-SNE-based clustering of 362 transcripts comprising 15 distinct cancer-related pathways. After showing that certain clusters were associated with differential survival, they were re-analyzed by t-SNE with a second pathway’s transcripts. Alternatively, groups with differential survival based on whole transcriptome profiling from tcga.ngchm.net were subject to a second, t-SNE-based analysis.

## 📈 Features

## 📕 Publication

**Sequential Analysis of Transcript Expression Patterns Improves Survival Prediction in Multiple Cancers**;

Jordan Mandel, Raghunandan Avula, Edward V Prochownik;
*Submitted for Review*

    

