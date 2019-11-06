library(shiny)
library(plotly)

ui <- fluidPage(theme = "bootstrap2.css",
  
  titlePanel("Sequential T-SNE Analysis"),
  
  helpText("We recently demonstrated that long-term intra-group survival disparities in 
           30 of 34 human cancer types in TCGA are associated with distinct expression pattern differences 
           of small numbers of functionally related transcripts relevant to cancer signaling, proliferation 
           and metabolism. These differences can be expressed as clusters using t-SNE, a dimensionality reduction technique in machine learning, as seen in the 3D plots (Tab 1). 
           The suvival data for patients in these identified cluster can be compared to highligh survival differences as seen in the survival curves for individual pathways (Tab 2).
           This app shows how the sequential t-SNE approach can be employed sequentially to improve predictive value with T-SNE clusters determined from other pathways (Tab 3) 
           OR in tandem with dendrogram groups identified from whole transcriptome profiling of TCGA data (Tab 4). 
           The whole transcriptome profiling from tcga.ngchm.net can be examined in (Tab 5). 
           This application dynamically explores these insights."),
  
  sidebarLayout(
    
    # sidebarPanel ------------------------------------------------------------
    wellPanel(
      #fluidRow(class = "text-center",column(12, htmlOutput("appdes"))),
      fluidRow(
        column(4,uiOutput("cancerSelector")),
        column(4,conditionalPanel(
          condition = "input.cancer != ''",
          uiOutput("pathway1Selector"))),
        column(4,conditionalPanel(
          condition = "input.pathway1 != ''",
          uiOutput("pathway2Selector")))
      ),#/fluidRow
      fluidRow(
        column(4,conditionalPanel(
          condition = "input.pathway2 != '' && input.pathway1 != ''",
          uiOutput("clusterSelector"))),
        column(4,conditionalPanel(
          condition = "input.pathway2 != '' && input.pathway1 != '' && input.clusters != ''",
          uiOutput("clusterSelector2"))),
        column(4,conditionalPanel(
          condition = "input.pathway2 != '' && input.pathway1 != '' && input.clusters != ''",
          uiOutput("groupSelector2")))
      )#/fluidRow2
    ),#/wellPanel
    
    mainPanel(
      tabsetPanel(
        #tabPanel("Table",DT::dataTableOutput("table")),
        tabPanel("T-SNE 3D Plots",
                 fluidRow(class = "text-center",column(12, htmlOutput("plot_3d_warning"))),
                 fluidRow(class = "text-center",
                          column(6, htmlOutput("pathway1_title_3d")),
                          column(6, htmlOutput("pathway2_title_3d"))
                 ),
                 fluidRow(
                   column(6,plotlyOutput('scatterplot_out')),
                   column(6,plotlyOutput('scatterplot_out2'))
                 )),
        tabPanel("Individual Pathways",
                fluidRow(class = "text-center",column(12, htmlOutput("cancer_name"))),
                fluidRow(class = "text-center",
                         column(6, htmlOutput("pathway1_title")),
                         column(6, htmlOutput("pathway2_title"))
                ),
                fluidRow(
                   column(6,plotOutput("survivalplot_P1_out")),
                   column(6,plotOutput("survivalplot_P2_out"))
                 )),#/fluidRow3
        tabPanel("Sequential T-SNE",
                fluidRow(class = "text-center",column(12, htmlOutput("survivalplotheader"))),
                fluidRow(class = "text-center",column(12, htmlOutput("survivalplotmid"))),
                fluidRow(class = "text-center",column(12, htmlOutput("survivalplotheader2"))),
                plotOutput('survivalplot_out'),
                fluidRow(class = "text-center",column(12, htmlOutput("text2"))),
                DT::dataTableOutput("counttable")),
        tabPanel("Sequential Dendrogram ",
                 fluidRow(class = "text-center",column(12, htmlOutput("dendrosurvivalplotheader"))),
                 fluidRow(class = "text-center",column(12, htmlOutput("dendrosurvivalplotmid"))),
                 fluidRow(class = "text-center",column(12, htmlOutput("dendrosurvivalplotheader2"))),
                plotOutput('survivalplot_dendro_out'),
                fluidRow(class = "text-center",column(12, htmlOutput("text3"))),
                DT::dataTableOutput("counttable_dendro")),
        tabPanel("Heirarchical Clustering",
                 fluidRow(class = "text-center",column(12, htmlOutput("heatmaptitle"))),
                 fluidRow(class = "text-center",column(12, htmlOutput("heatmaptitle2"))),
                 plotOutput('heatmap'),
                 fluidRow(column(12,uiOutput("go_to_web"))) #htmlOutput("webpage")
        )
    ),style='width: 1000px; height: 1500px', #/tabsetPanel
    tags$div(id="cite","Designed by Raghu Avula & Jordan Mandel - GitHub: https://github.com/RavulaPitt/Sequential-TSNE_RShiny")
    )#/mainPanel
  )#/sidebarLayout
)#/fluidPage