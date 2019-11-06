source("main.R")

# shinyServer -------------------------------------------------------------

server <- function(input, output) {
  
  # Tables -------------------------------------------------------------------
  load_clinical_data<- reactive({
    load_clinical_data=read.csv(clinical_data_file,sep="\t")})
  
  load_tsne_table1<-reactive ({ # Load TSNE Table 1
    req(input$pathway1)
    clinicalD=load_clinical_data()
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
    data1=read.csv(paste(c(tsne_table_dir,cancer_name,paste(input$pathway1,".txt",sep="")),collapse = "/"))
    load_tsne_table1=merge_clinical(data1,clinicalD)

  })
  
  load_tsne_table2<-reactive ({ # Load TSNE Table 1
    req(input$pathway2)
    clinicalD=load_clinical_data()
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
    data1=read.csv(paste(c(tsne_table_dir,cancer_name,paste(input$pathway2,".txt",sep="")),collapse = "/"))
    load_tsne_table2=merge_clinical(data1,clinicalD)
  })
  
  load_tsne_tables<-reactive ({ # Load TSNE Table 2 and Merge 
    req(input$clusters)
    req(input$clusters2)
    clinicalD=load_clinical_data()
    data1=load_tsne_table1()
    data2=load_tsne_table2()
    Pathway_Names=c(input$pathway1,input$pathway2)
    Cluster=input$clusters
    Cluster2=input$clusters2
    if ("all" %in% Cluster){Clusters1=sort(unique(data1$Cluster))
    } else{Clusters1=as.integer(Cluster)}
    load_tsne_tables=sequential_filter(data1,data2,Clusters1,reverse = FALSE,Pathway_Names,Cluster2)
  })
  
  display_data<- reactive ({ #Not Used but can show Merged Data used in Analysis
    req(input$pathway2)
    data=load_tsne_tables()[[1]]
    n=ncol(data)
    display_data=data[,-c(n-1,n)]
  })
  
  counttable<- reactive({ #Generated table of counts under survivial plots in Tab 2 
    req(input$pathway2)
    req(input$clusters)
    data=load_tsne_tables()[[1]]
    counttable=count_table(data)
  })
  
  load_mapping<-reactive({
    load_mapping=read.csv(cancer_mapping_file,sep="\t",header=TRUE)})
  
  load_dendrogram_groups<-reactive ({ 
    req(input$cancer)
    mapping=load_mapping()
    cancer=input$cancer
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
    cancer_index=which(mapping$Full_Name==cancer_name)
    symbol=as.vector(mapping[cancer_index,"Symbol"])
    dendg=readRDS(paste(c(dendro_group_dir,"/",symbol,"_dendro_groups.RDS"),collapse=""))
    dendro_groups=cbind(names(dendg),unname(dendg))
    colnames(dendro_groups)=c("Sample","Group")
    load_dendogram_groups=dendro_groups
    
  })
  
  load_tsne_tables_dendro <-reactive ({
    req(input$clusters)
    req(input$group2)
    data1=load_tsne_table1()
    dendro_groups=load_dendrogram_groups()
    Cluster=input$clusters
    Group2=input$group2
    if ("all" %in% Cluster){Clusters1=sort(unique(data1$Cluster))
    } else{Clusters1=as.integer(Cluster)}
    load_tsne_tables_dendro=sequential_filter_dendro(data1,dendro_groups,Cluster,Group2)
  })
  
  counttable_dendro<- reactive({ #Generated table of counts under survivial plots in Tab 2 
    req(input$group2)
    req(input$clusters)
    data=load_tsne_tables_dendro()[[1]]
    #print(colnames(data))
    counttable_dendro=count_table_dendro(data)
  })
  
  load_pal <-reactive ({
    load_pal=readRDS(pal_file)
    load_pal=unname(load_pal)
  })
  # Analysis ----------------------------------------------------------------
  
  fit_P1<- reactive ({
    table=load_tsne_table1()
    fit_P1=my_sfit_P1(table)
  })
  
  fit_P2<- reactive ({
    table=load_tsne_table2()
    fit_P2=my_sfit_P1(table)
  })
  
  fit<- reactive ({
    table=load_tsne_tables()[[1]]
    Clusters=input$clusters
    # if ("all" %in% Cluster){Clusters1="all"
    # } else{Clusters1=as.integer(Cluster)}
    fit=my_sfit(table)
  })
  
  fit_dendro<-reactive({
    table=load_tsne_tables_dendro()[[1]]
    fit_dendro=my_sfit(table)
  })
  
  # Plots -------------------------------------------------------------------
  survivalplot_P1_gen<-reactive({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    validate(
      need(input$pathway1 != "", "Please select 1st Pathway")
    )
    req(input$cancer)
    req(input$pathway1)
    f=fit_P1()
    pal=load_pal()
    survivalplot_P1_gen1=my_splot_P1_create1(f[[1]],f[[2]],input$pathway1,pal)
    splots=list(survivalplot_P1_gen1)
    survivalplot_P1_gen=my_splot_generate(splots,input$cancer,pdf=FALSE,filename='test_P1.pdf')

  })
  
  survivalplot_dendo_groups_gen<-reactive({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    validate(
      need(input$cancer != "Acute Myeloid Leukemia Marrow", "Acute Myeloid Leukemia marrow does not have heiarchical clustering data for analysis")
    )
    validate(
      need(input$pathway1 != "", "Please select 1st Pathway")
    )
    validate(
      need(input$pathway2 != "", "Please select 2nd Pathway")
    )
    validate(
      need(input$clusters != "", "Please select clusters in 1st pathway to analyze")
    )
    validate(
      need(input$group2!= "", "Please select groups from dendrogram in heirarchical clustering to analyze")
    )
    
    req(input$cancer)
    req(input$pathway1)
    req(input$clusters)
    req(input$group2)
    f=fit_dendro()
    Cluster=input$clusters
    if ("all" %in% Cluster){Clusters1="all"
    } else{Clusters1=as.integer(Cluster)}
    Group2=input$group2
    if ("all" %in% Group2){Group2a="all"
    } else{Group2a=as.integer(Group2)}
    pal=load_pal()
    survivalplot_gen1=my_splot_create1(f[[1]],f[[2]],table[[2]],Clusters1,pal)
    splots=list(survivalplot_gen1)
    survivalplot_gen=my_splot_generate(splots,input$cancer,FALSE,filename=NULL)
  })

  survivalplot_P2_gen<-reactive({
    req(input$cancer)
    req(input$pathway1)
    req(input$pathway2)
    f=fit_P2()
    pal=load_pal()
    survivalplot_P2_gen1=my_splot_P1_create1(f[[1]],f[[2]],input$pathway2,pal)
    splots=list(survivalplot_P2_gen1)
    survivalplot_P2_gen=my_splot_generate(splots,input$cancer,pdf=FALSE,filename='test_P2.pdf')
    
  })
  
  survivalplot_gen<-reactive({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    validate(
      need(input$pathway1 != "", "Please select 1st Pathway")
    )
    validate(
      need(input$pathway2 != "", "Please select 2nd Pathway")
    )
    validate(
      need(input$clusters != "", "Please select clusters in 1st pathway to analyze")
    )
    validate(
      need(input$clusters2 != "", "Please select clusters in 2nd pathway to analyze")
    )
    req(input$cancer)
    req(input$pathway1)
    req(input$pathway2)
    req(input$clusters)
    table=load_tsne_tables()
    f=fit()
    Cluster=input$clusters
    if ("all" %in% Cluster){Clusters1="all"
    } else{Clusters1=as.integer(Cluster)}
    Cluster2=input$clusters2
    if ("all" %in% Cluster2){Clusters2="all"
    } else{Clusters2=as.integer(Cluster2)}
    pal=load_pal()
    survivalplot_gen1=my_splot_create1(f[[1]],f[[2]],table[[2]],Clusters1,pal)
    splots=list(survivalplot_gen1)
    survivalplot_gen=my_splot_generate(splots,input$cancer,FALSE,filename=NULL)
  })
  
  scatterplot_gen<-reactive({
    req(input$cancer)
    req(input$pathway1)
    data=load_tsne_table1()
    if(is.null(data)){
      #dummy_table=matrix(3,3,3)
      #scatterplot_gen=scatterplot_gen = scatterplot3d(dummy_table)
      scatterplot_gen=NULL
    } else {
      table=data[,c("X","Y","Z","Cluster")]
      colors <- c("#999999", "#E69F00", "#56B4E9","red","green","black","purple")
      colors=colors[1:length(unique(table$Cluster))]
      colors <- colors[as.numeric(table$Cluster)] 
      scatterplot_gen = scatterplot3d(table[,1:3], pch = 16, color=colors)
      legend(scatterplot_gen$xyz.convert(10,10,10),legend = unique(table$Cluster),col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)
    } 
  })
  
  scatterplot_gen2_p1<-reactive({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    validate(
      need(input$pathway1 != "", "Please select Pathway")
    )
    req(input$cancer)
    req(input$pathway1)
    data=load_tsne_table1()
    if(is.null(data)){
      #dummy_table=matrix(3,3,3)
      #scatterplot_gen=scatterplot_gen = scatterplot3d(dummy_table)
      scatterplot_gen2=NULL
    } else {
      table=data[,c("X","Y","Z","Cluster")]
      table$Cluster=as.character(table$Cluster)
      scatterplot_gen2<-plot_ly(table,x=~X,y=~Y,z=~Z,color=~Cluster,opacity=.7) %>% #size_max=1,opacity=.7
            add_markers()
      }
    })
  
  scatterplot_gen2_p2<-reactive({
    req(input$cancer)
    req(input$pathway1)
    data=load_tsne_table2()
    if(is.null(data)){
      #dummy_table=matrix(3,3,3)
      #scatterplot_gen=scatterplot_gen = scatterplot3d(dummy_table)
      scatterplot_gen2=NULL
    } else {
      table=data[,c("X","Y","Z","Cluster")]
      table$Cluster=as.character(table$Cluster)
      scatterplot_gen2<-plot_ly(table,x=~X,y=~Y,z=~Z,color=~Cluster,opacity=.7) %>%
        add_markers()
    }
  })
  
  heatmap_gen<-reactive({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    validate(
      need(input$cancer != "Acute Myeloid Leukemia Marrow", "Acute Myeloid Leukemia marrow does not have heiarchical clustering data for analysis")
    )
    validate(
      need(input$pathway1 != "", "Please select 1st Pathway")
    )
    req(input$cancer)
    req(input$pathway1)
    mapping=load_mapping()
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
    cancer_index=which(mapping$Full_Name==cancer_name)
    cancer=as.vector(mapping[cancer_index,"Symbol"])
    pathway=input$pathway1
    base_dir=data_dir
    hm <- readRDS(file.path(base_dir,"heatmap_base",paste0(cancer,"_heatmap.RDS")))
    base_ano <- readRDS(file.path(base_dir,"dendro_annotations",paste0(cancer,"_annotation.RDS")))
    tsne_ano <- readRDS(file.path(base_dir,"tsne_annotations",cancer,paste0(pathway,"#tsneAnnotation.RDS")))
    heatmap_gen <- draw(tsne_ano%v% base_ano %v% hm)
  })
  

  # Heirarchical Clustering -------------------------------------------------
  getPage<-function() {
      validate(
        need(input$cancer != "", "Please select a Cancer")
      )
      cancer=paste(strsplit(input$cancer," ")[[1]],collapse="_")
      mapping=load_mapping()
      cancer_index=which(mapping$Full_Name==cancer)
      key=as.vector(mapping[cancer_index,"map"])
      symbol=tolower(as.vector(mapping[cancer_index,"Symbol"]))
      url=paste(c("http://tcga.ngchm.net/NGCHM/chm.html?map=",
                  key,
                  "collectionHome=http%3A%2F%2Ftcga.ngchm.net%2F%3Fview%3D0%26p0DiseaseInput%3D",
                  symbol,
                  '%26p0PlatformInput%3Drnaseq%26p0DiagramInput%3Dvariable_sample'),collapse="")
      print(url)
      return((browseURL(url)))
    }

  observeEvent(input$go_to_web, {
    htmlOutput(getPage())
  })
  
  
  # Output ------------------------------------------------------------------
  # User Input Buttons ------------------------------------------------------
  output$cancerSelector <- renderUI({
    Cancers=list.dirs(tsne_table_dir,full.names = FALSE,recursive = TRUE) #Get list of cancers from tsne_table_dir
    Cancers=unname(sapply(Cancers,function(x){return(paste(strsplit(x,"_")[[1]],collapse=" "))}))
    #print(Cancers)
    selectizeInput("cancer",label="Choose a Cancer",selected=Cancers[1],multiple=FALSE,choices=Cancers) #Cancers[1] vs NA
  })
  
  output$pathway1Selector <- renderUI({
    req(input$cancer)
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
    Pathways=c("",as.vector(sapply(list.files(paste(tsne_table_dir,cancer_name,sep="/"),full.names = FALSE),h1)))
    selectizeInput("pathway1",label="Choose 1st Pathway",selected=Pathways[1],multiple=FALSE,choices=Pathways) #NA Pathways[1]
  })
  
  output$pathway2Selector <- renderUI({
    req(input$pathway1)
    cancer_name=paste(strsplit(input$cancer," ")[[1]],collapse="_")
      Pathways2=c("",as.vector(sapply(list.files(paste(tsne_table_dir,cancer_name,sep="/"),full.names = FALSE),h1)))
      pathways2a=Pathways2[which(Pathways2!=input$pathway1)]
      selectizeInput("pathway2",label="Choose 2nd Pathway",selected=pathways2a[1],multiple=FALSE,choices=pathways2a)  #NA pathways2a[1]
  })
  
  output$clusterSelector <- renderUI({
    req(input$pathway2)
      tsne_table1=load_tsne_table1()
      Clusters=sort(unique(tsne_table1$Cluster))
      Clusters1=c("all",Clusters)
      selectizeInput("clusters",label="Choose Clusters in Pathway 1",multiple=TRUE,selected=NA,choices=Clusters1)
  })
  
  output$clusterSelector2 <- renderUI({
    req(input$clusters)
    tsne_table1=load_tsne_table2()
    Clusters=sort(unique(tsne_table1$Cluster))
    Clusters1=c("all",Clusters)
    selectizeInput("clusters2",label="Choose Clusters in Pathway 2",multiple=TRUE,selected="all",choices=Clusters1)
  })
  
  output$groupSelector2 <- renderUI({
    req(input$clusters)
    req(input$cancer!="Acute Myeloid Leukemia Marrow")

    table=load_dendrogram_groups()
    Groups=sort(unique(table[,'Group']))
    Groups1=c("all",Groups)
    selectizeInput("group2",label="Choose Dendogram Groups",multiple=TRUE,selected=NA,choices=Groups1)
  })
  
  output$go_to_web <- renderUI({
    validate(
      need(input$cancer != "", "Please select a Cancer")
    )
    #fluidRow(column(4,conditionalPanel(condition = "input.cancer != ''",uiOutput("go_to_web"))))
    actionButton("go_to_web", "View on Next-Generation Clustered Heat Map (NG-CHM) Viewer")
  })
  # Output Text -------------------------------------------------------------
  output$text1 <- renderText(input$pathway1)
  #output$text2 <- renderText("Number of Patients in Each Cluster")
  
  output$text2 <- renderText({
    req(input$clusters2);
    paste(c("<h5>","<b>Number of patients in each cluster per pathways<b>","</h5>"),collapse="")})
  
  output$text3 <- renderText({
    req(input$group2);
    paste(c("<h5>","<b>Number of patients in each t-SNE cluster and dendrogram groups<b>","</h5>"),collapse="")})
  
  
  output$appdes <- renderText({'<h6>TEST<h6>/'})
  
  output$cancer_name <- renderText({
    req(input$pathway1);
    paste(c("<h4>",input$cancer,"</h4>"),collapse="")})
  
  output$pathway1_title <- renderText({
    req(input$pathway1);
    paste(c("<h5>","<b>Pathway  1 : <b>",input$pathway1,"</h5>"),collapse="")})
  
  output$pathway1_title_3d <- renderText({
    req(input$pathway1);
    paste(c("<h5>","<b>Pathway  1 : <b>",input$pathway1,"</h5>"),collapse="")})

  output$pathway2_title <- renderText({
    req(input$pathway2);
    paste(c("<h5>","<b>Pathway 2 : <b>",input$pathway2,"</h5>"),collapse="")})
  
  output$pathway2_title_3d <- renderText({
    req(input$pathway2);
    paste(c("<h5>","<b>Pathway 2 : <b>",input$pathway2,"</h5>"),collapse="")})
  
  output$heatmaptitle<- renderText({
    req(input$pathway1);
    paste(c("<h4><b>",input$cancer,"<b><h4>"),collapse="")})  
  output$heatmaptitle2<- renderText({
    req(input$pathway1);
    paste(c("<h4><b>",input$pathway1,"<b><h4>"),collapse="")})  
  
  output$survivalplotmid<- renderText({
    req(input$clusters2);
    "sequential analysis with"})
  
  output$survivalplotheader <- renderText({
    req(input$clusters2);
    cluster=paste(input$clusters,collapse=" , ")
    paste(c("<h5>",paste(c("t-SNE clusters in ",input$pathway1,": <b>",cluster,'<b>'),collapse=" "),"</h5>"),collapse="")})
  
  output$survivalplotheader2 <- renderText({
    req(input$clusters2);
    cluster=paste(input$clusters2,collapse=" , ")
    paste(c("<h5>",paste(c("t-SNE clusters in",input$pathway2,": <b>",cluster,'<b>'),collapse=" "),"</h5>"),collapse="")})
  
  output$dendrosurvivalplotmid<- renderText({
    req(input$group2);
    "sequential analysis with"})
  
  output$dendrosurvivalplotheader <- renderText({
    req(input$group2);
    cluster=paste(input$clusters,collapse=" , ")
    paste(c("<h5>",paste(c("t-SNE clusters in ",input$pathway1,": <b>",cluster,'<b>'),collapse=" "),"</h5>"),collapse="")})
  
  output$dendrosurvivalplotheader2 <- renderText({
    req(input$group2);
    cluster=paste(input$group2,collapse=" , ")
    paste(c("<h5>",paste(c("Dendrogram Groups: <b>",cluster,'<b>'),collapse=" "),"</h5>"),collapse="")})
  
  
  # Output Tables ------------------------------------------------------------
  output$table <- DT::renderDataTable({ # Not currently used 
    #table <- display_data()
    table <- load_tsne_tables_dendro()[[1]]
    DT::datatable(table,filter="top",rownames=FALSE)
  })
  
  output$counttable <- DT::renderDataTable({
    table <- counttable()
    DT::datatable(table,rownames=TRUE)
  })
  
  output$counttable_dendro <- DT::renderDataTable({
    table <- counttable_dendro()
    DT::datatable(table,rownames=TRUE)
  })
  
  # Output Plots ------------------------------------------------------------
  output$survivalplot_P1_out <- renderPlot(survivalplot_P1_gen())
  output$survivalplot_P2_out <- renderPlot(survivalplot_P2_gen())
  output$survivalplot_out <- renderPlot(survivalplot_gen())
  output$scatterplot_out <- renderPlotly({scatterplot_gen2_p1()})
  output$scatterplot_out2 <- renderPlotly({scatterplot_gen2_p2()})
  
  output$survivalplot_dendro_out <- renderPlot(survivalplot_dendo_groups_gen())
  output$heatmap <- renderPlot(heatmap_gen())
  


  
  
}#/server