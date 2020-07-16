library(shiny)
library(plotly)
library(shinyBS)
source("DifferentialClusteringFunctions20200716.R", local=TRUE)



ui <- fluidPage(
  
  # Application title
  titlePanel("Sample size for partially or differentially clustered designs with baseline measurements on independent subjects or subjects nested in clusters"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("choice", label = ("Design type:"), 
                   choices = list("Design 1" = 1, "Design 2" = 2, "Design 3" = 3, "Design 4" = 4 , "Design 5" = 5), selected = 1),
      bsTooltip("choice", "See the Design Type tab for explanations of the different design types",
                "right", options = list(container = "body")),
      numericInput("effsize",
                   "Effect size of interest:",
                   step = 0.005,
                   value = 1.8),
      numericInput("alpha",
                   "Type 1 error rate:",
                   min = 0,
                   max=1,
                   step = 0.005,
                   value = 0.05),
      helpText("The min and max number of subjects in each intervention arm to consider. For Design 3, this is the number of subjects in each cluster."), 
      numericInput(
        "n1min",
        "Minimum number of subjects per intervention arm cluster:",
        min = 1,
        step = 1,
        value = 1),
      bsTooltip("n1min", "The minimum number of subjects in each intervention arm to consider. For Design 3, this is the minimum number of subjects in each cluster.",
                "right", options = list(container = "body"), trigger = "click"),
      numericInput(
        "n1max",
        "Maximum number of subjects per intervention arm cluster:",
        min = 1,
        step = 1,
        value = 100),
      helpText("Design-specific inputs below."), 
      conditionalPanel(
        condition = "input.choice == '1'",
        numericInput("k01",
                     label= HTML(paste("k",tags$sub("0"),": number of subjects in the control arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 38),
        numericInput("k11",
                     label= HTML(paste("k",tags$sub("1"),": number of clusters in the intervention arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 2),
       # numericInput("n1",
      #               label= HTML(paste("n",tags$sub("1"),": number of subjects in each intervention arm cluster", sep="")),
      #               min = 1,
      #               step = 1,
      #               value = 1),
        numericInput("sig1ind1",
                     label= HTML(paste("&sigma;",tags$sub("ind"),tags$sup("2"),": total variance of a subject at baseline", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 4.84),
        numericInput("sig1clus1",
                     label= HTML(paste("&sigma;",tags$sub("1clus"),tags$sup("2"),": unconditional variance of the outcome of a subject in the intervention arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 5.85),
      numericInput(
        "rho11",
        label= HTML(paste("&rho;",tags$sub("1"),": intracluster correlation at follow up in intervention clusters", sep="")),
        min = 0,
        max=1,
        step = 0.005,
        value = 0.05), 
       numericInput(
          "r_ind1",
          "r: test-retest reliability for individuals measured in unclustered setting twice",
          min = 0,
          max=1,
          step = 0.005,
          value = 0.29)
      ),
      conditionalPanel(
        condition = "input.choice == '2'",
        numericInput("k02",
                     label= HTML(paste("k",tags$sub("0"),": number of clusters in the control arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 38),
        numericInput("n02",
                     label= HTML(paste("n",tags$sub("0"),": number of subjects in a control arm cluster", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("k12",
                     label= HTML(paste("k",tags$sub("1"),": number of clusters in the intervention arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 2),
       # numericInput("n1",
      #               label= HTML(paste("n",tags$sub("1"),": number of subjects in each intervention arm cluster", sep="")),
      #               min = 1,
      #               step = 1,
      #               value = 1),
        numericInput("sig1ind2",
                     label= HTML(paste("&sigma;",tags$sub("ind"),tags$sup("2"),": total variance of a subject at baseline", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 4.84),
        numericInput("sig0clus2",
                     label= HTML(paste("&sigma;",tags$sub("0clus"),tags$sup("2"),": unconditional variance of the outcome of a subject in the control arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 4.84),
        numericInput("sig1clus2",
                     label= HTML(paste("&sigma;",tags$sub("1clus"),tags$sup("2"),": unconditional variance of the outcome of a subject in the intervention arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 5.85),
      numericInput(
        "rho02",
        label= HTML(paste("&rho;",tags$sub("0"),": intracluster correlation at follow up in control clusters", sep="")),
        min = 0,
        max=1,
        step = 0.005,
        value = 0), 
       numericInput(
        "rho12",
        label= HTML(paste("&rho;",tags$sub("1"),": intracluster correlation at follow up in intervention clusters", sep="")),
        min = 0,
        max=1,
        step = 0.005,
        value = 0.05), 
      numericInput(
          "r_ind2",
          "r: test-retest reliability for individuals measured in unclustered setting twice",
          min = 0,
          max=1,
          step = 0.005,
          value = 0.29)
      ),
      conditionalPanel(
        condition = "input.choice == '3'",
        numericInput("k03",
                     label= HTML(paste("k",tags$sub("0"),": number of clusters in the control arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("k13",
                     label= HTML(paste("k",tags$sub("1"),": number of clusters in the intervention arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
    #    numericInput("nall",
    #                 label= HTML(paste("n: number of subjects in each cluster", sep="")),
    #                 min = 1,
    #                 step = 1,
    #                 value = 1),
        numericInput("sig1ind3",
                     label= HTML(paste("&sigma;",tags$sub("base"),tags$sup("2"),": unconditional variance of a subject at baseline", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig0clus3",
                     label= HTML(paste("&sigma;",tags$sub("0clus"),tags$sup("2"),": unconditional variance of the outcome of a subject in the control arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig1clus3",
                     label= HTML(paste("&sigma;",tags$sub("1clus"),tags$sup("2"),": unconditional variance of the outcome of a subject in the intervention arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput(
          "rhobase3",
          label= HTML(paste("&rho;",tags$sub("base"),": intracluster correlation at baseline", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
    numericInput(
      "rho13",
      label= HTML(paste("&rho;",tags$sub("1"),": intracluster correlation at follow up in intervention clusters", sep="")),
      min = 0,
      max=1,
      step = 0.005,
      value = 0.29), 
    numericInput(
      "rho03",
      label= HTML(paste("&rho;",tags$sub("0"),": intracluster correlation at follow up in control clusters", sep="")),
      min = 0,
      max=1,
      step = 0.005,
      value = 0.29), 
        numericInput(
          "r_ind3",
          "r: correlation between means at baseline and follow-up of a cluster",
          min = 0,
          max=1,
          step = 0.005,
          value = 0.29)
      ),
      conditionalPanel(
        condition = "input.choice == '4'",
        numericInput("P",
                     label= HTML(paste("P: number of clusters at baseline", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("m",
                     label= HTML(paste("m: number of subjects in each cluster at baseline", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("k04",
                     label= HTML(paste("k",tags$sub("0"),": number of clusters in the control arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("n04",
                     label= HTML(paste("n",tags$sub("0"),": number of subjects in a control arm cluster (n0 + n1 = m)", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("k14",
                     label= HTML(paste("k",tags$sub("1"),": number of clusters in the intervention arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
      #  numericInput("n1",
      #               label= HTML(paste("n",tags$sub("1"),": number of subjects in each intervention arm cluster  (n0 + n1 = m)", sep="")),
      #               min = 1,
      #               step = 1,
      #               value = 1),
        numericInput("sigbase4",
                     label= HTML(paste("&sigma;",tags$sub("base"),tags$sup("2"),": unconditional variance of a subject at baseline", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig0clus4",
                     label= HTML(paste("&sigma;",tags$sub("0clus"),tags$sup("2"),": unconditional variance of the outcome at follow-up of a subject in the control arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig1clus4",
                     label= HTML(paste("&sigma;",tags$sub("1clus"),tags$sup("2"),": unconditional variance of the outcome at follow-up of a subject in the intervention arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput(
                  "rhobase4",
                   label= HTML(paste("&rho;",tags$sub("base"),": intracluster correlation at baseline", sep="")),
                    min = 0,
                    max=1,
                 step = 0.005,
                 value = 0.05), 
        numericInput(
          "rho04",
          label= HTML(paste("&rho;",tags$sub("0"),": intracluster correlation in control clusters", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
        numericInput(
          "rho14",
          label= HTML(paste("&rho;",tags$sub("1"),": intracluster correlation in intervention clusters", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
        numericInput(
          "rhoS4",
          label= HTML(paste("&rho;",tags$sub("S"),": correlation between two repeated measurements in the situation at baseline on a subject conditional on its cluster ", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05)
      ),
      conditionalPanel(
        condition = "input.choice == '5'",
        numericInput("k05",
                     label= HTML(paste("k",tags$sub("0"),": number of clusters in the control arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("n05",
                     label= HTML(paste("n",tags$sub("0"),": number of subjects in a control arm cluster", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
        numericInput("k15",
                     label= HTML(paste("k",tags$sub("1"),": number of clusters in the intervention arm", sep="")),
                     min = 1,
                     step = 1,
                     value = 1),
     #   numericInput("n1",
    #               label= HTML(paste("n",tags$sub("1"),": number of subjects in each intervention arm cluster", sep="")),
    #                 min = 1,
    #                 step = 1,
    #                 value = 1),
        numericInput("sigbase5",
                     label= HTML(paste("&sigma;",tags$sub("base"),tags$sup("2"),": unconditional variance of a subject at baseline", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig0clus5",
                     label= HTML(paste("&sigma;",tags$sub("0clus"),tags$sup("2"),": unconditional variance of the outcome at follow-up of a subject in the control arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput("sig1clus5",
                     label= HTML(paste("&sigma;",tags$sub("1clus"),tags$sup("2"),": unconditional variance of the outcome at follow-up of a subject in the intervention arm", sep="")),
                     min = 0,
                     step = 0.005,
                     value = 0.5),
        numericInput(
          "rhobase5",
          label= HTML(paste("&rho;",tags$sub("base"),": intracluster correlation at baseline", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
        numericInput(
          "rho05",
          label= HTML(paste("&rho;",tags$sub("0"),": intracluster correlation in control clusters", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
        numericInput(
          "rho15",
          label= HTML(paste("&rho;",tags$sub("1"),": intracluster correlation in intervention clusters", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05), 
        numericInput(
          "rhoS5",
          label= HTML(paste("&rho;",tags$sub("S"),": correlation between two repeated measurements in the situation at baseline on a subject conditional on its cluster ", sep="")),
          min = 0,
          max=1,
          step = 0.005,
          value = 0.05)
      )
    ),
    
    # Display output
    mainPanel(
      tabsetPanel(
        tabPanel("Power plot",  
                 plotlyOutput("PowerPlot")          
        ),
          tabPanel("Design Type",  
                  uiOutput("designs")          
          ),
        tabPanel("Details and contacts",
                 textOutput("details")
        )
      )
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PowerPlot <- renderPlotly({
    
    
    if(input$choice==1) {
      myvars= simplify2array(lapply(seq(input$n1min, input$n1max, 1), var_design1, 
                                    k1=input$k11, k0=input$k01, sig1clus=input$sig1clus1 ,
                                    sigind=input$sig1ind1, r=input$r_ind1 , rho1=input$rho11
                                     ))
      mypower <- pnorm( -qnorm(1-input$alpha/2) + sqrt(1/myvars)*input$effsize )
      
      p1<-plot_ly(x=seq(input$n1min, input$n1max, 1), y=mypower,type='scatter',mode='lines') %>%
        layout(xaxis=list(title="Intervention arm cluster size"),yaxis=list(title="Power"))
      
      p1
    }
    else if(input$choice ==2){
      myvars= simplify2array(lapply(seq(input$n1min, input$n1max, 1), var_design2, 
                                    k1=input$k12, k0=input$k02, n0=input$n02, 
                                    sig1clus=input$sig1clus2, sig0clus=input$sig0clus2,
                                    sigind=input$sig1ind2, r=input$r_ind2 , 
                                    rho1=input$rho12, rho0=input$rho02
      ))
      mypower <- pnorm( -qnorm(1-input$alpha/2) + sqrt(1/myvars)*input$effsize )
      
      p1<-plot_ly(x=seq(input$n1min, input$n1max, 1), y=mypower,type='scatter',mode='lines') %>%
        layout(xaxis=list(title="Intervention arm cluster size"),yaxis=list(title="Power"))
      
      p1
    }
    else if(input$choice ==3){
      myvars= simplify2array(lapply(seq(input$n1min, input$n1max, 1), var_design3, 
                                    k1=input$k13, k0=input$k03, 
                                    sig1clus=input$sig1clus3, sig0clus=input$sig0clus3,
                                    sigbase=input$sig1ind3, r = input$r_ind3, 
                                    rhobase=input$rhobase3 , 
                                    rho1=input$rho13, rho0=input$rho03
      ))
      mypower <- pnorm( -qnorm(1-input$alpha/2) + sqrt(1/myvars)*input$effsize )
      
      p1<-plot_ly(x=seq(input$n1min, input$n1max, 1), y=mypower,type='scatter',mode='lines') %>%
        layout(xaxis=list(title="Intervention arm cluster size"),yaxis=list(title="Power"))
      
      p1
    }
    else if(input$choice ==4){
      myvars= simplify2array(lapply(seq(input$n1min, input$n1max, 1), var_design4, 
                                    k1=input$k14, k0=input$k04, n0=input$n04, 
                                    m = input$m, P=input$P, 
                                    sig1clus=input$sig1clus4, sig0clus=input$sig0clus4,
                                    sigbase=input$sig1ind4,  
                                    rhobase=input$rhobase4 , rhoS = input$rhoS4,
                                    rho1=input$rho14, rho0=input$rho04
      ))
      mypower <- pnorm( -qnorm(1-input$alpha/2) + sqrt(1/myvars)*input$effsize )
      
      p1<-plot_ly(x=seq(input$n1min, input$n1max, 1), y=mypower,type='scatter',mode='lines') %>%
        layout(xaxis=list(title="Intervention arm cluster size"),yaxis=list(title="Power"))
      
      p1
    }
    
    else if(input$choice ==5){
      #var_design5(n1, k1, n0, k0, sig1clus, sig0clus, sigbase, rho1, rho0, rhoS, rhobase){

      myvars= simplify2array(lapply(seq(input$n1min, input$n1max, 1), var_design5, 
                                    k1=input$k15, k0=input$k05, n0=input$n05, 
                                    sig1clus=input$sig1clus5, sig0clus=input$sig0clus5,
                                    sigbase=input$sig1ind5,  
                                    rhobase=input$rhobase5, rhoS = input$rhoS5,
                                    rho1=input$rho15, rho0=input$rho05
      ))
      mypower <- pnorm( -qnorm(1-input$alpha/2) + sqrt(1/myvars)*input$effsize )
      
      p1<-plot_ly(x=seq(input$n1min, input$n1max, 1), y=mypower,type='scatter',mode='lines') %>%
        layout(xaxis=list(title="Intervention arm cluster size"),yaxis=list(title="Power"))
      
      p1
    }
    
  })
  
  output$designs <- renderUI({
    HTML(paste("<b>Design 1</b>: independent subjects at baseline randomized to either independent condition or clustered condition at follow-up (partially clustered design).", 
               "<b>Design 2</b>: independent subjects at baseline randomized to different clustered conditions (differentially clustered design).", 
               "<b>Design 3</b>: clusters at baseline randomized to the clusters of the same size but with different ICC (differentially clustered design).", 
               "<b>Design 4</b>: subjects nested in clusters at baseline are randomly redistributed over control and intervention clusters.", 
               "<b>Design 5</b>: some subjects in clusters at baseline are randomized to intervention clusters, the rest stay in their baseline cluster.", sep="<br/>"))
  })
  
  
  
  
  output$details <- renderText({ 
    "The shiny app to accompany ``Sample size for partially or differentially clustered designs with baseline measurements on independent subjects or subjects nested in clusters'', 
    by Steven Teerenstra, Jessica Kasza, Roeslan Leontjevas,
    and Andrew Forbes.
    
    Shiny app developed by Jessica Kasza. Email jessica.kasza@monash.edu with questions or comments about the app."
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

