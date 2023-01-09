library(shiny)
library(faux)
library(psych)

#ui

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("|Test| Simulations: Correlations as a function of SDs' width"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      n <- sliderInput("n",
                                 label = "Sample size",
                                 min = 1,
                                 max = 1000,
                                 value = 50,
                                 step = 1,
                                 width = "100%"),
    
    m1 <- sliderInput("m1",
                               label = "Mean Var 1",
                               min = 0,
                               max = 1,
                               value = 0.5,
                               step = .1,
                               width = "100%"),
    
    m2 <- sliderInput("m2",
                      label = "Mean Var 2",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = .1,
                      width = "100%"),
    
    sd1 <- sliderInput("sd1",
                      label = "SD Condition 1 (small SD)",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = .1,
                      width = "100%"),
    
    sd2 <- sliderInput("sd2",
                      label = "SD Condition 2 (large SD)",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = .1,
                      width = "100%"),
    
    r <- sliderInput("r",
                     label = "Correlation between Var 1 and Var 2 (between 0 and 1, similar for neg. correlations)",
                     min = 0,
                     max = .99,
                     value = 0.2,
                     step = .01,
                     width = "100%"),
    
    sim <- numericInput("sim", "Simulations (does not work for now)", 100, min = 10, max = 1000)
    
    
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("plotcor1"),
      plotOutput("plotcor2"), 
      textOutput("test_cor")  
    )
  )
)

#server

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  dat_sd1 = reactive({rnorm_multi(n = input$n, 
                               mu = c(input$m1, input$m2),
                               sd = c(input$sd1, input$sd1),
                               r = input$r,
                               varnames = c("varA", "varB"),
                               empirical = FALSE)
  })
  
  dat_sd2 = reactive({rnorm_multi(n = input$n, 
                        mu = c(input$m1, input$m2),
                        sd = c(input$sd2, input$sd2),
                        r = input$r,
                        varnames = c("varA", "varB"),
                        empirical = FALSE)
  })

  
  cor1 = reactive({cor.test(dat_sd1()$varA, dat_sd1()$varB)})
  cor2 = reactive({cor.test(dat_sd2()$varA, dat_sd2()$varB)})
  
  rdiff = reactive({r.test(n = input$n
                           ,n2 = input$n
                           ,r12 = cor1()$estimate
                           ,r34 = cor2()$estimate
                           ,twotailed = TRUE)
  })
  
  output$plotcor1 <- renderPlot({
    plot(dat_sd1()$varA, dat_sd1()$varB)+
      title(paste0("r = ", round(cor1()$estimate, digits = 3)
                                                      , " 95% CI = [", round(cor1()$conf.int[1], digits = 3), ",", round(cor1()$conf.int[2], digits = 3), "]")
            , adj=0)
  })

  output$plotcor2 <- renderPlot({
    plot(dat_sd2()$varA, dat_sd2()$varB)+title(paste0("r = ", round(cor2()$estimate, digits = 3)
                                                      , " 95% CI = [", round(cor2()$conf.int[1], digits = 3), ",", round(cor2()$conf.int[2], digits = 3), "]")
                                               , adj=0)
  })

  output$test_cor <- renderText({paste0("Significance test between the two correlations: ", "Z = ", round(rdiff()$z, digits=3), ", ", "p = ", round(rdiff()$p, digits=3))
                                        })
  

    
}

shinyApp(ui, server)
