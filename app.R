#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinyjs)
library(rgdal)
library(rgeos)
library(zip)
library(dismo)
library(raster)
library(matrixStats)
library(diagram)
load("NSO_Data")

# Define UI for application that draws a histogram
ui <- {fluidPage(
  useShinyjs(),
  titlePanel("Fire Refugia Ranking for Spotted Owl"),
  sidebarLayout(
    sidebarPanel(
      p("Select a fire"),
      selectInput(inputId = "inFireName", 
                  label = "Select a fire", 
                  choices = c("Table Mountain", "Jolly Mountain")),
      br(),
      tableOutput('table')
    ),
    mainPanel(
      tabsetPanel(type = "pills",
                  tabPanel("Location", plotOutput(outputId = "FirePlot")),
                  tabPanel("Zoom In", plotOutput(outputId = "PerimPlot"))
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Weights",
                           # Weighting inputs
                           fluidRow(
                             column(3,
                                    numericInput(inputId = "WI.wt", label = "Within unburned island NSO habitat value", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "OUT.wt", label = "Surrounding area (2.5 km) NSO habitat value", value = 1, min = 1, max = 5, step = 1))
                           ),
                           hr(),
                           fluidRow(
                             column(3,
                                    numericInput(inputId = "WI.AREA.wt", label = "Area of unburned island", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "WI.S.HS.wt", label = "% suitable habitat", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "WI.HS.wt", label = "% highly suitable habitat", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "WI.CORE.wt", label = "Core habitat present", value = 1, min = 1, max = 5, step = 1))
                           ),
                           hr(),
                           fluidRow(
                             column(3,
                                    numericInput(inputId = "OUT.S.HS.wt", label = "% suitable habitat within 2.5 km", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "OUT.HS.wt", label = "% highly suitable habitat within 2.5 km", value = 1, min = 1, max = 5, step = 1)),
                             column(3,
                                    numericInput(inputId = "OUT.CORE.wt", label = "% of core habitat within 2.5 km", value = 1, min = 1, max = 5, step = 1), style = "margin-top: 20px;"),
                             column(3)
                           ),
                           fluidRow(
                             checkboxInput(inputId = "checkweight", label = "Default weights", value = T)
                           )),
                  tabPanel("Thresholds",
                           # Establishing Thresholds
                           #"wi.area.t", "wi.suit.high.t", "wi.high.t", "wi.core.t", "out.suit.high.t", "out.high.t", "out.core.t"
                           fluidRow(
                             column(4, tags$b("Area of unburned island (ha)"), align = "center", style = "margin-top: 25px;"),
                             column(4,
                                    numericInput(inputId = "wi.area.f", value = 0.18, label = "False")),
                             column(4,
                                    numericInput(inputId = "wi.area.t", value = 418, label = "True")
                             )),
                           fluidRow(
                             column(4, tags$b("% suitable habitat"), align = "center"),
                             column(4,
                                    numericInput(inputId = "wi.suit.high.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "wi.suit.high.t", value = 100, label = NULL)
                             )),
                           fluidRow(
                             column(4, tags$b("% highly suitable habitat"), align = "center"),
                             column(4,
                                    numericInput(inputId = "wi.high.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "wi.high.t", value = 100, label = NULL)
                             )),
                           fluidRow(
                             column(4, tags$b("Presence of core habitat"), align = "center"),
                             column(4,
                                    numericInput(inputId = "wi.core.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "wi.core.t", value = 1, label = NULL)
                             )),
                           fluidRow(
                             column(4, tags$b("% suitable habitat within 2.5 km"), align = "center"),
                             column(4,
                                    numericInput(inputId = "out.suit.high.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "out.suit.high.t", value = 75, label = NULL)
                             )),
                           fluidRow(
                             column(4, tags$b("% highly suitable habitat within 2.5 km"), align = "center"),
                             column(4,
                                    numericInput(inputId = "out.high.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "out.high.t", value = 75, label = NULL)
                             )),
                           fluidRow(
                             column(4, tags$b("% core habitat within 2.5 km"), align = "center"),
                             column(4,
                                    numericInput(inputId = "out.core.f", value = 0, label = NULL)),
                             column(4,
                                    numericInput(inputId = "out.core.t", value = 50, label = NULL)
                             )),
                           fluidRow(checkboxInput(inputId = "checkthresh", label = "Default thresholds", value = T))
                  )
      )
    ),
    mainPanel(
      plotOutput(outputId = "treediagram")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      # Color select
      selectInput(inputId = "inColor", 
                  label = "Color Refugia by:", 
                  choices = criteria),
      # Download
      downloadButton(outputId = "downloadFire", label = "Download fire perimeter"),
      downloadButton(outputId = "downloadUnb", label = "Download unburned island"),
      br(), br(),
      fluidRow(column(4, offset = 8 
      ))
    ),
    mainPanel(
      fluidRow(column(1, plotOutput(outputId = "LegendPlot", width = 100, height = 180)),
               column(11, plotOutput(outputId = "UnbPlot", width = 800, height = 800)))
      
      #tableOutput("t.table")
    )
  )
)}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  fire <- reactive({input$inFireName})
  fire.sel <- reactive({perims[perims$FireName %in% input$inFireName,]})
  unb.sel <- reactive({unbs[unbs$FireName %in% input$inFireName,]})
  
  # Plot large map (zoomed out)
  output$FirePlot <- renderPlot({
    plot(wa)
    plot(fire.sel(), col = "red", border = "red", lwd = 1, add = T)
  })
  output$PerimPlot <- renderPlot({
    plot(fire.sel())
    plot(unb.sel(), col = "gray50", border = NA, add = T)
  })
  output$table <- renderTable(FireInfo[FireInfo$Name %in% input$inFireName,], colnames = T, align = "c")
  output$treediagram <- renderPlot({
    par(mfrow=c(1,1))
    par(mar=c(0,0,0,0))
    openplotmat()
    
    elpos<-coordinates(c(1,2,7))
    ##draw arrows from each row to next row
    treearrow(from=elpos[1,],to=elpos[2:3,],lwd=4, arr.side = 0)  
    treearrow(from=elpos[2,],to=elpos[4:7,],lwd=4, arr.side = 0)
    treearrow(from=elpos[3,],to=elpos[8:10,],lwd=4, arr.side = 0)
    
    
    label.1 <- c("Northern Spotted owl (NSO)\n habitat value",
                 "Within unburned island\n NSO habitat value", "Surrounding area (2.5 km)\n NSO habitat value",
                 "Area of unburned island", "% suitable habitat", "% highly suitable habitat", "Presence of core habitat",
                 "% suitable habitat\n within 2.5 km", "% highly suitable habitat\n within 2.5 km", "% of core habitat\n within 2.5 km")
    label.2 <- c("\nFinal Value", paste0("\nWeight: ", weights()))
    label.3 <- paste0(label.1, label.2)
    
    ##plot text boxes
    for (i in 1:10) textrect(elpos[i,],radx=0.065,rady=0.05,lab=label.3[i], cex = 0.8, shadow.size = 0)
  })
  
  # Default checkboxes
  observeEvent(input$checkweight, {
    toggleState(id = "WI.wt")
    toggleState(id = "OUT.wt")
    toggleState(id = "WI.AREA.wt")
    toggleState(id = "WI.S.HS.wt")
    toggleState(id = "WI.HS.wt")
    toggleState(id = "WI.CORE.wt")
    toggleState(id = "OUT.S.HS.wt")
    toggleState(id = "OUT.HS.wt")
    toggleState(id = "OUT.CORE.wt")
    updateNumericInput(session, "WI.wt", value = 1)
    updateNumericInput(session, "OUT.wt", value = 1)
    updateNumericInput(session, "WI.AREA.wt", value = 1)
    updateNumericInput(session, "WI.S.HS.wt", value = 1)
    updateNumericInput(session, "WI.HS.wt", value = 1)
    updateNumericInput(session, "WI.CORE.wt", value =1)
    updateNumericInput(session, "OUT.S.HS.wt", value = 1)
    updateNumericInput(session, "OUT.HS.wt", value = 1)
    updateNumericInput(session, "OUT.CORE.wt", value = 1)
  })
  weights <- reactive(c(input$WI.wt, input$OUT.wt, 
                        input$WI.AREA.wt, input$WI.S.HS.wt, input$WI.HS.wt, input$WI.CORE.wt, 
                        input$OUT.S.HS.wt, input$OUT.HS.wt, input$OUT.CORE.wt))
  
  observeEvent(input$checkthresh, {
    toggleState(id = "wi.area.t")
    toggleState(id = "wi.suit.high.t")
    toggleState(id = "wi.high.t")
    toggleState(id = "wi.core.t")
    toggleState(id = "out.suit.high.t")
    toggleState(id = "out.high.t")
    toggleState(id = "out.core.t")
    toggleState(id = "wi.area.f")
    toggleState(id = "wi.suit.high.f")
    toggleState(id = "wi.high.f")
    toggleState(id = "wi.core.f")
    toggleState(id = "out.suit.high.f")
    toggleState(id = "out.high.f")
    toggleState(id = "out.core.f")
    updateNumericInput(session, "wi.area.t", value = 418)
    updateNumericInput(session, "wi.suit.high.t", value = 100)
    updateNumericInput(session, "wi.high.t", value = 100)
    updateNumericInput(session, "wi.core.t", value = 1)
    updateNumericInput(session, "out.suit.high.t", value = 75)
    updateNumericInput(session, "out.high.t", value = 75)
    updateNumericInput(session, "out.core.t", value = 50)
    updateNumericInput(session, "wi.area.f", value = 0.18)
    updateNumericInput(session, "wi.suit.high.f", value = 0)
    updateNumericInput(session, "wi.high.f", value = 0)
    updateNumericInput(session, "wi.core.f", value = 0)
    updateNumericInput(session, "out.suit.high.f", value = 0)
    updateNumericInput(session, "out.high.f", value = 0)
    updateNumericInput(session, "out.core.f", value = 0)
  })
  
  thresh <- reactive(c(input$wi.area.t, input$wi.suit.high.t, input$wi.high.t, input$wi.core.t, 
                       input$out.suit.high.t, input$out.high.t, input$out.core.t, 
                       input$wi.area.f, input$wi.suit.high.f, input$wi.high.f, input$wi.core.f, 
                       input$out.suit.high.f, input$out.high.f, input$out.core.f))
  
  Cvt2Fz <- function(True, False, variable){
    x <- df()[,variable]
    t <- thresh()[True]
    f <- thresh()[False]
    m <- 2/(t-f)
    b <- 1-m*t
    ret <- (m*x)+b
    ret <- ifelse(ret > 1, 1, ret)
    ret <- ifelse(ret < -1, -1, ret)
    return(ret)
  }
  
  df <- reactive(replace(unb.sel()@data, is.na(unb.sel()@data), 0))
  df.fz2 <- reactive(data.frame(F_Area = Cvt2Fz(1, 8, "Area"),
                                F_Suit = Cvt2Fz(2, 9, "Suit"),
                                F_H_Suit = Cvt2Fz(3, 10, "H_Suit"),
                                F_Core = Cvt2Fz(4, 11, "Core"),
                                F_Suit25 = Cvt2Fz(5, 12, "Suit25"),
                                F_H_Suit25 = Cvt2Fz(6, 13, "H_Suit25"),
                                F_Core25 = Cvt2Fz(7, 14, "Core25")))
  df.fz1 <- reactive(replace(df.fz2(), is.na(df.fz2()), -1))
  F_Value_WI <- reactive((weights()[3]*df.fz1()$F_Area + 
                            weights()[4]*df.fz1()$F_Suit + 
                            weights()[5]*df.fz1()$F_H_Suit + 
                            weights()[6]*df.fz1()$F_Core) / 4) # Union (mean)
  F_Value_Out <- reactive((weights()[7]*df.fz1()$F_Suit25 + 
                             weights()[8]*df.fz1()$F_H_Suit25 + 
                             weights()[9]*df.fz1()$F_Core25) / 3) # Union (mean)
  F_Value_Ref <- reactive((weights()[1]*F_Value_WI() + 
                             weights()[2]*F_Value_Out()) / 2) # Union (mean)
  F_Rank <- reactive(rank(-F_Value_Ref()))
  
  df.fz <- reactive(data.frame(
    df(), df.fz1(),
    F_Value_WI = F_Value_WI(),
    F_Value_Out = F_Value_Out(),
    F_Value_Ref = F_Value_Ref(),
    F_Rank = F_Rank()
  ))
  
  Col <- function(df, crit){
    cl <- data.frame(crit = sort(unique(crit)), 
                     col = colorRampPalette(rev(c("#004529", "#78c679", "#f7fcb9")))
                     (length(unique(crit))))
    cl <- merge(data.frame(ID = df$ID, crit = crit), cl, by = "crit")
    cl <- as.character(cl[order(cl$ID), "col"])
    return(cl)
  }
  
  col <- reactive({
    if(input$inColor == criteria[1]){
      Col(df.fz(), df.fz()$F_Value_Ref)
    } else if(input$inColor == criteria[2]){
      Col(df.fz(), df.fz()$F_Value_WI)
    } else if(input$inColor == criteria[3]){
      Col(df.fz(), df.fz()$F_Value_Out)
    } else if(input$inColor == criteria[4]){
      Col(df.fz(), df.fz()$F_Area)
    } else if(input$inColor == criteria[5]){
      Col(df.fz(), df.fz()$F_Suit)
    } else if(input$inColor == criteria[6]){
      Col(df.fz(), df.fz()$F_H_Suit)
    } else if(input$inColor == criteria[7]){
      Col(df.fz(), df.fz()$F_Core)
    } else if(input$inColor == criteria[8]){
      Col(df.fz(), df.fz()$F_Suit25)
    } else if(input$inColor == criteria[9]){
      Col(df.fz(), df.fz()$F_H_Suit25)
    } else if(input$inColor == criteria[10]){
      Col(df.fz(), df.fz()$F_Core25)
    }
  })
  
  output$UnbPlot <- renderPlot({
    par(mar = c(0,0,0,0))
    plot(unb.sel(), border = col(), col = col())
    plot(fire.sel(), border = "gray70", lwd = 2, add = T)
  })
  
  output$LegendPlot <- renderPlot({
    par(mar = c(0,0,0,0))
    legend_image <- as.raster(matrix(colorRampPalette(c("#004529", "#78c679", "#f7fcb9"))(100), ncol = 1))
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
    text(x=1.5, y = c(0.05, 0.95), labels = c("Low\nvalue", "High\nvalue"))
    rasterImage(legend_image, 0, 0, 1,1)
  })
  
  
  output$t.table <- renderTable(df())
}

# Run the application 
shinyApp(ui = ui, server = server)

