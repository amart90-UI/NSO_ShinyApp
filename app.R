########################################################################################
########################################################################################
##            Ranking Unburned Islands for Northern Spotted Owl Management            ##
##                               Anthony J.  Martinez                                 ##
##                                    Spring 2019                                     ##
########################################################################################
########################################################################################


#################################################
##    Load packages and data                   ##
#################################################
library(shiny)
library(shinyjs)
library(rgdal)
library(rgeos)
library(zip)
library(raster)
library(diagram)
library(ggmap)
library(ggplot2)
library(broom)
load("NSO_Data")

#################################################
##    Define the UI for application            ##
#################################################
ui <- {fluidPage(
  useShinyjs(),
  titlePanel("Fire Refugia Ranking for Spotted Owl"),
  # Top section
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "inFireName", 
                  label = "Select a fire", 
                  choices = c("Table Mountain", "Jolly Mountain", "Rex Creek", "Tyee Creek", "B&B")),
      br(),
      tableOutput('table')
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Location", 
                           h3("Location of Fire", align = 'center'),
                           div(plotOutput(outputId = "FirePlot", width = 600, height = 600)), align = 'center'),
                  tabPanel("Zoom In", 
                           h3("Fire perimeter and unburned islands", align = 'center'),
                           div(plotOutput(outputId = "PerimPlot", width = 600, height = 600), align = 'center'))
      )
    )
  ),
  # Middle section
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Weights",
                           helpText("Specify weight of criteria when performing weighted union (mean) to combine criteria. Equally weighted by default"),
                           br(),
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
                           helpText("Specify the thresholds at which a criteria is entirely false or entirely true.,
                                    View the \"Additional Threshold Information\" tab for further explanation."),
                           br(),
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
                  ),
                  tabPanel("Additional Theshold Information",
                           helpText("Thresholds are used to determine the \"fuzzy values\" for each criteria.",
                                          "These fuzzy values describe the degree of truth or falseness for a given proposition based on the input value of a given criteria.",
                                          "The fuzzy values range from -1 (entirely false) to +1 (entirely true).",
                                          "The thresholds determine the criteria value at which it addresses a criteria is entirely true or entirely false"),
                           plotOutput(outputId = "FuzzyPlot"),
                           helpText("Above, an example of how fuzzy scores are determined. To determine the fuzzy value indicating the truth of the proposition \"the refugia is isolated,\" its distance (in meters) to the nearest live edge forest edge is compared to the line above.",
                                          "Any distance \u2264 200 m would return -1, or entirely false.  Any value \u2265 800 m would return 1 or completely true.",
                                          "Distances between 200 m and 800 m would return a value between -1 and 1, with 500 m returning a fuzzy value of 0 (dotted line).",
                                          "The thresholds (dashed lines) are manually set during the model building process."),
                           uiOutput("url"))
      )
    ),
    mainPanel(
      h3("Logic tree for model", align = 'center'),
      div(plotOutput(outputId = "treediagram"), align = 'center'),
      helpText("This shows the heriracy of the model.",
               "The root node (top box) is the final \"refugia value\" for determining an unburned island\'s importance for spotted owl habitat.",
               "The intermediate criteria (middle two) are determined by taking the mean of the input criteria (bottom row).")
    )
  ),
  # Bottom section
  sidebarLayout(
    sidebarPanel(
      # Color select
      selectInput(inputId = "inColor", 
                  label = "Color Refugia by:", 
                  choices = criteria),
      br(),
      # Download
      helpText("Download fire perimeter or unburned islands with all ranking data."),
      downloadButton(outputId = "downloadFire", label = "Download fire perimeter"),
      downloadButton(outputId = "downloadUnb", label = "Download unburned island"),
      br(), br(),
      fluidRow(column(4, offset = 8 
      ))
    ),
    mainPanel(
      fluidRow(column(11, offset = 1, h3("Ranked unburned islands", align = 'center'))),
      fluidRow(column(1, plotOutput(outputId = "LegendPlot", width = 100, height = 180)),
               column(11, plotOutput(outputId = "UnbPlot", width = 800, height = 800), align = 'center'))
    )
  )
)}

#################################################
##    Define server logic for the application  ##
#################################################
server <- function(input, output, session) {
  fire <- reactive({input$inFireName})
  fire.sel <- reactive({perims[perims$FireName %in% input$inFireName,]})
  unb.sel <- reactive({unbs[unbs$FireName %in% input$inFireName,]})
  
  # Plot location map (zoomed out)
  output$FirePlot <- renderPlot({
    plot(pnw[pnw@data$NAME_1 %in% c("Washington", "Oregon"),])
    plot(fire.sel(), col = "red", border = "red", lwd = 1, add = T)
  })
  
  # Plot perimeter and unburned island  map (zoomed in)
  #output$PerimPlot <- renderPlot({
  #  plot(fire.sel())
  #  plot(unb.sel(), col = "gray50", border = NA, add = T)
  #})
  output$PerimPlot <- renderPlot({
    if(input$inFireName == "Table Mountain"){
      plot(map.table)
    } else if(input$inFireName == "Jolly Mountain"){
      plot(map.jolly)
    } else if(input$inFireName == "Rex Creek"){
      plot(map.rex)
    } else if(input$inFireName == "Tyee Creek"){
      plot(map.tyee)
    } else if(input$inFireName == "B&B"){
      plot(map.BB)
    }
  })
  
  # Build fire information table
  output$table <- renderTable(FireInfo[FireInfo$Name %in% input$inFireName,], colnames = T, align = "c")
  
  # Plot final ranked unburned island map (colored)
  output$UnbPlot <- renderPlot({
    par(mar = c(0,0,0,0))
    plot(unb.sel(), border = col(), col = col())
    plot(fire.sel(), border = "gray70", lwd = 2, add = T)
  })
  
  # Plot legend
  output$LegendPlot <- renderPlot({
    par(mar = c(0,0,0,0))
    legend_image <- as.raster(matrix(colorRampPalette(c("#004529", "#78c679", "#f7fcb9"))(100), ncol = 1))
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
    text(x=1.5, y = c(0.05, 0.95), labels = c("Low\nvalue", "High\nvalue"))
    rasterImage(legend_image, 0, 0, 1,1)
  })
  
  # Plot fuzzy threshold example
  output$FuzzyPlot <- renderPlot({
    plot(x = -100, y = -10, xlim = c(0,1000), ylim = c(-1,1), 
         xlab = "Distance to fire perimeter (m)", ylab = "Fuzzy value (Truth/Falseness)", main = "The refugia is isolated")
    lines(x = c(0, 200, 800, 1100), y = c(-1, -1, 1, 1), lwd = 2)
    lines(x = c(200, 200), y = c(-2, -0.5), lty = 5, lwd = 1.5)
    lines(x = c(800, 800), y = c(-2, 1.06), lty = 5, lwd = 1.5)
    lines(x = c(-100, 500, 500), y = c(0, 0, -1.5), lty = 3, lwd = 1.5)
  })
  
  # Build fire perimeter download
  output$downloadFire <- downloadHandler(
    filename = 'fire_perim.zip',
    content = function(file) {
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
      writeOGR(fire.sel(), dsn="fire_perim.shp", layer="fire_perim", driver="ESRI Shapefile")
      zip(zipfile='fire_perim.zip', files=Sys.glob("fire_perim.*"))
      file.copy("fire_perim.zip", file)
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
    }
  )
  
  # Build unburned island download
  unb.sel.app <- reactive(SpatialPolygonsDataFrame(unb.sel(), data = df.fz()))
  output$downloadUnb <- downloadHandler(
    filename = 'unb_isl.zip',
    content = function(file) {
      if (length(Sys.glob("unb_isl.*")) > 0){
        file.remove(Sys.glob("unb_isl.*"))
      }
      writeOGR(unb.sel.app(), dsn="unb_isl.shp", layer="unb_isl", driver="ESRI Shapefile")
      write.csv(df.fz(), file =  "unb_isl.csv")
      zip(zipfile='unb_isl.zip', files=Sys.glob("unb_isl.*"))
      file.copy("unb_isl.zip", file)
      if (length(Sys.glob("unb_isl.*")) > 0){
        file.remove(Sys.glob("unb_isl.*"))
      }
    }
  )
  
  # EEMS URL
  url <- a("See the EEMS website for more information", target="_blank", 
           href = "https://consbio.org/products/tools/environmental-evaluation-modeling-system-eems")
  output$url <- renderUI(tagList(url))
  
  # Build model logic tree
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
                 "\nArea of unburned island", "\n% suitable habitat", "\n% highly suitable habitat", "\nPresence of core habitat",
                 "% suitable habitat\n within 2.5 km", "% highly suitable habitat\n within 2.5 km", "% of core habitat\n within 2.5 km")
    label.2 <- c("\nFinal Value", paste("\nWeight:", weights()))
    unit <- c("ha", "%", "%", "", "%", "%", "%")
    label.3 <- c(rep("", 3), paste0("\nThresholds: ", thresh()[8:14], unit, " ; ", thresh()[1:7], unit))
    label.4 <- paste0(label.1, label.2, label.3)
    
    ##plot text boxes
    for (i in 1:10) textrect(elpos[i,],radx=0.065,rady=0.06,lab=label.4[i], cex = 0.8, shadow.size = 0)
  })
  
  # Create default weights checkbox
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
  
  # Create default thresholds checkbox
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
 
  # Calculate rankings
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
  
  # Define colors
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
  
}

#################################################
##    Run the application                      ##
#################################################
shinyApp(ui = ui, server = server)

