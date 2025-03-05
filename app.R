library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(readxl)
library(googlesheets4)
library(auth0)
library(googledrive)
library(bslib)
library(bsicons)
library(DT)

gs4_auth(
  cache = "token",
  email = "husumnicklas@gmail.com"
)

readRenviron("Renviron.sh")

options(shiny.port =8080)


# Indlæs data
read_budget_data <- function(sheet_name) {
  data <- read_excel("data/budget.xlsx", 
                     sheet = sheet_name)
  
  # Fjern kolonne
  data <- data[, 1:2] 
  
  # Omdøb kolonnerne for ensartethed
  colnames(data) <- c("Kategori", "Beløb")
  
  # Fjern "kr" og konverter beløb til numerisk værdi
  data$Beløb <- as.numeric(gsub("kr|,", "", data$Beløb))
  
  # Fjern NA-værdier og tomme rækker
  data <- na.omit(data)
  
  return(data)
}

# Indlæs data fra Excel
Indtægter <- read_budget_data("Indtægter")
Udgifter <- read_budget_data("Udgifter")

# UI - Brugergrænseflade
ui <- dashboardPage(title = "Budget Dashboard", skin = "blue",
  
  dashboardHeader(title = "SU Budget Nicklas"), # Titel
  
  dashboardSidebar( # Sidebar med navigationsmenu
    collapsed = TRUE, # Sidebar er som standard skjult
    sidebarMenu(
      menuItem("Overblik", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Indtægter", tabName = "indtægter", icon = icon("money-bill")),
      menuItem("Udgifter", tabName = "udgifter", icon = icon("credit-card"))
    )
  ),
  
  dashboardBody( # Dashboardets hovedindhold
    fluidRow( # Værdibokse til at vise total indtægter og udgifter
      valueBoxOutput(width = 3, "totalIndtægter"),
      valueBoxOutput(width = 3, "totalUdgifter")
    ),
    fluidRow( # Faneboks med detaljerede visninger af indtægter og udgifter
      tabBox(
        title = "Overblik", id = "tabs", height = "520px", width = 10,
        tabPanel("Indtægter", plotOutput("indtægterPlot")), # Fane med indtægtsgraf
        tabPanel("Udgifter", plotOutput("udgifterPlot")) # Fane med udgiftsgraf
      )
    )
  )
)

# Server - Bagvedliggende logik
server <- function(input, output, session) {
  
  # Beregn total indtægter og udgifter ved at summere beløbene
  totalIndtægter <- sum(Indtægter$Beløb, na.rm = TRUE) # Grøn boks med samlet beløb
  totalUdgifter <- sum(Udgifter$Beløb, na.rm = TRUE) # Rød boks med samlet beløb
  
  # Beregn procenter for indtægter
  Indtægter$Procent <- Indtægter$Beløb / totalIndtægter * 100
  
  # Output værdiboks for total indtægter
  output$totalIndtægter <- renderValueBox({
    valueBox(
      paste0(format(totalIndtægter, big.mark = "."), " kr"), # Formatterer totalIndtægter med punktum som tusindtalsseparator og tilføjer " kr"
      "Total Indtægter", # Overskrift i boksen
      icon = icon("money-bill"), # Ikon af en pengeseddel for at indikere indtægter
      color = "green" # Grøn farve for at signalere positiv værdi
    )
  })
  
  # Output værdiboks for total udgifter
  output$totalUdgifter <- renderValueBox({
    valueBox(
      paste0(format(totalUdgifter, big.mark = "."), " kr"), # Formatterer totalUdgifter med punktum som tusindtalsseparator og tilføjer " kr"
      "Total Udgifter", # Overskrift i boksen
      icon = icon("credit-card"), # Ikon af et kreditkort for at indikere udgifter
      color = "red" # Rød farve for at signalere en negativ værdi
    )
  })
  
  # Genererer søjlediagram over indtægter pr. kategori med procenter
  output$indtægterPlot <- renderPlot({
    ggplot(Indtægter, aes(x = "", y = Procent, fill = Kategori)) + 
      geom_bar(stat = "identity", position = "stack") +
      coord_flip() + 
      theme_minimal() + 
      labs(title = "Fordeling af indtægter (%)", x = "", y = "Procent (%)", fill = "Kategori") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      geom_text(aes(label = sprintf("%.1f%%", Procent)), 
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 4
      )
  })
  
  # Genererer søjlediagram over udgifter pr. kategori
  output$udgifterPlot <- renderPlot({
    ggplot(Udgifter, aes(x = reorder(Kategori, -Beløb), y = Beløb)) + # Bruger ggplot til at lave et søjlediagram, hvor kategorier sorteres efter beløb
      geom_col(fill = "red") + # Søjlerne får rød farve for at indikere udgifter
      coord_flip() + # Roterer diagrammet horisontalt for bedre læsbarhed
      theme_minimal() + # Minimalistisk tema for et rent look
      labs(title = "Udgifter pr. kategori", x = "Kategori", y = "Beløb (kr)") # Tilføjer titel og akse-labels
  })
}

# Kør Shiny appen
# shinyApp(ui = ui, server = server)
auth0::shinyAppAuth0(ui, server)
