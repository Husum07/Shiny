# Makrofordeling Shiny Dashboard
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Makrofordeling Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Information", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Personlige data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  numericInput("weight", "Vægt (kg):", 100, min = 40, max = 200),
                  numericInput("height", "Højde (cm):", 190, min = 140, max = 220),
                  numericInput("age", "Alder (år):", 30, min = 18, max = 100),
                  selectInput("gender", "Køn:", 
                              choices = c("Mand" = "male", "Kvinde" = "female")),
                  selectInput("activity", "Aktivitetsniveau:",
                              choices = c("Stillesiddende (lidt eller ingen motion)" = 1.2,
                                          "Let aktiv (let motion/sport 1-3 dage/uge)" = 1.375,
                                          "Moderat aktiv (moderat motion/sport 3-5 dage/uge)" = 1.55,
                                          "Meget aktiv (hård motion/sport 6-7 dage/uge)" = 1.725,
                                          "Ekstremt aktiv (meget hård motion og fysisk arbejde)" = 1.9))
                ),
                box(
                  title = "Mål og præferencer",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("goal", "Mål:",
                              choices = c("Vedligehold" = "maintain",
                                          "Vægttab" = "lose",
                                          "Vægtøgning" = "gain")),
                  conditionalPanel(
                    condition = "input.goal != 'maintain'",
                    sliderInput("calorie_adjust_kcal", "Kalorie justering (kcal):", 
                                min = 100, max = 1000, value = 500, step = 50)
                  ),
                  sliderInput("protein_ratio", "Protein (g/kg kropsvægt):", 
                              min = 1.2, max = 2.5, value = 1.8, step = 0.1),
                  sliderInput("fat_percentage", "Fedt (% af totale kalorier):", 
                              min = 15, max = 40, value = 25, step = 5)
                )
              ),
              fluidRow(
                box(
                  title = "Makrofordeling",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("macro_summary")
                )
              ),
              fluidRow(
                box(
                  title = "Visuel fordeling af makronæringsstoffer",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("macro_piechart")
                ),
                box(
                  title = "Daglig kaloriefordeling",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("calorie_barchart")
                )
              )
      ),
      
      # Info tab
      tabItem(tabName = "info",
              box(
                title = "Om makrofordeling",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                HTML("<h4>Hvordan beregner vi dit kaloriebehov?</h4>
               <p>Vi bruger Mifflin-St Jeor formlen til at beregne dit basale stofskifte (BMR), og ganger derefter med din aktivitetsfaktor.</p>
               <h4>Hvad er makronæringsstoffer?</h4>
               <p>Makronæringsstoffer er de større næringsstofgrupper:</p>
               <ul>
                 <li><b>Protein</b>: 4 kalorier per gram. Vigtigt for muskelopbygning og reparation.</li>
                 <li><b>Kulhydrater</b>: 4 kalorier per gram. Vigtig energikilde for kroppen.</li>
                 <li><b>Fedt</b>: 9 kalorier per gram. Nødvendigt for hormonproduktion og cellesundhed.</li>
               </ul>
               <h4>Anbefalinger for makrofordeling</h4>
               <p>De generelle anbefalinger varierer baseret på mål:</p>
               <ul>
                 <li><b>Vægttab</b>: Højere protein, moderat fedt, lavere kulhydrat</li>
                 <li><b>Muskelopbygning</b>: Højere protein, moderat fedt, højere kulhydrat</li>
                 <li><b>Vedligehold</b>: Balanceret mellem alle makronæringsstoffer</li>
               </ul>")
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Calculate BMR using Mifflin-St Jeor Equation
  calculate_bmr <- reactive({
    if(input$gender == "male") {
      return((10 * input$weight) + (6.25 * input$height) - (5 * input$age) + 5)
    } else {
      return((10 * input$weight) + (6.25 * input$height) - (5 * input$age) - 161)
    }
  })
  
  # Calculate TDEE (Total Daily Energy Expenditure)
  calculate_tdee <- reactive({
    bmr <- calculate_bmr()
    activity_factor <- as.numeric(input$activity)
    return(bmr * activity_factor)
  })
  
  # Calculate target calories based on goal
  calculate_target_calories <- reactive({
    tdee <- calculate_tdee()
    
    if(input$goal == "maintain") {
      return(tdee)
    } else if(input$goal == "lose") {
      return(tdee - input$calorie_adjust_kcal)
    } else { # gain
      return(tdee - input$calorie_adjust_kcal)
    }
  })
  
  # Calculate macros
  calculate_macros <- reactive({
    target_calories <- calculate_target_calories()
    
    # Protein calculation (based on bodyweight)
    protein_g <- input$weight * input$protein_ratio
    protein_cals <- protein_g * 4
    
    # Fat calculation (based on percentage of total calories)
    fat_cals <- target_calories * (input$fat_percentage/100)
    fat_g <- fat_cals / 9
    
    # Remaining calories for carbs
    carb_cals <- target_calories - protein_cals - fat_cals
    carb_g <- carb_cals / 4
    
    return(list(
      calories = round(target_calories),
      protein = list(g = round(protein_g), cals = round(protein_cals), pct = round(protein_cals/target_calories*100)),
      fat = list(g = round(fat_g), cals = round(fat_cals), pct = round(fat_cals/target_calories*100)),
      carbs = list(g = round(carb_g), cals = round(carb_cals), pct = round(carb_cals/target_calories*100))
    ))
  })
  
  # Output text summary
  output$macro_summary <- renderText({
    macros <- calculate_macros()
    
    goal_text <- switch(input$goal,
                        "maintain" = "vedligehold",
                        "lose" = "vægttab",
                        "gain" = "vægtøgning")
    
    paste0(
      "Din basale kalorieforbrug (BMR): ", round(calculate_bmr()), " kalorier\n",
      "Dit totale daglige energiforbrug (TDEE): ", round(calculate_tdee()), " kalorier\n",
      "Dit kaloriemål for ", goal_text, ": ", macros$calories, " kalorier\n\n",
      "Anbefalet makrofordeling:\n",
      "- Protein: ", macros$protein$g, "g (", macros$protein$pct, "% - ", macros$protein$cals, " kalorier)\n",
      "- Fedt: ", macros$fat$g, "g (", macros$fat$pct, "% - ", macros$fat$cals, " kalorier)\n",
      "- Kulhydrater: ", macros$carbs$g, "g (", macros$carbs$pct, "% - ", macros$carbs$cals, " kalorier)"
    )
  })
  
  # Pie chart for macro distribution
  output$macro_piechart <- renderPlot({
    macros <- calculate_macros()
    
    macro_data <- data.frame(
      Makronæringsstof = c("Protein", "Fedt", "Kulhydrater"),
      Procent = c(macros$protein$pct, macros$fat$pct, macros$carbs$pct)
    )
    
    ggplot(macro_data, aes(x = "", y = Procent, fill = Makronæringsstof)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(Procent, "%")), position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18)
      ) +
      labs(fill = "Makronæringsstof")
  })
  
  # Bar chart for calorie distribution
  output$calorie_barchart <- renderPlot({
    macros <- calculate_macros()
    
    calorie_data <- data.frame(
      Makronæringsstof = c("Protein", "Fedt", "Kulhydrater"),
      Kalorier = c(macros$protein$cals, macros$fat$cals, macros$carbs$cals)
    )
    
    ggplot(calorie_data, aes(x = Makronæringsstof, y = Kalorier, fill = Makronæringsstof)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Kalorier), vjust = -0.5) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(y = "Kalorier", title = "Kaloriefordeling") +
      theme(legend.position = "none")
  })
}

# Run the app
shinyApp(ui = ui, server = server)