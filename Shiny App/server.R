library(rsconnect)
library(shiny)
library(ggplot2)
library(scales)

function(input, output, session) {
  cap_lfp = 69.45
  cap_nmc = 69.21
  cap_nca = 69.21
  
  # Define the amount of each critical mineral in each chemistry (LFP, NMC, NCA), divided by 70 (kWh)
  li_amt <- c(6.14/cap_lfp, 6.43/cap_nmc, 6.91/cap_nca)
  co_amt <- c(0.00/cap_lfp, 5.32/cap_nmc, 2.86/cap_nca)
  ni_amt <- c(0.00/cap_lfp, 42.42/cap_nmc, 45.59/cap_nca)
  mn_amt <- c(0.00/cap_lfp, 4.96/cap_nmc, 0.00/cap_nca)
  al_amt <- c(64.95/cap_lfp, 49.09/cap_nmc, 51.2/cap_nca)
  gr_amt <- c(69.10/cap_lfp, 61.76/cap_nmc, 62.06/cap_nca)
  
  # Define the constant costs for each chemistry
  other_prod_costs <- rbind(
    data.frame(
      Cost = rep(c("Implied Cathode SC Non-Material Costs"), times=3),
      Value = c(1378.31/cap_lfp, 2061.20/cap_nmc, 2430.79/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ), 
    data.frame(
      Cost = rep(c("Implied Anode SC Non-Material Costs"), times=3),
      Value = c(551.89/cap_lfp, 493.20/cap_nmc, 495.66/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Implied Other Material Costs"), times=3),
      Value = c(3501.65/cap_lfp, 3345.74/cap_nmc, 3443.18/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Direct Labor"), times=3),
      Value = c(172.26/cap_lfp, 150.18/cap_nmc, 151.22/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Energy"), times=3),
      Value = c(128.20/cap_lfp, 113.10/cap_nmc, 114.22/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Variable Overhead"), times=3),
      Value = c(229.32/cap_lfp, 205.78/cap_nmc, 207.10/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Depreciation"), times=3),
      Value = c(586.64/cap_lfp, 532.92/cap_nmc, 536.12/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Other Fixed Costs"), times=3),
      Value = c(435.49/cap_lfp, 396.10/cap_nmc, 398.99/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Profits"), times=3),
      Value = c(464.25/cap_lfp, 439.46/cap_nmc, 445.50/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    ),
    data.frame(
      Cost = rep(c("Warranty"), times=3),
      Value = c(429.82/cap_lfp, 491.19/cap_nmc, 513.37/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    )
  )
  
  # Reset button functionality
  observeEvent(input$reset, {
    updateSliderInput(session, "lithiumPrice", value = 26.78)
    updateSliderInput(session, "cobaltPrice", value = 47)
    updateSliderInput(session, "nickelPrice", value = 12.36)
    updateSliderInput(session, "manganesePrice", value = 5.71)
    updateSliderInput(session, "aluminumPrice", value = 2.12)
    updateSliderInput(session, "graphitePrice", value = 1.01)
  })
  
  # Reactive expressions to multiply the input values by the factors
  totLi <- reactive({
    sapply(li_amt, function(factor) input$lithiumPrice * factor)
  })
  
  totCo <- reactive({
    sapply(co_amt, function(factor) input$cobaltPrice * factor)
  })

  totNi <- reactive({
    sapply(ni_amt, function(factor) input$nickelPrice * factor)
  })
  
  totMn <- reactive({
    sapply(mn_amt, function(factor) input$manganesePrice * factor)
  })
  
  totAl <- reactive({
    sapply(al_amt, function(factor) input$aluminumPrice * factor)
  })
  
  totGr <- reactive({
    sapply(gr_amt, function(factor) input$graphitePrice * factor)
  })
  
  # Reactive expression for mineral price
  originalData <- reactive({
    data <- data.frame(
      Cost = rep(c("Lithium", "Cobalt", "Nickel", "Manganese", "Aluminum", "Graphite"), each = 3),
      Value = c(totLi(), totCo(), totNi(), totMn(), totAl(), totGr()),
      Chem = rep(c("LFP", "NMC811", "NCA"), times = 6)
    )
    data$Cost <- factor(data$Cost, levels = c("Graphite", "Aluminum", "Manganese", "Nickel", "Cobalt", "Lithium"))
    data
  })
  
  # Reactive expression to create normalized prices for supply chain scenarios
  normalizedData <- reactive({
    data <- originalData()
    
    # Calculate total cost for each chemistry
    totalCost <- aggregate(Value ~ Chem, data = data, sum)
    
    # Normalize values
    normalized_data <- merge(data, totalCost, by = "Chem")
    normalized_data$NormalizedValue <- normalized_data$Value.x / normalized_data$Value.y
    
    normalized_data <- normalized_data[, c("Cost", "NormalizedValue", "Chem")]
  })
  
  # Render the ggplot horizontal stacked bar plot
  output$pricePlot <- renderPlot({
    data <- originalData()
    
    if (input$includeConstantCosts) {
      data <- rbind(data, other_prod_costs)
      data$Cost <- factor(data$Cost, levels = c("Warranty", "Profits", "Other Fixed Costs",
                                                "Depreciation", "Variable Overhead", "Energy",
                                                "Direct Labor", "Implied Other Material Costs",
                                                "Implied Anode SC Non-Material Costs",
                                                "Implied Cathode SC Non-Material Costs",
                                                "Graphite", "Aluminum", "Manganese", "Nickel", 
                                                "Cobalt", "Lithium"))
    }

    # Determine xlim based on checkbox input
    xl <- if (input$includeConstantCosts) 180 else 50
    
    ggplot(data, aes(x = Value, y = Chem, fill = Cost)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Lithium" = "#337538", "Cobalt" = "#2e2585", "Nickel" = "#5da899", 
                                   "Manganese" = "#dccd7d", "Aluminum" = "#dddddd", "Graphite" = "#434343",
                                   "Implied Cathode SC Non-Material Costs" = "#F1C233",
                                   "Implied Anode SC Non-Material Costs" = "#FFE59A",
                                   "Implied Other Material Costs" = "#cc0201",
                                   "Direct Labor" = "#cc4124ff", "Energy" = "#b45f07ff",
                                   "Variable Overhead" = "#ff0002ff", "Depreciation" = "#dd5050ff",
                                   "Other Fixed Costs" = "#e06666ff", "Profits" = "#ea9999",
                                   "Warranty" = "#f4cccc"
                                   )) +
      labs(title = "Cost of Production, per kWh", subtitle = "Cost for 500,000 packs per year, 70 kWh packs; Fig. 3 in paper. Please note that all non-mineral costs are fixed (constant) for this tool.", 
           x = "Cost", y = "Chemistry") +
      xlim(0, xl) +  # Set fixed range for x-axis
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom",  # Move legend to the bottom
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  output$graphPanel <- renderUI({
    num_graphs <- input$numGraphs
    
    # Generate a list of columns with fixed width
    plot_list <- lapply(1:num_graphs, function(i) {
      plotname <- paste0("Supply Chain Option", i)
      li_cb <- paste0("Li", i)
      co_cb <- paste0("Co", i)
      ni_cb <- paste0("Ni", i)
      mn_cb <- paste0("Mn", i)
      al_cb <- paste0("Al", i)
      gr_cb <- paste0("Gr", i)
      
      output[[plotname]] <- renderPlot({
        data <- normalizedData()
        
        # Filter data based on the state of checkboxes
        if (!input[[li_cb]]) {
          data <- data[data$Cost != "Lithium", ]
        }
        if (!input[[co_cb]]) {
          data <- data[data$Cost != "Cobalt", ]
        }
        if (!input[[ni_cb]]) {
          data <- data[data$Cost != "Nickel", ]
        }
        if (!input[[mn_cb]]) {
          data <- data[data$Cost != "Manganese", ]
        }
        if (!input[[al_cb]]) {
          data <- data[data$Cost != "Aluminum", ]
        }
        if (!input[[gr_cb]]) {
          data <- data[data$Cost != "Graphite", ]
        }
        
        data$Chem <- factor(data$Chem, levels = c("LFP", "NMC811", "NCA"))
        
        ggplot(data, aes(x = Chem, y = NormalizedValue, fill = Cost)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = c("Lithium" = "#337538", "Cobalt" = "#2e2585", "Nickel" = "#5da899", 
                                       "Manganese" = "#dccd7d", "Aluminum" = "#dddddd", "Graphite" = "#434343")) +
          labs(title = paste("Scenario ", i), x = "Chemistry", y = "Percentage") +
          scale_y_continuous(
            breaks = seq(0, 1, by = 0.1),
            labels = scales::label_percent(),
            limits = c(0,1.01)
          ) +
          theme_minimal() + 
          theme(legend.position = "none", 
                panel.grid.minor = element_line(size = 0.1), 
                panel.grid.major = element_line(size = 0.2),
                plot.title = element_text(size = 14),
                axis.title = element_text(size = 12))
      })
      
      column(width = 2,  
             plotOutput(plotname, height = "300px"),  # Fixed height for consistency
             checkboxInput(li_cb, label = paste("Li, Scenario ", i), value = TRUE),
             checkboxInput(co_cb, label = paste("Co, Scenario ", i), value = TRUE),
             checkboxInput(ni_cb, label = paste("Ni, Scenario ", i), value = TRUE),
             checkboxInput(mn_cb, label = paste("Mn, Scenario ", i), value = TRUE),
             checkboxInput(al_cb, label = paste("Al, Scenario ", i), value = TRUE),
             checkboxInput(gr_cb, label = paste("Gr, Scenario ", i), value = TRUE)
            )
    })
    
    do.call(fluidRow, plot_list)
  })
}