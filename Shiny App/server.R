library(rsconnect)
library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)
library(plyr)

cost_stack <- function(sum_vals, c_factor) {
  # Create copies of sum_vals for each combination of Type and Location
  usa_cost <- sum_vals
  china_cost <- sum_vals
  usa_incent <- sum_vals
  china_incent <- sum_vals
  
  # Add new columns for Type and Location
  usa_cost$Type <- "Costs"
  usa_cost$Location <- "USA"
  
  china_cost$Type <- "Costs"
  china_cost$Location <- "China"
  
  usa_incent$Type <- "Incentives"
  usa_incent$Location <- "USA"
  
  china_incent$Type <- "Incentives"
  china_incent$Location <- "China"
  
  # Combine the data frames into one
  stack_values <- rbind(usa_cost, china_cost, usa_incent, china_incent)
  
  # Update all China values by multiplying by the factor
  stack_values$Value <- ifelse(
    stack_values$Location == "China", 
    stack_values$Value * c_factor,  # Multiply by the factor for China
    stack_values$Value  # Keep the original value for other rows
  )
  
  # Update all incentive values to 0
  stack_values$Value <- ifelse(
    stack_values$Type == "Incentives", 
    0,  # Set to 0 for Incentives
    stack_values$Value  # Keep the original value for other rows
  )
  
  return(stack_values)
}

incent_stack <- function(sum_vals, china) {
  # Create copies of sum_vals for each combination of Type and Location
  usa_cost <- sum_vals
  usa_incent <- sum_vals
  china_cost <- sum_vals
  china_incent <- sum_vals
  
  # Add new columns for Type and Location
  usa_cost$Type <- "Costs"
  usa_cost$Location <- "USA"
  
  usa_incent$Type <- "Incentives"
  usa_incent$Location <- "USA"
  
  china_cost$Type <- "Costs"
  china_cost$Location <- "China"
  
  china_incent$Type <- "Incentives"
  china_incent$Location <- "China"
  
  # Combine the data frames into one
  stack_values <- rbind(usa_cost, china_cost, usa_incent, china_incent)
  
  # Update the "Value" column based on the "Type" factor
  stack_values$Value <- ifelse(
    stack_values$Type == "Costs", 
    0,  # Set to input value for "Incentives" dataset, usually 0 w.r.t. costs
    stack_values$Value  # Keep the original value for other rows
  )
  
  # Set the "Value" column to 0 depending on the location
  if (china) {
    stack_values$Value <- ifelse(
      stack_values$Location == "USA", 
      0,  # Set to 0 for USA data if processing China data
      stack_values$Value  # Keep the original value for other rows
    )
  } else {
    stack_values$Value <- ifelse(
      stack_values$Location == "China", 
      0,  # Set to 0 for China data if processing USA data
      stack_values$Value  # Keep the original value for other rows
    )
  }
  
  return(stack_values)
}

function(input, output, session) {
  
  #############
  #   DATA    #
  #############
  cap_lfp = 69.45
  cap_nmc = 69.21
  cap_nca = 69.21
  
  crit_min <- data.frame(
    Cost = rep(c("Constant Critical Mineral Costs"), times=3),
    Value = c(371.91/cap_lfp, 1141.26/cap_nmc, 1054.16/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  
  other_cath <- data.frame(
    Cost = rep(c("Implied Other Cathode Costs"), times=3),
    Value = c(1378.31/cap_lfp, 2061.20/cap_nmc, 2430.79/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  other_anode <- data.frame(
    Cost = rep(c("Implied Other Anode Costs"), times=3),
    Value = c(551.89/cap_lfp, 493.20/cap_nmc, 495.66/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  other_mat <- data.frame(
    Cost = rep(c("Implied Other Material Costs"), times=3),
    Value = c(3356.74/cap_lfp, 3237.14/cap_nmc, 3332.77/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  labor <- data.frame(
    Cost = rep(c("Direct Labor"), times=3),
    Value = c(172.26/cap_lfp, 150.18/cap_nmc, 151.22/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  energy <- data.frame(
    Cost = rep(c("Energy"), times=3),
    Value = c(128.20/cap_lfp, 113.10/cap_nmc, 114.22/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  var_over <- data.frame(
    Cost = rep(c("Variable Overhead"), times=3),
    Value = c(229.32/cap_lfp, 205.78/cap_nmc, 207.10/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  deprec <- data.frame(
    Cost = rep(c("Depreciation"), times=3),
    Value = c(586.64/cap_lfp, 532.92/cap_nmc, 536.12/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  other_fixed <- data.frame(
    Cost = rep(c("Other Fixed Costs"), times=3),
    Value = c(435.49/cap_lfp, 396.10/cap_nmc, 398.99/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  prof <- data.frame(
    Cost = rep(c("Profits"), times=3),
    Value = c(464.25/cap_lfp, 439.46/cap_nmc, 445.50/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  warr <- data.frame(
    Cost = rep(c("Warranty"), times=3),
    Value = c(429.82/cap_lfp, 491.19/cap_nmc, 513.37/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  
  #############
  # PANEL ONE #
  #############
  
  incentVals <- reactiveValues()
  cbStates <- reactiveValues()
  
  # # Hide unneeded input sliders
  # observe({
  #   if(input$lfpGraph == FALSE) {
  #     shinyjs::hide(id = "lfpGraphUSCost")
  #     shinyjs::hide(id = "lfpGraphUSInc")
  #     shinyjs::hide(id = "lfpGraphChCost")
  #     shinyjs::hide(id = "lfpGraphChInc")
  #   } else {
  #     shinyjs::show(id = "lfpGraphUSCost")
  #     shinyjs::show(id = "lfpGraphUSInc")
  #     shinyjs::show(id = "lfpGraphChCost")
  #     shinyjs::show(id = "lfpGraphChInc")
  #   }
  #   
  #   if(input$nmcGraph == FALSE) {
  #     shinyjs::hide(id = "nmcGraphUSCost")
  #     shinyjs::hide(id = "nmcGraphUSInc")
  #     shinyjs::hide(id = "nmcGraphChCost")
  #     shinyjs::hide(id = "nmcGraphChInc")
  #   } else {
  #     shinyjs::show(id = "nmcGraphUSCost")
  #     shinyjs::show(id = "nmcGraphUSInc")
  #     shinyjs::show(id = "nmcGraphChCost")
  #     shinyjs::show(id = "nmcGraphChInc")
  #   }
  #   
  #   if(input$ncaGraph == FALSE) {
  #     shinyjs::hide(id = "ncaGraphUSCost")
  #     shinyjs::hide(id = "ncaGraphUSInc")
  #     shinyjs::hide(id = "ncaGraphChCost")
  #     shinyjs::hide(id = "ncaGraphChInc")
  #   } else {
  #     shinyjs::show(id = "ncaGraphUSCost")
  #     shinyjs::show(id = "ncaGraphUSInc")
  #     shinyjs::show(id = "ncaGraphChCost")
  #     shinyjs::show(id = "ncaGraphChInc")
  #   }
  # })
  
  # Check value of inputs
  observe({
    incentVals$incPrice <- input$incrementalPrice
    incentVals$dcVal <- input$dualCreditVal
    incentVals$dcNum <- input$dualCreditNum
  })
  
  # Check which incentives selected 
  observe({
    cbStates$`45XCM` <- input$`45XCM`
    cbStates$`45XEAM` <- input$`45XEAM`
    cbStates$`45XCell` <- input$`45XCell`
    cbStates$`30DCrit` <- input$`30DCrit`
    cbStates$`30DComp` <- input$`30DComp`
    cbStates$`45W` <- input$`45WLease`
  })
  
  # Handle mutual exclusivity between 30D and 45W
  observeEvent(input$`45WLease`, {
    if (input$`45WLease`) {
      if (input$`30DCrit` || input$`30DComp`) {
        showModal(modalDialog(
          title = "Selection Error",
          "Cannot claim both 30D and 45W credits simultaneously. (Click anywhere to close.)",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      updateCheckboxInput(session, "30DCrit", value = FALSE)
      updateCheckboxInput(session, "30DComp", value = FALSE)
    }
  })
  
  observeEvent(input$`30DCrit`, {
    if (input$`30DCrit` && input$`45WLease`) {
      updateCheckboxInput(session, "45WLease", value = FALSE)
      showModal(modalDialog(
        title = "Selection Error",
        "Cannot claim both 30D and 45W credits simultaneously. (Click anywhere to close)",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$`30DComp`, {
    if (input$`30DComp` && input$`45WLease`) {
      updateCheckboxInput(session, "45WLease", value = FALSE)
      showModal(modalDialog(
        title = "Selection Error",
        "Cannot claim both 30D and 45W credits simultaneously. (Click anywhere to close)",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Check scenarios 
    observe({
    if (input[["USScen"]] == "User") {
      # Do nothing
    }
    if (input[["USScen"]] == "2024 Tesla Model Y Long Range (75 kWh)") {
      updateCheckboxInput(session, "45XCM", value = FALSE)
      updateCheckboxInput(session, "45XEAM", value = TRUE)
      updateCheckboxInput(session, "45XCell", value = TRUE)
      updateCheckboxInput(session, "45XMod", value = TRUE)
      updateCheckboxInput(session, "30DCrit", value = TRUE)
      updateCheckboxInput(session, "30DComp", value = TRUE)
      updateCheckboxInput(session, "45WLease", value = FALSE)
      
      updateCheckboxInput(session, "lfpGraph", value = FALSE)
      updateCheckboxInput(session, "nmcGraph", value = FALSE)
      updateCheckboxInput(session, "ncaGraph", value = TRUE)
      updateCheckboxInput(session, "graphChCost", value = FALSE)
      updateCheckboxInput(session, "graphChInc", value = FALSE)
    }
    if (input[["USScen"]] == "2023 Chevrolet Bolt EUV (65 kWh)") {
      updateCheckboxInput(session, "45XCM", value = FALSE)
      updateCheckboxInput(session, "45XEAM", value = FALSE)
      updateCheckboxInput(session, "45XCell", value = FALSE)
      updateCheckboxInput(session, "45XMod", value = FALSE)
      updateCheckboxInput(session, "30DCrit", value = TRUE)
      updateCheckboxInput(session, "30DComp", value = TRUE)
      updateCheckboxInput(session, "45WLease", value = FALSE)
      
      updateCheckboxInput(session, "lfpGraph", value = FALSE)
      updateCheckboxInput(session, "nmcGraph", value = TRUE)
      updateCheckboxInput(session, "ncaGraph", value = FALSE)
      updateCheckboxInput(session, "graphChCost", value = FALSE)
      updateCheckboxInput(session, "graphChInc", value = FALSE)
    }
    if (input[["USScen"]] == "2024 Hyundai Ioniq Long Range (77.4 kWh)") {
      updateCheckboxInput(session, "45XCM", value = FALSE)
      updateCheckboxInput(session, "45XEAM", value = FALSE)
      updateCheckboxInput(session, "45XCell", value = FALSE)
      updateCheckboxInput(session, "45XMod", value = FALSE)
      updateCheckboxInput(session, "30DCrit", value = FALSE)
      updateCheckboxInput(session, "30DComp", value = FALSE)
      updateCheckboxInput(session, "45WLease", value = TRUE)
      
      updateCheckboxInput(session, "lfpGraph", value = FALSE)
      updateCheckboxInput(session, "nmcGraph", value = TRUE)
      updateCheckboxInput(session, "ncaGraph", value = FALSE)
      updateCheckboxInput(session, "graphChCost", value = FALSE)
      updateCheckboxInput(session, "graphChInc", value = FALSE)
    }
    if (input[["USScen"]] == "2024 Mustang Mach-E Standard Range (70 kWh)") {
      updateCheckboxInput(session, "45XCM", value = FALSE)
      updateCheckboxInput(session, "45XEAM", value = FALSE)
      updateCheckboxInput(session, "45XCell", value = FALSE)
      updateCheckboxInput(session, "45XMod", value = FALSE)
      updateCheckboxInput(session, "30DCrit", value = FALSE)
      updateCheckboxInput(session, "30DComp", value = TRUE)
      updateCheckboxInput(session, "45WLease", value = FALSE)
      
      updateCheckboxInput(session, "lfpGraph", value = TRUE)
      updateCheckboxInput(session, "nmcGraph", value = FALSE)
      updateCheckboxInput(session, "ncaGraph", value = FALSE)
      updateCheckboxInput(session, "graphChCost", value = FALSE)
      updateCheckboxInput(session, "graphChInc", value = FALSE)
    }
    if (input[["USScen"]] == "2024 Volkswagen ID.4 Standard Range (62 kWh)") {
      updateCheckboxInput(session, "45XCM", value = FALSE)
      updateCheckboxInput(session, "45XEAM", value = FALSE)
      updateCheckboxInput(session, "45XCell", value = TRUE)
      updateCheckboxInput(session, "45XMod", value = TRUE)
      updateCheckboxInput(session, "30DCrit", value = TRUE)
      updateCheckboxInput(session, "30DComp", value = TRUE)
      updateCheckboxInput(session, "45WLease", value = FALSE)
      
      updateCheckboxInput(session, "lfpGraph", value = FALSE)
      updateCheckboxInput(session, "nmcGraph", value = TRUE)
      updateCheckboxInput(session, "ncaGraph", value = FALSE)
      updateCheckboxInput(session, "graphChCost", value = FALSE)
      updateCheckboxInput(session, "graphChInc", value = FALSE)
    }
  })
  
  # Reset button functionality
  observeEvent(input$resetUSAValues, {
    updateSliderInput(session, "incrementalPrice", value = 7500)
    updateCheckboxInput(session, "45XCM", value = TRUE)
    updateCheckboxInput(session, "45XEAM", value = TRUE)
    updateCheckboxInput(session, "45XCell", value = TRUE)
    updateCheckboxInput(session, "45XMod", value = TRUE)
    updateCheckboxInput(session, "30DCrit", value = TRUE)
    updateCheckboxInput(session, "30DComp", value = TRUE)
    updateCheckboxInput(session, "45WLease", value = FALSE)
    updateCheckboxInput(session, "lfpGraph", value = TRUE)
    updateCheckboxInput(session, "nmcGraph", value = TRUE)
    updateCheckboxInput(session, "ncaGraph", value = TRUE)
    updateCheckboxInput(session, "graphChCost", value = TRUE)
    updateCheckboxInput(session, "graphChInc", value = TRUE)
  })
  
  observeEvent(input$resetChinaValues, {
    updateSliderInput(session, "dualCreditVal", value = 1000)
    updateSliderInput(session, "dualCreditNum", value = 2.3)
    updateSliderInput(session, "laborPct", value = 0.26)
    updateSliderInput(session, "materialPct", value = 0.90)
    updateSliderInput(session, "capitalPct", value = 0.80)
  })
  
  # Stack data
  sum_mat <- data.frame(Cost = rep("Material Costs", times = 3),  # Replace Cost with "Material Costs"
    Value = crit_min$Value + other_cath$Value + other_anode$Value + other_mat$Value,
    Chem = crit_min$Chem # LFP, NMC811, NCA
  )
  tot_mat_costs <- reactive({ 
    cost_stack(sum_mat,input$materialPct)
  })
  tot_labor <- reactive({ 
    cost_stack(labor,input$laborPct)
  })
  tot_energy <- reactive({ 
    cost_stack(energy,input$capitalPct)
  })
  tot_var_over <- reactive({ 
    cost_stack(var_over,input$capitalPct)
  })
  tot_deprec <- reactive({ 
    cost_stack(deprec,input$capitalPct)
  })
  tot_other_fixed <- reactive({ 
    cost_stack(other_fixed,input$capitalPct)
  })
  tot_prof <- reactive({ 
    cost_stack(prof,input$capitalPct)
  })
  tot_warr <- reactive({ 
    cost_stack(warr,input$capitalPct)
  })
  
  `45X_CM` <- data.frame(
    Cost = rep(c("Critical Minerals Processing"), times=3),
    Value = c(0.34, 1.83, 1.67),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_45X_CM <- incent_stack(`45X_CM`,FALSE)
  
  `45X_EA` <- data.frame(
    Cost = rep(c("Electroactive Materials Production"), times=3),
    Value = c(5.32, 7.61, 8.19),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_45X_EA <- incent_stack(`45X_EA`,FALSE)
  
  `45X_Cell` <- data.frame(
    Cost = rep(c("Battery Cell Production"), times=3),
    Value = rep(c(35), times=3),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_45X_Cell <- incent_stack(`45X_Cell`,FALSE)
  
  `45X_Mod` <- data.frame(
    Cost = rep(c("Battery Module Production"), times=3),
    Value = rep(c(10), times=3),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_45X_Mod <- incent_stack(`45X_Mod`,FALSE)
  
  `30D_min` <- data.frame(
    Cost = rep(c("Critical Minerals-Based Purchase Credit"), times=3),
    Value = c(3750/cap_lfp, 3750/cap_nmc, 3750/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_30D_min <- incent_stack(`30D_min`,FALSE)
  
  `30D_comp` <- data.frame(
    Cost = rep(c("Battery Component-Based Purchase Credit"), times=3),
    Value = c(3750/cap_lfp, 3750/cap_nmc, 3750/cap_nca),
    Chem = rep(c("LFP", "NMC811", "NCA"))
  )
  tot_30D_comp <- incent_stack(`30D_comp`,FALSE)
  
  tot_45W <- reactive({
    incPrice = input$incrementalPrice
    if (input$incrementalPrice > 7500) {
      incPrice = 7500
    }
    
    `45W` <- data.frame(
      Cost = rep(c("Commercial and Leased Vehicle Credit"), times=3),
      Value = c(incPrice/cap_lfp, incPrice/cap_nmc, incPrice/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    )
    tot_45W <- incent_stack(`45W`,FALSE)
  })
  
  tot_DC <- reactive({
    dc_value = input$dualCreditVal*input$dualCreditNum
    DC <- data.frame(
      Cost = rep(c("Dual Credit Value"), times=3),
      Value = c(dc_value/cap_lfp, dc_value/cap_nmc, dc_value/cap_nca),
      Chem = rep(c("LFP", "NMC811", "NCA"))
    )
    tot_DC <- incent_stack(DC,TRUE)
  })
  
  # AGGREGATE DATA
  data <- reactive({
    data <- rbind(tot_mat_costs(), tot_labor(), tot_energy(), tot_var_over(), tot_deprec(), 
                  tot_other_fixed(), tot_prof(), tot_warr(), tot_DC())
    
    if (input$`45XCM`) {data <- rbind(data,tot_45X_CM)}
    if (input$`45XEAM`) {data <- rbind(data,tot_45X_EA)}
    if (input$`45XCell`) {data <- rbind(data,tot_45X_Cell)}
    if (input$`45XMod`) {data <- rbind(data,tot_45X_Mod)}
    if (input$`30DCrit`) {data <- rbind(data,tot_30D_min)}
    if (input$`30DComp`) {data <- rbind(data,tot_30D_comp)}
    if (input$`45WLease`) {data <- rbind(data,tot_45W())}
    
    data
  })

  # Panel 1 Graph for comparing effect of incentives 
  output$comparePlots <- renderUI({
    chemistries <- c("LFP", "NMC811", "NCA")
    plot_outputs <- list()
    
    for (chem in chemistries) {
      showLFP <- input$`lfpGraph` && (chem == "LFP")
      showNMC <- input$`nmcGraph` && (chem == "NMC811")
      showNCA <- input$`ncaGraph` && (chem == "NCA")
      if (showLFP || showNMC || showNCA) {
        plot_output_id <- paste0("comparePlot_", chem)
        plot_outputs[[chem]] <- plotOutput(outputId = plot_output_id)
        
        local({
          chem_local <- chem  # Capture the value of chem for use inside the renderPlot
          output[[plot_output_id]] <- renderPlot({
            data <- data()
            data_subset <- subset(data, Chem == chem_local)
            data_subset$Cost <- factor(data_subset$Cost, 
                                       levels = c("Dual Credit Value", 
                                       "Commercial and Leased Vehicle Credit",
                                       "Battery Component-Based Purchase Credit",
                                       "Critical Minerals-Based Purchase Credit",
                                       "Critical Minerals Processing",
                                       "Electroactive Materials Production", 
                                       "Battery Module Production", "Battery Cell Production", 
                                       "Warranty", "Profits", "Other Fixed Costs",
                                       "Depreciation", "Variable Overhead", 
                                       "Energy", "Direct Labor", "Material Costs"))
            
            data_subset$Type_Location <- factor(interaction(data_subset$Type, data_subset$Location),
                                                levels = c("Incentives.China", "Costs.China", 
                                                           "Incentives.USA", "Costs.USA"))
            levels(data_subset$Type_Location) <- c("Incentives in China", "Costs in China", 
                                                   "Incentives in the USA", "Costs in the USA")
            
            if (input$graphUSCost == FALSE) {
              data_subset <- subset(data_subset, Type_Location != "Costs in the USA")
            }
            if (input$graphUSInc == FALSE) {
              data_subset <- subset(data_subset, Type_Location != "Incentives in the USA")
            }
            if (input$graphChCost == FALSE) {
              data_subset <- subset(data_subset, Type_Location != "Costs in China")
            }
            if (input$graphChInc == FALSE) {
              data_subset <- subset(data_subset, Type_Location != "Incentives in China")
            }
            
            ggplot(data_subset, aes(x = Value, y = Type_Location, fill = Cost)) +
              geom_bar(stat = "identity") +
              scale_fill_manual(values = c("Material Costs" = "#cc0100ff",
                                           "Direct Labor" = "#cc4124ff", "Energy" = "#b45f07ff",
                                           "Variable Overhead" = "#ff0002ff", "Depreciation" = "#dd5050ff",
                                           "Other Fixed Costs" = "#e06666ff", "Profits" = "#ea9999",
                                           "Warranty" = "#f4cccc", 
                                           "Battery Cell Production" = "#38761dff",
                                           "Battery Module Production" = "#6aa84fff",
                                           "Electroactive Materials Production" = "#d9ead3ff",
                                           "Critical Minerals Processing" = "#b5d7a8ff",
                                           "Critical Minerals-Based Purchase Credit" = "#3c78d8ff",
                                           "Battery Component-Based Purchase Credit" = "#a4c2f4ff",
                                           "Commercial and Leased Vehicle Credit" = "#c9daf8",
                                           "Dual Credit Value" = "#e69138")
              ) +
              labs(
                title = sprintf("Cost of Production and Incentives for %s, per kWh", chem_local),
                x = "Values in USD", y = ""
              ) +
              xlim(0, 180) +
              guides(fill = guide_legend(reverse = TRUE)) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 20),
                plot.subtitle = element_text(size = 12),
                legend.position = "bottom",
                legend.title = element_text(size = 0),
                legend.text = element_text(size = 14),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 14)
              )
          })
        })
      }
    }
    
    do.call(tagList, plot_outputs)  # Render all plots in a list
  })
  
  #############
  # PANEL TWO #
  #############
  
  # Define the amount of each critical mineral in each chemistry (LFP, NMC, NCA), divided by 70 (kWh)
  li_amt <- c(6.14/cap_lfp, 6.43/cap_nmc, 6.91/cap_nca)
  co_amt <- c(0.00/cap_lfp, 5.32/cap_nmc, 2.86/cap_nca)
  ni_amt <- c(0.00/cap_lfp, 42.42/cap_nmc, 45.59/cap_nca)
  mn_amt <- c(0.00/cap_lfp, 4.96/cap_nmc, 0.00/cap_nca)
  al_amt <- c(64.95/cap_lfp, 49.09/cap_nmc, 51.2/cap_nca)
  gr_amt <- c(69.10/cap_lfp, 61.76/cap_nmc, 62.06/cap_nca)
  
  # Define the constant costs for each chemistry
  other_costs  <- rbind(other_cath,other_anode,other_mat,labor,energy,var_over,deprec,other_fixed,prof,warr)

  # Reset button functionality
  observeEvent(input$resetPrice, {
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
      data <- rbind(data, other_costs)
      data$Cost <- factor(data$Cost, levels = c("Warranty", "Profits", "Other Fixed Costs",
                                                "Depreciation", "Variable Overhead", "Energy",
                                                "Direct Labor", "Implied Other Material Costs",
                                                "Implied Other Anode Costs",
                                                "Implied Other Cathode Costs",
                                                "Graphite", "Aluminum", "Manganese", "Nickel", 
                                                "Cobalt", "Lithium"))
    }

    data$Chem <- factor(data$Chem, levels = c("NCA","NMC811","LFP"))
    
    # Determine xlim based on checkbox input
    xl <- if (input$includeConstantCosts) 180 else 50
    
    ggplot(data, aes(x = Value, y = Chem, fill = Cost)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Lithium" = "#337538", "Cobalt" = "#2e2585", "Nickel" = "#5da899", 
                                   "Manganese" = "#dccd7d", "Aluminum" = "#dddddd", "Graphite" = "#434343",
                                   "Implied Other Cathode Costs" = "#F1C233",
                                   "Implied Other Anode Costs" = "#FFE59A",
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
      dropdown <- paste0("drop", i)
      
      output[[plotname]] <- renderPlot({
        data <- normalizedData()
        
        if (input[[dropdown]] == "All") {
          show <- rep(c(TRUE),times=6)
        }
        if (input[[dropdown]] == "2024 Generic") {
          show <- c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)
        }
        if (input[[dropdown]] == "2024 Tesla") {
          show <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
        }
        if (!input[[dropdown]] == "User") {
          updateCheckboxInput(session,li_cb,value = show[1])
          updateCheckboxInput(session,co_cb,value = show[2])
          updateCheckboxInput(session,ni_cb,value = show[3])
          updateCheckboxInput(session,mn_cb,value = show[4])
          updateCheckboxInput(session,al_cb,value = show[5])
          updateCheckboxInput(session,gr_cb,value = show[6])
        }
        
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
          geom_hline(yintercept = 0.8, color = "red", linetype = "dashed", size = 0.5) +
          geom_hline(yintercept = 0.7, color = "#ff6267", linetype = "dashed", size = 0.5) +
          geom_hline(yintercept = 0.6, color = "#f79faa", linetype = "dashed", size = 0.5) +
          geom_hline(yintercept = 0.5, color = "lightgray", linetype = "dashed", size = 0.5) +
          theme_minimal() + 
          theme(legend.position = "none", 
                panel.grid.minor = element_line(size = 0.1), 
                panel.grid.major = element_line(size = 0.2),
                plot.title = element_text(size = 14),
                axis.title = element_text(size = 12))
      })
      
      column(width = 2,  
             plotOutput(plotname, height = "300px"),  # Fixed height for consistency
             
             checkboxInput(li_cb, label = paste("Li, Scenario ", i), value=TRUE),
             checkboxInput(co_cb, label = paste("Co, Scenario ", i), value=TRUE),
             checkboxInput(ni_cb, label = paste("Ni, Scenario ", i), value=TRUE),
             checkboxInput(mn_cb, label = paste("Mn, Scenario ", i), value=TRUE),
             checkboxInput(al_cb, label = paste("Al, Scenario ", i), value=TRUE),
             checkboxInput(gr_cb, label = paste("Gr, Scenario ", i), value=TRUE),
             
             selectInput(dropdown, "Pre-defined scenarios", choices = c("User","All", "2024 Generic", "2024 Tesla")),
            )
    })
    
    do.call(fluidRow, plot_list)
  })
}