library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Critical Mineral Price Effect Explorer for the Inflation Reduction Act"),
  
  wellPanel(
    p(HTML("This tool accompanies the forthcoming study 'U.S. Industrial Policy Could Reduce Electric Vehicle Battery Supply Chain 
           Vulnerabilities and Influence Battery Technology Choice' in the journal <i>Nature Energy</i>, corresponding to Figures 
           2 and 3, which show the relative cost of production against the incentive values and value-percentage combinations of 
           critical mineral supply chains that must be domestic or free-trade agreement countries to meet 30D critical mineral 
           credit requirements. This tool compares the effect of price fluctuations in the six electric vehicle battery-related 
           critical minerals identified in the Inflation Reduction Act, drawing upon the methodology described in 
           <a href='https://doi.org/10.1038/s41893-023-01079-8'>Trost and Dunn (2023)</a>. For more information about the price 
           ranges for each mineral, other costs of production, the scenarios under which batteries of each chemistry can qualify 
           for the 30D credit, and more, please see the paper and supplementary information, particularly Supplementary Data S2, 
           S3, and S5 and Supplementary Text S2. {To add - link to this code.}"),
    tags$ul(
      tags$li(HTML("<a href='https://en.wikipedia.org/wiki/Lithium-ion_battery#Cathode'> Battery Chemistries</a>: LFP = Lithium Iron 
                    Phosphate; NMC811 = Nickel Manganese Cobalt Oxide, where '811' represents the ratios of nickel to manganese to 
                    cobalt; NCA = Nickel Cobalt Aluminum.")),
      tags$li(HTML("Costs were modeled with the Argonne National Lab's <a href='https://www.anl.gov/cse/batpac-model-software'>BatPaC</a> 
                    model, using critical minerals data from the Department of Energy and Argonne National Lab's 
                    <a href='https://www.energy.gov/eere/greet'>GREET</a> model.")),
      tags$li(HTML("Implied costs were found by calculating the difference between the total amount of material multipled by the cost 
                   the material, e.g. total amount of cathode used in the model by price of the cathode, less the total cost of the raw
                   material used in the cathode, per GREET, times the price of each raw material. We assume that these implied costs 
                   occur due to costs in the supply chain, such as processing/refining/manufacturing costs, transportation, markups
                   due to profit, etc. The 'other' material costs refer to all other material costs, as calculated by BatPaC, less the 
                   cost of the cathode materials, anode materials, and the cost of critical minerals not accounted for in the cathode
                   and anode.")),
      tags$li(HTML("The underlying code and other code related to this paper can be found at 
                   <a href='https://www.doi.org/10.5281/zenodo.11182063'>doi.org/10.5281/zenodo.11182063</a>."))
      ),
    )
  ),
  
  # Sidebar with a slider input for aluminum price
  sidebarLayout(
    sidebarPanel(
      sliderInput('lithiumPrice', 'Lithium Price ($/kg)', min = 1, max = 161, 
                  value = 26.78, step = 2, round = 0),
      sliderInput('cobaltPrice', 'Cobalt Price ($/kg)', min = 22, max = 94, 
                  value = 47, step = 1, round = 0),
      sliderInput('nickelPrice', 'Nickel Price ($/kg)', min = 10, max = 32, 
                  value = 12.36, step = 0.04, round = 0),
      sliderInput('manganesePrice', 'Manganese Price ($/kg)', min = 2.87, max = 6.67, 
                  value = 5.71, step = 0.02, round = 0),
      sliderInput('aluminumPrice', 'Aluminum Price ($/kg)', min = 1, max = 4, 
                  value = 2.12, step = 0.01, round = 0),
      sliderInput('graphitePrice', 'Graphite Price ($/kg)', min = 0.75, max = 1.35, 
                  value = 1.01, step = 0.01, round = 0),
      checkboxInput('includeConstantCosts', 'Show constant, non-mineral costs', value = TRUE),
      actionButton('reset', 'Reset Prices'),
      
      tags$div(style = "margin-bottom: 10px;"),
      
      tags$hr(),
      
      tags$div(style = "margin-bottom: 10px;"),
      
      sliderInput("numGraphs", "Number of Scenarios", min = 2, max = 5, value = 3),
      
      tags$div(style = "margin-bottom: 20px;"),
      
      p(HTML("These graphs (corresponding to Fig. 2 in the paper) allow for definition of scenarios where the user can define 
            the critical mineral supply chains that satisfy the 30D requirements - 50% of the value of critical minerals must 
            be sourced in the United States or from countries that have a free trade agreement in 2024, increasing up to 80% 
            by 2027. See Supplementary Data S2 and S3 for more information.")),
    ),
    mainPanel(
      plotOutput('pricePlot'),
      
      tags$hr(),
      
      tags$div(style = "margin-bottom: 40px;"),
      fluidRow(uiOutput("graphPanel")),
    )
  )
)



