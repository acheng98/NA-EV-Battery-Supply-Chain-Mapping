library(shiny)
library(shinyjs)
library(ggplot2)
library(bslib)
library(bsicons)

input_switch_tooltip <- function(inputId, label, value, tooltipText) {
  tags$div(
    class = "input-tooltip-container",
    input_switch(inputId, label, value = value),
    tags$a(
      bs_icon("info-circle"),
      class = "tooltip-icon-1",
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "right",
      title = tooltipText
    )
  )
}

input_slider_tooltip <- function(inputId, label, value, min, max, step, round, tooltipText) {
  tags$div(
    class = "input-container",
    tags$div(
      class = "input-tooltip-container",
      tags$label(
        class = "control-label",
        `for` = inputId,
        label
      ),
      tags$a(
        bs_icon("info-circle"),
        class = "tooltip-icon-2",
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "right",
        title = tooltipText
      )
    ),
    sliderInput(inputId, NULL, value = value, min = min, max = max, step = step, round = round),
  )
}

title_tooltip_container <- function(title, tooltipText) {
  tags$div(
    class = "input-tooltip-container",
    span(h4(title)),
    tags$a(
      bs_icon("info-circle"),
      class = "tooltip-icon-1",
      `data-bs-toggle` = "tooltip",
      `data-bs-placement` = "right",
      title = tooltipText
    )
  )
}

# Define UI for application
ui <- fluidPage(
  # JavaScript
  theme = bs_theme(bootswatch = "minty"),
  tags$script(HTML("
    $(document).ready(function() {
      $('[data-bs-toggle=\"tooltip\"]').tooltip();
    });
  ")),
  
  # Application title
  titlePanel("Critical Mineral Price Effect Explorer - Battery Manufacturing and Inflation Reduction Act"),
  wellPanel(
    p(HTML("This tool accompanies the forthcoming study 'U.S. Industrial Policy Could Reduce Electric Vehicle Battery Supply Chain 
           Vulnerabilities and Influence Battery Technology Choice' in the journal <i>Nature Energy</i>.")),
    tags$ul(
      tags$li(HTML("<a href='https://en.wikipedia.org/wiki/Lithium-ion_battery#Cathode'> Battery Chemistries</a>: LFP = Lithium Iron 
                        Phosphate; NMC811 = Nickel Manganese Cobalt Oxide, where '811' represents the ratios of nickel to manganese to 
                        cobalt; NCA = Nickel Cobalt Aluminum.")),
      tags$li(HTML("Costs were modeled with the Argonne National Lab's <a href='https://www.anl.gov/cse/batpac-model-software'>BatPaC</a> 
                        model, using critical minerals data from the Department of Energy and Argonne National Lab's 
                        <a href='https://www.energy.gov/eere/greet'>GREET</a> model.")),
      tags$li(HTML("The underlying code for this applet, as well as other code related to this paper, can be found at 
                       <a href='https://www.doi.org/10.5281/zenodo.11182063'>doi.org/10.5281/zenodo.11182063</a>."))
    ),
    tags$div(style = "margin-bottom: -5px;")
  ),
  
  tags$div(style = "margin-bottom: 15px;"),
  
  navset_tab(
    nav_panel(title = "Scenarios",
       tags$div(style = "margin-bottom: 20px;"),
       sidebarLayout(
         sidebarPanel(
           span(h4("Inflation Reduction Act (USA) Options")),
           selectInput("USScen", "US Pre-defined Scenarios", choices = c("User","2024 Tesla Model Y Long Range (75 kWh)", 
                                                                         "2023 Chevrolet Bolt EUV (65 kWh)", 
                                                                         "2024 Hyundai Ioniq Long Range (77.4 kWh)", 
                                                                         "2024 Mustang Mach-E Standard Range (70 kWh)",
                                                                         "2024 Volkswagen ID.4 Standard Range (62 kWh)")),
           # Checkboxes
           input_switch_tooltip('45XCM', 'Critical Mineral Processing Credit', value = TRUE, "A 45X Manufacturing Credit. 10% of production cost for 
                                mineral extraction only (not the value of the minerals themselves). Claimable for domestic (US-based) production only."),
           input_switch_tooltip('45XEAM', 'Electroactive Materials Production Credit', value = TRUE, "A 45X Manufacturing Credit. 10% of production 
                                cost for production of electroactive materials (not the value the materials), e.g. cathodes, anodes, electrolyte. 
                                Claimable for domestic (US-based) production only."),
           input_switch_tooltip('45XCell', 'Battery Cell Production Credit', value = TRUE, "A 45X Manufacturing Credit. $35 per kilowatt-
                                hour of battery cells produced. Claimable for domestic (US-based) production only."),
           input_switch_tooltip('45XMod', 'Battery Module Production Credit', value = TRUE, "A 45X Manufacturing Credit. $10 per kilowatt-hour
                                battery modules produced. If no modules exist in the battery design, the cell credit becomes $45 per kilowatt-hour. 
                                Claimable for domestic (US-based) production only."),
           input_switch_tooltip('30DCrit', 'Critical Minerals-Based Purchase Credit', value = TRUE, "A 30D New Clean Vehicle Credit. $3750 per vehicle, 
                                depending on the source of the critical minerals in the battery of the vehicle. With battery component-based credit, 
                                replaced previous 30D credit that was worth $7500 total. For details, see scrolling info box below."),
           input_switch_tooltip('30DComp', 'Battery Component-Based Purchase Credit)', value = TRUE, "A 30D New Clean Vehicle Credit. $3750 per vehicle, 
                                depending on the source of the battery components in the vehicle. With critical mineral-based credit, replaced previous 
                                30D credit that was worth $7500 total. For details, see scrolling info box below."),
           input_switch_tooltip('45WLease', 'Credit for vehicles for commercial use (including leasing)', value = FALSE, "The 45W Commercial Clean 
                                Vehicle Credit, for any vehicle sold for a commercial use (including leases to consumers). For further detail, see 
                                scrolling info box below."),
            
           input_slider_tooltip('incrementalPrice', 'Incremental Price of an Electric Vehicle (USD)', min = 0, max = 12000, 
                       value = 7500, step = 100, round = 0, tooltipText = "The additional cost of an electric vehicle relative to a comparable internal 
                       combustion engine (ICE/traditional) vehicle. This is to be reevaluated on a regular basis by the Department of Energy, and capped
                       at a maximum value of $7500. For the year 2024, the incremental price was determined to be that maximum amount of $7500."),
           
           actionButton('resetUSAValues', 'Reset to default USA input values'),
           
           tags$div(style = "margin-bottom: 10px;"),
           tags$hr(),
           tags$div(style = "margin-bottom: 10px;"),
           
           title_tooltip_container("Dual Credit Policy (China) Options","Note that these inputs approximate the outputs of the more complex BatPaC model 
                    used for the results in the paper, and thus the results in this applet may differ slightly from the more accurately measured model."),
           
           sliderInput('materialPct', 'Material costs in China as percentage of US costs', min = 0, max = 1, 
                       value = 0.90, step = 0.01, round = 0),
           sliderInput('laborPct', 'Labor costs in China as percentage of US costs', min = 0, max = 1, 
                       value = 0.26, step = 0.01, round = 0),
           sliderInput('capitalPct', 'Capital and Other costs in China as percentage of US costs', min = 0, max = 1, 
                       value = 0.80, step = 0.01, round = 0),
           
           tags$div(style = "margin-bottom: 20px;"),
           
           input_slider_tooltip('dualCreditVal', 'Value per New Energy Vehicle credit, China (USD)', min = 0, max = 1600, 
                       value = 1000, step = 100, round = 0, tooltipText = "Tesla reportedly sold credits at $460 each in April 2021, and prices were 
                       speculated to be $900 per credit in 2022. Other analyses have projected values of up to $1200 per credit."),
           input_slider_tooltip('dualCreditNum', 'Number of New Energy Vehicle credits per vehicle, China', min = 0, max = 6, 
                       value = 2.3, step = 0.05, round = 0, tooltipText = "When the Dual Credit System was promulgated, EVs were able to earn up to
                       six credits each, though the value of the credit was much lower then. This was revised to an upper limit of 3.4 between 
                       January 2021 and July 2023, and was revised to 2.3 starting in August 2023."),
           
           actionButton('resetChinaValues', 'Reset input values for China to default assumptions made in paper'),
           
           tags$hr(),
           
           card(
             id = "IRADetails",
             max_height = 300,
             card_header("Details and Caveats of the IRA Electric Vehicle-related Credits"),
             p(HTML("Details and caveats for claiming the IRA credits are detailed below. For more information, read Supplementary Note 3-1 
                    of the paper, specifically Supplementary Tables 8 and 9."),
               tags$ul(
                 tags$li(HTML("To claim either of the 30D credits:")),
                  tags$ul(
                    tags$li("Final assembly of the vehicle must be within the United States"),
                    tags$li("The battery capacity of the vehicle must exceed 7 kWh"),
                    tags$li("The amount of the credit cannot exceed 30% of the purchase price of the vehicle"),
                    tags$li("Credits cannot be awarded for vehicles that cost more than $55K for a sedan, $80K for a van, $80K for an SUV, and
                            $80K for a pickup truck"),
                    tags$li("Credits cannot be claimed by individuals earning more than an adjusted gross income of $150K, $225 for head of 
                            household, or $300K for partners married and filing jointly.")
                  ),
                 tags$li(HTML("Foreign Entities of Concern (FEOCs): as currently (Aug 2024) defined in US code, anything produced in a 'covered
                              nation' (i.e. Russia, Iran, North Korea, Japan), or produced by a company headquartered in or partially owned 
                              (25% or more) by a citizen of a covered nation or the covered nation itself.")),
                 tags$li(HTML("Critical Minerals-Based Purchase Credit: The vehicle's battery critical minerals must not include any FEOCs 
                              starting in 2025. To qualify, a minimum of 50% must be from countries with a 'free trade agreement' with the United 
                              States in 2024 OR produced via recycling within North America, increasing by 10% each year until 2027 (capping at 
                              80%) until credits expire in 2032. See 'Minerals' Tab to explore price-based scenarios for meeting this credit.")),
                 tags$li(HTML("Battery Components-Based Purchase Credit: Must not include any FEOCs starting 2024. To qualify, a minimum of 60% 
                              must be manufacuted in North America in 2024 and 2025, and then increasing by 10% each year by 2029 (all the way to 
                              100%), until credits expire in 2032.")),
                 tags$li(HTML("Commercial Clean Vehicle credit: This credit can be claimed by any purchaser of a vehicle for commercial credits,
                              which has been interpreted to include vehicles purchased by a dealer or retailer for lease to a consumer. In theory,
                              that company would not need to pass on the full value of the credit to the consumer, but in practice there would be 
                              little incentive to not do so (as competitors would undercut that sale price). The value of the credit is determined 
                              by the incremental cost of the electric vehicle relative to a comparable internal combustion engine vehicle. The 
                              credit can be claimed, regardless of any critical mineral or battery component requirement, as well as the income 
                              and price cap limits.")),
               )
             )
           )
         ),       
        
        mainPanel(
          shinyjs::useShinyjs(),
          card(
            height = 135,
            card_header("Graphs and Bars to Show"),
            card_body(
              min_height = 50,
              layout_column_wrap(
                width = 1/4,
                input_switch("lfpGraph", "LFP", value = TRUE),
                input_switch("nmcGraph", "NMC811", value = TRUE),
                input_switch("ncaGraph", "NCA", value = TRUE),
              ),
              layout_column_wrap(
                width = 1/4,
                input_switch("graphUSCost", "Costs in the USA", value = TRUE),
                input_switch("graphUSInc", "Incentives in the USA", value = TRUE),
                input_switch("graphChCost", "Costs in China", value = TRUE),
                input_switch("graphChInc", "Incentives in China", value = TRUE),
              )
            ),
          ),
          uiOutput("comparePlots"))
      )
    ),
    
    nav_panel("Minerals",
      # Sidebar with a slider input for aluminum price
      tags$div(style = "margin-bottom: 20px;"),
      
      sidebarLayout(
        sidebarPanel(
          
          sliderInput('lithiumPrice', 'Lithium Price ($/kg contained Li)', min = 1, max = 161, 
                      value = 26.78, step = 2, round = 0),
          sliderInput('cobaltPrice', 'Cobalt Price ($/kg contained Co)', min = 22, max = 94, 
                      value = 47, step = 1, round = 0),
          sliderInput('nickelPrice', 'Nickel Price ($/kg contained Ni)', min = 10, max = 32, 
                      value = 12.36, step = 0.04, round = 0),
          sliderInput('manganesePrice', 'Manganese Price ($/kg contained Mn)', min = 2.87, max = 6.67, 
                      value = 5.71, step = 0.02, round = 0),
          sliderInput('aluminumPrice', 'Aluminum Price ($/kg contained Al)', min = 1, max = 4, 
                      value = 2.12, step = 0.01, round = 0),
          sliderInput('graphitePrice', 'Graphite Price ($/kg contained G)', min = 0.75, max = 1.35, 
                      value = 1.01, step = 0.01, round = 0),
          
          tags$div(style = "margin-bottom: 5px;"),
          p("Note: Range is equal to min and max prices over last 15 years; default value is value used in paper"),
          tags$div(style = "margin-bottom: 5px;"),
          
          checkboxInput('includeConstantCosts', 'Show constant, non-mineral costs', value = TRUE),
          actionButton('resetPrice', 'Reset prices to assumptions made in paper'),
          
          
          tags$div(style = "margin-bottom: 10px;"),
          
          tags$hr(),
          
          tags$div(style = "margin-bottom: 10px;"),
          
          sliderInput("numGraphs", "Number of Scenarios", min = 2, max = 5, value = 3),
          
          tags$div(style = "margin-bottom: 20px;"),
          
          p(HTML("These graphs (corresponding to Fig. 2 in the paper) allow for definition of scenarios where the user can define 
                the critical mineral supply chains that satisfy the 30D requirements - 50% of the value of critical minerals must 
                be sourced in the United States or from countries that have a free trade agreement in 2024, increasing by 10% each
                year up to 80% by 2027. See Supplementary Data S2 and S3 for more information.")),
        ),
        mainPanel(
          tags$div(style = "margin-bottom: 20px;"),
          
          plotOutput('pricePlot'),
          
          tags$hr(),
          tags$div(style = "margin-bottom: 20px;"),
          
          p(h4("Critical mineral value-percentage combinations for exploring IRA 30D 'New Clean Vehicle Credit' compliance 
              scenarios (Fig 2. in paper)")),
          
          tags$div(style = "margin-bottom: 20px;"),
          fluidRow(uiOutput("graphPanel")),
        )
      ),
      
      wellPanel(
        p(HTML("This tool corresponds to Figures 2 and 3 of the source paper, which show the relative cost of production against the 
               incentive values and value-percentage combinations of critical mineral supply chains that must be domestic or free-trade 
               agreement countries to meet 30D critical mineral credit requirements. This tool compares the effect of price fluctuations 
               in the six electric vehicle battery-related critical minerals identified in the Inflation Reduction Act, drawing upon the 
               methodology described in <a href='https://doi.org/10.1038/s41893-023-01079-8'>Trost and Dunn (2023)</a>."),
          tags$ul(
            tags$li(HTML("'Implied' costs were found by calculating the difference between the total amount of material multipled by the 
                       cost of the material, e.g. total amount of cathode used in the model by price of the cathode, less the total cost of 
                       the raw material used in the cathode per GREET, times the price of each raw material. We assume that these implied 
                       costs occur due to other costs in the supply chain, such as non-critical mineral costs, processing/refining/
                       manufacturing costs, transportation, markups due to profit, etc. The 'other' material costs refer to all other 
                       material costs, as calculated by BatPaC, less the cost of the cathode materials, anode materials, and the cost of 
                       critical minerals not accounted for in the cathode and anode.")),
            tags$li(HTML("The price ranges for each mineral are taken from the minimum and maximum prices for each over the last 15 years. 
                       For more information about the price ranges for each mineral, other costs of production, the scenarios under which 
                       batteries of each chemistry can qualify for the 30D credit, and more, please see the paper and supplementary 
                       information, particularly Supplementary Data S2, S3, and S5 and Supplementary Text S2.")),
          )
        )
      )
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(tags$a("GitHub Code for Project", href = "https://github.com/acheng98/NA-EV-Battery-Supply-Chain-Mapping")),
      nav_item(tags$a("Link to paper", href = "https://github.com/acheng98/NA-EV-Battery-Supply-Chain-Mapping")),
    )
  ),
  
  tags$style(
    HTML("
      .input-tooltip-container {
        display: flex;
        align-items: center;
      }
      .tooltip-icon-1 {
        margin-left: 10px;
        margin-bottom: 18px;
        cursor: pointer;
      }
      .tooltip-icon-2 {
        margin-left: 10px;
        cursor: pointer;
      }
      .control-label {
        display: flex;
        align-items: center;
      }
    ")
  )
)



