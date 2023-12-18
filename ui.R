# Define dashboard header
header <-  dashboardHeader(
  title = "Democracy Around the World",
  titleWidth = 300
)

# Define dashboard sidebar
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About the Data", tabName = "about-the-data", icon = icon("book"))
  ),
  sliderInput("year", "Year", min = 1990, max = max(vdem_data$year),
              value = 2010, sep = ""),
  selectInput("indicator", "Indicator", choices = vars),
  radioButtons("regions", "Regions",
               choices = region_choices,
               selected = "Global"),
  div(style = "margin: 10px; color: #FFFFFF; font-weight: lighter", 
  HTML("Use the checkbox above to explore democracy trends in a particular region.
                  Click on the map to view the trends for an individual country.")
  )
)

# Define dashboard body
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(width = 6, countryMapModuleUI("map1")),
              box(width = 6, plotlyLineChartModuleUI("linechart1"))
            ),
            fluidRow(
              box(width = 6, plotlyBarChartModuleUI("barchart1")),
              box(width = 6, plotlyScatterPlotModuleUI("scatterplot1"))
            )
    ),
    # Second tab content
    tabItem(tabName = "about-the-data",
            h2("The V-Dem Project"),
            p("The Varieties of Democracy ",
              a("(V-Dem)", href="https://www.v-dem.net/", target="_blank"),
              "Project offers a comprehensive approach to understanding and measuring democracy. It stands out for its multidimensional and detailed dataset, reflecting the complexity of democracy beyond just the presence of elections. V-Dem identifies five high-level principles of democracy: electoral, liberal, participatory, deliberative, and egalitarian. Each principle is meticulously measured through data that encompasses various components like free and fair elections, civil liberties, judicial independence, and media freedom."),
            p("At the heart of V-Dem is the innovative use of expert-coded data and advanced Bayesian Item-Response Theory (IRT) estimation strategies. These methods address potential biases and measurement errors inherent in expert-coded data, providing credible and reliable estimates of various democratic concepts. The expert network, comprising over 3,700 individuals, offers detailed, local knowledge from qualified professionals, ensuring nuanced analysis of democracy's aspects. V-Dem’s methodology accounts for differences in expert judgment and varying reliability, allowing for a nuanced understanding of democracy in its full complexity. This unique approach, combined with strict confidentiality policies for experts and the integration of expert judgments, positions V-Dem as a leader in the field of democracy research and analysis."),
            h2("Indicator Definitions"),
            p(strong("Electoral Democracy: "), "This the Polyarchy score, which measures the extent to which the ideal of electoral democracy is achieved, focusing on the responsiveness of rulers to citizens through fair electoral competition and extensive suffrage, coupled with freedoms for civil society and media."),
            p(strong("Liberal Democracy: "), "The Liberal Democracy Index measures the protection of individual and minority rights against state and majority tyranny, focusing on constitutionally protected civil liberties, the rule of law, an independent judiciary, and effective checks and balances on executive power."),
            p(strong("Participatory Democracy: "), "This index measures the active citizen participation in both electoral and non-electoral political processes. Key aspects include engagement in civil society organizations, direct democracy initiatives, and involvement in subnational elected bodies."),
            p(strong("Deliberative Democracy: "), "Deliberative Democracy emphasizes the importance of public reasoning and dialogue focused on the common good as the basis for political decisions, rather than relying on emotional appeals, solidary attachment, parochial interests, or coercion. Deliberative democracy requires not just the aggregation of preferences but also respectful, informed, and competent dialogue at all levels of decision-making."),
            p(strong("Egalitarian Democracy: "), "This index measures the extent to which social groups are not hindered by material and immaterial inequalities in their ability to exercise of formal rights and liberties. Egalitarian democracy is considered achieved when: 1) rights and freedoms are equally protected across all social groups; 2) resources are distributed equitably among all social groups; and 3) groups and individuals have equal access to power."),
            p(strong("Civil Liberties"), "The extent to which civil liberties are respected by the government.Civil liberty is understood as liberal freedom that is defined by the absence of physical violence committed by government agents and the absence of constraints of private liberties and political liberties by the government."),
            p(strong("Rule of Law"), "This index meassures whether laws are transparently, independently, predictably, impartially, and equally enforced, and to what extent the actions of government officials comply with the law. The index is formed by taking the point estimates from a Bayesian factor analysis model of a number of indicators related to judicial authority and independence."),
            p(strong("Clientelism"), "The Clientelism Index measures the extent to which politics in a given country are based on clientelistic relationships, which are are characterized by the targeted and contingent distribution of resources such as goods, services, jobs, and money in exchange for political support. In this index, lower scores indicate a more democratic situation with less reliance on clientelism, while higher scores point to a more problematic scenario with greater prevalence of clientelistic practices. Higher scores indicate higher levels of clientelism."),
            p(strong("Corruption"), "This is the Regime Corruption Index. It assesses the degree to which political actors in a country use their office for personal or political gain. This index closely relates to the V-Dem political corruption index but specifically focuses on the conduct of politicians and a narrower range of corrupt practices associated with neopatrimonial rule. Higher scores indicate higher levels of corruption."),
            p(strong("Women's Empowerment"), "An index that measures women's choice, agency, and participation in societal decision-making. It incorporates three equally-weighted dimensions: fundamental civil liberties, women’s open discussion of political issues and participation in civil society organizations, and the descriptive representation of women in formal political positions"),
            h2("Natural Earth Map Data"),
            p("The map data is from the ",
              a("rnaturalearth", href="http://ropensci.github.io/rnaturalearth/", target="_blank"), 
              "package, which is a wrapper for the ",
              a("Natural Earth", href="https://www.naturalearthdata.com/", target="_blank"),
              "project. Natural Earth is a public domain map dataset that provides both vector and raster map data at various scales, designed for use in a range of applications from web mapping to large-scale printing. The project aims to offer accurate and up-to-date geographical information, including political boundaries, physical features, and populated places. It is notable for its inclusion of the Natural Earth projection, a map projection that seeks to balance the visual distortion inherent in traditional projections. Unlike the more common Mercator projection, which significantly distorts size and shape at high latitudes, the Natural Earth projection aims to present a more balanced view of the world, reducing distortion of land masses as they near the poles."),
    )
  )
)
 
# Define dashboard page
dashboardPage(header, sidebar, body, skin = "purple")
