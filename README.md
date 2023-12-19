# Democracy Around the World App

## About the App

The V-Dem Democracy Indicators Visualization App is an interactive tool designed to explore and visualize a selection of democracy and related indicators.Here are some of its key features: 

- **Interactive Map:** Visualize democracy indicators by country with a clickable leaflet map.
- **Regional Focus:** Filter data to view regional or global averages with interactive plotly charts.
- **Time Series Analysis:** Explore trends over time with an interactive year slider.
- **Multiple Indicators:** Includes the five primary V-Dem democracy indexes plus civil liberties, rule of law, clientelism, corruption, and women's empowerment.

## Using the App

1. **Select a Year:** Use the slider to choose the year of interest.
2. **Choose an Indicator:** Select a democracy indicator from the dropdown menu.
3. **Pick a Region:** Choose a specific region or opt for a global view.
4. **Explore the Data:** Click on the map for country-specific trends or view the charts for broader analysis.

## Running the Code

Before running the code, you will need to wrangle the data. Open the `data-wrangling.md` script and run the code chunks to create the necessary vdem_data.rds file.

## Data Source

The data is sourced from the [Varieties of Democracy (V-Dem) Project](https://www.v-dem.net/), an approach to understanding and measuring democracy. Map data are from the [rnaturalearth])http://ropensci.github.io/rnaturalearth/) package.

## License
This project is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License - see the [LICENSE](LICENSE.md) file for details.

## Acknowledgements

Special thanks to the V-Dem Project for providing the data and to Natural Earth for the map data.
