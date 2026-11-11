library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(vegan)
library(rinat)
library(dplyr)
library(tidyr)
library(leaflet) 
library(leaflet.extras) 

calculate_diversity_indices <- function(data) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  if (!all(sapply(data, is.numeric))) {
    stop("All columns must be numeric (species abundance counts)")
  }
  

  if (any(data < 0, na.rm = TRUE)) {
    stop("Data cannot contain negative values")
  }
  

  shannon <- vegan::diversity(data, index = "shannon")
  simpson <- vegan::diversity(data, index = "simpson")
  alpha <- vegan::specnumber(data)  
  

  results <- data.frame(
    Site = paste0("Site_", 1:nrow(data)),
    Shannon_Diversity = shannon,
    Simpson_Index = simpson,
    Alpha_Diversity = alpha,
    Total_Abundance = rowSums(data)
  )
  
  return(results)
}


create_sampling_curve <- function(data) {

  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  if (!("Time" %in% names(data))) {
    stop("Data must contain a 'Time' column")
  }
  
  if (!("Transect" %in% names(data))) {
    stop("Data must contain a 'Transect' column")
  }
  

  species_cols <- names(data)[!names(data) %in% c("Time", "Transect")]
  
  if (length(species_cols) == 0) {
    stop("No species columns found in data")
  }
  
  data <- data[order(data$Time), ]
  
  unique_times <- sort(unique(data$Time))
  cumulative_species <- numeric(length(unique_times))
  
  for (i in seq_along(unique_times)) {
    current_data <- data[data$Time <= unique_times[i], species_cols, drop = FALSE]
    species_present <- apply(current_data, 2, function(x) any(x > 0, na.rm = TRUE))
    current_species <- names(species_present)[species_present]
    cumulative_species[i] <- length(current_species)
  }
  
  all_times <- c(0, unique_times)
  all_cumulative <- c(0, cumulative_species)
  
  sampling_curve_data <- data.frame(
    Time = all_times,
    Cumulative_Species = all_cumulative,
    New_Species = c(0, all_cumulative[1], diff(cumulative_species))
  )
  
  saturation_point <- NULL
  for (i in 3:nrow(sampling_curve_data)) {
    if (sampling_curve_data$New_Species[i] == 0) {
      saturation_point <- sampling_curve_data$Time[i]
      break
    }
  }
  
 
  curve_plot <- ggplot2::ggplot(sampling_curve_data, ggplot2::aes(x = Time, y = Cumulative_Species)) +
    ggplot2::geom_line(color = "#2c3e50", linewidth = 1.5) +
    ggplot2::geom_point(ggplot2::aes(color = ifelse(New_Species > 0, "New species found", "No new species")), 
                       size = 3, alpha = 0.8) +
    ggplot2::scale_color_manual(values = c("New species found" = "#c22416ff", "No new species" = "rgba(121, 121, 123, 1)"),
                               name = "Discovery Status") +
    
    
    {if (!is.null(saturation_point)) {
     
      ggplot2::geom_vline(xintercept = saturation_point, 
                         linetype = "dashed", color = "#e74c3c", linewidth = 1.2)
    }} +
    
    {if (!is.null(saturation_point)) {
      ggplot2::annotate("rect", 
                       xmin = saturation_point - 0.5, xmax = saturation_point + 3,
                       ymin = max(cumulative_species) * 0.7, ymax = max(cumulative_species) * 0.95,
                       fill = "#fff3cd", color = "#856404", alpha = 0.8)
    }} +
    
    {if (!is.null(saturation_point)) {
      ggplot2::annotate("text", x = saturation_point + 1.25, y = max(cumulative_species) * 0.87,
                       label = "SAMPLING SATURATION", fontface = "bold", size = 3.5, color = "#856404")
    }} +
    
    {if (!is.null(saturation_point)) {
      ggplot2::annotate("text", x = saturation_point + 1.25, y = max(cumulative_species) * 0.82,
                       label = "This is the sampling effort needed", size = 3, color = "#856404")
    }} +
    
    {if (!is.null(saturation_point)) {
      ggplot2::annotate("text", x = saturation_point + 1.25, y = max(cumulative_species) * 0.78,
                       label = "to reach biodiversity saturation", size = 3, color = "#856404")
    }} +
    
    {if (!is.null(saturation_point)) {
      ggplot2::annotate("text", x = saturation_point + 1.25, y = max(cumulative_species) * 0.74,
                       label = paste("Effort required:", saturation_point, "time units"), 
                       size = 3, color = "#856404", fontface = "bold")
    }} +
    
   
    {if (!is.null(saturation_point) && max(unique_times) > saturation_point) {
      ggplot2::annotate("rect", 
                       xmin = max(unique_times) - 2.5, xmax = max(unique_times) + 0.5,
                       ymin = max(cumulative_species) * 0.2, ymax = max(cumulative_species) * 0.5,
                       fill = "#d4f7d4", color = "#155724", alpha = 0.8)
    }} +
    
    {if (!is.null(saturation_point) && max(unique_times) > saturation_point) {
      ggplot2::annotate("text", x = max(unique_times) - 1, y = max(cumulative_species) * 0.42,
                       label = "DIMINISHING RETURNS", fontface = "bold", size = 3.5, color = "#155724")
    }} +
    
    {if (!is.null(saturation_point) && max(unique_times) > saturation_point) {
      ggplot2::annotate("text", x = max(unique_times) - 1, y = max(cumulative_species) * 0.37,
                       label = "Time spent here has little payoff.", size = 3, color = "#155724")
    }} +
    
    {if (!is.null(saturation_point) && max(unique_times) > saturation_point) {
      ggplot2::annotate("text", x = max(unique_times) - 1, y = max(cumulative_species) * 0.33,
                       label = "Use sampling curves to optimize", size = 3, color = "#155724")
    }} +
    
    {if (!is.null(saturation_point) && max(unique_times) > saturation_point) {
      ggplot2::annotate("text", x = max(unique_times) - 1, y = max(cumulative_species) * 0.29,
                       label = "future research effort allocation.", size = 3, color = "#155724")
    }} +

    ggplot2::labs(
      title = "Species Discovery Curve (Remember: Maximize Information while Minimizing Effort!)",
      subtitle = "Learn how sampling effort relates to biodiversity discovery",
      x = "Time (sampling effort units)",
      y = "Cumulative species count",
      caption = "Points show when new species are discovered vs. when no new species are found"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", color = "#2c3e50"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "#7f8c8d"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 10, color = "#7f8c8d", hjust = 0)
    )
  
  return(list(
    data = sampling_curve_data,
    plot = curve_plot,
    total_species = max(cumulative_species),
    total_time_points = length(unique_times),
    saturation_point = saturation_point,
    saturation_species = if (!is.null(saturation_point)) {
      sampling_curve_data$Cumulative_Species[sampling_curve_data$Time == saturation_point]
    } else NULL
  ))
}

fetch_inaturalist_data <- function(location = NULL, bounds = NULL) {

  end_date <- Sys.Date()
  start_date <- end_date - 1095
  

  fetch_chunked_data <- function(bounds, start_date, end_date, chunk_days = 30) {
    all_observations <- data.frame()
    current_start <- start_date
    
    while (current_start <= end_date && nrow(all_observations) < 3000) {
      current_end <- min(current_start + chunk_days - 1, end_date)
      
      tryCatch({
        chunk_obs <- rinat::get_inat_obs(
          taxon_id = 3,                         # Aves (birds)
          bounds = bounds,                      # [south, west, north, east] set by the user: cool shit.
          d1 = as.character(current_start),
          d2 = as.character(current_end),
          maxresults = 100,                     
          quality = "research"
        )
        
        if (!is.null(chunk_obs) && nrow(chunk_obs) > 0) {
          all_observations <- rbind(all_observations, chunk_obs)
          
          # 3000 is resonable. 
          if (nrow(all_observations) >= 3000) {
            all_observations <- all_observations[1:3000, ]
            break
          }
        }

        Sys.sleep(0.5)
        
      }, error = function(chunk_error) {
      
        if (chunk_days > 14) {
          smaller_chunk <- fetch_chunked_data(bounds, current_start, current_end, chunk_days = 14)
          if (!is.null(smaller_chunk) && nrow(smaller_chunk) > 0) {
            all_observations <<- rbind(all_observations, smaller_chunk)
          }
        }
      })
      
      current_start <- current_end + 1
    }
    
    return(all_observations)
  }
  
  tryCatch({
    if (!is.null(bounds)) {

      observations <- tryCatch({
        rinat::get_inat_obs(
          taxon_id = 3,                         # Aves (birds)
          bounds = bounds,                      # [south, west, north, east]
          maxresults = 3000,                     # Conservative limit
          quality = "research"
        )
      }, error = function(e) {
        if (grepl("too many results", e$message, ignore.case = TRUE)) {
          fetch_chunked_data(bounds, start_date, end_date, chunk_days = 30)
        } else {
          stop(e)
        }
      })
      
    } else {
      return(list(
        data = data.frame(Species = character(0), Count = numeric(0)),
        raw_observations = NULL,
        shannon_diversity = 0,
        species_count = 0,
        total_observations = 0,
        location = "No search area selected",
        date_range = paste(start_date, "to", end_date),
        data_source = "iNaturalist API",
        error = "Please draw a search area on the map"
      ))
    }
    
    if (!is.null(observations) && nrow(observations) > 0) {
      observations <- observations[!duplicated(observations$id), ]
      species_counts <- table(observations$scientific_name)
      species_df <- data.frame(
        Species = names(species_counts),
        Count = as.numeric(species_counts),
        stringsAsFactors = FALSE
      )
      

      species_df$Photo_URL <- sapply(species_df$Species, function(sp) {
        sp_obs <- observations[observations$scientific_name == sp, ]
        if (nrow(sp_obs) > 0 && !is.null(sp_obs$image_url) && !is.na(sp_obs$image_url[1]) && sp_obs$image_url[1] != "") {
          return(sp_obs$image_url[1])
        } else {
          return(NA)  
        }
      })
      
      shannon_div <- vegan::diversity(species_counts, index = "shannon")
      
      return(list(
        data = species_df,
        raw_observations = observations,
        shannon_diversity = shannon_div,
        species_count = length(species_counts),
        total_observations = nrow(observations),
        location = location,
        date_range = paste(start_date, "to", end_date),
        data_source = "iNaturalist API"
      ))
    } else {
      return(list(
        data = data.frame(Species = character(0), Count = numeric(0)),
        raw_observations = NULL,
        shannon_diversity = 0,
        species_count = 0,
        total_observations = 0,
        location = location,
        date_range = paste(start_date, "to", end_date),
        data_source = "iNaturalist API",
        error = paste("No bird observations found in the selected area and date range")
      ))
    }
    
  }, error = function(e) {
    error_msg <- e$message
    
    if (grepl("too many results", error_msg, ignore.case = TRUE)) {
      error_msg <- "Search area is too large or time range too broad. Try selecting a smaller area or shorter time period."
    } else if (grepl("network", error_msg, ignore.case = TRUE) || grepl("internet", error_msg, ignore.case = TRUE)) {
      error_msg <- "Network connection issue. Please check your internet connection and try again."
    } else if (grepl("timeout", error_msg, ignore.case = TRUE)) {
      error_msg <- "Request timed out. Try selecting a smaller search area or shorter time period."
    }

    return(list(
      data = data.frame(Species = character(0), Count = numeric(0)),
      raw_observations = NULL,
      shannon_diversity = 0,
      species_count = 0,
      total_observations = 0,
      location = location,
      date_range = paste(start_date, "to", end_date),
      data_source = "iNaturalist API",
      error = paste("API Error:", error_msg)
    ))
  })
}

# Function to create species accumulation curve
create_species_accumulation_curve <- function(data) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  # Get numeric columns (species data)
  numeric_cols <- sapply(data, is.numeric)
  species_data <- data[, numeric_cols, drop = FALSE]
  
  if (ncol(species_data) == 0) {
    stop("No numeric species columns found")
  }
  
  # Create presence/absence data
  species_pa <- species_data
  species_pa[species_pa > 0] <- 1
  
  # Track cumulative number of unique species
  cumulative_unique_species <- numeric(length = nrow(species_pa))
  species_seen <- rep(0, ncol(species_pa))
  
  for (i in 1:nrow(species_pa)) {
    species_seen <- pmax(species_seen, species_pa[i, ])
    cumulative_unique_species[i] <- sum(species_seen)
  }
  
  # Create dataframe for plotting
  accumulation_df <- data.frame(
    Site = paste0("Site_", 1:nrow(species_data)),
    Site_Number = 1:nrow(species_data),
    Cumulative_Species = cumulative_unique_species
  )
  
  return(accumulation_df)
}

# Function to create rank abundance data
create_rank_abundance_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  
  # Get numeric columns (species data)
  numeric_cols <- sapply(data, is.numeric)
  species_data <- data[, numeric_cols, drop = FALSE]
  
  if (ncol(species_data) == 0) {
    stop("No numeric species columns found")
  }
  
  # Calculate total abundance for each species across all sites
  species_totals <- colSums(species_data, na.rm = TRUE)
  
  # Remove species with zero abundance
  species_totals <- species_totals[species_totals > 0]
  
  # Create rank abundance dataframe
  rank_abundance_df <- data.frame(
    Species = names(species_totals),
    Total_Abundance = as.numeric(species_totals),
    stringsAsFactors = FALSE
  )
  
  # Sort by abundance (descending)
  rank_abundance_df <- rank_abundance_df[order(rank_abundance_df$Total_Abundance, decreasing = TRUE), ]
  rank_abundance_df$Rank <- 1:nrow(rank_abundance_df)
  
  return(rank_abundance_df)
}

server <- function(input, output, session) {
  
  values <- reactiveValues(
    uploaded_data = NULL,
    diversity_results = NULL,
    inaturalist_data = NULL,
    sampling_data = NULL,
    sampling_results = NULL,
    selected_species = character(0),  
    search_bounds = NULL,  
    clicked_species = NULL,  
    species_photo_url = NULL 
  )
  
  observeEvent(input$file1, {
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$datapath)
    if (ext != "csv") {
      showNotification("Please upload a CSV file", type = "error")
      return()
    }
    
    tryCatch({
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
      
      cat("Data uploaded, dimensions:", dim(df), "\n")
      cat("Column names:", names(df), "\n")
      
      numeric_cols <- sapply(df, is.numeric)
      cat("Numeric columns:", sum(numeric_cols), "\n")
      
      if (!any(numeric_cols)) {
        showNotification("Data must contain numeric columns for species counts", type = "error")
        return()
      }
      
      values$uploaded_data <- df
      showNotification("Data uploaded successfully!", type = "message")
    
      numeric_data <- df[, numeric_cols, drop = FALSE]
      cat("Processing numeric data, dimensions:", dim(numeric_data), "\n")
      
      if (any(numeric_data < 0, na.rm = TRUE)) {
        showNotification("Warning: Negative values found in data", type = "warning")
      }
      
      diversity_results <- calculate_diversity_indices(numeric_data)
      cat("Diversity results calculated, dimensions:", dim(diversity_results), "\n")
      values$diversity_results <- diversity_results
      
    }, error = function(e) {
      cat("Error in data upload:", e$message, "\n")
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  output$data_preview <- DT::renderDataTable({
    if (is.null(values$uploaded_data)) {
      return(data.frame(Message = "No data uploaded yet"))
    }
    DT::datatable(values$uploaded_data, options = list(scrollX = TRUE, pageLength = 10))
  })

  output$data_uploaded <- reactive({
    !is.null(values$uploaded_data)
  })
  outputOptions(output, "data_uploaded", suspendWhenHidden = FALSE)

  output$diversity_table <- DT::renderDataTable({
    req(values$diversity_results)
    DT::datatable(values$diversity_results, 
                  options = list(pageLength = 10, scrollX = TRUE)) %>%
      DT::formatRound(columns = c("Shannon_Diversity", "Simpson_Index"), digits = 3)
  })

  output$diversity_plot <- renderPlotly({
    req(values$diversity_results, values$uploaded_data)
    
    plot_type <- input$plot_type %||% "bar_diversity"
    
    if (plot_type == "bar_diversity") {

      diversity_long <- tidyr::pivot_longer(
        values$diversity_results, 
        cols = c("Shannon_Diversity", "Simpson_Index", "Alpha_Diversity"),
        names_to = "Index_Type", 
        values_to = "Value"
      )
      
      p <- ggplot(diversity_long, aes(x = Site, y = Value, fill = Index_Type)) +
        geom_col(position = "dodge", alpha = 0.8) +
        labs(title = "Diversity Indices by Site",
             x = "Site", y = "Diversity Index Value",
             fill = "Index Type") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values = c("Shannon_Diversity" = "#0294f6ff", 
                                   "Simpson_Index" = "#221615ff", 
                                   "Alpha_Diversity" = "#0a15e9ff"))
      
    } else if (plot_type == "boxplot_abundance") {
      numeric_cols <- sapply(values$uploaded_data, is.numeric)
      species_data <- values$uploaded_data[, numeric_cols, drop = FALSE]
      species_long <- tidyr::pivot_longer(species_data, 
                                        cols = everything(), 
                                        names_to = "Species", 
                                        values_to = "Abundance")
      
      p <- ggplot(species_long, aes(x = Species, y = Abundance)) +
        geom_boxplot(fill = "#060a0cff", alpha = 0.7) +
        labs(title = "Species Abundance Distribution",
             x = "Species", y = "Abundance Count") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (plot_type == "scatter_rich_abund") {
      p <- ggplot(values$diversity_results, aes(x = Total_Abundance, y = Alpha_Diversity)) +
        geom_point(size = 3, color = "#140a08ff", alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "#0498faff") +
        labs(title = "Species Richness vs Total Abundance",
             x = "Total Abundance", y = "Species Richness (Alpha Diversity)") +
        theme_light()
      
    } else if (plot_type == "heatmap_species") {
      numeric_cols <- sapply(values$uploaded_data, is.numeric)
      species_data <- values$uploaded_data[, numeric_cols, drop = FALSE]
      species_data$Site <- paste0("Site_", 1:nrow(species_data))
      
      species_long <- tidyr::pivot_longer(species_data, 
                                        cols = -Site, 
                                        names_to = "Species", 
                                        values_to = "Abundance")
      
      p <- ggplot(species_long, aes(x = Species, y = Site, fill = Abundance)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "#f75db2ff") +
        labs(title = "Species Abundance Heatmap",
             x = "Species", y = "Site", fill = "Abundance") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    } else if (plot_type == "species_accumulation") {
      # Species accumulation curve
      accum_data <- create_species_accumulation_curve(values$uploaded_data)
      accum_data$Site <- accum_data$Site_Number
      p <- ggplot(accum_data, aes(x = as.factor(Site), y = Cumulative_Species, group = 1)) +
        geom_line(color = "black", linewidth = 1.2) +
        geom_point(size = 3, color = "red") +
        geom_text(aes(label = Cumulative_Species), vjust = -1, size = 5) +
        labs(title = "Species Accumulation by Site",
             x = "Site", y = "Cumulative Number of Species") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    } else if (plot_type == "rank_abundance") {
      # Rank abundance curve
      rank_data <- create_rank_abundance_data(values$uploaded_data)
      
      p <- ggplot(rank_data, aes(x = reorder(Species, -Total_Abundance), y = Total_Abundance)) +
        geom_col(fill = "#2c3e50", alpha = 0.8) +
        labs(title = "Rank Abundance Curve",
             x = "Bird Species (Ranked by Abundance)", 
             y = "Total Abundance") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  output$plot_code <- renderText({
    plot_type <- input$plot_type %||% "bar_diversity"
    
    if (plot_type == "bar_diversity") {
      code <- '# Bar Chart of Diversity Indices
library(ggplot2)
library(tidyr)

# Create bar chart
ggplot(diversity_long, aes(x = Site, y = Value, fill = Index_Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Diversity Indices by Site",
       x = "Site", y = "Diversity Index Value") +
  theme_light()'

  
      
    } else if (plot_type == "boxplot_abundance") {
      code <- '# Box Plot of Species Abundance
library(ggplot2)
library(tidyr)

# Create box plot
ggplot(species_long, aes(x = Species, y = Abundance)) +
  geom_boxplot(fill = "#3498db", alpha = 0.7) +
  labs(title = "Species Abundance Distribution",
       x = "Species", y = "Abundance Count") +
  theme_light()'
      
    } else if (plot_type == "scatter_rich_abund") {
      code <- '# Scatter Plot: Richness vs Abundance
library(ggplot2)

# Create scatter plot with regression line
ggplot(diversity_results, aes(x = Total_Abundance, y = Alpha_Diversity)) +
  geom_point(size = 3, color = "#e74c3c", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#3498db") +
  labs(title = "Species Richness vs Total Abundance",
       x = "Total Abundance", 
       y = "Species Richness (Alpha Diversity)") +
  theme_light()'
      
    } else if (plot_type == "heatmap_species") {
      code <- '# Heatmap of Species by Site
library(ggplot2)
library(tidyr)


# Create heatmap
ggplot(species_long, aes(x = Species, y = Site, fill = Abundance)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#3498db") +
  labs(title = "Species Abundance Heatmap",
       x = "Species", y = "Site") +
  theme_light()'
  
    } else if (plot_type == "species_accumulation") {
      code <- '# Species Accumulation Curve
library(ggplot2)

# Create presence/absence data
species_pa <- species_data
species_pa[species_pa > 0] <- 1

# Track cumulative species
cumulative_species <- numeric(nrow(species_pa))
species_seen <- rep(0, ncol(species_pa))

for (i in 1:nrow(species_pa)) {
  species_seen <- pmax(species_seen, species_pa[i, ])
  cumulative_species[i] <- sum(species_seen)
}

# Plot accumulation curve
ggplot(accum_data, aes(x = Site, y = Cumulative_Species, group = 1)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(size = 3, color = "red") +
  labs(title = "Species Accumulation by Site",
       x = "Site", y = "Cumulative Species") +
  theme_light()'
  
    } else if (plot_type == "rank_abundance") {
      code <- '# Rank Abundance Curve
library(ggplot2)
library(dplyr)

# Calculate total abundance per species
species_totals <- colSums(species_data, na.rm = TRUE)
rank_data <- data.frame(
  Species = names(species_totals),
  Total_Abundance = species_totals
) %>%
  arrange(desc(Total_Abundance))

# Plot rank abundance curve
ggplot(rank_data, aes(x = reorder(Species, -Total_Abundance), 
                      y = Total_Abundance)) +
  geom_col(fill = "#2c3e50", alpha = 0.8) +
  labs(title = "Rank Abundance Curve",
       x = "Species (Ranked)", y = "Total Abundance") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))'
    }
    
    return(code)
  })

  observeEvent(input$days_back, {
    if (!is.null(values$inaturalist_data) && !is.null(input$location) && input$location != "") {
      showNotification("Days changed - click 'Fetch Data' to update results", type = "message")
    }
  })
  
  output$search_area_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -91.1, lat = 30.3, zoom = 6) %>%  # Center on Louisiana
      addDrawToolbar(
        targetGroup = "search_area",
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(
          shapeOptions = drawShapeOptions(
            fillOpacity = 0.2,
            color = "#0022ffff",
            weight = 3
          )
        ),
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()
        )
      ) %>%
      addLayersControl(
        overlayGroups = "search_area",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$search_area_map_draw_new_feature, {
    feature <- input$search_area_map_draw_new_feature
    
    if (feature$properties$feature_type == "rectangle") {
      coords <- feature$geometry$coordinates[[1]]

      lats <- sapply(coords, function(x) x[[2]])
      lngs <- sapply(coords, function(x) x[[1]])
      
      bounds <- c(
        min(lats),  # south
        min(lngs),  # west
        max(lats),  # north
        max(lngs)   # east
      )
      
      values$search_bounds <- bounds
      
      showNotification(
        paste("Search area selected! Bounds:", 
              round(bounds[1], 3), ",", round(bounds[2], 3), "to", 
              round(bounds[3], 3), ",", round(bounds[4], 3)),
        type = "message"
      )
    }
  })
  
  observeEvent(input$search_area_map_draw_deleted_features, {
    values$search_bounds <- NULL
    showNotification("Search area cleared", type = "message")
  })
  
  observeEvent(input$fetch_data_map, {
    if (!is.null(values$search_bounds)) {
      withProgress(message = 'Fetching iNaturalist data...', value = 0, {
        incProgress(0.5, detail = "Searching map area for past year")
        
        tryCatch({
          inaturalist_results <- fetch_inaturalist_data(
            bounds = values$search_bounds
          )
          
          bounds_text <- paste0("Map Area [", 
                              round(values$search_bounds[1], 2), ",", round(values$search_bounds[2], 2), 
                              " to ", 
                              round(values$search_bounds[3], 2), ",", round(values$search_bounds[4], 2), "]")
          inaturalist_results$location <- bounds_text
          
          values$inaturalist_data <- inaturalist_results
          
        }, error = function(e) {
          showNotification(paste("Error fetching map area data:", e$message), type = "error")
        })
        
        if (!is.null(values$inaturalist_data)) {
          if (!is.null(values$inaturalist_data$error)) {
            showNotification(values$inaturalist_data$error, type = "error")
          } else if (values$inaturalist_data$total_observations == 0) {
            showNotification("No bird observations found for this area and date range", type = "warning")
          } else {
            showNotification(
              paste("Fetched", values$inaturalist_data$total_observations, 
                    "observations of", values$inaturalist_data$species_count, "species from iNaturalist"),
              type = "message"
            )
          }
        }
        
        incProgress(0.5, detail = "Complete: Good Job!")
      })
    } else {
      showNotification("Please draw a search area on the map first", type = "warning")
    }
  })
  
  output$inaturalist_fetched <- reactive({
    !is.null(values$inaturalist_data) && !is.null(values$inaturalist_data$data)
  })
  outputOptions(output, "inaturalist_fetched", suspendWhenHidden = FALSE)
  
  output$inaturalist_summary <- renderText({
    req(values$inaturalist_data)
    
    if (!is.null(values$inaturalist_data$error)) {
      return(values$inaturalist_data$error)
    }
    
    paste(
      "Location:", values$inaturalist_data$location, "\n",
      "Date Range:", values$inaturalist_data$date_range, "\n", 
      "Data Source:", values$inaturalist_data$data_source, "\n",
      "Total Observations:", values$inaturalist_data$total_observations, "\n",
      "Species Count:", values$inaturalist_data$species_count, "\n",
      "Shannon Diversity:", round(values$inaturalist_data$shannon_diversity, 3)
    )
  })
  
  output$download_inaturalist <- downloadHandler(
    filename = function() {
      paste("iNaturalist_birds_", values$inaturalist_data$location, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$inaturalist_data$raw_observations)) {
        write.csv(values$inaturalist_data$raw_observations, file, row.names = FALSE)
      } else {
        write.csv(values$inaturalist_data$data, file, row.names = FALSE)
      }
    }
  )
  
  output$inaturalist_plot <- renderPlotly({
    req(values$inaturalist_data)
    
    if (is.null(values$inaturalist_data$data) || nrow(values$inaturalist_data$data) == 0) {
      return(plotly::plot_ly() %>% 
             plotly::add_text(x = 0.5, y = 0.5, text = "No iNaturalist data available", 
                            textfont = list(size = 16)) %>%
             plotly::layout(showlegend = FALSE, 
                          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
                          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)))
    }
    
    plot_type <- input$inaturalist_plot_type %||% "bar_species"
    inaturalist_df <- values$inaturalist_data$data
    
    if (plot_type == "bar_species") {
      inaturalist_df <- inaturalist_df[order(inaturalist_df$Count, decreasing = TRUE), ]
      
      num_species <- input$num_species_display %||% 15
      num_species <- min(num_species, nrow(inaturalist_df))
      inaturalist_df <- head(inaturalist_df, num_species)
      
      inaturalist_df$hover_text <- paste0(
        "<b>", inaturalist_df$Species, "</b><br>",
        "Observations: ", inaturalist_df$Count, "<br>",
        "Click bar to see photo"
      )
      
      # Keep the scientific name attached to each bar. Plotly's pointNumber is
      # not a reliable index into the original data after ggplot reorders bars.
      p <- ggplot(inaturalist_df, aes(x = reorder(Species, Count), y = Count,
                                      text = hover_text, key = Species)) +
        geom_col(fill = "#24a4efff", alpha = 0.8) +
        coord_flip() +
        labs(title = paste("iNaturalist Species Abundance - Top", num_species, "Species"),
             subtitle = paste("Species in", values$inaturalist_data$location),
             x = "Species", y = "Number of Observations") +
        theme_light()
      
      plot_ly_obj <- ggplotly(p, tooltip = "text", source = "species_plot")
      plot_ly_obj <- plotly::event_register(plot_ly_obj, "plotly_click")
      
      return(plot_ly_obj)
        
    } else if (plot_type == "comparison_diversity" && !is.null(values$diversity_results)) {
      user_avg_shannon <- mean(values$diversity_results$Shannon_Diversity, na.rm = TRUE)
      inaturalist_shannon <- values$inaturalist_data$shannon_diversity
      
      comparison_df <- data.frame(
        Source = c("Your Survey Data", "iNaturalist Data"),
        Shannon_Diversity = c(user_avg_shannon, inaturalist_shannon),
        Type = c("Survey", "Citizen Science")
      )
      
      p <- ggplot(comparison_df, aes(x = Source, y = Shannon_Diversity, fill = Type)) +
        geom_col(alpha = 0.8, width = 0.6) +
        labs(title = "Shannon Diversity Comparison",
             subtitle = "Survey Data vs Citizen Science Data",
             x = "Data Source", y = "Shannon Diversity Index") +
        theme_light() +
        scale_fill_manual(values = c("Survey" = "#2c29f0f1", "Citizen Science" = "#100d08ff")) +
        geom_text(aes(label = round(Shannon_Diversity, 3)), 
                  vjust = -0.5, fontface = "bold")
      
      return(ggplotly(p))
        
    } else if (plot_type == "interactive_map") {
      return(NULL)
        
    } else if (plot_type == "monthly_diversity") {
      if (!is.null(values$inaturalist_data$raw_observations) && 
          "observed_on" %in% names(values$inaturalist_data$raw_observations)) {
        
        obs_data <- values$inaturalist_data$raw_observations
        obs_data$date <- as.Date(obs_data$observed_on)
        obs_data <- obs_data[!is.na(obs_data$date), ]

        twelve_months_ago <- Sys.Date() - 1095
        obs_data <- obs_data[obs_data$date >= twelve_months_ago, ]
        
        if (nrow(obs_data) > 0) {
          obs_data$year_month <- format(obs_data$date, "%Y-%m")
          monthly_diversity <- obs_data %>%
            group_by(year_month) %>%
            summarise(
              shannon_diversity = vegan::diversity(table(scientific_name), index = "shannon"),
              species_count = n_distinct(scientific_name),
              total_obs = n(),
              .groups = 'drop'
            )

          monthly_diversity$date <- as.Date(paste0(monthly_diversity$year_month, "-01"))

          monthly_diversity <- monthly_diversity[monthly_diversity$date >= twelve_months_ago, ]
          monthly_diversity <- monthly_diversity[order(monthly_diversity$date), ]
          
          if (nrow(monthly_diversity) == 0) {
            return(plotly::plot_ly() %>% 
                   plotly::add_text(x = 0.5, y = 0.5, text = "No data available for the past 12 months", 
                                  textfont = list(size = 14)) %>%
                   plotly::layout(showlegend = FALSE))
          }
          
          p <- ggplot(monthly_diversity, aes(x = date, y = shannon_diversity)) +
            geom_line(color = "#4f34feff", size = 1.2) +
            geom_point(color = "#162432ff", size = 3, alpha = 0.7) +
            labs(title = "Monthly Shannon Diversity - Past 12 Months",
                 subtitle = paste("Based on", sum(monthly_diversity$total_obs), "iNaturalist observations"),
                 x = "Month", y = "Shannon Diversity Index") +
            theme_light() +
            scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          if (!is.null(values$diversity_results)) {
            user_avg_shannon <- mean(values$diversity_results$Shannon_Diversity, na.rm = TRUE)
            current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
            
            p <- p + 
              geom_point(aes(x = current_month, y = user_avg_shannon), 
                        color = "#fd1e06ff", size = 6, shape = 17) +
              annotate("text", x = current_month, y = user_avg_shannon + 0.3, 
                      label = "Your Survey Data", color = "#120706ff", 
                      fontface = "bold", hjust = 0.5)
          }
          
          if (nrow(monthly_diversity) > 0) {
            latest_month <- monthly_diversity[nrow(monthly_diversity), ]
            p <- p + 
              geom_point(aes(x = latest_month$date, y = latest_month$shannon_diversity), 
                        color = "#3012f3ff", size = 6, shape = 16) +
              annotate("text", x = latest_month$date, y = latest_month$shannon_diversity + 0.3, 
                      label = "Latest iNaturalist", color = "#141009ff", 
                      fontface = "bold", hjust = 0.5)
          }
          
          return(ggplotly(p))
        }
      }
      
      return(plotly::plot_ly() %>% 
             plotly::add_text(x = 0.5, y = 0.5, text = "No temporal data available for monthly trends: Try Again!", 
                            textfont = list(size = 14)) %>%
             plotly::layout(showlegend = FALSE))
        
    } else {
      p <- ggplot(inaturalist_df, aes(x = reorder(Species, Count), y = Count)) +
        geom_col(fill = "#2812f3ff", alpha = 0.8) +
        coord_flip() +
        labs(title = "iNaturalist Species Data", x = "Species", y = "Count") +
        theme_light()
      
      return(ggplotly(p))
    }
  })

  output$inaturalist_map <- renderLeaflet({
    req(values$inaturalist_data)
    
    if (is.null(values$inaturalist_data$raw_observations) || 
        input$inaturalist_plot_type != "interactive_map") {
      return(NULL)
    }
    
    obs_data <- values$inaturalist_data$raw_observations
    valid_coords <- !is.na(obs_data$latitude) & !is.na(obs_data$longitude)
    map_data <- obs_data[valid_coords, ]
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = -91.1, lat = 30.3, popup = "No coordinates available for observations"))
    }

    if (length(values$selected_species) > 0) {
      map_data <- map_data[map_data$scientific_name %in% values$selected_species, ]
    }
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
        addTiles() %>% 
        setView(lng = -91.1, lat = 30.3, zoom = 8) %>%
        addMarkers(lng = -91.1, lat = 30.3, popup = "No observations for selected species"))
    }

    unique_species <- unique(map_data$scientific_name)
    colors <- rainbow(length(unique_species))
    species_colors <- setNames(colors, unique_species)
    map_data$color <- species_colors[map_data$scientific_name]
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~color,
        popup = ~paste0(
          "<b>", scientific_name, "</b><br>",
          "Common Name: ", common_name, "<br>",
          "Date: ", observed_on, "<br>",
          "Observer: ", user_login
        ),
        radius = 6,
        fillOpacity = 0.8,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        colors = colors,
        labels = unique_species,
        title = "Bird Species",
        opacity = 0.8
      )
  })
  
  output$species_table <- DT::renderDataTable({
    req(values$inaturalist_data)
    
    if (is.null(values$inaturalist_data$data) || 
        input$inaturalist_plot_type != "interactive_map") {
      return(NULL)
    }
    
    species_df <- values$inaturalist_data$data
    species_df <- species_df[order(species_df$Count, decreasing = TRUE), ]
    
    DT::datatable(
      species_df[, c("Species", "Count")],
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        selection = list(mode = 'multiple', target = 'row'),
        columnDefs = list(
          list(width = '70%', targets = 0), 
          list(width = '30%', targets = 1, className = 'dt-center') 
        ),
        autoWidth = FALSE
      ),
      caption = "Click species to show their observations over the past year",
      selection = 'multiple'
    ) %>%
      DT::formatStyle(
        'Count',
        fontWeight = 'bold',
        color = '#2c3e50',
        backgroundColor = '#ecf0f1'
      )
  })
  
  observeEvent(input$species_table_rows_selected, {
    req(values$inaturalist_data$data)
    
    if (length(input$species_table_rows_selected) > 0) {
      species_df <- values$inaturalist_data$data
      species_df <- species_df[order(species_df$Count, decreasing = TRUE), ]
      selected_rows <- input$species_table_rows_selected
      values$selected_species <- species_df$Species[selected_rows]
    } else {
      values$selected_species <- character(0)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "species_plot"), {
    click_data <- event_data("plotly_click", source = "species_plot")
    
    if (!is.null(click_data) && !is.null(values$inaturalist_data$data)) {

      # Use the species key emitted by ggplotly, rather than pointNumber, whose
      # order changes when ggplot reorders categories (especially with coord_flip).
      clicked_species_name <- as.character(click_data$key[1])
      species_row <- values$inaturalist_data$data[
        values$inaturalist_data$data$Species == clicked_species_name, , drop = FALSE
      ]

      if (nzchar(clicked_species_name) && nrow(species_row) > 0) {
        values$clicked_species <- clicked_species_name
        values$species_photo_url <- species_row$Photo_URL[1]
        showNotification(paste("Look, it's", clicked_species_name), type = "message")
      }
    }
  })

  # Launch reference sites with the operating system's default browser. This
  # avoids Positron Viewer treating external anchor targets as internal pages.
  observeEvent(input$open_inaturalist_species, {
    req(values$clicked_species)
    species_query <- utils::URLencode(enc2utf8(values$clicked_species), reserved = TRUE)
    utils::browseURL(paste0("https://www.inaturalist.org/taxa/search?q=", species_query))
  }, ignoreInit = TRUE)

  observeEvent(input$open_gbif_species, {
    req(values$clicked_species)
    species_query <- utils::URLencode(enc2utf8(values$clicked_species), reserved = TRUE)
    utils::browseURL(paste0("https://www.gbif.org/species/search?q=", species_query))
  }, ignoreInit = TRUE)

  observeEvent(input$open_onezoom_species, {
    req(values$clicked_species)
    onezoom_name <- gsub(" ", "_", trimws(enc2utf8(values$clicked_species)), fixed = TRUE)
    onezoom_name <- utils::URLencode(onezoom_name, reserved = TRUE)
    utils::browseURL(paste0("https://www.onezoom.org/life.html/@", onezoom_name))
  }, ignoreInit = TRUE)
  
  output$species_photo_display <- renderUI({
    if (!is.null(values$clicked_species)) {
      div(
        style = "text-align: center; padding: 30px; background-color: #f8f9fa; border-radius: 15px; margin-top: 30px; box-shadow: 0 5px 15px rgba(0,0,0,0.1);",
        
        # Species name header
        h2(values$clicked_species, 
           style = "color: #2c3e50; font-weight: bold; margin-bottom: 25px;"),
        
        # Large prominent photo
        if (!is.null(values$species_photo_url) && !is.na(values$species_photo_url)) {
          tags$img(
            src = values$species_photo_url,
            alt = paste("Photo of", values$clicked_species),
            style = "max-width: 90%; max-height: 600px; border-radius: 15px; box-shadow: 0 10px 30px rgba(0,0,0,0.2); margin-bottom: 20px;",
            onclick = "window.open(this.src, '_blank');"
          )
        } else {
          div(
            style = "background-color: #ecf0f1; border: 2px dashed #bdc3c7; border-radius: 15px; padding: 80px; color: #7f8c8d; margin-bottom: 20px;",
            icon("image", style = "font-size: 64px; margin-bottom: 20px;"),
            br(),
            h4("Photo not available for this species", style = "margin: 0;")
          )
        },
        

        if (!is.null(values$inaturalist_data$data)) {
          species_info <- values$inaturalist_data$data[values$inaturalist_data$data$Species == values$clicked_species, ]
          if (nrow(species_info) > 0) {
            h4(paste(species_info$Count, "observations in your search area"), 
               style = "color: #27ae60; margin: 20px 0;")
          }
        },
        
        div(
          style = "background-color: white; padding: 25px; border-radius: 12px; margin: 20px 0; box-shadow: 0 3px 10px rgba(0,0,0,0.1);",
          h4("Learn More About This Species", style = "color: #2c3e50; margin-bottom: 20px; text-align: center;"),
          
          div(
            style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap;",
            
            # Search each site with the scientific name. Query strings must be
            # encoded separately from path segments.
            # iNaturalist link
            actionLink(
              "open_inaturalist_species",
              label = div(
                style = "background-color: #74b816; color: white; padding: 15px 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.2); transition: all 0.3s; cursor: pointer; min-width: 150px;",
                icon("camera", style = "font-size: 20px; margin-bottom: 8px;"),
                br(),
                strong("iNaturalist"),
                br(),
                span("Citizen Science Photos & Observations", style = "font-size: 12px;")
              ),
              style = "display: inline-block; text-decoration: none;"
            ),
            
            # GBIF link  
            actionLink(
              "open_gbif_species",
              label = div(
                style = "background-color: #509e2f; color: white; padding: 15px 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.2); transition: all 0.3s; cursor: pointer; min-width: 150px;",
                icon("globe", style = "font-size: 20px; margin-bottom: 8px;"),
                br(),
                strong("GBIF"),
                br(),
                span("Global Biodiversity Database", style = "font-size: 12px;")
              ),
              style = "display: inline-block; text-decoration: none;"
            ),
            
            # OneZoom link
            actionLink(
              "open_onezoom_species",
              label = div(
                style = "background-color: #2c5aa0; color: white; padding: 15px 20px; border-radius: 8px; text-align: center; box-shadow: 0 2px 5px rgba(0,0,0,0.2); transition: all 0.3s; cursor: pointer; min-width: 150px;",
                icon("sitemap", style = "font-size: 20px; margin-bottom: 8px;"),
                br(),
                strong("OneZoom"),
                br(),
                span("Tree of Life Explorer", style = "font-size: 12px;")
              ),
              style = "display: inline-block; text-decoration: none;"
            )
          ),
          
          br(),
          p("Click the links above to explore this species across different scientific databases and platforms!", 
            style = "text-align: center; color: #7f8c8d; font-style: italic; margin-top: 15px; margin-bottom: 0;")
        ),
        
        p("Click on other species bars in the chart above to explore more birds!", 
          style = "color: #7f8c8d; font-style: italic; margin-top: 20px;")
      )
    }
  })
  

  output$inaturalist_code <- renderText({
    plot_type <- input$inaturalist_plot_type %||% "bar_species"
    
    if (plot_type == "bar_species") {
      code <- '# Clickable Bar Chart of iNaturalist Species (Dynamic)
library(ggplot2)
library(plotly)

# Create horizontal bar chart
p <- ggplot(top_species, aes(x = reorder(Species, Count), y = Count)) +
  geom_col(fill = "#345ffaff", alpha = 0.8) +
  coord_flip() +
  labs(title = paste("iNaturalist Species Abundance - Top", num_species, "Species"),
       x = "Species", y = "Number of Observations") +
  theme_light()'
  
    } else if (plot_type == "comparison_diversity") {
      code <- '# Diversity Comparison Bar Chart
library(ggplot2)
library(plotly)

# Create comparison bar chart
p <- ggplot(comparison_df, aes(x = Source, y = Shannon_Diversity, fill = Type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  labs(title = "Shannon Diversity Comparison",
       x = "Data Source", y = "Shannon Diversity Index") +
  theme_light() +
  geom_text(aes(label = round(Shannon_Diversity, 3)), vjust = -0.5)

ggplotly(p)'
  
    } else if (plot_type == "interactive_map") {
      code <- '# Interactive Species Map with Selection Table
library(leaflet)
library(DT)

# This one is tricky. 
unique_species <- unique(map_data$scientific_name)
colors <- rainbow(length(unique_species))
species_colors <- setNames(colors, unique_species)

leaflet(map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~species_colors[scientific_name],
    popup = ~paste0("<b>", scientific_name, "</b><br>",
                   "Date: ", observed_on),
    radius = 6, fillOpacity = 0.8
  ) %>%
  addLegend(colors = colors, labels = unique_species,
           title = "Bird Species")'
  
    } else if (plot_type == "multi_location_comparison") {
      code <- '# Multi-Location Biodiversity Comparison
library(ggplot2)
library(dplyr)
library(vegan)

ggplot(combined_data, aes(x = date, y = shannon_diversity, 
                         color = location)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Biodiversity Comparison Across Locations",
       x = "Date", y = "Shannon Diversity Index") +
  theme_light() +
  scale_color_brewer(type = "qual", palette = "Set2")'
    }
    
    return(code)
  })
  
}
