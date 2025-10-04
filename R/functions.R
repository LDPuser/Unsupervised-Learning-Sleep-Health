# Custom Functions for Sleep Health Analysis
# Author: Louis-David PIRON
# Date: 2024


# ============================================================================
# DATA LOADING FUNCTION
# ============================================================================

#' Load sleep health dataset
#'
#' @param filename Name of the CSV file (default: "sleep_health_data.csv")
#' @return Data frame with sleep health data
load_sleep_data <- function(filename = "sleep_health_data.csv") {
  
  data <- read.csv(file.path("..", "data", filename), 
                   sep = ";", 
                   dec = ".", 
                   row.names = 1
  )
  
  message("Data loaded successfully: ", nrow(data), " observations, ", 
          ncol(data), " variables")
  
  return(data)
}


# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

#' Plot histogram for a given variable
#'
#' @param data Data frame containing the variables
#' @param var_index Index of the variable to plot
#' @return A histogram plot
plot_histogram <- function(data, var_index) {
  df <- data.frame(value = data[, var_index])
  
  ggplot(df, aes(x = value)) +
    geom_histogram(bins = 30, fill = "lightgray", color = "white", linewidth = 0.3) +
    labs(
      title = paste("Distribution of", colnames(data)[var_index]),
      x = colnames(data)[var_index],
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
}

#' Plot barplot for categorical variables
#'
#' @param data Data frame containing the variables
#' @param var_index Index of the variable to plot
#' @param las Label orientation (0 = parallel, 2 = perpendicular)
#' @return A barplot
plot_categorical <- function(data, var_index, las = 0) {
  freq_table <- as.data.frame(table(data[var_index]))
  colnames(freq_table) <- c("category", "count")
  
  p <- ggplot(freq_table, aes(x = category, y = count)) +
    geom_bar(stat = "identity", fill = "lightgray", color = "white", linewidth = 0.3) +
    labs(
      title = colnames(data)[var_index],
      x = "",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  # Rotation des labels si las = 2
  if (las == 2) {
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  
  return(p)
}

## ============================================================================
# PCA QUALITY ASSESSMENT
# ============================================================================

#' Calculate and visualize quality of representation for variables
#'
#' @param pca_result Result object from PCA function
#' @param n_components Number of principal components to consider
#' @param threshold Threshold for good representation (default: 0.5)
#' @return A barplot showing cos2 values
quality_representation_var <- function(pca_result, n_components = 2, threshold = 0.5) {
  cos2_sum <- round(sort(rowSums(pca_result$var$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    variable = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = variable, y = cos2, fill = quality)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = "#E74C3C", "Good" = "lightgray")) +
    labs(
      title = paste("Variable Quality (p =", n_components, ")"),
      x = "",
      y = expression(cos^2)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  print(p)
  return(cos2_sum)
}

#' Calculate and visualize quality of representation for individuals
#'
#' @param pca_result Result object from PCA function
#' @param n_components Number of principal components to consider
#' @param threshold Threshold for good representation (default: 0.5)
#' @return Number of poorly represented individuals
quality_representation_ind <- function(pca_result, n_components = 2, threshold = 0.5) {
  cos2_sum <- round(sort(rowSums(pca_result$ind$cos2[, 1:n_components])), digits = 3)
  n_poor <- sum(cos2_sum < threshold)
  n_total <- length(cos2_sum)
  
  df <- data.frame(
    individual = 1:length(cos2_sum),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = individual, y = cos2, fill = quality)) +
    geom_bar(stat = "identity", width = 1, color = NA) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = "#E74C3C", "Good" = "lightgray")) +
    labs(
      title = paste("Individual Quality (p =", n_components, ")"),
      subtitle = paste(n_poor, "out of", n_total, "individuals poorly represented"),
      x = "Individual",
      y = expression(cos^2)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  print(p)
  return(n_poor)
}

#' Quality of representation for categorical variables in PCA
#'
#' @param data Data frame with quantitative and categorical variables
#' @param quanti_indices Indices of quantitative variables
#' @param quali_indices Indices of categorical variables
#' @param n_components Number of principal components
#' @param threshold Threshold for good representation
#' @return Barplot of cos2 values for categorical levels
quality_representation_categorical <- function(data, quanti_indices, quali_indices, 
                                               n_components = 2, threshold = 0.5) {
  pca_result <- PCA(data, 
                    ncp = 3, 
                    graph = FALSE, 
                    quali.sup = quali_indices)
  
  cos2_sum <- round(sort(rowSums(pca_result$quali.sup$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    category = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = category, y = cos2, fill = quality)) +
    geom_bar(stat = "identity", color = "white", linewidth = 0.3) +
    geom_hline(yintercept = threshold, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = "#E74C3C", "Good" = "lightgray")) +
    labs(
      title = paste("Categorical Levels Quality (p =", n_components, ")"),
      x = "",
      y = expression(cos^2)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
  
  print(p)
  return(cos2_sum)
}

# ============================================================================
# PCA VISUALIZATION WITH CATEGORICAL OVERLAY
# ============================================================================

#' Add categorical variable overlay to PCA individual plot
#'
#' @param data Data frame containing all variables
#' @param quanti_data Quantitative variables for PCA
#' @param var_index Index of categorical variable to overlay
#' @param cos2_threshold Minimum cos2 for individual display
#' @return ggplot object
add_categorical_overlay <- function(data, quanti_data, var_index, cos2_threshold = 0.5) {
  # Prepare data with categorical variable
  data_with_cat <- data.frame(quanti_data, 
                              category = as.factor(data[[var_index]]))
  
  # Run PCA with categorical as supplementary
  pca_result <- PCA(data_with_cat, 
                    ncp = 3, 
                    graph = FALSE, 
                    quali.sup = ncol(data_with_cat))
  
  # Create base plot with individuals colored by cos2
  p <- fviz_pca_ind(pca_result,
                    gradient.cols = c("lightblue", "darkblue"),
                    col.ind = "cos2",
                    label = "",
                    title = colnames(data)[var_index],
                    select.ind = list(cos2 = cos2_threshold)) +   
    theme(axis.title = element_text(size = 6),
          axis.text = element_text(size = 6),
          text = element_text(size = 6),
          plot.title = element_text(hjust = 0.5))
  
  # Extract coordinates of categorical levels
  coords_quali <- as.data.frame(pca_result$quali.sup$coord[, 1:2])
  coords_quali$labels <- rownames(coords_quali)
  
  # Add categorical centroids to plot
  p <- p + 
    geom_point(data = coords_quali, 
               aes(x = Dim.1, y = Dim.2), 
               color = "magenta", 
               size = 4, 
               shape = 22,
               inherit.aes = FALSE) +
    geom_text(data = coords_quali, 
              aes(x = Dim.1, y = Dim.2, label = labels), 
              color = "magenta", 
              vjust = -1, 
              size = 2.5,
              inherit.aes = FALSE)
  
  return(p)
}

# ============================================================================
# CLUSTERING FUNCTIONS
# ============================================================================

#' Calculate BSS/TSS ratio for hierarchical clustering
#'
#' @param hclust_result Result from hclust function
#' @param n_clusters Number of clusters
#' @return BSS/TSS ratio as percentage
calculate_bss_tss <- function(hclust_result, n_clusters) {
  TSS <- sum(hclust_result$height)
  BSS <- sum(tail(hclust_result$height, n = (n_clusters - 1)))
  
  ratio <- (BSS / TSS) * 100
  return(round(ratio, 2))
}

#' Plot height for cluster selection
#'
#' @param hclust_result Result from hclust function
#' @param n_display Number of merges to display
#' @param optimal_k Optimal number of clusters to highlight
#' @return Barplot of aggregation levels
plot_cluster_heights <- function(hclust_result, n_display = 11, optimal_k = NULL) {
  heights <- hclust_result$height
  start_idx <- length(heights) - n_display + 1
  subset_heights <- heights[start_idx:length(heights)]
  subset_labels <- seq(n_display, 1, -1)
  
  df <- data.frame(
    n_clusters = factor(subset_labels, levels = rev(subset_labels)),
    height = subset_heights
  )
  
  p <- ggplot(df, aes(x = n_clusters, y = height)) +
    geom_bar(stat = "identity", fill = "lightgray", color = "white", linewidth = 0.3) +
    labs(
      title = "Hierarchical Clustering Heights",
      x = "Number of Clusters",
      y = "Aggregation Level"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    scale_x_discrete(limits = rev(levels(df$n_clusters)))
  
  if (!is.null(optimal_k)) {
    p <- p + geom_vline(xintercept = 12 - optimal_k, 
                        linetype = "dashed", color = "#E74C3C", linewidth = 0.8)
  }
  
  return(p)
}

# ============================================================================
# MCA FUNCTIONS
# ============================================================================

#' Create blood pressure profile categories
#'
#' @param data Data frame with SBP and DBP variables
#' @return Character vector of BP categories
create_bp_profile <- function(data) {
  bp_profile <- with(data, 
                     ifelse(SBP < 90 & DBP < 60, "Hypotension",
                            ifelse(SBP > 140 | DBP > 80, "Hypertension", 
                                   "Normal")))
  return(bp_profile)
}

#' Categorize sleep duration
#'
#' @param sleep_duration Numeric vector of sleep durations
#' @return Factor with sleep categories
categorize_sleep_duration <- function(sleep_duration) {
  sleep_cat <- cut(sleep_duration,
                   breaks = c(-Inf, 6.5, 7.5, Inf),
                   labels = c("Below 6.5", "6.5 to 7.5", "Above 7.5"),
                   right = TRUE)
  return(sleep_cat)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Generate descriptive statistics table
#'
#' @param data Data frame with quantitative variables
#' @return Data frame with Min, Max, Mean, SD
generate_summary_stats <- function(data) {
  stats_df <- data.frame(
    Min = round(sapply(data, min, na.rm = TRUE), 2),
    Max = round(sapply(data, max, na.rm = TRUE), 2),
    Mean = round(sapply(data, mean, na.rm = TRUE), 2),
    SD = round(sapply(data, sd, na.rm = TRUE), 2)
  )
  return(stats_df)
}

#' Save plot to figures directory
#'
#' @param plot_obj ggplot or base R plot
#' @param filename Name of file (without extension)
#' @param width Width in inches
#' @param height Height in inches
save_figure <- function(plot_obj, filename, width = 8, height = 6) {
  if (!dir.exists("../figures")) {
    dir.create("../figures", recursive = TRUE)
  }
  
  filepath <- file.path("../figures", paste0(filename, ".png"))
  
  if (inherits(plot_obj, "ggplot")) {
    ggsave(filepath, plot_obj, width = width, height = height, dpi = 300)
  } else {
    png(filepath, width = width, height = height, units = "in", res = 300)
    print(plot_obj)
    dev.off()
  }
  
  message(paste("Figure saved:", filepath))
}
