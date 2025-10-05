# Custom Functions for Sleep Health Analysis
# Author: Louis-David PIRON
# Date: 2024


# ============================================================================
# DATA LOADING FUNCTION
# ============================================================================

load_sleep_data <- function(filename = "sleep_health_data.csv") {
  
  data <- read.csv(file.path("..", "data", filename), 
                   sep = ";", 
                   dec = ".", 
                   row.names = 1
  )
  
  return(data)
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

plot_histogram <- function(data, var_index) {
  df <- data.frame(value = data[, var_index])
  
  ggplot(df, aes(x = value)) +
    geom_histogram(bins = 30, fill = clean_colors$primary) +
    labs(
      title = paste("Distribution of", colnames(data)[var_index]),
      x = colnames(data)[var_index],
      y = "Frequency"
    ) +
    theme_clean()
}


plot_categorical <- function(data, var_index, las = 0) {
  freq_table <- as.data.frame(table(data[var_index]))
  colnames(freq_table) <- c("category", "count")
  
  p <- ggplot(freq_table, aes(x = category, y = count)) +
    geom_col(fill = clean_colors$primary) +
    labs(
      title = colnames(data)[var_index],
      x = NULL,
      y = "Frequency"
    ) +
    theme_clean() +
    theme(panel.grid.major.x = element_blank())
  
  if (las == 2) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  
  return(p)
}

quality_representation_var <- function(pca_result, n_components = 2, threshold = 0.5) {
  cos2_sum <- round(sort(rowSums(pca_result$var$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    variable = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = variable, y = cos2, fill = quality)) +
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("p =", n_components),
      x = NULL,
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return(cos2_sum)
}

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
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("p =", n_components),
      subtitle = paste(n_poor, "out of", n_total, "individuals poorly represented"),
      x = "Individual Index",
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return(n_poor)
}

quality_representation_categorical <- function(data, quanti_indices, quali_indices, 
                                               n_components = 2, threshold = 0.5) {
  pca_result <- PCA(data, ncp = 3, graph = FALSE, quali.sup = quali_indices)
  cos2_sum <- round(sort(rowSums(pca_result$quali.sup$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    category = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = category, y = cos2, fill = quality)) +
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.8) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("p =", n_components),
      x = NULL,
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return(cos2_sum)
}

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
    geom_col(fill = clean_colors$primary) +
    labs(
      x = "Number of Clusters",
      y = "Aggregation Level"
    ) +
    theme_clean() +
    theme(panel.grid.major.x = element_blank()) +
    scale_x_discrete(limits = rev(levels(df$n_clusters)))
  
  if (!is.null(optimal_k)) {
    p <- p + geom_vline(xintercept = 12-optimal_k, linetype = "dashed", 
                        color = clean_colors$secondary, linewidth = 0.5)
  }
  
  return(p)
}

## ============================================================================
# PCA QUALITY ASSESSMENT
## ============================================================================

quality_representation_var <- function(pca_result, n_components = 2, threshold = 0.5) {
  cos2_sum <- round(sort(rowSums(pca_result$var$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    variable = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = variable, y = cos2, fill = quality)) +
    geom_col(width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.5) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("Variable Quality (p =", n_components, ")"),
      x = NULL,
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return(cos2_sum)
}

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
    geom_col(width = 1, alpha = 0.85) +
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.5) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("Individual Quality (p =", n_components, ")"),
      subtitle = paste(n_poor, "out of", n_total, "individuals poorly represented"),
      x = "Individual Index",
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return(n_poor)
}

quality_representation_categorical <- function(data, quanti_indices, quali_indices, 
                                               n_components = 2, threshold = 0.5) {
  pca_result <- PCA(data, ncp = 3, graph = FALSE, quali.sup = quali_indices)
  cos2_sum <- round(sort(rowSums(pca_result$quali.sup$cos2[, 1:n_components])), digits = 3)
  
  df <- data.frame(
    category = factor(names(cos2_sum), levels = names(cos2_sum)),
    cos2 = cos2_sum,
    quality = ifelse(cos2_sum < threshold, "Poor", "Good")
  )
  
  p <- ggplot(df, aes(x = category, y = cos2, fill = quality)) +
    geom_col(width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = threshold, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.5) +
    scale_fill_manual(values = c("Poor" = clean_colors$secondary, 
                                 "Good" = clean_colors$primary)) +
    labs(
      title = paste("Categorical Levels Quality (p =", n_components, ")"),
      x = NULL,
      y = expression(cos^2)
    ) +
    theme_clean() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  print(p)
  return()
}

# ============================================================================
# PCA VISUALIZATION WITH CATEGORICAL OVERLAY
# ============================================================================

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
# PCA SCREE PLOT
# ============================================================================

plot_scree <- function(pca_result, n_components = NULL) {
  if (is.null(n_components)) {
    n_components <- nrow(pca_result$eig)
  }
  
  df <- data.frame(
    component = 1:n_components,
    eigenvalue = pca_result$eig[1:n_components, "eigenvalue"],
    cumulative = pca_result$eig[1:n_components, "cumulative percentage of variance"]
  )
  
  p <- ggplot(df, aes(x = component)) +
    geom_col(aes(y = eigenvalue), fill = clean_colors$light, 
             width = 0.7, alpha = 0.85) +
    geom_hline(yintercept = 1, linetype = "dashed", 
               color = clean_colors$secondary, linewidth = 0.6) +
    geom_line(aes(y = cumulative / 100 * max(eigenvalue)), 
              color = clean_colors$dark, linewidth = 0.8) +
    geom_point(aes(y = cumulative / 100 * max(eigenvalue)), 
               color = clean_colors$dark, size = 2.5) +
    scale_y_continuous(
      name = expression(lambda[alpha]),
      sec.axis = sec_axis(~ . / max(df$eigenvalue) * 100, 
                          name = "Cumulative % of variance")
    ) +
    scale_x_continuous(breaks = 1:n_components) +
    labs(
      title = "Eigenvalues and Cumulative Variance",
      x = expression(alpha)
    ) +
    theme_clean() +
    theme(
      axis.title.y.right = element_text(color = clean_colors$dark),
      axis.text.y.right = element_text(color = clean_colors$dark),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

# ============================================================================
# CLUSTERING FUNCTIONS
# ============================================================================

calculate_bss_tss <- function(hclust_result, n_clusters) {
  TSS <- sum(hclust_result$height)
  BSS <- sum(tail(hclust_result$height, n = (n_clusters - 1)))
  
  ratio <- (BSS / TSS) * 100
  return(round(ratio, 2))
}

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
    geom_col(fill = clean_colors$primary, width = 0.7, alpha = 0.85) +
    labs(
      title = "Hierarchical Clustering Heights",
      x = "Number of Clusters",
      y = "Aggregation Level"
    ) +
    theme_clean() +
    theme(panel.grid.major.x = element_blank()) +
    scale_x_discrete(limits = rev(levels(df$n_clusters)))
  
  if (!is.null(optimal_k)) {
    p <- p + geom_vline(xintercept = optimal_k, linetype = "dashed", 
                        color = clean_colors$secondary, linewidth = 0.5)
  }
  
  return(p)
}

# ============================================================================
# MCA FUNCTIONS
# ============================================================================

create_bp_profile <- function(data) {
  bp_profile <- with(data, 
                     ifelse(SBP < 90 & DBP < 60, "Hypotension",
                            ifelse(SBP > 140 | DBP > 80, "Hypertension", 
                                   "Normal")))
  return(bp_profile)
}

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


generate_summary_stats <- function(data) {
  stats_df <- data.frame(
    Min = round(sapply(data, min, na.rm = TRUE), 2),
    Max = round(sapply(data, max, na.rm = TRUE), 2),
    Mean = round(sapply(data, mean, na.rm = TRUE), 2),
    SD = round(sapply(data, sd, na.rm = TRUE), 2)
  )
  return(stats_df)
}

# ============================================================================
# CUSTOM THEME
# ============================================================================

#' Clean minimal theme matching factoextra style
theme_clean <- function() {
  theme_minimal(base_size = 10) +
    theme(
      # Fond
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Titres
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, 
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = 9, hjust = 0.5, 
                                   margin = margin(b = 8), color = "grey40"),
      
      # Axes
      axis.title = element_text(size = 9, color = "grey20"),
      axis.text = element_text(size = 8, color = "grey30"),
      axis.line = element_line(color = "grey80", linewidth = 0.3),
      axis.ticks = element_line(color = "grey80", linewidth = 0.3),
      
      # Grille très subtile
      panel.grid.major = element_line(color = "grey92", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      
      # Pas de bordure
      panel.border = element_blank(),
      
      # Légende
      legend.position = "right",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.background = element_blank(),
      legend.key = element_blank(),
      
      # Marges réduites
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Palette simple et élégante
clean_colors <- list(
  primary = "#4A5899",      # Bleu doux (proche de factoextra)
  secondary = "#E8384F",    # Rouge vif
  light = "#B8C5D6",        # Bleu clair
  dark = "#2C3E50"          # Bleu très foncé
)
