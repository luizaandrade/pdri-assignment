# Standardize ATE for subpopulations ----------------------------------------
reg_subpop <-
  function(x, variable, value) {
    
    data <-
      x %>%
      filter(get(variable) == value)
    
    sd <-
      x %>%
      summarise(sd = sd(re_arrest)) %>%
      unlist
    
    reg <- 
      felm(
        re_arrest ~ treat + age + prior_arrests | 0 | 0 | person_id,
        data
      )
    
    results <-
      bind_cols(confint(reg), reg$coefficients)[2,]
    
    results <-
      results / sd
    
    names(results) <- c("ll", "ul", "beta")
    
    return(results)
  }

# Plot standardized ATE --------------------------------------------------------
plot <-
  function(data, variable, values) {
    
    coefs <- 
      lapply(
        values, 
        function(x) reg_subpop(data, variable, x)
      ) %>%
      bind_rows()
    
    names <-
      data %>%
      group_by(get(variable)) %>%
      summarise(count = n()) %>%
      mutate(
        label = paste0(
          values,
          "\n(N = ",
          count,
          ")"
        ) 
      ) %>%
      ungroup %>%
      select(label) %>%
      unlist
    
    coefs <-
      coefs %>%
      mutate(group = names)
    
    label <- 
      data %>%
      select(variable) %>%
      get_label %>%
      str_to_lower()
    
    graph <-
      coefs %>%
      ggplot(aes(y = group)) +
      geom_segment(
        aes(
          yend = group,
          x = ll,
          xend = ul
        ), 
        color = "orange",
        size = 1,
        alpha = .5
      ) +
      geom_point(
        aes(x = beta),
        color = "orange",
        size = 3
      ) +
      geom_text(
        aes(
          x = beta,
          label = round(beta, 3),
          vjust = -1
        )
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed", 
        color = "red"
      ) +
      bbc_style() +
      labs(
        title = paste(
          "Treatment effect by",
          label
        ),
        subtitle = "Measured in standard deviations"
      )
    
    finalise_plot(
      plot_name = graph,
      source_name = "Author's estimates based on data provided by CCSAO.",
      save_filepath = here(
        "output",
        paste0("effect_by_", variable, ".png")
      ),
      width_pixels = 640,
      height_pixels = 550
    )
    
  }