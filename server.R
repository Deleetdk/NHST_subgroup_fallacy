
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


# functions ---------------------------------------------------------------

t_df = function(x) {
  # browser()
  x_rownames = rownames(x)
  x_colnames = colnames(x)
  x2 = x %>% t()
  rownames(x2) = x_colnames
  colnames(x2) = x_rownames
  x2
}

shinyServer(function(input, output, session) {
  
  # simualte samples --------------------------------------------------------
  
  reac_l_samples = reactive({
    #go button
    input$update
    
    #generate data, but only when button is pressed
    isolate({
      
      #validate that replications > 0
      validate(need(input$replications > 0, "You must use at least one replication!"))
      
      #seed
      set.seed(input$seed) #reproducible numbers
      
      #generate
      withProgress({
        l_samples = lapply(1:input$replications, FUN = function(i) {
          
          #progress bar update, every 10th
          incProgress(1/i, detail = paste("Simulation ", i))
          
          #random sample
          tmp = data.frame(gender = sample(c(rep("M", input$cell_size), rep("F", input$cell_size))),
                           father = factor(sample(c(rep("Good", input$cell_size), rep("Bad", input$cell_size))), levels = c("Good", "Bad")),
                           mother = factor(sample(c(rep("Good", input$cell_size), rep("Bad", input$cell_size))), levels = c("Good", "Bad"))
          )
          

          #vectorized probability calculation
          #make logical df first
          tmp_lgl = data.frame("bad_F" = tmp$father == "Bad",
                               "bad_M" = tmp$mother == "Bad",
                               "gender_M" = tmp$gender == "M",
                               "FS" = tmp$gender == "M" & tmp$father == "Bad",
                               "MD" = tmp$gender == "F" & tmp$mother == "Bad"
                               ) %>% as.matrix()
          
          #multiply row-wise with probabilities
          tmp_num = tmp_lgl %>%
            t() %>% multiply_by(c(
                              input$father_effect,
                              input$mother_effect,
                              input$gender_effect,
                              input$FS_interaction,
                              input$MD_interaction
          )) %>% t() %>%
            as.data.frame()
          tmp_num$baserate = input$baserate_outcome
          
          #sum rows to get aggregate prob
          v_probs = rowSums(tmp_num)
          
          #check for validity
          validate(need(all(v_probs < 1), "Probability of outcome must be less than 1. Adjust your parameters."))
          validate(need(all(v_probs > 0), "Probability of outcome must be greater than 0. Adjust your parameters."))
          
          #generate outcomes
          #make a semi-prefilled function
          sampler = function(prob) {
            sample(x = c("Good", "Bad"), size = 1, prob = c(1-prob, prob))
          }
          #generate outcomes
          tmp$outcome = sapply(X = v_probs, FUN = sampler) %>% factor(levels = c("Good", "Bad"))
          
          # browser()
          
          #outcome, old row-based version
          # tmp$outcome = mapply(father = tmp$father, mother = tmp$mother, gender = tmp$gender, FUN = function(father, mother, gender) {
          #   
          #   #sum parental influence
          #   parent_sum = sum(c(father, mother)) - 2 #subtract 2 because we want 0-2 values
          #   
          #   #calculate probability
          #   v_prop = input$baserate_outcome #begin with base rate
          #   
          #   #add gender effect
          #   if (gender == "M") {
          #     v_prop = v_prop + input$gender_effect * .5  #centering
          #   } else {
          #     v_prop = v_prop + input$gender_effect * -.5
          #   }
          #   
          #   #add parental effects
          #   v_prop = v_prop + 
          #     input$father_effect * (father == "Bad") + #add effect if father is bad
          #     input$mother_effect * (mother == "Bad")   #add effect if mother is bad
          #   
          #   #add interaction
          #   if (gender == "M" & father == "Bad") {
          #     v_prop = v_prop + input$FS_interaction * .5 #multiply by half for centering
          #   } else {
          #     v_prop = v_prop + input$FS_interaction * -.5
          #   }
          #   if (gender == "F" & mother == "Bad") {
          #     v_prop = v_prop + input$MD_interaction * .5
          #   } else {
          #     v_prop = v_prop + input$MD_interaction * -.5
          #   }
          #   
          #   #check if probability is impossible
          #   validate(need(v_prop < 1, "Probability of outcome must be less than 1. Adjust your parameters."))
          #   validate(need(v_prop > 0, "Probability of outcome must be greater than 0. Adjust your parameters."))
          #   
          #   #outcome
          #   base::sample(c("Good", "Bad"), size = 1, prob = c(1-v_prop, v_prop))
          #   
          # }) %>% factor(levels = c("Good", "Bad"))
          
          tmp
        })
      }, message = "Simulating samples", value = 0)

    })
    
    l_samples
  })  


  # run models --------------------------------------------------------------
  
  reac_l_models = reactive({
    
    #progress
    withProgress({

      l_models = purrr::pmap(list(d = reac_l_samples(), i = 1:length(reac_l_samples())), .f = function(d, i) {
        
        #progress bar update
        incProgress(1/i, detail = paste("Model ", i))
        
        #fit models
        fit_male = glm("outcome ~ father + mother", data = d, subset = gender == "M", family = binomial)
        fit_female = glm("outcome ~ father + mother", data = d, subset = gender == "F", family = binomial)
        fit_full = glm("outcome ~ father + mother + gender + gender*father + gender*mother", data = d, family = binomial)
        
        #return in named list
        list("male" = fit_male,
             "female" = fit_female,
             "full" = fit_full)
      })
      
      l_models
    }, message = "Fitting models", value = 0)
    
    l_models
  })
  
  # calculate p<alpha status -------------------------------------------------------
  
  
  
  # calculate p<alpha status -------------------------------------------------------
  
  reac_d_subgroup_p_values_below_alpha = reactive({
    
    
    #extract p values from models
    d_subgroup_p_values = lapply(reac_l_models(), FUN = function(ms) {
      
      #extract p values
      c("male_father_p" = ms$male %>% summary() %>% extract2("coefficients") %>% extract(2, 4),
        "male_mother_p" = ms$male %>% summary() %>% extract2("coefficients") %>% extract(3, 4),
        "female_father_p" = ms$female %>% summary() %>% extract2("coefficients") %>% extract(2, 4),
        "female_mother_p" = ms$female %>% summary() %>% extract2("coefficients") %>% extract(3, 4),
        "full_father_p" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(2, 4),
        "full_mother_p" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(3, 4),
        "full_gender_p" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(4, 4),
        "full_gender_father_interaction" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(5, 4),
        "full_gender_mother_interaction" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(6, 4)
      )
    }) %>% as.data.frame() %>% t_df() %>% set_rownames(NULL) %>% as_data_frame()
    
    #get their PUA status
    d_subgroup_p_values_below_alpha = (d_subgroup_p_values < as.numeric(input$alpha)) %>% as.data.frame()
    
    d_subgroup_p_values_below_alpha
  })
  
  output$PUA_barplot = renderPlot({

    #actual vs. fallacy interactions
    v_PUAs = sapply(reac_d_subgroup_p_values_below_alpha(), mean)
    d = data.frame(name = names(v_PUAs), PUA = v_PUAs)
    
    #add human readable names
    v_better_names = c("Father",
                       "Mother",
                       "Father",
                       "Mother",
                       "Father",
                       "Mother",
                       "Gender",
                       "Gender x father (interaction)",
                       "Gender x mother (interaction)")
    
    #re-level predictors
    d$name = factor(d$name, levels = rev(d$name))
    
    #model colors (fill)
    v_models = c(rep("Male subgroup model", 2),
                 rep("Female subgroup model", 2),
                 rep("Full model", 5))
    v_models_uniq = c("Male subgroup model", "Female subgroup model", "Full model")
    d$model = factor(v_models, levels = rev(v_models_uniq))

    # browser()
    #plot
    ggplot(d, aes(name, PUA, fill = model, group = NULL)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_discrete(labels = rev(v_better_names)) +
      scale_fill_discrete(name = "Model", breaks = v_models_uniq) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .05)) +
      geom_hline(yintercept = as.numeric(input$alpha), linetype = "dotted") +
      xlab("Predictor") + ylab("Proportion of p-values of predictor that are PUA (p under alpha)") +
      coord_flip()
  })
  
  output$NHST_subgroup_fallacy = renderPlot({
    #actual vs. fallacy interactions
    d = reac_d_subgroup_p_values_below_alpha()

    #NHST fallacies
    d$PUA_diff_father = (d$male_father_p + d$female_father_p) == 1
    d$PUA_diff_mother = (d$male_mother_p + d$female_mother_p) == 1
    
    #proportion of studies that find 'interaction' effect
    d_sum = data.frame("diff_PUA" = c(mean(d$PUA_diff_father), mean(d$PUA_diff_mother)),
                       "parent" = c("Father", "Mother"))
    
    #plot
    ggplot(d_sum, aes(parent, diff_PUA, fill = parent, group = NULL)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 1)) +
      scale_fill_manual(name = "Parent", values=c("blue", "red")) +
      xlab("Parent") + ylab("Proportion of studies finding a discrepancy in PUA for males/females")
    
  })
  
  
  output$effect_sizes = renderPlot({

    #models
    l_models = reac_l_models()
    
    #extract effect sizes from the models
    d_effect_sizes = lapply(l_models, FUN = function(ms) {
      
      #extract p values
      c("male_father" = ms$male %>% summary() %>% extract2("coefficients") %>% extract(2, 1),
        "male_mother" = ms$male %>% summary() %>% extract2("coefficients") %>% extract(3, 1),
        "female_father" = ms$female %>% summary() %>% extract2("coefficients") %>% extract(2, 1),
        "female_mother" = ms$female %>% summary() %>% extract2("coefficients") %>% extract(3, 1),
        "full_father" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(2, 1),
        "full_mother" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(3, 1),
        "full_gender" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(4, 1),
        "full_gender_father_interaction" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(5, 1),
        "full_gender_mother_interaction" = ms$full %>% summary() %>% extract2("coefficients") %>% extract(6, 1)
      )
    }) %>% as.data.frame() %>% t_df() %>% set_rownames(NULL) %>% as.data.frame()

    
    #long form
    d_effect_sizes_long = reshape::melt.data.frame(d_effect_sizes)
    
    #add model info
    d_effect_sizes_long$model = c(rep("Male subgroup model", 2 * input$replications),
                                  rep("Female subgroup model", 2 * input$replications),
                                  rep("Full model", 5 *  input$replications)) %>% factor(levels = c("Male subgroup model", "Female subgroup model", "Full model"))
    
    #add human readable names
    v_better_names = c("Father",
                       "Mother",
                       "Father",
                       "Mother",
                       "Father",
                       "Mother",
                       "Gender",
                       "Gender x father (interaction)",
                       "Gender x mother (interaction)")


    #plot
    ggplot(d_effect_sizes_long, aes(variable, value, color = model, group = NULL)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dotted") +
      scale_x_discrete(labels = v_better_names) +
      scale_y_continuous(breaks = -100:100) +
      theme(axis.text.x = element_text(angle = -20, hjust = 0)) +
      xlab("Predictor") + ylab("Logits (log odds)")
  })
  
  ### UPDATE PARAMETERS BASED ON PRESET SELECTOR
  observe({
    #get preset
    preset = input$preset
    
    #main effects
    if (preset == "main_only") {

      updateSliderInput(session,
                        "baserate_outcome",
                        value = .20)
      updateSliderInput(session,
                        "gender_effect",
                        value = 0)
      updateSliderInput(session,
                        "father_effect",
                        value = .20)
      updateSliderInput(session,
                        "mother_effect",
                        value = .20)
      updateSliderInput(session,
                        "FS_interaction",
                        value = 0)
      updateSliderInput(session,
                        "MD_interaction",
                        value = 0)
    }
    
    #interaction effects
    if (preset == "interactions_only") {

      updateSliderInput(session,
                        "baserate_outcome",
                        value = .20)
      updateSliderInput(session,
                        "gender_effect",
                        value = 0)
      updateSliderInput(session,
                        "father_effect",
                        value = 0)
      updateSliderInput(session,
                        "mother_effect",
                        value = 0)
      updateSliderInput(session,
                        "FS_interaction",
                        value = .20)
      updateSliderInput(session,
                        "MD_interaction",
                        value = .20)
    }
    
  })

})
