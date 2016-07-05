
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("The NHST subgroup fallacy"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      numericInput("cell_size",
                  "Sample size",
                  min = 5,
                  max = 1000,
                  value = 80,
                  step = 5),
      helpText("The sample size is split evenly between the genders."),
      numericInput("replications",
                  "Replications",
                  min = 0,
                  max = 10000,
                  value = 100,
                  step = 25),
      selectInput("preset",
                  "Preset configurations",
                  choices = c("Main effects only (default)" = "main_only",
                              "Interactions only" = "interactions_only")),
      sliderInput("baserate_outcome",
                  "Base rate of outcome",
                  min = .00,
                  max = 1,
                  value = .20,
                  step = .01),
      sliderInput("gender_effect",
                  "Gender effect (male)",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .01),
      sliderInput("father_effect",
                  "Father effect",
                  min = 0,
                  max = 1,
                  value = .20,
                  step = .01),
      sliderInput("mother_effect",
                  "Mother effect",
                  min = 0,
                  max = 1,
                  value = .20,
                  step = .01),
      sliderInput("FS_interaction",
                  "Father-son interaction effect",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .01),
      sliderInput("MD_interaction",
                  "Mother-daughter interaction effect",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = .01),
      actionButton("update",
                   label = "Simulate!"),
      helpText("Advanced options"),
      numericInput("seed",
                   "Seed for random number generator",
                   min = 1,
                   max = 99999,
                   value = 1,
                   step = 1),
      selectInput("alpha",
                  "Alpha-level",
                  choices = c(.10, .05, .01, .001),
                  selected = .05)
      
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      HTML("<p>There are several common fallacies (errors of reasoning) that are related to the use of NHST (null hypothesis significance testing) statistics. One fallacy, <i>the NHST subgroup fallacy</i>, involves analyzing the association between the predictor and the outcome within different subgroups. If in one subgroup the association is found to have a PUA (p under alpha) but not the other, it is concluded that the association is only present for one subgroup. In fact, one cannot approach it like this. One must test for an interaction effect directly. This is illustrated by simulating a large number of studies. Inspirered by <a href='http://slatestarcodex.com/2016/06/27/i-wrote-a-blog-post-but-i-did-not-adjust-for-the-fact-that-the-title-would-be-too-l/'>this real paper</a> making the error, we look at the effects of relationships to one's parents on some kind of bad outcome. For each study, we divide the sample into two by gender and look at the effects of the two parental predictors for each.</p>
            <p>The default parameters are designed to reflect typical studies of this sort. After examining the results of these, try changing them to see how this affects the results.</p>"),
      tabsetPanel(
        tabPanel("NHST statistics",
          HTML("<p>In the plot below the proportion of simulated studies that found a PUA is displayed. Similar to many studies, models are fit by gender. We also include a full model that includes an interaction term, which is the proper way of establishing that a predictor works differently for each gender. Note that the full model includes a gender term. This is because including higher-order terms without their lower-order components can lead to <a href='http://stats.stackexchange.com/questions/11009/including-the-interaction-but-not-the-main-effects-in-a-model'>odd or misleading results</a>.</p>"),
          plotOutput("PUA_barplot"),
          HTML("<p>The plot below shows the proportion of models where one predictor is PUA for one gender but not the other. This is directly related to the power of the design. When power is around 50%, such discrepancies in PUA happen about 50% of the time. If there is no actual intereaction effect (default settings), then this is a false positive rate of about 50%.</p>"),
          plotOutput("NHST_subgroup_fallacy")
        ),
        tabPanel("Effect sizes",
                 HTML("<p>The plot shows the distribution of effect sizes estimated by the models. Note that the models estimate effect sizes in logits which is a logistic transformation of the odds ratio. For this reason, the numbers in the left menu bar do not correspond to the numbers in the plot, but they relative comparisons are possible.</p>"),
                 plotOutput("effect_sizes"),
                 HTML("<p>Note that gender is coded with male as the reference class. This affects the direction of the effect for the interaction effects.</p>")),
        tabPanel("Details",
                 HTML("<p>This tab contains details about how the simulation works and where one can learn more.</p>
                       <h3>Simulation details</h3>
                       <p>The simulator generates a sample for each replication. To generate a sample, a random sample of 'male' and 'female' strings is generated with 50% of each gender. These are then assigned either a good or bad relationship to each of their parents at random, each with 50% probability. After this, each person is assigned a probability of them having the bad outcome. This probability is the sum of each cause that is true for them plus the base rate. In the default scenario, the base rate is 20% and each bad parental relationship adds another 20%. This gives a range of 20-60% chance. If gender effects or interactions are present, these are added as well. Before assigned an outcome to each person, a validity check is then done to see if any probability value is below 0% or above 100%. If so, an error is returned. If not, an outcome is generated for each person with the probability of the bad outcome being that which was calculated before.</p>
                       <p>After generating the samples, three logistic models (using <i>glm</i>) are fitted for each sample. One for only the males, one for only the females and one for both together. The first two models only contain the parental predictors, while the last model contains a gender predictor as well as two interaction terms.</p>
                       <p>Information is then extracted from the models and displayed in the plots.</p>
                       <p>The source code for this simulator can be found on <a href='https://github.com/Deleetdk/NHST_subgroup_fallacy'>Github</a>.</p>
                       <h3>Further reading</h3>
                       <p>Errors in reasoning related to the use of NHST are well known. In fact, many authors have been calling for the abolision of NHST statistics entirely advocating either confidence intervals, meta-analysis, Bayesian statistics or some combination of these.</p>
                       <ul>
                        <li>Cumming, Geoff. (2011). <a href='http://gen.lib.rus.ec/book/index.php?md5=2D78D182BB9623A201AFF6B19568FC89'>Understanding The New Statistics: Effect Sizes, Confidence Intervals, and Meta-Analysis</a>.</li>
                        <li>Schmidt, F. L. (1996). <a href='http://www.phil.vt.edu/dmayo/personal_website/Schmidt_StatSigTesting.pdf'>Statistical significance testing and cumulative knowledge in psychology: Implications for training of researchers</a>. Psychological methods, 1(2), 115.</li>
                        <li>Schmidt, Frank L. and Hunter, John E. (2004). <a href='http://gen.lib.rus.ec/book/index.php?md5=D43B07F375C98403D672D186F91C3407'>Methods of meta-analysis: Correcting error and bias in research findings</a>.</li>
                        <li>Nickerson, R. S. (2000). <a href='http://psych.colorado.edu/~willcutt/pdfs/Nickerson_2000.pdf'>Null hypothesis significance testing: a review of an old and continuing controversy</a>. Psychological methods, 5(2), 241.</li>
                        <li>Gelman, Andrew and Stern, Hal. (2006). <a href='http://www.stat.columbia.edu/~gelman/research/published/signif4.pdf'>The Difference Between 'Significant' and 'Not Significant' is not Itself Statistically Significant</a></li>
                       </ul>
                      "))
      )
      
    )
  )
))
