#' A function that runs a simulation of an
#' Evidence-Based Random Walk memory model
#'
#' @param seed numeric. A single integer to generate consistent random numbers.
#' @param n_letters an integer. The number of letters used in the model.
#' @param n_trials an integer. The number of iterations the model runs.
#' @param updated_probs_sd an integer. The standard deviation of random samples
#' drawn to update probabilities.
#' @param max_forgetting a single integer, the probability of forgetting any one
#' item after you've learned it.
#' @param forgetting_slope a single integer, the rate you need to see an item to
#' decrease the probability it will be forgotten.
#' @param initial_exposure_matrix optional. A matrix of initial letter exposures
#' to be used to start the model running. if NULL an initial exposure matrix
#' will be generated from scratch.
#'
#' @return a matrix of learned memory strengths between letters, a matrix of the
#' randomly generated probabilities used when drawing random samples of letters,
#' and three graphs of the strengths between letters in memory the model's
#' memory representation.
#' @export
#' @importFrom stats rnorm
#' @importFrom reshape2 melt
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom ggplot2 aes facet_wrap geom_line geom_point geom_text ggplot
#' guides guide_legend scale_x_continuous theme theme_minimal
#' @importFrom kableExtra kable
#'
#' @examples
#' memory_model(seed = 1234, n_letters = 12, n_trials = 200)

memory_model <- function(seed = 20240709,
                         n_letters = 12,
                         n_trials = 500,
                         updated_probs_sd = .1,
                         max_forgetting = .6,
                         forgetting_slope = 3,
                         initial_exposure_matrix = NULL) {
  ### UPDATE: Fully implement the initial.exposure.matrix option by using if
  ### statements to either use the input given, or start from scratch

  target <- NULL
  cue <- NULL
  strength <- NULL
  time <- NULL
  mean_strength <- NULL # due to NSE notes in R CMD check

  # Setup and Constants -----------------------------------------------------
  set.seed(seed)
  n_letters <- n_letters # number of letters to be sampled from
  n_trials <- n_trials # total number of trials
  updated_probs_sd <- updated_probs_sd # sd of sampling probabilities
  max_forgetting <- max_forgetting # probability of forgetting an item
  forgetting_slope <- forgetting_slope # rate you need to see an item to
  # decrease the probability it will be forgotten

  # probabilities of each letter occurring
  letter_probs <- matrix(
    data = c(
      .085, .0207, .0454, .0338, .1116, .0181,
      .0247, .03, .0754, .001, .011, .0549
    ),
    nrow = 1,
    dimnames = list(NULL, LETTERS[1:n_letters])
  )

  init_mem_rep <- matrix(
    data = rep(
      x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      each = n_letters
    ),
    ncol = length(letter_probs),
    byrow = TRUE,
    dimnames = list(
      LETTERS[1:n_letters], # row names: the target
      LETTERS[1:n_letters]  # column names : the cue
    )
  )

  diag(init_mem_rep) <- 0 # ensure letters don't have the chance of immediately
  # following themselves

  learned_strengths <- 0 * init_mem_rep # makes a matrix to store the strengths
  # of the learned transition probabilities between each letter.

  if (is.null(initial_exposure_matrix)) {
    init_exposure <- matrix(
      data = sample(
        x = LETTERS[1:n_letters],
        size = n_letters * (n_trials * .05),
        # initial exposure will be 5% of trials
        replace = TRUE,
        prob = prop.table(letter_probs)
      ),
      ncol = n_letters,
      byrow = TRUE
    )
  } else {
    init_exposure <- initial_exposure_matrix
  }

  mem_rep_dev <- array(
    data = 0,
    dim = c(
      n_trials,
      nrow(learned_strengths),
      ncol(learned_strengths)
    ),
    dimnames = list(
      time = c(1:n_trials),
      cue = LETTERS[1:n_letters],
      target = LETTERS[1:n_letters]
    )
  ) # make an array the length of the exposure "trials", each trial will be a
  # 12 x 12, representing memory strength from one letter to the other letters

  full_exposure <- matrix(
    data = NA,
    nrow = n_trials,
    ncol = n_letters,
    dimnames = list(NULL, 1:n_letters)
  )

  # Exposure Stream with Updating Letter Probabilities ----------------------
  # Initialize the matrices that you need
  updated_exposure <- matrix(
    data = NaN,
    nrow = n_trials * .95, # the exposure simulation will be 95% of all trials
    ncol = n_letters,
    dimnames = list(NULL, 1:n_letters)
  ) # a matrix to store the letters drawn based on updating probabilities

  probability_record <- matrix(
    data = 0,
    ncol = n_letters,
    nrow = (n_trials * .95),
    dimnames = list(
      NULL, LETTERS[1:n_letters]
    )
  ) # a matrix to record the probabilities generated when sampling letters

  updated_probs <- matrix(
    data = 0,
    nrow = nrow(updated_exposure),
    ncol = n_letters,
    dimnames = list(NULL, LETTERS[1:n_letters])
  ) # a matrix to store the "temp" letter probabilities for each step of the for
  # loop used later

  updated_probs[1, ] <- letter_probs + rnorm(
    n = length(letter_probs),
    mean = 0,
    sd = updated_probs_sd
  ) # fill the first row with probabilities generated from initial letter probs
  # and random variance from a normal distribution

  updated_probs[updated_probs < 0] <- 0 # and immediately after, set any
  # probabilities that would have been
  # less than zero to 0
  updated_probs[updated_probs > 1] <- 1 # or any probability over 1 to 1

  probability_record[1, ] <- updated_probs[1, ] # store the probabilities used
  # to draw the previous sample
  # into the probability record
  # your keeping

  updated_exposure[1, ] <- sample(
    x = LETTERS[1:n_letters],
    size = n_letters,
    replace = TRUE,
    prob = prop.table(updated_probs[1, ])
  ) # using the first row of updated probs, draw your sample of letters and
  # store it in the first row of the updated exposure matrix


  # UPDATE:
  # You keep track of the probabilities letters are drawn with, but you can also
  # keep track of the literal frequencies (because maybe the probabilities won't
  # actually match the frequencies)


  for (j in 2:nrow(updated_exposure)) {
    updated_probs[j, ] <- updated_probs[j - 1, ] + rnorm(
      n = n_letters,
      mean = 0,
      sd = updated_probs_sd
    ) # create updated probabilities for letters being drawn based on the
    # previous rows' probabilities plus random variance from a normal
    # distribution

    updated_probs[updated_probs < 0] <- 0 # same as row 1, cap probabilities
    # between 0 and 1 for the rest of the
    # exposure stream
    updated_probs[updated_probs > 1] <- 1

    probability_record[j, ] <- updated_probs[j, ] # and make sure to store
    # everything in the
    # probability record

    updated_exposure[j, ] <- sample(
      x = LETTERS[1:n_letters],
      size = n_letters,
      replace = TRUE,
      prob = prop.table(updated_probs[j, ])
    ) # then store the letter samples you draw in the updated exposure matrix
  }

  full_exposure <- rbind(init_exposure, updated_exposure)
  # bind the exposures together into a complete exposure stream

  # Running the Model -------------------------------------------------------
  for (i in 2:nrow(full_exposure)) {
    # Evaluate the following expression for each index (after the first, all the
    # way to the end) of the exposure vector
    if (full_exposure[i] %in% colnames(init_mem_rep)) {
      # but only do it if that index matches a letter in the column names of the
      # initial probability matrix.

      learned_strengths[
        full_exposure[i - 1], # To identify the row to be updated, identify the
        # row of the previous index's letter in the exposure vector
        full_exposure[i]
      ] <- # and to know what column, get the column from the current index of
        # the exposure vector
        learned_strengths[full_exposure[i - 1], full_exposure[i]] + 1 # the
      # value you assign to the row and column gathered from the previous steps
      # will be +1 than the value currently stored in that location in the
      # learned associations matrix. The additional presentation strengthens the
      # memory representation.
    }

    # Memory isn't perfect though, here you implement a forgetting term to
    # account for letter relationships people will forget
    for (j in seq_len(nrow(learned_strengths))) {
      # For each row in the learned strengths matrix

      for (k in seq_len(ncol(learned_strengths))) {
        # and each column in the learned strengths matrix

        if (learned_strengths[j, k] > 0) {
          # as long as the value in each cell in the matrix is greater than 0
          # (because you can't forget what you have not learned)
          p_forget <- max_forgetting *
            exp(-forgetting_slope * learned_strengths[j, k]) # calculate the
          # probability, using the negative exponent function, someone will
          # forget the association they just learned
          if (runif(n = 1) < p_forget) { # if that probability is > 1,
            learned_strengths[j, k] <- learned_strengths[j, k] - 1
            # subtract one from the current value of the learned strength
            # association in that cell of the matrix
          }
        }
      }
    }

    mem_rep_dev[i, , ] <- learned_strengths # store the letter memory strengths
  }

  df_activations <- melt(mem_rep_dev[, , ])
  names(df_activations) <- c("time", "cue", "target", "strength")

  g1 <- ggplot(
    data = df_activations,
    mapping = aes(
      x = target,
      y = cue,
      color = factor(strength)
    )
  ) +
    geom_point() +
    guides(color = guide_legend(nrow = 1))

  g1 <- theme_pcj(
    ggplot_object = g1,
    graph_text =
      list(
        title = "Learned Strengths Between
                                  Cue and Target Letters",
        xlab = "Target",
        ylab = "Cue",
        caption = paste("Revised:", Sys.time())
      )
  )

  df_summary <- df_activations |>
    group_by(time, target) |>
    summarise(mean_strength = mean(strength)) |>
    ungroup()

  g2 <- ggplot(
    data = df_summary,
    mapping =
      aes(
        x = time,
        y = mean_strength,
        color = target,
        group = target,
        alpha = .8
      )
  ) +
    geom_line(
      lwd = 1.2,
      show.legend = FALSE
    ) +
    scale_x_continuous(limits = c(0, n_trials)) +
    geom_text(
      data =
        filter(df_summary, time == max(time)),
      mapping =
        aes(label = paste0(target, ": ", round(mean_strength, 2))),
      hjust = -0.1,
      show.legend = FALSE
    )

  g2 <- theme_pcj(
    ggplot_object = g2, graph_text =
      list(
        title = "Average Learned Target Strength",
        xlab = "Trial",
        ylab = "Average Target Strength",
        caption = paste("Revised:", Sys.time())
      )
  )

  g3 <- ggplot(
    data = df_activations,
    mapping =
      aes(
        x = time,
        y = strength,
        color = target,
        group = target,
        alpha = .8
      )
  ) +
    geom_line(lwd = .8) +
    scale_x_continuous(limits = c(0, n_trials)) +
    facet_wrap(facets = df_activations$cue) +
    guides(alpha = "none", color = guide_legend(nrow = 1))

  g3 <- theme_pcj(
    ggplot_object = g3, graph_text =
      list(
        title = "Cue and Target Strength Over Time",
        xlab = "Trial",
        ylab = "Learned Strength",
        caption = paste("Revised:", Sys.time())
      )
  )

  model_output <- list(
    ct_graph = g1,
    avgt_graph = g2,
    facet_graph = g3,
    strengths = learned_strengths,
    prob_record = kable(probability_record, digits = 3),
    initial_exposure = init_exposure
  )

  return(model_output)
}
