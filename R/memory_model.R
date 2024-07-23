#' Run a memory model simulation
#'
#' @param seed a single integer value used to set the state of randomly generated numbers to keep consistency when random numbers are sampled.
#' @param n.letters a single integer, the number of letters to be used in the memory model.
#' @param n.trials a single integer, the number of iterations the model should be run for.
#' @param updated.probs.sd a single integer, the standard deviation value to be used.
#' @param max.forgetting a single integer, the probability of forgetting any one item after you've learned it.
#' @param forgetting.slope a single integer, the rate you need to see an item to decrease the probability it will be forgotten.
#' @param ... optional. Settings to pass to the theme() function.
#'
#' @return a matrix of learned memory strengths between letters, a matrix of the randomly generated probabilities used when drawing random samples of letters, and three graphs of the strengths between letters in memory the model's memory representation.
#' @export
#' @importFrom stats rnorm
#' @importFrom reshape2 melt
#' @importFrom dplyr filter group_by summarise ungroup
#' @import ggplot2
#' @importFrom kableExtra kable
#'
#' @examples
#' memory_model(500)

memory_model <- function(seed = 20240709, n.letters = 12, n.trials = 500,
                        updated.probs.sd = .1, max.forgetting = .6,
                        forgetting.slope = 3, ...) {

  Target = Cue = Strength = Time = MeanStrength = NULL # due to NSE notes in R CMD check

  # Setup and Constants -----------------------------------------------------
  set.seed(seed)
  n.letters <- n.letters # # number of letters to be sampled from for the exposure vector later
  n.trials <- n.trials # total number of trials
  updated.probs.sd <- updated.probs.sd # standard deviation used when randomly sampling for updated letter probabilities
  max.forgetting <- max.forgetting # probability of forgetting any one item after you've learned it
  forgetting.slope <- forgetting.slope # rate you need to see an item to decrease the probability it will be forgotten


  letter.probs <- matrix(data = c(.085, .0207, .0454, .0338, .1116, .0181,
                                  .0247, .03, .0754, .001, .011, .0549), # probabilities of each letter occurring
                         nrow = 1,
                         dimnames = list(NULL, LETTERS[1:n.letters])
  )

  init.mem.rep <- matrix(data = rep(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                                    each = n.letters),
                         ncol = length(letter.probs),
                         byrow = TRUE,
                         dimnames = list(
                           LETTERS[1:n.letters],
                           LETTERS[1:n.letters]
                         )
  )

  diag(init.mem.rep) <- 0 # ensure letters don't have the chance of immediately following themselves

  learned.strengths <- 0 * init.mem.rep # makes a matrix to store the strengths of the learned transition probabilities between each letter

  init.exposure <- matrix(data = sample(x = LETTERS[1:n.letters],
                                        size = n.letters * (n.trials * .05), # initial exposure will only be 5% of total trials
                                        replace = TRUE,
                                        prob = prop.table(letter.probs)
  ),
  ncol = n.letters,
  byrow = TRUE
  )

  mem.rep.dev <- array(data = 0,
                       dim = c(n.trials, nrow(learned.strengths), ncol(learned.strengths)),
                       dimnames = list(time = c(1:n.trials),
                                       cue = LETTERS[1:n.letters],
                                       target = LETTERS[1:n.letters])
  ) # make an array the length of the total number of exposure "trials", each trial will be a 12 x 12, representing the memory strength from a letter to each of the other letters

  full.exposure <- matrix(data = NA,
                          nrow = n.trials,
                          ncol = n.letters,
                          dimnames = list(NULL, 1:n.letters)
  )

  # Exposure Stream with Updating Letter Probabilities ----------------------
  # Initialize the matrices that you need
  updated.exposure <- matrix(data = NaN,
                             nrow = n.trials * .95,
                             ncol = n.letters,
                             dimnames = list(NULL, 1:n.letters)
  ) # a matrix to store the letters drawn based on updating probabilities

  probability.record <- matrix(data = 0,
                               ncol = n.letters,
                               nrow = (n.trials * .95),
                               dimnames = list(
                                 NULL, LETTERS[1:n.letters])
  ) # a matrix to record the probabilities generated to be used in the sampling of letters

  updated.probs <- matrix(data = 0,
                          nrow = nrow(updated.exposure),
                          ncol = n.letters,
                          dimnames = list(NULL, LETTERS[1:n.letters])
  ) # a matrix to store the "temp" letter probabilities for each step of the for loop later

  updated.probs[1, ] <- letter.probs + rnorm(n = length(letter.probs),
                                             mean = 0,
                                             sd = updated.probs.sd
  ) # fill the first row with probabilities generated from initial letter probs and random variance from a normal distribution

  updated.probs[updated.probs < 0] <- 0 # and immediately after, set any probabilities that would have been less than zero to 0
  updated.probs[updated.probs > 1] <- 1 # or any probability over 1 to 1

  probability.record[1, ] <- updated.probs[1, ] # store the probabilities used to draw the previous sample into the probability record your keeping

  updated.exposure[1, ] <- sample(x = LETTERS[1:n.letters],
                                  size = n.letters,
                                  replace = TRUE,
                                  prob = prop.table(updated.probs[1, ])
  ) # using the first row of updated probs, draw your sample of letters and store it in the first row of the updated exposure matrix


  # You keep track of the probabilities letters are drawn with, but you can also keep track of the literal frequencies (because maybe the probabilities won't actually match the frequencies)


  for (j in 2:nrow(updated.exposure)) {

    updated.probs[j, ] <- updated.probs[j - 1, ] + rnorm(n = n.letters,
                                                         mean = 0,
                                                         sd = updated.probs.sd
    ) # create updated probabilities for letters being drawn based on the previous rows' probabilities plus random variance from a normal distribution

    updated.probs[updated.probs < 0] <- 0 # same as row 1, cap probabilities between 0 and 1 for the rest of the exposure stream
    updated.probs[updated.probs > 1] <- 1

    probability.record[j, ] <- updated.probs[j, ] # and make sure to store everything in the probability record

    updated.exposure[j, ] <- sample(x = LETTERS[1:n.letters],
                                    size = n.letters,
                                    replace = TRUE,
                                    prob = prop.table(updated.probs[j, ])
    ) # then actually store the letter samples you draw in the updated exposure matrix

  }

  full.exposure <- rbind(init.exposure, updated.exposure) # bind the initial exposure and "probability updating" exposure together into a complete exposure stream

  # Running the Model -------------------------------------------------------
  for (i in 2:nrow(full.exposure)) { # Evaluate the following expression for each index (after the first, all the way to the end) of the exposure vector
    if (full.exposure[i] %in% colnames(init.mem.rep)) { # but only do it if that index matches a letter in the column names of the initial probability matrix.

      learned.strengths[full.exposure[i - 1], # To identify the row in the learned strengths matrix you want to update, identify the row of the previous index's letter in the exposure vector
                        full.exposure[i]] <- # and to know what column, grab the column from the current index of the exposure vector
        learned.strengths[full.exposure[i - 1], full.exposure[i]] + 1 # the value you assign to the row and column gathered from the previous steps will be +1 more than the value that is currently stored in that location in the learned associations matrix because of the additional presentation strengthening the memory representation
    }

    # Memory isn't perfect though, here you implement a forgetting term to account for letter relationships people will forget
    for (j in 1:nrow(learned.strengths)) { # For each row in the learned strengths matrix
      for (k in 1:ncol(learned.strengths)) { # and each column in the learned strengths matrix

        if (learned.strengths[j, k] > 0) { # as long as the value in each cell in the matrix is greater than 0 (because you can't forget what you have not learned)
          p.forget <- max.forgetting * exp(-forgetting.slope * learned.strengths[j, k]) # calculate the probability someone will forget the association they just learned using the negative exponent function
          if (runif(n = 1) < p.forget) { # if that probability is greater than 1,
            learned.strengths[j, k] <- learned.strengths[j, k] - 1 # subtract one from the current value of the learned strength association in that cell of the matrix
          }
        }
      }
    }

    mem.rep.dev[i, , ] <- learned.strengths # for each time slice (or exposure trial) store the letter memory strengths

  }

  df.activations <- melt(mem.rep.dev[ , , ])
  names(df.activations) <- c("Time", "Cue", "Target", "Strength")

  g1 <- ggplot(data = df.activations,
               mapping =
                 aes(
                   x = Target,
                   y = Cue,
                   size = Strength,
                   color = Strength)
  ) +
    geom_point() +
    theme_minimal() +
    scale_size_continuous(range = c(min(learned.strengths),
                                    max(learned.strengths))) +
    labs(
      title = "Learned Strengths Between Cue and Target Letters",
      caption = paste("Revised:", Sys.time())
    ) +
    theme(...)

  print(g1)

  df.summary <- df.activations |>
    group_by(Time, Target) |>
    summarise(MeanStrength = mean(Strength)) |> ungroup()

  g2 <- ggplot(
    data = df.summary,
    mapping =
      aes(
        x = Time,
        y = MeanStrength,
        color = Target,
        group = Target,
        alpha = .8
      )
  ) +
    geom_line(
      lwd = 1.2,
      show.legend = F
    ) +
    theme_minimal() +
    scale_x_continuous(limits = c(0, n.trials)) +
    geom_text(
      data =
        filter(df.summary, Time == max(Time)),
      mapping =
        aes(label = paste0(Target, ": ", round(MeanStrength, 2))),
      hjust = -0.1,
      show.legend = F) +
    labs(
      title = "Average Target Letter Strength Over Time",
      y = "Average Strength",
      x = "Trial",
      caption = paste("Revised:", Sys.time())
    ) +
    theme(...)

  print(g2)

  g3 <- ggplot(
    data = df.activations,
    mapping =
      aes(
        x = Time,
        y = Strength,
        color = Target,
        group = Target,
        alpha = .8
      )
  ) +
    geom_line(
      lwd = .8
    ) +
    theme_minimal() +
    scale_x_continuous(limits = c(0, n.trials)) +
    facet_wrap(
      facets = df.activations$Cue
    ) +
    labs(
      title = "Cue and Target Strength Over Time",
      caption = paste("Revised:", Sys.time())
    ) +
    theme(...)

  print(g3)

  print(learned.strengths)
  kable(probability.record, digits = 3)
}
