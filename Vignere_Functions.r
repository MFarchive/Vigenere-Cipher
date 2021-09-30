#R Vignere Functions Document

alphabet <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",
"l", "m", "n", "o", "p", "q", "r", "s",
"t", "u", "v", "w", "x", "y", "z")

english_frequencies <- data.frame(
    letters = alphabet,
    frequencies = c(8.4966, 2.0720, 4.5388, 3.3844, 11.1607, 1.8121, 2.4705,
    3.0034, 7.5448, 0.1965, 1.1016, 5.4893, 3.0129, 6.6544, 7.1635, 3.1671,
    0.1962, 7.5809, 5.7351, 6.9509, 3.6308, 1.0074, 1.2899, 0.2902, 1.7779,
    0.2722) / 100
)

#Removes all punctuation, upper case letters and spaces from a string
format_func <- function(string) {
    string <- gsub("[[:punct:][:blank:]]", "", string)
    string <- gsub("[0-9]", "", string)
    string <- tolower(string)
    return(string)
}

#Function returns the encrypted text by applying the vigenere cipher
#Function requires a key, and a string, returns the ciphertext
encrypt_func <- function(text, key) {
    encrypted <- c()
    #loop to get alphabetical indexes of the characters in the text
    split_text <- strsplit(text, "")[[1]] #creates a character class
    for (i in 1:nchar(text)) {
        letter_index <- which(alphabet %in% split_text[i])
        j <- i %% nchar(key)
        if (j == 0) {
            j <- nchar(key)
        }
        key_index <- which(alphabet %in% substring(key, j, j))
        #applying the relevant shift to the letters, using the vigenere cipher
        alphabet_index <- (letter_index + key_index - 2) %% length(alphabet) + 1
        encrypted <- paste(encrypted, alphabet[alphabet_index], sep = "")
    }
    return(encrypted)
}

decrypt <- function(ciphertext, key) {
    decrypted <- c()
    for (i in 1:nchar(ciphertext)) {
        j <- i %% nchar(key)
        if (j == 0) {
            j <- nchar(key)
        }
        key_index <- which(alphabet %in% substring(key, j, j))
        cipher_index <- which(alphabet %in% substring(ciphertext, i, i))
        alphabet_index <- (cipher_index - key_index) %% 26 + 1
        decrypted <- paste(decrypted, alphabet[alphabet_index], sep = "")
    }
    return(decrypted)
}

#Kasiski Algorithm Functions to find the key length


#Repeats function, finds all n letter repeats within the ciphertext
#   then counts their frequency, finds the spacing between repeats,
#   then finds the most comming factor of all the spacings (kas_mode)
repeats <- function(ciphertext, n) {
    elements <- c()
    indexes <- list()
    spacings <- list()
    for (i in 1:nchar(ciphertext)) {
        if (i <= nchar(ciphertext) - n + 1) {
            substr <- substring(ciphertext, i, i + n - 1)
            if (substr %in% elements == FALSE) {
                elements <- c(elements, substr)
                data <- gregexpr(substr, ciphertext)
                data2 <- list("attributes<-" (data[[1]], NULL))
                indexes <- append(indexes, data2)
                if (length(data2[[1]]) > 1) {
                    diff <- c()
                    for (j in 1:(length(data2[[1]]) - 1)) {
                        for (k in 1:(length(data2[[1]]) - j)) {
                            diff <- c(diff, data2[[1]][j + k] - data2[[1]][j])
                        }
                    }
                    spacings <- append(spacings, diff)
                }
            }
        }
    }
    factors_matrix <- c()
    for (i in 2:nchar(ciphertext)) {
        counts <- 0
        for (d in spacings) {
            if ((d %% i) == 0) {
                counts <- counts + 1
            }
        }
        if (counts != 0) {
            factors_matrix <- rbind(factors_matrix, c(i, counts))
        }
    }
    kas_mode <- 0
    mode_val <- 0 
    for (r in 1:nrow(factors_matrix)) {
        if (factors_matrix[r, 2] >= mode_val) {
            kas_mode <- factors_matrix[r, 1]
            mode_val <- factors_matrix[r, 2]
        }
    }
    return(kas_mode)
}


#Function takes every nth letter starting from index m,
#where m is in range 0:n-1
str_subset <- function(junk, n, m) {
   ifelse(n == 1, junk,
paste(strsplit(junk, split = NULL)[[1]][(1:nchar(junk)) %% n == m],
collapse = ""))
}


#Function below returns a list of the ciphertext split into n=key_length
#subsections, so that letter frequency analysis can be used
nth_letter <- function(key_length, ciphertext) {
    list_of_substrings <- c()
    sub_indexes <- 1:(key_length - 1)
    sub_indexes <- append(sub_indexes, 0)
    if (key_length == 1) {
        sub_indexes <- c(1)
    }
    for (i in sub_indexes) {
        list_of_substrings <- append(list_of_substrings,
        str_subset(ciphertext, key_length, i))
    }
    return(list_of_substrings)
}

sub_letter_totals <- function(key_length, ciphertext) {
    sorted_frequencies <- c()
    list_of_substrings <- nth_letter(key_length, ciphertext)
    index <- 1
    for (sub in list_of_substrings) {
        letter_freqs <- c()
        sub_split <- strsplit(sub, "")[[1]]
        for (letter in alphabet) {
            count <- length(grep(letter, sub_split))
            letter_freqs <- append(letter_freqs, count)
        }
        sorted_frequencies[[index]] <- letter_freqs
        index <- index + 1
    }
    return(sorted_frequencies)
}

percentages <- function(vec_of_vecs) {
    int <- 1
    s_percs <- c()
    for (i in vec_of_vecs) {
        j <- i / sum(i)
        s_percs[[int]] <- j
        int <- int + 1
    }
    return(s_percs)
}

chi_squared <- function(eng_freqs, sub_percs) {
    chis <- c()
    ind <- 1
    for (sublist in sub_percs) {
        chi_stats <- c()
        for (i in 1:length(alphabet)) {
            chi <- 0
            if (i == 1) {
                series <- i:26
            }
            else {
                series <- (28 - i):26
                series <- c(series, 1:(27 - i))
            }
            for (j in seq_along(sublist)) {
                value <- ((sublist[j] - eng_freqs[[2]][series[j]]) ^ 2) / (eng_freqs[[2]][series[j]])
                chi <- chi + value
            }
            chi_stats <- c(chi_stats, chi)
        }
        chis[[ind]] <- chi_stats
        ind <- ind + 1
    }
    return(chis)
}

#Function which returns the n most likely letters for every key letter.
#It takes in two arguments, the vector list of chi squared values, and n.
#It returns a vector, of length == key length.
likely_letters <- function(chi_vector, n) {
    n_likely <- c()
    step <- 1
    for (chis in chi_vector) {
        letters <- c()
        sorted_freqs <- chis[order(chis)]
        for (int in 1:n) {
            letters <- c(letters, alphabet[match(sorted_freqs[int], chis)])
        }
        n_likely[[step]] <- letters
        step <- step + 1
    }
    return(n_likely)
}

### Twist Algorithm Implementation, used to find the key length

twist_subsets_func <- function(key_limit, text) {
    all_key_lengths <- c()
    for (m in 1:(key_limit + 1)) {
        nth_sub <- sub_letter_totals(m, text)
        perc <- percentages(nth_sub)
        all_key_lengths[[m]] <- perc
    }
    return(all_key_lengths)
}

twists <- function(twist_subsets) {
    twists <- c()
    for (c in 1:length(twist_subsets)) {
        twist <- 0
        for (r in 1:length(twist_subsets[[c]])) {
            list_of_percs <- sort(twist_subsets[[c]][r][[1]], decreasing = TRUE)
            twist <- twist + sum(list_of_percs[1:13]) - sum(list_of_percs[14:26])
        }
        twist <- twist * (100 / c)
        twists <- c(twists, twist)
    }
    return(twists)
}

twistplus <- function(twists) {
    twistplus <- c()
    for (i in seq_along(twists)) {
        subtract <- 0
        if (i != length(twists)) {
            for (j in 1:i) {
                subtract <- subtract + (twists[j]/(i))
            }
            number <- twists[i + 1] - subtract
            twistplus <- c(twistplus, number, i + 1)
        }
    }
    mode_value <- 0
    sequence <- seq_along(twistplus)
    integers <- which(sequence %% 2 == 1)
    for (j in integers) {
        if(twistplus[j] > mode_value) {
            mode <- twistplus[j + 1]
            mode_value <- twistplus[j]
        }
    }
    return (mode)
}

############################
#Testing
############################

#Code below demonstrates how the Vigenere Cipher is solved with the twistplus algorithm
#to identify the key length.

normal_text <- "there are various kinds of certainty. a belief is psychologically certain when the subject who has it is supremely convinced of its truth. certainty in this sense is similar to incorrigibility, which is the property a belief has of being such that the subject is incapable of giving it up.but psychological certainty is not the same thing as incorrigibility. a belief can be certain in this sense without being incorrigible; this may happen, for example, when the subject receives a very compelling bit of counterevidence to the (previously) certain belief and gives it up for that reason."
keyword <- "notebook"

c_text <- encrypt_func(format_func(normal_text), keyword)
twists_a <- twists(twist_subsets_func(8, c_text))
mode <- twistplus(twists_a)
print(mode)

output <- sub_letter_totals(mode, c_text)
percs <- percentages(output)
chi_vals <- chi_squared(english_frequencies, percs)
likely <- likely_letters(chi_vals, 3)
print(likely)

decryp <- decrypt(c_text, "notebook")
print(decryp)


#######################
#Testing the Kasiski Algorithm
#######################

#Try different length ciphertexts and keys to determine the effectiveness of the Kasiski Algorithm
#compared to the Twistplus algorithm. You may find different results for the function repeats(text, n), when
#changing 'n' the length of the repeated fragment of text.
repeats(c_text, 5)
