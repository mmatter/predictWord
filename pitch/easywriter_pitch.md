<style>
    font-size: 14pts
}
</style>

Coursera DataScience Capstone project: Easywriter
========================================================
author: Michel Matter (mich1983mat@gmail.com)
date: November 30th, 2017
autosize: true
font-family: 'Helvetica'


Project's framework
========================================================

In the frame of the Coursera Data Science Capstone project, we have been working
on a Natural Language Processing (NLP) problem which consists in predicting, given a phrase,
the next word in the phrase.

We have been focusing on developing a Shiny web app being a typesetting helper suggesting to the user 
word completions as well as entire next plausible word, in the spirit of the various typesetting helpers available on
most smartphones.

Presenting the Easywriter
========================================================

Check out the app [here](https://ptitmatheux.shinyapps.io/easywriter/) !

Easywriter's main features:

- suggests you entire next word
- helps you to complete words being typed
- high reactivity: recalculates each time a character is typed !
- simplicity of use: just starts typing anything...

Training Data and Algorithm
========================================================

We have trained the model on three corpora coming from the following sources: news, blogs, and twitter; we have used about 25% of the available data from each corpus to train the model.

The following basic pre-processing was performed: removing numbers and special characters; putting all letters to lowercase.

We have used N-grams models (for N=1,...,4) combined with a *stupid backoff* smoothing approach
in order to select the most likely next words given the user's input.

During the training phase, we have computed transition probabilities for the next word given the N last words and stored
them in (sparse) matrices.

For efficiency, both vocabulary and N-grams were pruned at some levels (e.g., at least 5 occurences for bigrams).
The pruned N-grams were considered as *unkown* using the generic word <UNK>.


Accuracy
========================================================

- 16% accuracy of matching entire next word
- 26% accuracy of matching one of the three most likely predictions
- 52% accuracy of matching one of the three most likely predictions when knowing the first letter of the target word

Overall accuracy could be improved by a more careful preprocessing of the training data, such as word cleaning, keeping track of beginning/ends of sentences, etc. 
