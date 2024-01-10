# SwiftText word prediction app

## SwiftText Shiny App Project Summary:

**Algorithm and Model Design:**

Implemented n-grams (unigram, bigram, trigram, 4-gram) frequency tables with stupid backoff for predictive modelling.
Utilized a dictionary for efficient word matching and incorporated Jaccard metric-based string distance to handle erroneous text input.
Models demonstrate an overall top-3 score of 16.11%, top-1 precision of 11.68%, and top-3 precision of 20.05%.

**Data Preprocessing:**

We conducted thorough preprocessing, including the removal of noise such as stopwords and digits.
Captured word patterns and relationships through frequency tables, enhancing the model's predictive capabilities.
Shiny App Development:

Created a Shiny app hosted on shinyapps.io that accepts a phrase as input and predicts the next word.
Ensured the app loads promptly and functions effectively, showcasing the SwiftText algorithm in an interactive and accessible manner.
Evaluation and Testing:

Evaluated the Shiny app based on criteria, including its accessibility, loading speed, and accuracy in predicting the next word.
Tested the app with five phrases from Twitter or news articles, ensuring predictions are provided for each input, showcasing the algorithm's real-world applicability.

This project encapsulates the development of the SwiftText Shiny app, highlighting the algorithm's design, preprocessing steps, app functionality, and rigorous testing to demonstrate its predictive accuracy and usability.

 Go to this URL to demo the app https://8onzsa-rishav-dhariwal.shinyapps.io/coursera_prediction_proj/  
 The pitch presentation can be viewed here https://rishavdaredevil.github.io/SwiftText-word-prediction-app/
 
