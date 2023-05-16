# Project Title: Predicting Top Ranking Songs

## Overview

This project aims to uncover the elements that contribute to a song's success in the music industry. I analyzed various features of songs, such as loudness, danceability, and energy, to identify patterns that could help predict whether a song will rank highly in music charts. I employed data analysis techniques such as logistic regression, clustering, and random forest to study the relationships within the data.

## Data

The dataset we used for this analysis is the "Spotify top chart songs 2022" dataset from Kaggle, which includes 646 observations and 17 columns. The dataset was cleaned and preprocessed, removing unnecessary columns and re-categorizing some variables for effective analysis.

## Methodology

I began by using a correlation matrix to identify relationships between variables. I then employed logistic regression, hierarchical clustering, k-means clustering, and random forest algorithms to build models predicting a song's peak rank.

## Installation and Usage

1. Clone this repository to your local machine using `https://github.com/BlancheC/Predicting-Top-Ranking-Songs.git`
2. Install the necessary packages listed in `requirements.txt` using pip:
   `pip install -r requirements.txt`
3. Open the Jupyter Notebook (`Predicting_Top_Ranking_Songs.ipynb`) and run the cells to see the analysis and models in action.

## Results

The final model, a Random Forest classifier, identified loudness, energy, and danceability as significant predictors of a song's success. For a song to potentially rank high, it should aim for the following values: 

- Loudness >= -6.307 decibels
- Energy >= .63 points
- Danceability >= .68 points

## Future Work

Future iterations of this project may consider adding other variables into the mix, such as the artist, genre, record label, and marketing strategies, to further enhance the prediction model's accuracy.

## Contributing

Any contributions to this project is welcomed! Please fork the repository and make changes as you'd like. If you have any ideas, just open an issue and tell us what you think.

