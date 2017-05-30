# StreamingActiveLearningStrategies
The main.R file includes a collection of hybrid active learning strategies for streaming scenarios.

Those strategies are tested and compared in a Credit Card Fraud Detection setting.
A full description and the comparison is presented in the paper "An Assessment of Streaming Active Learning Strategies for Real-Life Credit Card Fraud Detection" which has been submitted to DSAA2017.

The code can be runned on the synthetic dataframes in the folder "./syntdf/".
Such folder contains 30 files, which include some genuine and fraudulent transactions.

You can run the code using your own dataset. The dependent feature should be called "y" and there should be as well a "CARD_ID" feature which refer to the credit card identificator.
