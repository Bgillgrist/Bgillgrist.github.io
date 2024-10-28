# Baseball Lineup Optimization Tool

A simulation and regression tool for finding the optimal baseball lineup given key stats. This project was my capstone project with the Arroyo Seco Saints baseball team over the summer 2024 season.

# Explanation and Usage:

This is a baseball lineup optimization tool that I created in python during my time at the Arroyo Seco Saints. There are two methods in this code to determine the best possible lineup.
1. Linear regression predicting runs from the OBP and SLG of each batting order spot from the 2023 Saints season.
2. Game simulation using OBP, SLG, AVG, and PA to find out the expected number of runs for each batting order.

The code runs through every permutation of the batting order and outputs the new best batting order for each method as the old one is beat. Running through every possible combination would take a long time and the diminishing return for waiting for it to finish is often not worth it, however it is optimal to let it finish. I believe the game simulation method to be more accurate and a better way to set a batting order due to the small sample size of the regression (1 season). 

To run the code, just input the player names and stats in the code (labeled in the code) and then let it run. Don't compare the two different methods side by side because they are calculated completely differently. Find out which method you prefer and then only compare those method results to each other.

# Output Image Example:

- ![Lineup Optimization Example](../assets/Lineup%20Optimization%20Example.png)

# Example Code Snippet:

- ![Lineup Code Snippet](../lineup-optimization/Lineup%20Code%20Snippet.png)  
