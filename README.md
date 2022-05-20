# pattern_language_data_analysis

The purpose of this analysis is to explore the pattern language data.
The pattern data is the result of analyzing the patterns present and their relationships to one another in 100 applied behavioral science cases.
The patterns were generated using a practice-based synthesis approach that is outside the scope of the quantitative data analysis.

Data analysis is concerned with the occureces of patterns, 
the use of patterns as problems become more difficult, and 
the relationships of applying patterns in practice.

The data analysis results in: 
- total occuences of patterns across cases
- the scale at which patterns exist (individual, community, systems)
- a linear regression of total patterns used in a case relative to the difficulty of the problem at hand
- a graph (i.e., network) of complementary relationships between patterns
- a graph of interchangeable relationships between patterns

# A detail of the files input and their meanings

Occurences.csv
Column names include: [Case, Pattern 1, Pattern 2, ..., Patern n]
Values should be numerical where:
case values represent case number and
pattern values are 0 (pattern is absent) or 1 (pattern is present in case)

Complementary.csv 
Column names include: [Case, Pattern 1, Pattern 2, ..., Pattern n]
Values should be numerical where:
case values represents case number (1, 2, 3, ... n_cases) and 
pattern values are 0 (pattern is absent from case) or 1 (pattern is present and complementary to other patterns in case)

Interchangeable.csv
Column names include: [Case, Pattern 1, Pattern 2, ..., Pattern n]
Values should be numerical where:
case values represents case number (1, 2, 3, ... n_cases) and 
pattern values are 0 (pattern is absent from case) or 1 (pattern is present and interchangeable to other patterns in case)

Proportional.csv
Describes the scale that each pattern acts on: 
Scales include: 
Individual and the immediate choice environment
Community, institutional, and civic conditions
Systems level scales, the sociocultural conditions
