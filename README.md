# Project 4: Algorithm implementation and evaluation: Collaborative Filtering

### [Project Description](doc/project4_desc.md)

Term: Spring 2020

+ Team 12
+ Projec title: Algorithm implementation and evaluation: Collaborative Filtering
+ Team members
	+ Siqi Zhao (Prensenter)
	+ Vikki Sui
	+ Liangcao Ling
	+ Sol Lee
	+ Lu Cheng
    + Jacquelyn Blum

+ Project summary: In this project, our group implement the matrix factorization algorithm for recommender system and compare two diffenrent methods of post processing. The matrix factorization algorithm outputs a feature matrix for both users and movies so that one can predict the rating of a specific user - movie pair using those matrix. First, we include temporal effect of users and movies in the loss function. Second we build the algorithm using stochastic gradient descent to estimate the parameters in the loss function. Then we use cross - validation to choose the best parameters. Last we compare the test RMSE and train RMSE of two different postprocessing methods - KNN and kernel ridge regression. 
        
	
    + Result

     | Method      | Train RMSE  | Test RMSE |
     | ------------- |:-------------:| ------------:|
     | Without Postprocessing      |   0.50   | 1.06  |
     | Postprocessing SVD with KNN | 1.21 | 1.20   |
     | Postprocessing SVD with kernel ridge regression | 1.25 | 1.32  |      
        
	
+ Reference    
              
 1. Koren, Y., Bell, R. & Volinsky, C. (2009). Matrix factorization techniques for recommender systems. IEEE computer society.

 2. Paterek, A. (2007). Improving regularized singular value decomposition for collaborative filtering. KDDCup.07.

 3. Koren, Y. (2009). Collaborative filtering with temporal dynamics. KDD'09.       
        
	
+ **Contribution statement**: All team members contributed in all stages of this project. 
  
  + **Siqi Zhao** 
  
  + **Vikki Sui** 
  
  + **Liangcao Ling**  
  
  + **Sol Lee** 

  + **Lu Cheng**    
        
  + **Jacquelyn Blum**

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
