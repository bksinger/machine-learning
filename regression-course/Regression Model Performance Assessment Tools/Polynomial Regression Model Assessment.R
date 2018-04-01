path = "C:\\Users\\bksin\\Desktop\\Education\\Machine Learning\\Regression\\Week 3"
setwd(path)
options(digits=20)

make_polynomial_model = function(data, target_variable, feature_variable, degree) 
{
  
  regression_expression = "target ~ power_1"
  x = data.frame(target=data[,target_variable], power_1=data[,feature_variable])
  
  if(degree > 1)
  {
    for (i in 2:degree)
    {
      power = paste("power_",toString(i),sep="")
      #power = "power_2"
      regression_expression = paste(regression_expression, " + ", power,sep="")
      #print(paste("regression_expression = ", regression_expression),sep="")
      
      x = cbind(x,power_2=data[,feature_variable]^i)
      names(x)[i+1] = power
    }  
  }
  
  fit = lm(regression_expression, data=x)
  return(fit)
}


sales = read.csv("kc_house_data.csv")
sales = sales[order(sales["sqft_living"],sales["price"]),]
sales["sqft_living"] = as.integer(sales[1:nrow(sales),"sqft_living"])
sales["price"] = as.integer(sales[1:nrow(sales),"price"])

fit = make_polynomial_model(sales,"price","sqft_living",1)
fit

fit2 = make_polynomial_model(sales,"price","sqft_living",2) 
fit2

fit3 = make_polynomial_model(sales,"price","sqft_living",3) 
fit3

fit4 = make_polynomial_model(sales,"price","sqft_living",4) 
fit4

fit15 = make_polynomial_model(sales,"price","sqft_living",15) 
fit15