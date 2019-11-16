a = 10
b = 5

x = seq(1, 100, 1)
x2 = cos(x)
y = a*x + b

model <- lm(y ~ x)

print('perfect fit')
print(summary(model))
plot(y, model$residuals)

y = a*jitter(x, factor = 10) + b + x2
model <- lm(y ~ x)


print('with some randomness in y')
print(summary(model))
plot(y, model$residuals)