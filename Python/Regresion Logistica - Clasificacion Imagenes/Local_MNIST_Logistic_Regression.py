from sklearn.linear_model import LogisticRegression
import Load_Dataset as ld
import numpy as np

X_train, y_train, X_test, y_test = ld.get_data()

clf = LogisticRegression()
clf.fit(X_train, y_train)

y_hat = clf.predict(X_test)
print(np.average(y_hat == y_test))