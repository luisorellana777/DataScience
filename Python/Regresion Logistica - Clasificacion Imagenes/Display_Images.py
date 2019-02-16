# make sure utils.py is in the same directory as this code
import matplotlib.pyplot as plt
import numpy as np
import Load_Dataset as ld



# note we also shrink the intensity values (X) from 0-255 to 0-1. This helps the model converge faster.
X_train, y_train, X_test, y_test = ld.get_data()

# now let's show some randomly chosen images from the traininng set.
count = 0
sample_size = 30
plt.figure(figsize = (16, 6))
for i in np.random.permutation(X_train.shape[0])[:sample_size]:
    count = count + 1
    plt.subplot(1, sample_size, count)
    plt.axhline('')
    plt.axvline('')
    plt.text(x=10, y=-10, s=y_train[i], fontsize=18)
    plt.imshow(X_train[i].reshape(28, 28), cmap=plt.cm.Greys)
plt.show()