# -*- coding: utf-8 -*-
"""
Created on Tue Nov 29 11:30:15 2016

@author: DELL
"""

import numpy as np
import scipy as sp
from sklearn.ensemble import RandomForestClassifier
from PIL import Image
import os
import winsound


#Convert image data into binary image before making it a matrix
#Grayscale image looks much better than binary

#A function to Extract the shapes from Filename
def shape(x):
    return {
    'A': 'Triangle',
    'C': 'Circle',
    'c': 'Circle',
    'H': 'Hexagon',
    'P': 'Pentagon',
    'T': 'Rectangle',
    }[x]

#Function to validate the Result
def Prediction_accuracy(Result_data):
    right_pred = 0
    obs = len(Result_data)
    i = 0
    for i in range(obs):
        if Result_data[i,1]==shape(Result_data[i,0][0]):
            right_pred+=1
        i+=1
    print('Total number of Correct predictions :' + str(right_pred))
    print('Accuracy of Algorithm :' + str(round(right_pred/obs,4)))

#Function to convert all the images in a folder to Grayscale. This should 
#remove the color from the images and reduce the size of our data
def Grayscale(path):
    for filename in os.listdir(path):
      target_path = path + '\\' + filename
      Temp_Image = Image.open(target_path)
      Temp_Image = Temp_Image.convert('L')
      Temp_Image.save(target_path)

#Function to Extract the image Shape, FileName and Pixel data and store it as
# an array in the format <shape>,<name>,<Pixel_data>
def ExtractImageData(path):
    i=0
    Train_data_shapes = np.empty(shape=[0,2])
    Pixel_data = np.empty(shape=[0, 784])
    File_data = np.empty(shape = [0,786])
    for filename in os.listdir(path):
      target_path = path + '\\' + filename
      image_data = np.reshape(sp.misc.imread(target_path), (1,784))
      Pixel_data = np.vstack((Pixel_data, image_data))
      Train_data_shapes = np.vstack((Train_data_shapes, np.asarray((shape(filename[0]), filename))))
      print(i)
      i+=1
    File_data = np.c_[Train_data_shapes, Pixel_data]
    return File_data
    
Train_path = 'G:\Mate labs Exercise\Data\Shapes\Database\Train\Consolidated'
Grayscale(Train_path)
Train_Image_data = ExtractImageData(Train_path)

Test_path = 'G:\Mate labs Exercise\Data\Shapes\Database\Test\Consolidated'
Grayscale(Test_path)
Test_Image_data = ExtractImageData(Test_path)

rf = RandomForestClassifier(n_estimators = 1000, max_features = 28, n_jobs = -1)
rf.fit(Train_Image_data[:,2:], Train_Image_data[:,0])

prediction = rf.predict(Test_Image_data[:,2:])

Result_path = 'G:\Mate labs Exercise\Data\Shapes\Database\Test\shapes.csv'
Result_data = np.c_[ Test_Image_data[:,1], prediction]
np.savetxt(Result_path, Result_data , delimiter = ',', header = 'Filename,Prediction', comments = '', fmt = '%s')

Prediction_accuracy(Result_data)
winsound.Beep(3000,5000)

binary_path = 'G:\Mate labs Exercise\Binary conversion'
def BinarizeFolder(path):
    #Assuming the images are already in grayscale
    for filename in os.listdir(path):
      target_path = path + '\\' + filename
      Temp_Image = sp.misc.imread(target_path)
      #Temp_Image = np.reshape(Temp_Image, (1,784))
      Temp_Image = BinaryNeutralize(Temp_Image)
      sp.misc.imsave(target_path, Temp_Image)

def BinaryNeutralize (Image_data):
    Image_density = np.sum(Image_data)/Image_data.size
    Final_Image = np.where(Image_data[:,:]>Image_density, 1, 0)
    return Final_Image
    
BinarizeFolder(binary_path)
