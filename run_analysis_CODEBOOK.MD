<<<<<<< HEAD
Getting and Cleaning Data Project
Nagesh Padiyar

Description

Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

Source Data
A full description of the data used in this project can be found at UCI Machine Learning Repository

The Source data for the project can be found here.

Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a Smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers were selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Attribute Information

For each record in the dataset it is provided:

Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
Triaxial Angular velocity from the gyroscope.
A 561-feature vector with time and frequency domain variables.
Its activity label.
An identifier of the subject who carried out the experiment.
 

Section 1. Merge the training and the test sets to create one data set.

Load the following packages

1.	plyr – Load plyr first. Then dplyr.
2.	dplyr
3.	data.table
4.	reshape2 – This is required for melt package

After setting the source directory for the files, read into tables the data located in 

1.	features.txt
2.	activity_labels.txt
3.	subject_train.txt
4.	x_train.txt
5.	y_train.txt
6.	subject_test.txt
7.	x_test.txt
8.	y_test.txt

Assign Column Names for both Training and Test datasets.

Create Final data set by merging both Training and Test data sets to create one data set.

Section 2. Extract only the measurements on the mean and standard deviation for each measurement.

1.	Create an index called mean_col_index which selects only those rows which have “mean” in the Final data set.

2.	Create a dataset called mean_col_names which gets all the names from Final data set which correspond to the index mean_col_index.

3.	Create an index called std_col_index which selects only those rows which have “std” in the Final data set.

4.	Create a dataset called std_col_names which gets all the names from Final data set which correspond to the index std_col_index.

5.	Create a new dataset called finalmeanstdData which contains records which have “subjectID”, “activityID”, mean_col_names and std_col_names.

 


Section 3. Use descriptive activity names to name the activities in the data set

Merge the activityLabels with finalmeanstdData using activityID as the Id for merging between the two datasets.

Section 4. Appropriately label the data set with descriptive activity names.

Use gsub function for pattern replacement to clean up the data labels.

Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

Use the melt function to melt the data set finalmeanstdData.

Use the dcast function to find the average of each variable for each activity and each subject.
=======
Getting and Cleaning Data Project
Nagesh Padiyar

Description

Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

Source Data
A full description of the data used in this project can be found at UCI Machine Learning Repository

The Source data for the project can be found here.

Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a Smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers were selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Attribute Information

For each record in the dataset it is provided:

Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
Triaxial Angular velocity from the gyroscope.
A 561-feature vector with time and frequency domain variables.
Its activity label.
An identifier of the subject who carried out the experiment.
 

Section 1. Merge the training and the test sets to create one data set.

Load the following packages

1.	plyr – Load plyr first. Then dplyr.
2.	dplyr
3.	data.table
4.	reshape2 – This is required for melt package

After setting the source directory for the files, read into tables the data located in 

1.	features.txt
2.	activity_labels.txt
3.	subject_train.txt
4.	x_train.txt
5.	y_train.txt
6.	subject_test.txt
7.	x_test.txt
8.	y_test.txt

Assign Column Names for both Training and Test datasets.

Create Final data set by merging both Training and Test data sets to create one data set.

Section 2. Extract only the measurements on the mean and standard deviation for each measurement.

1.	Create an index called mean_col_index which selects only those rows which have “mean” in the Final data set.

2.	Create a dataset called mean_col_names which gets all the names from Final data set which correspond to the index mean_col_index.

3.	Create an index called std_col_index which selects only those rows which have “std” in the Final data set.

4.	Create a dataset called std_col_names which gets all the names from Final data set which correspond to the index std_col_index.

5.	Create a new dataset called finalmeanstdData which contains records which have “subjectID”, “activityID”, mean_col_names and std_col_names.

 


Section 3. Use descriptive activity names to name the activities in the data set

Merge the activityLabels with finalmeanstdData using activityID as the Id for merging between the two datasets.

Section 4. Appropriately label the data set with descriptive activity names.

Use gsub function for pattern replacement to clean up the data labels.

Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

Use the melt function to melt the data set finalmeanstdData.

Use the dcast function to find the average of each variable for each activity and each subject.
>>>>>>> 185953a09fb0437fd406a979343a11a7759c04c3
