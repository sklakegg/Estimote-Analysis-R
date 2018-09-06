# Estimote-Analysis-R

### Introduction
This script analysises various aspects of the sociocultural context in a nursing home:
- Patients activity, sleep and proximity to other patients and nurses
- Nurses activity and proximity to patients

The data collection is done by equipping patients, patients' bed and nurses with [Estimote Stickers](https://estimote.com/) which broadcast 
[Nearable](https://estimote.github.io/Android-SDK/JavaDocs/com/estimote/sdk/Nearable.html) packets (containing RSSI, 
accelerometer, UUID). These packets are received and parced by automation units which runs an app built using the 
[Estimote SDK](https://developer.estimote.com/).

### Script variables
Start by declaring the following variables:
- Directories for location of the data, scripts, output and models.
- Declare vectors patients_all and nurses_all which contains the alias of all users.
- Declare the named vector nearables which links the alias with the UUID of the sensors for all the users.
  - Repeat for automation units
  
### Script flow and expected data structure
 
The script expects .Rdata files to be located at the path of the data_dir variable.
Each .Rdata file contains a data frame (named df_combined) with data for 1 day (from 06.30 in the morning, until 06.29 the next day e.g., 2017-08-24 06:30:00 EEST to 2017-08-25 06:29:59 EEST). The structure of the data is shown in the figures below.
 
![screen shot 2018-09-06 at 16 04 43](https://user-images.githubusercontent.com/14874913/45159430-65526980-b1ef-11e8-9a40-c53979c21018.png)

![screen shot 2018-09-06 at 16 05 16](https://user-images.githubusercontent.com/14874913/45159452-73a08580-b1ef-11e8-8675-8f7a53f9c2c5.png)

The script loops through each .Rdata file and outputs a .Rdata file containing the result for each analysis.
To use the script for real time computations, the for loop should be replaced with a daily db query that retrieves the data for that particular day.

 
