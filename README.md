# Estimote-Analysis-R

**Introduction**

This script analysises various aspects of the sociocultural context in a nursing home:
- Patients activity, sleep and proximity to other patients and nurses
- Nurses activity and proximity to patients

The data collection is done by equipping patients and nurses with [Estimote Stickers](https://estimote.com/) which broadcast 
[Nearable](https://estimote.github.io/Android-SDK/JavaDocs/com/estimote/sdk/Nearable.html) packets (containing RSSI, 
accelerometer, UUID). These packets are received and parced by automation units which runs an app built using the 
[Estimote SDK](https://developer.estimote.com/).



**Script variables**
