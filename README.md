# RECGDT
R package for ECGDT (ECG Diagnosis Tool). A system that is capable of:
  - Load digitized ECG records.
  - Show the ECG signal in the different channels available.
  - Detect beats, using single-channel and multi-channel methods.
  - Show the ECG signal, with the detected beats highlighted.
  - Create a basal (average) beat for a given one.
  - Delineate waves inside a beat.
  - Obtain a diagnosis score for six major diseases (bradycardia, ischemia, myocardial infarction, tachycardia, ventricular hypertrophy and Wolff-Parkinson-White syndrome), indicating the possibility of their presence in the current record.
 
(*) ECGDT directory contains the GUI files, and the RDS objects needed with the diagnosis models for each disease.

(*) R directory contains R code files, with the core functions of the tool.

(*) src directory contains C code files, with the more efficient code for more complex tasks.
