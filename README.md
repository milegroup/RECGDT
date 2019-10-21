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

---------------------------------------------------------------------------------------------------------------------------------

If you want to run an ECGDT test as in the demo video, you can download a 12-lead ECG record from PhysioNet (https://physionet.org/content/incartdb/1.0.0/). For example, if you use record I17:

- First, download I17.atr, I17.dat and I17.hea files.
- Then, you can obtain an ASCII file using the WFDB applications, running the command: rdsamp -r I17 > I17.
- After that, you can add to the resultant ASCII file the header in ECGDT format, by pasting at the beginning of the file:

    <Header>
      id=I17
      sFreq=257
      nLeads=12
      nSamples=462600
      channels=I,II,III,AVR,AVL,AVF,V1,V2,V3,V4,V5,V6
      gender=M
      conditions=bradycardia,bundleBranchBlock,ischemia,PVCs
    </Header>
    
- With your record file ready, you can select it from the Load ECG menu.

