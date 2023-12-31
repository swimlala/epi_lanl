CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:32Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  P(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ``   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  dp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   @   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   \   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
�      � 
�Argo profile    3.1 1.2 19500101000000  20180724220232  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               
   
DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�f�F���@�f�F���11  @�f�>��@�f�>��@6�t�q�@6�t�q��cү%;�cү%;11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?L��@   @@  @�  @�  @�33@�33A��A  A#33A@  A`  A~ffA�33A�  A�ffA���A�  A�  A�  B   B  B  BffB ffB(  B/��B8  B@��BHffBPffBY33B`��Bh��Bp��BxffB�33B���B�  B�ffB�ffB�ffB�ffB�ffB�ffB�33B���B�  B�33B�ffB�ffB�  B�  B�  B�  B�33B�33Bԙ�B�ffB�  B�  B���B�ffB�33B�  B�ffB�ffB�33C   C�fC33C33C  C
  C  C  C  C�C�C�C  C�C�C33C 33C"L�C#�fC%�fC(  C)�fC,  C.  C0  C2  C4  C5�fC833C:L�C<�C>  C?�fCB33CD�CF  CHL�CJ33CL�CNL�CP33CR  CT�CV  CW�fCZ33C\  C]��C`�Cb33Cd�Ce�fCh�Cj33Cl  Cn  Co��Cr33Ct�Cu��Cx�CzL�C|33C~  C��C��3C��fC�  C�  C��fC��C�  C��3C��C��C��3C��C��C�  C��C��C��3C��C��C��3C��C��C�  C�&fC��C��C�  C��3C�&fC��C�  C�  C��fC��C��C��3C��C�  C��fC�  C��C�  C��fC�  C��C��C��fC�  C��C�&fC�&fC��C��fC��3C�  C��C�  C��fC��3C��C��C��C��fC��3C��C��C�&fC��C�  C��C��C�  C��C��C�  C��fC�  C��C��C��fC�  C�  C��C�  C��3C��C�&fC��C��fC�  C��C��C�&fC��C��fC��3C�  C��C�  C��fC�  C��C�  C��fC��3C�  C��C�  C�ٚC��3C��C��C�  C��3C��3C�  C��C��C��fC�  C��C�  D@ D��D�fD
FfD��D� DL�D�D� Dy�D9�D fD"�3D%l�D(�D*�fD-� D0  D2�fD5` D7�3D:y�D=fD?�fDBfDD�3DG&fDI� DL33DN��DQY�DS� DVl�DX�3D[�fD^�D`� Dc�De��Dh3Dj� Dl�3DoS3Dq�fDt  Dv��Dx�3D{@ D}9�D� D�3D�@ D��fD�ɚD�fD�ffD��3D��D�l�D��3D�0 D�� D�� D�S3D��3D��D�|�D��fD�0 D�|�D��fD�3D�S3D���D��fD�3D�9�D�i�D�� D��fD���D�  D�#3D�@ D�Y�D�|�D���D���D���D���D��D�<�D�Y�D�|�D��3D�� D��fD�)�D�Y�D��3D���D��fD�	�D�6fD�i�DĖfDż�D��fD�#3D�I�D�|�D˩�D�ٚD�	�D�<�D�i�DіfD�� D��3D�&fD�S3D׃3Dس3D��3D�fD�FfD�p Dޜ�D��3D��fD� D�,�D�P D�s3D�fD� D���D��3D��3D���D�	�D�  D�&fD�9�D�@ D�I�D�S3D�\�D�i�D�p D�y�D�� D��3D�l�D�s3D�y�D�|�D��3D���E I�E ɚEP E�fE[3E� Ed�E��Eh E��Es3E�3E��E��E	�fE3E3E��E��EɚE` El�Ey�E�E0 ENfE��E�E>fE\�E��E;3E a�E!��E"�fE#ɚE%t�E&�3E'�3E(� E*X E+ffE,t�E-��E.��E0p E1�fE2њE4+3E5� E6��E7� E8�fE:+3E;t�E<� E@�EB�fEFNfEI4�EL�3EOvfERɚEU��EY�E\�E_` Eb4�EeY�Ehx Ek� En�fEr  Et�fEx�E{Q�E~vfE���E�@�E�ٚE�k3E� E��fE� E��3E�K3E��3E�ZfE��3E��3E��3E�3E�k3E���E�fE�S3E��fE��3E�X�E���E���E�8�E��fE��f>���>���?   ?   ?��>���?��?   ?��?��?333?��?L��?333?L��?L��?�  ?���?�33?���?�ff@   @��@,��@9��@S33@l��@�  @���@�33@�  @���@���@�33@�  @���@陚@���A��A��A33A33A#33A+33A4��A;33AD��AK33AS33A\��Ad��Ak33As33A{33A���A���A���A�  A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141414141414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?fff?�ff@   @`  @�  @�  @�33@�33A	��A  A+33AH  Ah  A�33A�33A�  A�ffAř�A�  A�  A�  B  B
  B  BffB"ffB*  B1��B:  BB��BJffBRffB[33Bb��Bj��Br��BzffB�33B���B�  B�ffB�ffB�ffB�ffB�ffB�ffB�33B���B�  B�33B�ffB�ffB�  B�  B�  B�  B�33B�33Bՙ�B�ffB�  B�  B���B�ffB�33B�  B�ffB�ffB�33C � CffC�3C�3C� C
� C� C� C� C��C��C��C� C��C��C�3C �3C"��C$ffC&ffC(� C*ffC,� C.� C0� C2� C4� C6ffC8�3C:��C<��C>� C@ffCB�3CD��CF� CH��CJ�3CL��CN��CP�3CR� CT��CV� CXffCZ�3C\� C^L�C`��Cb�3Cd��CfffCh��Cj�3Cl� Cn� CpL�Cr�3Ct��CvL�Cx��Cz��C|�3C~� C�L�C�33C�&fC�@ C�@ C�&fC�L�C�@ C�33C�Y�C�Y�C�33C�L�C�L�C�@ C�L�C�L�C�33C�Y�C�Y�C�33C�Y�C�Y�C�@ C�ffC�Y�C�L�C�@ C�33C�ffC�Y�C�@ C�@ C�&fC�Y�C�L�C�33C�L�C�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�Y�C�ffC�ffC�L�C�&fC�33C�@ C�L�C�@ C�&fC�33C�Y�C�Y�C�L�C�&fC�33C�L�C�Y�C�ffC�Y�C�@ C�Y�C�L�C�@ C�L�C�Y�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�@ C�Y�C�@ C�33C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�Y�C�@ C�&fC�@ C�L�C�@ C�&fC�33C�@ C�Y�C�@ C��C�33C�L�C�Y�C�@ C�33C�33C�@ C�Y�C�L�C�&fC�@ C�Y�C�@ D` D��D�fD
ffD�D� Dl�D,�D� D��DY�D &fD"�3D%��D(,�D*�fD-� D0@ D2�fD5� D83D:��D=&fD?�fDB&fDD�3DGFfDI� DLS3DN��DQy�DT  DV��DY3D[�fD^9�D`� Dc9�De��Dh33Dj� Dm3Dos3Dq�fDt@ Dv��Dy3D{` D}Y�D� D�3D�P D��fD�ٚD�&fD�vfD��3D��D�|�D��3D�@ D�� D�  D�c3D��3D�)�D���D��fD�@ D���D��fD�#3D�c3D���D��fD�3D�I�D�y�D�� D��fD���D� D�33D�P D�i�D���D���D�ɚD���D�	�D�,�D�L�D�i�D���D��3D�� D�fD�9�D�i�D��3D���D��fD��D�FfD�y�DĦfD���D�fD�33D�Y�Dʌ�D˹�D��D��D�L�D�y�DѦfD�� D�3D�6fD�c3Dד3D��3D��3D�&fD�VfD݀ Dެ�D��3D��fD�  D�<�D�` D�3D�fD�� D���D��3D�3D��D��D�0 D�6fD�I�D�P D�Y�D�c3D�l�D�y�D�� D���D�� D��3D�|�D��3D���D���D��3D���E Q�E њEX E�fEc3E� El�E��Ep E��E{3E�3E�E��E	�fE3E#3E��E��EњEh Et�E��E�E8 EVfE�E!�EFfEd�E��EC3E i�E!��E"�fE#њE%|�E&�3E'�3E(� E*` E+nfE,|�E.�E/�E0x E1�fE2ٚE433E5� E6��E7� E8�fE:33E;|�E<� E@�ECfEFVfEI<�EL�3EO~fERњEU��EY�E\�E_h Eb<�Eea�Eh� Ek� EofEr Et�fEx!�E{Y�E~~fE���E�D�E�ݚE�o3E� E��fE�  E��3E�O3E��3E�^fE��3E��3E��3E�3E�o3E���E�fE�W3E��fE��3E�\�E���E���E�<�E��fE��fG�O�?fffG�O�?�  G�O�?fffG�O�?�  G�O�?���G�O�?���G�O�?���G�O�?�ff?�  ?ٙ�?�33@ff@33@   @9��@L��@Y��@s33@�ff@�  @���@�33@�  @���@ə�@�33@�  @���@���A��A��A��A33A#33A+33A333A<��AC33AL��AS33A[33Ad��Al��As33A{33A���A���A���A���A�  A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414141414141414111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ �@ v@ �@ {@ O@ ""@ )�@ 0x@ 7L@ =q@ E�@ Q�@ _�@ l�@ z3@ ��@ �<@ �5@ �~@ �&@ ��@ �t@ �@ ��@@�@�@+�@:@I@V@c�@r�@�@�P@��@��@��@��@�7@�;@��@��@1@�@#�@0x@<�@K@Yn@g�@uk@��@�\@�@��@�@ƨ@խ@�@��@��@	�@B@&;@33@B8@O�@\�@i�@v�@�|@�#@�m@�@��@�c@�
@�`@�@ �@�@�@)�@7�@E�@S�@^�@l�@z�@��@�0@��@�~@�&@��@��@�y@��@@@
@-�@:�@G�@Wb@dZ@qS@�W@�P@��@��@��@��@є@��@�(@��@1@*@!s@0x@>�@K@X�@e	@uk@�d@��@��@��@��@��@�O@��@�@��@
=@�@&;@33@@,@O�@]�@i!@x&@��@��@�@��@�@��@�h@�@�@^@�@[@*S@7L@DD@Q=@a�@n�@z�@��@��@�5@�-@�w@�|@�t@�@��@	�@	@	[@	,`@	;d@	H]@	S�@	b�@	r@	�W@	��@	�H@	��@	�9@	@	��@	��@	�(@	�~@
1@
�@
"�@
.l@
<�@
K�@
Z@
hs@
uk@
��@
��@
��@
��@
�@
�W@
Ӡ@
��@
��@
��@
�@�@%�@33@B8@N�@[z@j@y�@��@�h@�m@��@�@�o@׹@�T@�@  @@O@'�@6�@D�@Q�@^5@l�@z�@��@�0@��@��@��@�*@�t@�m@�@j@o@g@+@:@I@UU@�@*S@t�@��@1@Q�@��@�@/�@z3@�J@�@Z�@�5@��@7L@��@�c@�@X�@�@�@)�@m�@�-@��@>@��@�@@T�@��@�;@$.@j@��@��@9X@}�@�2@j@FQ@�+@�@
=@Lu@��@��@�@DD@��@�c@@S�@��@�@)�@s_@�w@
�@UU@�m@�@7L@�d@��@ �@ dZ@ �@ �@!;d@!�d@!ƨ@"	�@"Lu@"�@"�C@#@#Q�@#��@#ψ@$�@$K�@$��@$Ĝ@%�@%?}@%|�@%�@%��@&5�@&s_@&�!@&�@@'-@'m:@'�@'�@(.l@(m�@(�f@(��@).l@)n�@)�!@)�L@*/@*r@*�-@*��@+2�@+r�@+��@+�e@,5�@,v@,�F@,��@-7L@-x�@-�@-��@.:�@.{�@.�@.��@/=q@/}�@/�j@/��@0:@0v�@0��@0�@11'@1m:@1��@1�`@2g@2X@2�i@2��@3�@3?}@3ww@3�!@3��@4!s@4Z�@4��@4�o@5j@5:�@5l�@5��@5܀@6�@6K�@6�p@6�k@6�@7,`@7e�@7�a@7�
@8�@8FQ@8~K@8��@8�L@9&�@9�<@:> @:�f@;V�@;�W@<qS@<�`@=Yn@>�@>y�@>�4@?��@@{@@��@AFQ@A��@B<�@B��@C3�@C�L@Dm�@D��@Ee�@E��@F�<@Go@G��@H  @H�@I!s@I��@J:�@J��@KG�@K�@LK�@L�;@Mp�@M��@N\)@N�l@On�@O�9@P��@Q��@S2�@T�T@U�@WE�@X�p@Y�@[1�@\�@]�T@_N�@`��@a�#@c/�@d��@e�}@g@�@h}�@i�#@k:�@l�@mލ@o&;@p�@q��@s=q@t��@u�t@w(G@x�@y��@{.l@|�@}�l@~	@~T�@~��@~�<@5?@l�@�1@�}@�%�@�A�@�j(@��v@��@��iG�O�@ �G�O�@ jG�O�@ �G�O�@ jG�O�@ G�O�@ G�O�@ �G�O�@ v@ �@ 1@ 	�@ 
�@ J@ �@ b@ o@ �@ �@ B@ O@ [@ g@ ""@ $�@ '�@ )�@ ,`@ /@ 1�@ 5?@ 8�@ <@ >�@ B8@ E�@ I@ M$@ O�@ S�@ V�@ Z@ ^5@ a�@ dZ@ g�@ k.@ n�@ qS@ t�@ ww@ |?@ ~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��mA��A��A��A��A���A��A��A��A���A�A�%A�%A�1A�A��A��#A�O�A��mAăA¬A�jA��A�1A���A��wA��DA�"�A��#A��A���A��
A�oA��/A�I�A��uA�9XA�t�A��#A�l�A���A��A�;dA��`A�dZA�bA��\A���A��A���A���A��A�7LA�C�A���A�=qA���A�t�A�33A��A��
A��RA�ZA�+A��`A�ȴA��A�A�A��\A�?}A��7A�ĜA�/A���A�/A��FA��#A��TA�K�A�ƨA�33A�&�A��jA��A�l�A�O�A��A�^5A��wA��FA�;dA�\)A��A�JA�A�A�dZA�A��DA�hsA�9XA�%A���A�z�A�-A�bA��A��PA�hsA��\A���A���A�p�A�1'A���A���A��A��9A��A�ƨA��A~ȴA|~�AzĜAvZAr�9AqVAnr�Al1Ah�RAf��Ae��Ad�Ad{AcG�Ab�\Aa&�A^JAZ�AY�FAW/AU��AU%ATn�AT-AR�jAN�HAL��AK��AH�/AGx�AF(�ADVACdZAB5?AA;dA@��A@I�A?�PA>ȴA>��A>��A=�A<�yA< �A:n�A8�RA7K�A6^5A6bA5��A5��A533A4��A4��A4��A4�\A4M�A4  A2ȴA1��A0�!A.�A.5?A,��A+�A*�jA)��A)�A(~�A&�A$��A#��A#p�A#?}A!�A ȴA��A�AbA33AZAJAK�A�A/AƨAx�A�AZAhsA��A�\AA�A7LA�A�AĜA �A|�A�RA�7A
��A
��A
�A
�uA	�A	%AȴA�DA7LA�RAbNA�^AS�A�/AZA�A�A@�
=@�Z@�J@�^5@�@�bN@�r�@��@�V@�{@�+@�Ĝ@�R@�(�@��@Ϯ@�ȴ@Ǯ@���@��@���@���@���@��@�V@��@��m@���@�+@� �@�v�@��@�;d@�1@�1@�9X@�E�@�@��D@�`B@�+@�j@�I�@��F@�ȴ@�x�@��/@� �@�S�@�~�@��7@�b@�|�@�n�@��@��@�"�@�O�@�(�@�@~��@~@{dZ@w�@u@r��@p �@n$�@lz�@kS�@i��@g\)@e�@d1@bJ@a�@`b@]�h@\z�@Z��@Z��@Z~�@Y7L@W�@U��@TZ@S33@R^5@Q%@PQ�@O�@N�y@M�@MV@K�@I�#@I%@F��@F5?@D�@C�
@B�@@Q�@>V@<�j@<I�@;�
@:~�@8r�@7;d@6v�@5`B@3��@2J@0A�@/+@.$�@-V@,9X@*�H@)�@(��@'�w@&ȴ@%�@$�D@#��@#@!hs@!�@ A�@+@��@@�-@�@dZ@J@&�@b@�@��@��@�@�F@��@��@=q@7L@1'@�@��@�h@�@9X@33@	��@	&�@	%@�`@A�@  @�w@�R@@�@�@��@I�@�m@t�@C�@�@��@ ��@ �u@ Q�@ 1'?��?��?�1?�"�?�=q?��?��+?�?��?�7?��`?�O�?�dZ?�=q?�1'?�+?�?��
?�-?��?��?�v�?�/?ܬ?�?�~�?ٙ�?���?��?֧�?��T?���?��/?��
?�t�?ҏ\?щ7?У�?�\)?Η�?�/?̋D?�dZ?�=q?���?ȓu?ǍP?��y?��T?�`B?�z�?��
?���?�  ?���?�{?�/?��?��?�"�?���?�^5?���?���?���?��?��?��?��?���?���?�"�?��?�1?�j?�V?�p�?��?���?��?��;?�bN?�%?�hs?��7?���?���?���?���?���?��7?���?�hs?�G�?�&�?�&�?��7?���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A���A�  A�A�A�A�%A�A�1A�%A�%A�%A�%A�%A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��A��A��mA��A��A��A��A���A��A��A��A���A�A�%A�%A�1A�A��A��#A�O�A��mAăA¬A�jA��A�1A���A��wA��DA�"�A��#A��A���A��
A�oA��/A�I�A��uA�9XA�t�A��#A�l�A���A��A�;dA��`A�dZA�bA��\A���A��A���A���A��A�7LA�C�A���A�=qA���A�t�A�33A��A��
A��RA�ZA�+A��`A�ȴA��A�A�A��\A�?}A��7A�ĜA�/A���A�/A��FA��#A��TA�K�A�ƨA�33A�&�A��jA��A�l�A�O�A��A�^5A��wA��FA�;dA�\)A��A�JA�A�A�dZA�A��DA�hsA�9XA�%A���A�z�A�-A�bA��A��PA�hsA��\A���A���A�p�A�1'A���A���A��A��9A��A�ƨA��A~ȴA|~�AzĜAvZAr�9AqVAnr�Al1Ah�RAf��Ae��Ad�Ad{AcG�Ab�\Aa&�A^JAZ�AY�FAW/AU��AU%ATn�AT-AR�jAN�HAL��AK��AH�/AGx�AF(�ADVACdZAB5?AA;dA@��A@I�A?�PA>ȴA>��A>��A=�A<�yA< �A:n�A8�RA7K�A6^5A6bA5��A5��A533A4��A4��A4��A4�\A4M�A4  A2ȴA1��A0�!A.�A.5?A,��A+�A*�jA)��A)�A(~�A&�A$��A#��A#p�A#?}A!�A ȴA��A�AbA33AZAJAK�A�A/AƨAx�A�AZAhsA��A�\AA�A7LA�A�AĜA �A|�A�RA�7A
��A
��A
�A
�uA	�A	%AȴA�DA7LA�RAbNA�^AS�A�/AZA�A�A@�
=@�Z@�J@�^5@�@�bN@�r�@��@�V@�{@�+@�Ĝ@�R@�(�@��@Ϯ@�ȴ@Ǯ@���@��@���@���@���@��@�V@��@��m@���@�+@� �@�v�@��@�;d@�1@�1@�9X@�E�@�@��D@�`B@�+@�j@�I�@��F@�ȴ@�x�@��/@� �@�S�@�~�@��7@�b@�|�@�n�@��@��@�"�@�O�@�(�@�@~��@~@{dZ@w�@u@r��@p �@n$�@lz�@kS�@i��@g\)@e�@d1@bJ@a�@`b@]�h@\z�@Z��@Z��@Z~�@Y7L@W�@U��@TZ@S33@R^5@Q%@PQ�@O�@N�y@M�@MV@K�@I�#@I%@F��@F5?@D�@C�
@B�@@Q�@>V@<�j@<I�@;�
@:~�@8r�@7;d@6v�@5`B@3��@2J@0A�@/+@.$�@-V@,9X@*�H@)�@(��@'�w@&ȴ@%�@$�D@#��@#@!hs@!�@ A�@+@��@@�-@�@dZ@J@&�@b@�@��@��@�@�F@��@��@=q@7L@1'@�@��@�h@�@9X@33@	��@	&�@	%@�`@A�@  @�w@�R@@�@�@��@I�@�m@t�@C�@�@��@ ��@ �u@ Q�@ 1'?��?��?�1?�"�?�=q?��?��+?�?��?�7?��`?�O�?�dZ?�=q?�1'?�+?�?��
?�-?��?��?�v�?�/?ܬ?�?�~�?ٙ�?���?��?֧�?��T?���?��/?��
?�t�?ҏ\?щ7?У�?�\)?Η�?�/?̋D?�dZ?�=q?���?ȓu?ǍP?��y?��T?�`B?�z�?��
?���?�  ?���?�{?�/?��?��?�"�?���?�^5?���?���?���?��?��?��?��?���?���?�"�?��?�1?�j?�V?�p�?��?���?��?��;?�bN?�%?�hs?��7?���?���?���?���?���?��7?���?�hs?�G�?�&�?�&�?��7?���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A���A�  A�A�A�A�%A�A�1A�%A�%A�%A�%A�%A�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�B
��B;dB� B�B>wBT�BW
BT�BR�BQ�BI�B@�BB�B=qB<jBB�BA�B?}BD�BA�BD�BG�BK�BO�BT�BVBYBW
B[#B_;Bp�Bn�Bk�BjBk�Bk�Bo�By�B�B�B~�Bz�B~�B�B�7B�uB��B��B��B��B��B��B�{B�JB�B~�By�Bt�Bq�BhsBcTB_;BYBD�B.B(�B%�B"�B �B{BBB  B��B�B�B�5B��B��B�!B�B��B��B��B��B��B��Bz�Bk�B]/BI�B.BDBB
�B
�B
�ZB
��B
�3B
��B
�%B
q�B
^5B
K�B
-B
�B

=B	�NB	�
B	ǮB	�3B	��B	�=B	�B	t�B	n�B	jB	cTB	]/B	R�B	;dB	0!B	&�B	�B	uB	JB	DB	1B��B�HB�B��BBBÖBB�jB�wB�qB�qB�jB�}BBƨB��B�;B�sB�fB�;B�)B��B��BɺBȴB��B�#B�/B�)B�)B�)B�)B�#B��B��B��BB��B�^B�?B�'B��B��B��B��B��B��B�{B�oB�DB�=B�%B�B�B{�B{�Bz�Bv�Bt�Bn�Bo�Bl�BiyBdZBdZBbNBaHB^5B]/B]/B\)BT�BZBXBVBR�BT�BW
BYBaHBbNBdZBiyBgmBgmBbNBhsBiyBiyBiyBm�Bn�Bv�By�Bn�B`BBS�BQ�BYBVBR�BbNB`BB;dB2-B7LB49B2-B?}BD�BG�BE�BM�BQ�B\)B^5BcTBjBr�B{�B�B�\B��B�'BŢB�#B�B	1B	bB	�B	$�B	7LB	A�B	I�B	_;B	n�B	�B	�DB	�hB	�B	�^B	�wB	ĜB	ȴB	��B	��B	�)B	�5B	�BB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
1B
JB
PB
VB
bB
uB
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
"�B
#�B
%�B
'�B
)�B
)�B
,B
-B
.B
/B
/B
1'B
2-B
2-B
5?B
7LB
7LB
9XB
9XB
;dB
;dB
=qB
>wB
?}B
A�B
B�B
B�B
D�B
F�B
G�B
G�B
H�B
J�B
L�B
N�B
O�B
O�B
Q�B
Q�B
S�B
S�B
T�B
VB
W
B
XB
ZB
ZB
ZB
\)B
]/B
^5B
_;B
_;B
`BB
`BB
aHB
bNB
dZB
dZB
e`B
e`B
ffB
gmB
iyB
iyB
hsB
jB
k�B
k�B
l�B
m�B
m�B
p�B
o�B
p�B
r�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
x�B
y�B
y�B
z�B
y�B
|�B
|�B
|�B
}�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�DB
�JB
�PB
�\B
�\B
�hB
�hB
�oB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�qB
�qB
�jB
�jB
�B
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�sB
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�B
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�B
�B
�yB
�yB
�B
�yB
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B
�yB
�yB
�B
�yB
�yB
�yB
�yB
�yB
�yB
�B
�yB
�B
�yB
�yB
�yB
�sB
�yB
�B
�B
��B:^B~�B�B=qBS�BVBS�BQ�BP�BH�B?}BA�B<jB;dBA�B@�B>wBC�B@�BC�BF�BJ�BN�BS�BT�BXBVBZB^5Bo�Bm�BjBiyBjBjBn�Bx�B�B� B}�By�B}�B� B�1B�oB��B��B��B��B��B��B�uB�DB�B}�Bx�Bs�Bp�BgmBbNB^5BXBC�B-B'�B$�B!�B�BuBB  B��B��B�B�yB�/B��B��B�B��B��B��B��B��B��B��By�BjB\)BH�B-B
=B  B
�B
�B
�TB
��B
�-B
��B
�B
p�B
]/B
J�B
,B
�B
	7B	�HB	�B	ƨB	�-B	��B	�7B	� B	s�B	m�B	iyB	bNB	\)B	Q�B	:^B	/B	%�B	�B	oB	DB	
=B	+B��B�BB�
B��B��B��BB��B�dB�qB�jB�jB�dB�wB��BŢB��B�5B�mB�`B�5B�#B��BɺBȴBǮBɺB�B�)B�#B�#B�#B�#B�B��B��B��B��B��B�XB�9B�!B��B��B��B��B��B�{B�uB�hB�=B�7B�B�B� Bz�Bz�By�Bu�Bs�Bm�Bn�Bk�BhsBcTBcTBaHB`BB]/B\)B\)B[#BS�BYBW
BT�BQ�BS�BVBXB`BBaHBcTBhsBffBffBaHBgmBhsBhsBhsBl�Bm�Bu�Bx�Bm�B_;BR�BP�BXBT�BQ�BaHB_;B:^B1'B6FB33B1'B>wBC�BF�BD�BL�BP�B[#B]/BbNBiyBq�Bz�B�B�VB��B�!BĜB�B�B	+B	\B	�B	#�B	6FB	@�B	H�B	^5B	m�B	�B	�=B	�bB	�B	�^B	�wB	ĜB	ȴB	��B	��B	�)B	�5B	�BB	�mB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
1B
JB
PB
VB
bB
uB
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
"�B
#�B
%�B
'�B
)�B
)�B
,B
-B
.B
/B
/B
1'B
2-B
2-B
5?B
7LB
7LB
9XB
9XB
;dB
;dB
=qB
>wB
?}B
A�B
B�B
B�B
D�B
F�B
G�B
G�B
H�B
J�B
L�B
N�B
O�B
O�B
Q�B
Q�B
S�B
S�B
T�B
VB
W
B
XB
ZB
ZB
ZB
\)B
]/B
^5B
_;B
_;B
`BB
`BB
bNB
cTB
e`B
e`B
ffB
ffB
gmB
hsB
jB
jB
iyB
k�B
l�B
l�B
m�B
n�B
n�B
q�B
p�B
q�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
z�B
{�B
z�B
}�B
}�B
}�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�+B
�1B
�7B
�7B
�JB
�PB
�VB
�bB
�bB
�oB
�oB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�9B
�FB
�LB
�RB
�XB
�^B
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�jB
�qB
�qB
�wB
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
��B
��B
��B
�}B
��B
��B
��B
��B
��B
��B
�yB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�mB
�yB
�yB
�B
�yB
�yB
�B
�sB
�yB
�yB
�yB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�yB
�yB
�yB
�B
�yB
�B
�yB
�yB
�yB
�sB
�yB
�yB
�sB
�sB
�yB
�sB
�yB
�yB
�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202322021061413521020210614135210202106141746192021061417461920210614174619201807242202322021061413521020210614135210202106141746192021061417461920210614174619PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023220180724220232  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023220180724220232QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023220180724220232QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                