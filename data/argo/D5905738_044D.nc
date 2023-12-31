CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-22T13:00:36Z creation      
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
resolution        =���   axis      Z        h  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   LH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  Pd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  uP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Ӭ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    \   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        |   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20181122130036  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ,   ,DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؐ3vm�@ؐ3vm�11  @ؐ3*�@ؐ3*�@5�T���^@5�T���^�c�;�D=G�c�;�D=G11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?L��@   @@  @�  @�33@�33@���A   A33A&ffAC33Aa��A�  A���A���A���A���A���A�33A�  B ffBffBffBffB ffB(ffB0ffB8  B@  BH  BP  BW��B`  Bh��BpffBx  B�  B�  B�  B���B�33B�ffB�  B�33B�33B�  B���B���B�  B�  B�33B�33B�33B�ffB�33B�ffB�ffB�ffB���B���B���B���B���B���B�  B�  B�  B�33C 33C�C�C�C33C
�C�C33CL�C�fC�fC�fC  C�fC  C  C   C"  C#�fC&33C(33C*�C,�C-�fC033C2�C4  C6  C7�fC:33C<�C=�fC@33CB�CD  CFL�CH33CJ�CL  CM�fCP33CR�CT  CV  CW��CZ�C\�C]�fC`33Cb  Cc�fCf33Ch�Cj  Cl�Cn�Cp  CrL�Ct�Cv  Cx  Cz  C|  C}�fC�fC�  C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��3C�  C�  C�  C�&fC��C��C��C�  C�  C�  C��3C��fC��C�&fC��C��3C�  C��C��C��C�&fC�  C�ٚC��fC��3C�  C��C��C��C��C�  C�ٚC��fC��fC��fC��3C��3C�  C�  C�  C��C��C�  C�  C��C��fC�  C��C��C��C��3C��C�  C��fC�  C�  C��3C�  C��C��C��3C��C��C��C��fC��3C��C��C�&fC��C��fC�  C�  C��C�&fC�  C��fC�  C��C��C��C��C��C�&fC��C��fC��3C��3C��C��C�&fC�33C��C��fC��3C�  C��C��C��C��C��C��C�&fC�  C��fC��fC��3C�  C��C�  C��C��D 3D �3D�D��D��DFfD�D	� D,�D��Ds3D3D�3DS3D��D��D!Y�D#��D&�fD)�D+��D.�D0��D3�D5��D8fD:` D<�fD?33DA� DCٚDF33DH�fDK�DMy�DO��DR�DTY�DV��DX�fD[,�D]�fD_� DbFfDd��Dg  DiY�Dk�3Dn  DpS3Dr�fDt�fDw@ Dy�fD{` D}�3D�  D�0 D�ffD���D��fD���D�,�D�ffD���D��3D� D�C3D�� D���D���D�,�D�c3D���D��3D�	�D�6fD�` D��3D�ɚD���D�#3D�I�D�l�D���D���D��fD��D�@ D�i�D�� D���D�� D�	�D�,�D�\�D�|�D���D���D�ٚD���D��D�<�D�c3D���D�� D�� D��D��D�&fD�I�D�ffD��3D3Dé�DĶfD��fD��3D��3D��3D���D���D�fD�fD� D��D�#3D�0 D�6fD�I�D�\�D�s3D։�Dל�Dذ D��fD�ٚD���D��D�@ D�Y�D�s3D��D� D�fD��fD�ٚD��3D���D� D��D�)�D�6fD�I�D�S3D�\�D�c3D�ffD�y�D�|�D�D��fD��3D���D��fD���D���D��fD��fD�� D���D��3E {3EfE��EfE��E$�E33E@ E�3E�E�fE
� E�fE��EP Ed�E~fE� E8 EH E[3E�3E� E��E�3E�E3E|�Et�E �3E!њE#6fE$��E%�fE&��E(T�E)�fE*��E,#3E-�E.~fE/�3E0ٚE2H E3�3E4�fE6fE73E8�fE9�fE:�3E<@ E?|�EB�fEEx EH�fEK�EN� ER,�EUc3EX|�E[|�E^x EaњEd�fEg� Ej��En3EqI�Etx Ew�fEz|�E}��E�\ E� E���E�fE��fE�8�E��3E��E�]�E���E� E�jfE�� E��E�P�E��3E���E�6fE���?��>���?   ?   ?   ?��?��?��?��?333?333?333?333?fff?fff?�  ?���?���?�33?���?ٙ�?�33@��@��@,��@@  @S33@fff@�  @���@�ff@�33@���@���@ə�@�ff@�33@�33A   A  A  AffAffA(  A.ffA8  A>ffAFffAL��AT��A^ffAd��Al��As33A|��A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414441444141141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?�  ?�ff@   @`  @�  @�33@�33@���A  A33A.ffAK33Ai��A�  A���A���A���A���A���A�33A�  BffB
ffBffBffB"ffB*ffB2ffB:  BB  BJ  BR  BY��Bb  Bj��BrffBz  B�  B�  B�  B���B�33B�ffB�  B�33B�33B�  B���B���B�  B�  B�33B�33B�33B�ffB�33B�ffB�ffB�ffB���B���B���B���B���B���B�  B�  B�  B�33C �3C��C��C��C�3C
��C��C�3C��CffCffCffC� CffC� C� C � C"� C$ffC&�3C(�3C*��C,��C.ffC0�3C2��C4� C6� C8ffC:�3C<��C>ffC@�3CB��CD� CF��CH�3CJ��CL� CNffCP�3CR��CT� CV� CXL�CZ��C\��C^ffC`�3Cb� CdffCf�3Ch��Cj� Cl��Cn��Cp� Cr��Ct��Cv� Cx� Cz� C|� C~ffC�33C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�ffC�33C�@ C�@ C�@ C�ffC�Y�C�Y�C�L�C�@ C�@ C�@ C�33C�&fC�L�C�ffC�L�C�33C�@ C�L�C�L�C�Y�C�ffC�@ C��C�&fC�33C�@ C�L�C�Y�C�L�C�Y�C�@ C��C�&fC�&fC�&fC�33C�33C�@ C�@ C�@ C�L�C�Y�C�@ C�@ C�L�C�&fC�@ C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�@ C�33C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�33C�L�C�Y�C�ffC�L�C�&fC�@ C�@ C�L�C�ffC�@ C�&fC�@ C�L�C�L�C�L�C�Y�C�Y�C�ffC�L�C�&fC�33C�33C�L�C�L�C�ffC�s3C�L�C�&fC�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�@ C�&fC�&fC�33C�@ C�L�C�@ C�Y�C�Y�D 33D �3D9�D��D�DffD9�D	� DL�D��D�3D33D�3Ds3D�D��D!y�D$�D&�fD),�D+��D.9�D0��D39�D5��D8&fD:� D<�fD?S3DA� DC��DFS3DH�fDK9�DM��DO��DR,�DTy�DV��DYfD[L�D]�fD`  DbffDd��Dg  Diy�Dk�3Dn  Dps3Dr�fDufDw` Dy�fD{� D}�3D� D�@ D�vfD���D��fD��D�<�D�vfD���D��3D�  D�S3D�� D���D�	�D�<�D�s3D���D��3D��D�FfD�p D��3D�ٚD�	�D�33D�Y�D�|�D���D�ɚD��fD��D�P D�y�D�� D���D�� D��D�<�D�l�D���D���D�ɚD��D��D�,�D�L�D�s3D���D�� D�� D���D��D�6fD�Y�D�vfD��3D£3Dù�D��fD��fD��3D��3D�3D��D��D�fD�fD�  D�)�D�33D�@ D�FfD�Y�D�l�DՃ3D֙�D׬�D�� D��fD��D��D�,�D�P D�i�D��3D��D� D��fD��fD��D��3D�	�D�  D�,�D�9�D�FfD�Y�D�c3D�l�D�s3D�vfD�D��D���D��fD��3D���D��fD�ɚD���D��fD��fD�� D���D��3E �3EfE��EfE��E,�E;3EH E�3E�E�fE
� E�fE��EX El�E�fE� E@ EP Ec3E�3E  E��E�3E	�E3E��E|�E �3E!ٚE#>fE$��E&fE&��E(\�E)�fE*��E,+3E-�E.�fE/�3E0�E2P E3�3E4�fE6&fE73E8�fE9�fE:�3E<H E?��EB�fEE� EH�fEK�EN� ER4�EUk3EX��E[��E^� EaٚEd�fEh  Ej��En3EqQ�Et� Ew�fEz��E}��E�` E� E���E�fE��fE�<�E��3E��E�a�E���E� E�nfE�� E��E�T�E��3E���E�:fE���G�O�?fffG�O�G�O�?�  G�O�G�O�G�O�?���G�O�G�O�G�O�?���G�O�?�33?�  G�O�?ٙ�?�33@ff@��@��@,��@9��@L��@`  @s33@�33@�  @���@�ff@�33@���@���@ٙ�@�ff@�33A��A  A  A  AffA&ffA0  A6ffA@  AFffANffAT��A\��AfffAl��At��A{33A�ffA���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414414441444141141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ j@ v@ �@ {@ O@ "�@ )�@ /@ 6�@ >�@ F�@ SI@ `B@ m:@ {�@ �7@ ��@ ��@ �-@ �w@ ��@ �#@ ��@ �q@@�@g@-@:@G�@UU@b�@o�@~K@�P@�H@�A@��@@�7@�/@�4@��@�@*@"�@/�@<@Ji@X�@ff@t�@�d@�@�a@�Y@��@�W@��@��@�@@��@	�@6@$�@33@@�@N�@\�@k.@x&@��@�u@��@��@�k@��@�@�@�@�Q@�@�@(�@6�@DD@Q�@^�@n�@|?@�7@��@�(@��@��@��@�t@�m@� @@b@ @-@:@I�@V�@c�@p�@}�@�P@�H@�A@��@�2@��@ލ@��@��@�@�@#�@0x@=q@K�@Yn@ff@v@�d@�\@�@��@�R@�J@��@�H@�@�E@
�@�@&;@3�@B8@O0@\�@j@x&@��@��@��@�@��@�c@�@�@�@ �@�@O@(�@5�@B�@R�@a�@m�@z3@��@��@��@��@�2@��@�h@�@�@	j@	�@	 @	-@	;d@	G�@	SI@	a�@	oF@	|�@	�D@	��@	�A@	��@	@	��@	�;@	�@	�,@
�@
@
""@
0x@
>�@
K�@
X@
g@
t@
�W@
�\@
�@
��@
�R@
�W@
�O@
��@
�@
��@
�@�@$�@3�@B8@P�@\�@hs@ww@�@�u@�y@�@�^@�c@׹@�`@�@^@@[@)�@5?@C�@Q=@`B@m�@|�@�D@��@�y@��@�&@�|@�#@�y@� @�@o@ �@,`@8�@FQ@T�@b�@qS@~K@�P@��@�M@��@�J@��@��@�`@i!@�@�@;d@��@�o@@Z�@�(@��@5�@}�@�>@1@Lu@�@�\@�@^�@�@�H@"�@e	@��@�@$.@g@��@��@*S@g�@��@�@"�@`�@�@�H@"�@dZ@��@�@$.@b�@�y@��@g@_�@��@�7@�@N�@�\@є@@SI@��@�\@B@Z�@��@�H@"�@ff@��@�@/@qS@��@�q@ 8�@ x�@ �R@ ��@!<@!|�@!�j@!�9@"9X@"v�@"��@"� @#5�@#ww@#��@#��@$5�@$t@$��@$�@%2�@%o�@%��@%�(@&'�@&e�@&�(@&��@'g@'^5@'�@'�t@(�@(T�@(��@(��@)�@)H]@)�d@)��@)� @*1'@*j@*��@*ލ@+6@+M�@+�|@+�@+��@,.l@,g@,�m@,�h@-@-M�@-�7@-Ĝ@-�Q@.:@.uk@.�!@.�@@/+�@/i�@/��@/��@0
@0X�@0�#@0�*@1�@1A�@1|�@1�R@1�@2+@2dZ@2�@2׹@3b@3H]@3�@3�^@3�@4+@4dZ@4�H@4�C@5�@5FQ@5z3@5��@5��@6%�@6^�@6��@6є@7�@7FQ@7�@7�^@7�e@8g�@8�t@9�|@9��@:l�@;O@;�@<v@<�R@=.l@=��@>�@>�7@?DD@?��@@g�@@�
@A~�@A��@B�@B��@C�@D�@D��@E
�@E�(@F<@F��@G8�@G�C@Ho�@Hխ@Ir@I׹@Js_@K�@Kt�@L@L�r@M6@M�9@N�@N�@OT�@O��@PQ�@Q��@S�@T@�@U��@V�E@X@�@Y��@[�@\^5@]��@^�@`Yn@a��@b�,@d<@e�#@f�@hN�@i��@j��@lB�@m��@n�,@pA�@q��@r�'@t> @u��@u�h@vo@vj@v��@v��@w/�@wg�@w�k@w�@xLv@x�W@xєG�O�@ �G�O�G�O�@ jG�O�G�O�G�O�@ G�O�G�O�G�O�@ �G�O�@ %@ �G�O�@ 1@ 	�@ 
�@ �@ �@ @ b@ o@ {@ �@ �@ O@ [@  @ "�@ $�@ (G@ +@ -�@ 0x@ 3�@ 6�@ :@ =q@ @,@ C�@ G�@ Ji@ N�@ Q=@ T�@ Wb@ Z�@ ^�@ a�@ e	@ g�@ k�@ n�@ r@ uk@ x�@ {�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�oA�{A��A�"�A�"�A�$�A�&�A�+A�+A�+A�-A�+A�+A�33A�+A�bA�JA��A�&�A�I�A�\)A�E�A�5?A�5?A�33A�=qA�I�A��mAϲ-Aϣ�A�z�A�VA�&�A�1'A˩�Aʉ7A�5?A�5?A�VA�&�A�7LA�K�A�1'A�ƨA�bA�dZA�A��/A��A�bNA��7A�$�A�ffA�A�oA�dZA�  A�A���A��jA��7A��#A��hA��A��DA���A�1A���A�~�A�v�A�M�A�I�A�I�A��A�$�A��;A�p�A��A��hA��mA�t�A�bA��RA�A�A��A���A��+A�x�A�n�A�?}A���A���A�bA���A�p�A�(�A�
=A��uA�&�A�z�A�x�A���A�
=A�C�A�jA�?}A�VA�VA���A��!A��
A�-A�l�A��`A�&�A���A���A�"�A|~�Ay
=Axn�Ax5?AwO�At(�Ar�Ar^5Aq`BAnJAk\)Ah �Ae�AdȴAd  Ab��A_�A]�mA[�mAZ�HAY�AYK�AW�#AU\)AS�AP  AN(�AMt�ALn�AK�-AJbAHE�AF1AD��ADZAB�HAAdZA?�mA>�yA=G�A;�-A:-A9G�A7G�A5K�A4(�A3�wA2�A/VA-�A+�7A+A*~�A*VA*5?A)ƨA(�9A'A'K�A%x�A$�A#33A"��A!�A�DA�9An�A5?AAx�A/A�!AJA�+A"�A�/Ar�A�-A��AbNAJA��A��Ap�AS�A"�A�A��A�AM�AVA
=AA
��A	A	t�A�hAVA&�Ar�A  A�
A+A ��A A�@��y@���@�^5@�%@�z�@��F@��R@�$�@�  @�Ĝ@���@�dZ@���@��@��@��@�=q@��@�w@���@噚@�r�@�@�?}@�1'@݁@ٲ-@֧�@�-@��@��@��T@�K�@��@ѡ�@�j@�ȴ@��@͙�@�C�@�@ɩ�@��@�Ĝ@���@���@�dZ@�-@��H@� �@��@�V@��@�=q@�&�@��w@�l�@��\@���@�I�@��+@�$�@�Ĝ@��+@�r�@�33@���@��+@�^5@���@�  @�S�@���@���@�Ĝ@��@��@�1@�+@���@�A�@�ȴ@��T@���@�~�@�@��T@��@� �@�33@���@��^@��^@��@��@l�@}��@{t�@y�@v�y@u�@sS�@pr�@m��@j��@jJ@hA�@g
=@d�@co@`A�@^$�@\�D@[�F@Z��@XA�@X �@Xb@VV@V@U�T@S�m@St�@Q�^@O�@N��@MO�@L��@K@I��@IX@H�9@Fv�@DZ@C33@B�!@?�;@>V@<z�@<(�@:�@:=q@9%@8b@7�P@5@4I�@3o@2~�@1��@0�u@/��@.5?@-�T@,z�@)��@)�#@(��@&�y@%V@#o@"�\@"^5@!��@!�@ r�@�@��@V@�/@j@(�@ƨ@n�@&�@b@�@@@O�@�j@��@dZ@o@��@^5@��@�@��@@I�@I�@�@dZ@
n�@
=q@	�^@	X@	&�@�P@�R@5?@�@V@�@�@�@�H@~�@M�@X@&�@ �`@ �9@ �u?��w?�/?���?�r�?�$�?�t�?��?�\)?��?�~�?�x�?��?�ȴ?��T?�F?�hs?��?�\)?�p�?��H?��#?�7L?�1'?�ȴ?֧�?��/?���?�33?���?�%?�  ?�|�?�{?͑h?��?�I�?˥�?�?��#?�7L?�r�?��?�+?�K�?�ȴ?Ƈ+?Ł?�9X?�o?�J?�  ?���?���?���?�V?�I�?��m?�1?�C�?���?�^5?��#?��^?���?���?��?�^5?���?�dZ?��m?�j?�V?��-?�v�?���?��R?��R?��?��?�;d?�\)?�|�?�|�?���?��w?��;A��A��A��A��A��A��A��A�{A��A�{A�oA�1A�1A�JA�VA�VA�bA�oA�oA�bA�bA�oA��A��A�{A�"�A�&�A�$�A�"�A�"�A�"�A�$�A�"�A�"�A�$�A�$�A�(�A�(�A�+A�+A�+A�+A�(�A�+A�+A�(�A�-A�-A�-A�+A�+A�(�A�+A�(�A�+A�+A�(�A�+A�1'A�5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A��A�oA�{A��A�"�A�"�A�$�A�&�A�+A�+A�+A�-A�+A�+A�33A�+A�bA�JA��A�&�A�I�A�\)A�E�A�5?A�5?A�33A�=qA�I�A��mAϲ-Aϣ�A�z�A�VA�&�A�1'A˩�Aʉ7A�5?A�5?A�VA�&�A�7LA�K�A�1'A�ƨA�bA�dZA�A��/A��A�bNA��7A�$�A�ffA�A�oA�dZA�  A�A���A��jA��7A��#A��hA��A��DA���A�1A���A�~�A�v�A�M�A�I�A�I�A��A�$�A��;A�p�A��A��hA��mA�t�A�bA��RA�A�A��A���A��+A�x�A�n�A�?}A���A���A�bA���A�p�A�(�A�
=A��uA�&�A�z�A�x�A���A�
=A�C�A�jA�?}A�VA�VA���A��!A��
A�-A�l�A��`A�&�A���A���A�"�A|~�Ay
=Axn�Ax5?AwO�At(�Ar�Ar^5Aq`BAnJAk\)Ah �Ae�AdȴAd  Ab��A_�A]�mA[�mAZ�HAY�AYK�AW�#AU\)AS�AP  AN(�AMt�ALn�AK�-AJbAHE�AF1AD��ADZAB�HAAdZA?�mA>�yA=G�A;�-A:-A9G�A7G�A5K�A4(�A3�wA2�A/VA-�A+�7A+A*~�A*VA*5?A)ƨA(�9A'A'K�A%x�A$�A#33A"��A!�A�DA�9An�A5?AAx�A/A�!AJA�+A"�A�/Ar�A�-A��AbNAJA��A��Ap�AS�A"�A�A��A�AM�AVA
=AA
��A	A	t�A�hAVA&�Ar�A  A�
A+A ��A A�@��y@���@�^5@�%@�z�@��F@��R@�$�@�  @�Ĝ@���@�dZ@���@��@��@��@�=q@��@�w@���@噚@�r�@�@�?}@�1'@݁@ٲ-@֧�@�-@��@��@��T@�K�@��@ѡ�@�j@�ȴ@��@͙�@�C�@�@ɩ�@��@�Ĝ@���@���@�dZ@�-@��H@� �@��@�V@��@�=q@�&�@��w@�l�@��\@���@�I�@��+@�$�@�Ĝ@��+@�r�@�33@���@��+@�^5@���@�  @�S�@���@���@�Ĝ@��@��@�1@�+@���@�A�@�ȴ@��T@���@�~�@�@��T@��@� �@�33@���@��^@��^@��@��@l�@}��@{t�@y�@v�y@u�@sS�@pr�@m��@j��@jJ@hA�@g
=@d�@co@`A�@^$�@\�D@[�F@Z��@XA�@X �@Xb@VV@V@U�T@S�m@St�@Q�^@O�@N��@MO�@L��@K@I��@IX@H�9@Fv�@DZ@C33@B�!@?�;@>V@<z�@<(�@:�@:=q@9%@8b@7�P@5@4I�@3o@2~�@1��@0�u@/��@.5?@-�T@,z�@)��@)�#@(��@&�y@%V@#o@"�\@"^5@!��@!�@ r�@�@��@V@�/@j@(�@ƨ@n�@&�@b@�@@@O�@�j@��@dZ@o@��@^5@��@�@��@@I�@I�@�@dZ@
n�@
=q@	�^@	X@	&�@�P@�R@5?@�@V@�@�@�@�H@~�@M�@X@&�@ �`@ �9@ �u?��w?�/?���?�r�?�$�?�t�?��?�\)?��?�~�?�x�?��?�ȴ?��T?�F?�hs?��?�\)?�p�?��H?��#?�7L?�1'?�ȴ?֧�?��/?���?�33?���?�%?�  ?�|�?�{?͑h?��?�I�?˥�?�?��#?�7L?�r�?��?�+?�K�?�ȴ?Ƈ+?Ł?�9X?�o?�J?�  ?���?���?���?�V?�I�?��m?�1?�C�?���?�^5?��#?��^?���?���?��?�^5?���?�dZ?��m?�j?�V?��-?�v�?���?��R?��R?��?��?�;d?�\)?�|�?�|�?���?��w?��;A��A��A��A��A��A��A��A�{A��A�{A�oA�1A�1A�JA�VA�VA�bA�oA�oA�bA�bA�oA��A��A�{A�"�A�&�A�$�A�"�A�"�A�"�A�$�A�"�A�"�A�$�A�$�A�(�A�(�A�+A�+A�+A�+A�(�A�+A�+A�(�A�-A�-A�-A�+A�+A�(�A�+A�(�A�+A�+A�(�A�+A�1'A�5?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B�}B�}B�}B�}B�}B��B��BB{B�B�B�B%�B(�B%�B%�B&�B)�B1'B9XB:^B=qB;dB<jB=qB<jB:^BI�BS�BS�BM�BZBm�BffB[#B^5Be`Bp�Bo�Br�Bt�By�B|�B{�B�B�%B�7B�DB�oB�oB�bB�hB�PB�DB�bB�\B�VB�VB�=B�=B�=B�1B�+B�Bs�Bo�BjBaHBS�BN�BJ�BB�B6FB49B1'B.B%�B�B�B�B�B�BbBB�B�BB�/B��B��B��BƨB��B�B�DB�Bp�BN�B:^B49B.B�B
�B
�B
�9B
��B
�PB
n�B
O�B
B�B
49B
'�B
  B	�B	�ZB	�NB	�B	B	�XB	�-B	��B	�JB	~�B	e`B	\)B	VB	N�B	B�B	.B	)�B	�B	 �B	�B	�B	uB	VB	B�B�B�mB�ZB��B	B	B��B��B�B�mB�5B�B��BB�XB�LB�B��B��B�{B�hB�=Bw�Bw�Bo�Bm�Bl�Bm�Bm�BjBffBcTB`BB[#BW
BVBQ�BN�BB�BH�BE�BB�BB�BA�B?}B>wB;dB6FB:^B7LB6FB49B49B33B33B33B2-B2-B1'B1'B/B/B/B/B-B.B.B,B/B-B&�B.B0!B.B0!B/B.B0!B/B0!B.B/B1'B1'B/B0!B/B-B1'B0!B0!B/B/B/B.B33B1'B5?B6FB7LB7LB:^B:^B<jB:^B;dBB�BA�B@�B?}B>wB?}BB�B@�B?}B?}BB�BA�BD�BG�BG�BI�BYBffBm�Bq�Bw�B�B�oB��B�XBȴB�B��B	B	1B	oB	�B	6FB	H�B	L�B	t�B	q�B	}�B	�B	�+B	�uB	��B	��B	��B	�B	�B	�3B	�^B	ĜB	ɺB	��B	��B	��B	�B	�HB	�BB	�;B	�fB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
	7B
DB
JB
bB
oB
�B
�B
�B
�B
�B
�B
!�B
#�B
%�B
&�B
'�B
)�B
(�B
(�B
,B
,B
,B
/B
.B
0!B
2-B
33B
49B
5?B
7LB
8RB
8RB
9XB
<jB
>wB
?}B
>wB
A�B
C�B
E�B
F�B
F�B
G�B
H�B
I�B
I�B
L�B
L�B
M�B
N�B
N�B
O�B
P�B
Q�B
Q�B
S�B
VB
S�B
VB
W
B
ZB
]/B
\)B
]/B
^5B
^5B
^5B
aHB
bNB
cTB
cTB
dZB
cTB
cTB
ffB
ffB
hsB
iyB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
l�B
l�B
l�B
n�B
p�B
p�B
r�B
r�B
r�B
s�B
t�B
t�B
s�B
t�B
t�B
v�B
w�B
x�B
y�B
y�B
z�B
y�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
~�B
� B
�B
�B
�B
�B
�+B
�1B
�7B
�DB
�DB
�VB
�\B
�bB
�hB
�hB
�uB
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
��B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�3B
�9B
�?B
�FB
�?B
�LB
�RB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�jB
�jB
�jB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BBB��B�}B��B��B�wB��B��B�}B��B�}B��B��B�}B��B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�}B�wB�}B�}B�}B�}B�}B�}B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B�qB�qB�wB�qB�qB�qB�qB�qB�jB�jB�jB�jB�jB�qB��B��BhBoB�B�B"�B%�B"�B"�B#�B&�B.B6FB7LB:^B8RB9XB:^B9XB7LBF�BP�BP�BJ�BW
BjBcTBXB[#BbNBm�Bl�Bo�Bq�Bv�By�Bx�B�B�B�%B�1B�\B�\B�PB�VB�=B�1B�PB�JB�DB�DB�+B�+B�+B�B�B� Bp�Bl�BgmB^5BP�BK�BG�B?}B33B1'B.B+B"�B�B�B�B�B�BVBB�B�5B�#B��B��B��BĜB�wB�B�7B� Bn�BL�B8RB2-B,BuB
�B
��B
�-B
��B
�DB
l�B
M�B
@�B
2-B
%�B	��B	�B	�NB	�BB	�B	��B	�LB	�!B	��B	�=B	|�B	cTB	ZB	S�B	L�B	@�B	,B	'�B	�B	�B	�B	�B	hB	JB��B�B�B�`B�NB�B	B	  B��B�B�B�`B�)B�B��B��B�LB�?B�B��B��B�oB�\B�1Bu�Bu�Bm�Bk�BjBk�Bk�BhsBdZBaHB^5BYBT�BS�BO�BL�B@�BF�BC�B@�B@�B?}B=qB<jB9XB49B8RB5?B49B2-B2-B1'B1'B1'B0!B0!B/B/B-B-B-B-B+B,B,B)�B-B+B$�B,B.B,B.B-B,B.B-B.B,B-B/B/B-B.B-B+B/B.B.B-B-B-B,B1'B/B33B49B5?B5?B8RB8RB:^B8RB9XB@�B?}B>wB=qB<jB=qB@�B>wB=qB=qB@�B?}BB�BE�BE�BG�BW
BdZBk�Bo�Bu�B�B�bB��B�LBƨB�B��B	B	%B	bB	�B	49B	F�B	J�B	r�B	o�B	{�B	~�B	�B	�hB	��B	��B	��B	��B	�B	�'B	�RB	B	ǮB	ȴB	��B	��B	�B	�;B	�5B	�/B	�ZB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
1B

=B
DB
\B
hB
�B
�B
�B
�B
�B
�B
 �B
"�B
$�B
%�B
&�B
(�B
'�B
'�B
+B
+B
+B
.B
-B
/B
1'B
2-B
33B
49B
6FB
7LB
7LB
8RB
;dB
=qB
>wB
=qB
@�B
B�B
D�B
E�B
E�B
F�B
G�B
H�B
H�B
K�B
K�B
L�B
M�B
M�B
N�B
O�B
P�B
P�B
R�B
T�B
R�B
T�B
VB
YB
\)B
[#B
\)B
]/B
]/B
]/B
`BB
aHB
bNB
bNB
cTB
bNB
bNB
e`B
e`B
gmB
hsB
gmB
hsB
hsB
hsB
jB
jB
jB
k�B
k�B
k�B
m�B
o�B
o�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
v�B
w�B
x�B
y�B
y�B
z�B
y�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
~�B
� B
�B
�B
�B
�B
�+B
�1B
�7B
�DB
�DB
�VB
�\B
�bB
�hB
�hB
�uB
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
�B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�3B
�9B
�?B
�FB
�LB
�FB
�RB
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�qB
�wB
�wB
�qB
�qB
�qB
�qB
�wB
�wB
�}B
�}B�qB�qB�qB�qB�wB�qB�wB�wB�qB�qB�wB�wB�qB�wB�qB�qB�wB�qB�qB�qB�}B�}B�qB�jB�wB�qB�dB�qB�qB�jB�qB�jB�qB�qB�jB�qB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�jB�dB�jB�jB�jB�jB�jB�jB�qB�qB�qBȴBɺG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811221300362021061413525720210614135257202106141746552021061417465520210614174655201811221300362021061413525720210614135257202106141746552021061417465520210614174655PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018112213003620181122130036  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112213003620181122130036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112213003620181122130036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                