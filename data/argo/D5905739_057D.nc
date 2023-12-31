CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-22T05:26:33Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   |   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    @   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        `   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190122052633  20210617131513  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               9   9DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؜�3���@؜�3���11  @؜�`��@؜�`��@67�+j��@67�+j���c�r�
��c�r�
�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@ff@Fff@�  @�33@�33@�  A   A��A(  AA��Aa��A���A���A���A���A�ffAљ�A���A���B   B��B��BffB ��B(��B0ffB8  B@  BG��BO��BX  B`  Bg��BpffBx��B�33B�33B�ffB�33B�33B�  B�33B�33B�33B�ffB�ffB�ffB�ffB�33B�33B�  B���B�  B���B�ffB�33B�33B�  B�  Bߙ�B�  B�33B�33B�ffB�ffB���B�33B���C  C33C�C�fC
�C�C�fC�CL�C33C  C33CL�CL�C  C 33C"�C#�fC&  C(33C*�C+��C-�fC0�C233C4L�C633C7�fC:�C<33C>33C@33CB  CC��CE�fCH  CJ�CL33CNL�CP  CQ��CS��CU�fCX  CZ33C\L�C^�C_��Ca�fCd  Cf33Ch  Ci��Cl  Cn33Cp  Cq�fCt�Cu�fCw��Cz  C|33C~�C�fC�&fC��C�  C�&fC��C�  C��C��C��3C�  C��C�&fC��C�  C��C��C��C��3C�  C�  C��C��C��C��C�&fC�  C��fC��fC��3C��3C��3C�  C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��3C��3C�  C��C��C��C��C��C��C�&fC�&fC��C��3C��3C��C��C��C�33C��C�  C��C��C��fC��C�&fC��C��C��C��C�  C��C��C��3C��C�&fC��C��3C��C�  C��fC��C��3C��fC�  C�&fC��C�  C��C��C��3C�  C��C�  C��3C�  C��C�  C��fC�  C�  C�&fC��C��3C�  C��C��C��3C��3C�  C��C��C�33C�ٚD FfD&fD� D
fD�3D�D�fD&fD��D@ D�fD��D!,�D#�fD&�fD)` D,  D.�3D1� D4ffD7�D9�3D<9�D>��DAffDC�fDFffDH� DKL�DM�fDP  DRffDT�3DW,�DY�fD[��D^@ D`�fDc�De��Dh3Dj��Dm  Do��Dr@ Dt��DwY�Dz�D|9�D~ٚD���D�3D�S3D��3D��3D�<�D�p D��fD�ٚD�	�D�<�D�c3D�� D���D���D���D���D���D�� D��D��D���D���D���D���D�  D�	�D��D�)�D�0 D�9�D�C3D�S3D�ffD�|�D�� D��fD���D���D��3D��D�,�D�S3D�� D���D�� D� D�<�D�s3D���D�� D��D�P D���D��3D���D�33D�i�D¦fD��3D�fD�C3D�ffDȐ Dɹ�D�ٚD���D��D�0 D�I�D�Y�D�p D҃3DӐ DԜ�Dգ3D֩�D׳3DضfDٶfDڬ�D۰ Dܣ3Dݙ�Dޓ3D߃3D�y�D�l�D�c3D�S3D�C3D�33D�#3D�fD��D�3D���D�� D��3D�� D�� D�3D� D���D�3D�s3D�ffD�c3D�` D�Y�D�L�D�@ D�6fD�0 D��D�fD� D�fD��E   E � E�E��E��E��E E  E��E��E	� EC3EC3E�3E33E.fE��E�3E  EnfEffE�fE�fE4�E��E�fE�3E;3E ��E!� E"ɚE$ E%T�E&�3E'�fE)�E*FfE+y�E- E.I�E/x E0��E1� E3�E4Q�E5��E6�fE8T�E9� E:��E;��E?  EA�fEEc3EHY�EK�3EN� EQ��ET�3EW��E[3E^I�Ea� Edd�Eg�fEj� Em� EnVfEn�Eo� Ep,�Eq�Eq� ErI�Er�3EsvfEt�Et�fEu0 Ev Ev� Ew#3Ew��Ex�3Ey Ey��Ez.fEz�fE{��E|�E|�E}h E}�E~�fE1�E�E�8 E�� E���E�73E�t E�� E�( E�c3E�� E� >L��>L��>L��>L��>L��>���>���>L��=���>L��>L��>���>���=���>L��>���>L��>L��>L��>L��>���>���>���>���?   ?   ?��?L��?fff?�  ?���?�33?�  ?ٙ�?�33@��@��@,��@@  @L��@Y��@l��@y��@���@�33@���@���@�  @���@�ff@�33@�  @���@���A33A33A��A  A   A(  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441414411444414141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?L��?���@&ff@fff@�  @�33@�33@�  A  A��A0  AI��Ai��A���A���A���A���A�ffAՙ�A���A���B  B	��B��BffB"��B*��B2ffB:  BB  BI��BQ��BZ  Bb  Bi��BrffBz��B�33B�33B�ffB�33B�33B�  B�33B�33B�33B�ffB�ffB�ffB�ffB�33B�33B�  B���B�  B���B�ffB�33B�33B�  B�  B���B�  B�33B�33B�ffB�ffB���B�33C L�C� C�3C��CffC
��C��CffC��C��C�3C� C�3C��C��C� C �3C"��C$ffC&� C(�3C*��C,L�C.ffC0��C2�3C4��C6�3C8ffC:��C<�3C>�3C@�3CB� CDL�CFffCH� CJ��CL�3CN��CP� CRL�CTL�CVffCX� CZ�3C\��C^��C`L�CbffCd� Cf�3Ch� CjL�Cl� Cn�3Cp� CrffCt��CvffCxL�Cz� C|�3C~��C�33C�ffC�L�C�@ C�ffC�L�C�@ C�Y�C�L�C�33C�@ C�Y�C�ffC�Y�C�@ C�L�C�Y�C�L�C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�@ C�&fC�&fC�33C�33C�33C�@ C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�&fC�&fC�&fC�&fC�&fC�&fC��C��C��C�33C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�L�C�33C�33C�L�C�L�C�Y�C�s3C�Y�C�@ C�Y�C�L�C�&fC�L�C�ffC�Y�C�L�C�Y�C�Y�C�@ C�L�C�L�C�33C�L�C�ffC�L�C�33C�L�C�@ C�&fC�L�C�33C�&fC�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�@ C�Y�C�@ C�33C�@ C�L�C�@ C�&fC�@ C�@ C�ffC�L�C�33C�@ C�Y�C�L�C�33C�33C�@ C�Y�C�Y�C�s3D �D ffDFfD� D
&fD�3D9�D�fDFfD��D` DfD��D!L�D$fD&�fD)� D,@ D/3D1� D4�fD79�D9�3D<Y�D>��DA�fDDfDF�fDI  DKl�DM�fDP  DR�fDT�3DWL�DY�fD\�D^` D`�fDc9�De��Dh33Dj��Dm@ Do��Dr` Dt��Dwy�Dz,�D|Y�D~��D���D�3D�c3D��3D�3D�L�D�� D��fD��D��D�L�D�s3D�� D���D���D���D���D���D�� D���D���D���D���D���D�	�D� D��D�,�D�9�D�@ D�I�D�S3D�c3D�vfD���D�� D��fD���D���D�3D��D�<�D�c3D�� D���D�� D�  D�L�D��3D���D�� D�,�D�` D���D��3D��D�C3D�y�D¶fD��3D�&fD�S3D�vfDȠ D�ɚD��D��D�)�D�@ D�Y�D�i�Dр Dғ3DӠ DԬ�Dճ3Dֹ�D��3D��fD��fDڼ�D�� Dܳ3Dݩ�Dޣ3Dߓ3D���D�|�D�s3D�c3D�S3D�C3D�33D�&fD��D�3D�	�D�  D��3D�� D�� D��3D� D�D�3D�3D�vfD�s3D�p D�i�D�\�D�P D�FfD�@ D�)�D�&fD�  D�fD��E  E � E	�E��E��E��E  E( E��E��E	� EK3EK3E�3E;3E6fE��E�3E EvfEnfE�fE�fE<�E��E�fE�3EC3E ��E!� E"њE$ E%\�E&�3E'�fE)�E*NfE+��E-  E.Q�E/� E0��E1� E3!�E4Y�E5��E6�fE8\�E9� E:��E;��E?( EA�fEEk3EHa�EK�3EN� EQ��ET�3EX�E[#3E^Q�Ea� Edl�Eg�fEj� Em� En^fEn��Eo� Ep4�Eq�Eq� ErQ�Er�3Es~fEt�Et�fEu8 Ev Ev� Ew+3Ew��Ex�3Ey  Ey��Ez6fE{fE{��E|�E|�E}p E}�E~�fE9�E��E�< E�� E���E�;3E�x E�� E�, E�g3E�� E� G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?��G�O�?333G�O�G�O�?��?333G�O�G�O�G�O�G�O�?333G�O�?L��G�O�?fffG�O�?�  ?���?�ff?�33?�  ?ٙ�?�33@   @��@��@,��@9��@L��@`  @l��@y��@�ff@���@���@�33@���@���@�  @���@�ff@�33@�  @���A��A33A33A��A   A(  A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441414411444414141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ @ �@ V@ *@ O@ "�@ )�@ /�@ 6�@ >@ G�@ R�@ `B@ m�@ {�@ ��@ ��@ ��@ ��@ ��@ �|@ �t@ �m@ �@@o@ @-@:@G�@T�@bN@p�@~K@�D@�H@��@��@�>@є@ލ@�4@�,@�@*@"�@1'@>�@Lu@Z@g@t�@��@��@�@��@��@ƨ@�O@�H@��@�9@
=@�@&;@4�@B8@P�@\�@hs@ww@�|@�u@��@��@�k@ȴ@׹@�@�@  @@[@+@6�@E�@R�@^�@m:@|?@�7@��@�(@�-@��@��@��@�m@�q@�@o@ @,`@8�@F�@UU@c�@r@�W@��@�<@��@�9@@є@��@�4@��@%@{@#�@/�@<@K@Z@ff@s_@�d@��@��@��@��@ƨ@��@�T@�@��@J@�@%�@4�@A�@M�@\)@k.@y�@�|@��@�@�r@�k@ȴ@�
@�@�@ �@@�@+@6�@B�@P�@^�@l�@z3@��@�0@�(@��@�w@�@��@�m@�@	�@	b@	
@	+�@	8�@	F�@	S�@	a�@	oF@	|�@	��@	�<@	�5@	��@	��@	ψ@	�/@	�@	��@
�@
*@
"�@
1'@
>�@
M$@
Z�@
g@
s_@
�@
�@
��@
�@
�@
�W@
Ӡ@
�@
�@
�9@
�@�@&�@3�@B8@O�@\)@j@x&@�p@�u@�y@��@�@�@�
@�T@�@�Q@J@O@+@7�@DD@SI@`B@l�@z�@��@�0@�(@�~@��@��@�@�@��@v@�@
@,`@;d@H]@T�@bN@p�@�@�P@�U@�5@��@3�@t@�@��@C�@�7@�|@o@X�@�@�y@1'@{�@ƨ@@\)@�M@�Y@>@��@��@�@Z@�@�`@)�@m:@�r@�@/�@qS@��@�@3�@uk@��@�q@9X@}�@�2@%@K@��@�
@�@bN@�@�m@/@v�@�j@@K�@�u@��@O@]�@�@��@!s@`A@�@�@@M�@��@��@�,@ 1�@ hs@ ��@ �\@!�@!FQ@!~K@!��@!�@"+@"b�@"��@"�O@#V@#I@#�p@#�&@#��@$5�@$s_@$��@$��@%(G@%g@%�A@%�m@&(�@&i�@&��@&�4@'/@'p�@'�9@'��@(9X@({�@(�w@) �@)B�@)�|@)�@*�@*K�@*��@*�c@+�@+FQ@+�p@+�2@+��@,8�@,r�@,�@,��@-""@-[z@-�u@-�o@.@.;d@.r@.��@.��@/�@/FQ@/{�@/��@/�T@06@0K�@0~�@0�-@0�`@1�@1Lu@1�@1��@1�(@2�@2R�@2�@2�R@2�4@3�@3P�@3��@3�@3��@4"�@4X�@4��@4��@4��@5*S@5_�@5�i@5�W@5��@61'@6i!@6�@6Ӡ@7
�@7A�@7�-@8g@8�c@9:@9�T@:S�@:��@;k.@;�h@<x�@=B@=�p@>g@>��@?%�@?��@@+�@@ȴ@A2�@A�o@Be	@Bȴ@C]�@C�L@D��@E{@Et�@F  @F��@Go@G�T@H"�@H��@I)�@I�t@J\�@J��@Ka�@K�@Lj@L�@Mr�@M�@N��@O(�@O�!@P1�@Q��@R��@T7�@U{�@V��@X33@Yt�@Z��@\'�@]|?@^׹@`6�@ar�@bȴ@d{@em:@e��@e�@f6�@fy�@f܀@g�@g`B@g��@g��@h �@h^�@h�@h�,@i6�@ir@i�!@j�@jG�@j��@j�w@k6@kS�@k��@k�`@l�@lV@l��@l��@m3�@mi�@m��@m�@nC�@nww@n��@o@oC�@o�\@o�tG�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@  �G�O�@ ^G�O�G�O�@  �@ ^G�O�G�O�G�O�G�O�@ ^G�O�@ G�O�@ �G�O�@ j@ @ v@ %@ �@ 1@ 	�@ 
=@ �@ �@ @ b@ o@ {@ �@ 6@ B@ �@ [@ g@ !s@ $.@ %�@ (G@ *S@ -@ /�@ 2�@ 5?@ 7�@ ;d@ >@ @�@ DD@ G�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�%A��A��A��A� �A��A��A� �A� �A�"�A�"�A�"�A�$�A�"�A�+A�+A�-A�-A�(�A�"�A� �A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�(�A�(�A�(�A�(�A�1'A�=qA�Q�A�^5A�bNA�XA�M�A�/A��A�{A��A��A��A��A�{A�bA�%A�%A�A��yA��!A���A��PA���A�\)A�A�dZA��
A�oA���A�ȴA�n�A�1'A�bA��FA�ĜA�ȴA�S�A�VA��uA��A��A�VA��A�&�A�33A�VA��
A�|�A���A���A��`A��^A�oA�(�A�C�A�9XA�%A��\A�S�A�5?A��A�t�A��TA���A�x�A�S�A��HA��RA��-A���A���A�1A�A��A��A���A��\A�A�bNA��^A�A}XA|v�A{S�Az�uAy��Aw��As�mAr5?Aql�AnbNAk�AiƨAg��Af(�AeK�Ad�/Ad-Ac�Ac?}Ac/Ab�Abz�Ab�Aa�7A_�A_�A]O�AZVAXJAVv�AU�AS"�AQ33AP  AN�ALJAH�+AGC�AF��AE\)ADbAB�AB$�AA��A@�!A?�#A?&�A>�A=�^A;C�A9"�A8�uA6��A69XA4ffA3�A1�A0z�A.bNA,A�A+�A*E�A)��A)&�A(�!A(9XA'p�A&n�A%G�A#�;A"��A �HA�hAO�A;dA"�AVA��A��A�\A�A�A��A��A+AM�A�AoAE�A�yA��A��A�wA�A��Al�A33AA�AVA�#A
�AQ�A�wA��An�A��AdZAO�AC�A33A&�AVA%A�A�!An�AJA ��@�?}@���@�p�@��u@�1'@�|�@��@�C�@�h@�@�!@�`B@�@�R@�ff@�h@� �@�J@�P@�`B@��@ߕ�@�
=@�@�V@܃@�1@��/@�%@���@��@���@�O�@�C�@��-@�t�@�A�@��D@��\@�C�@��u@�t�@� �@���@�z�@�;d@���@�1@�S�@��T@�b@�=q@��@��j@� �@���@�ȴ@��!@���@��@�J@��h@��j@�  @��m@�ȴ@�n�@���@�@��@��h@�7L@�9X@K�@~$�@|Z@z-@x �@u�T@r-@m@lZ@j�@h��@h��@g+@e/@dz�@c��@`�@^��@[�m@Z�!@Yx�@Xr�@W�@Up�@Tz�@R��@Q�^@P  @M�-@L�/@K��@I�#@H��@H�9@Gl�@F�y@E�-@E�@CS�@B��@B�\@A��@?�@>v�@=�T@=�h@<(�@97L@7�@5/@4��@3�F@1�@0��@/l�@.E�@.{@-O�@,�D@,j@*�\@)��@(�`@(��@(��@'�;@&�@%?}@$�@$�/@$j@$1@"��@!�#@ b@�@�@��@O�@z�@�@��@G�@��@�y@E�@��@O�@O�@z�@ƨ@�H@^5@J@7L@��@1'@�w@;d@�R@$�@��@��@(�@�m@�m@t�@
�\@	�^@	%@�9@  @K�@v�@$�@p�@Z@t�@"�@�\@J@��@��@�@ Ĝ@ b?��R?���?���?�
=?�?}?���?�\?���?� �?�j?��?��y?�E�?���?���?�\?�J?��`?���?���?��H?�~�?�^5?�x�?�r�?�l�?�ȴ?��T?ԛ�?��?ӕ�?щ7?�  ?�V?�/?�1?˅?�?��#?�x�?ȓu?�Q�?��?Ƨ�?�?��?ě�?�M�?�&�?�|�?�{?�O�?�j?��?���?�=q?��?�Q�?���?���?���?��?�b?�1'?�b?�1'?�Q�?�r�?��u?��u?��9?��9?��9?��9?���?���?���?��?�7L?�X?�X?�X?�x�?�x�?�X?�X?�x�?���?��^?��^?��#?���?���?�^5?�^5?�~�?�~�?���?���?��H?�?�C�A�%A�
=A�1A�1A�1A�A�A�  A���A���A���A���A��A���A���A���A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A�A���A�A�
=A�JA�JA��A��A��A��A��A��A��A��A��A��A� �A��A��A� �A��A��A��A� �A�"�A� �A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               A���A���A�%A��A��A��A� �A��A��A� �A� �A�"�A�"�A�"�A�$�A�"�A�+A�+A�-A�-A�(�A�"�A� �A�$�A�"�A�"�A�"�A�"�A�"�A�$�A�$�A�(�A�(�A�(�A�(�A�1'A�=qA�Q�A�^5A�bNA�XA�M�A�/A��A�{A��A��A��A��A�{A�bA�%A�%A�A��yA��!A���A��PA���A�\)A�A�dZA��
A�oA���A�ȴA�n�A�1'A�bA��FA�ĜA�ȴA�S�A�VA��uA��A��A�VA��A�&�A�33A�VA��
A�|�A���A���A��`A��^A�oA�(�A�C�A�9XA�%A��\A�S�A�5?A��A�t�A��TA���A�x�A�S�A��HA��RA��-A���A���A�1A�A��A��A���A��\A�A�bNA��^A�A}XA|v�A{S�Az�uAy��Aw��As�mAr5?Aql�AnbNAk�AiƨAg��Af(�AeK�Ad�/Ad-Ac�Ac?}Ac/Ab�Abz�Ab�Aa�7A_�A_�A]O�AZVAXJAVv�AU�AS"�AQ33AP  AN�ALJAH�+AGC�AF��AE\)ADbAB�AB$�AA��A@�!A?�#A?&�A>�A=�^A;C�A9"�A8�uA6��A69XA4ffA3�A1�A0z�A.bNA,A�A+�A*E�A)��A)&�A(�!A(9XA'p�A&n�A%G�A#�;A"��A �HA�hAO�A;dA"�AVA��A��A�\A�A�A��A��A+AM�A�AoAE�A�yA��A��A�wA�A��Al�A33AA�AVA�#A
�AQ�A�wA��An�A��AdZAO�AC�A33A&�AVA%A�A�!An�AJA ��@�?}@���@�p�@��u@�1'@�|�@��@�C�@�h@�@�!@�`B@�@�R@�ff@�h@� �@�J@�P@�`B@��@ߕ�@�
=@�@�V@܃@�1@��/@�%@���@��@���@�O�@�C�@��-@�t�@�A�@��D@��\@�C�@��u@�t�@� �@���@�z�@�;d@���@�1@�S�@��T@�b@�=q@��@��j@� �@���@�ȴ@��!@���@��@�J@��h@��j@�  @��m@�ȴ@�n�@���@�@��@��h@�7L@�9X@K�@~$�@|Z@z-@x �@u�T@r-@m@lZ@j�@h��@h��@g+@e/@dz�@c��@`�@^��@[�m@Z�!@Yx�@Xr�@W�@Up�@Tz�@R��@Q�^@P  @M�-@L�/@K��@I�#@H��@H�9@Gl�@F�y@E�-@E�@CS�@B��@B�\@A��@?�@>v�@=�T@=�h@<(�@97L@7�@5/@4��@3�F@1�@0��@/l�@.E�@.{@-O�@,�D@,j@*�\@)��@(�`@(��@(��@'�;@&�@%?}@$�@$�/@$j@$1@"��@!�#@ b@�@�@��@O�@z�@�@��@G�@��@�y@E�@��@O�@O�@z�@ƨ@�H@^5@J@7L@��@1'@�w@;d@�R@$�@��@��@(�@�m@�m@t�@
�\@	�^@	%@�9@  @K�@v�@$�@p�@Z@t�@"�@�\@J@��@��@�@ Ĝ@ b?��R?���?���?�
=?�?}?���?�\?���?� �?�j?��?��y?�E�?���?���?�\?�J?��`?���?���?��H?�~�?�^5?�x�?�r�?�l�?�ȴ?��T?ԛ�?��?ӕ�?щ7?�  ?�V?�/?�1?˅?�?��#?�x�?ȓu?�Q�?��?Ƨ�?�?��?ě�?�M�?�&�?�|�?�{?�O�?�j?��?���?�=q?��?�Q�?���?���?���?��?�b?�1'?�b?�1'?�Q�?�r�?��u?��u?��9?��9?��9?��9?���?���?���?��?�7L?�X?�X?�X?�x�?�x�?�X?�X?�x�?���?��^?��^?��#?���?���?�^5?�^5?�~�?�~�?���?���?��H?�?�C�A�%A�
=A�1A�1A�1A�A�A�  A���A���A���A���A��A���A���A���A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A�A���A�A�
=A�JA�JA��A��A��A��A��A��A��A��A��A��A� �A��A��A� �A��A��A��A� �A�"�A� �A� �A� �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B\)B\)B\)B[#BZB[#BZB[#B[#B[#B[#B[#B[#B[#B[#B\)B[#B\)B\)B[#B[#BZB[#BZBZBZBZBZBZBZBZBZBZBZBZB^5BbNBiyB|�B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B�hB}�B�7B�=B�+B�B�B�B� B�B�B~�B}�Bz�B{�By�Bs�Bn�BgmBdZB`BB]/BS�BK�BA�B8RB.B"�B�BDB��B��B�B�B�B��BȴB�B��B��B�=BgmBC�B-B�BbB
��B
�B
�5B
��B
��B
�wB
��B
n�B
O�B
>wB
:^B
0!B
.B
%�B
�B
  B	��B	�B	�B	ɺB	�B	��B	��B	�uB	�\B	�7B	�B	�B	�B	� B	|�B	z�B	s�B	jB	hsB	YB	H�B	;dB	9XB	0!B	!�B	�B	bB	+B�B�ZB�TB�TB�)B�B��B��BǮB��B�qB�dB�dB�FB�B��B��B��B��B�{B�bB�1B�%B|�By�Bw�Bu�Br�Bq�Bo�Bl�BjBe`BdZB_;B\)BS�BT�BS�BR�BQ�BO�BN�BL�BI�BG�BF�BF�BE�BB�B:^B;dB8RB5?B49B49B33B2-B2-B1'B0!B0!B0!B/B/B.B+B0!B.B+B-B+B+B)�B(�B(�B(�B(�B(�B'�B(�B(�B)�B&�B$�B&�B$�B$�B$�B"�B$�B(�B0!B33B33B2-B49B2-B2-B33B33B6FB49B33B0!B1'B33B9XB<jB=qB=qBH�BK�BVBZBcTBm�Bs�By�B}�B�=B��B�B�qBǮB��B�B�`B��B	JB	$�B	,B	2-B	8RB	@�B	S�B	o�B	s�B	y�B	}�B	�B	�hB	��B	��B	�B	�RB	�dB	��B	ĜB	��B	��B	�#B	�;B	�`B	�B	�B	��B	��B	��B	��B
  B
B
B
B
B
B
+B

=B
JB
VB
oB
uB
uB
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
%�B
%�B
'�B
)�B
+B
-B
.B
0!B
1'B
2-B
33B
5?B
5?B
7LB
8RB
9XB
;dB
>wB
?}B
?}B
?}B
A�B
D�B
E�B
F�B
F�B
G�B
I�B
J�B
O�B
O�B
N�B
O�B
Q�B
Q�B
T�B
T�B
W
B
VB
W
B
W
B
YB
[#B
\)B
[#B
\)B
]/B
]/B
_;B
_;B
`BB
_;B
`BB
cTB
cTB
dZB
ffB
ffB
ffB
hsB
hsB
jB
iyB
iyB
jB
k�B
k�B
k�B
l�B
m�B
m�B
n�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
y�B
z�B
{�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�+B
�1B
�1B
�7B
�1B
�=B
�JB
�VB
�bB
�bB
�hB
�hB
�oB
�{B
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
�B
�B
�B
�B
�'B
�-B
�3B
�3B
�9B
�9B
�FB
�LB
�RB
�RB
�XB
�XB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�RB
�^B
�^B
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�^B
�^B
�dB
�XB
�dB
�^B
�^B
�^B]/B[#B\)B\)B[#B\)B\)B\)B[#B\)B[#B\)B]/B[#B\)B[#B\)B\)B\)B\)B\)B\)B]/B]/B\)BZB[#B[#B\)B]/B\)B\)B[#B]/B]/B[#B[#B[#B[#B[#B[#BZBZB[#B[#B[#B[#B[#BZBZB[#B[#B[#B[#B[#B[#BZB[#B[#B[#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B[�B[�B\ BZ�BY�BZ�BY�BZ�BZ�BZ�BZ�BZ�BZ�BZ�BZ�B\B[ B\B\B[B[BY�B[BY�BY�BY�BZ BZ BZBZBZBZBZBZBZB^Bb7BicB|�B�AB�lB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�lB�xB�`B}�B�/B�6B�$B�B�B�B�B�B�B~�B}�Bz�B{�By�Bs�Bn�BgmBd[B`CB]1BS�BK�BA�B8VB.B"�B�BJB� B��B�B�B�B�BȽB�B��B��B�HBgxBC�B-B�BoB
��B
�B
�CB
��B
��B
��B
��B
n�B
O�B
>�B
:oB
02B
.&B
%�B
�B
 B	��B	�B	�1B	��B	�0B	��B	��B	��B	�sB	�OB	�7B	�+B	�B	�B	}B	z�B	s�B	j�B	h�B	Y4B	H�B	;�B	9vB	0@B	!�B	�B	�B	KB��B�{B�uB�vB�KB�'B�B��B��B��B��B��B��B�mB�)B�B��B��B��B��B��B�[B�PB}BzBw�Bu�Br�Bq�Bo�Bl�Bj�Be�Bd�B_lB\ZBT)BU0BT*BS%BR BPBOBMBI�BG�BF�BF�BE�BB�B:�B;�B8�B5yB4sB4tB3nB2iB2iB1dB0_B0_B0`B/ZB/[B.TB+CB0bB.VB+DB-PB+EB+FB*@B);B);B)<B)<B)=B(8B)>B)?B*EB'3B%'B'4B%(B%)B%)B#B%*B)DB0oB3�B3�B2}B4�B2~B2~B3�B3�B6�B4�B3�B0uB1|B3�B9�B<�B=�B=�BIBL&BVeBZ�Bc�Bm�Bt#BzKB~gB��B�0B��B��B�0B�jBڥB��B�vB	�B	%rB	,�B	2�B	8�B	A$B	T�B	pEB	t`B	z�B	~�B	��B	�B	�YB	�mB	��B	�B	�(B	�JB	�fB	ҹB	��B	��B	�B	�9B	�gB	�}B	��B	��B	��B	��B
 �B
�B
B
B
B
B
+B
@B
PB
_B
zB
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
'B
'B
)B
+-B
,5B
.DB
/LB
1\B
2dB
3mB
4uB
6�B
6�B
8�B
9�B
:�B
<�B
?�B
@�B
@�B
@�B
B�B
E�B
GB
HB
HB
IB
K+B
L4B
QUB
QXB
PUB
Q^B
SnB
SqB
V�B
V�B
X�B
W�B
X�B
X�B
Z�B
\�B
]�B
\�B
]�B
^�B
^�B
`�B
`�B
a�B
`�B
a�B
eB
eB
fB
h&B
h(B
h+B
j:B
j<B
lKB
kGB
kIB
lRB
mZB
m\B
m_B
ngB
ooB
oqB
p{B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
w�B
x�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
}�B
�B
�	B
�B
�B
�B
�B
�B
�$B
�,B
�7B
�OB
�VB
�gB
�uB
�zB
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
�B
�B
�B
�%B
�8B
�<B
�=B
�IB
�WB
�cB
�mB
�tB
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
�B
�B
�B
�,B
�,B
�>B
�DB
�ZB
�sB
��B
��B
��B
��B
��B
��B
�B
� B
�6B
�LB
�ZB
�oB
�~B
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
��B
��B
��B
��B
�B
��B
�B
�B
�B
�B]BZ�B[�B[�BZ�B[�B[�B[�BZ�B[�BZ�B[�B]BZ�B[�BZ�B[�B[�B[�B[�B[�B[�B]B]B[�BY�BZ�BZ�B[�B]B[�B[�BZ�B]B]BZ�BZ�BZ�BZ�BZ�BZ�BY�BY�BZ�BZ�BZ�BZ�BZ�BY�BY�BZ�BZ�BZ�BZ�BZ�BZ�BY�BZ�BZ�BZ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901220526332021061413560320210614135603202106171314032021061713140320210617131403201901220526332021061413560320210614135603202106171314032021061713140320210617131403PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019012205263320190122052633  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012205263320190122052633QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019012205263320190122052633QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151320210617131513IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                