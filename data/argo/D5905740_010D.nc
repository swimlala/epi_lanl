CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:01Z creation      
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
_FillValue                 $  L\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   |   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220301  20210722161419  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               
   
DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�l���@�l���11  @�l�`�@�l�`�@*T��*�@*T��*��cM�kP���cM�kP��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?fff@ff@Fff@�  @���@�  @���@���A��A$��AA��Ac33A�  A�33A�  A�  A���A���AᙚA�  A�33B  B33BffB��B'��B/��B8ffB@ffBG��BP  BX  B`  Bh  Bp  Bw��B��B���B���B���B�  B�33B�  B���B�33B�ffB�ffB�  B�  B�33B�33B�33B�33B�33B���B̙�B�ffB�33B�33B�  B���B㙚B�ffB�ffB�  B�  B�  B�33C �C33C33C  C�C
�C�3C�fC  C�C�C33CL�C�fC  C33C��C!�fC$  C&�C(L�C*  C,  C.33C033C2L�C4  C6�C833C9��C;�fC>33C@�CB�CD33CE�fCH33CJ  CK��CM�fCP  CR�CT�CV�CX33CZ33C\�C^33C`33Cb�Cd�Ce�fCg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}�fC��C��3C��3C��fC��fC�ٚC��fC��fC��3C�  C�  C��C��C�&fC��C��fC�  C��C��C��3C��C�&fC��C��3C��C��3C�ٚC��3C��C�  C��fC�  C��C�  C��3C�  C��C��C��3C��C��C��C��fC��3C�  C��C��C��C��fC��3C�  C��C��C��C�&fC�33C�&fC�&fC��C��fC��3C��3C�  C��3C��3C��C�&fC��C��C�  C��C�  C�  C��C�  C��fC�  C��C�  C��3C��C�&fC��C�  C��C��C�  C��fC��3C��C��3C��3C��C��C��C�  C�ٚC�  C��C��3C��fC��C�&fC��C�  C�  C��3C��C�&fC��C��C��3C��fC�ٚC�  C�&fC�&fC�&fC�&fC�&fC��C��C��3C��C�&fC�&fC�&fD��D��D	L�D�DٚD��D� DY�D9�D,�D fD"� D%�3D(ffD+3D-� D0s3D3�D5�3D8&fD:� D=  D?l�DA� DDL�DF�3DI�DK��DN�DP� DR� DUL�DW� DZ33D\�3D_�Dal�Dc�fDf  Dh� Dj��DmL�Do�3Dr�Dts3Dv� Dy@ D{9�D}� D�fD�I�D�y�D���D�� D�3D�FfD�s3D�� D�� D�  D�&fD�S3D�� D��3D��fD�,�D�i�D�� D�� D�0 D�y�D�ɚD��D�c3D���D�� D�9�D�|�D���D���D�I�D�� D��3D�3D�\�D���D�ٚD�fD�L�D���D���D�3D�<�D�s3D���D��3D�3D�L�D��fD�� D�� D�  D�P D Dì�D���D�fD�6fD�Y�Dɀ Dʩ�D��3D���D�  D�C3D�c3Dь�Dҩ�D�ɚD��3D��D�6fD�I�D�\�D�p DۆfDܖfDݬ�D��3D�� D��D���D� D� D�#3D�,�D�@ D�L�D�` D�i�D�p D�p D�l�D�p D�l�D�|�D�3D� D�3D���D���D�� D��3D��3D���D���D��3D�� D�� D���D���E a�E �3Eq�E��E��E3E��EfE�fE<�E^fE~fE� E	�fE~fE��EٚE  E)�EP E�E9�E^fE� E��EP Ed�Ex E�3E  E,�E � E!��E#0 E$.fE%��E&��E'�3E)\�E*I�E+�fE-�E.a�E/K3E0��E23E3 E4l�E5�fE6�3E833E9��E:�3E;��E?P EB.fEE��EHa�EK�3EN�3EQ�fEU3EW�fE[.fE^S3Ead�Edh Eg~fEj� Em�Eq!�EtfEwT�Ez6fE}l�E�P E��3E�^fE�� E���E� E�� E�D�E��3E�Q�E�� E�p E��E��3E��E�q�E�͚E�	�E�i�E���E�fE�E�E���E��fE�BfE�� E�ݚE�< E�|�E��fE��E�w3E��3E� ?L��?��?L��?L��?L��?333?L��?L��?L��?L��?fff?L��?L��?L��?fff?fff?fff?L��?L��?fff?�  ?�  ?�  ?���?���?�ff?�33?���?�ff@   @ff@��@   @9��@Fff@Y��@l��@�33@���@�33@���@���@�33@���@���@�33@���@陚@�ffA��A	��A��A  AffA$��A,��A333A9��AA��AH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444144414444114411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?�33@&ff@fff@�  @���@�  @���AffA��A,��AI��Ak33A�  A�33A�  A�  A���A���A噚A�  B��B
  B33BffB!��B)��B1��B:ffBBffBI��BR  BZ  Bb  Bj  Br  By��B���B���B���B���B�  B�33B�  B���B�33B�ffB�ffB�  B�  B�33B�33B�33B�33B�33B���B͙�B�ffB�33B�33B�  B���B䙚B�ffB�ffB�  B�  B�  B�33C ��C�3C�3C� C��C
��C33CffC� C��C��C�3C��CffC� C�3C L�C"ffC$� C&��C(��C*� C,� C.�3C0�3C2��C4� C6��C8�3C:L�C<ffC>�3C@��CB��CD�3CFffCH�3CJ� CLL�CNffCP� CR��CT��CV��CX�3CZ�3C\��C^�3C`�3Cb��Cd��CfffChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~ffC�&fC�33C�33C�&fC�&fC��C�&fC�&fC�33C�@ C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�Y�C�L�C�33C�L�C�ffC�L�C�33C�L�C�33C��C�33C�L�C�@ C�&fC�@ C�Y�C�@ C�33C�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�L�C�Y�C�ffC�s3C�ffC�ffC�L�C�&fC�33C�33C�@ C�33C�33C�L�C�ffC�Y�C�L�C�@ C�Y�C�@ C�@ C�L�C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�ffC�L�C�@ C�L�C�Y�C�@ C�&fC�33C�L�C�33C�33C�L�C�Y�C�L�C�@ C��C�@ C�L�C�33C�&fC�L�C�ffC�L�C�@ C�@ C�33C�L�C�ffC�Y�C�L�C�33C�&fC��C�@ C�ffC�ffC�ffC�ffC�ffC�Y�C�Y�C�33C�Y�C�ffC�ffC�ffD�D��D	l�D9�D��D��D� Dy�DY�DL�D &fD#  D%�3D(�fD+33D-� D0�3D3,�D5�3D8FfD:� D=  D?��DB  DDl�DF�3DI9�DK��DN,�DP� DS  DUl�DW� DZS3D\�3D_,�Da��Dc�fDf@ Dh� Dk�Dml�Do�3Dr,�Dt�3Dw  Dy` D{Y�D}� D�&fD�Y�D���D���D�� D�#3D�VfD��3D�� D�� D� D�6fD�c3D�� D��3D�fD�<�D�y�D�� D�  D�@ D���D�ٚD�)�D�s3D���D�  D�I�D���D���D��D�Y�D�� D��3D�#3D�l�D���D��D�&fD�\�D���D���D�3D�L�D��3D���D��3D�#3D�\�D��fD�� D�  D�0 D�` D Dü�D���D�fD�FfD�i�Dɐ Dʹ�D��3D��D�0 D�S3D�s3Dќ�Dҹ�D�ٚD�3D�)�D�FfD�Y�D�l�Dڀ DۖfDܦfDݼ�D��3D�� D���D�	�D�  D�  D�33D�<�D�P D�\�D�p D�y�D� D� D�|�D� D�|�D���D�3D� D�3D���D���D�� D��3D��3D���D���D��3D�� D�� D���D�ɚE i�E �3Ey�E�E��E3E��E&fE�fED�EffE�fE� E	�fE�fE��E�E E1�EX E�EA�EffE� E��EX El�E� E�3E( E4�E � E!��E#8 E$6fE%��E&��E'�3E)d�E*Q�E+�fE-�E.i�E/S3E0��E2#3E3 E4t�E5�fE6�3E8;3E9��E:�3E<�E?X EB6fEE��EHi�EK�3EN�3EQ�fEU3EW�fE[6fE^[3Eal�Edp Eg�fEj� Em�Eq)�EtfEw\�Ez>fE}t�E�T E��3E�bfE�� E���E� E�� E�H�E��3E�U�E�� E�t E��E��3E��E�u�E�њE��E�m�E���E�
fE�I�E���E��fE�FfE�� E��E�@ E���E��fE��E�{3E��3E� G�O�?���G�O�G�O�G�O�?���G�O�G�O�G�O�?�ffG�O�G�O�G�O�?�ffG�O�G�O�G�O�G�O�?�ff?�33G�O�G�O�?�  ?���?ٙ�?�ff?�33@ff@33@   @&ff@9��@@  @Y��@fff@y��@�ff@�33@���@�33@���@���@�33@���@���@�33@���@���A33A	��A��A��A   A&ffA,��A4��A;33AA��AI��AP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444144414444114411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ %@ V@ *@ O@ !s@ (�@ /@ 5�@ >@ FQ@ R�@ `�@ m:@ z3@ ��@ �0@ ��@ �-@ ��@ ��@ ��@ �@ ��@@b@
@+�@:�@H]@T�@b�@p�@~K@��@��@��@�9@��@ψ@�/@�@��@�@�@"�@1'@>�@K@X�@g@t�@�d@�@��@��@�^@�W@�O@��@��@��@�@B@&�@33@@�@N�@\�@j@x�@�|@��@�@��@��@ȴ@�
@�`@�@^@�@�@(�@7�@B�@Q=@_�@m�@|�@��@�0@�5@��@�2@��@�#@�y@�e@�@o@g@-@;d@F�@V�@b�@oF@}�@��@�H@��@��@��@є@ލ@��@��@�@*@!s@.l@<@I�@Wb@e	@r�@�W@��@��@�M@��@�J@�C@��@�@@�9@�@�@$.@1�@@,@N�@\)@j@x�@�+@�u@�@�@�@�@�[@�`@�e@ �@�@�@(G@4�@C�@R�@_�@k�@z�@��@�0@�(@�~@��@�|@��@��@� @	@	�@	
@	,`@	:�@	I@	V@	a�@	o�@	~K@	��@	�H@	��@	��@	�J@	�C@	��@	�4@	��@
%@
�@
""@
/@
<�@
K�@
Z�@
g�@
t�@
��@
��@
�@
��@
�@
��@
�C@
�H@
�L@
��@	�@�@'�@3�@@�@O0@]�@i�@v@�p@�u@��@�f@�k@��@׹@�@�L@  @V@�@'�@7L@FQ@R�@_�@m:@z3@�7@�<@�5@�-@�w@�o@�h@�@��@v@@ �@.l@;d@I@T�@dZ@r�@�W@��@�@Z@�5@�@<�@��@�
@$�@s_@��@�@_�@��@�q@?}@��@�C@B@^5@��@�@(�@k.@�@�L@1�@s_@��@��@=q@~K@��@j@FQ@��@��@�@K�@��@��@@O�@�h@є@@UU@�0@�@@SI@��@խ@6@X�@�H@��@�@\)@�@��@�@\�@�@ލ@#�@e�@�M@��@ 33@ ww@ ��@!v@!M$@!�u@!��@"�@"e	@"��@"�@@#2�@#y�@#�&@$@$H]@$��@$�C@%�@%Z@%�U@%��@&$�@&g@&��@&�4@'+�@'m�@'�-@'�@(7�@(z�@(��@(��@)=q@)~K@)�w@)�Q@*>�@*�@*��@*��@+<@+{�@+�@+�,@,7L@,t�@,�9@,��@-.l@-m�@-��@-�y@.$/@.^�@.��@.��@/@/Ji@/��@/@/�9@05@@0p�@0�A@0��@1�@1UU@1��@1�c@2@2:@2p�@2��@2��@3�@3M�@3��@3�&@3�q@4.l@4e�@4�@4�O@5
�@5B�@5v�@5��@5�@6O@6T�@6��@6ƨ@7]@7:�@7v@7��@7�y@8$/@8^�@8�T@8�@9T�@9ψ@:K@:ȴ@;�p@<v@<��@=j@=�d@>  @>��@?> @?�@@6�@@��@Ai�@A��@BUU@B��@Cww@C�(@D�\@D�E@E�m@F�@F�M@G�@G��@HB�@H��@I<�@I�C@Jg@J��@Ke	@K��@Lb�@L�9@M��@M��@N��@O1�@O��@P5@@Q�m@R��@TI�@U~�@V�y@XE�@Yl�@Z�@\$�@]�p@^��@`+@at@b�J@d�@e~�@f��@h	@i�+@j��@l �@m~K@n��@p�@qww@r��@tO@u��@v׹@x�@yr@zӠ@|O@}|?@~�@��@�0x@�W�@�qS@��I@��@@��/@��&@� �@�;@�d@���@��;@�΂@��(@�@�,�@�T�@�n�@��0G�O�@ G�O�G�O�G�O�@ �G�O�G�O�G�O�@ vG�O�G�O�G�O�@ vG�O�G�O�G�O�G�O�@ v@ %G�O�G�O�@ �@ �@ 1@ �@ 	�@ 
�@ J@ �@ V@ b@ @ �@ *@ 6@ B@ �@ [@ g@ !s@ $.@ &;@ (G@ +�@ -@ /@ 1�@ 4�@ 7L@ :�@ >@ @�@ C�@ FQ@ I�@ Lu@ O0@ R�@ UUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A��
A�A�&�A�/A�-A�/A�/A�/A�-A�&�A�$�A�$�A�&�A�(�A�+A�&�A�"�A� �A��A��A��A��A�A�
=A���A��HA��#A��;A��#A���A���A�ĜA�AٸRAٟ�AָRA�`BA�AɶFA��HAƇ+A�7LA��DA��hA�"�A��;A���A�ƨA��A�{A���A��A�"�A�{A�`BA���A� �A��TA���A��A�&�A���A��#A�G�A�
=A�&�A�&�A���A��9A��A�l�A��RA�ȴA�  A�/A�oAyK�AvA�Ar��An$�Ak�wAi+Ae�A`��A\��AW33ATn�AO|�AK�;AI?}AEl�ADJAC33AA�A@�A>~�A>bA=�A=K�A;�A;&�A8��A7"�A4r�A4�9A5��A5�mA6�DA5VA21A0��A0��A0ZA0�A/�A/�hA/"�A/+A.��A.M�A-�wA-��A-K�A,�A,�+A,bNA+��A*�9A*v�A*-A)�mA)��A)S�A(~�A'�A'S�A&E�A%�
A%t�A%dZA$��A$z�A#ƨA#�A"�+A"(�A!��A!|�A ��A v�A �A�A?}A�A��Al�A��A��AA�A�#AoA��A��A^5A1'A��A%A�9A$�A�PA7LA�`A�DA-A�Ap�A+A�jAQ�A1A�-A\)A;dA�HA��A�+A9XA�AƨAx�AS�AVA�A�9A1AA`BA/AoA�A$�A�At�A�/AVAA�
A�FA��AC�A
��A
�DA
bA	ƨA	�7A	VA�AbNAJAx�AA�RAr�A1'A�#A�AG�A��A~�A1Ap�A+A�A�A��A=qA�hAG�A;dA7LA�A �!A M�A   @�|�@���@��+@��^@�/@��/@��u@� �@���@�33@��!@��@�"�@�r�@���@�w@��@���@��m@�A�@�@؛�@ա�@ҏ\@�\)@�;d@��/@�/@Å@��@�@���@�&�@��@�p�@�dZ@��@��@�`B@�t�@��7@���@�=q@���@��@��-@�A�@���@��-@���@�1@���@�?}@��P@�-@�/@��@��!@�X@�C�@��@�dZ@���@��/@���@�v�@�7L@�9X@�S�@�{@��/@
=@|z�@{S�@y�^@w;d@u�@s��@r��@q��@o��@n@kƨ@i�#@g��@f5?@d�j@cC�@a�@`r�@_;d@^{@\z�@Z=q@WK�@U��@Tj@SC�@Q��@Pb@Nȴ@L��@L�@K33@Jn�@HĜ@G�w@F5?@E�@C33@A��@@  @>�+@=�h@=/@;��@:��@9&�@81'@6�@5�-@5/@49X@2~�@1�@/�P@.E�@,�@+�m@*J@)G�@( �@&V@%`B@$�@#�
@"~�@!x�@ �@  �@K�@E�@��@�j@��@�!@J@X@bN@�@
=@5?@�@��@(�@33@��@-@�#@�`@ �@;d@��@�@`B@/@Z@�m@C�@
�!@	��@	G�@�9@bN@|�@�@�R@ff@��@�h@V@I�@1@t�@-@ ��@ b?���?��?��?�?�X?��?�`B?�9X?��?��?�&�?� �?�R?�{?�V?�ƨ?�C�?�^5?���?�Q�?�+?�?��?��?�S�?�n�?�7?�bN?��;?�;d?�V?ݲ-?�V?�I�?�dZ?��H?��#?��?ؓu?���?�E�?�Z?�33?ѩ�?� �?�V?�O�?�ƨ?ʟ�?�X?�Q�?�K�?��T?��?��?�A�?���?�{?��?�1?�C�?�^5?��^?�x�?�1'?��?�+?���?��?��/?��j?��
?��?�n�?���?���?���?���?��7?��7?��7?���?���?��?��?�J?�J?�-?�-?�M�?�n�?�n�?��\?��!?��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��;A���A�A�VA� �A�+A�+A�-A�/A�1'A�-A�-A�/A�/A�/A�/A�1'A�-A�/A�/A�/A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A���A���A���A��
A�A�&�A�/A�-A�/A�/A�/A�-A�&�A�$�A�$�A�&�A�(�A�+A�&�A�"�A� �A��A��A��A��A�A�
=A���A��HA��#A��;A��#A���A���A�ĜA�AٸRAٟ�AָRA�`BA�AɶFA��HAƇ+A�7LA��DA��hA�"�A��;A���A�ƨA��A�{A���A��A�"�A�{A�`BA���A� �A��TA���A��A�&�A���A��#A�G�A�
=A�&�A�&�A���A��9A��A�l�A��RA�ȴA�  A�/A�oAyK�AvA�Ar��An$�Ak�wAi+Ae�A`��A\��AW33ATn�AO|�AK�;AI?}AEl�ADJAC33AA�A@�A>~�A>bA=�A=K�A;�A;&�A8��A7"�A4r�A4�9A5��A5�mA6�DA5VA21A0��A0��A0ZA0�A/�A/�hA/"�A/+A.��A.M�A-�wA-��A-K�A,�A,�+A,bNA+��A*�9A*v�A*-A)�mA)��A)S�A(~�A'�A'S�A&E�A%�
A%t�A%dZA$��A$z�A#ƨA#�A"�+A"(�A!��A!|�A ��A v�A �A�A?}A�A��Al�A��A��AA�A�#AoA��A��A^5A1'A��A%A�9A$�A�PA7LA�`A�DA-A�Ap�A+A�jAQ�A1A�-A\)A;dA�HA��A�+A9XA�AƨAx�AS�AVA�A�9A1AA`BA/AoA�A$�A�At�A�/AVAA�
A�FA��AC�A
��A
�DA
bA	ƨA	�7A	VA�AbNAJAx�AA�RAr�A1'A�#A�AG�A��A~�A1Ap�A+A�A�A��A=qA�hAG�A;dA7LA�A �!A M�A   @�|�@���@��+@��^@�/@��/@��u@� �@���@�33@��!@��@�"�@�r�@���@�w@��@���@��m@�A�@�@؛�@ա�@ҏ\@�\)@�;d@��/@�/@Å@��@�@���@�&�@��@�p�@�dZ@��@��@�`B@�t�@��7@���@�=q@���@��@��-@�A�@���@��-@���@�1@���@�?}@��P@�-@�/@��@��!@�X@�C�@��@�dZ@���@��/@���@�v�@�7L@�9X@�S�@�{@��/@
=@|z�@{S�@y�^@w;d@u�@s��@r��@q��@o��@n@kƨ@i�#@g��@f5?@d�j@cC�@a�@`r�@_;d@^{@\z�@Z=q@WK�@U��@Tj@SC�@Q��@Pb@Nȴ@L��@L�@K33@Jn�@HĜ@G�w@F5?@E�@C33@A��@@  @>�+@=�h@=/@;��@:��@9&�@81'@6�@5�-@5/@49X@2~�@1�@/�P@.E�@,�@+�m@*J@)G�@( �@&V@%`B@$�@#�
@"~�@!x�@ �@  �@K�@E�@��@�j@��@�!@J@X@bN@�@
=@5?@�@��@(�@33@��@-@�#@�`@ �@;d@��@�@`B@/@Z@�m@C�@
�!@	��@	G�@�9@bN@|�@�@�R@ff@��@�h@V@I�@1@t�@-@ ��@ b?���?��?��?�?�X?��?�`B?�9X?��?��?�&�?� �?�R?�{?�V?�ƨ?�C�?�^5?���?�Q�?�+?�?��?��?�S�?�n�?�7?�bN?��;?�;d?�V?ݲ-?�V?�I�?�dZ?��H?��#?��?ؓu?���?�E�?�Z?�33?ѩ�?� �?�V?�O�?�ƨ?ʟ�?�X?�Q�?�K�?��T?��?��?�A�?���?�{?��?�1?�C�?�^5?��^?�x�?�1'?��?�+?���?��?��/?��j?��
?��?�n�?���?���?���?���?��7?��7?��7?���?���?��?��?�J?�J?�-?�-?�M�?�n�?�n�?��\?��!?��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��;A���A�A�VA� �A�+A�+A�-A�/A�1'A�-A�-A�/A�/A�/A�/A�1'A�-A�/A�/A�/A�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	;dB	_;B	n�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	}�B	K�B	D�B	T�B	�B	�?B	��B
K�B
|�B
�bB
��B
�!B
�LB
�qB
�RB
��B
��B
�;BI�B�B
�B
��B
ĜB
�B
�B
�B
�B
��B
�B
�jB%B
�B
��B
ÖB
�B
�B
\)B
;dB
JB	�B	�uB	cTB	>wB	.B	{B	B��B�mB�/BŢB�B��B��B�uB��B��B�dB��B�B�
B�TB��B	PB	�B	�B	7LB	D�B	ZB	n�B	p�B	�B	�?B	ÖB	�;B	��B	��B	ĜB	��B	��B	�#B	�B
B
JB
�B
�B
#�B
/B
:^B
;dB
?}B
E�B
H�B
J�B
O�B
O�B
O�B
N�B
O�B
T�B
ZB
]/B
ZB
YB
XB
ZB
ZB
\)B
[#B
YB
XB
YB
XB
W
B
VB
W
B
XB
XB
W
B
T�B
R�B
Q�B
Q�B
P�B
P�B
Q�B
Q�B
R�B
S�B
R�B
T�B
S�B
R�B
S�B
S�B
Q�B
P�B
O�B
N�B
N�B
N�B
M�B
M�B
N�B
K�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
L�B
L�B
J�B
J�B
K�B
J�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
M�B
N�B
M�B
J�B
J�B
I�B
J�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
E�B
D�B
B�B
@�B
@�B
>wB
<jB
<jB
<jB
;dB
:^B
9XB
9XB
7LB
7LB
6FB
5?B
5?B
49B
49B
33B
2-B
0!B
2-B
5?B
5?B
5?B
6FB
49B
5?B
49B
33B
33B
2-B
2-B
2-B
2-B
1'B
2-B
2-B
2-B
1'B
2-B
'�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
uB
{B
VB
VB
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
"�B
#�B
$�B
%�B
%�B
'�B
'�B
,B
.B
0!B
0!B
1'B
1'B
33B
49B
6FB
7LB
8RB
:^B
;dB
=qB
=qB
>wB
@�B
A�B
B�B
B�B
B�B
D�B
E�B
F�B
H�B
I�B
I�B
K�B
L�B
L�B
M�B
N�B
N�B
P�B
Q�B
S�B
T�B
VB
VB
XB
YB
ZB
ZB
ZB
[#B
[#B
]/B
]/B
^5B
^5B
_;B
aHB
bNB
bNB
cTB
cTB
cTB
e`B
ffB
gmB
gmB
gmB
hsB
hsB
jB
k�B
k�B
m�B
m�B
n�B
p�B
p�B
r�B
s�B
s�B
s�B
t�B
u�B
u�B
v�B
v�B
x�B
x�B
x�B
y�B
z�B
{�B
{�B
|�B
}�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�1B
�1B
�7B
�=B
�DB
�=B
�DB
�DB
�JB
�JB
�PB
�PB
�VB
�PB
�\B
�\B
�\B
�\B
�bB
�hB
�oB
�uB
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
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�!B
�'B
�'B
�-B
�3B
�3B
�9B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�XB
�XB
�^B
�jB
�dB
�qB
�jB
�wB
�wB
�}B
��B
��B
��B
��B
B
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ĜB
ŢB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ĜB
ĜB
ĜB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	6FB	5?B	F�B	XB	aHB	dZB	iyB	n�B	t�B	{�B	}�B	}�B	~�B	� B	� B	� B	�B	� B	� B	� B	� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B	zB	tB	nB	�B	;?B	_B	ntB	|�B	~�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	}�B	K�B	D�B	T�B	��B	�-B	��B
K�B
|�B
�RB
��B
�B
�<B
�bB
�CB
��B
��B
�-BI�B�B
�B
��B
ĐB
�B
�	B
�	B
��B
��B
��B
�aBB
�B
��B
ÏB
��B
�B
\"B
;^B
DB	�B	�oB	cNB	>qB	.B	uB	B��B�hB�+BŞB�B��B��B�rB��B��B�aB��B�B�	B�SB��B	PB	�B	�B	7NB	D�B	Z B	n�B	p�B	�B	�DB	ÜB	�AB	��B	��B	ģB	��B	��B	�,B	�B
B
UB
�B
�B
#�B
/(B
:lB
;rB
?�B
E�B
H�B
J�B
O�B
O�B
O�B
N�B
O�B
UB
Z1B
]DB
Z3B
Y-B
X'B
Z4B
Z5B
\AB
[<B
Y0B
X*B
Y1B
X+B
W&B
V B
W'B
X-B
X.B
W(B
UB
SB
RB
RB
QB
QB
RB
RB
SB
TB
SB
U#B
TB
SB
TB
TB
RB
QB
PB
OB
OB
OB
M�B
M�B
OB
K�B
N B
NB
L�B
NB
NB
NB
NB
OB
L�B
M B
J�B
J�B
K�B
J�B
MB
MB
K�B
K�B
K�B
MB
MB
NB
OB
NB
J�B
J�B
I�B
J�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
E�B
D�B
B�B
@�B
@�B
>�B
<�B
<�B
<�B
;�B
:�B
9�B
9�B
7�B
7�B
6�B
5�B
5�B
4�B
4�B
3�B
2zB
0oB
2{B
5�B
5�B
5�B
6�B
4�B
5�B
4�B
3�B
3�B
2�B
2�B
2�B
2�B
1}B
2�B
2�B
2�B
1�B
2�B
(OB
$9B
"0B
 &B
B
B
B
B
B
B
B
	B
B
B
B
B
B
B
�B
�B
�B
B
/B
,B
;B
EB
4B
JB
4B
=B
YB
bB
dB
mB
pB
B
�B
 �B
!�B
 �B
#�B
$�B
%�B
&�B
&�B
(�B
(�B
,�B
.�B
1B
1B
2B
2B
4,B
55B
7EB
8MB
9VB
;eB
<nB
>}B
>�B
?�B
A�B
B�B
C�B
C�B
C�B
E�B
F�B
G�B
I�B
J�B
J�B
L�B
NB
N	B
OB
PB
PB
R-B
S7B
UFB
VOB
WXB
W[B
YjB
ZtB
[}B
[�B
[�B
\�B
\�B
^�B
^�B
_�B
_�B
`�B
b�B
c�B
c�B
d�B
d�B
d�B
f�B
g�B
iB
iB
iB
jB
jB
l!B
m*B
m,B
o;B
o>B
pGB
rVB
rYB
thB
upB
usB
uvB
v~B
w�B
w�B
x�B
x�B
z�B
z�B
z�B
{�B
|�B
}�B
}�B
~�B
�B
�B
��B
��B
��B
��B
��B
�B
�	B
�B
�B
�B
� B
�)B
�+B
�4B
�<B
�>B
�GB
�OB
�YB
�TB
�]B
�`B
�iB
�kB
�tB
�vB
�B
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
�B
�B
�!B
�-B
�>B
�>B
�RB
�]B
�bB
�oB
�vB
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
�B
�B
�B
�&B
�-B
�+B
�8B
�EB
�JB
�WB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�#B
�8B
�NB
�cB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�7B
�FB
�[B
�qB
ŁB
ƕB
ƥB
ǺB
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�#B
�'B
�)B
�3B
�5B
�9B
�;B
�9B
�;B
�?B	zB	tB	zB	zB	zB	tB	zB	zB	zB	zB	�B	tB	�B	�B	�B	�B	�B	zB	mB	mB	tB	tB	mB	mB	mB	mB	mB	gB	mB	mB	tB	zB	tB	{B	nB	hB	�B	hB	#�B	6!B	5B	F�B	W�B	a#B	d6B	iUB	ntB	t�B	{�B	}�B	}�B	~�B	�B	�B	�B	��B	�B	�B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203012021061413572920210614135729202107221611292021072216112920210722161129201807242203012021061413572920210614135729202107221611292021072216112920210722161129PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030120180724220301  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030120180724220301QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030120180724220301QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�40000           0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141920210722161419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                