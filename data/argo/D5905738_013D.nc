CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:34Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Q   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 <  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ܈   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20180724220234  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�jvOr�@�jvOr�11  @�jvDD_P@�jvDD_P@7���ݘ@7���ݘ�c���zN{�c���zN{11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?fff?�33@@  @�  @�  @�33@�33A��A��A#33AA��Aa��A~ffA���A���A���A���Aљ�A�  A�B ��B  B��B  B   B(ffB0��B8ffB@ffBHffBPffBX��B`  BhffBpffBx  B�  B�  B�  B�  B�33B�  B���B���B�  B�33B�33B�ffB�ffB�33B�  B�33B�  B���BǙ�B���B���B���B�ffB�33B�  B�  B���B�ffB�33B���B�33B���C 33C  C33C�C  C
33CL�C33C�fC�C33C  C��C�fC�CL�C 33C"  C$33C&�C'�fC*�C,33C.�C/��C1�fC4�C633C8ffC:33C;�fC>�C@33CBffCD33CF  CH�CJ33CLL�CN�CO��CR�CT  CU�fCX�CZ  C\  C^L�C`33Cb�Cc�fCe�fCh�CjL�Cl33Cn�Cp�Cq�fCt33Cv�Cx  Cz33C|33C~�C�  C��3C��C�  C��3C��C�  C��3C��C�33C��C�  C��C�  C��fC��C�&fC�&fC��C��fC�  C��C�&fC�&fC�  C��C��C��C��fC��fC��3C��3C��C��C�&fC��C�  C��C��C�&fC��C��3C�  C�  C�  C�  C��C��C��C��C��C��fC��3C�  C��C��C�&fC��C��3C��C�&fC�33C�&fC�  C�  C��C�  C��fC��3C�  C��C�  C�ٚC��fC��C�&fC��C��3C�  C�  C��fC�  C��C�  C��3C��C��C��C��3C��C�  C��fC�  C��C��C��C��C��fC�  C��C�  C��fC��3C�  C��C�&fC��C��3C��C��C�33C��C�  C��C��C��C�&fC�33C��C��fC��fC�  C��fC��C�&fC�ٚC��fC��3D   D �fD�D�3D�3DL�D�D	y�D  D�3D,�D�3D�fDFfD��D�3D!s3D$&fD&��D)ffD,�D.��D1Y�D3�3D6��D93D;��D>&fD@��DC@ DE�3DHS3DJ��DM  DO��DQ��DTFfDV�3DY3D[l�D]� D`3Dbs3Dd�3DgFfDi� Dl9�Dn� Dq@ Ds� DvS3DxٚD{s3D}�fD�33D��3D�� D�)�D�y�D��3D� D�S3D�� D�� D�  D�c3D�� D�� D�  D�,�D�Y�D��fD��3D�� D�	�D�33D�c3D�� D���D��D� D�,�D�S3D�� D���D��3D���D�)�D�VfD��fD���D���D�)�D�VfD�� D��fD�  D�6fD�i�D�� D�� D�  D�)�D�Y�D�� D���D��3D�  D�,�D�VfD�|�Dģ3D�� D��D�3D�#3D�9�D�I�D�VfD�c3D�s3D�|�DЌ�DіfDҠ DӦfDԩ�Dճ3Dּ�D��3D���D��fD��3D��fD�3D� D�#3D�33D�FfD�S3D�l�D�3D�fD橚D��D�ٚD���D�3D�33D�L�D�l�D��D�D��fD���D�)�D�VfD�|�D�� D���D�fD��D�@ D�ffD�� D��fE p E�E��E1�E��Ed�E� E��E!�E� EL�E�3Et�E� E	�fEq�E��E��E+3E1�E�fE��E� EH EI�E�fE9�E;3E�3E��E�fE9�E ��E!��E#!�E$�E%��E&�3E(�E(�fE*vfE+�E,� E.ffE/a�E0�3E1�fE3H E4�fE5�fE7	�E8i�E9L�E:�fE;��E>�EB6fEE�EH@ EK� EN��EQ� ET�fEX&fE[K3E^a�Eaq�Edx Eg� Ej��Em�3Ep��Et8 Ew$�EzNfE}��E�U�E���E�h�E� E���E� E���E�)�E���E�3E�T E���E��fE�G3E��fE��E�8 E�� E��3E�2fE�| E��fE�'3E�nfE��fE��E�Y�E��3E�3E�L E��fE��E�6fE��3E�� E�&fE�s3E��3>���?   >���?   >���?   ?   ?   ?   >���?   ?   ?��?   ?333?333?fff?�  ?�  ?�ff?�33?���?�ff@ff@��@&ff@333@Fff@Y��@l��@�33@���@���@���@���@�ff@�33@���@ٙ�@陚@�ffA��A  A��A��A��A#33A+33A333A9��AA��AH  AP  AX  A\��AfffAl��At��A{33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414444141414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?�  ?�33@��@`  @�  @�  @�33@�33A	��A��A+33AI��Ai��A�33A���A���A���Ař�Aՙ�A�  A���B��B
  B��B  B"  B*ffB2��B:ffBBffBJffBRffBZ��Bb  BjffBrffBz  B�  B�  B�  B�  B�33B�  B���B���B�  B�33B�33B�ffB�ffB�33B�  B�33B�  B���Bș�B���B���B���B�ffB�33B�  B�  B���B�ffB�33B���B�33B���C �3C� C�3C��C� C
�3C��C�3CffC��C�3C� CL�CffC��C��C �3C"� C$�3C&��C(ffC*��C,�3C.��C0L�C2ffC4��C6�3C8�fC:�3C<ffC>��C@�3CB�fCD�3CF� CH��CJ�3CL��CN��CPL�CR��CT� CVffCX��CZ� C\� C^��C`�3Cb��CdffCfffCh��Cj��Cl�3Cn��Cp��CrffCt�3Cv��Cx� Cz�3C|�3C~��C�@ C�33C�L�C�@ C�33C�Y�C�@ C�33C�L�C�s3C�Y�C�@ C�Y�C�@ C�&fC�L�C�ffC�ffC�L�C�&fC�@ C�Y�C�ffC�ffC�@ C�Y�C�Y�C�L�C�&fC�&fC�33C�33C�L�C�Y�C�ffC�Y�C�@ C�L�C�Y�C�ffC�L�C�33C�@ C�@ C�@ C�@ C�L�C�L�C�Y�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�ffC�Y�C�33C�L�C�ffC�s3C�ffC�@ C�@ C�Y�C�@ C�&fC�33C�@ C�Y�C�@ C��C�&fC�L�C�ffC�L�C�33C�@ C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�Y�C�L�C�&fC�@ C�L�C�@ C�&fC�33C�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�s3C�Y�C�@ C�L�C�L�C�Y�C�ffC�s3C�L�C�&fC�&fC�@ C�&fC�Y�C�ffC��C�&fC�33D   D �fD,�D�3D3Dl�D9�D	��D  D�3DL�D�3D�fDffD�D�3D!�3D$FfD&��D)�fD,9�D.ٚD1y�D43D6��D933D;��D>FfD@��DC` DE�3DHs3DJٚDM@ DO��DR�DTffDV�3DY33D[��D]� D`33Db�3Dd�3DgffDi� DlY�Dn� Dq` Ds� Dvs3Dx��D{�3D}�fD�C3D��3D�� D�9�D���D��3D�  D�c3D�� D�� D�0 D�s3D�� D�� D� D�<�D�i�D��fD��3D�� D��D�C3D�s3D�� D���D���D�  D�<�D�c3D�� D���D��3D�	�D�9�D�ffD��fD���D���D�9�D�ffD�� D��fD� D�FfD�y�D�� D�� D� D�9�D�i�D�� D���D��3D� D�<�D�ffDÌ�Dĳ3D�� D���D�3D�33D�I�D�Y�D�ffD�s3D΃3Dό�DМ�DѦfDҰ DӶfDԹ�D��3D���D��3D���D��fD��3D�fD�3D�  D�33D�C3D�VfD�c3D�|�D�3D�fD湚D���D��D�	�D�#3D�C3D�\�D�|�D��D�D��fD��D�9�D�ffD���D�� D���D�fD�)�D�P D�vfD�� D��fE x E�E��E9�E��El�E  E��E)�E� ET�E�3E|�E� E	�fEy�E��E��E33E9�E�fE��E� EP EQ�E�fEA�EC3E�3EɚE�fEA�E ��E!��E#)�E$$�E%��E&�3E(�E)fE*~fE+��E,� E.nfE/i�E0�3E1�fE3P E4�fE5�fE7�E8q�E9T�E:�fE;��E>�EB>fEE�EHH EK� ENɚEQ� EUfEX.fE[S3E^i�Eay�Ed� Eg� Ej��Em�3Ep��Et@ Ew,�EzVfE}��E�Y�E���E�l�E� E���E� E���E�-�E���E�3E�X E���E��fE�K3E��fE��E�< E�� E��3E�6fE�� E��fE�+3E�rfE��fE��E�]�E��3E�3E�P E��fE���E�:fE��3E�� E�*fE�w3E��3?fffG�O�?fffG�O�?fffG�O�G�O�G�O�G�O�?fffG�O�?�  G�O�?�  G�O�?���?�33G�O�?�  ?�ff?�33@ff@33@&ff@,��@Fff@S33@fff@y��@�ff@�33@���@���@���@���@�ff@�33@���@陚@���A33A	��A  A��A��A$��A+33A333A;33AA��AI��AP  AX  A`  Ad��AnffAt��A|��A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414444141414114111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ j@ %@ �@ {@ O@ ""@ )�@ 0x@ 7L@ >@ E�@ R�@ `B@ l�@ {�@ �7@ ��@ �5@ ��@ �&@ �*@ ��@ �@ �@j@@g@-�@:�@H]@V@c�@r@~K@��@�H@�A@��@@�7@��@�4@�,@%@�@""@0x@>@Lu@Z@g@t@�d@�\@�U@�M@��@�J@��@�@�@��@
=@6@&�@3�@@,@O0@^5@k.@ww@�|@�u@�m@�r@��@��@�[@�`@�@  @J@�@)�@8�@E�@Q�@`�@m�@z3@�7@��@��@�!@�w@�|@��@��@� @�@�@ @/@;d@G�@V@dZ@r�@~�@��@�H@�A@�9@�>@�7@��@�@��@�@�@!s@0x@?}@Lu@Yn@g@s_@�@�@�@�@��@ƨ@Ӡ@��@�@��@	�@B@%�@2�@A�@Q=@]�@i�@x�@�@�h@�@�!@��@�@խ@�@�@@�@O@*S@7�@D�@P�@^5@l�@z3@�7@��@��@��@�&@�|@��@�(@�q@	�@	@	�@	,`@	:@	H]@	V@	dZ@	r@	~�@	��@	��@	�A@	��@	��@	�C@	�;@	��@	��@
�@
6@
$.@
/�@
=q@
Lu@
X�@
e	@
s_@
��@
��@
�@
��@
��@
ƨ@
խ@
��@
�@@
��@
=@�@%�@4�@@�@M�@\�@k.@x&@�p@�u@�m@��@��@��@׹@�@�@��@�@�@(�@5?@C�@Q�@`�@oF@{�@��@��@�5@�9@��@��@�#@��@� @v@�@g@+@8�@G�@S�@dZ@r�@|?@��@��@�A@��@��@�C@܀@�@i!@��@��@5?@|?@Ĝ@V@Yn@�(@�@8�@�d@��@�@[z@�(@��@1�@x�@��@�@H]@�P@Ӡ@�@^5@��@�H@#�@dZ@��@�@'�@g�@�A@�@'�@hs@�Y@��@2�@ww@��@  @FQ@�D@�C@V@Yn@�@�@1�@y�@��@�@K�@��@�
@O@`A@��@�@%�@e�@��@�@ &;@ ff@ ��@ �`@!&;@!ff@!��@!�@"%�@"bN@"�@"�H@# �@#`A@#�@#��@$ @$`�@$�(@$�@%'�@%g�@%��@%��@&/�@&r@&��@&��@'6�@'ww@'��@'��@(6�@(v�@(��@(��@)5�@)uk@)�9@)�@*/�@*oF@*�Y@*��@+$/@+^5@+��@+��@,
�@,C�@,}�@,�F@,��@-&�@-^5@-��@-ψ@.�@.@,@.x�@.�-@.��@/&;@/_�@/�H@/�O@0@0H]@0�p@0��@0��@15@@1o�@1��@1�(@2&;@2c�@2��@2�/@3�@3Wb@3��@3�\@4�@4V�@4��@4�
@56@5V�@5�i@5�7@6@6N�@6�P@6��@7J@7M$@7��@7��@8�@8N�@8��@8�|@9�@9M$@9�P@9�o@:K@:ȴ@;~�@;�q@<j@=�@=��@>/@>�@?�@?�~@@g@@��@A`B@A�*@Br@B��@CM�@C�@D�i@D�9@E�I@Fv@F��@Go@G�!@H�@H�@I\�@I�c@Ji!@J�O@Kuk@K�/@L~K@M�@M�@N�@N��@O�@O�(@P1�@Qt�@R�/@T�@Up�@V�@X7L@Y��@Z��@\9X@]��@^��@`0x@az�@b�@dg@euk@f�@h33@ir�@j�@l.l@m�@n�O@p"�@q��@r�|@t@u~�@v��@x"�@xi!@x��@xލ@y$.@yi!@y� @y�@z6�@zww@z�@{I@{K@{��@{�/@|�@|Wa@|��@|�@}!s@}s_@}�~@}�@~>�@~y�@~ȴ@�@FP@��@��@ �G�O�@ �G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�@ jG�O�@ �@ %G�O�@ �@ �@ 	�@ 
�@ J@ V@ @ �@ @ *@ 6@ B@ �@ [@  �@ !s@ $�@ &�@ )�@ +�@ .l@ 1�@ 4�@ 7L@ :@ <@ ?}@ B�@ E�@ I@ Lu@ O0@ R�@ UU@ X�@ \)@ ^5@ bN@ e	@ hs@ k.@ m�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��HA��A���A�ȴA���A�A;wA;wAͰ!AͮA͕�AͅA�~�A�|�A�x�A�v�A�t�A�r�A�p�A�hsA̺^A˓uA���Aƛ�Aś�A�dZA�A�dZA��TA�$�A�%A�%A���A�bA��
A�{A��A�M�A�^5A�?}A��A���A�x�A��jA�(�A�A��A��
A�bNA��yA���A�hsA�1A�S�A�{A�A��uA�bA�O�A��uA�/A�
=A��A��A��HA�|�A� �A� �A�bA� �A���A�XA��A�9XA�"�A�VA�ƨA�z�A�;dA��`A��-A��\A�O�A��mA��wA��A��A���A���A�1A�&�A�E�A��A��yA��yA��hA���A�=qA��FA���A�r�A�{A��uA�A��A�A�O�A���A��
A���A��\A�&�A���A��yA�&�A��PA�$�A�{A���A���A��\A��FA� �A�K�AC�A}�
A|1'Ax�AvQ�AtM�Aq�
Ao�FAnv�An  AmdZAkXAix�Ah1'AfZAd1'AaƨA_�A]�A\�RA[�#AY�7AX~�AX{AY
=AYC�AW�TAU?}AT��AT(�AR��AQ�hAQVAPffAN�AL^5AJ�uAI��AF�HAD�!AC�ABffA@��A?��A>bNA=dZA<(�A;l�A;%A:��A:ȴA:$�A8�A6��A4�A3/A2A�A/��A-�A,�A+&�A)33A'��A&-A%oA$�RA#�hA"v�A!�A -A�7A��A9XA�#A+A�`A��A-A��Av�A��A+A �A�jAdZA�
A\)Az�A�wA�A�^A1A
jA	��A1Az�A�mA�PAVA��A~�A��A��A�HA{A��A��A �A =q@��+@��m@�&�@��!@��#@�p�@��@�1'@�+@�/@��
@�n�@�J@�h@�O�@�n�@柾@�@�7@��@�|�@���@�!@�G�@��T@ݙ�@��@۝�@��y@��@�%@��
@׮@�;d@�p�@���@�O�@�b@���@���@�G�@��@���@�J@�=q@��R@��@��u@�
=@���@�$�@�b@���@�33@���@�r�@��w@�C�@�K�@��^@��@��D@���@���@��7@�%@�Z@��@��P@��@�&�@�(�@�n�@���@���@���@�X@���@��F@���@�{@�p�@��u@~�+@}�@|1@z�H@y��@x��@w��@v$�@sƨ@q�#@o�@nV@l�/@k33@h�`@f�+@d1@co@a7L@^�R@]�-@\9X@Z�@Z�@YX@W�@V$�@U`B@TI�@R~�@O��@Nff@M�@L9X@J�H@JM�@Hr�@G��@F��@E/@B�@B-@@Ĝ@?�@>{@<�@:�@:J@8�`@8 �@7\)@6ff@5@4��@3o@2�@0��@/�w@/�@.��@-��@,��@+ƨ@*�@*-@)��@)&�@(Q�@'�@&�y@%��@$z�@#�@"�\@!��@ �u@+@��@@�j@t�@��@^5@G�@ �@l�@E�@��@/@�/@1@C�@�H@J@J@x�@ �@K�@�R@�@p�@Z@��@@
-@	7L@��@ �@\)@�@�T@?}@�@��@I�@t�@�@J@%@ Ĝ@ �9@ bN?��w?��R?���?�(�?�x�?�1'?�
=?��
?���?� �?�v�?�h?�C�?���?�b?�+?�?��?�o?��?�A�?�V?��?�ƨ?��H?��?ش9?׍P?׍P?�ff?�?ԛ�?��
?�S�?�-?�%?� �?ϝ�?��?Η�?Ͳ-?�O�?�j?�ƨ?ɺ^?ȴ9?�
=?Ł?��
?�n�?�G�?��?�\)?��R?�V?�V?���?�O�?�V?���?��D?��?��D?��?��?��?���?��?�/?�p�?��h?�5??�v�?���?��?�;d?�;d?�\)?�|�?���?��w?��w?��;?�  ?�  ?�A�?�bN?��?���?�Ĝ?��`?�%?�%?�&�?�&�?�G�?�hs?�hs?�&�?�%?�%?�%?�&�A���A��/A��;A��/A��yA��#A��/A��HA��HA��`A��`A��yA��`A��yA��A��A��yA���A���A�A�  A���A���A���A���A��
A���A�A���A;wA���A�A�A�A�A���A;wA;wA;wA;wA͸RAͧ�AͮAͮAͮAͣ�A͑hA͋DA͇+A͇+A͉7A̓A̓ÁA�~�A�~�A�z�A�|�A�~�A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A��HA��A���A�ȴA���A�A;wA;wAͰ!AͮA͕�AͅA�~�A�|�A�x�A�v�A�t�A�r�A�p�A�hsA̺^A˓uA���Aƛ�Aś�A�dZA�A�dZA��TA�$�A�%A�%A���A�bA��
A�{A��A�M�A�^5A�?}A��A���A�x�A��jA�(�A�A��A��
A�bNA��yA���A�hsA�1A�S�A�{A�A��uA�bA�O�A��uA�/A�
=A��A��A��HA�|�A� �A� �A�bA� �A���A�XA��A�9XA�"�A�VA�ƨA�z�A�;dA��`A��-A��\A�O�A��mA��wA��A��A���A���A�1A�&�A�E�A��A��yA��yA��hA���A�=qA��FA���A�r�A�{A��uA�A��A�A�O�A���A��
A���A��\A�&�A���A��yA�&�A��PA�$�A�{A���A���A��\A��FA� �A�K�AC�A}�
A|1'Ax�AvQ�AtM�Aq�
Ao�FAnv�An  AmdZAkXAix�Ah1'AfZAd1'AaƨA_�A]�A\�RA[�#AY�7AX~�AX{AY
=AYC�AW�TAU?}AT��AT(�AR��AQ�hAQVAPffAN�AL^5AJ�uAI��AF�HAD�!AC�ABffA@��A?��A>bNA=dZA<(�A;l�A;%A:��A:ȴA:$�A8�A6��A4�A3/A2A�A/��A-�A,�A+&�A)33A'��A&-A%oA$�RA#�hA"v�A!�A -A�7A��A9XA�#A+A�`A��A-A��Av�A��A+A �A�jAdZA�
A\)Az�A�wA�A�^A1A
jA	��A1Az�A�mA�PAVA��A~�A��A��A�HA{A��A��A �A =q@��+@��m@�&�@��!@��#@�p�@��@�1'@�+@�/@��
@�n�@�J@�h@�O�@�n�@柾@�@�7@��@�|�@���@�!@�G�@��T@ݙ�@��@۝�@��y@��@�%@��
@׮@�;d@�p�@���@�O�@�b@���@���@�G�@��@���@�J@�=q@��R@��@��u@�
=@���@�$�@�b@���@�33@���@�r�@��w@�C�@�K�@��^@��@��D@���@���@��7@�%@�Z@��@��P@��@�&�@�(�@�n�@���@���@���@�X@���@��F@���@�{@�p�@��u@~�+@}�@|1@z�H@y��@x��@w��@v$�@sƨ@q�#@o�@nV@l�/@k33@h�`@f�+@d1@co@a7L@^�R@]�-@\9X@Z�@Z�@YX@W�@V$�@U`B@TI�@R~�@O��@Nff@M�@L9X@J�H@JM�@Hr�@G��@F��@E/@B�@B-@@Ĝ@?�@>{@<�@:�@:J@8�`@8 �@7\)@6ff@5@4��@3o@2�@0��@/�w@/�@.��@-��@,��@+ƨ@*�@*-@)��@)&�@(Q�@'�@&�y@%��@$z�@#�@"�\@!��@ �u@+@��@@�j@t�@��@^5@G�@ �@l�@E�@��@/@�/@1@C�@�H@J@J@x�@ �@K�@�R@�@p�@Z@��@@
-@	7L@��@ �@\)@�@�T@?}@�@��@I�@t�@�@J@%@ Ĝ@ �9@ bN?��w?��R?���?�(�?�x�?�1'?�
=?��
?���?� �?�v�?�h?�C�?���?�b?�+?�?��?�o?��?�A�?�V?��?�ƨ?��H?��?ش9?׍P?׍P?�ff?�?ԛ�?��
?�S�?�-?�%?� �?ϝ�?��?Η�?Ͳ-?�O�?�j?�ƨ?ɺ^?ȴ9?�
=?Ł?��
?�n�?�G�?��?�\)?��R?�V?�V?���?�O�?�V?���?��D?��?��D?��?��?��?���?��?�/?�p�?��h?�5??�v�?���?��?�;d?�;d?�\)?�|�?���?��w?��w?��;?�  ?�  ?�A�?�bN?��?���?�Ĝ?��`?�%?�%?�&�?�&�?�G�?�hs?�hs?�&�?�%?�%?�%?�&�A���A��/A��;A��/A��yA��#A��/A��HA��HA��`A��`A��yA��`A��yA��A��A��yA���A���A�A�  A���A���A���A���A��
A���A�A���A;wA���A�A�A�A�A���A;wA;wA;wA;wA͸RAͧ�AͮAͮAͮAͣ�A͑hA͋DA͇+A͇+A͉7A̓A̓ÁA�~�A�~�A�z�A�|�A�~�A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��BBt�B�B�`B�NB�/B�ZB�B��B+B!�B%�B'�B(�B5?B6FB7LB:^BH�BF�BB�BO�BR�BZB[#B[#B\)BcTBhsBhsBk�Bo�Bw�Bw�Bx�By�B|�B�B� B�B�B� B�B�1B�PB�1B�B� Bw�Bq�Bm�BhsBW
BR�BO�BK�BN�BVBYBdZBn�Bn�BiyBn�B\)BJ�B;dB6FB1'B�B��B�;BÖB�FB�B��B�JB�Bo�BcTBe`BQ�B:^B/B&�B�BDBB
�mB
��B
ȴB
��B
��B
ǮB
��B
�3B
��B
�+B
�B
}�B
m�B
R�B
F�B
>wB
/B
&�B

=B
B	�B	�5B	��B	B	�qB	�9B	��B	��B	�oB	~�B	t�B	e`B	aHB	YB	N�B	L�B	F�B	J�B	N�B	gmB	q�B	ffB	^5B	[#B	VB	L�B	G�B	F�B	A�B	2-B	&�B	!�B	�B	
=B	B��B�B�B�mB�;B�)B�
B��B�B��B��B��B��BǮB�-B�B��B��B�uB�{B�B�B�B�+B�B�B�By�Bs�Bn�Bp�Bl�Bq�Bs�Br�Bq�Bn�Bl�BjBffBffBe`BbNB`BB^5B`BB^5BZB\)BYBR�BK�BF�B?}B7LB5?B6FB33B33B33B1'B/B0!B/B.B/B-B-B.B-B1'B.B/B.B.B.B/B33B5?B5?B7LB7LB7LB7LB6FB;dB;dB:^B8RB8RB:^B9XB9XB9XBD�BD�BE�BD�BC�BJ�BQ�BP�BR�B`BB^5BXB_;Bm�By�By�B�PB�\B��B�B��BÖB��B�B�`B	B	1B	�B	2-B	49B	C�B	Q�B	aHB	y�B	�B	�=B	�{B	��B	��B	��B	�B	�?B	�jB	ÖB	ɺB	��B	��B	�B	�B	�/B	�NB	�sB	�yB	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
B
+B

=B
JB
VB
bB
oB
uB
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
%�B
%�B
'�B
)�B
+B
,B
.B
1'B
2-B
2-B
33B
5?B
5?B
8RB
7LB
9XB
;dB
=qB
=qB
>wB
?}B
@�B
C�B
C�B
D�B
E�B
G�B
F�B
G�B
H�B
I�B
K�B
L�B
M�B
N�B
N�B
N�B
P�B
R�B
Q�B
R�B
S�B
S�B
T�B
W
B
VB
XB
YB
ZB
\)B
\)B
^5B
_;B
`BB
`BB
`BB
aHB
cTB
dZB
dZB
ffB
hsB
hsB
jB
jB
k�B
jB
l�B
m�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
w�B
x�B
y�B
z�B
z�B
z�B
z�B
|�B
|�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�%B
�7B
�=B
�DB
�DB
�JB
�VB
�\B
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
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�?B
�FB
�?B
�FB
�LB
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
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
�dB
�dB
�jB
�dB
�jB
�^B
�dB
�jB
�jB
�dB
�dB
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�dB
�qB
�jB
�jB
�jB
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B
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
��B  Bs�B�
B�ZB�HB�)B�TB�B��B%B �B$�B&�B'�B49B5?B6FB9XBG�BE�BA�BN�BQ�BYBZBZB[#BbNBgmBgmBjBn�Bv�Bv�Bw�Bx�B{�B�B~�B�B�B~�B�B�+B�JB�+B�B~�Bv�Bp�Bl�BgmBVBQ�BN�BJ�BM�BT�BXBcTBm�Bm�BhsBm�B[#BI�B:^B5?B0!B�B��B�5BB�?B�B��B�DB�Bn�BbNBdZBP�B9XB.B%�B�B
=BB
�fB
��B
ǮB
��B
��B
ƨB
��B
�-B
��B
�%B
� B
|�B
l�B
Q�B
E�B
=qB
.B
%�B
	7B
B	�B	�/B	��B	��B	�jB	�3B	��B	��B	�hB	}�B	s�B	dZB	`BB	XB	M�B	K�B	E�B	I�B	M�B	ffB	p�B	e`B	]/B	ZB	T�B	K�B	F�B	E�B	@�B	1'B	%�B	 �B	�B		7B	B��B�B�B�fB�5B�#B�B��B��B��B��B��B��BƨB�'B�B��B��B�oB�uB�B� B�B�%B�B�B�Bx�Br�Bm�Bo�Bk�Bp�Br�Bq�Bp�Bm�Bk�BiyBe`Be`BdZBaHB_;B]/B_;B]/BYB[#BXBQ�BJ�BE�B>wB6FB49B5?B2-B2-B2-B0!B.B/B.B-B.B,B,B-B,B0!B-B.B-B-B-B.B2-B49B49B6FB6FB6FB6FB5?B:^B:^B9XB7LB7LB9XB8RB8RB8RBC�BC�BD�BC�BB�BI�BP�BO�BQ�B_;B]/BW
B^5Bl�Bx�Bx�B�JB�VB��B�B��BBɺB�B�ZB	B	+B	�B	1'B	33B	B�B	P�B	`BB	x�B	�B	�7B	�uB	��B	��B	��B	��B	�9B	�dB	B	ȴB	��B	��B	��B	�B	�/B	�NB	�sB	�yB	�B	�B	�B	��B	��B	��B	��B
  B
B
B
B
B
+B

=B
JB
VB
bB
oB
uB
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
%�B
%�B
'�B
)�B
+B
,B
.B
1'B
2-B
2-B
33B
5?B
5?B
8RB
7LB
9XB
;dB
=qB
=qB
>wB
?}B
@�B
C�B
C�B
D�B
E�B
G�B
F�B
G�B
H�B
I�B
K�B
L�B
M�B
N�B
N�B
N�B
P�B
R�B
Q�B
R�B
S�B
S�B
T�B
W
B
VB
XB
YB
ZB
\)B
\)B
^5B
_;B
`BB
`BB
`BB
aHB
cTB
dZB
dZB
ffB
hsB
hsB
k�B
k�B
l�B
k�B
m�B
n�B
o�B
o�B
o�B
p�B
q�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
v�B
w�B
x�B
x�B
y�B
z�B
{�B
{�B
{�B
{�B
}�B
}�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�=B
�DB
�JB
�JB
�PB
�\B
�bB
�hB
�oB
�oB
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
�B
�B
�B
�'B
�'B
�-B
�3B
�9B
�FB
�LB
�RB
�XB
�RB
�XB
�^B
�XB
�^B
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�wB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
��B
�wB
�}B
��B
��B
�}B
�}B
�}B
�}B
�}B
��B
��B
�}B
�}B
�}B
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202342021061413521420210614135214202106141746232021061417462320210614174623201807242202342021061413521420210614135214202106141746232021061417462320210614174623PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023420180724220234  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023420180724220234QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023420180724220234QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                