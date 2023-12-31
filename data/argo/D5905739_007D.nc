CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:44Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  P0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ֌   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   <   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � \   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                        HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar            HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        (   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � |Argo profile    3.1 1.2 19500101000000  20180724220244  20210617131453  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�d<�3~@�d<�3~11  @�d;����@�d;����@6�`�N�@6�`�N��c�(�z�c�(�z11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@9��@�  @�ff@�33@���@���A��A&ffAC33Ac33A���A�  A�  A�  A�33Aљ�AᙚA�  A�33B��B  BffB ffB(ffB0  B8  B@ffBHffBQ33BXffB`��Bh��Bp��BxffB�33B�33B�  B�  B���B�  B�ffB�  B�33B�ffB�  B�33B�  B�33B�33B�  B�  B�  B�  B�33B�33B�33B�  B�  Bߙ�B���B�  B�33B�B���B�ffB���B���C�fC�fC  C  C	�fC�fC�fC�fC�fC�fC�fC�fC�fC��C��C�fC!�fC#�fC&  C(  C*  C,�C.33C033C233C433C633C8L�C:ffC<�C=��C?��CA��CC��CE��CG��CI�fCK�fCM�fCP  CR�CT�CV33CXffCZL�C\  C^  C`�Cb�Cd33Cf33Ch33Cj33Cl33Cn�Cp�Cr�Ct�Cv�Cx�Cz  C|�C~33C�&fC�&fC��C��C�&fC�&fC��C��C��C��C��C��C��C�  C��3C��3C��fC�ٚC�  C�&fC�&fC�&fC�&fC��C��C��C��C��C��C��C�&fC��C��fC��fC��3C�  C�  C��C��C�&fC��C��3C��3C��3C�  C��C��C�&fC�&fC��C��fC��3C�  C��C��C�  C��fC��fC�  C��C��C��C��C�  C��fC��fC�  C�  C��C��C��fC��fC��3C�  C��C��C��C��C��C��C��C��C�&fC��C��C��C��C�  C��3C��C�&fC��C��C�  C��3C��fC��fC��C��C��C��C�  C��3C��3C��fC�  C��C�  C��C�&fC��C��3C�  C��C��C��C��3C��C�  C��3C��fC�  C��fC�  C�33C�&fD3DS3D�fD
�D�fD�3DffD�fDl�D�3Dl�D�3D S3D"��D%9�D'�3D)�fD,9�D.��D0� D3&fD5l�D7��D9��D<S3D>� DA�DCs3DEٚDH9�DJ�3DL�3DO@ DQ�3DS�fDV  DXffDZ�3D]fD_S3Da�3Dc�3Df  Dh,�DjS3DlffDny�Dp� Dr�fDt��DvٚDx��Dz�3D|��D~��D�i�D�s3D�� D���D���D���D���D�ɚD�� D��3D��D�#3D�<�D�VfD�s3D���D�� D�� D��3D��D�FfD�l�D��3D��fD��3D��D�3D�  D�9�D�P D�c3D�s3D��fD���D���D���D�� D��D��fD�3D�3D�#3D�0 D�9�D�@ D�FfD�VfD�Y�D�i�D�s3D�|�D�� D��fD��fD��fD�� D��fD���D���D��fD�� D��fD��fD��D���D��D���D��fD���D��fD��fD���D���D���D�  D�3D�fD�	�D�3D��D��D��D��D�  D�#3D�,�D�,�D�)�D�0 D�0 D�9�D�<�D�FfD�I�D�L�D�L�D�S3D�\�D�` D�Y�D�S3D�FfD�<�D�0 D�)�D�  D��D��D���D���D�ٚD��fD��fD���D��fD��fD�s3D�c3D�L�D�#3D� D��fD���E4�E�fE� E��EQ�E9�E� E	��ET�E��E��E��E33E��E� EfEH E�3E��E�EK3E��E��E��E3E ��E!�3E"�fE$L�E%p E&��E(3E)3E*��E+��E-fE.  E/4�E0�fE1�3E38 E4P E5�3E6�fE7��E9l�E:�3E;��E>��EBfEEP EH��EK� EN��EQ�3ET�EX3E[fE^C3Eay�Ed�fEg�fEj�fEm��Enh En� Eo� Ep<�Eq�Eq�3ErC3Er� Est�Es� Et� EuP Eu�fEvq�Ew  Ew�3Exs3Ex��Ey�fEz.f>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?��?��?333?L��?���?�ff?�  ?���?�33@ff@33@   @,��@9��@L��@fff@l��@�ff@�  @���@�ff@�  @���@ə�@�ff@�33@�33@���A��A��A��A33A#33A+33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414444444441414441444414111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?L��?�33@&ff@Y��@�  @�ff@�33@���AffA��A.ffAK33Ak33A���A�  A�  A�  A�33Aՙ�A噚A�  B��B	��B  BffB"ffB*ffB2  B:  BBffBJffBS33BZffBb��Bj��Br��BzffB�33B�33B�  B�  B���B�  B�ffB�  B�33B�ffB�  B�33B�  B�33B�33B�  B�  B�  B�  B�33B�33B�33B�  B�  B���B���B�  B�33B�B���B�ffB���C ffCffCffC� C� C
ffCffCffCffCffCffCffCffCffCL�CL�C ffC"ffC$ffC&� C(� C*� C,��C.�3C0�3C2�3C4�3C6�3C8��C:�fC<��C>L�C@L�CBL�CDL�CFL�CHL�CJffCLffCNffCP� CR��CT��CV�3CX�fCZ��C\� C^� C`��Cb��Cd�3Cf�3Ch�3Cj�3Cl�3Cn��Cp��Cr��Ct��Cv��Cx��Cz� C|��C~�3C�ffC�ffC�Y�C�Y�C�ffC�ffC�Y�C�Y�C�L�C�Y�C�Y�C�Y�C�L�C�@ C�33C�33C�&fC��C�@ C�ffC�ffC�ffC�ffC�Y�C�Y�C�L�C�L�C�L�C�L�C�Y�C�ffC�L�C�&fC�&fC�33C�@ C�@ C�L�C�L�C�ffC�L�C�33C�33C�33C�@ C�L�C�L�C�ffC�ffC�L�C�&fC�33C�@ C�Y�C�Y�C�@ C�&fC�&fC�@ C�L�C�L�C�L�C�Y�C�@ C�&fC�&fC�@ C�@ C�Y�C�L�C�&fC�&fC�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�ffC�Y�C�Y�C�L�C�L�C�@ C�33C�L�C�ffC�Y�C�L�C�@ C�33C�&fC�&fC�L�C�Y�C�Y�C�L�C�@ C�33C�33C�&fC�@ C�Y�C�@ C�L�C�ffC�L�C�33C�@ C�L�C�L�C�Y�C�33C�L�C�@ C�33C�&fC�@ C�&fC�@ C�s3C�ffD33Ds3D�fD
9�D�fD3D�fDfD��D3D��D3D s3D"��D%Y�D'�3D*fD,Y�D.��D1  D3FfD5��D7ٚD:�D<s3D>� DA,�DC�3DE��DHY�DJ�3DM3DO` DQ�3DTfDV@ DX�fDZ�3D]&fD_s3Da�3Dc�3Df  DhL�Djs3Dl�fDn��Dp� Dr�fDtٚDv��Dy�D{3D|��D~ٚD�y�D��3D�� D���D���D���D�ɚD�ٚD�� D�3D��D�33D�L�D�ffD��3D���D�� D�� D�3D�)�D�VfD�|�D��3D��fD��3D���D�3D�0 D�I�D�` D�s3D��3D��fD���D�ɚD���D�� D���D�fD�3D�#3D�33D�@ D�I�D�P D�VfD�ffD�i�D�y�D��3D���D�� D��fD��fD��fD�� D��fD���D���D��fD�� D��fD��fD���D���D���D���D�fD�	�D�fD�fD�	�D�	�D��D� D�3D�fD��D�#3D�)�D�)�D�,�D�,�D�0 D�33D�<�D�<�D�9�D�@ D�@ D�I�D�L�D�VfD�Y�D�\�D�\�D�c3D�l�D�p D�i�D�c3D�VfD�L�D�@ D�9�D�0 D�)�D��D��D���D��D��fD��fD���D��fD��fD��3D�s3D�\�D�33D�  D�fD�ɚE<�E�fE� E��EY�EA�E� E
�E\�E��E��E��E;3E��E� EfEP E�3E��E�ES3E��E��E��E3E ��E!�3E"�fE$T�E%x E&��E(3E)3E*��E+��E-fE.( E/<�E0�fE1�3E3@ E4X E5�3E6�fE8�E9t�E:�3E;��E>��EB&fEEX EH��EK� EN��EQ�3ET�EX3E[&fE^K3Ea��Ed�fEg�fEj�fEm��Enp En� Eo� EpD�Eq	�Eq�3ErK3Er� Es|�Es� Et� EuX EvfEvy�Ew( Ew�3Ex{3Ex��Ey�fEz6f?L��G�O�?333G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�?fffG�O�?���?���?�ff?���?�ff@   @ff@��@&ff@333@@  @L��@Y��@l��@�33@�ff@�ff@�  @���@�ff@�  @���@ٙ�@�ff@�33A��AffA��A��A��A#33A+33A333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414444444441414441444414111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ @ %@ V@ �@ O@ #�@ )�@ /@ 5�@ >@ F�@ SI@ `�@ m�@ z�@ ��@ �0@ �(@ ��@ ��@ ��@ ��@ �m@ ��@@�@g@,`@:@H]@V@e	@qS@�@�P@��@��@��@�>@�7@��@��@�,@1@{@"�@1'@=q@K�@X�@g@t�@��@�\@�@��@�@ƨ@�O@�H@��@�9@	�@�@&;@5?@C�@O�@Z�@i!@v�@�p@��@�m@�f@�@ȴ@�[@�@�@�Q@�@�@'�@5?@C�@Q=@^�@m:@z�@��@��@�5@��@��@�*@��@�(@�~@@�@[@+@8�@FQ@S�@bN@o�@}�@��@�H@��@�F@�J@�C@��@�@��@�@�@#�@1'@>�@Lu@Yn@g@t�@�d@�@��@��@�@�W@խ@�T@�L@��@J@�@&�@4�@A�@O�@]�@k.@x&@�@�@��@��@��@�c@�@�@�e@@@�@)�@7L@D�@R�@`�@oF@{�@�+@��@�(@�~@�&@�|@�#@�(@�q@	�@	b@	
@	,`@	:�@	H]@	Wb@	e	@	qS@	|�@	�D@	��@	��@	�F@	@	��@	܀@	�@	��@
�@
*@
#�@
/�@
<@
I�@
X�@
ff@
uk@
�d@
��@
��@
��@
�R@
ƨ@
�O@
�@
�L@
��@�@B@&�@5?@B8@O�@\�@j@ww@�p@�u@�y@�r@�k@�c@�[@�T@��@ �@@�@)�@6�@C�@Q=@^5@m:@|?@��@��@��@�-@�w@��@�#@��@� @�@�@�@+�@8�@G�@S�@b�@s_@�W@�9@8�@x&@�@�E@?}@�d@ƨ@�@P�@�$@�@�@]�@��@��@g@^�@�a@��@�@Z@��@�\@�@UU@��@�@�@[z@��@܀@O@Z�@�H@�
@*@S�@�u@�C@�@M$@��@��@��@7L@o�@�A@��@�@T�@�P@Ĝ@�@+�@e	@��@�
@�@I�@��@��@��@33@m�@��@�`@!s@]�@�H@�\@{@Q�@�@��@ @ M�@ ��@ ��@!�@!B�@!~�@!��@!��@"33@"m�@"��@"�@#
@#Z�@#��@#�7@$�@$B8@${�@$��@$�@%(�@%a�@%��@%є@&�@&B�@&|�@&��@&�@@'%�@']�@'��@'є@(
=@(B8@(z3@(�9@(��@)%�@)]�@)�#@)�o@*�@*8�@*o�@*��@*��@+�@+Lu@+��@+�^@+�@,(�@,`A@,��@,��@-�@-?}@-v@-�f@-�@.O@.R�@.�D@.��@.��@//�@/ff@/�@/�\@0@0FQ@0}�@0�9@0�4@1$�@1\)@1�i@1ƨ@1��@2/@2b�@2�<@2��@3@35�@3i!@3�U@3��@4]@44�@4hs@4��@4�*@5 �@53�@5e�@5�u@5��@5� @6��@7 �@7��@8�@8��@9O1@9�-@:G�@:ލ@;r�@<@<e	@<��@=�|@>*@>�m@?(G@?�~@@;d@@Ĝ@AM$@A��@BYn@B܀@C^5@C��@D~�@D��@Ev�@F�@F�0@GV@G�r@H#�@H�2@I5@@I��@JK@J�2@KbN@K�h@Lww@L��@M�P@N%@N~�@O[@O�#@P1�@Qk�@R��@T/�@U��@V�H@X.l@Yr@Z�O@\*S@]}�@^��@`3�@a��@bȴ@d�@e|�@e�R@e�Y@fG�@f�W@f�O@g�@g]�@g��@g��@h{@h_�@h��@h�@i&;@ip�@i��@j]@j1�@jv�@j�w@ G�O�@ ^G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�@ �G�O�@ @ �@ v@ �@ �@ 
=@ 
�@ �@ V@ �@ @ o@ �@ �@ �@ B@ �@ �@  �@ #�@ %�@ (G@ +@ -�@ 0x@ 3�@ 5�@ 8�@ <@ ?}@ B8@ E�@ IG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AȬAȰ!Aȩ�Aȧ�AȮAȬAȬAȩ�Aȩ�Aȩ�AȬAȬAȮAȰ!AȃA�XA���AǮA�jA��yA�"�A��A�K�A�A���A�{A���A�I�A���A�v�A�?}A��HA�|�A�E�A�;dA��A�ȴA�33A��TA��wA��A��!A��A��DA�(�A�1A�p�A�{A���A��A��A�K�A�=qA�7LA�33A��A��A�~�A�{A���A�z�A�1'A���A���A��mA���A�
=A�&�A��\A�bA��A��!A�{A��/A���A��A��A���A���A�ĜA���A��A�l�A���A�`BA�+A��A��A�ZA���A�ĜA�jA�bA��PA���A�5?A�E�A��PA��+A� �A��/A��9A���A��A��+A��uA�VA��A��-A��7A�\)A�=qA��A��TA�1'A��7A�\)A���A�t�Ax�A~�`A~�A~(�A|JAx�HAxjAxbNAx�Aw�mAvz�As��Ar�uAq"�An��Al~�Aj�/Ah5?AfJAd(�AcdZAb�jAb�AbQ�Aa�wA_�TA\A�AY%AV��AV�AU��ATĜASK�AQ�hAN�AK�AH(�AFA�AE�AD�9AB�/A@�A=t�A< �A;��A;�
A:��A9��A9VA5\)A2��A0�A/�A/A.n�A.E�A.9XA-��A,v�A+�^A*��A)�TA(�jA(I�A'|�A& �A%�hA$�uA#��A#��A"r�A!��A!/A z�AAVAJAz�A�A��A/A�AAĜAbNA�TA7LA��A��AS�A/AA�/AȴA��A��A�+A�+Av�An�AffAVAI�A�FAĜAVA�hAK�AoA�hA�HA�A
��A	�mA�RA�A�/AZA&�A�!AO�@��F@���@�l�@�J@��@��\@�hs@��u@��@��@�ȴ@���@�E�@�%@��@���@�ff@�/@�7@��m@Ϯ@�`B@��@�|�@��@�Z@��7@��@�=q@��w@�{@��F@��/@��@��R@��D@��h@��;@�5?@��-@��@�+@�M�@�@���@�`B@��/@�ƨ@�"�@���@��\@�&�@�Z@�b@�^5@���@��F@�@�~�@��^@��@��@�t�@�5?@�&�@��`@��F@��@�@�X@��u@
=@~�@}�-@|�@z�!@y��@xr�@v��@u`B@s33@q��@q��@p�`@n�R@n{@m�T@m?}@l1@j^5@fff@e��@c�
@a��@_�;@_��@_l�@^E�@[�@ZM�@W��@V{@U�-@T�/@S�F@R�@O�P@N�+@M�T@M�-@MV@L9X@J�H@Ix�@I%@G��@F�@F@DZ@C@BJ@@��@@A�@?�@>��@=�T@<��@<1@;dZ@9�@9hs@7��@6E�@5�@3�m@1��@1&�@01'@/+@.$�@,�@+t�@*M�@)X@'�@&E�@%/@$j@#C�@"�H@"M�@!hs@ ��@ b@;d@�@�R@@/@�@�@S�@@J@��@��@��@��@ff@��@��@ƨ@@�!@^5@�7@%@�u@�@A�@��@�@��@$�@��@O�@��@��@��@dZ@
�\@	7L@�9@�u@�@�@V@z�@"�@�!@ ��?�V?�O�?�^5?�Q�?��T?�t�?�M�?�A�?��-?�1?��#?�Q�?�ff?䛦?�o?�&�?�|�?��?�V?ۅ?���?���?׍P?֧�?Ձ?�Z?�t�?�J?У�?�\)?�v�?��?˥�?��H?���?�X?ȓu?���?��y?�ff?�E�?�`B?���?�Z?��?�G�?�Ĝ?� �?�|�?�v�?��-?�O�?��?���?��?�C�?�?���?���?���?�~�?���?�~�?���?���?�~�?���?�^5?�~�?���?���?�^5?�=q?�^5?�~�?�^5?�^5?�^5?�^5?�~�AȮAȬAȩ�AȰ!AȲ-Aȩ�Aȩ�Aȟ�Aȥ�Aȩ�Aȩ�AȬAȧ�Aȡ�Aȣ�Aȥ�Aȩ�AȬAȬAȩ�AȮAȰ!AȰ!AȰ!AȰ!AȲ-AȲ-AȮAȮAȲ-AȰ!Aȩ�AȰ!AȬAȩ�AȬAȩ�Aȧ�Aȧ�Aȧ�Aȩ�AȬAȮAȬAȮAȬAȬAȬAȬAȩ�Aȩ�Aȩ�AȬAȩ�Aȧ�Aȧ�AȬAȬAȩ�AȮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               AȬAȰ!Aȩ�Aȧ�AȮAȬAȬAȩ�Aȩ�Aȩ�AȬAȬAȮAȰ!AȃA�XA���AǮA�jA��yA�"�A��A�K�A�A���A�{A���A�I�A���A�v�A�?}A��HA�|�A�E�A�;dA��A�ȴA�33A��TA��wA��A��!A��A��DA�(�A�1A�p�A�{A���A��A��A�K�A�=qA�7LA�33A��A��A�~�A�{A���A�z�A�1'A���A���A��mA���A�
=A�&�A��\A�bA��A��!A�{A��/A���A��A��A���A���A�ĜA���A��A�l�A���A�`BA�+A��A��A�ZA���A�ĜA�jA�bA��PA���A�5?A�E�A��PA��+A� �A��/A��9A���A��A��+A��uA�VA��A��-A��7A�\)A�=qA��A��TA�1'A��7A�\)A���A�t�Ax�A~�`A~�A~(�A|JAx�HAxjAxbNAx�Aw�mAvz�As��Ar�uAq"�An��Al~�Aj�/Ah5?AfJAd(�AcdZAb�jAb�AbQ�Aa�wA_�TA\A�AY%AV��AV�AU��ATĜASK�AQ�hAN�AK�AH(�AFA�AE�AD�9AB�/A@�A=t�A< �A;��A;�
A:��A9��A9VA5\)A2��A0�A/�A/A.n�A.E�A.9XA-��A,v�A+�^A*��A)�TA(�jA(I�A'|�A& �A%�hA$�uA#��A#��A"r�A!��A!/A z�AAVAJAz�A�A��A/A�AAĜAbNA�TA7LA��A��AS�A/AA�/AȴA��A��A�+A�+Av�An�AffAVAI�A�FAĜAVA�hAK�AoA�hA�HA�A
��A	�mA�RA�A�/AZA&�A�!AO�@��F@���@�l�@�J@��@��\@�hs@��u@��@��@�ȴ@���@�E�@�%@��@���@�ff@�/@�7@��m@Ϯ@�`B@��@�|�@��@�Z@��7@��@�=q@��w@�{@��F@��/@��@��R@��D@��h@��;@�5?@��-@��@�+@�M�@�@���@�`B@��/@�ƨ@�"�@���@��\@�&�@�Z@�b@�^5@���@��F@�@�~�@��^@��@��@�t�@�5?@�&�@��`@��F@��@�@�X@��u@
=@~�@}�-@|�@z�!@y��@xr�@v��@u`B@s33@q��@q��@p�`@n�R@n{@m�T@m?}@l1@j^5@fff@e��@c�
@a��@_�;@_��@_l�@^E�@[�@ZM�@W��@V{@U�-@T�/@S�F@R�@O�P@N�+@M�T@M�-@MV@L9X@J�H@Ix�@I%@G��@F�@F@DZ@C@BJ@@��@@A�@?�@>��@=�T@<��@<1@;dZ@9�@9hs@7��@6E�@5�@3�m@1��@1&�@01'@/+@.$�@,�@+t�@*M�@)X@'�@&E�@%/@$j@#C�@"�H@"M�@!hs@ ��@ b@;d@�@�R@@/@�@�@S�@@J@��@��@��@��@ff@��@��@ƨ@@�!@^5@�7@%@�u@�@A�@��@�@��@$�@��@O�@��@��@��@dZ@
�\@	7L@�9@�u@�@�@V@z�@"�@�!@ ��?�V?�O�?�^5?�Q�?��T?�t�?�M�?�A�?��-?�1?��#?�Q�?�ff?䛦?�o?�&�?�|�?��?�V?ۅ?���?���?׍P?֧�?Ձ?�Z?�t�?�J?У�?�\)?�v�?��?˥�?��H?���?�X?ȓu?���?��y?�ff?�E�?�`B?���?�Z?��?�G�?�Ĝ?� �?�|�?�v�?��-?�O�?��?���?��?�C�?�?���?���?���?�~�?���?�~�?���?���?�~�?���?�^5?�~�?���?���?�^5?�=q?�^5?�~�?�^5?�^5?�^5?�^5?�~�AȮAȬAȩ�AȰ!AȲ-Aȩ�Aȩ�Aȟ�Aȥ�Aȩ�Aȩ�AȬAȧ�Aȡ�Aȣ�Aȥ�Aȩ�AȬAȬAȩ�AȮAȰ!AȰ!AȰ!AȰ!AȲ-AȲ-AȮAȮAȲ-AȰ!Aȩ�AȰ!AȬAȩ�AȬAȩ�Aȧ�Aȧ�Aȧ�Aȩ�AȬAȮAȬAȮAȬAȬAȬAȬAȩ�Aȩ�Aȩ�AȬAȩ�Aȧ�Aȧ�AȬAȬAȩ�AȮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�
B
�B
�B
�
B
�
B
�B
�
B
�
B
�
B
�
B
�B
�
B
�
B
�B
�B
�
B
�/B
�ZB
�sB
�B
=B?}BYBt�B��B��B�RBB�5B�B�B�B�B��BB%BB��B
=B�B33B�BI�BjB�B�%B��B�B�!B�'B�-B�9B�?B�9B�9B�9B�LB�LB�dB�^B�wB�}B��B��B��B��B��BĜB��B��B��B�qB�-B�!B��B�DBy�BgmBQ�BL�BI�BF�BE�B<jB7LB6FB49B1'B)�B�BPB��B�TBȴB�XB�B�\B_;BM�BC�B?}B:^B-B&�B!�BVB
�B
�#B
�B
�B
��B
��B
��B
��B
��B
��B
��B
{�B
k�B
aHB
]/B
YB
T�B
@�B
2-B
0!B
.B
-B
+B
 �B
�B
PB
B	�B	�ZB	�B	ƨB	�XB	�'B	�B	��B	��B	��B	��B	�hB	z�B	gmB	]/B	XB	S�B	P�B	J�B	>wB	49B	"�B	uB		7B	B	  B��B�B�;B�;B�)B�#B��B��B��B�jB�3B�B��B��B��B��B��B��B��B��B��B�uB�hB�hB�DB�=B�%B�B�B~�B{�Bz�Bx�Bu�Br�Bs�Bq�BjBl�Bl�BjBhsBgmBe`BdZBbNBcTBaHBbNBe`BhsBk�Bn�Bp�Bp�Bq�Bs�Bs�Bs�Bs�Bt�Bu�Bt�Bs�Bu�Bu�Bt�Bs�Bq�BiyBXBM�BK�BI�BYBgmBgmBdZB^5BQ�BM�BF�B>wB:^B8RB8RB7LB8RB;dB>wB[#Bk�Bm�Bt�Bt�Bo�Bp�Bm�BffBm�BO�BH�BM�BT�B\)B`BBp�B�B�\B��B�3BB��B��B��B�
B�B	oB	2-B	G�B	ZB	e`B	iyB	|�B	� B	�B	�+B	�PB	��B	��B	��B	��B	�B	�'B	�-B	�dB	��B	ƨB	ɺB	��B	��B	��B	�
B	�B	�BB	�ZB	�ZB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
1B
	7B
	7B
PB
PB
PB
VB
\B
oB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
&�B
'�B
'�B
)�B
+B
,B
/B
/B
0!B
/B
0!B
1'B
33B
49B
49B
6FB
6FB
8RB
9XB
:^B
;dB
=qB
=qB
>wB
?}B
A�B
A�B
B�B
B�B
D�B
D�B
E�B
F�B
G�B
I�B
J�B
K�B
L�B
M�B
N�B
P�B
Q�B
R�B
R�B
T�B
VB
W
B
XB
ZB
ZB
ZB
[#B
\)B
]/B
_;B
_;B
^5B
_;B
_;B
`BB
aHB
aHB
aHB
bNB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
hsB
gmB
hsB
iyB
jB
jB
k�B
jB
k�B
k�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
q�B
s�B
s�B
t�B
t�B
s�B
v�B
x�B
y�B
y�B
|�B
~�B
� B
�B
�B
�B
�%B
�%B
�1B
�=B
�DB
�PB
�VB
�\B
�hB
�hB
�oB
�{B
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
�B
�B
�B
�B
�B
�B
�!B
�!B
�'B
�-B
�-B
�3B
�3B
�9B
�9B
�9B
�FB
�?B
�FB
�?B
�?B
�FB
�?B
�FB
�FB
�?B
�FB
�LB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�LB
��B
�B
�
B
�
B
�B
�
B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�
B
�
B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�
B
�
B
�B
�
B
��B
�B
�
B
�B
�B
�B
�B
�
B
�
B
�
B
�B
�
B
�B
�B
�B
�B
�B
�
B
�
B
�B
�B
�
B
�
B
�
B
�B
�B
�B
�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�8B
�RB
�kB
B?]BX�Bt�B��B��B�5B�rB�B�uB�pB�vB�B��B �BB�B��B
&B}B3BxBI�BjkB��B�B��B��B�B�B�B�)B�0B�+B�+B�,B�?B�@B�YB�SB�mB�sB�zB�{B�{B�|B��BĖB��B�~B�~B�mB�)B�B��B�ABy�BgkBQ�BL�BI�BF�BE�B<kB7NB6HB4<B1*B* B�BUB��B�ZBȺB�^B�B�cB_CBM�BC�B?�B:hB-B&�B!�BaB
�B
�/B
�#B
�B
��B
��B
��B
��B
��B
�B
��B
{�B
k�B
aZB
]AB
Y*B
UB
@�B
2AB
06B
.)B
-$B
+B
 �B
�B
hB
*B	�B	�sB	�7B	��B	�rB	�BB	�B	�B	�B	��B	��B	��B	z�B	g�B	]NB	X/B	TB	QB	J�B	>�B	4[B	"�B	�B		ZB	<B	 $B��B�B�`B�`B�OB�IB�%B�B��B��B�[B�=B�B�B�B��B��B��B��B��B��B��B��B��B�sB�mB�UB�CB�7B,B|B{ByBu�Br�Bs�Bq�Bj�Bl�Bl�Bj�Bh�Bg�Be�Bd�Bb�Bc�Ba�Bb�Be�Bh�Bk�Bn�Bp�Bp�Bq�Bs�Bs�Bs�Bs�Bt�BvBu Bs�BvBvBuBs�Bq�Bi�BXXBNBLBJBYaBg�Bg�Bd�B^�BR8BNBF�B>�B:�B8�B8�B7�B8�B;�B>�B[tBk�Bm�BuBuBo�Bp�Bm�Bf�Bm�BP9BIBN2BU`B\�B`�BqB�B��B�B��B�B�BB�WB�MB׍B�B	�B	2�B	H<B	Z�B	e�B	jB	}�B	��B	��B	��B	��B	�4B	�VB	�xB	��B	��B	��B	��B	�B	�FB	�hB	�}B	͒B	ЧB	ҷB	��B	��B	�B	�/B	�1B	�YB	�gB	�wB	�B	��B	��B	��B	��B	��B	��B	��B
 �B
B
B
!B
	0B

8B

;B
WB
YB
\B
dB
mB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
$ B
(B
)$B
)&B
+5B
,>B
-FB
0\B
0^B
1gB
0dB
1lB
2uB
4�B
5�B
5�B
7�B
7�B
9�B
:�B
;�B
<�B
>�B
>�B
?�B
@�B
B�B
B�B
DB
DB
FB
FB
G!B
H*B
I2B
KAB
LJB
MSB
N[B
OdB
PlB
RzB
S�B
T�B
T�B
V�B
W�B
X�B
Y�B
[�B
[�B
[�B
\�B
]�B
^�B
`�B
`�B
_�B
`�B
`�B
bB
cB
cB
cB
dB
f*B
f,B
g5B
g7B
g:B
hBB
hDB
jTB
iPB
jYB
kaB
liB
llB
mtB
lpB
myB
m{B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
u�B
u�B
v�B
v�B
u�B
x�B
z�B
|B
|B
%B
�7B
�BB
�NB
�aB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�%B
�=B
�BB
�PB
�\B
�bB
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
��B
� B
�B
�B
�B
�&B
�1B
�>B
�RB
�hB
�xB
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�<B
�PB
�_B
�oB
�B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202442021061413551420210614135514202106171311402021061713114020210617131140201807242202442021061413551420210614135514202106171311402021061713114020210617131140PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024420180724220244  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024420180724220244QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024420180724220244QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145320210617131453IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                