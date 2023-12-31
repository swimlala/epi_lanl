CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:45Z creation      
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
_FillValue                 0  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  at   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  ք   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ڴ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180724220245  20210617131453  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               	   	DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�e-��r@�e-��r11  @�e-�[�@�e-�[�@6���:�@6���:��cȍ�2#�cȍ�2#11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@Fff@�33@�33@�33@�33A��A  A$��AA��A`  A���A���A���A���A���A���A�  A�  B ffB��B33B  B ffB(ffB0ffB8ffB@ffBH��BP  BXffB`  Bg��Bo��Bx  B�33B�  B���B�33B�33B�  B�  B�33B�ffB�ffB�ffB�33B�33B�  B�  B�  B�  B�  BǙ�B���B���Bә�B�33B�  B�  B�  B�  B�  B���B�B���B�ffC ffC  C  C�C33C
�C�C33C33C33C33CL�C  C�C33C33C ffC"  C$  C&  C(�C*33C,�C.33C033C2L�C3�fC6  C8  C:  C<  C>  C@�CB  CD  CE�fCH�CJ�CL  CN  CP  CRL�CT33CV  CX  CY��C\33C^33C`33Cb�Cd  CfL�ChL�Cj33Cl  Cm�fCp�Cr�Cs�fCu�fCw��Cz�C|  C~  C�  C��3C��C��C�  C�  C��3C��3C��fC��C�  C�  C��3C��3C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��3C��fC��C��C��C��C�  C��C�  C��3C��fC��C��C��3C��3C��C��C��C�  C��3C��C��C��C��C��3C��C��C��3C��C�  C��3C��C��C�  C��C��C��3C��C�  C��fC�  C��C��C��3C�&fC��C��3C��C��C��3C�  C��3C��fC�  C��C��C��3C��3C��fC�  C��C��C��3C��C��C�  C�&fC�&fC��C��C�  C�  C��fC�&fC��C�  C��C��C�  C�  C��3C��C�  C��3C��C��C�  C��3C��3C��C��C��C��C�  C��3C��3C�� D9�D��D�3D�fD33D� DS3D� DS3D�3D` D� D"l�D%3D'�fD*@ D,��D/��D2@ D4�fD7��D:9�D<ٚD?l�DB�DD��DG�DI��DL` DN�3DQ� DT@ DVٚDYl�D\  D^� DaFfDc�fDfL�DhٚDkS3Dm��DpFfDr�fDu9�Dw��Dz9�D|FfD~�fD�� D��fD�6fD��3D��fD�3D�` D��3D�� D�33D�|�D�ɚD� D�L�D���D���D�fD�33D�p D��fD��3D���D��D�@ D�i�D�� D�� D�� D�fD�FfD�p D�� D�� D�  D�,�D�Y�D���D��fD�� D��D�<�D�vfD��fD�ٚD�3D�I�D�|�D���D���D� D�FfD�s3D���D���D�fD�33D�ffDŌ�DƼ�D���D��D�<�D�p D̠ D�ɚD�� D�fD�<�D�ffDӌ�DԬ�D��3D��D�  D��D�0 D�P D�ffD݃3Dޣ3D߶fD�ɚD���D��D���D�3D�	�D��D�fD�fD��D��D��D��D��D�  D�#3D�&fD�&fD�6fD�<�D�FfD�Y�D�i�D�vfD��3D��3D��3D���D�� D��3E 3E �3E&fE��EFfE�fEd�E� E� EfE� E8 EX E	  E
�E4�EA�E�3E� E|�E��E�3E!�E)�E�3E�fE)�E#3E��E��E EnfE ^fE!�3E#&fE$�E%s3E&� E'��E)�E*|�E+�3E,��E.+3E/��E0� E1�fE3�E4q�E5� E73E8  E9P E:�3E;� E>�3EBNfEED�EH{3EK��EN�3EQ�fEU3EWٚEZ��E^3Eay�Edy�Eg��Ej��En�EpٚEs��Ew3Ez{3E}��E�BfE��E�zfE�� E�| E�!�E���E�73E�� E�D E��3E�8 E�zfE�� E�3E��fE��fE�3E�^fE���E��3E�VfE��3E���E�I�E��3E���E�4�E���E�� E�fE�x E��fE�
fE�` E���E��E�D�E��fE��fE�@�>���>���>���>���>���>���>L��>���>���>���>L��>���>���>L��>���>���>���>���>���>���>���?��?333?L��?fff?���?�ff?�  ?ٙ�?�33@��@��@,��@9��@L��@`  @l��@�33@�  @���@�33@�  @���@ə�@�ff@�ff@�  @���A��A33A33A��A!��A)��A0  A4��A>ffAD��AL��AT��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444144411414144441111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?L��?�  @&ff@fff@�33@�33@�33@�33A	��A  A,��AI��Ah  A���A���A���A���A���A���A�  A�  BffB	��B33B  B"ffB*ffB2ffB:ffBBffBJ��BR  BZffBb  Bi��Bq��Bz  B�33B�  B���B�33B�33B�  B�  B�33B�ffB�ffB�ffB�33B�33B�  B�  B�  B�  B�  Bș�B���B���Bԙ�B�33B�  B�  B�  B�  B�  B���B���B���B�ffC �fC� C� C��C�3C
��C��C�3C�3C�3C�3C��C� C��C�3C�3C �fC"� C$� C&� C(��C*�3C,��C.�3C0�3C2��C4ffC6� C8� C:� C<� C>� C@��CB� CD� CFffCH��CJ��CL� CN� CP� CR��CT�3CV� CX� CZL�C\�3C^�3C`�3Cb��Cd� Cf��Ch��Cj�3Cl� CnffCp��Cr��CtffCvffCxL�Cz��C|� C~� C�@ C�33C�Y�C�L�C�@ C�@ C�33C�33C�&fC�L�C�@ C�@ C�33C�33C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�Y�C�ffC�33C�&fC�Y�C�Y�C�L�C�L�C�@ C�L�C�@ C�33C�&fC�Y�C�Y�C�33C�33C�Y�C�L�C�L�C�@ C�33C�Y�C�Y�C�L�C�L�C�33C�Y�C�L�C�33C�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�Y�C�L�C�33C�ffC�L�C�33C�Y�C�L�C�33C�@ C�33C�&fC�@ C�Y�C�L�C�33C�33C�&fC�@ C�Y�C�L�C�33C�Y�C�Y�C�@ C�ffC�ffC�L�C�L�C�@ C�@ C�&fC�ffC�L�C�@ C�Y�C�L�C�@ C�@ C�33C�L�C�@ C�33C�Y�C�L�C�@ C�33C�33C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�  DY�D�D�3D�fDS3D� Ds3D  Ds3D�3D� D   D"��D%33D'�fD*` D-�D/��D2` D5fD7��D:Y�D<��D?��DB,�DD��DG9�DIٚDL� DO3DQ� DT` DV��DY��D\  D^� DaffDc�fDfl�Dh��Dks3Dm��DpffDr�fDuY�DwٚDzY�D|ffD~�fD�� D�fD�FfD��3D��fD�#3D�p D��3D�  D�C3D���D�ٚD�  D�\�D���D���D�fD�C3D�� D��fD��3D���D�,�D�P D�y�D�� D�� D�  D�&fD�VfD�� D�� D�� D� D�<�D�i�D���D��fD�� D��D�L�D��fD��fD��D�#3D�Y�D���D���D���D�  D�VfD��3D���D���D�fD�C3D�vfDŜ�D���D���D�)�D�L�Dˀ D̰ D�ٚD�  D�&fD�L�D�vfDӜ�DԼ�D��3D���D� D�)�D�@ D�` D�vfDݓ3D޳3D��fD�ٚD���D���D�	�D�3D��D��D�&fD�&fD�,�D�,�D�,�D�)�D�,�D�0 D�33D�6fD�6fD�FfD�L�D�VfD�i�D�y�D��fD��3D��3D��3D���D�� D��3E 3E �3E.fE��ENfE�fEl�E  E� EfE� E@ E` E	 E
!�E<�EI�E�3E� E��E��E�3E)�E1�E�3E�fE1�E+3E��E��E EvfE ffE!�3E#.fE$�E%{3E&� E'ɚE)$�E*��E+�3E,��E.33E/��E0� E1�fE3$�E4y�E5� E7#3E8 E9X E:�3E;� E>�3EBVfEEL�EH�3EK��EN�3EQ�fEU3EW�E[�E^3Ea��Ed��Eg��EjɚEn�Ep�Et�Ew#3Ez�3E}��E�FfE���E�~fE�  E�� E�%�E���E�;3E�� E�H E��3E�< E�~fE�� E�#3E��fE��fE�#3E�bfE���E��3E�ZfE��3E���E�M�E��3E���E�8�E���E�� E�"fE�| E��fE�fE�d E���E��E�H�E��fE��fE�D�G�O�G�O�?L��G�O�G�O�G�O�?333G�O�G�O�G�O�?333?L��G�O�?333G�O�?L��G�O�G�O�G�O�G�O�?fff?���?���?�ff?�33?���?�ff@   @��@��@,��@9��@L��@Y��@l��@�  @�ff@�33@�  @���@�33@�  @���@ٙ�@�ff@�ffA   AffA��A33A33A!��A)��A1��A8  A<��AFffAL��AT��A\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444144411414144441111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ @ �@ V@ *@ �@ "�@ )�@ 0x@ 7L@ =q@ FQ@ R�@ _�@ m�@ |?@ �7@ ��@ ��@ �-@ �&@ ��@ �#@ �m@ �e@j@�@g@-@:�@H]@V�@b�@qS@~K@�D@��@�A@��@@ψ@ލ@�4@�,@�@*@#�@1'@>�@K�@Yn@ff@t@��@�\@�@�M@��@�J@�C@��@��@��@
=@�@%�@2�@?}@M$@]�@l�@ww@�@�u@��@��@�k@��@�h@�@�@@�@�@*S@7�@F�@Q�@_�@m:@{�@��@��@�5@��@�2@�@�t@�@��@j@@g@,`@:@F�@V@c�@p�@~K@��@��@��@��@@��@�;@��@��@�@{@$.@1�@>�@K@X@g@t�@�@��@��@�Y@�R@��@Ӡ@��@�L@�E@
=@�@$�@2�@?}@O0@\)@i�@v�@�p@�$@��@�r@�@�@׹@�@�@^@@�@)�@7�@FQ@Q=@^5@n�@|?@�7@��@��@�-@�&@�@�@�y@� @	�@	b@	 @	-@	:�@	G�@	T�@	dZ@	r@	~�@	��@	��@	��@	��@	��@	��@	��@	��@	��@
�@
{@
#�@
0x@
<�@
K�@
X�@
e	@
t@
�@
�@
�U@
��@
�@
�J@
��@
��@
�@@
��@	�@�@%�@4�@A�@M�@[z@hs@ww@�|@�u@��@�r@�@�c@�@�@�@ �@�@O@'�@8�@D�@Q�@`�@m�@z�@��@��@��@�~@�w@�*@�#@�@�@�@o@ @-@:�@G�@T�@bN@m:@�Q@Ji@�<@��@+@p�@��@��@?}@��@�c@�@SI@��@��@(�@r@�@j@K�@��@�/@$�@k.@��@� @<�@�p@��@@\)@��@��@1'@ww@�&@�@K�@��@�\@�@]�@�@�`@(G@l�@��@��@-@t@��@��@D�@��@��@�@\�@��@��@/@v@��@�Q@B�@��@��@ 
�@ N�@ �P@ �|@!�@!M�@!��@!�o@"
=@"K@"��@"��@#�@#K@#��@#��@$�@$M�@$��@$��@%@%N�@%��@%ψ@&o@&SI@&��@&׹@'�@'[z@'��@'�/@(�@(`�@(�@(�T@)$�@)dZ@)��@)�@*$�@*e�@*��@*�@+$�@+ff@+�A@+�@,%�@,dZ@,�(@,�@-!s@-^�@-��@-�@.{@.P�@.��@.�c@/�@/A�@/~�@/��@/�e@0/@0hs@0�z@0�#@1@1Ji@1�@1��@1�@2(G@2^�@2��@2�@3j@3:�@3r@3��@3�@4�@4SI@4��@4�@5]@5:�@5qS@5�Y@5�@6""@6`A@6�a@6��@7�@7Wb@7��@7��@8�@8N�@8��@8ȴ@9�@9DD@9�&@:t@:�4@;e	@;׹@<�|@<�,@=��@>�@>��@?3�@?��@@Lu@@��@AYn@A��@Be	@B��@Cm:@D%@Dl�@E�@E�T@F@F��@G/�@G�u@H'�@H��@ISI@I��@JO�@J�T@Kww@K��@Lk�@L�E@M�\@N @N��@O@O�a@P(�@Qr@R�l@T+@U��@V�(@X>�@Y�@Z�y@\�@]oF@^��@`3�@a{�@b�@d)�@e��@f�>@h�@in�@j�;@l)�@mr�@n�@p1�@qz�@r@t*T@u��@v�@x�@yff@z�w@{@{I�@{�@{�[@|+@|a�@|��@|�@}7�@}l�@}�^@}�@@~:�@~��@~�@�@R�@�a@�7@��@�33@�L@�q�@��0@���@���@���@�T@�@,@�cTG�O�G�O�@ G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ ^@ G�O�@ ^G�O�@ G�O�G�O�G�O�G�O�@ �@ @ �@ v@ %@ �@ �@ 
=@ �@ �@ @ b@ o@ �@ �@ �@ B@ �@ �@  �@ "�@ %�@ (G@ +@ -�@ 1'@ 33@ 5�@ 8�@ ;d@ >�@ A�@ D�@ H]@ K@ M$@ Q=@ S�@ Wb@ Z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�A�|�Aȇ+Aȉ7Aȉ7Aȉ7Aȉ7Aȉ7Aȇ+Aȉ7Aȏ\Aȕ�Aȏ\Aȏ\AȑhAȏ\AȑhAȕ�Aȗ�AȑhAȍPAȉ7A�z�A�hsA�{A�+Aƛ�A�
=A�/AîA��#A��A��DA�VA�`BA��RA�C�A���A��
A�G�A���A�/A�A��#A��DA�O�A�"�A��A��A�A�;dA�1A���A�v�A�C�A�+A��A�G�A���A��+A�I�A��`A��9A�(�A�(�A���A���A��hA�ȴA�?}A�+A��FA�33A�/A�ȴA�ZA��A�1'A��-A���A�  A�I�A���A�jA�oA�A�"�A�t�A���A�
=A��9A���A�  A�l�A��A�I�A���A��DA�jA�=qA�9XA���A���A���A� �A�
=A�p�A��A��\A��A��#A��PA��A�{A��TA��A���A�z�A�wA}�PA|��Azr�Aw��AwS�Av~�Au��At�`Ar�ApjAo�#Ao
=AnA�Am33Ak�^AihsAhz�Ag\)Af=qAe�Ad�AdE�Ab��A_S�A[��AZ�AYAU��AQ�FAP(�ANZAK��AI?}AG33AE�;ADĜAB9XA@��A@�A?7LA;7LA8I�A7�#A7�-A7�A6��A5��A5�A4�RA2ffA1A0�jA-l�A+A+��A+�A)��A)?}A(��A(�+A(I�A(A'�hA';dA%G�A#S�A"�A �A A�A �A��A��A(�A��A�AI�A�AĜAn�A�Al�A+A��AjAE�AA�^AA�;A�7AdZAv�Av�AQ�A�
A�^AG�A  A�A	�mAȴA9XA�Al�A7LAbNA�wA�/Av�AbNA��A �A A�A��A�Al�A �+A Q�A �@�\)@��h@�dZ@���@���@���@�@���@�F@��
@��;@�-@�%@�1'@�|�@��@�`B@�Q�@睲@��y@�@���@϶F@�/@å�@�n�@��@���@��j@��/@�Z@���@�A�@��@��
@��@�?}@�n�@�"�@���@�@��@��#@�o@�@��@�Z@�ȴ@�7L@�O�@��
@��@��@��!@�ff@��`@�+@�$�@��@���@��@�ff@�J@�1'@�|�@�@��`@�bN@K�@|�/@|z�@y��@xA�@w+@vV@tz�@s"�@rJ@p��@n��@l�@jn�@h�u@f�R@e�T@c�m@bM�@`��@]�-@\��@[dZ@Y��@XbN@V��@T�j@S�F@RM�@P�u@O|�@Nȴ@M��@K"�@Jn�@HQ�@G+@E?}@Co@B=q@A��@@�`@?�@=�@<��@;@:^5@9��@9�@7�w@6v�@6{@4z�@3�@2��@1�@0bN@/\)@.��@-�h@,�D@+�m@+"�@*�@(��@';d@&�y@%��@$�@$I�@#C�@"M�@!��@ ��@  �@��@5?@�@�@9X@33@n�@�@1'@��@��@@�@�
@C�@~�@��@Ĝ@�P@��@{@��@�@�D@dZ@
J@	&�@r�@ �@�;@|�@;d@�@�@p�@j@1@dZ@�H@�@x�@%@ �u?�;d?�v�?��-?��m?�C�?���?��?�`B?�Z?�!?�%??�ƨ?�^?�9?�1'?�+?�?}?㕁?�33?�bN?߾w?ޗ�?��?�ƨ?���?�~�?���?�1'?׮?�E�?�Z?ӕ�?ҏ\?�G�?��`?Ͼw?�V?Ͳ-?���?�j?�"�?���?��?�x�?�7L?�r�?ǍP?��y?Ł?���?°!?�hs?�Ĝ?��;?�v�?��-?�/?��D?��m?��?���?�^5?�^5?�^5?��?��#?��?�~�?���?�?�C�?���?�1?���?�p�?�{?���?��?���?�A�?�bN?��?��?���?�Ĝ?�Ĝ?��`?�&�?�&�?�G�?��7?��7?���?���?���?��7?��7?�hs?�hs?�hs?���?���?���?��?�J?�M�?�M�?�n�?�M�?�n�A�v�A�z�A�x�A�z�A�z�A�|�A�|�A�~�AȁAȁAȃA�z�A�~�A�|�A�~�AȁA�|�A�~�A�~�A�|�A�z�A�|�A�|�A�~�A�|�A�z�A�z�A�~�Aȇ+Aȉ7Aȇ+Aȇ+Aȉ7Aȇ+AȋDAȉ7Aȉ7Aȉ7Aȉ7AȋDAȇ+Aȉ7Aȉ7Aȇ+Aȉ7Aȉ7Aȇ+Aȇ+AȅAȇ+AȋDAȋDAȍPAȏ\AȓuAȕ�Aȕ�Aȗ�AȓuAȏ\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�|�A�|�Aȇ+Aȉ7Aȉ7Aȉ7Aȉ7Aȉ7Aȇ+Aȉ7Aȏ\Aȕ�Aȏ\Aȏ\AȑhAȏ\AȑhAȕ�Aȗ�AȑhAȍPAȉ7A�z�A�hsA�{A�+Aƛ�A�
=A�/AîA��#A��A��DA�VA�`BA��RA�C�A���A��
A�G�A���A�/A�A��#A��DA�O�A�"�A��A��A�A�;dA�1A���A�v�A�C�A�+A��A�G�A���A��+A�I�A��`A��9A�(�A�(�A���A���A��hA�ȴA�?}A�+A��FA�33A�/A�ȴA�ZA��A�1'A��-A���A�  A�I�A���A�jA�oA�A�"�A�t�A���A�
=A��9A���A�  A�l�A��A�I�A���A��DA�jA�=qA�9XA���A���A���A� �A�
=A�p�A��A��\A��A��#A��PA��A�{A��TA��A���A�z�A�wA}�PA|��Azr�Aw��AwS�Av~�Au��At�`Ar�ApjAo�#Ao
=AnA�Am33Ak�^AihsAhz�Ag\)Af=qAe�Ad�AdE�Ab��A_S�A[��AZ�AYAU��AQ�FAP(�ANZAK��AI?}AG33AE�;ADĜAB9XA@��A@�A?7LA;7LA8I�A7�#A7�-A7�A6��A5��A5�A4�RA2ffA1A0�jA-l�A+A+��A+�A)��A)?}A(��A(�+A(I�A(A'�hA';dA%G�A#S�A"�A �A A�A �A��A��A(�A��A�AI�A�AĜAn�A�Al�A+A��AjAE�AA�^AA�;A�7AdZAv�Av�AQ�A�
A�^AG�A  A�A	�mAȴA9XA�Al�A7LAbNA�wA�/Av�AbNA��A �A A�A��A�Al�A �+A Q�A �@�\)@��h@�dZ@���@���@���@�@���@�F@��
@��;@�-@�%@�1'@�|�@��@�`B@�Q�@睲@��y@�@���@϶F@�/@å�@�n�@��@���@��j@��/@�Z@���@�A�@��@��
@��@�?}@�n�@�"�@���@�@��@��#@�o@�@��@�Z@�ȴ@�7L@�O�@��
@��@��@��!@�ff@��`@�+@�$�@��@���@��@�ff@�J@�1'@�|�@�@��`@�bN@K�@|�/@|z�@y��@xA�@w+@vV@tz�@s"�@rJ@p��@n��@l�@jn�@h�u@f�R@e�T@c�m@bM�@`��@]�-@\��@[dZ@Y��@XbN@V��@T�j@S�F@RM�@P�u@O|�@Nȴ@M��@K"�@Jn�@HQ�@G+@E?}@Co@B=q@A��@@�`@?�@=�@<��@;@:^5@9��@9�@7�w@6v�@6{@4z�@3�@2��@1�@0bN@/\)@.��@-�h@,�D@+�m@+"�@*�@(��@';d@&�y@%��@$�@$I�@#C�@"M�@!��@ ��@  �@��@5?@�@�@9X@33@n�@�@1'@��@��@@�@�
@C�@~�@��@Ĝ@�P@��@{@��@�@�D@dZ@
J@	&�@r�@ �@�;@|�@;d@�@�@p�@j@1@dZ@�H@�@x�@%@ �u?�;d?�v�?��-?��m?�C�?���?��?�`B?�Z?�!?�%??�ƨ?�^?�9?�1'?�+?�?}?㕁?�33?�bN?߾w?ޗ�?��?�ƨ?���?�~�?���?�1'?׮?�E�?�Z?ӕ�?ҏ\?�G�?��`?Ͼw?�V?Ͳ-?���?�j?�"�?���?��?�x�?�7L?�r�?ǍP?��y?Ł?���?°!?�hs?�Ĝ?��;?�v�?��-?�/?��D?��m?��?���?�^5?�^5?�^5?��?��#?��?�~�?���?�?�C�?���?�1?���?�p�?�{?���?��?���?�A�?�bN?��?��?���?�Ĝ?�Ĝ?��`?�&�?�&�?�G�?��7?��7?���?���?���?��7?��7?�hs?�hs?�hs?���?���?���?��?�J?�M�?�M�?�n�?�M�?�n�A�v�A�z�A�x�A�z�A�z�A�|�A�|�A�~�AȁAȁAȃA�z�A�~�A�|�A�~�AȁA�|�A�~�A�~�A�|�A�z�A�|�A�|�A�~�A�|�A�z�A�z�A�~�Aȇ+Aȉ7Aȇ+Aȇ+Aȉ7Aȇ+AȋDAȉ7Aȉ7Aȉ7Aȉ7AȋDAȇ+Aȉ7Aȉ7Aȇ+Aȉ7Aȉ7Aȇ+Aȇ+AȅAȇ+AȋDAȋDAȍPAȏ\AȓuAȕ�Aȕ�Aȗ�AȓuAȏ\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�
B
�B
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
�B
�B
�B
�B
�B
�#B
�5B
�NB
�BbB$�B/BC�B^5B�B�VB�B�FB�}BB5?B=qB[#Bs�B�B�1B�=B�\B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�'B�-B�FB�?B�'B�^B�dB�jB�^B�3B�3B�'B��B�uB��B��B��B�hB�\B�1Bw�Bx�By�B}�B� Bz�Bp�BgmBC�B)�B�BB�B�NB��BŢB�B��B�=B�%B�Bo�Bk�BJ�B<jB-B �B\BB
��B
�B
�/B
�dB
�?B
�B
�uB
{�B
t�B
l�B
\)B
L�B
E�B
0!B
'�B
"�B
�B
{B
+B	��B	�B	�fB	�5B	�B	��B	ÖB	�^B	�9B	�B	��B	��B	��B	��B	��B	�\B	�B	y�B	l�B	VB	B�B	:^B	-B	%�B	�B	PB	+B	B�yB�yB�mB�B�dB�9B�^B�dB�}B��B�}B�}B�RB�!B�B�B��B��B��B��B�{B�oB�hB�VB�PB�JB�JB�DB�B�B�B}�B}�B}�B{�B|�B{�Bw�Bt�Bp�Bk�Bk�BiyBhsBhsBjBiyBk�Bo�Bq�Bp�Bn�Bm�B\)BI�BM�BL�BK�BJ�BJ�BF�BG�BB�BC�B?}B@�B?}B=qB=qB;dB:^B8RB8RB8RB8RB5?B@�BYBjBiyBjBp�Bs�Bq�Bm�BcTB\)B\)BYB^5BZBZB`BBhsB`BBaHB`BB]/B]/BZBXBW
BW
BW
BS�BQ�BR�BZBaHBp�Bn�Bv�B�B�\B��B��B�B�!B��B��B	B	oB	.B	=qB	H�B	P�B	^5B	x�B	�+B	�oB	��B	��B	��B	�!B	�!B	�3B	�RB	�^B	ĜB	��B	��B	�B	�
B	�B	�BB	�HB	�sB	�B	�B	�B	��B	��B	��B	��B	��B
B
B
B
%B
1B
	7B
DB
VB
bB
uB
{B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
%�B
&�B
)�B
+B
,B
.B
.B
/B
/B
33B
33B
5?B
6FB
7LB
:^B
;dB
<jB
<jB
>wB
?}B
@�B
A�B
C�B
C�B
D�B
D�B
F�B
F�B
H�B
H�B
I�B
J�B
K�B
K�B
L�B
M�B
N�B
Q�B
R�B
R�B
S�B
VB
T�B
W
B
XB
YB
YB
[#B
\)B
\)B
]/B
^5B
_;B
_;B
_;B
`BB
bNB
bNB
cTB
dZB
dZB
ffB
ffB
gmB
hsB
hsB
iyB
jB
k�B
l�B
m�B
m�B
n�B
n�B
n�B
p�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
w�B
w�B
y�B
x�B
y�B
y�B
{�B
{�B
|�B
}�B
}�B
}�B
}�B
� B
� B
�B
�B
�B
�B
�%B
�+B
�=B
�DB
�PB
�JB
�JB
�VB
�\B
�bB
�bB
�uB
�uB
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
�B
�B
�!B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�?B
�FB
�FB
�RB
�LB
�LB
�RB
�LB
�RB
�RB
�XB
�RB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�RB
�^B
�XB
�XB
�XB
�XB
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
�^B
�^B
�^B
�XB
�XB
�^B
�^B
�^B
�XB
�^B
�XB
�^B
�^B
�B
�
B
�B
�
B
�B
�
B
�B
�
B
�
B
�B
�
B
�
B
�
B
�B
�
B
�
B
�
B
�B
�
B
�
B
�B
�B
�B
�
B
�
B
�B
�B
�B
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
�
B
�
B
�B
�B
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
�
B
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
�B
�B
�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�0B
�{BEB$�B.�BC{B^B��B�<B��B�-B�dBB5'B=ZB[Bs�B�B�B�(B�HB�zB��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�B�#B�<B�6B�B�VB�\B�cB�WB�-B�.B�"B��B�qB��B��B��B�fB�[B�0Bw�Bx�By�B}�B�Bz�Bp�BgpBC�B* B�BB�B�TB��BũB�B��B�EB�-B�Bo�Bk�BJ�B<uB-B �BhBB
��B
�B
�=B
�rB
�MB
�#B
��B
{�B
t�B
l�B
\:B
L�B
E�B
04B
(B
"�B
�B
�B
@B	��B	�B	�|B	�LB	�.B	��B	îB	�wB	�RB	�(B	�
B	��B	��B	��B	��B	�yB	�<B	y�B	l�B	V"B	B�B	:}B	--B	&B	�B	pB	LB	@B�B�B�B�:B��B�\B��B��B��B��B��B��B�yB�IB�<B�7B��B��B��B��B��B��B��B��B�}B�xB�xB�sB�HB�NB�BB~$B~%B~%B|B}!B|BxBt�Bp�Bk�Bk�Bi�Bh�Bh�Bj�Bi�Bk�Bo�Bq�Bp�Bn�Bm�B\eBI�BNBM
BLBJ�BK BF�BG�BB�BC�B?�B@�B?�B=�B=�B;�B:�B8�B8�B8�B8�B5�B@�BY_Bj�Bi�Bj�Bp�BtBq�Bm�Bc�B\vB\vBYeB^�BZlBZlB`�Bh�B`�Ba�B`�B]�B]�BZqBXdBW_BW_BWeBTVBRMBSVBZ�Ba�BqBoBw:B�zB��B��B�#B��B��B�SB�JB	�B	�B	.�B	>B	IMB	Q�B	^�B	yxB	��B	�B	�9B	�<B	��B	��B	��B	��B	�B	� B	�aB	͕B	ѰB	��B	��B	��B	�B	�"B	�PB	�_B	�B	�B	��B	��B	��B	��B	��B
B
B
B
%B
	4B

=B
MB
bB
qB
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%B
'B
(B
+3B
,<B
-EB
/TB
/WB
0`B
0cB
4~B
4�B
6�B
7�B
8�B
;�B
<�B
=�B
=�B
?�B
@�B
A�B
B�B
EB
E	B
FB
FB
H#B
H&B
J5B
J8B
KAB
LKB
MSB
MVB
N_B
OhB
PqB
S�B
T�B
T�B
U�B
W�B
V�B
X�B
Y�B
Z�B
Z�B
\�B
]�B
]�B
^�B
_�B
`�B
aB
aB
bB
dB
dB
e'B
f0B
f3B
hAB
hDB
iMB
jVB
jXB
k`B
liB
mqB
nzB
o�B
o�B
p�B
p�B
p�B
r�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
y�B
y�B
{�B
z�B
{�B
{�B
~B
~B
B
� B
�#B
�&B
�)B
�7B
�:B
�CB
�TB
�cB
�oB
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
�B
�B
� B
�$B
�7B
�>B
�IB
�UB
�\B
�nB
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
�B
�B
�B
�B
�B
�+B
�FB
�]B
�rB
��B
��B
��B
��B
��B
��B
�B
�B
�,B
�AB
�WB
�fB
�}B
��B
��B
��B
��B
��B
��B
��B
�B
�B
� B
�1B
�@B
�UB
�dB
�sB
�|B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202452021061413551620210614135516202106171311462021061713114620210617131146201807242202452021061413551620210614135516202106171311462021061713114620210617131146PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024520180724220245  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024520180724220245QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024520180724220245QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145320210617131453IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                