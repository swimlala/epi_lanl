CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-24T00:01:57Z creation      
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
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180924000157  20210617131504  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               #   #DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؁H���@؁H���11  @؁H�=@@؁H�=@@6�p���a@6�p���a�cݖ�����cݖ����11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@@  @�  @�  @�  @�33A   A33A$��A@  A`  A���A�  A�33A���A�ffAљ�AᙚA�B ffBffB  B��B33B'33B0  B8��B@  BH  BPffBY33B`ffBg��Bo��BxffB�ffB�  B�33B�33B�33B�ffB�ffB�ffB�33B�33B�ffB�33B�33B�33B���B�  B�  B���B�  B���BЙ�B�  B�  Bۙ�B�ffB�  B���B�  B�ffB�33B�  B�ffC �C  C�fC��C�C
  C�fCL�C33C�C�C  C33C�C�fC33C   C!��C$  C&33C(L�C*L�C,33C.  C0�C2L�C4�C5�fC833C:33C<L�C>�C?��CB  CD�CF33CHL�CJ�CK�fCM�fCP�CRL�CT33CU��CX  CZ33C\�C^  C`�Cb  Cc�fCf�Cg�fCi��Cl  Cn33Cp�Cq��Cs�fCv33Cx  Cy��C|�C~  C��C��C�&fC��C��3C��C�&fC��C��fC�  C��C�&fC��C��fC��3C��3C��C��C��C��C��fC��3C��3C�  C��C�&fC�&fC��C��fC��3C�  C��C��C��C�&fC�  C��fC��fC�  C�  C�  C��C��C��C��C��C�&fC�&fC�&fC�33C�&fC�33C��C��fC��fC��fC��3C��fC��3C�  C��3C��3C�  C��3C��3C��3C��3C��3C�  C��C�&fC�33C��C��3C��fC��3C��3C�  C�  C�  C��C��C��C�&fC�&fC��C��fC�  C�  C��C�&fC�&fC��C��fC��3C��3C�  C�  C��C�  C�  C��C��fC��3C��fC��3C�  C��C�&fC�  C��fC�  C��C�  C��fC��3C��C��C��C��3C�  C��C��C�  C��C�33C��3C��D��DffD	��D��D33D�fDy�DfD�3D,�D� D!S3D#� D&ffD(��D+s3D-��D0ffD2�3D533D7� D:fD<ffD>�3DAfDCY�DE��DH�DJl�DL�3DO�DQs3DSٚDV&fDX�fDZ�fD]@ D_��Da��Dd&fDfs3Dh� Dk�Dml�Do� Dr  Dt��Dv�3DyL�D{L�D}��D��D�I�D�� D�� D���D�9�D�|�D���D��fD�0 D�ffD�� D�ٚD��D�L�D���D���D��3D�#3D�S3D��fD��3D��D��D�P D�� D���D��fD�3D�&fD�FfD�ffD�|�D���D��fD�� D��D���D�3D�&fD�@ D�VfD�vfD��fD�� D�� D��3D��D���D�	�D�fD��D�,�D�33D�9�D�9�D�@ D�I�D�S3D�` D�p DŃ3DƐ DǓ3DȜ�Dɠ DʦfD˹�D�ɚD�� D��fD�fD�6fD�Y�DӀ Dԣ3D�� D��D��D�0 D�Y�DۆfDܰ D���D�3D�&fD�P D�s3D��D�ɚD�� D�  D�C3D�ffD�y�D� D칚D�ٚD���D��D�<�D�Y�D�y�D�� D���D��3D��fD��3D�� D�ٚD��3D���D��fD���D���E |�E ��Ex E��Es3E�E�fE�3Ek3EffE��E	��ES3EK3E�3E� E9�E�fE�3EfE E�fE� E��Ep EvfE��E��E~fE ��E!��E#3E$fE%�fE&��E(�E(��E*q�E+��E,� E.>fE/�3E0�fE1�fE3i�E4VfE5�3E7 E8a�E9H E:��E;��E>��EBX EE,�EHK3EKT�EN��EQ�fET�fEX!�EZ� E^Y�Ead�Ed|�Eg�3Ej��Em�fEq�EtfEw�Ez6fE}P E�73E��fE�o3E���E�|�E��3E��E�� E��3E�  E�`�E�� E���E�Y�E�� E���E�33E���E���E�<�E�t�E�� E�'3E�d E��3E��E�l E��3E��fE�VfE�� E��E�FfE�� E��f>���?   >���>���>���>���?   >���?   >���?   >���?   ?   ?   >���?   ?��?333?L��?L��?�  ?�  ?���?�ff?�33?���?�ff@   @��@   @,��@@  @S33@fff@s33@�33@���@���@�  @���@�ff@���@���@���@陚@�33A   AffAffAffAffA#33A,��A4��A;33AC33AI��AQ��A[33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414141444111141411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?fff?�  @&ff@`  @�  @�  @�  @�33A  A33A,��AH  Ah  A���A�  A�33A���A�ffAՙ�A噚A���BffB
ffB  B��B!33B)33B2  B:��BB  BJ  BRffB[33BbffBi��Bq��BzffB�ffB�  B�33B�33B�33B�ffB�ffB�ffB�33B�33B�ffB�33B�33B�33B���B�  B�  B���B�  B���Bљ�B�  B�  Bܙ�B�ffB�  B���B�  B�ffB�33B�  B�ffC ��C� CffCL�C��C
� CffC��C�3C��C��C� C�3C��CffC�3C � C"L�C$� C&�3C(��C*��C,�3C.� C0��C2��C4��C6ffC8�3C:�3C<��C>��C@L�CB� CD��CF�3CH��CJ��CLffCNffCP��CR��CT�3CVL�CX� CZ�3C\��C^� C`��Cb� CdffCf��ChffCjL�Cl� Cn�3Cp��CrL�CtffCv�3Cx� CzL�C|��C~� C�&fC�L�C�ffC�L�C�33C�L�C�ffC�L�C�&fC�@ C�L�C�ffC�L�C�&fC�33C�33C�L�C�Y�C�Y�C�L�C�&fC�33C�33C�@ C�L�C�ffC�ffC�L�C�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�@ C�&fC�&fC�@ C�@ C�@ C�L�C�L�C�Y�C�L�C�Y�C�ffC�ffC�ffC�s3C�ffC�s3C�L�C�&fC�&fC�&fC�33C�&fC�33C�@ C�33C�33C�@ C�33C�33C�33C�33C�33C�@ C�Y�C�ffC�s3C�L�C�33C�&fC�33C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�ffC�ffC�L�C�&fC�@ C�@ C�L�C�ffC�ffC�L�C�&fC�33C�33C�@ C�@ C�L�C�@ C�@ C�L�C�&fC�33C�&fC�33C�@ C�L�C�ffC�@ C�&fC�@ C�L�C�@ C�&fC�33C�L�C�Y�C�L�C�33C�@ C�Y�C�L�C�@ C�Y�C�s3C�33C�L�D�D�fD
�D��DS3DfD��D&fD�3DL�D� D!s3D$  D&�fD)�D+�3D.�D0�fD2�3D5S3D7� D:&fD<�fD>�3DA&fDCy�DE��DH,�DJ��DL�3DO,�DQ�3DS��DVFfDX�fD[fD]` D_��Da��DdFfDf�3Dh� Dk9�Dm��Do� Dr@ Dt��Dw3Dyl�D{l�D}��D��D�Y�D�� D�� D�	�D�I�D���D�ɚD�fD�@ D�vfD�� D��D��D�\�D���D���D�3D�33D�c3D��fD��3D���D�,�D�` D�� D���D��fD�3D�6fD�VfD�vfD���D���D��fD�� D���D��D�#3D�6fD�P D�ffD��fD��fD�� D�� D��3D���D��D��D�&fD�,�D�<�D�C3D�I�D�I�D�P D�Y�D�c3D�p DĀ Dœ3DƠ Dǣ3DȬ�Dɰ DʶfD�ɚD�ٚD�� D�fD�&fD�FfD�i�DӐ DԳ3D�� D���D��D�@ D�i�DۖfD�� D���D�3D�6fD�` D�3D��D�ٚD�  D�0 D�S3D�vfDꉚD� D�ɚD��D�	�D�,�D�L�D�i�D�D�� D���D��3D��fD��3D�  D��D��3D���D�fD��E �E ��E�E� E��E{3E��E�fE�3Es3EnfE��E	��E[3ES3E�3E� EA�E�fE�3E&fE  E�fE� E��Ex E~fE�E�E�fE ��E!��E#3E$fE%�fE&��E(	�E)�E*y�E+��E,� E.FfE/�3E0�fE2fE3q�E4^fE5�3E7 E8i�E9P E:��E;��E?�EB` EE4�EHS3EK\�EN��EQ�fET�fEX)�EZ� E^a�Eal�Ed��Eg�3Ej��Em�fEq�Et&fEw�Ez>fE}X E�;3E��fE�s3E���E���E��3E�!�E�� E��3E�$ E�d�E�� E��E�]�E�� E���E�73E���E���E�@�E�x�E�� E�+3E�h E��3E��E�p E��3E�fE�ZfE�� E��E�JfE�� E��f?fffG�O�?L��G�O�G�O�?fffG�O�?L��G�O�?L��G�O�?fffG�O�G�O�G�O�?fff?�  ?���?���G�O�?�ffG�O�?�  ?ٙ�?�ff?�33@ff@33@   @9��@@  @L��@`  @s33@�33@���@�33@���@���@�  @���@�ff@���@���@���@���A��A  AffAffAffA&ffA+33A4��A<��AC33AK33AQ��AY��Ac33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414141444111141411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ �@ �@ V@ {@ O@ ""@ (�@ 0x@ 6�@ >�@ FQ@ Q�@ _�@ m�@ z�@ ��@ ��@ ��@ ��@ ��@ �*@ �#@ ��@ ��@�@�@[@,`@;d@G�@UU@c�@r�@~�@�D@��@��@�F@@��@ލ@�4@��@1@�@"�@0x@>�@K�@Yn@g@s_@��@�\@�U@��@�@�@Ӡ@�H@�@��@
=@6@%�@4�@A�@N�@]�@j@ww@�p@�h@�@�@�@�o@�h@�`@�@  @@�@(G@7�@DD@P�@_�@n�@|�@��@��@��@�-@�2@�|@��@�y@� @v@�@[@,`@:�@I@Wb@c�@o�@}�@��@��@��@��@@є@ލ@�@��@�@�@"�@/@<@K@Z@g@r�@�@��@�@�M@�@��@�C@��@��@�E@	�@�@'�@3�@?}@N�@\�@k�@x&@��@�@��@��@�@��@׹@�T@�@�Q@�@�@+@8�@D�@P�@^�@m:@{�@��@��@��@�~@��@�o@�t@�@��@	@	�@	 @	-@	;d@	I�@	Wb@	e	@	s_@	�W@	��@	�H@	��@	��@	�2@	ψ@	܀@	��@	�,@
%@
�@
""@
/@
<�@
Ji@
X@
e�@
t@
�@
�h@
��@
�Y@
��@
Ĝ@
��@
��@
��@
��@
=@�@&;@4�@B�@P�@\�@hs@ww@�@�u@�y@�!@�k@�@�[@�@�Y@  @V@O@(�@7L@B�@Q=@^5@l�@z�@�7@�<@��@�!@�&@�|@�t@�@�@@o@g@+�@:@I@V@b�@r@�@�D@�H@-�@qS@��@�Q@FQ@�@�\@�@a�@��@��@5?@z�@��@%@I�@�P@��@@S�@�0@׹@�@Wb@��@�\@�@V�@��@խ@�@Wb@��@׹@�@Yn@��@�h@�@V@��@Ӡ@�@SI@��@Ӡ@�@Wb@��@�*@@Q=@��@�
@O@^5@�z@�m@+@n�@�~@�@6�@y�@�@�Q@B�@�p@ƨ@ �@ H]@ ��@ �@!J@!M�@!�\@!�7@"�@"O�@"�@"�*@#�@#I@#�p@#�2@#��@$:@$v@$��@$�4@%&�@%b�@%�a@%��@&�@&Q�@&�\@&�@'v@'@,@'y�@'��@'��@($�@(\�@(��@(�o@)j@)<@)t�@)�@)�@*"�@*\)@*�u@*�@+j@+;d@+v@+�!@+�@,&�@,dZ@,��@,��@-�@-\�@-��@-�@.6@.UU@.��@.��@/{@/T�@/�u@/є@0@0O0@0��@0��@1�@1N�@1��@1��@2v@2DD@2�W@2��@2�9@39X@3v�@3��@3��@4,`@4hs@4��@4�;@5�@5Q�@5��@5�j@5�@6-�@6e�@6��@6�C@7�@7=q@7r�@7��@7ލ@8Ji@8�F@9Z@9�J@:hs@:�C@;r@;��@<|?@<�@=�7@>+�@>�#@?2�@?�@@9X@@��@AB�@A�@BT�@B��@Cg@D�@D|�@D�(@E��@E�Q@F�(@GV@G��@H�@H�@IS�@I��@JX@J�@KX@K�@L��@L�@M�|@NO@N�Z@O�@O�@P.l@Q{�@R�@T �@Uuk@V�2@X&;@Y�@Z�/@\7L@]i�@^ލ@`+@a|�@b��@d'�@e��@fխ@h(G@ij@j��@l{@mi!@n�@p(G@qul@r�>@s�@sLv@s�m@s�[@t(�@t`B@t�~@t�@u4�@ui�@u��@u�@@v:�@v��@v��@w �@wK@w��@w��@x6@xdZ@x�@x��@y+@yv@y�&@y��@zB�@z��@z��@ �G�O�@ G�O�G�O�@ �G�O�@ G�O�@ G�O�@ �G�O�G�O�G�O�@ �@ j@ @ �G�O�@ vG�O�@ �@ 1@ �@ 	�@ 
�@ J@ �@ b@ @ o@ {@ �@ �@ �@ �@ 
@  �@ ""@ $.@ &�@ (G@ +�@ /@ 1�@ 3�@ 6�@ 9X@ <�@ @,@ C�@ E�@ I�@ M$@ O�@ SI@ V@ Yn@ ]�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A�
=A��A��A��A��A��A� �A� �A��A��A��A�oA�
=A��AնFA�l�A��A�(�AӸRAӬAө�Aӧ�AӍPAӃA�|�A�|�A�t�A�v�A�r�A�ffA��AҰ!A�"�A���A�ZA���A�M�A�n�A��`A�bNA���A�z�A��DA�A�XA��A�|�A���A�&�A�\)A�bA��TA� �A���A�VA���A�ĜA��wA��A���A���A���A�ȴA��^A�t�A���A�l�A�+A�r�A���A�K�A�;dA��A��TA�K�A�ĜA��A���A�~�A� �A�S�A��A�1A���A��A���A��
A�dZA��RA�JA���A��A��A�%A���A��A���A�(�A��^A���A���A���A���A��;A�JA�XA�v�A��A��TA�1A��A�dZA�bNAVA}�mA{��Az�HAzM�Ay\)Aw+Au�-AsƨAr�DAq?}Ap�Ao33An��Am�#Al��Aj�uAh��Ag�;Ag%Ad��AaXA`�A`�uA_�A^1A\��A\��A\ZA[��AY��AX�jAV��AT �AQAP�AN$�AM�;AM��AJ�AG?}AE��AE�7AD�!AC��ABn�AA��A@^5A?A=��A<z�A;�hA;S�A:�A:�A9��A8�yA7�A6�yA61'A5K�A3/A1�TA1&�A0(�A/A.��A,�A*ffA)/A(�`A(�RA($�A&��A&��A&�+A%A$VA"��A"1A!�#A!�A A�A�PA�AI�AXAVA�7A��A�TA�9A�;At�AĜA-A�A+A��A  A&�A��AffA��A�wAp�A��AbNA��AȴA�A��A�A
�A
��A
E�A	�wA	�-A	��A��AJA�A�A�jA5?A�#AA�`An�A=qA�A��A%@�n�@�z�@�{@�A�@��@�dZ@��@��@�9X@��@��T@�  @�dZ@��y@���@ܴ9@պ^@�hs@�\)@���@��j@�~�@��@�Q�@��`@�|�@�j@�x�@��@�^5@���@��+@���@��/@��
@���@�-@��h@�V@���@��\@��!@�`B@���@�(�@���@���@��h@�@��R@���@���@���@�p�@��@��T@�V@��`@��@�Ĝ@��F@��D@���@��7@�@�n�@��D@
=@}�h@{��@x��@v{@uO�@t�@q�#@o�;@nv�@kS�@h��@gK�@e��@dj@c��@cS�@a�@`��@_
=@]�-@\j@Y��@Xb@U�T@TI�@S��@R�@P�9@O
=@M�@L��@K��@J�\@I�@G\)@E��@E@D�@B=q@?�;@?�P@>v�@=�@<�D@<9X@;dZ@9G�@8bN@7|�@6$�@4��@4�@333@2�H@1�@17L@0r�@/;d@.$�@-`B@,�@+�F@+S�@*M�@)%@(�9@'�@';d@&{@$j@$9X@$�@#�
@#C�@"�\@!��@ ��@|�@��@�-@/@�j@33@��@�#@��@�;@l�@�+@�T@�@��@"�@~�@�7@�`@A�@�w@K�@5?@�@�h@�@z�@�F@t�@
��@	��@	%@�`@�u@1'@\)@
=@v�@5?@@/@"�@�\@hs@ �`?��?��m?��^?��y?�?�Z?�S�?�G�?�|�?�h?�j?��H?�^?��?��/?�F?�n�?�G�?޸R?�p�?ܬ?�"�?��?�7L?��?׮?��y?�?}?��/?�z�?�S�?���?�M�?�J?�%?Ѓ?У�?�  ?�\)?�\)?�V?���?�~�?ȴ9?ǍP?�?��
?�33?��?�%?� �?�V?�/?�j?�1?�C�?���?��^?���?��#?���?��^?�=q?�~�?�?��?�1?���?��?�V?�/?�O�?�p�?�p�?��-?���?��?�{?�5??�V?���?���?��R?��?��?�;d?�\)?�\)?�|�?���?��w?��;?� �?�A�?�bN?��?���?�ĜA�  A�A�A�  A�  A�%A�bA�JA�bA�
=A�A�JA�bA�oA�JA�
=A�A�
=A�1A�1A�%A�1A�1A�JA�VA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A�1A�
=A��A��A��A��A��A� �A� �A��A��A��A�oA�
=A��AնFA�l�A��A�(�AӸRAӬAө�Aӧ�AӍPAӃA�|�A�|�A�t�A�v�A�r�A�ffA��AҰ!A�"�A���A�ZA���A�M�A�n�A��`A�bNA���A�z�A��DA�A�XA��A�|�A���A�&�A�\)A�bA��TA� �A���A�VA���A�ĜA��wA��A���A���A���A�ȴA��^A�t�A���A�l�A�+A�r�A���A�K�A�;dA��A��TA�K�A�ĜA��A���A�~�A� �A�S�A��A�1A���A��A���A��
A�dZA��RA�JA���A��A��A�%A���A��A���A�(�A��^A���A���A���A���A��;A�JA�XA�v�A��A��TA�1A��A�dZA�bNAVA}�mA{��Az�HAzM�Ay\)Aw+Au�-AsƨAr�DAq?}Ap�Ao33An��Am�#Al��Aj�uAh��Ag�;Ag%Ad��AaXA`�A`�uA_�A^1A\��A\��A\ZA[��AY��AX�jAV��AT �AQAP�AN$�AM�;AM��AJ�AG?}AE��AE�7AD�!AC��ABn�AA��A@^5A?A=��A<z�A;�hA;S�A:�A:�A9��A8�yA7�A6�yA61'A5K�A3/A1�TA1&�A0(�A/A.��A,�A*ffA)/A(�`A(�RA($�A&��A&��A&�+A%A$VA"��A"1A!�#A!�A A�A�PA�AI�AXAVA�7A��A�TA�9A�;At�AĜA-A�A+A��A  A&�A��AffA��A�wAp�A��AbNA��AȴA�A��A�A
�A
��A
E�A	�wA	�-A	��A��AJA�A�A�jA5?A�#AA�`An�A=qA�A��A%@�n�@�z�@�{@�A�@��@�dZ@��@��@�9X@��@��T@�  @�dZ@��y@���@ܴ9@պ^@�hs@�\)@���@��j@�~�@��@�Q�@��`@�|�@�j@�x�@��@�^5@���@��+@���@��/@��
@���@�-@��h@�V@���@��\@��!@�`B@���@�(�@���@���@��h@�@��R@���@���@���@�p�@��@��T@�V@��`@��@�Ĝ@��F@��D@���@��7@�@�n�@��D@
=@}�h@{��@x��@v{@uO�@t�@q�#@o�;@nv�@kS�@h��@gK�@e��@dj@c��@cS�@a�@`��@_
=@]�-@\j@Y��@Xb@U�T@TI�@S��@R�@P�9@O
=@M�@L��@K��@J�\@I�@G\)@E��@E@D�@B=q@?�;@?�P@>v�@=�@<�D@<9X@;dZ@9G�@8bN@7|�@6$�@4��@4�@333@2�H@1�@17L@0r�@/;d@.$�@-`B@,�@+�F@+S�@*M�@)%@(�9@'�@';d@&{@$j@$9X@$�@#�
@#C�@"�\@!��@ ��@|�@��@�-@/@�j@33@��@�#@��@�;@l�@�+@�T@�@��@"�@~�@�7@�`@A�@�w@K�@5?@�@�h@�@z�@�F@t�@
��@	��@	%@�`@�u@1'@\)@
=@v�@5?@@/@"�@�\@hs@ �`?��?��m?��^?��y?�?�Z?�S�?�G�?�|�?�h?�j?��H?�^?��?��/?�F?�n�?�G�?޸R?�p�?ܬ?�"�?��?�7L?��?׮?��y?�?}?��/?�z�?�S�?���?�M�?�J?�%?Ѓ?У�?�  ?�\)?�\)?�V?���?�~�?ȴ9?ǍP?�?��
?�33?��?�%?� �?�V?�/?�j?�1?�C�?���?��^?���?��#?���?��^?�=q?�~�?�?��?�1?���?��?�V?�/?�O�?�p�?�p�?��-?���?��?�{?�5??�V?���?���?��R?��?��?�;d?�\)?�\)?�|�?���?��w?��;?� �?�A�?�bN?��?���?�ĜA�  A�A�A�  A�  A�%A�bA�JA�bA�
=A�A�JA�bA�oA�JA�
=A�A�
=A�1A�1A�%A�1A�1A�JA�VA�oA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A� �A� �A��A��A��A��A��A��A��A��A��A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B~�B~�B}�B}�B}�B}�B|�B|�B}�B�B�B�B�7B�VB��B�}B�;B�BR�Bk�Bk�BjBiyBffBffBffBffBe`Be`BffBe`BdZBgmBe`BbNBM�BZBw�B|�Bw�Bu�Bw�Bx�B�B�B�B�1B�bB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�7B�B� B{�B{�Bz�Bx�Bs�BgmBe`BcTB`BB^5BW
BR�BH�B>wB/B#�B�BB��B�/B��B�LB��B�oB�Bt�BgmB]/BVBF�BA�B9XB$�B�B
=B
�HB
��B
��B
��B
��B
�DB
�B
w�B
k�B
_;B
P�B
H�B
C�B
9XB
(�B
�B
VB
B	��B
VB
\B
bB
	7B
B	�B	�sB	�BB	�B	�wB	��B	��B	��B	��B	�+B	�B	�B	�B	}�B	s�B	hsB	VB	C�B	9XB	/B	-B	-B	)�B	PB	B��B��B�B�sB�ZB�;B�B��B�
B��BĜBŢB��B��BǮB�wB�^B�3B�9B�B��B��B��B�!B�'B��B��B�JB�B� B}�By�Bu�Bt�Bs�Bn�BiyBiyBhsBjBn�Be`BbNB[#B_;B[#BXBR�BO�BL�BH�BG�BE�BD�BC�BC�BC�B@�B?}B?}B?}B<jB<jB<jB;dB:^B:^B;dB>wBA�B@�B?}B@�BB�BA�BA�BA�B@�BA�BB�B?}B;dB6FB49B33B2-B-B,B-B.B.B-B(�B'�B(�B.B.B-B,B+B,B0!B,B.B/B0!B>wB>wBB�BD�BJ�BM�B`BBk�Bt�B�B�PB�hB��B�B�FB�jB��B�B�NB�B��B	JB	{B	�B	'�B	.B	@�B	O�B	YB	cTB	jB	t�B	z�B	�DB	�7B	�bB	��B	��B	��B	�-B	�9B	�?B	�dB	��B	ƨB	��B	��B	�/B	�NB	�;B	�B	�B	��B	��B	��B	��B
B
%B
B
%B
JB
VB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
&�B
(�B
+B
,B
-B
-B
-B
.B
1'B
33B
49B
5?B
7LB
8RB
:^B
;dB
<jB
?}B
?}B
?}B
B�B
A�B
B�B
C�B
E�B
F�B
G�B
H�B
I�B
J�B
K�B
K�B
L�B
M�B
N�B
O�B
O�B
P�B
Q�B
Q�B
S�B
S�B
T�B
VB
W
B
W
B
XB
YB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
^5B
`BB
`BB
bNB
bNB
cTB
dZB
dZB
ffB
ffB
gmB
gmB
hsB
hsB
jB
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
q�B
r�B
q�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
}�B
}�B
~�B
~�B
�B
�B
�B
�B
�%B
�+B
�1B
�7B
�DB
�JB
�JB
�PB
�\B
�\B
�hB
�hB
�oB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�9B
�?B
�FB
�FB
�LB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�RB
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
�RB
�XB
�XB
�XB
�XB
�XB~�B~�B�B}�B~�B~�B~�B�B}�B� B~�B|�B� B|�B|�B}�B}�B}�B}�B}�B~�B~�B~�B~�B~�B}�B~�B~�B}�B}�B}�B~�B}�B}�B}�B}�B}�B}�B}�B}�B}�B|�B|�B|�B{�B|�B}�B}�B}�B�B�B�B�B�B�B�B�B�%B�1B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             B~�B~�B}�B}�B}�B}�B|�B|�B}�B��B��B��B�B�4B��B�\B�BmBR�BkfBkfBjaBi\BfIBfJBfJBfKBeFBeFBfMBeHBdBBgVBeIBb8BM�BZBw�B|�Bw�Bu�Bw�Bx�B��B�B� B�B�QB�^B�_B�}B�~B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�IB�1B�B�B{�B{�Bz�Bx�Bs�BgkBe^BcSB`AB^5BW
BR�BH�B>yB/B#�B�BB��B�4B��B�QB��B�uB�Bt�BgtB]7BVBF�BA�B9bB$�B�B
HB
�SB
��B
��B
�B
��B
�QB
�-B
w�B
k�B
_JB
P�B
H�B
C�B
9iB
)B
�B
hB
2B	�B
jB
pB
wB
	MB
(B	��B	�B	�YB	�/B	��B	�B	��B	��B	��B	�FB	�'B	�(B	�.B	~B	s�B	h�B	V"B	C�B	9wB	/:B	-.B	-.B	*B	qB	@B��B��B��B�B�~B�_B�)B�#B�0B��B��B��B��B��B��B��B��B�]B�dB�9B��B�B�B�NB�UB�$B��B�yB�BB�0B~%BzBu�Bt�Bs�Bn�Bi�Bi�Bh�Bj�Bn�Be�Bb�B[ZB_sB[[BXHBS+BPBMBH�BG�BE�BD�BC�BC�BC�B@�B?�B?�B?�B<�B<�B<�B;�B:�B:�B;�B>�BA�B@�B?�B@�BB�BA�BA�BA�B@�BA�BB�B?�B;�B6�B4�B3B2zB-\B,VB-]B.cB.dB-^B)GB(AB)HB.fB.gB-aB,\B+VB,]B0vB,^B.kB/rB0yB>�B>�BB�BEBK)BN>B`�Bk�Bu/B��B��B��B�DB��B��B��B�cB٤B��B�B��B	�B	B	PB	(�B	.�B	A*B	P�B	Y�B	dB	k1B	uqB	{�B	��B	��B	�"B	��B	��B	��B	��B	�B	�B	�7B	�YB	ǁB	лB	��B	�B	�2B	�"B	�B	��B	��B	��B	��B	��B
B
#B
 B
)B
QB
`B
uB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
$ B
(B
*+B
,:B
-BB
.KB
.NB
.QB
/YB
2oB
4~B
5�B
6�B
8�B
9�B
;�B
<�B
=�B
@�B
@�B
@�B
C�B
B�B
C�B
EB
GB
HB
I%B
J-B
K6B
L?B
MHB
MJB
NRB
O[B
PcB
QlB
QnB
RwB
S�B
S�B
U�B
U�B
V�B
W�B
X�B
X�B
Y�B
Z�B
\�B
\�B
\�B
\�B
]�B
]�B
^�B
_�B
bB
b	B
dB
dB
e#B
f,B
f/B
h>B
h@B
iJB
iMB
jVB
jYB
lgB
nvB
nyB
n{B
o�B
p�B
p�B
q�B
q�B
s�B
t�B
s�B
t�B
t�B
u�B
u�B
v�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
|B
|B
|B
�&B
�+B
�8B
�=B
�\B
�aB
�oB
�yB
��B
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
�B
�B
�B
�8B
�=B
�BB
�PB
�UB
�bB
�gB
�tB
�yB
��B
��B
��B
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
�B
�B
�.B
�JB
�_B
�zB
��B
��B
��B
��B
��B
�B
�B
�2B
�AB
�WB
�mB
�}B
��B
��B
��B
��B
��B
��B
��B
�	B
�B
�'B
�*B
�-B
�*B
�-B
�7B
�9B
�=B
�9B
�CB
�EB
�IB
�KB
�HB
�RB
�UB
�WB
�[B
�^B
�`B
�dB
�gB
�kB
�mB
�pB
�nB
�wB
�yB
�}B
��B
��B~�B~�B��B}�B~�B~�B~�B��B}�B�B~�B|�B�B|�B|�B}�B}�B}�B}�B}�B~�B~�B~�B~�B~�B}�B~�B~�B}�B}�B}�B~�B}�B}�B}�B}�B}�B}�B}�B}�B}�B|�B|�B|�B{�B|�B}�B}�B}�B��B��B��B��B��B��B��B��B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809240001572021061413554120210614135541202106171313012021061713130120210617131301201809240001572021061413554120210614135541202106171313012021061713130120210617131301PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018092400015720180924000157  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092400015720180924000157QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092400015720180924000157QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150420210617131504IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                