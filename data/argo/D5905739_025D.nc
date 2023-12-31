CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-06T07:03:07Z creation      
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
resolution        =���   axis      Z        8  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  dd   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  t�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     8  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
�Argo profile    3.1 1.2 19500101000000  20180806070307  20210617131500  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�u!��u,@�u!��u,11  @�u!��`P@�u!��`P@6�kz�]�@6�kz�]��cۍ%��S�cۍ%��S11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@@  @�  @�33@�33@�33A��A33A&ffAA��A`  A~ffA�33A�33A�33A�ffA�  A���A�  A�33B  BffBffB   B'��B/��B8  BA33BH��BPffBW��B_��Bh  BpffBxffB�33B�  B���B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�  B�  BǙ�B˙�B�ffB�33B�  B�  B���B���B���B�ffB�B�ffB���B�ffC L�C�fC  C  C�C
�C�C33C33CL�C33C33C�C  C��C  C L�C"33C#�fC&�C(L�C*�C+��C-�fC/�fC2  C4�C6�C833C:�C<33C>33C@�CA��CC��CE�fCH�CJL�CL33CN  CP33CR�CT  CV33CX�CY��C\�C^L�C`33Cb�Cc�fCe��Ch�Cj  Ck�fCn33Cp33Cq�fCt�Cv�Cw�fCz33C|�C~�C�  C��fC�  C�  C��fC�  C��C��C��C�  C��C��C��fC��3C��C��C��C��3C��C��C��C��fC�  C�  C��C��C��fC��3C��C��C�&fC�33C��C��3C��3C��C��C��C��C�&fC��C��fC��3C�  C��C��C�&fC��C��fC�  C��C��C��C�&fC��C�&fC�&fC��C��C��C��C�  C�  C�  C�  C�  C��fC��C�&fC�&fC��C��C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�&fC�&fC��C��fC��3C��C��C�&fC��C��3C��3C�  C��C��3C��fC�  C��C�  C��fC�  C�&fC��C��3C��C�  C��fC�  C��C��C��3C��C��C�&fC��C��fC��fC�  C��C��C�  C��fC�  C��C��C�33C��fD �fD3Dl�D�3D	,�D� D�fD9�D�fD�fD�D� D` D ��D#��D&  D(��D+S3D-��D0� D2�3D5��D8,�D:�3D=9�D?ٚDBy�DE3DG��DJ�DL� DO3DQ� DS�3DV` DXٚD[FfD]�3D`3DbffDd��Dg9�Di��Dk�3DnS3Dp�3Ds�Du�fDw�3DzY�D|` D~�fD��3D�ٚD� D�P D��fD�ɚD�3D�C3D��fD���D�)�D�vfD�� D�	�D�P D��fD��D�0 D�y�D���D�3D�C3D�|�D��fD��3D�&fD�VfD��3D�� D�ٚD�	�D�6fD�` D��3D��3D��3D��fD� D�,�D�S3D�y�D�� D�ɚD��3D�3D�33D�VfD�vfD��fD���D���D�3D�#3D�L�D�s3D�D��fD��3D��D�L�D�|�Dɰ D��D��D�L�D΀ Dϰ D��fD��D�9�D�i�D՜�D���D��fD��D�@ D�ffD܌�Dݰ D��fD�� D��3D�	�D�  D�0 D�FfD�P D�` D�s3D� D��D� D왚D��3DD﹚D���D���D��fD��3D�fD�3D�  D�0 D�FfD�S3D�FfD�VfD�p D��fD��fE VfE �3Ep E��E�3EfE� EfE� E4�E�fEP Ek3E� E
,�E@ ES3E��E��E{3E~fE~fE��E` EQ�E� E+3EfE�fE�3EٚE@ E �fE!� E"� E$L�E%@ E&��E($�E)�E*��E+��E,��E.p E/s3E0�3E1��E3nfE4l�E5�3E6�3E8X E9NfE:�fE<&fE>��EB�EE1�EH��EK� EN�3EQ�fET�fEX,�E[�E^\�Ea0 Ed[3Eg|�Ej��Em� Ep��Et$�EwA�EzD�E}t�E�L E���E�k3E��E�y�E� E��fE��E�D E�� E�� E�, E���E���E�3E�w3E�� E�f?   ?   >���>���>���>���>���?   >���>���>���?   >���>���>���>���>���>���>���?   ?   ?��?L��?fff?�  ?���?�33?�  ?ٙ�@   @��@��@,��@@  @S33@l��@�33@���@���@�  @�  @���@ə�@�ff@�33@�  A   A  A  AffAffA&ffA.ffA8  A<��AFffAL��AT��A\��Ad��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144141414441444141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ?fff?�  @&ff@`  @�  @�33@�33@�33A��A33A.ffAI��Ah  A�33A�33A�33A�33A�ffA�  A���A�  B��B
  BffBffB"  B)��B1��B:  BC33BJ��BRffBY��Ba��Bj  BrffBzffB�33B�  B���B�33B�33B�  B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�ffB�  B�  Bș�B̙�B�ffB�33B�  B�  B���B���B���B�ffB�B�ffB���B�ffC ��CffC� C� C��C
��C��C�3C�3C��C�3C�3C��C� CL�C� C ��C"�3C$ffC&��C(��C*��C,L�C.ffC0ffC2� C4��C6��C8�3C:��C<�3C>�3C@��CBL�CDL�CFffCH��CJ��CL�3CN� CP�3CR��CT� CV�3CX��CZL�C\��C^��C`�3Cb��CdffCfL�Ch��Cj� ClffCn�3Cp�3CrffCt��Cv��CxffCz�3C|��C~��C�@ C�&fC�@ C�@ C�&fC�@ C�L�C�Y�C�L�C�@ C�Y�C�L�C�&fC�33C�L�C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�@ C�@ C�Y�C�L�C�&fC�33C�L�C�Y�C�ffC�s3C�Y�C�33C�33C�L�C�L�C�Y�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�Y�C�Y�C�ffC�Y�C�ffC�ffC�Y�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�&fC�L�C�ffC�ffC�Y�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�ffC�ffC�L�C�&fC�33C�L�C�Y�C�ffC�L�C�33C�33C�@ C�L�C�33C�&fC�@ C�L�C�@ C�&fC�@ C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�&fC�&fC�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�Y�C�s3D 3D �fD33D��D�3D	L�D  D�fDY�DfD�fD9�D� D� D!�D#��D&@ D(ٚD+s3D.�D0� D33D5��D8L�D:�3D=Y�D?��DB��DE33DG��DJ9�DL� DO33DQ� DT3DV� DX��D[ffD]�3D`33Db�fDd��DgY�Di��Dl3Dns3Dp�3Ds9�Du�fDx3Dzy�D|� D~�fD��3D��D�  D�` D��fD�ٚD�3D�S3D��fD���D�9�D��fD�� D��D�` D��fD���D�@ D���D���D�3D�S3D���D��fD�3D�6fD�ffD��3D�� D��D��D�FfD�p D��3D��3D��3D��fD�  D�<�D�c3D���D�� D�ٚD�3D�#3D�C3D�ffD��fD��fD���D���D�3D�33D�\�D��3D¬�D��fD�3D�)�D�\�DȌ�D�� D���D�,�D�\�Dΐ D�� D��fD��D�I�D�y�Dլ�D���D�fD�,�D�P D�vfDܜ�D�� D��fD�� D�3D��D�0 D�@ D�VfD�` D�p D�3D� D��D� D쩚D��3DD�ɚD���D���D��fD�3D�fD�#3D�0 D�@ D�VfD�c3D�VfD�ffD�� D��fD��fE ^fE �3Ex E�E�3EfE� E&fE� E<�E�fEX Es3E� E
4�EH E[3E��E��E�3E�fE�fE��Eh EY�E� E33E&fE�fE�3E�EH E �fE!� E"� E$T�E%H E&��E(,�E)!�E*��E+��E,��E.x E/{3E0�3E1��E3vfE4t�E5�3E6�3E8` E9VfE:�fE<.fE>��EB$�EE9�EH��EK� EN�3EQ�fET�fEX4�E[$�E^d�Ea8 Edc3Eg��Ej��Em� Ep��Et,�EwI�EzL�E}|�E�P E���E�o3E��E�}�E� E��fE��E�H E�� E�� E�0 E���E���E�3E�{3E�� E�fG�O�G�O�G�O�?L��G�O�G�O�?L��G�O�?L��G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�?fffG�O�?�  ?���?�ff?�33?�  ?���?�33@   @��@   @,��@9��@L��@`  @s33@�ff@�33@���@���@�  @�  @���@ٙ�@�ff@�33A   A  A  A  AffA&ffA.ffA6ffA@  AD��ANffAT��A\��Ad��Al��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144141414441444141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             @ �@ �@ V@ {@ O@ "�@ )�@ 0x@ 8�@ >�@ F�@ R�@ _�@ l�@ z3@ ��@ ��@ �z@ �~@ ��@ ��@ ��@ �@ �q@@@
@+�@:@I�@V�@c�@o�@}�@��@�H@��@��@@ψ@ލ@�4@�,@�@*@"�@0x@>@K�@Yn@g@uk@�@�\@�@�M@��@�W@�O@�H@��@��@	�@6@&�@5?@B8@P�@]�@k�@v�@�@��@�@��@�k@��@�h@�@�@^@V@O@'�@6�@FQ@SI@^�@m�@|�@�7@��@�(@��@�&@�|@�#@�y@�q@�@o@g@+@8�@F�@V@e	@r@~K@�P@�H@�A@�F@�>@��@ލ@�@��@�@�@ �@0x@=q@Ji@Z@g�@s_@�d@�@�U@�@�@ƨ@Ӡ@��@��@��@�@�@&;@4�@A�@N�@]�@j@v@�p@�u@��@��@�@�@�h@�`@��@  @�@�@)�@5?@C�@R�@`�@oF@}�@��@��@�(@�-@��@�*@��@�(@�q@	@	b@	�@	-@	;d@	I�@	V@	a�@	p�@	~�@	�P@	��@	�M@	�F@	Ĝ@	�C@	�;@	�4@	��@
�@
{@
""@
/�@
=q@
K@
Wb@
g@
v@
��@
��@
��@
��@
�R@
��@
Ӡ@
�H@
��@
�E@
�@�@&;@4�@B�@P�@\�@hs@v�@��@�$@�y@��@�@ȴ@�
@�`@�@��@�@�@(�@5?@DD@S�@`B@l�@|?@��@��@��@��@��@�@�#@�y@��@@�@[@,`@:�@I@UU@a�@p�@~�@�P@�U@��@��@Ĝ@�*@Z�@��@�@3�@}�@ƨ@V@T�@�@�@*S@r@�R@�Q@FQ@�P@Ӡ@�@]�@�5@�(@/@v�@�w@v@Ji@��@Ӡ@�@X�@��@��@!s@c�@��@�@&;@g�@��@��@+@k�@��@�@@0x@r�@�9@�@-@qS@��@��@:@|?@�2@@H]@��@�\@[@dZ@��@��@6�@|?@Ĝ@ 
=@ P�@ ��@ �#@!g@!bN@!�4@!��@"*S@"k.@"�Y@"�@#+@#k�@#�@#�@$)�@$g@$��@$�@%""@%^�@%��@%܀@&O@&Z�@&�H@&׹@'*@'SI@'��@'�*@(�@(Ji@(�7@(ƨ@)%@)D�@)�p@)��@*@*B�@*�p@*�J@+�@+I�@+�D@+�@,�@,N�@,��@,ψ@-�@-P�@-�@-��@.o@.Q=@.�\@.�*@/�@/K@/�|@/@/�E@08�@0t@0�@0�y@1""@1\)@1��@1�7@2	�@2@�@2y�@2�-@2�(@3$/@3^�@3��@3є@4
�@4E�@4~�@4�R@4�Y@5-�@5g@5��@5��@6@6Lu@6�|@6��@6��@7:@7t�@7�r@7��@8%�@8^�@8��@8խ@9b@9N�@9�W@:@�@:�e@;i�@;�;@<��@<��@=�4@>�@>�@? �@?��@@"�@@�&@AZ@A��@B[z@B�@CYn@C�Y@D��@D�@E��@F�@F��@G[@G��@H&;@H�>@I-@I�o@Jm:@J��@K�@K�@L��@L�9@M��@N�@N�A@Ob@O�f@PF�@Qv@R�C@T"�@U��@VӠ@X4�@Yl�@Z�J@\<@]|�@^��@`{@an�@bĜ@d*@e~K@fĜ@h+@i~�@j�@l$/@mz�@n��@p$�@qg�@r��@tO@uoF@u��@u��@v33@v��@v@w�@wR�@w�C@w�/@x{@xe	G�O�G�O�G�O�@ G�O�G�O�@ G�O�@ G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�@ �G�O�@ j@ @ v@ %@ �@ �@ 	�@ 
=@ �@ �@ @ b@ o@ {@ �@ B@ �@ [@  �@ ""@ %�@ (G@ +@ -�@ 0x@ 33@ 6�@ :@ =q@ @,@ C�@ F�@ Ji@ N�@ P�@ T�@ Wb@ Z�@ ^5@ a�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A�  A�A�1A�1A�%A�1A�1A�1A�
=A�
=A�JA�oA�oA�{A�{A��A��A��A��A�{A��A��A�oA���A��HA���A�ȴAѾwAѺ^AѸRAѮAхA�=qA�bNA��mA���A���A�p�A�VA���A�XAŏ\A��A�A�~�A�1A���A���A��DA��A���A�A�A��A���A��PA���A���A��TA��\A�"�A��-A�S�A���A�G�A�ƨA���A�+A�"�A�l�A�{A�t�A��#A���A���A���A�(�A�VA�;dA���A��A�r�A���A��FA�z�A�$�A�9XA���A���A�VA��-A�?}A�bNA�z�A�JA�=qA�+A�`BA���A�bNA��7A���A�jA�9XA�x�A�;dA�XA���A��FA�A�A���A�bA���A���A�|�A��uA��9A��DA� �A���A�bA��hA�^A};dA{C�Ax�RAvv�AtffAr��Aq7LAp(�Anz�Al��AlA�Ajr�Ai�Ah�9Ag��Ag
=Ae��AdbNAa�-A_��A^�`A\�AZ(�AZbAY�-AX��AXQ�AX-AV�AU&�ASS�AR5?AP�AO�AO�AO33AM��AK�AJ1AIO�AI"�AIVAH�9AG�;AF��AD��ADbNACƨAC"�AA�A@�DA>�!A<�A;;dA:VA9�FA9C�A85?A7�A7C�A7+A6��A5C�A4��A41'A4�A3O�A1�^A0�+A0$�A/��A/7LA.ĜA.E�A-t�A+"�A)�A)hsA'��A&�HA&-A$ȴA#?}A"^5A!��A�TAv�A�A�/A�uA��AA��Ax�A��At�A�`A  A7LA=qA��A^5AS�A��Ax�A��A9XA�A%A
�A
bNA	dZA�9A�Ax�A�+AȴA�A ��@�ȴ@��@��F@�K�@�^5@��@��w@�?}@�1'@�F@�P@�!@�=q@�`B@�j@�+@�u@���@�b@�+@��@��
@�@υ@ȣ�@Ο�@��@�=q@�%@�I�@��@��@���@��#@�&�@��+@�p�@�X@�p�@��@�&�@���@�1@��^@��@�?}@��w@�@��D@��y@���@���@�ȴ@�$�@��`@�ƨ@��@�{@�%@��9@�1'@���@�n�@�p�@�A�@�K�@�M�@��@��@�"�@�v�@��h@��9@|�@|�j@z�!@y%@w+@u�-@t�@s�@r�\@qG�@p1'@l1@h1'@f�@e�@b~�@`r�@^v�@]�T@\��@\1@[��@Z�@Y�@X�@X  @U��@U/@T�j@Q�^@Q�@P�9@NV@M��@L�j@K�@J=q@I��@H �@G
=@F5?@E@D��@B�@AX@@�u@?l�@>�y@=�h@;ƨ@:��@9G�@7+@6��@4��@3�m@2�!@1x�@0��@0��@/K�@.V@-��@,�j@+�m@*��@*^5@*J@)hs@(�@';d@&ff@%�@%V@$��@#ƨ@#C�@!�#@!%@ Q�@��@?}@j@�F@��@�@�7@�@��@v�@@��@9X@t�@�@��@�`@��@��@$�@O�@�j@(�@"�@
�@
��@	�#@�`@|�@�R@��@�+@�@��@9X@ƨ@S�@��@J@�@hs@   ?�/?��#?�7L?�l�?��j?�F?��?��??�/?��H?�7L?�?�?��?�33?�G�?��;?��?��m?�"�?���?�r�?�
=?�E�?���?�Z?��
?�o?�M�?�%?Ѓ?�  ?�\)?���?θR?�O�?̬?�j?�C�?���?���?�l�?�`B?��?���?�G�?� �?��?��?�I�?�1?�"�?��H?�^5?�^5?��?��?�=q?�^5?�^5?���?���?�?�"�?���?�I�?��?�O�?���?��?�{?�{?�5??�V?���?���?��?��?���?�\)A��yA��A��A��mA��mA��mA��A��yA��yA��A��A��A��A��A��A��mA��`A��mA��mA��yA��`A��mA��`A��yA��A���A�  A�A�A�A���A�  A�A�%A�A�%A�1A�1A�1A�1A�1A�%A�%A�1A�1A�1A�%A�1A�1A�%A�
=A�JA�1A�1A�1A�
=A�
=A�
=A�
=A�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             A��yA��A�  A�A�1A�1A�%A�1A�1A�1A�
=A�
=A�JA�oA�oA�{A�{A��A��A��A��A�{A��A��A�oA���A��HA���A�ȴAѾwAѺ^AѸRAѮAхA�=qA�bNA��mA���A���A�p�A�VA���A�XAŏ\A��A�A�~�A�1A���A���A��DA��A���A�A�A��A���A��PA���A���A��TA��\A�"�A��-A�S�A���A�G�A�ƨA���A�+A�"�A�l�A�{A�t�A��#A���A���A���A�(�A�VA�;dA���A��A�r�A���A��FA�z�A�$�A�9XA���A���A�VA��-A�?}A�bNA�z�A�JA�=qA�+A�`BA���A�bNA��7A���A�jA�9XA�x�A�;dA�XA���A��FA�A�A���A�bA���A���A�|�A��uA��9A��DA� �A���A�bA��hA�^A};dA{C�Ax�RAvv�AtffAr��Aq7LAp(�Anz�Al��AlA�Ajr�Ai�Ah�9Ag��Ag
=Ae��AdbNAa�-A_��A^�`A\�AZ(�AZbAY�-AX��AXQ�AX-AV�AU&�ASS�AR5?AP�AO�AO�AO33AM��AK�AJ1AIO�AI"�AIVAH�9AG�;AF��AD��ADbNACƨAC"�AA�A@�DA>�!A<�A;;dA:VA9�FA9C�A85?A7�A7C�A7+A6��A5C�A4��A41'A4�A3O�A1�^A0�+A0$�A/��A/7LA.ĜA.E�A-t�A+"�A)�A)hsA'��A&�HA&-A$ȴA#?}A"^5A!��A�TAv�A�A�/A�uA��AA��Ax�A��At�A�`A  A7LA=qA��A^5AS�A��Ax�A��A9XA�A%A
�A
bNA	dZA�9A�Ax�A�+AȴA�A ��@�ȴ@��@��F@�K�@�^5@��@��w@�?}@�1'@�F@�P@�!@�=q@�`B@�j@�+@�u@���@�b@�+@��@��
@�@υ@ȣ�@Ο�@��@�=q@�%@�I�@��@��@���@��#@�&�@��+@�p�@�X@�p�@��@�&�@���@�1@��^@��@�?}@��w@�@��D@��y@���@���@�ȴ@�$�@��`@�ƨ@��@�{@�%@��9@�1'@���@�n�@�p�@�A�@�K�@�M�@��@��@�"�@�v�@��h@��9@|�@|�j@z�!@y%@w+@u�-@t�@s�@r�\@qG�@p1'@l1@h1'@f�@e�@b~�@`r�@^v�@]�T@\��@\1@[��@Z�@Y�@X�@X  @U��@U/@T�j@Q�^@Q�@P�9@NV@M��@L�j@K�@J=q@I��@H �@G
=@F5?@E@D��@B�@AX@@�u@?l�@>�y@=�h@;ƨ@:��@9G�@7+@6��@4��@3�m@2�!@1x�@0��@0��@/K�@.V@-��@,�j@+�m@*��@*^5@*J@)hs@(�@';d@&ff@%�@%V@$��@#ƨ@#C�@!�#@!%@ Q�@��@?}@j@�F@��@�@�7@�@��@v�@@��@9X@t�@�@��@�`@��@��@$�@O�@�j@(�@"�@
�@
��@	�#@�`@|�@�R@��@�+@�@��@9X@ƨ@S�@��@J@�@hs@   ?�/?��#?�7L?�l�?��j?�F?��?��??�/?��H?�7L?�?�?��?�33?�G�?��;?��?��m?�"�?���?�r�?�
=?�E�?���?�Z?��
?�o?�M�?�%?Ѓ?�  ?�\)?���?θR?�O�?̬?�j?�C�?���?���?�l�?�`B?��?���?�G�?� �?��?��?�I�?�1?�"�?��H?�^5?�^5?��?��?�=q?�^5?�^5?���?���?�?�"�?���?�I�?��?�O�?���?��?�{?�{?�5??�V?���?���?��?��?���?�\)A��yA��A��A��mA��mA��mA��A��yA��yA��A��A��A��A��A��A��mA��`A��mA��mA��yA��`A��mA��`A��yA��A���A�  A�A�A�A���A�  A�A�%A�A�%A�1A�1A�1A�1A�1A�%A�%A�1A�1A�1A�%A�1A�1A�%A�
=A�JA�1A�1A�1A�
=A�
=A�
=A�
=A�JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{B�B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B�B�B{B�B�B�B�B�B�B�B�B�B�B�B"�B^5B�+B�9B��B�BB�B��B��B�B(�B0!B49B@�BS�Bo�Bm�Bw�B�B�B�B�B{�B�PB��B��B��B��B��B��B��B��B��B�!B��B�?B�LB�9B�B��B��B��B��B��B�\B�Br�Bn�BffB^5BS�BE�B6FB0!B'�B �B�B�BB��B�B�B��B�qB�9B��B�bB�Bu�B_;BO�BC�B8RB0!B,B'�B�B
=B
�B
�TB
��B
ǮB
�jB
��B
��B
�bB
�JB
�B
�B
x�B
gmB
\)B
@�B
.B
�B
bB
B	��B	�B	�B	�B	�sB	�sB	�`B	�BB	�)B	�B	��B	�wB	�-B	�B	��B	�uB	�\B	�DB	�B	�B	~�B	v�B	o�B	hsB	cTB	ZB	XB	VB	R�B	I�B	B�B	;dB	8RB	6FB	5?B	2-B	.B	(�B	 �B	�B	�B	�B	bB	DB	  B��B�B�B�mB�ZB�5B�/B�#B�#B�B��B��B��B��BȴB��B�}B�jB�^B�LB�FB�9B�!B��B��B��B��B��B�{B�bB�7B�Bz�Bo�Bm�BjBbNBL�BJ�BG�BM�BT�BW
BVBR�BP�BO�BN�BM�BG�BF�BD�B@�B>wB=qB=qB?}B?}B@�B>wB>wB<jB:^B49B5?B2-B1'B0!B1'B0!B/B0!B/B1'B33B33B33B2-B49B49B5?B5?B7LB;dB9XB8RB7LB1'B9XBL�B?}B5?B�1B��B��B��B��B�!B�LBȴB�
B�B�B	B	�B	33B	1'B	:^B	Q�B	_;B	iyB	o�B	v�B	{�B	�B	�JB	��B	��B	�!B	�9B	�XB	�}B	ǮB	��B	��B	��B	��B	�B	�B	�;B	�NB	�fB	�B	�B	�B	�B	�B	��B	��B	��B
  B
B
%B
1B
	7B
JB
JB
PB
VB
bB
hB
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
"�B
%�B
%�B
'�B
'�B
+B
+B
+B
.B
-B
-B
0!B
0!B
1'B
2-B
33B
33B
5?B
5?B
7LB
7LB
8RB
;dB
<jB
=qB
>wB
?}B
A�B
B�B
D�B
E�B
G�B
F�B
H�B
H�B
J�B
K�B
L�B
L�B
N�B
N�B
O�B
P�B
R�B
R�B
S�B
R�B
T�B
T�B
W
B
XB
XB
YB
XB
ZB
ZB
\)B
\)B
^5B
_;B
aHB
aHB
aHB
cTB
cTB
dZB
gmB
ffB
hsB
iyB
jB
k�B
k�B
l�B
l�B
n�B
o�B
p�B
p�B
r�B
r�B
r�B
t�B
t�B
u�B
u�B
w�B
y�B
y�B
y�B
x�B
x�B
y�B
y�B
x�B
y�B
{�B
z�B
{�B
|�B
|�B
� B
�B
�B
�B
�B
�B
�B
�%B
�%B
�7B
�DB
�JB
�PB
�VB
�\B
�hB
�hB
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
�B
�B
�B
�B
�B
�'B
�-B
�3B
�9B
�9B
�?B
�?B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
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
�XB
�^B
�XB
�RB�B{B{B�B�B�B�B{B�B�B�B{B{B�BuBuB�B{B{B{B�B�B�B�B�B�B{B{B{BuB{B{B�B{B{B{B{B{B{B{BuB{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             BSBZBTBTBUBUBUBVBVBVBWBWBXBXBYBYBZB[B[B\BbBcB^BdBeBkBlBmBmBnBoBoBpBpB"�B^B�B�$B��B�.B�}B��B��B�B(�B0B4)B@sBS�Bo�Bm�Bw�B��B��B��B�B{�B�EB��B��B��B��B��B��B��B��B��B�B��B�:B�GB�5B�B��B��B��B��B��B�\B�Br�Bn�BfgB^7BS�BE�B6IB0%B'�B �B�B�BB��B�B�%B��B�yB�BB��B�lB�Bu�B_FBO�BC�B8_B0.B,B'�B�B
LB
�B
�dB
��B
ǾB
�{B
�B
��B
�tB
�]B
�B
�3B
x�B
g�B
\>B
@�B
.*B
�B
yB
6B	�B	��B	��B	��B	�B	�B	�zB	�]B	�EB	� B	��B	��B	�JB	�&B	��B	��B	�{B	�dB	�?B	�:B	B	v�B	o�B	h�B	cwB	ZAB	X4B	V)B	SB	I�B	B�B	;�B	8yB	6nB	5gB	2VB	.=B	) B	 �B	�B	�B	�B	�B	qB	 -B��B��B�B�B�B�eB�_B�TB�UB�6B�+B�B�B�B��B��B��B��B��B��B�~B�qB�ZB�5B�B�B��B��B��B��B�sB�VB{Bo�Bm�Bj�Bb�BMBKBG�BNBU@BWLBVGBS5BQ)BP#BOBNBG�BF�BD�B@�B>�B=�B=�B?�B?�B@�B>�B>�B<�B:�B4�B5�B2zB1uB0oB1vB0pB/kB0rB/lB1yB3�B3�B3�B2�B4�B4�B5�B5�B7�B;�B9�B8�B7�B1�B9�BM,B?�B5�B��B�B��B�B�B��B��B�0B׉B� B�5B	�B	B	3�B	1�B	:�B	R�B	_�B	jB	p?B	wmB	|�B	��B	��B	�VB	��B	��B	��B	�B	�;B	�oB	̋B	ϠB	ӻB	��B	��B	��B	�B	�&B	�@B	�\B	�qB	�B	��B	�B	��B	��B	��B
 �B
	B
B
	.B

6B
LB
PB
YB
bB
qB
zB
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$B
$B
'B
'B
),B
)/B
,CB
,FB
,IB
/^B
.ZB
.]B
1sB
1vB
2~B
3�B
4�B
4�B
6�B
6�B
8�B
8�B
9�B
<�B
=�B
>�B
?�B
@�B
CB
DB
FB
G(B
I6B
H3B
JBB
JEB
LUB
M]B
NfB
NiB
PxB
P{B
Q�B
R�B
T�B
T�B
U�B
T�B
V�B
V�B
X�B
Y�B
Y�B
Z�B
Y�B
[�B
[�B
]�B
]�B
`	B
aB
c!B
c$B
c&B
e5B
e7B
f@B
iVB
hQB
jaB
kiB
lrB
mzB
m}B
n�B
n�B
p�B
q�B
r�B
r�B
t�B
t�B
t�B
v�B
v�B
w�B
w�B
y�B
{�B
{�B
{�B
z�B
z�B
|B
|	B
{B
|B
~B
}B
~"B
+B
1B
�HB
�\B
�[B
�fB
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
�B
�B
�$B
�1B
�DB
�BB
�UB
�bB
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
� B
�B
�B
�B
�3B
�AB
�\B
�qB
��B
��B
��B
��B
��B
��B
�B
�#B
�1B
�GB
�VB
�lB
�|B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�&B
�6B
�EB
�IB
�LB
�NB
�RB
�TB
�XB
�aB
�^B
�gB
�dB
�aBYBTBSBYBYBYBYBSBYB_BYBTBSBYBMBMBYBSBSBSBYBZBZBZBZB`BTBTBTBNBTBTBZBTBTBTBUBUBUBUBOBUBUBUBVBVBVBVBVBVBVBWBWBWBWBWBWBWBXBXG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808060703072021061413553120210614135531202106171312312021061713123120210617131231201808060703072021061413553120210614135531202106171312312021061713123120210617131231PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018080607030720180806070307  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080607030720180806070307QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080607030720180806070307QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150020210617131500IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                