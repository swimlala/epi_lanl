CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-20T22:01:27Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � |Argo profile    3.1 1.2 19500101000000  20180820220127  20210617131501  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�x�ٱ�@@�x�ٱ�@11  @�x��}*�@�x��}*�@6Տq!�K@6Տq!�K�c��.�c��.11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@Fff@�ff@�33@�  @�  A��AffA$��AA��Aa��A���A�  A���A���A�  A�33A�  A���B   B  BffB��B ��B(ffB0ffB8ffB@ffBI33BPffBX  B`  Bg��Bo��Bw��B��B�33B�ffB�33B�  B���B�  B�33B���B���B�33B�33B�33B�ffB���B�33B�  B�  B�  B�ffB�ffBԙ�B�  B�  B�  B�33B�33B�ffB�ffB�33B�ffB�33C �C�C  C�fCL�C
33C�CL�C�C�C  C�fC�fC��C  C�fC��C"  C$  C%�fC(�C*L�C,  C-�fC0�C2L�C4  C5�fC833C:�C<  C>33C@  CA��CD�CF  CG��CJ�CK�fCM��CP�CR�CT  CV33CX33CZ�C\  C]�fC`33Cb  Cc�fCf33Ch  Ci�fCl33Cn�Co�fCr  Ct33Cv  Cw�3Cy�fC|  C~33C�&fC��C��fC�  C��C��C��C�  C��C�&fC�&fC�33C�  C�ٚC��fC�ٚC��fC��fC��fC��3C�  C�  C��C��C�  C��fC��3C�  C��C��C�&fC��C��fC��3C�  C��C�&fC��C��fC�  C��C�33C�&fC��C�  C��3C��C�  C��fC��C�  C��3C��C��C�  C��3C��fC��C��C��C��C��C��C��C��C��C��3C��3C��C��C�  C�&fC��C��C�  C��fC��C�  C��3C��C�  C��3C��C�  C��3C��C�  C��3C��C�  C��3C��C�&fC��C�  C�  C��3C��C��C�  C�  C��3C��C��C�  C�&fC�&fC�  C�  C��fC��C�&fC��C��3C��C��C�  C��C�  C��3C��C�  C��3C��C��C�33C��3C�  D �D ffD �3Dy�DfD��Dy�D
  D�3D  D�fD&fD��D,�D�3D��D!FfD$fD&��D)�fD,33D.� D1��D4,�D6�fD9l�D<3D>��DAY�DD  DF�3DI�DK��DN33DP��DS9�DU��DX&fDZ��D]&fD_� Db�Dd�fDg  Di�fDk��DnY�Dp��DsFfDu�3Dx&fDz�3D|��D  D��3D���D�)�D�c3D��fD��fD�  D�VfD��fD���D�3D�9�D�c3D���D���D��D��D�I�D�s3D��fD��3D���D�&fD�VfD��3D��fD��3D���D�#3D�P D�vfD��fD��3D�3D�)�D�VfD�� D�� D�ٚD�	�D�<�D�ffD���D���D���D�,�D�\�D��3D��3D��fD�  D�P D�vfD���D��fD���D�fD�9�D�c3Dƃ3Dǣ3D�� D���D��fD��D�  D�9�D�I�D�Y�D�ffD�s3DӉ�Dԙ�Dթ�DֶfD׼�D�ɚD�� D���D���D�  D��D�,�D�@ D�P D�c3D�p D�|�D剚D�fD��D��D�ɚD��3D�� D�� D���D��D�  D�,�D�@ D�P D�c3D�p D�� D��fD�� D���D���D��fD���D�ɚD�� D��3E ~fE E��E E��E)�E��E;3E�3E��E` Ed�E	��E
� EX ES3E� E��E+3E� E�fE�fE[3EFfE�fE�E��Eh E� E�3E 33E!��E"�fE#��E$�fE&P E'��E) E*�E+` E,� E-� E/�E0k3E1��E3 E3�fE56fE6vfE7�fE8�E:��E;��E>��EB�ED�3EHK3EK!�ENVfEQ�3ET� EW�EZ�fE]��Ea�EdfEgfEjp Emd�Ep��Es��EvɚEy�3E}.fE�'3E�� E�< E��fE�X E��E�-�E��3E���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?��?��?L��?L��?�  ?���?�ff?�  ?ٙ�?�33@��@��@,��@9��@S33@`  @y��@�ff@���@���@�33@���@���@�ff@�33@�  @���@���A33A	��A��A��A!��A(  A.ffA4��A>ffAD��AK33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144414144144441411414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?fff?�  @&ff@fff@�ff@�33@�  @�  A	��AffA,��AI��Ai��A���A�  A���A���A�  A�33A�  A���B  B
  BffB��B"��B*ffB2ffB:ffBBffBK33BRffBZ  Bb  Bi��Bq��By��B���B�33B�ffB�33B�  B���B�  B�33B���B���B�33B�33B�33B�ffB���B�33B�  B�  B�  B�ffB�ffBՙ�B�  B�  B�  B�33B�33B�ffB�ffB�33B�ffB�33C ��C��C� CffC��C
�3C��C��C��C��C� CffCffCL�C� CffC L�C"� C$� C&ffC(��C*��C,� C.ffC0��C2��C4� C6ffC8�3C:��C<� C>�3C@� CBL�CD��CF� CHL�CJ��CLffCNL�CP��CR��CT� CV�3CX�3CZ��C\� C^ffC`�3Cb� CdffCf�3Ch� CjffCl�3Cn��CpffCr� Ct�3Cv� Cx33CzffC|� C~�3C�ffC�L�C�&fC�@ C�L�C�L�C�L�C�@ C�Y�C�ffC�ffC�s3C�@ C��C�&fC��C�&fC�&fC�&fC�33C�@ C�@ C�Y�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�Y�C�ffC�L�C�&fC�@ C�Y�C�s3C�ffC�Y�C�@ C�33C�Y�C�@ C�&fC�Y�C�@ C�33C�L�C�L�C�@ C�33C�&fC�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�33C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�@ C�&fC�L�C�@ C�33C�Y�C�@ C�33C�Y�C�@ C�33C�L�C�@ C�33C�L�C�@ C�33C�L�C�ffC�Y�C�@ C�@ C�33C�L�C�L�C�@ C�@ C�33C�Y�C�L�C�@ C�ffC�ffC�@ C�@ C�&fC�L�C�ffC�Y�C�33C�Y�C�L�C�@ C�Y�C�@ C�33C�L�C�@ C�33C�L�C�Y�C�s3C�33C�@ D ,�D �fD3D��D&fD��D��D
  D�3D@ D�fDFfD��DL�D�3D��D!ffD$&fD&��D)�fD,S3D/  D1��D4L�D6�fD9��D<33D>ٚDAy�DD  DF�3DI9�DK��DNS3DPٚDSY�DU��DXFfDZ��D]FfD_� Db9�Dd�fDg  Di�fDl�Dny�Dp��DsffDu�3DxFfDz�3D|��D  D��3D���D�9�D�s3D��fD��fD�0 D�ffD��fD���D�3D�I�D�s3D���D���D���D�)�D�Y�D��3D��fD��3D��D�6fD�ffD��3D��fD��3D�	�D�33D�` D��fD��fD��3D�3D�9�D�ffD�� D�� D��D��D�L�D�vfD���D���D��D�<�D�l�D��3D��3D�fD�0 D�` D��fD���D��fD���D�&fD�I�D�s3DƓ3Dǳ3D�� D���D�fD��D�0 D�I�D�Y�D�i�D�vfD҃3Dә�Dԩ�Dչ�D��fD���D�ٚD�� D���D���D� D�)�D�<�D�P D�` D�s3D� D��D噚D�fD��D���D�ٚD��3D�� D�  D�	�D��D�0 D�<�D�P D�` D�s3D�� D�� D��fD�� D���D���D��fD�ɚD�ٚD�� E �E �fE E��E  E��E1�E��EC3E�3E��Eh El�E	��E
� E` E[3E� E��E33E� E�fE�fEc3ENfE�fE�E�Ep E� E�3E ;3E!��E"�fE$�E$�fE&X E'��E)  E*	�E+h E,� E-� E/�E0s3E1��E3 E3�fE5>fE6~fE7�fE8��E:��E;��E>��EB	�ED�3EHS3EK)�EN^fEQ�3ET� EW�EZ�fE]��Ea	�EdfEg&fEjx Eml�Ep��Es��EvњEz3E}6fE�+3E�� E�@ E��fE�\ E���E�1�E��3E���?L��G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�G�O�?L��G�O�?L��?fffG�O�?���G�O�?�ff?�  ?ٙ�?�ff@   @��@��@,��@9��@L��@Y��@s33@�  @���@�ff@���@���@�33@���@ə�@�ff@�33@�  @���AffA33A��A��A!��A)��A0  A6ffA<��AFffAL��AS33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144414144144441411414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ �@ �@ V@ *@ �@ "�@ (�@ /�@ 7L@ <�@ FQ@ R�@ `B@ n�@ z�@ �7@ ��@ ��@ ��@ �&@ �|@ �t@ �@ �q@�@o@g@-@:�@H]@Wb@c�@p�@~K@�D@��@��@�9@�>@є@ލ@�@�~@�@*@!s@/@>@K�@Yn@g�@v@�d@�\@�@��@��@�W@խ@�H@��@��@
�@�@&�@4�@A�@O�@\�@j@x&@�@�@�z@�r@�k@�o@׹@�`@�Y@�Q@�@�@(�@5�@B�@Q�@_�@l�@{�@��@�0@�(@�-@�2@��@��@�y@�q@j@o@�@+@:�@G�@S�@c�@o�@|�@��@�H@�A@�F@��@��@��@��@��@�@�@#�@/�@<�@Lu@Yn@e�@t@�@�\@��@��@�R@�W@խ@��@�@��@
�@�@&;@33@B8@P�@^5@l�@ww@�@�h@�a@��@�^@�@�[@�@�Y@^@@O@'�@5�@DD@R�@`�@oF@{�@�+@��@��@��@�2@�|@�@�@� @	%@	@	 @	,`@	9X@	I@	UU@	a�@	r@	~K@	�D@	�H@	��@	��@	��@	��@	�;@	��@	��@
1@
�@
#�@
0x@
>@
K�@
X@
e�@
uk@
�d@
�\@
�@
�@
�@
��@
�C@
��@
��@
��@�@�@$�@4�@@�@M�@\�@i�@v�@��@��@��@��@��@��@�
@�@�@ �@V@O@(�@5�@E�@R�@_�@oF@|�@��@�0@�y@�-@�2@�*@��@�y@�q@j@o@�@+�@:�@G�@T�@c�@r@�@�D@��@��@�-@�2@ψ@ލ@��@s_@�R@��@DD@�7@�|@b@V�@�@�y@3�@~�@��@*@^5@�A@�L@7�@~�@�W@�@X@��@�@.l@s_@��@��@C�@��@��@V@SI@��@�t@
@`A@��@��@*S@l�@�r@�@5?@x&@�^@�@33@t�@��@�9@>@�@�W@
=@Lu@��@��@*@Wb@��@�\@6@Wb@�<@�@ �@ Z@ �H@ ��@!B@!Z@!�H@!�h@"�@"Wb@"��@"�
@#�@#V�@#��@#׹@$�@$V�@$�0@$�
@%�@%Wb@%��@%�h@&�@&[z@&�U@&�/@'
@'`A@'�@'�@(""@(b�@(��@(��@) @)^�@)�a@)܀@*�@*Yn@*��@*Ӡ@+b@+Lu@+��@+@+��@,8�@,r�@,�@,�`@- �@-Z�@-��@-�*@.%@.?}@.ww@.��@.��@/%�@/a�@/�U@/�
@0@0K�@0�@0�w@0��@14�@1l�@1��@1��@2�@2Q�@2��@2Ĝ@2�Q@3:@3s_@3�@3�@4"�@4\)@4�0@4�*@5�@5@,@5v@5�r@5�(@6$/@6_�@6�H@6��@7�@7H]@7��@7�k@7�q@80x@8k.@8�4@9�@9@:1�@:խ@;@�@;�H@<Lu@<�@=S�@=�L@>�\@>�@?��@@&�@@�C@A!s@A�^@B �@B��@CUU@C�@DZ@D�e@E[z@E�q@F^�@F��@G��@H%�@H�7@I�@I��@JO@J�-@KE�@Kխ@Lff@L�J@MQ=@M��@NbN@N��@O�<@P[@QYn@Rƨ@T1@Uuk@V�Z@X	�@Yg�@Z@\�@]e�@^��@` �@aQ=@b�T@d�@eI�@f��@g�e@iK�@j��@l%@m[z@n�@o��@qB�@r��@t^@t4�@t}�@t�@ G�O�G�O�@ G�O�G�O�G�O�@ G�O�@ G�O�G�O�@ G�O�G�O�G�O�G�O�@ G�O�@ @ �G�O�@ G�O�@ v@ �@ 1@ �@ 
=@ �@ �@ @ b@ o@ �@ �@ �@ �@ �@ 
@  �@ "�@ $�@ '�@ *S@ -@ /�@ 2�@ 5�@ 7�@ :�@ >@ A�@ D�@ G�@ Ji@ M$@ Q=@ S�@ V�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�I�A�O�A�Q�A�/A�/A�;dA�7LA��A�1A͙�A�5?A��HA̺^Ạ�Ȁ\ÃA�n�A�jA�dZA�`BA�^5A�^5A�ZA�S�A�O�A�G�A�;dA�^5A��wA�VA��A�ZA���A��A���A�?}A�M�A��7A�JA��jA�9XA�{A���A��A�ȴA��A�dZA�I�A�(�A�ȴA���A��A�ZA���A�
=A��A��A�1'A��A��yA��+A�&�A��hA��A�Q�A�A�ZA�v�A�{A�K�A��TA�z�A�G�A�bNA�G�A�A�~�A�(�A�oA��A���A��A�(�A�VA�
=A���A��A�I�A�%A��A��9A�ȴA��+A��A�=qA�ȴA��A���A��7A�K�A��wA��mA��FA�t�A��yA���A��
A��A�\)A�(�A��A���A��A�M�A���A�+A�%A+A}�mA}�A|jAz��Ax{AwAv=qAu
=Aq+Ao�wAnz�Am��Al9XAj��AiC�AhbAg?}AfE�Ad=qAa�A`v�A^�A]��A]��A\�AZ�uAY�AX�AV��AU��AS�TAO�AM�AL{AK�AIAI�AH��AGXAFn�AE�AEl�AD��ADr�AC&�AB��AA��A@��A?��A>ZA=+A<A�A;
=A:�A9"�A8�\A8VA7��A7x�A6�RA5hsA4ȴA2ȴA1C�A/�A/VA.~�A.=qA-�A,�yA+hsA*  A)�A( �A't�A&��A&^5A&(�A%�;A%hsA$M�A#�7A#|�A#�hA#G�A#oA"�A!�
A!�A!x�A!dZA!G�A ��A ��AK�A��A �A�A/A��AK�A�A�!AbNA$�A�TA��AbAl�A�DA�A�uA��A��A��A�\A��A�TA�A	�A	dZA�AjA��A^5A �A+A�mA��A"�A �A ��A �!A VA @�C�@�v�@�p�@��@�1@��w@���@��@�b@�&�@�S�@�hs@� �@�;d@ى7@�$�@�J@��@�@��7@�Q�@��
@���@�7L@� �@�hs@��`@�ȴ@�O�@���@��j@���@��T@�%@��F@���@��-@�&�@���@�+@�J@�ƨ@�9X@�1'@���@���@�O�@�Ĝ@���@���@�v�@��@���@�{@�Q�@��;@��@���@��;@���@�5?@��-@�O�@��@�j@~�+@|(�@zJ@xr�@u`B@sdZ@q��@p�u@o�;@n�R@m?}@lz�@kt�@h��@g�w@f@d(�@a��@`�u@^@\��@[��@ZJ@Y7L@W;d@V��@Up�@TI�@S33@Q��@Q�@O�;@N@L�@L1@I��@I%@H  @F�y@E@D(�@CS�@B=q@A7L@@r�@?+@=�h@<��@;t�@:=q@8�@6��@5p�@4��@3dZ@2��@1�#@0��@/�w@/+@.5?@-`B@,I�@+t�@+S�@*n�@)X@)%@(r�@'+@&V@$��@#ƨ@#t�@"~�@!��@!7L@ ��@�w@v�@��@?}@��@"�@�@&�@Q�@�@�@�-@��@9X@"�@�!@��@7L@Ĝ@ �@�@ȴ@v�@�@z�@Z@9X@t�@C�@
=q@	��@Q�@|�@;d@��@�T@V@9X@�F@t�@S�@��@ A�?��-?�"�?�=q?��+?�?���?��?�w?�v�?�(�?�=q?�b?�
=?���?��
?�n�?��?�p�?�V?�(�?��H?���?�b?�
=?�ff?�ff?�`B?��/?ӶF?��?Ұ!?�hs?Ѓ?�|�?�{?�p�?̋D?�I�?˥�?�=q?ə�?��?��?�ff?���?\?�M�?��?�A�?���?�5??��?�p�?��?��D?�I�?���?�dZ?�C�?�dZ?��H?�?�C�?�"�?�C�?�ƨ?�1?��?�O�?�p�?�p�?��-A�K�A�K�A�M�A�O�A�O�A�O�A�S�A�Q�A�Q�A�S�A�VA�S�A�S�A�S�A�S�A�XA�O�A�S�A�M�A�Q�A�E�A�E�A�E�A�I�A�K�A�K�A�S�A�A�A�I�A�K�A�O�A�Q�A�S�A�S�A�S�A�S�A�I�A�5?A�+A�+A�(�A�/A�5?A�;dA�;dA�?}A�=qA�+A��A�bA�
=A�A�ƨA͙�A͑hA�r�A�XA�A�A�-A�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A�Q�A�I�A�O�A�Q�A�/A�/A�;dA�7LA��A�1A͙�A�5?A��HA̺^Ạ�Ȁ\ÃA�n�A�jA�dZA�`BA�^5A�^5A�ZA�S�A�O�A�G�A�;dA�^5A��wA�VA��A�ZA���A��A���A�?}A�M�A��7A�JA��jA�9XA�{A���A��A�ȴA��A�dZA�I�A�(�A�ȴA���A��A�ZA���A�
=A��A��A�1'A��A��yA��+A�&�A��hA��A�Q�A�A�ZA�v�A�{A�K�A��TA�z�A�G�A�bNA�G�A�A�~�A�(�A�oA��A���A��A�(�A�VA�
=A���A��A�I�A�%A��A��9A�ȴA��+A��A�=qA�ȴA��A���A��7A�K�A��wA��mA��FA�t�A��yA���A��
A��A�\)A�(�A��A���A��A�M�A���A�+A�%A+A}�mA}�A|jAz��Ax{AwAv=qAu
=Aq+Ao�wAnz�Am��Al9XAj��AiC�AhbAg?}AfE�Ad=qAa�A`v�A^�A]��A]��A\�AZ�uAY�AX�AV��AU��AS�TAO�AM�AL{AK�AIAI�AH��AGXAFn�AE�AEl�AD��ADr�AC&�AB��AA��A@��A?��A>ZA=+A<A�A;
=A:�A9"�A8�\A8VA7��A7x�A6�RA5hsA4ȴA2ȴA1C�A/�A/VA.~�A.=qA-�A,�yA+hsA*  A)�A( �A't�A&��A&^5A&(�A%�;A%hsA$M�A#�7A#|�A#�hA#G�A#oA"�A!�
A!�A!x�A!dZA!G�A ��A ��AK�A��A �A�A/A��AK�A�A�!AbNA$�A�TA��AbAl�A�DA�A�uA��A��A��A�\A��A�TA�A	�A	dZA�AjA��A^5A �A+A�mA��A"�A �A ��A �!A VA @�C�@�v�@�p�@��@�1@��w@���@��@�b@�&�@�S�@�hs@� �@�;d@ى7@�$�@�J@��@�@��7@�Q�@��
@���@�7L@� �@�hs@��`@�ȴ@�O�@���@��j@���@��T@�%@��F@���@��-@�&�@���@�+@�J@�ƨ@�9X@�1'@���@���@�O�@�Ĝ@���@���@�v�@��@���@�{@�Q�@��;@��@���@��;@���@�5?@��-@�O�@��@�j@~�+@|(�@zJ@xr�@u`B@sdZ@q��@p�u@o�;@n�R@m?}@lz�@kt�@h��@g�w@f@d(�@a��@`�u@^@\��@[��@ZJ@Y7L@W;d@V��@Up�@TI�@S33@Q��@Q�@O�;@N@L�@L1@I��@I%@H  @F�y@E@D(�@CS�@B=q@A7L@@r�@?+@=�h@<��@;t�@:=q@8�@6��@5p�@4��@3dZ@2��@1�#@0��@/�w@/+@.5?@-`B@,I�@+t�@+S�@*n�@)X@)%@(r�@'+@&V@$��@#ƨ@#t�@"~�@!��@!7L@ ��@�w@v�@��@?}@��@"�@�@&�@Q�@�@�@�-@��@9X@"�@�!@��@7L@Ĝ@ �@�@ȴ@v�@�@z�@Z@9X@t�@C�@
=q@	��@Q�@|�@;d@��@�T@V@9X@�F@t�@S�@��@ A�?��-?�"�?�=q?��+?�?���?��?�w?�v�?�(�?�=q?�b?�
=?���?��
?�n�?��?�p�?�V?�(�?��H?���?�b?�
=?�ff?�ff?�`B?��/?ӶF?��?Ұ!?�hs?Ѓ?�|�?�{?�p�?̋D?�I�?˥�?�=q?ə�?��?��?�ff?���?\?�M�?��?�A�?���?�5??��?�p�?��?��D?�I�?���?�dZ?�C�?�dZ?��H?�?�C�?�"�?�C�?�ƨ?�1?��?�O�?�p�?�p�?��-A�K�A�K�A�M�A�O�A�O�A�O�A�S�A�Q�A�Q�A�S�A�VA�S�A�S�A�S�A�S�A�XA�O�A�S�A�M�A�Q�A�E�A�E�A�E�A�I�A�K�A�K�A�S�A�A�A�I�A�K�A�O�A�Q�A�S�A�S�A�S�A�S�A�I�A�5?A�+A�+A�(�A�/A�5?A�;dA�;dA�?}A�=qA�+A��A�bA�
=A�A�ƨA͙�A͑hA�r�A�XA�A�A�-A�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Br�Bs�Bs�Bq�Br�Br�Br�Bq�Br�Bn�Bo�Bn�Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bl�BG�B|�Bv�Bt�Bu�Bq�Bl�Bn�Bl�Bn�Bk�BffBjBo�Bo�Bp�Bp�Bs�Bw�By�Bz�By�B�B�B�B�B�1B�DB�DB�PB�bB�hB�hB��B��B��B��B��B��B��B��B��B��B��B��B�\B�PB�7B}�Bk�BbNBT�BR�BH�BA�B:^B8RB2-B.B%�B�B{B%B��B�B�
B��B�RB�B��B��B�Bv�Bl�BaHB?}B33B&�B{BB
�B
�B
�yB
�
B
ĜB
�^B
�-B
��B
��B
�oB
{�B
x�B
q�B
m�B
[#B
N�B
H�B
C�B
8RB
�B
oB

=B
B	��B	�B	�TB	�;B	�B	��B	ŢB	�qB	�3B	��B	��B	��B	��B	�oB	�VB	�B	y�B	s�B	_;B	C�B	7LB	(�B	#�B	�B	�B	�B	VB	PB		7B	%B		7B	�B	�B	�B	{B	hB		7B	B��B��B�B�yB�TB�HB�5B�B�
B��BȴBǮB�LB�B��B��B��B��B��B��B�uB�hB�\B�DB�1B�+B�1B�%B�%B�B~�B�B�%B�1B�%B�=B�7B�=B�PB�PB�PB�PB�JB�7B}�Bz�Bs�Bk�BcTBaHB`BB^5B^5B\)B\)BZBVBT�BR�BO�BL�BH�BB�B=qB:^B;dB;dB9XB49B33B1'B/B/B,B-B,B)�B)�B,B-B+B)�B)�B(�B(�B'�B'�B'�B)�B(�B.B/B33B6FB;dB=qB<jB;dB?}B@�BB�BL�BR�BXB_;Bm�Bz�B�PB��B�B��B�`B��B	+B	PB	�B	%�B	-B	9XB	K�B	\)B	e`B	k�B	t�B	~�B	�B	�B	��B	�B	�'B	�FB	�jB	��B	ƨB	ɺB	��B	��B	�B	�;B	�TB	�`B	�B	�sB	�B	�B	��B	��B	��B	��B	��B
B
B
%B
	7B
DB
PB
PB
\B
\B
hB
uB
{B
�B
�B
�B
�B
�B
"�B
"�B
$�B
%�B
'�B
(�B
+B
,B
-B
-B
/B
0!B
2-B
2-B
49B
6FB
6FB
7LB
:^B
:^B
;dB
;dB
<jB
>wB
=qB
?}B
?}B
@�B
A�B
D�B
C�B
D�B
F�B
F�B
H�B
J�B
K�B
K�B
M�B
M�B
N�B
O�B
O�B
P�B
Q�B
R�B
S�B
S�B
T�B
VB
VB
W
B
XB
YB
[#B
\)B
\)B
]/B
^5B
_;B
^5B
`BB
aHB
aHB
bNB
bNB
cTB
dZB
dZB
ffB
gmB
hsB
hsB
iyB
jB
jB
k�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
v�B
w�B
w�B
x�B
x�B
z�B
}�B
~�B
~�B
�B
�B
�B
�B
�%B
�%B
�+B
�1B
�=B
�DB
�PB
�PB
�\B
�bB
�hB
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
�'B
�-B
�-B
�3B
�9B
�9B
�?B
�?B
�FB
�FB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�RB
�XB
�XBs�Bs�Bs�Br�Br�Bs�Br�Br�Bs�Br�Bs�Bs�Bs�Br�Bs�Br�Bt�Bq�Bs�Bp�Br�Bq�Br�Bs�Bs�Bt�Br�Bt�Bs�Bs�Bs�Br�Br�Bs�Bq�Br�Bm�Bq�Br�Br�Br�Br�Br�Br�Br�Br�Bq�Bp�Br�Bq�Bp�Bl�Bn�Br�Bm�Bn�Bn�Bo�Bk�Bn�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Br�Bs�Bs�Bq�Br�Br�Br�Bq�Br�BnrBoyBnsBlgBlgBkbBkcBkcBkdBkdBkeBllBllBlmBmsBmtBmuBmuBlpBG�B|�Bv�Bt�Bu�Bq�BlrBn�BlsBn�BknBfPBjiBo�Bo�Bp�Bp�Bs�Bw�By�Bz�By�B��B�B�B�B�#B�7B�7B�DB�WB�]B�^B�}B��B��B��B��B��B�{B��B��B��B��B��B�YB�MB�5B}�Bk�BbMBT�BR�BH�BA�B:_B8TB2/B.B%�B�BB*B��B�B�B��B�YB�"B��B��B�"Bv�Bl�BaRB?�B3>B&�B�BB
��B
�B
�B
�B
ĪB
�mB
�<B
�B
��B
��B
{�B
x�B
q�B
m�B
[6B
N�B
H�B
C�B
8gB
�B
�B

SB
)B	��B	�B	�lB	�TB	�6B	�B	żB	��B	�NB	�B	�B	��B	��B	��B	�tB	�*B	y�B	s�B	_[B	C�B	7lB	)B	#�B	�B	�B	�B	yB	tB		[B	JB		\B	�B	�B	�B	�B	�B		_B	;B�
B��B��B�B�B�tB�aB�JB�7B�B��B��B�{B�KB�,B�B�B��B��B��B��B��B��B�yB�fB�aB�gB�\B�\B�>B2B�EB�_B�kB�`B�xB�sB�yB��B��B��B��B��B�wB~4B{"Bs�Bk�Bc�Ba�B`�B^xB^yB\nB\nBZcBVJBUEBS9BP'BMBH�BB�B=�B:�B;�B;�B9�B4�B3�B1tB/iB/iB,WB-]B,XB*LB*MB,YB-`B+TB*OB*OB)JB)JB(EB(EB(FB*SB)MB.lB/sB3�B6�B;�B=�B<�B;�B?�B@�BB�BM5BS]BX}B_�BnB{WB��B�:B��B�B��B�}B	�B	�B	2B	&xB	-�B	9�B	LfB	\�B	fB	l-B	ugB	�B	��B	��B	�uB	��B	��B	�B	�-B	�OB	�qB	ʆB	ϨB	��B	��B	�B	�.B	�=B	�eB	�VB	�~B	��B	��B	��B	��B	��B	��B
B
B
%B

:B
JB
XB
[B
jB
mB
|B
�B
�B
�B
�B
�B
�B
 �B
#�B
#�B
&B
'B
)%B
*.B
,=B
-FB
.NB
.QB
0aB
1jB
3yB
3{B
5�B
7�B
7�B
8�B
;�B
;�B
<�B
<�B
=�B
?�B
>�B
@�B
@�B
A�B
CB
FB
EB
FB
H,B
H/B
J>B
LMB
MVB
MYB
OhB
OjB
PsB
Q|B
Q~B
R�B
S�B
T�B
U�B
U�B
V�B
W�B
W�B
X�B
Y�B
Z�B
\�B
]�B
]�B
^�B
_�B
aB
`B
bB
cB
cB
d&B
d(B
e1B
f9B
f<B
hJB
iTB
j\B
j_B
kgB
lpB
lrB
m{B
n�B
o�B
o�B
p�B
p�B
p�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
x�B
y�B
y�B
{B
{B
}B
�/B
�<B
�AB
�UB
�_B
�sB
�xB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�#B
�*B
�/B
�<B
�IB
�SB
�`B
�lB
�sB
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
�B
�B
�B
�$B
�?B
�VB
�pB
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�?B
�NB
�dB
�sB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�5B
�1B
�:B
�=Bs�Bs�Bs�Br�Br�Bs�Br�Br�Bs�Br�Bs�Bs�Bs�Br�Bs�Br�Bt�Bq�Bs�Bp|Br�Bq�Br�Bs�Bs�Bt�Br�Bt�Bs�Bs�Bs�Br�Br�Bs�Bq�Br�BmjBq�Br�Br�Br�Br�Br�Br�Br�Br�Bq�Bp~Br�Bq�Bp~BleBnrBr�BmlBnsBnsBoyBk`BnsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808202201272021061413553420210614135534202106171312402021061713124020210617131240201808202201272021061413553420210614135534202106171312402021061713124020210617131240PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018082022012720180820220127  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082022012720180820220127QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082022012720180820220127QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150120210617131501IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                