CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-08T17:02:19Z creation      
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
resolution        =���   axis      Z        p  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   LD   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  P`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  u\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20181008170219  20210617131505  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               &   &DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؄�D�@؄�D�11  @؄��G0@؄��G0@6���Q�@6���Q��c�f�3]%�c�f�3]%11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@ff@Fff@�ff@�33@�33@�  A��A  A!��A@  A`  A�  A���A�  A�  A�  A���A�33A�  B ffB  B  BffB ffB(ffB/��B8  B@ffBH  BP  BXffB`  Bh  BpffBx  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�33B�33B�  B���B���B�33B�  B�  B�33B�ffB�33B�33B�ffB�ffB�ffB�ffB���B���B�B�ffB���B���C   C  C  C�fC  C	�fCL�CL�CL�C33C33C�C  CL�CL�C�C �C"�C$  C&  C'�fC)��C,�C.  C/�fC2�C4  C5��C8�C:  C;��C>  C@L�CB�CC�fCF  CH�CJ  CK��CN  CP  CR33CTL�CV33CW�fCZ�C\33C^L�C`�Ca��Cc��Ce�fCg�fCj�Cl33Cn  Co�fCq�fCt  Cv�Cx�Cz33C|L�C~�C��C�  C��C�&fC��C��3C��3C��C�  C��fC�  C��C��C�  C��fC��3C��3C�  C��C�&fC��C�  C��C��C��3C��C��C�  C��fC��3C��C�&fC��C��3C��C�  C��fC�  C��C�  C��fC��C�&fC��C��3C�  C��C��C��fC��3C�  C��C��C��C�&fC��C��3C�  C��C��C��C��C��C�&fC��C��3C��3C�  C�  C��C��C�&fC��C�  C�  C��C��C��C��fC�  C��C�&fC��C��3C�  C��C��C��3C�ٚC��fC��3C��C��C��C��fC��3C�  C��C�&fC��C��3C��C��C��C��3C�  C�&fC�  C��fC�  C��C��C�  C��fC��fC�  C��C��C��3C�ٚC��fC��3C�  C��C�&fC�33C�  C��D �D s3D  D��D�fD�D	�3Dl�D�D� D33D��D9�D� DL�D ��D#&fD%��D'��D*y�D,��D/y�D2fD4� D733D9�fD<Y�D>�3DA�3DD9�DF�3DI�fDL  DN� DQY�DS��DV�fDY3D[�fD^33D`�3Dc33De��Dh33Dj��Dm3Do� Dr3Dt�fDv�3DyS3D{` D}��D�3D�FfD�p D��fD�� D���D�,�D�` D��fD��fD��fD�)�D�c3D��3D�� D�	�D�<�D�p D��3D��fD� D�L�D�� D��fD���D�9�D�vfD��fD�� D�&fD�` D��3D�ٚD��D�@ D�y�D���D���D��D�<�D�l�D���D��3D��D�fD�9�D�\�D���D���D��3D� D�FfD�s3D�� D��3D�fD�6fD�ffDŖfDƼ�D�ٚD���D�,�D�P D�y�Dͩ�D�� D���D�)�D�\�Dӌ�DԼ�D��D�fD�C3D�s3Dک�D���D� D�<�D�ffD��fD��fD�� D�  D�S3D�vfD�3D��3D���D�#3D�L�D�vfD��D��3D��D�fD�9�D�Y�D�vfD���D���D���D���D�  D��D�6fD�VfD�vfE I�E �fE` E�3Ex E�E� E�E�fE0 E��E>fE�fEP E\�E	��E
� E�3E|�E� E�EfE�fE� E� EX Ea�E�fE��E E��E�fE( E ,�E!�fE"�3E#� E%>fE&A�E'� E(��E*  E+�fE,x E-ٚE/1�E0� E1ɚE2��E3�3E5@ E6��E7��E9$�E:l�E;�fE>�3EA� ED��EHA�EKA�EN)�EQ~fETd�EW��EZњE^fEa4�EdY�Eg` EjP Em��Ep��Es��Ev��Ez3E}�E�.fE���E�:fE�� E�W3E�� E�}�E�fE��fE�73E�s3E�� E��E�k3E��3E�3E�a�E��3E���E�Q�E���E�� E�:fE��fE��3E�3>���>���>���>���>L��>���?   >���>���>���>���>���>���>���?   >���?   >���>���?   ?   >���?��?��?333?fff?�  ?�ff?�  ?���?�33@��@   @,��@9��@S33@`  @s33@�ff@�  @���@�33@�  @���@ə�@�33@�33@���@���A��A��A33A��A#33A)��A1��A9��A@  AH  AP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414411414444414144144141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?fff?�33@&ff@fff@�ff@�33@�33@�  A	��A  A)��AH  Ah  A�  A���A�  A�  A�  A���A�33A�  BffB
  B  BffB"ffB*ffB1��B:  BBffBJ  BR  BZffBb  Bj  BrffBz  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�33B�33B�  B���B���B�33B�  B�  B�33B�ffB�33B�33B�ffB�ffB�ffB�ffB���B���B�B�ffB���B���C � C� C� CffC� C
ffC��C��C��C�3C�3C��C� C��C��C��C ��C"��C$� C&� C(ffC*L�C,��C.� C0ffC2��C4� C6L�C8��C:� C<L�C>� C@��CB��CDffCF� CH��CJ� CLL�CN� CP� CR�3CT��CV�3CXffCZ��C\�3C^��C`��CbL�CdL�CfffChffCj��Cl�3Cn� CpffCrffCt� Cv��Cx��Cz�3C|��C~��C�&fC�@ C�Y�C�ffC�L�C�33C�33C�Y�C�@ C�&fC�@ C�L�C�Y�C�@ C�&fC�33C�33C�@ C�L�C�ffC�L�C�@ C�L�C�L�C�33C�L�C�L�C�@ C�&fC�33C�L�C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�@ C�&fC�L�C�ffC�L�C�33C�@ C�L�C�L�C�&fC�33C�@ C�L�C�Y�C�Y�C�ffC�Y�C�33C�@ C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�L�C�33C�33C�@ C�@ C�L�C�Y�C�ffC�L�C�@ C�@ C�Y�C�Y�C�L�C�&fC�@ C�Y�C�ffC�L�C�33C�@ C�L�C�L�C�33C��C�&fC�33C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�@ C�ffC�@ C�&fC�@ C�L�C�Y�C�@ C�&fC�&fC�@ C�L�C�Y�C�33C��C�&fC�33C�@ C�L�C�ffC�s3C�@ C�Y�D 9�D �3D  D��DfD9�D	�3D��D,�D� DS3DٚDY�D� Dl�D ٚD#FfD%��D(�D*��D-�D/��D2&fD4� D7S3D9�fD<y�D?3DA�3DDY�DG3DI�fDL@ DN� DQy�DT�DV�fDY33D[�fD^S3D`�3DcS3DeٚDhS3Dj��Dm33Do� Dr33Dt�fDw3Dys3D{� D}ٚD�#3D�VfD�� D��fD�� D��D�<�D�p D��fD��fD�fD�9�D�s3D��3D�� D��D�L�D�� D��3D��fD�  D�\�D�� D��fD�	�D�I�D��fD��fD�  D�6fD�p D��3D��D��D�P D���D���D���D��D�L�D�|�D���D��3D���D�&fD�I�D�l�D���D�ɚD��3D�  D�VfD��3D�� D��3D�fD�FfD�vfDŦfD���D��D��D�<�D�` D̉�D͹�D�� D��D�9�D�l�DӜ�D���D���D�&fD�S3Dك3Dڹ�D���D�  D�L�D�vfD�fD��fD�  D�0 D�c3D�fD�3D��3D�	�D�33D�\�D�fD��D��3D���D�&fD�I�D�i�D��fD���D���D���D�	�D� D�)�D�FfD�ffD��fE Q�E �fEh E�3E� E�E� E$�E�fE8 E��EFfE�fEX Ed�E	��E  E3E��E� E�EfE�fE� E� E` Ei�E�fE�E E��E�fE0 E 4�E!�fE"�3E#� E%FfE&I�E'� E(��E*( E+�fE,� E-�E/9�E0� E1њE2��E3�3E5H E6��E7��E9,�E:t�E;�fE>�3EA� ED��EHI�EKI�EN1�EQ�fETl�EW��EZٚE^fEa<�Eda�Egh EjX Em��Ep��Es��Ew�Ez3E}�E�2fE���E�>fE�� E�[3E�� E���E�
fE��fE�;3E�w3E�� E��E�o3E��3E�3E�e�E��3E���E�U�E���E�� E�>fE��fE��3E�3G�O�?L��G�O�G�O�?333?fffG�O�?L��G�O�G�O�G�O�G�O�G�O�?fffG�O�?L��G�O�G�O�?fffG�O�G�O�?fffG�O�?���?���?�33?�  ?�ff@   @ff@��@,��@@  @L��@Y��@s33@�  @���@�ff@�  @���@�33@�  @���@ٙ�@�33@�33@���A��A��A��A33A!��A+33A1��A9��AA��AH  AP  AX  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414411414444414144144141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @ �@ %@ V@ *@ �@ "�@ )�@ /�@ 7L@ =q@ D�@ Q�@ _�@ m:@ {�@ ��@ �0@ ��@ �-@ �w@ ��@ �#@ �@ ��@@�@g@+�@:@H]@UU@b�@qS@~K@��@�H@�A@��@@�7@��@�4@��@%@{@""@/�@>@K�@X�@e�@s_@�d@�\@�@�Y@��@ƨ@�O@�@�L@��@�@6@$�@5?@B8@P�@^5@i�@ww@�@�@�m@�f@��@�o@�@�@�@ �@�@[@+@7L@D�@R�@_�@m:@z3@�+@��@��@��@��@��@�@��@��@@@ �@-@9X@G�@V@b�@oF@~K@��@��@�M@�F@��@��@�;@�@��@v@@!s@/@>@Lu@X�@e�@s_@��@�@��@�@�^@ƨ@�C@�H@�L@��@
�@6@$�@4�@@�@M$@\)@j@x�@�@�h@��@�f@��@�@�@�`@�Y@ �@V@�@)�@7L@DD@P�@^�@m�@|�@�7@��@�5@�~@��@��@��@�@�e@	@	@	g@	+�@	:@	H]@	V@	a�@	o�@	~K@	��@	��@	��@	��@	��@	ψ@	��@	�4@	��@
1@
�@
#�@
1�@
>@
Ji@
X@
ff@
t@
�d@
��@
�@
�Y@
�R@
��@
��@
�@
�@
�9@
=@B@'�@3�@@,@N�@\�@j@v�@�@�h@��@��@�@�@խ@�@�Y@ �@�@�@(G@7L@E�@R�@^�@m:@|�@��@��@��@�-@��@��@�@�@��@@o@
@*S@8�@F�@UU@c�@r�@�@��@��@��@��@@є@�#@i!@�!@��@B8@��@��@�@X@�@�@$�@g@��@��@/@s_@��@�E@DD@��@��@6@^5@��@�@@8�@~�@��@�@T�@��@��@'�@m�@��@��@<@�@Ĝ@1@I�@�\@�C@*@Wb@�<@�7@b@R�@�#@Ӡ@�@UU@��@�\@�@Z@��@��@[@`A@�@�@'�@i!@��@�4@ -�@ p�@ �9@ ��@!7�@!|�@!�2@"�@"I@"��@"�*@#@#V@#�<@#��@$O@$^5@$�@$��@%!s@%bN@%�(@%�T@&"�@&a�@&��@&��@'
@'^5@'�@'ލ@(�@(`�@(�@(�H@)"�@)dZ@)�4@)�@*&�@*e�@*�z@*��@+!s@+_�@+�@+��@,�@,^�@,�@,��@-!s@-bN@-�z@-�@."�@.c�@.��@.�m@/(�@/i!@/��@/�y@0*S@0i�@0��@0�4@1*S@1j@1�Y@1�(@2)�@2i!@2��@2�m@3&;@3e	@3�4@3�T@4 �@4]�@4��@4��@56@5S�@5��@5�@6�@6B8@6�@6�k@6�}@733@7n�@7��@7�@8""@8^5@8��@8Ӡ@9J@9F�@9�@9��@:.l@:�@;K@;��@<^5@<��@=r�@=�@>��@?j@?ww@@%�@@��@AC�@A��@B%�@B�|@C@,@C�@DWb@D��@Ek.@E�t@F�@F�@G��@G�E@H�0@I2�@I�0@J-@J��@KN�@K�#@L:�@L�W@MUU@M��@NoF@N��@O��@P�@Q]�@R�~@S��@UqS@V�@W�q@YbN@Z�@\ �@]\�@^�w@`�@am�@b�R@c�,@eYn@f��@h�@i`B@j�r@k��@ma�@n��@o�8@qN�@r�(@t  @uSI@v�y@x@y[z@y��@y��@zo@zbN@z��@z�l@{4�@{i!@{�F@|^@|4�@|~K@|�@}�@}X@}��G�O�@ G�O�G�O�@ ^@ �G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�@ G�O�G�O�@ �G�O�G�O�@ �G�O�@ @ �@ %@ �@ �@ 
=@ 
�@ �@ @ @ o@ �@ �@ �@ �@ �@ �@  �@ "�@ %�@ (G@ +@ -@ 0x@ 2�@ 5?@ 8�@ <@ >�@ A�@ E�@ H]@ K�@ O0@ Q�@ UU@ X�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�5?A�5?A�1'A�-A�/A�1'A�/A�/A�/A�/A�+A�$�A���A�"�A��;A���AӰ!Aӣ�Aә�AӋDA�~�A�n�A�\)A�M�A�G�A�C�A�;dA�1'A�+A� �A�bA�1A�  A��A���Aҗ�A�n�A���A�O�A��A�+A��/A���A˲-A�K�A�C�A� �A��FA�1A�Q�A��A���A�A���A�$�A��A�Q�A��FA�XA���A�z�A�v�A���A�-A��A��FA�VA�t�A��#A��7A�$�A��A��hA� �A��yA�/A�jA��jA��;A�ĜA��A�ȴA��A���A�&�A��/A���A���A�+A���A�n�A���A��#A� �A�7LA�ĜA���A��-A��hA��`A���A��\A�?}A���A�\)A��A�A�;dA�"�A��A�33A��yA��jA�%A�A�A��A�O�A�dZA�JA~�A{VAu�TAs��As7LAr�`Ar5?Ao&�Al~�Aj1'Ahv�Ag�Ae�Ac�Ac��Act�Ab�!Aa�^A`�DA^�A[XAY�
AX��AW��AU�wAUC�AU+ATv�AS?}AQ%AN��AL�+AKl�AJM�AHVAF��AF=qAE;dACG�ABjA@ĜA@JA?"�A>ȴA>1A<�A;/A9��A8�/A7�A6��A5�FA5hsA49XA2ĜA2�\A2E�A0�`A/�A/VA.�`A.v�A-��A-��A-
=A,��A+�A)XA(�!A'�A'&�A&A�A$�/A$~�A#|�A!�hA ZAG�A�A�jAM�A�yA+A;dA�9A�DA1'An�A��A33A�A�AA�-A��A��A�PAn�A�jA�AoA
jAv�AC�A�AS�A-AhsAC�A�/Av�A  A�A �yA (�@��m@�;d@�ff@���@� �@��@���@�b@�@��@��`@�+@�E�@��@�@�@�hs@�?}@�@�K�@�@陚@�Ĝ@�t�@�^5@��@�o@�x�@�I�@߮@�dZ@ҏ\@ʰ!@��@���@��P@�~�@���@�X@��T@���@��H@�`B@�;d@��h@�+@���@��-@��-@�ƨ@���@�J@���@�&�@�A�@���@��\@�9X@��j@�9X@��@��D@�1@�
=@���@�Z@��R@���@�7L@�l�@���@���@�9X@��+@�-@��@�b@;d@|�@y��@xbN@x1'@w+@u@t(�@q��@o�@n�R@m�@lZ@j�@i�@g�w@e��@cƨ@`��@_�;@^��@\�/@["�@Y�^@XĜ@W�@V$�@S�
@Q�^@Ol�@N@L9X@Kt�@I��@I%@G�;@F�@E�-@C��@BM�@@r�@?l�@>5?@<j@;��@:�H@9��@8Ĝ@7\)@6v�@5�-@4j@3��@2�H@1��@1X@0 �@.ȴ@-�-@,9X@+C�@*��@*�@)&�@(�@(A�@&ȴ@%��@$1@"�H@"�@!7L@  �@|�@�@ff@�@�@9X@�
@~�@�^@hs@�@�;@;d@5?@@�-@O�@�/@S�@��@�7@7L@��@�9@�;@;d@v�@�T@O�@��@t�@
=q@��@r�@  @�@��@ff@p�@��@j@�
@C�@n�@-@�@ �`@ b?��R?�5??�/?�(�?�C�?�X?�1'?�l�?�ff?�z�?��?��?�R?�V?�ƨ?�1'?�?��?�\?�-?�\)?ޗ�?܋D?�ƨ?���?ؓu?�1'?�+?�$�?ա�?��?��?�t�?���?�J?��`?��;?�v�?���?�p�?���?�ƨ?��H?���?ə�?���?���?Ƨ�?�?�?}?��?�-?���?��`?�\)?�V?��h?��?�(�?��H?�~�?��^?���?���?��^?���?�=q?���?�"�?���?�(�?��?�V?��-?�V?���?���?��w?��;?�  ?� �?�A�?�bN?���?���?�Ĝ?��`?�%?��`?�%?�&�?�&�?�&�A�/A�/A�1'A�1'A�/A�-A�/A�/A�1'A�5?A�33A�33A�33A�33A�33A�33A�5?A�7LA�5?A�5?A�33A�33A�1'A�5?A�33A�33A�5?A�7LA�5?A�5?A�5?A�5?A�33A�5?A�33A�/A�+A�+A�-A�-A�-A�/A�/A�/A�1'A�/A�1'A�/A�/A�-A�/A�/A�/A�/A�/A�/A�+A�(�A�+A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  A�33A�5?A�5?A�1'A�-A�/A�1'A�/A�/A�/A�/A�+A�$�A���A�"�A��;A���AӰ!Aӣ�Aә�AӋDA�~�A�n�A�\)A�M�A�G�A�C�A�;dA�1'A�+A� �A�bA�1A�  A��A���Aҗ�A�n�A���A�O�A��A�+A��/A���A˲-A�K�A�C�A� �A��FA�1A�Q�A��A���A�A���A�$�A��A�Q�A��FA�XA���A�z�A�v�A���A�-A��A��FA�VA�t�A��#A��7A�$�A��A��hA� �A��yA�/A�jA��jA��;A�ĜA��A�ȴA��A���A�&�A��/A���A���A�+A���A�n�A���A��#A� �A�7LA�ĜA���A��-A��hA��`A���A��\A�?}A���A�\)A��A�A�;dA�"�A��A�33A��yA��jA�%A�A�A��A�O�A�dZA�JA~�A{VAu�TAs��As7LAr�`Ar5?Ao&�Al~�Aj1'Ahv�Ag�Ae�Ac�Ac��Act�Ab�!Aa�^A`�DA^�A[XAY�
AX��AW��AU�wAUC�AU+ATv�AS?}AQ%AN��AL�+AKl�AJM�AHVAF��AF=qAE;dACG�ABjA@ĜA@JA?"�A>ȴA>1A<�A;/A9��A8�/A7�A6��A5�FA5hsA49XA2ĜA2�\A2E�A0�`A/�A/VA.�`A.v�A-��A-��A-
=A,��A+�A)XA(�!A'�A'&�A&A�A$�/A$~�A#|�A!�hA ZAG�A�A�jAM�A�yA+A;dA�9A�DA1'An�A��A33A�A�AA�-A��A��A�PAn�A�jA�AoA
jAv�AC�A�AS�A-AhsAC�A�/Av�A  A�A �yA (�@��m@�;d@�ff@���@� �@��@���@�b@�@��@��`@�+@�E�@��@�@�@�hs@�?}@�@�K�@�@陚@�Ĝ@�t�@�^5@��@�o@�x�@�I�@߮@�dZ@ҏ\@ʰ!@��@���@��P@�~�@���@�X@��T@���@��H@�`B@�;d@��h@�+@���@��-@��-@�ƨ@���@�J@���@�&�@�A�@���@��\@�9X@��j@�9X@��@��D@�1@�
=@���@�Z@��R@���@�7L@�l�@���@���@�9X@��+@�-@��@�b@;d@|�@y��@xbN@x1'@w+@u@t(�@q��@o�@n�R@m�@lZ@j�@i�@g�w@e��@cƨ@`��@_�;@^��@\�/@["�@Y�^@XĜ@W�@V$�@S�
@Q�^@Ol�@N@L9X@Kt�@I��@I%@G�;@F�@E�-@C��@BM�@@r�@?l�@>5?@<j@;��@:�H@9��@8Ĝ@7\)@6v�@5�-@4j@3��@2�H@1��@1X@0 �@.ȴ@-�-@,9X@+C�@*��@*�@)&�@(�@(A�@&ȴ@%��@$1@"�H@"�@!7L@  �@|�@�@ff@�@�@9X@�
@~�@�^@hs@�@�;@;d@5?@@�-@O�@�/@S�@��@�7@7L@��@�9@�;@;d@v�@�T@O�@��@t�@
=q@��@r�@  @�@��@ff@p�@��@j@�
@C�@n�@-@�@ �`@ b?��R?�5??�/?�(�?�C�?�X?�1'?�l�?�ff?�z�?��?��?�R?�V?�ƨ?�1'?�?��?�\?�-?�\)?ޗ�?܋D?�ƨ?���?ؓu?�1'?�+?�$�?ա�?��?��?�t�?���?�J?��`?��;?�v�?���?�p�?���?�ƨ?��H?���?ə�?���?���?Ƨ�?�?�?}?��?�-?���?��`?�\)?�V?��h?��?�(�?��H?�~�?��^?���?���?��^?���?�=q?���?�"�?���?�(�?��?�V?��-?�V?���?���?��w?��;?�  ?� �?�A�?�bN?���?���?�Ĝ?��`?�%?��`?�%?�&�?�&�?�&�A�/A�/A�1'A�1'A�/A�-A�/A�/A�1'A�5?A�33A�33A�33A�33A�33A�33A�5?A�7LA�5?A�5?A�33A�33A�1'A�5?A�33A�33A�5?A�7LA�5?A�5?A�5?A�5?A�33A�5?A�33A�/A�+A�+A�-A�-A�-A�/A�/A�/A�1'A�/A�1'A�/A�/A�-A�/A�/A�/A�/A�/A�/A�+A�(�A�+A�(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B(�BL�BO�BR�BXBZB[#B\)B]/B^5B_;BaHBaHBaHBbNBbNBbNBbNBbNBbNBcTBbNBcTBe`Be`BdZBhsBjBffBn�Bk�Bn�BjBu�B�oB�VB�PB�PB�VB��B��B��B��B��B��B��B��B��B��B��B�%B��B��B��B��B��B��B��B�uB�DB�B{�Bl�BgmB[#BVBI�B>wB7LB#�B
=B��B�B�HB�;B�#B��B��B��B��B�qB�B��B��B�JB}�Bk�BcTB^5BE�B:^B%�B �B�B�B
=B
��B
�B
�TB
�/B
�B
ǮB
�9B
��B
��B
�uB
�B
p�B
I�B
-B
�B
�B
�B
�B
%B	�B	��B	ƨB	�dB	�B	��B	��B	��B	��B	��B	�hB	�B	jB	bNB	ZB	O�B	E�B	B�B	A�B	9XB	7LB	$�B	�B	\B	1B	B��B	B��B��B�B�B�BB�)B��B��B��BÖB�dB�RB�'B�B��B��B��B��B�{B�uB�hB�VB�\B�\B�PB�JB�hB�{B��B��B�1B�B�B�B�B~�Bv�Bz�Bx�Bo�Bl�Bk�Bl�Bn�Bs�Bo�B\)BS�BP�BO�BQ�BM�BO�BN�BL�BJ�BJ�BF�BG�BF�B@�B?}B7LB9XB9XB8RB5?B49B2-B0!B0!B1'B/B49B;dB;dB:^B8RB6FB5?B2-B1'B0!B-B)�B,B,B,B,B)�B-B-B-B,B/B/B.B/B0!B2-B2-B33B49B7LB6FB9XB:^B<jB;dB;dBC�BN�BM�B[#BhsBk�Bt�B�DB��B��B�9B�XBŢB��B�BB�B��B		7B	�B	#�B	7LB	\)B	s�B	l�B	hsB	jB	� B	�PB	��B	��B	�!B	�FB	�dB	��B	ȴB	��B	��B	��B	�B	�)B	�BB	�`B	�yB	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B

=B
DB
\B
hB
oB
{B
�B
�B
�B
�B
�B
 �B
"�B
#�B
$�B
%�B
'�B
)�B
,B
/B
0!B
1'B
2-B
49B
49B
5?B
7LB
6FB
:^B
;dB
=qB
>wB
A�B
B�B
B�B
E�B
E�B
E�B
G�B
H�B
I�B
J�B
K�B
K�B
L�B
L�B
M�B
N�B
O�B
Q�B
R�B
Q�B
S�B
S�B
T�B
T�B
W
B
XB
ZB
[#B
[#B
\)B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
aHB
cTB
cTB
cTB
cTB
dZB
e`B
ffB
ffB
hsB
jB
k�B
jB
l�B
k�B
l�B
m�B
l�B
n�B
n�B
o�B
o�B
r�B
q�B
s�B
u�B
u�B
v�B
v�B
v�B
w�B
x�B
y�B
y�B
{�B
{�B
{�B
}�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�7B
�DB
�JB
�PB
�\B
�\B
�hB
�oB
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
�B
�B
�B
�B
�'B
�'B
�3B
�3B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�XB
�XB
�XB
�^B
�^B
�^B
�XB
�^B
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�^B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  B��B��B��B��B��B��B��B��B��B��B��B��BϼB(�BL�BO�BR�BW�BY�B[B\
B]B^B_Ba,Ba,Ba-Bb4Bb4Bb5Bb5Bb6Bb7Bc=Bb8Bc?BeKBeLBdFBh`BjlBfTBn�BktBn�BjoBu�B�_B�FB�AB�AB�HB�B�zB��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�|B�pB�@B�B{�Bl�BgkB[!BVBI�B>wB7LB#�B
>B��B�B�JB�>B�&B��B��B��B��B�wB�!B��B��B�RB}�Bk�Bc]B^?BE�B:hB%�B �B�B�B
JB
�B
�B
�bB
�>B
�&B
ǾB
�IB
��B
��B
��B
�$B
p�B
I�B
-!B
�B
�B
�B
�B
:B	�B	�B	ƾB	�{B	�B	��B	��B	��B	��B	��B	��B	�'B	j�B	biB	Z9B	O�B	E�B	B�B	A�B	9vB	7kB	$�B	�B	|B	RB	:B�B	(B�B��B��B�B�fB�NB�#B�B��BýB��B�zB�OB�=B�B�B��B��B��B��B��B��B��B��B�~B�yB��B��B��B��B�bB�KB�QB�9B�RB.Bv�B{By
Bo�Bl�Bk�Bl�Bn�Bs�Bo�B\bBT2BQBPBR'BNBPBOBM
BJ�BJ�BF�BG�BF�B@�B?�B7�B9�B9�B8�B5�B4|B2qB0eB0fB1lB/aB4B;�B;�B:�B8�B6�B5�B2wB1rB0lB-ZB*HB,UB,UB,VB,VB*KB-]B-^B-_B,YB/mB/mB.gB/nB0uB2�B2�B3�B4�B7�B6�B9�B:�B<�B;�B;�BC�BO;BN8B[�Bh�Bk�Bu,B��B�B�0B��B��B�"B�hB��B�!B�aB		�B	B	$lB	7�B	\�B	tUB	m-B	iB	k'B	��B	��B	�^B	��B	��B	�B	�"B	�JB	�xB	̎B	ϣB	ѲB	��B	��B	�B	�9B	�UB	�XB	�lB	�B	�B	��B	��B	��B	��B	��B	��B	��B
	B
%B
@B
JB
eB
sB
}B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
&B
'B
)!B
+0B
-?B
0UB
1^B
2gB
3pB
5B
5�B
6�B
8�B
7�B
;�B
<�B
>�B
?�B
B�B
C�B
C�B
G
B
GB
GB
IB
J'B
K0B
L9B
MBB
MEB
NNB
NQB
OZB
PcB
QkB
S{B
T�B
S�B
U�B
U�B
V�B
V�B
X�B
Y�B
[�B
\�B
\�B
]�B
^�B
^�B
_�B
_�B
`�B
bB
cB
cB
eB
e!B
e$B
e'B
f0B
g9B
hBB
hDB
jTB
lcB
mlB
liB
nwB
mtB
n}B
o�B
n�B
p�B
p�B
q�B
q�B
t�B
s�B
u�B
w�B
w�B
x�B
x�B
x�B
y�B
z�B
{�B
{�B
~B
~B
~B
�!B
B
�'B
�5B
�8B
�@B
�CB
�LB
�NB
�SB
�nB
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
� B
�B
�B
�$B
�)B
�7B
�BB
�OB
�ZB
�fB
�sB
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
�B
�B
�B
�B
�*B
�8B
�UB
�iB
��B
��B
��B
��B
��B
��B
� B
�B
�+B
�GB
�\B
�kB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�%B
�5B
�JB
�SB
�iB
�xB
�uB
�xB
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
��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810081702192021061413554420210614135544202106171313102021061713131020210617131310201810081702192021061413554420210614135544202106171313102021061713131020210617131310PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018100817021920181008170219  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100817021920181008170219QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018100817021920181008170219QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150520210617131505IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                