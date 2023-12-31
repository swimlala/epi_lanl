CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:50Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �D   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   м   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   
D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   
L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   
T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   
\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 
d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                       HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                       HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        (   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    @   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 	�Argo profile    3.1 1.2 19500101000000  20180724220250  20210617131456  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�ka��@�ka��11  @�ka��`P@�ka��`P@6�j����@6�j�����c�	����c�	���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @L��@�ff@�ff@�  @�  A��A  A$��AC33Aa��A~ffA�33A�  A���A���A�  A�  A���B ffB  B33B  B   B(  B0  B7��B?��BG��BO��BW��B`ffBh  Bp��By33B�ffB�ffB�33B�33B�  B�  B���B���B�  B�  B�  B�33B�33B�33B�33B�  B�ffB�ffB�  B�  B�  B�  B���B�ffB�33B�  B�33B�  B�  B�33B�  B���C   C33C�C�3C�fC	�fC�C  C�3C�fC�CL�C�C  C�fC�fC �C"�C#�fC%�fC'��C*33C,33C.�C0  C1��C433C6�C8  C:L�C<33C>�C?�fCA��CD�CF  CG��CJ�CL�CN  CP  CQ�fCT  CU�fCX  CY�fC[�fC]�fC`  Ca�fCc�fCe�fCh33CjL�Cl�Cn33Cp�Cr  Ct  Cu�fCw�fCy�fC{�fC~�C�  C��3C��C��C�  C��C�&fC��C��C�&fC��C��C�  C��fC��3C�&fC��C��3C��C��C�  C��3C��C��C��3C�  C��C��C�  C��C��C��C�  C��3C��C��C�  C��C��C�  C�  C��3C��C��C��C�  C�  C��3C��fC��C��C�  C��C��C�  C�&fC��C��C��C��C��3C��C�&fC��C��C��C�  C��C��C�  C��C��C��C�  C��fC��3C�  C��C�  C��3C�  C��C��C��3C�  C��C��3C��fC��3C�  C��C�  C��fC��C�&fC��C��3C�  C��C��C��3C�  C��C�&fC��C��fC��3C�  C��C��C��C��fC��3C�  C��C�&fC��C��fC��3C��C��C�&fC�  D�D�fD@ D	ٚDs3D��Dy�DfD�fD3D� D9�D � D#s3D&3D(�fD+s3D.  D0�3D3�3D69�D8�3D;� D>s3DA33DC�3DF� DI` DL�DN�fDQ� DT  DV�3DY��D\&fD^��DaY�Dc� Df` Dh� Dkl�Dm�3DpL�Dr� Du9�Dw� Dz�D|3D~�fD�p D��fD��3D�#3D�\�D���D��fD��D�VfD��fD���D�fD�S3D���D�ɚD��D�FfD�vfD��fD�ٚD��D�@ D�vfD���D���D��D�I�D�|�D��fD���D�&fD�\�D���D��3D�fD�<�D�� D��fD� D�VfD���D��fD��D�\�D���D��fD�C3D���D��3D��D�Y�D�� D��fD�0 D�y�DĹ�D��fD�6fD�s3DɶfD���D�9�D�|�Dμ�D���D�6fD�s3Dө�D��fD�3D�FfD�s3D٠ D��3D��3D�	�D�33D�S3D�p DቚD⩚D�� D��3D��fD�3D�&fD�9�D�L�D�\�D�l�D� D�fD署D�D���D���D��3D�	�D��D�9�D�L�D�ffD�c3D�� D��fD���D�ɚD��E ��E�E� E  E�3E4�E�3EFfE� EX E�fEh E�Ey�E��E
&fE4�E�fE� E�3ES3EVfE� E�fEFfE��E�3E( E&fE�fE�3E E3E q�E!�3E"��E$T�E%K3E&��E(6fE)0 E*� E+�fE- E.s3E/c3E0� E1�3E3�E4t�E5�fE7)�E8�E9X E:�fE;�fE?)�EB&fEEfEH� EKx EN� EQ��ET�3EW��E[L�E^K3EaA�Ed��Eg�3Ej�3Em�fEp� Es��EwD�Ez6fE}��E�K3E��fE�ZfE��E���E�3E��fE�D E���E� �E�]�E���E��fE�H E�� E�� E�K3E���E��E�0�E�t E��E�8�E�� E�՚>���>L��>���>���>���>L��>L��>L��>L��>L��>���>���>���>���>���>���>���>���>���?   ?��?fff?fff?���?���?�33?���?ٙ�@   @33@   @333@Fff@S33@`  @y��@�ff@�  @���@�ff@�  @���@�ff@�33@�  @���@���A��A33A33A��A   A)��A0  A8  A@  AFffANffAVffA^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444444144414144111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?L��?�  @   @l��@�ff@�ff@�  @�  A	��A  A,��AK33Ai��A�33A�33A�  A���A���A�  A�  A���BffB
  B33B  B"  B*  B2  B9��BA��BI��BQ��BY��BbffBj  Br��B{33B�ffB�ffB�33B�33B�  B�  B���B���B�  B�  B�  B�33B�33B�33B�33B�  B�ffB�ffB�  B�  B�  B�  B���B�ffB�33B�  B�33B�  B�  B�33B�  B���C � C�3C��C33CffC
ffC��C� C33CffC��C��C��C� CffCffC ��C"��C$ffC&ffC(L�C*�3C,�3C.��C0� C2L�C4�3C6��C8� C:��C<�3C>��C@ffCBL�CD��CF� CHL�CJ��CL��CN� CP� CRffCT� CVffCX� CZffC\ffC^ffC`� CbffCdffCfffCh�3Cj��Cl��Cn�3Cp��Cr� Ct� CvffCxffCzffC|ffC~��C�@ C�33C�Y�C�L�C�@ C�L�C�ffC�Y�C�L�C�ffC�Y�C�L�C�@ C�&fC�33C�ffC�L�C�33C�L�C�Y�C�@ C�33C�L�C�L�C�33C�@ C�Y�C�Y�C�@ C�Y�C�Y�C�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�Y�C�@ C�@ C�33C�&fC�Y�C�L�C�@ C�Y�C�Y�C�@ C�ffC�Y�C�L�C�Y�C�L�C�33C�L�C�ffC�Y�C�L�C�L�C�@ C�Y�C�L�C�@ C�Y�C�Y�C�L�C�@ C�&fC�33C�@ C�Y�C�@ C�33C�@ C�Y�C�Y�C�33C�@ C�Y�C�33C�&fC�33C�@ C�Y�C�@ C�&fC�L�C�ffC�L�C�33C�@ C�Y�C�L�C�33C�@ C�L�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�@ D9�D�fD` D	��D�3D�D��D&fD�fD33D� DY�D!  D#�3D&33D(�fD+�3D.@ D0�3D3�3D6Y�D93D;� D>�3DAS3DD3DF� DI� DL,�DN�fDQ� DT@ DV�3DY��D\FfD^��Day�Dd  Df� Di  Dk��Dm�3Dpl�Dr� DuY�Dw� Dz,�D|33D~�fD�� D��fD��3D�33D�l�D���D��fD�)�D�ffD��fD���D�&fD�c3D���D�ٚD��D�VfD��fD��fD��D��D�P D��fD���D���D�)�D�Y�D���D��fD���D�6fD�l�D���D��3D�fD�L�D�� D��fD�  D�ffD���D��fD�)�D�l�D���D�fD�S3D���D��3D�)�D�i�D�� D��fD�@ DÉ�D�ɚD�fD�FfDȃ3D��fD��D�I�D͌�D���D��D�FfD҃3Dӹ�D��fD�#3D�VfD؃3Dٰ D��3D��3D��D�C3D�c3D�� DᙚD⹚D�� D��3D�fD�#3D�6fD�I�D�\�D�l�D�|�D� D�fD﹚D�ɚD���D���D�3D��D�,�D�I�D�\�D�vfD�s3D�� D��fD���D�ٚD���E ��E�E� E( E�3E<�E�3ENfE� E` E�fEp E��E��E��E
.fE<�E�fE� E�3E[3E^fE� E�fENfE��E�3E0 E.fE�fE�3E E3E y�E!�3E"��E$\�E%S3E&��E(>fE)8 E*� E+�fE- E.{3E/k3E0� E1�3E3$�E4|�E5�fE71�E8�E9` E:�fE;�fE?1�EB.fEE&fEH� EK� EN� EQ��ET�3EW��E[T�E^S3EaI�Ed��Eg�3Ej�3Em�fEp� Es��EwL�Ez>fE}��E�O3E��fE�^fE��E���E�3E��fE�H E���E�$�E�a�E���E��fE�L E�� E�� E�O3E���E��E�4�E�x E��E�<�E�� E�ٚG�O�?333G�O�G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�?fff?�  ?���G�O�?�33?���?ٙ�?�33@ff@��@   @333@@  @S33@fff@s33@�  @���@�ff@�  @���@�ff@�  @ə�@�ff@�33@�  @���A��A��A33A33A!��A(  A1��A8  A@  AH  ANffAVffA^ffAfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444444144414144111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ @ �@ �@ �@ �@ #�@ (�@ /�@ 7L@ =q@ FQ@ SI@ `B@ l�@ z3@ ��@ ��@ ��@ �~@ �&@ �|@ �#@ �@ �e@j@@�@,`@9X@F�@T�@bN@o�@~�@��@��@�M@�F@��@��@ލ@�@�,@%@�@""@/�@=q@K�@Yn@g@t�@��@��@�a@��@�R@��@Ӡ@��@�L@�E@
=@�@%�@33@A�@N�@[z@i�@x�@��@��@��@�f@�k@�c@��@�@�@@V@O@(G@5�@D�@R�@^�@l�@y�@��@��@��@�~@��@�*@�#@�@��@�@�@
@+@:�@G�@S�@c�@qS@~K@��@��@�A@�9@@ψ@�/@��@�,@%@�@!s@1'@?}@K�@Z@g@t@��@��@�U@��@��@ƨ@Ӡ@��@�L@�E@
=@�@'�@4�@A�@P�@]�@j@ww@��@�@�y@��@�@�@�h@�@�@ �@V@�@(�@7�@E�@Q�@`�@n�@{�@��@��@�5@�-@�&@�*@�#@�@��@	�@	o@	 @	-�@	:@	G�@	T�@	a�@	r@	~�@	��@	��@	��@	��@	Ĝ@	є@	ލ@	��@	��@
%@
*@
$.@
1'@
>@
K�@
X�@
g�@
t�@
��@
��@
�a@
�Y@
�R@
Ĝ@
��@
�H@
�L@
��@	�@�@&�@4�@@,@N�@]�@i!@v@�p@��@��@�@�^@�@�@�`@�@  @@�@(G@6�@D�@S�@`B@k�@z3@��@��@�5@�-@��@�@�t@��@��@@�@
@-@;d@I�@UU@��@&;@m:@�9@�9@@,@�p@�@V@S�@��@��@(�@oF@��@ �@I�@��@܀@'�@o�@�^@�@P�@��@�@/�@z�@��@V@X�@�m@�(@4�@{�@��@	�@N�@��@�
@�@^5@��@�@(G@i�@�@�T@&;@ff@��@�4@0x@s_@��@��@?}@�@�W@�@O�@�u@�\@�@^�@��@�@ #�@ e	@ ��@ �@!*S@!k�@!�f@!��@"1�@"s_@"�F@"�~@#;d@#}�@#�2@$@$E�@$��@$��@%o@%X�@%�a@%�@&&�@&k�@&��@&��@'>�@'��@'�@(�@(Wb@(��@(�H@)&�@)m:@)��@)��@*;d@*�@*�>@+1@+M�@+�i@+�\@,�@,^�@,��@,�`@-'�@-k.@-�Y@-��@.-@.m:@.�Y@.��@/'�@/g@/��@/�H@0[@0Z�@0�0@0�O@1@1K�@1�|@1�2@1��@25�@2o�@2��@2�@3 �@3Z�@3��@3ψ@4
�@4FQ@4�@4��@4�~@54�@5j@5�A@5�@6
@6Z�@6�<@6�O@7�@7K@7�@7��@7�9@84�@8o�@8��@8�@9	@9X�@9�u@9�|@:B�@:�@;e	@<b@<��@<�L@=�#@>�@>��@?b@?��@@M$@@�F@AX�@A�J@Be�@Bє@Cp�@C�h@Dt�@Eo@E|�@F[@F�|@G$/@G�J@H/�@H�7@I9X@IӠ@Jn�@J��@Kp�@K��@Lk�@L��@M�@N&;@N�+@O{@O��@P$�@Q�@R�[@T�@U�\@V�7@X6�@Yk�@Z�W@\[@]�i@^�h@`�@a�7@b�W@d&�@e�p@f�&@h�@i�W@j��@l3�@mz2@n��@p�@q��@r�|@t�@ur�@v�
@x�@xm�@x��@x�@y$.@yi�@y�~@y��@zF�@z{�@zє@{
�@{DD@{��@{�4@|33@|rG�O�@ ^G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�@ G�O�G�O�@ �@ j@ G�O�@ %@ �@ 1@ 	�@ 
�@ �@ �@ �@ @ @ *@ �@ �@ �@ �@ �@ !s@ #�@ %�@ '�@ *S@ -@ /�@ 2�@ 5?@ 8�@ ;d@ >�@ A�@ DD@ H]@ K@ N�@ Q�@ T�@ X@ [z@ ^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�A΁AΏ\AΓuAΓuAΓuAΑhAΏ\A΋DA΁A�/A͝�A͑hA͏\A͍PA�dZA�oA���Ạ�A�~�A�`BA�O�A�/A��A���A�l�Aɥ�A�A�hsA�t�AÃA��A�ȴA�1A�hsA���A�{A�XA���A��DA�jA�$�A��A��jA� �A�p�A�O�A�?}A��A�ƨA��+A�A�v�A�C�A�VA��\A��HA���A��A�|�A�t�A�r�A�jA�ffA�dZA�XA���A��A�A�A��#A�bA��;A�A�$�A�bA�r�A�+A�ffA��wA�ZA���A�hsA��;A�XA�1A��A�M�A�ZA�M�A���A��7A��A�  A��+A�jA�O�A�&�A��A���A�VA��A�?}A���A��A���A�dZA��A��A��A�E�A�O�A�=qA��#A��mA�\)A��uA�jA�ĜA���A���A��hA�=qA���A�JA��`A}��A| �A{O�Az�Av^5At��Aq��An��Al1Ai�;Ahn�AgVAeVAb(�A`��A_�A^�A\1'AZ^5AYl�AWG�AT��AP��AO�FAOt�AN�yAMO�AKƨAKl�AKhsAI��AH�RAH1AG��AF��AEx�ADffAB�RAA�A?�
A?hsA?VA>��A>  A=?}A;
=A9p�A7�mA6ȴA5ƨA4{A3�FA2��A1��A1x�A/�A/�A-�mA-oA,�\A+�A)�TA)S�A(�+A'��A'�7A&�9A$bNA"I�A!O�A �9A  �A��A�!A�7AĜAbA�A$�A�-AĜAƨA��A��A{A"�A9XA�PA�A|�A�+A�#A�wAVAXA
1'A�jA�A��AoA�DA5?A�mA��A&�A�A�AdZA ��@���@��@�Ĝ@���@�E�@�V@���@���@���@��m@���@�Q�@�1@�P@�33@��y@�ȴ@��@�~�@���@�&�@�|�@���@�v�@�Z@ͺ^@��@�b@Ý�@�dZ@��H@�Ĝ@�@�V@��/@���@�;d@�K�@�
=@�V@�^5@�&�@���@�(�@��!@�hs@��#@�hs@�
=@�"�@�p�@�C�@��@�hs@�Ĝ@���@���@���@�"�@�=q@�ff@��j@� �@��@�J@��T@��@�Ĝ@�  @~5?@}�-@z��@zM�@x�9@v{@u`B@r~�@q�7@o�@nV@lI�@k��@j��@ix�@iX@hr�@g
=@e�h@cƨ@b~�@a�@_��@^��@]�@[��@Xr�@V@TZ@Sƨ@RJ@Q�@Ol�@M`B@L�j@L�@K33@J=q@H �@FV@EV@CC�@A�@?�@>5?@=p�@;�m@;C�@:�@9&�@8 �@7|�@6E�@5O�@4�@3��@2�H@1�^@0bN@/K�@.��@-p�@,�D@*�@*n�@)�@'�P@'K�@'+@&�+@%�@$z�@$�@#@"~�@!��@ �9@K�@�@��@�-@Z@�F@"�@�@G�@bN@�P@��@5?@/@(�@1@�!@��@7L@��@1'@|�@ff@�T@/@9X@dZ@
M�@
�@	��@	7L@r�@l�@�R@��@@�h@O�@�@I�@1@��@"�@M�@��@��@ �`?��w?��-?��?�b?�?��
?��`??���?�I�?��#?�P?��T?�j?�\?��`?�;d?��?�1?�^5?���?���?��y?�ff?�?�?}?��?�o?�n�?щ7?�&�?У�?θR?�V?�O�?�(�?�C�?ʟ�?��?ə�?�r�?���?�?ļj?Õ�?���?�G�?� �?�;d?��?�V?���?��?��D?��m?��m?�?���?�~�?�^5?���?���?�?�"�?��?�ƨ?�(�?��?�O�?��-?�V?���?��?�\)?�\)?�|�?�|�?��;?��;?�  ?�A�?�bN?��?��?�Ĝ?�%?�G�?�G�A΍PA΋DAΉ7AΉ7A�|�A�v�A�|�A�~�A�~�A�x�A΅A�x�A�v�A�x�A�|�A�v�A�t�A�x�A�v�A�v�A�x�A΃A΁A·+A�~�A�~�A΅AΉ7AΑhAΕ�AΕ�AΓuAΑhAΓuAΓuAΓuAΓuAΓuAΓuAΓuAΑhAΑhAΏ\AΏ\AΏ\AΏ\A΍PAΉ7A΁A΁A�r�A�Q�A��A��
AͰ!A͝�A͗�A͑hA͑hA͑hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�|�A΁AΏ\AΓuAΓuAΓuAΑhAΏ\A΋DA΁A�/A͝�A͑hA͏\A͍PA�dZA�oA���Ạ�A�~�A�`BA�O�A�/A��A���A�l�Aɥ�A�A�hsA�t�AÃA��A�ȴA�1A�hsA���A�{A�XA���A��DA�jA�$�A��A��jA� �A�p�A�O�A�?}A��A�ƨA��+A�A�v�A�C�A�VA��\A��HA���A��A�|�A�t�A�r�A�jA�ffA�dZA�XA���A��A�A�A��#A�bA��;A�A�$�A�bA�r�A�+A�ffA��wA�ZA���A�hsA��;A�XA�1A��A�M�A�ZA�M�A���A��7A��A�  A��+A�jA�O�A�&�A��A���A�VA��A�?}A���A��A���A�dZA��A��A��A�E�A�O�A�=qA��#A��mA�\)A��uA�jA�ĜA���A���A��hA�=qA���A�JA��`A}��A| �A{O�Az�Av^5At��Aq��An��Al1Ai�;Ahn�AgVAeVAb(�A`��A_�A^�A\1'AZ^5AYl�AWG�AT��AP��AO�FAOt�AN�yAMO�AKƨAKl�AKhsAI��AH�RAH1AG��AF��AEx�ADffAB�RAA�A?�
A?hsA?VA>��A>  A=?}A;
=A9p�A7�mA6ȴA5ƨA4{A3�FA2��A1��A1x�A/�A/�A-�mA-oA,�\A+�A)�TA)S�A(�+A'��A'�7A&�9A$bNA"I�A!O�A �9A  �A��A�!A�7AĜAbA�A$�A�-AĜAƨA��A��A{A"�A9XA�PA�A|�A�+A�#A�wAVAXA
1'A�jA�A��AoA�DA5?A�mA��A&�A�A�AdZA ��@���@��@�Ĝ@���@�E�@�V@���@���@���@��m@���@�Q�@�1@�P@�33@��y@�ȴ@��@�~�@���@�&�@�|�@���@�v�@�Z@ͺ^@��@�b@Ý�@�dZ@��H@�Ĝ@�@�V@��/@���@�;d@�K�@�
=@�V@�^5@�&�@���@�(�@��!@�hs@��#@�hs@�
=@�"�@�p�@�C�@��@�hs@�Ĝ@���@���@���@�"�@�=q@�ff@��j@� �@��@�J@��T@��@�Ĝ@�  @~5?@}�-@z��@zM�@x�9@v{@u`B@r~�@q�7@o�@nV@lI�@k��@j��@ix�@iX@hr�@g
=@e�h@cƨ@b~�@a�@_��@^��@]�@[��@Xr�@V@TZ@Sƨ@RJ@Q�@Ol�@M`B@L�j@L�@K33@J=q@H �@FV@EV@CC�@A�@?�@>5?@=p�@;�m@;C�@:�@9&�@8 �@7|�@6E�@5O�@4�@3��@2�H@1�^@0bN@/K�@.��@-p�@,�D@*�@*n�@)�@'�P@'K�@'+@&�+@%�@$z�@$�@#@"~�@!��@ �9@K�@�@��@�-@Z@�F@"�@�@G�@bN@�P@��@5?@/@(�@1@�!@��@7L@��@1'@|�@ff@�T@/@9X@dZ@
M�@
�@	��@	7L@r�@l�@�R@��@@�h@O�@�@I�@1@��@"�@M�@��@��@ �`?��w?��-?��?�b?�?��
?��`??���?�I�?��#?�P?��T?�j?�\?��`?�;d?��?�1?�^5?���?���?��y?�ff?�?�?}?��?�o?�n�?щ7?�&�?У�?θR?�V?�O�?�(�?�C�?ʟ�?��?ə�?�r�?���?�?ļj?Õ�?���?�G�?� �?�;d?��?�V?���?��?��D?��m?��m?�?���?�~�?�^5?���?���?�?�"�?��?�ƨ?�(�?��?�O�?��-?�V?���?��?�\)?�\)?�|�?�|�?��;?��;?�  ?�A�?�bN?��?��?�Ĝ?�%?�G�?�G�A΍PA΋DAΉ7AΉ7A�|�A�v�A�|�A�~�A�~�A�x�A΅A�x�A�v�A�x�A�|�A�v�A�t�A�x�A�v�A�v�A�x�A΃A΁A·+A�~�A�~�A΅AΉ7AΑhAΕ�AΕ�AΓuAΑhAΓuAΓuAΓuAΓuAΓuAΓuAΓuAΑhAΑhAΏ\AΏ\AΏ\AΏ\A΍PAΉ7A΁A΁A�r�A�Q�A��A��
AͰ!A͝�A͗�A͑hA͑hA͑hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�B
�uB
�qB
��B%�B5?BK�BZBk�Bs�B�7B�{B�9B�}B��B�;B��BBB%BPBoB%�B.B0!B33B8RB<jBC�BM�B_;Bp�B�PB�{B��B��B��B�B�B�B�B�B�B�3B�-B�3B�3B�3B�9B�B��B��B�1Bp�B}�Bn�BbNBXBK�BC�B@�B>wB@�BG�BJ�B:^B%�B�B�B�B�BuBuBoBbBB�B�5B��B�By�BK�B.B"�B�B�BhB  B
�B
��B
��B
�^B
�?B
�B
��B
�oB
�JB
�B
x�B
aHB
K�B
33B
(�B
�B
JB	�B	�TB	��B	�LB	��B	��B	�JB	� B	n�B	bNB	XB	R�B	H�B	6FB	-B	!�B	�B	B�B�B�yB�`B�;B�HB�fB�B�ZB�mB�mB�mB�fB�ZB�;B�#B��B��B��B��B��B��B��BB�qB�9B�B�B��B��B��B��B��B�VB�\B�=B�1B�+B�B�B�B�B�B~�B}�Bs�Bs�Bo�Bl�BjBgmBdZB_;B_;B_;B]/B\)BZBVBYBW
BW
BW
BS�BS�BN�BM�BM�BM�BI�BF�BD�B@�B>wB>wB<jB:^B8RB8RB6FB5?B5?B49B33B33B2-B1'B1'B1'B0!B.B0!B.B/B/B.B-B2-B49B5?B6FB7LB6FB7LB8RB:^B:^B<jB<jB<jBC�BO�BS�B\)BdZBjBs�B�B�1B�PB�oB��B��B�?BBŢB��B�B�mB��B		7B	+B	bB	8RB	,B	49B	B�B	G�B	O�B	R�B	dZB	iyB	s�B	}�B	�1B	��B	��B	��B	�B	�RB	ɺB	��B	��B	��B	�B	�;B	�NB	�ZB	�mB	�B	�B	��B	��B	��B
B
B
+B
1B
	7B
DB
PB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
$�B
'�B
(�B
(�B
,B
+B
-B
0!B
0!B
1'B
1'B
2-B
49B
6FB
7LB
8RB
;dB
;dB
=qB
>wB
A�B
A�B
B�B
C�B
D�B
F�B
G�B
H�B
H�B
I�B
J�B
L�B
M�B
N�B
O�B
P�B
P�B
Q�B
S�B
S�B
S�B
T�B
VB
W
B
XB
YB
YB
ZB
ZB
ZB
\)B
^5B
^5B
^5B
_;B
aHB
bNB
bNB
cTB
e`B
ffB
ffB
gmB
gmB
hsB
jB
iyB
k�B
l�B
l�B
l�B
m�B
n�B
o�B
o�B
p�B
r�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
v�B
w�B
x�B
w�B
x�B
y�B
y�B
y�B
z�B
|�B
{�B
|�B
~�B
� B
� B
�B
�B
�B
�%B
�1B
�1B
�=B
�DB
�JB
�VB
�bB
�bB
�hB
�oB
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
�B
�!B
�!B
�!B
�-B
�-B
�3B
�9B
�?B
�?B
�?B
�FB
�FB
�FB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�RB
�^B
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�XB
�RB
�^B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
}�B
�B
}�B
�B
�B
�B
�B
�%B
�%B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�YB
�UB
��B%�B5$BK�BZBklBs�B�B�cB�"B�fB�sB�%B��B �B�BB=B\B%�B.B0B3"B8BB<[BC�BM�B_-Bp�B�DB�oB��B��B��B��B�B�B�B�B�B�-B�(B�.B�/B�/B�6B�B��B�B�0Bp�B}�Bn�BbNBXBK�BC�B@�B>zB@�BG�BJ�B:cB%�B�B�B�B�B}B~BxBlB*B�B�@B�
B�By�BK�B.!B"�B�B�BwB B
�B
��B
��B
�oB
�PB
�B
��B
��B
�]B
�3B
x�B
a]B
K�B
3HB
)B
�B
aB	�B	�kB	��B	�dB	��B	��B	�cB	�B	n�B	bhB	X+B	SB	H�B	6bB	-+B	!�B	�B	0B��B�B�B�B�[B�iB�B�B�|B�B�B�B�B�B�`B�IB�B�B�B�B�B�B��B¹B��B�cB�FB�9B�B�B��B��B��B��B��B�lB�aB�[B�=B�7B�>B�8B�9B-B~(Bs�Bs�Bo�Bl�Bj�Bg�Bd�B_sB_sB_tB]hB\cBZWBV?BYRBWFBWFBWGBT5BT6BOBNBNBNBI�BF�BD�B@�B>�B>�B<�B:�B8�B8�B6�B5�B5�B4B3zB3{B2uB1pB1pB1qB0kB._B0lB.`B/gB/hB.aB-\B2{B4�B5�B6�B7�B6�B7�B8�B:�B:�B<�B<�B<�BC�BP>BTZB\�Bd�Bj�Bt#B�|B��B��B��B�	B�PB��B�B�*B�LB٥B��B�oB		�B	�B	 B	8�B	,�B	4�B	C:B	H\B	P�B	S�B	eB	j4B	ttB	~�B	��B	�aB	��B	��B	��B	�%B	ʐB	ϲB	��B	��B	��B	� B	�5B	�DB	�ZB	�uB	�B	��B	��B	��B
B
B
0B
	9B

BB
RB
aB
|B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
&B
)&B
*/B
*2B
-GB
,DB
.SB
1iB
1kB
2tB
2wB
3�B
5�B
7�B
8�B
9�B
<�B
<�B
>�B
?�B
B�B
B�B
DB
EB
FB
H&B
I/B
J8B
J;B
KDB
LNB
N]B
OfB
PoB
QxB
R�B
R�B
S�B
U�B
U�B
U�B
V�B
W�B
X�B
Y�B
Z�B
Z�B
[�B
[�B
[�B
]�B
` B
`B
`B
aB
cB
d&B
d)B
e1B
g@B
hIB
hKB
iUB
iWB
j`B
loB
kkB
mzB
n�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
t�B
t�B
u�B
u�B
u�B
v�B
w�B
x�B
y�B
x�B
y�B
z�B
y�B
{B
|B
|B
|B
}B
)B
~%B
.B
�=B
�HB
�PB
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
�B
�B
�$B
�1B
�<B
�IB
�VB
�bB
�iB
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
��B
�B
�B
�B
�%B
�2B
�HB
�\B
�qB
��B
��B
��B
��B
��B
��B
�B
�B
�+B
�BB
�VB
�fB
�uB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�0B
�KB
�UB
�dB
�hB
�dB
�nB
�pB
�sB
�pB
�zB
�}B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
}�B
��B
}�B
��B
��B
��B
��B
�B
�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202502021061413552420210614135524202106171312092021061713120920210617131209201807242202502021061413552420210614135524202106171312092021061713120920210617131209PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025020180724220250  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025020180724220250QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025020180724220250QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145620210617131456IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                