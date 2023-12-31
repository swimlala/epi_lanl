CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-01-31T13:00:31Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   |   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   (   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       `   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20190131130031  20210617131514  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               :   :DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؟QN�@؟QN�11  @؟Qww�@؟Qww�@6=s�g�@6=s�g��c�(�z�c�(�z11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?���@ff@@  @�  @�ff@�ff@�ffA��AffA$��AC33Aa��A�  A�33A�  A���A���A�ffAᙚA���B   B  B��B��B��B(  B0ffB8  B@ffBI33BP��BXffB`  BhffBp��BxffB��B�  B�33B�  B�33B�33B�  B�ffB�33B�  B�33B�33B�ffB�  B�  B�33B�  B�  BǙ�B���B�  Bә�B�ffB�33B�33B�ffB�33B�33B�  B���B�  B���B���C33C33C�C  C
  C33C33C  C33C  C��C  C�CL�C33C 33C"L�C$�C%��C'�fC*  C,�C.33C0L�C2L�C4�C6�C8ffC:L�C<�C=��C?�fCB  CD�CFL�CH�CJ  CL�CN  CO��CR  CT  CU�fCX33CZ�C[�fC]�fC_�fCb33Cd�Cf  Ch33Cj�Cl  CnL�CpL�Cr�Ct  Cu��Cx  Cz33C|�C}�fC��C��fC��C�&fC��C��fC��C�&fC�&fC��C�  C��fC��C�&fC��C��3C��C�&fC��C��fC��3C�  C��C��C�  C�ٚC��fC��fC��3C�  C�  C�  C�  C�  C�  C��C�  C��C��C��C��C��C��C�  C�  C�&fC�&fC��C��fC��3C��3C�  C��C��C��C��C��C��C�&fC�&fC�&fC��C�ٚC��fC��fC�ٚC��C��C��fC��3C��3C�  C��C��C��C�  C��fC��3C�  C�  C��C��C��C�&fC�  C��fC��3C��C��C�  C��fC��3C�  C��C�  C��fC�  C��C��C��fC�  C�  C��C�33C��C�  C�&fC��C��3C��C�  C��fC��3C�  C�  C�&fC�  C��fC��3C��C��C��fC��3C��C�&fC�@ C��3C��D fD ��D�D�3DffD
�fD@ D�3D�fD  D` D�3D�Ds3D� D"@ D$��D'9�D)�3D,9�D.�fD1l�D4fD6�3D9,�D;��D>FfD@�3DC` DE�fDHs3DJ��DMs3DO��DR  DT�3DV� DY,�D[y�D]��D`  DbL�Dd��Df�fDh��Dk3DmY�Do��Dq� Dt33Dv��Dx� D{FfD}@ D�fD���D�0 D�` D���D�� D���D�0 D�` D���D��fD��3D� D�6fD�Y�D�vfD��fD���D�� D���D��D�)�D�C3D�Y�D�p D���D�� D���D���D�� D�� D�  D�fD��D�0 D�9�D�C3D�L�D�S3D�Y�D�c3D�i�D�s3D�� D���D���D���D��fD��fD�ɚD���D��D��3D���D�3D��D�fD�3D��D�fD�fD��D��D�#3D�#3D�33D�<�D�FfD�P D�\�D�i�D�vfD̓3DΙ�Dϩ�Dй�D��fD�ٚD��3D���D�3D��D�&fD�33D�9�D�FfD�Y�D�l�Dހ DߖfD� D��3D���D���D�  D�3D�,�D�9�D�I�D�Y�D�i�D�|�D�fD��D�ɚD�� D��3D�	�D��D�33D�C3D�Y�D�� D���D��fD�� D�� D���D��3D�� E h E �fEp E� E�E` EX E� E	!�E
t�EL�E�3E��E` E�3E�3E�fE.fE^fE�3EњEfEi�E��E E|�Ei�E ��E"$�E#	�E$ffE%��E'0 E(  E)��E*�fE+�fE-@ E.��E/�E0� E2fE3\�E4�3E5ɚE7\�E8��E9�fE:�E<3E?p EB^fEE��EH� EK�3EO�EQ�EUH EX$�E[y�E^�3Ea�3Ed� Eg� Ek!�En.fEn�3EoK3Eo��Ep�fEq<�Eq�fEr�3Es3Es��EtVfEt� Eu�3EvNfEv�fEw� Ex0 Ex� EyS3Ey� Ez��E{D�E{��E|a�E} E}� E~L�E~��E�fE� E�m�E��3E� E�P E��fE� �E�;3E��3E�� E�73>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���?   ?   ?   ?   ?��?L��?fff?�  ?���?�33?�  ?ٙ�?�33@ff@33@   @,��@Fff@S33@fff@y��@�33@�  @���@�ff@�  @���@�ff@�  @�  @���@���A��A33A��A��A!��A&ffA0  A8  A>ffAH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444114441444441444111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?���@&ff@`  @�  @�ff@�ff@�ffA	��AffA,��AK33Ai��A�  A�33A�  A���A���A�ffA噚A���B  B
  B��B��B!��B*  B2ffB:  BBffBK33BR��BZffBb  BjffBr��BzffB���B�  B�33B�  B�33B�33B�  B�ffB�33B�  B�33B�33B�ffB�  B�  B�33B�  B�  Bș�B���B�  Bԙ�B�ffB�33B�33B�ffB�33B�33B�  B���B�  B���C L�C�3C�3C��C� C
� C�3C�3C� C�3C� CL�C� C��C��C�3C �3C"��C$��C&L�C(ffC*� C,��C.�3C0��C2��C4��C6��C8�fC:��C<��C>L�C@ffCB� CD��CF��CH��CJ� CL��CN� CPL�CR� CT� CVffCX�3CZ��C\ffC^ffC`ffCb�3Cd��Cf� Ch�3Cj��Cl� Cn��Cp��Cr��Ct� CvL�Cx� Cz�3C|��C~ffC�&fC�&fC�L�C�ffC�L�C�&fC�L�C�ffC�ffC�L�C�@ C�&fC�L�C�ffC�L�C�33C�L�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�@ C��C�&fC�&fC�33C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�@ C�@ C�ffC�ffC�L�C�&fC�33C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�ffC�ffC�ffC�L�C��C�&fC�&fC��C�L�C�L�C�&fC�33C�33C�@ C�L�C�Y�C�Y�C�@ C�&fC�33C�@ C�@ C�L�C�L�C�L�C�ffC�@ C�&fC�33C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�@ C�&fC�@ C�Y�C�L�C�&fC�@ C�@ C�Y�C�s3C�Y�C�@ C�ffC�L�C�33C�Y�C�@ C�&fC�33C�@ C�@ C�ffC�@ C�&fC�33C�L�C�L�C�&fC�33C�Y�C�ffC�� C�33C�L�D &fD ��D,�D3D�fDfD` D�3DfD@ D� D�3D,�D�3D   D"` D$ٚD'Y�D)�3D,Y�D/fD1��D4&fD6�3D9L�D;ٚD>ffD@�3DC� DFfDH�3DK�DM�3DO��DR@ DT�3DW  DYL�D[��D]ٚD`  Dbl�Dd��Df�fDi�Dk33Dmy�Do��Dr  DtS3Dv��Dy  D{ffD}` D�fD��D�@ D�p D���D�� D��D�@ D�p D���D��fD��3D�  D�FfD�i�D��fD��fD�ɚD�� D���D��D�9�D�S3D�i�D�� D���D�� D���D���D�� D�  D� D�&fD�,�D�@ D�I�D�S3D�\�D�c3D�i�D�s3D�y�D��3D�� D���D���D���D��fD��fD�ٚD���D���D�3D�	�D�3D��D�&fD�#3D�)�D�&fD�&fD�)�D�)�D�33D�33D�C3D�L�D�VfD�` D�l�D�y�D̆fD͓3DΩ�DϹ�D�ɚD��fD��D��3D���D�3D�)�D�6fD�C3D�I�D�VfD�i�D�|�Dސ DߦfD�� D��3D���D���D� D�#3D�<�D�I�D�Y�D�i�D�y�D��D��fD��D�ٚD�� D�3D��D�,�D�C3D�S3D�i�D�� D���D��fD�� D�� D���D��3D�� E p E �fEx E  E��Eh E` E� E	)�E
|�ET�E�3E��Eh E�3E�3EfE6fEffE�3EٚE&fEq�E��E  E��Eq�E ��E",�E#�E$nfE%��E'8 E(( E)��E*�fE+�fE-H E.��E/��E0� E2&fE3d�E4�3E5њE7d�E8��E9�fE:�E<3E?x EBffEE��EH� EK�3EO$�EQ�EUP EX,�E[��E^�3Ea�3Ed� Eg� Ek)�En6fEn�3EoS3Eo��Ep�fEqD�Eq�fEr�3Es3Es��Et^fEt� Eu�3EvVfEv�fEw� Ex8 Ex� Ey[3Ey� Ez��E{L�E{��E|i�E} E}� E~T�E�E�fE� E�q�E��3E� E�T E��fE��E�?3E��3E�� E�;3G�O�G�O�G�O�G�O�G�O�G�O�?333?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?�  ?���?�ff?�33?�  ?ٙ�?�33@   @��@��@&ff@333@@  @L��@fff@s33@�33@���@�33@�  @���@�ff@�  @ə�@�ff@�  @�  @���A��A��A33A��A!��A)��A.ffA8  A@  AFffAP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444114441444441444111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ �@ V@ {@ O@ #�@ *S@ 1'@ 7L@ <�@ FQ@ SI@ `B@ m:@ z3@ ��@ ��@ ��@ ��@ ��@ �|@ �t@ �@ �@�@b@�@-@:@H]@Wb@dZ@qS@~K@��@��@��@�9@@��@��@�4@��@�@�@"�@/�@>@K�@Z@ff@t@�d@�\@�@�M@��@��@�C@�@�@�E@�@�@&;@33@@,@N�@[z@hs@x�@�|@�u@�m@�@�@��@�
@�@�Y@��@�@�@+@7�@E�@S�@`B@k�@z3@��@��@�5@��@�2@�|@�#@��@��@@�@
@,`@:�@I�@V@b�@qS@~K@��@��@�A@�9@��@��@�/@��@�~@1@*@""@1'@>@K@Z�@hs@t�@��@��@�@�@�@�J@�C@��@�@��@
�@�@&;@5?@B�@O0@\)@hs@x&@�+@�u@��@��@��@�@խ@�@�Y@ �@@O@&�@5?@B�@Q=@_�@m:@z�@��@�0@��@�-@�&@�|@�#@��@�q@	�@	o@	�@	,`@	<@	I�@	V@	a�@	o�@	}�@	��@	�H@	��@	��@	�>@	є@	�;@	�@	�9@
�@
*@
 @
.l@
<@
I@
Yn@
g@
r�@
�@
��@
�@
�Y@
��@
�W@
Ӡ@
��@
�@@
��@
=@�@&;@3�@B�@N�@Z�@i!@x&@�|@��@�@�f@��@�@�
@�T@�Y@^@V@�@(�@6�@E�@T�@`�@m:@|�@�7@��@�5@�~@��@�@�t@�@��@j@�@
@-@:�@FQ@T�@dZ@r�@��@�D@�H@��@�F@��@I�@��@��@@P�@�@��@
=@I�@��@�o@�@N�@�@�\@�@^�@��@��@3�@y�@��@%@K�@�h@�
@�@a�@��@�(@*S@i�@��@�@*S@i!@��@�@#�@`�@��@�h@@Q=@��@��@J@Lu@��@�|@j@D�@�p@��@�@Ji@��@�@�@N�@��@�*@V@N�@�P@�o@1@E�@��@�&@��@ 9X@ v@ �-@ �@!(�@!e	@!�m@!�/@"6@"Q�@"��@"��@#]@#9X@#t@#��@#�`@$
@$V@$��@$ƨ@$��@%7L@%p�@%�M@%��@&�@&SI@&�P@&�@'�@'<@'t�@'��@'�`@(
@(V�@(��@(Ĝ@(��@)1'@)hs@)�@)׹@*V@*H]@*�@*��@*�Y@++�@+e	@+�a@+׹@,@,M$@,�+@,��@,�9@-3�@-l�@-��@-�T@.�@.V@.��@.�W@/@/<�@/ww@/��@/��@0)�@0e�@0��@0�t@1*@1Q=@1��@1Ĝ@1��@28�@2s_@2�r@2��@3'�@3b�@3��@3�@4�@4O0@4�7@4Ĝ@5j@5?}@5uk@5�@5�@6!s@6Yn@6��@6�c@7�@7:@7�H@8K�@8�@9Q�@9�@:�d@;@;oF@;� @<|�@=,`@=�r@>4�@>��@?9X@?�@@>�@@ƨ@AT�@A��@Br�@C�@C�@D@D��@E.l@E�@F$�@F��@GUU@G��@HS�@H��@IQ=@I�@J�d@K�@Kp�@K�Q@L�+@M�@M�@N<@N�k@O<�@O��@P>�@Q�@R�@@TYn@U�0@V�@X^5@Y��@[  @\8�@]��@^� @`;d@a��@b�@dR�@e��@e��@fB@fWb@f�9@f�@g(G@g|?@g�F@g�@h@,@hww@h�W@i6@iM�@i��@i�@j{@j`�@j��@j�(@k5@@kg@k��@k��@l:@l�W@l�@m6@mK@m��@m�@n""@nX�@n��@n�@o!s@oi!@o�@o�}G�O�G�O�G�O�G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ j@ @ v@ %@ �@ 1@ 	�@ 
=@ �@ �@ V@ �@ @ o@ *@ �@ �@ �@ �@ �@  �@ #�@ %�@ '�@ *S@ ,`@ /�@ 2�@ 5?@ 8�@ ;d@ >@ A�@ D�@ F�@ K@ N�@ Q=@ UUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aŗ�Ař�Aũ�Aũ�Aŧ�AŅA�9XA��A�VA���A��A��TA���A��A���Ağ�AąA�`BA�?}A�1'A�(�A�$�A�"�A�"�A��A�oA�
=A�A���A��A��HA��A��A���A��A��#A��
A��#A��/A�AÃA� �A¸RA�E�A��#A�33A�`BA�^5A��-A��uA�jA�l�A�r�A��mA�%A�O�A���A�jA�"�A���A���A�E�A�7LA���A��-A�dZA��/A��^A��hA�|�A�33A��wA�Q�A���A���A��7A��hA�M�A���A��A�A��A��TA��/A��wA�&�A�S�A���A��A�33A�A�VA��
A�|�A���A��A�=qA�{A�A�G�A���A���A�=qA��wA��;A��RA��FA�{A��jA���A���A�C�A�/A��/A�?}A�JA���A�{A�jA��A}Ax�HAwƨAwG�Av�Au��As�FAqG�AoS�Al��Al(�Aj��Ai��Ah�yAg�
Af1Ad1'Ab��Aa��A_��A]�A[x�AY�;AY;dAX�RAX$�AW��AV��AU�ATE�AS�wAS�7AS7LAQ�wAO��AOC�AM�AL��AL^5AL�AK�wAJ��AH��AG�wAE��ADZAC�AAp�A@JA>Q�A;��A9�FA8��A7��A6I�A4��A4�A3��A.�A-dZA+��A*��A)�#A(�A(  A&��A&�RA&�+A&-A%�-A%/A$bA#�A"^5A!`BA!A �RA VAp�A�hAM�A�A�
AXA��A�FA?}A{A�AA�A��A��A�AA
��A	�
A	��A�yA��A��A|�A�DA  AƨA��A|�A\)AG�A/A��Ax�@�|�@��@��+@���@���@�E�@�@�X@�&�@��@�D@�M�@�u@�b@�dZ@�+@�&�@�Q�@���@�t�@�C�@�@�ȴ@�ff@�J@�hs@�j@�  @�\)@��@�9@��y@�j@�{@˥�@�-@��m@�"�@���@�5?@�r�@�j@��@��!@��@���@��R@�ff@��@���@�Q�@�J@�I�@���@�C�@��^@�V@��@�Q�@��F@��9@�ȴ@��7@�t�@��@��u@�V@�{@�p�@�  @���@�~�@�$�@��@���@�9X@~ȴ@}�@|j@z^5@w�@v�+@u?}@st�@r-@p�`@n�R@l�D@kdZ@j-@hb@f$�@d9X@ct�@cdZ@b~�@`��@` �@^ff@[��@Z=q@Wl�@W
=@U/@TZ@S�@R�!@Q%@PbN@N��@M�@LZ@J��@J=q@I%@Hb@F��@E�-@C��@B�\@A�^@@b@>��@>��@>@<Z@;33@;@:�@9�#@9hs@8��@8b@7��@5/@4�/@4z�@333@1��@0bN@.�@-�@,�/@+�@*�!@)��@)�@(�u@'�@'+@'�@&ff@$�@$1@#��@"��@"J@!�@��@�P@�@`B@�D@33@M�@��@%@��@�@\)@�@�-@�/@�F@S�@33@n�@��@G�@�u@b@��@�@E�@O�@Z@�@33@
��@
n�@	x�@	%@�`@�u@�@ȴ@ȴ@$�@��@�h@�@�@Z@��@�F@t�@��@ �?�v�?�I�?�C�?�+?���?�n�?��?�A�?���?��#?�r�?��y?�`B?�t�?�hs?� �?���?ܬ?�ƨ?��H?�7L?��y?�E�?ԛ�?�9X?ӕ�?��?�M�?щ7?�Ĝ?�  ?�\)?�;d?�{?�O�?�1?��m?�x�?���?ȴ9?��?Ƈ+?�?}?���?��
?�t�?�n�?���?�A�?��R?�V?��-?��?�j?�I�?�dZ?�^5?���?���?���?�Q�?���?���?���?���?�X?�x�?�x�?���?�x�?�x�?��?���?���?��9?��9?���?���?���?���?��u?��9?��9?���?���?���?�7L?�7L?�x�?���?��^?��#?��#?��#?���?���?�=q?�^5?�^5?�^5?�^5Aś�Ař�Aŗ�AœuAœuAŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŏ\Aŏ\Aŝ�AőhAř�AőhAŗ�Aş�Ař�Aş�Aş�AőhAŗ�Ař�Aś�Aŗ�Aś�Aś�Ař�Aš�AŰ!AŲ-AŰ!AŮAũ�Aŧ�Aŧ�AŬAŬAš�Aŕ�AŋDA�n�A�Q�A�9XA� �A��A��A�oA�1A�  A���A���A���A��A��yA��`A��HA��TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aŗ�Ař�Aũ�Aũ�Aŧ�AŅA�9XA��A�VA���A��A��TA���A��A���Ağ�AąA�`BA�?}A�1'A�(�A�$�A�"�A�"�A��A�oA�
=A�A���A��A��HA��A��A���A��A��#A��
A��#A��/A�AÃA� �A¸RA�E�A��#A�33A�`BA�^5A��-A��uA�jA�l�A�r�A��mA�%A�O�A���A�jA�"�A���A���A�E�A�7LA���A��-A�dZA��/A��^A��hA�|�A�33A��wA�Q�A���A���A��7A��hA�M�A���A��A�A��A��TA��/A��wA�&�A�S�A���A��A�33A�A�VA��
A�|�A���A��A�=qA�{A�A�G�A���A���A�=qA��wA��;A��RA��FA�{A��jA���A���A�C�A�/A��/A�?}A�JA���A�{A�jA��A}Ax�HAwƨAwG�Av�Au��As�FAqG�AoS�Al��Al(�Aj��Ai��Ah�yAg�
Af1Ad1'Ab��Aa��A_��A]�A[x�AY�;AY;dAX�RAX$�AW��AV��AU�ATE�AS�wAS�7AS7LAQ�wAO��AOC�AM�AL��AL^5AL�AK�wAJ��AH��AG�wAE��ADZAC�AAp�A@JA>Q�A;��A9�FA8��A7��A6I�A4��A4�A3��A.�A-dZA+��A*��A)�#A(�A(  A&��A&�RA&�+A&-A%�-A%/A$bA#�A"^5A!`BA!A �RA VAp�A�hAM�A�A�
AXA��A�FA?}A{A�AA�A��A��A�AA
��A	�
A	��A�yA��A��A|�A�DA  AƨA��A|�A\)AG�A/A��Ax�@�|�@��@��+@���@���@�E�@�@�X@�&�@��@�D@�M�@�u@�b@�dZ@�+@�&�@�Q�@���@�t�@�C�@�@�ȴ@�ff@�J@�hs@�j@�  @�\)@��@�9@��y@�j@�{@˥�@�-@��m@�"�@���@�5?@�r�@�j@��@��!@��@���@��R@�ff@��@���@�Q�@�J@�I�@���@�C�@��^@�V@��@�Q�@��F@��9@�ȴ@��7@�t�@��@��u@�V@�{@�p�@�  @���@�~�@�$�@��@���@�9X@~ȴ@}�@|j@z^5@w�@v�+@u?}@st�@r-@p�`@n�R@l�D@kdZ@j-@hb@f$�@d9X@ct�@cdZ@b~�@`��@` �@^ff@[��@Z=q@Wl�@W
=@U/@TZ@S�@R�!@Q%@PbN@N��@M�@LZ@J��@J=q@I%@Hb@F��@E�-@C��@B�\@A�^@@b@>��@>��@>@<Z@;33@;@:�@9�#@9hs@8��@8b@7��@5/@4�/@4z�@333@1��@0bN@.�@-�@,�/@+�@*�!@)��@)�@(�u@'�@'+@'�@&ff@$�@$1@#��@"��@"J@!�@��@�P@�@`B@�D@33@M�@��@%@��@�@\)@�@�-@�/@�F@S�@33@n�@��@G�@�u@b@��@�@E�@O�@Z@�@33@
��@
n�@	x�@	%@�`@�u@�@ȴ@ȴ@$�@��@�h@�@�@Z@��@�F@t�@��@ �?�v�?�I�?�C�?�+?���?�n�?��?�A�?���?��#?�r�?��y?�`B?�t�?�hs?� �?���?ܬ?�ƨ?��H?�7L?��y?�E�?ԛ�?�9X?ӕ�?��?�M�?щ7?�Ĝ?�  ?�\)?�;d?�{?�O�?�1?��m?�x�?���?ȴ9?��?Ƈ+?�?}?���?��
?�t�?�n�?���?�A�?��R?�V?��-?��?�j?�I�?�dZ?�^5?���?���?���?�Q�?���?���?���?���?�X?�x�?�x�?���?�x�?�x�?��?���?���?��9?��9?���?���?���?���?��u?��9?��9?���?���?���?�7L?�7L?�x�?���?��^?��#?��#?��#?���?���?�=q?�^5?�^5?�^5?�^5Aś�Ař�Aŗ�AœuAœuAŕ�Aŕ�Aŕ�Aŗ�Aŗ�Aŗ�Aŏ\Aŏ\Aŝ�AőhAř�AőhAŗ�Aş�Ař�Aş�Aş�AőhAŗ�Ař�Aś�Aŗ�Aś�Aś�Ař�Aš�AŰ!AŲ-AŰ!AŮAũ�Aŧ�Aŧ�AŬAŬAš�Aŕ�AŋDA�n�A�Q�A�9XA� �A��A��A�oA�1A�  A���A���A���A��A��yA��`A��HA��TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��BJB$�B=qBT�BcTBjBn�Bo�Bp�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bo�Bm�Bo�Bq�Bv�B�7B��B��B�3B�jB�#BB%�B8RB:^BE�B]/B[#BF�B,B6FB<jB6FB&�B2-Bp�Bz�B�B� B|�B�VB��B��B� B�oB��B�uB�uB�hB�bB�\B�PB�7B�7B�7B�7B�B�B{�Bw�Bu�Bk�BgmB]/BH�B>wB7LB,B$�B�BhBVBB�B�HB��B�FB��B�hB}�Bt�BjBbNBVBN�B>wB-BJB
��B
�B
�B
�5B
ŢB
��B
�=B
�B
|�B
u�B
m�B
ffB
aHB
I�B
8RB
/B
)�B
#�B
�B
PB
B	��B	�B	�B	�NB	�;B	�)B	��B	��B	�}B	�^B	�-B	��B	��B	�\B	�+B	�B	|�B	x�B	t�B	o�B	gmB	aHB	^5B	]/B	[#B	N�B	H�B	A�B	:^B	49B	0!B	.B	)�B	 �B	bB	1B��B�B�ZB�B��B�qB��B��B��B�uB�=B�+B�=B�Bo�BffB`BB[#BW
BR�BR�BP�BQ�BO�BN�BL�BK�BH�BG�BC�BD�BC�BB�B@�B?}B?}BC�BD�BF�BG�BG�BH�BE�BE�BE�BC�B?}B=qB=qB7LB6FB5?B33B.B.B-B+B,B,B+B(�B)�B(�B(�B'�B'�B%�B+B&�B(�B'�B'�B&�B&�B&�B%�B%�B$�B#�B&�B%�B&�B"�B&�B&�B&�B&�B&�B&�B%�B&�B&�B&�B'�B'�B'�B)�B)�B)�B-BA�BC�BM�BP�BdZBr�B�B�VB��B�B�qB��B�)B�fB�B	1B	�B	$�B	7LB	@�B	P�B	XB	\)B	jB	cTB	\)B	e`B	cTB	e`B	q�B	~�B	�JB	��B	��B	��B	��B	��B	�-B	�9B	�RB	�qB	ĜB	ȴB	��B	��B	��B	�#B	�BB	�BB	�NB	�`B	�mB	�sB	�B	��B	��B	��B
B
B
	7B
DB
DB
PB
bB
oB
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
%�B
&�B
(�B
)�B
+B
.B
.B
0!B
0!B
1'B
33B
49B
6FB
6FB
8RB
:^B
9XB
:^B
=qB
?}B
@�B
A�B
B�B
A�B
C�B
D�B
D�B
G�B
F�B
F�B
G�B
H�B
I�B
K�B
K�B
L�B
N�B
N�B
O�B
O�B
Q�B
P�B
R�B
Q�B
S�B
T�B
VB
W
B
XB
YB
ZB
\)B
[#B
\)B
]/B
_;B
`BB
aHB
cTB
cTB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
iyB
jB
l�B
l�B
l�B
n�B
n�B
n�B
o�B
o�B
p�B
q�B
r�B
r�B
s�B
r�B
u�B
t�B
t�B
u�B
v�B
x�B
w�B
w�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
|�B
|�B
�B
�B
�B
�B
�+B
�1B
�1B
�7B
�=B
�DB
�\B
�\B
�bB
�hB
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
��B
�B
��B
�B
�B
�B
�B
�!B
�'B
�'B
�3B
�3B
�3B
�?B
�?B
�LB
�FB
�RB
�RB
�XB
�XB
�XB
�XB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�XB
�XB
�^B
�^B
�dB
�^B
�dB
�^B
�^B
�dB
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�^B
�dB
�^B
�^B
�dB
�^B
�dB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B  BB1BDBPG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B$B$�B=MBT�Bc1Bj\BnvBo}Bp�Bq�Bq�Bq�Br�Bq�Bq�Bq�Bq�Bq�Bo�BmwBo�Bq�Bv�B�B��B��B�B�UB�BB%�B8?B:KBE�B]B[BF�B+�B67B<[B68B&�B2Bp�Bz�B��B�B|�B�KB�wB�}B�B�fB�yB�nB�nB�bB�\B�WB�KB�3B�3B�4B�4B�B�B{�Bw�Bu�Bk�BgnB]0BH�B>yB7OB,B$�B�BlB[BB�B�NB�B�MB��B�pB}�Bt�Bj�BbXBVBN�B>�B-BVB
��B
�B
�B
�CB
ŰB
��B
�LB
�!B
|�B
u�B
m�B
fwB
aZB
I�B
8dB
/.B
*B
#�B
�B
eB
B	��B	�B	�B	�eB	�SB	�AB	�B	��B	��B	�xB	�GB	��B	��B	�xB	�GB	�)B	}B	x�B	t�B	o�B	g�B	ahB	^UB	]PB	[DB	N�B	H�B	A�B	:�B	4]B	0EB	.9B	*!B	 �B	�B	XB�B��B�B�8B��B��B�B��B��B��B�hB�WB�iB�FBo�Bf�B`pB[QBW9BS!BS"BQBRBPBOBM BK�BH�BG�BC�BD�BC�BB�B@�B?�B?�BC�BD�BF�BG�BG�BH�BE�BE�BE�BC�B?�B=�B=�B7�B6�B5~B3sB.TB.TB-OB+CB,JB,JB+EB):B*@B);B);B(6B(6B&*B+IB'1B)>B(9B(9B'3B'3B'4B&.B&/B%*B$$B'7B&1B'8B# B'9B'9B':B':B';B'<B&6B'=B'=B'>B(EB(FB(FB*SB*TB*TB-gBA�BC�BN6BQJBd�BsB�yB��B�CB��B��B�GBܦB��B�B	�B	
B	%iB	7�B	AB	QzB	X�B	\�B	kB	c�B	\�B	fB	c�B	fB	rZB	�B	��B	�}B	�yB	��B	��B	��B	��B	�B	�B	�?B	�lB	ɇB	ϯB	��B	��B	�B	�#B	�%B	�4B	�HB	�XB	�aB	�B	��B	��B	��B
B
B

9B
IB
LB
[B
oB
B
�B
�B
�B
�B
�B
�B
"�B
$�B
$�B
'B
(B
*&B
+.B
,7B
/LB
/NB
1^B
1`B
2iB
4wB
5�B
7�B
7�B
9�B
;�B
:�B
;�B
>�B
@�B
A�B
B�B
C�B
B�B
D�B
FB
FB
IB
HB
HB
I$B
J-B
K5B
MEB
MGB
NOB
P^B
P`B
QiB
QkB
SzB
RvB
T�B
S�B
U�B
V�B
W�B
X�B
Y�B
Z�B
[�B
]�B
\�B
]�B
^�B
`�B
a�B
cB
eB
eB
h'B
h*B
h,B
h/B
i8B
i;B
jCB
kLB
lUB
ncB
nfB
niB
pxB
p{B
p}B
q�B
q�B
r�B
s�B
t�B
t�B
u�B
t�B
w�B
v�B
v�B
w�B
x�B
z�B
y�B
y�B
z�B
z�B
{�B
{�B
|�B
~B
~B
B
B
�3B
�>B
�KB
�VB
�oB
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
�B
�B
�B
�&B
�2B
�?B
�SB
�]B
�jB
�pB
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
�B
�B
�"B
�-B
�9B
�9B
�EB
�aB
�oB
��B
��B
��B
��B
��B
��B
��B
�B
�*B
�FB
�PB
�kB
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�	B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B�BBB+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201901311300312021061413560420210614135604202106171314062021061713140620210617131406201901311300312021061413560420210614135604202106171314062021061713140620210617131406PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019013113003120190131130031  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019013113003120190131130031QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019013113003120190131130031QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151420210617131514IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                