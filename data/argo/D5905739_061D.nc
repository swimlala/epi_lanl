CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  !   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-02T04:00:25Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       Q    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  b(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       fl   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       wt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  �|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ݬ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                      HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   $   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � <   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar            HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �\   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �\   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   \   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � \Argo profile    3.1 1.2 19500101000000  20190302040025  20210617131515  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               =   =DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ئ��J�@ئ��J�11  @ئ���U�@ئ���U�@6)(����@6)(�����c�W}�W�c�W}�W11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >L��?fff@ff@Fff@y��@���@�  @�33A   A33A#33A@  Ad��A���A���A�33A�  A���A�  A�  A���B ffB	33BffB��B��B(  B0ffB8��B@��BH  BP��BX��B`ffBhffBp  Bx  B�  B���B�  B�33B���B�  B�33B�ffB�33B���B�  B�  B�33B�  B���B�33B�33B���BǙ�B�  B�33B�33B�ffB�ffB���B���B�  B�  B�33B���B�  B�33C L�C  C  C33C33C
ffC  C  C�C�C33C��C�fC�fC��C33C 33C"�C$33C&�C(  C)�fC,33C.33C0�C2  C4  C5�fC7�fC:33C<�C>  C@L�CBL�CD�CF  CH  CJ33CL�CM�fCPL�CR�CT  CV  CX  CZL�C\33C^  C`  Ca�fCc�fCf  Cg�fCj  Cl  Cm�fCp�Cr�Ct  Cv  Cw�fCz  C{�fC~33C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��C��C��C�  C��3C��C��3C��fC�  C��C��C��3C��C�  C��3C�  C��C�  C�ٚC��3C�  C��C��C�&fC�&fC�33C��C��3C��C��C�&fC��C��3C��C��C��C��C�&fC�&fC��C��fC�  C��C��C�&fC��C��3C�  C�&fC��C��C�  C��fC��C��C��C��C��C�  C��3C��3C��C��C�  C��3C��3C�&fC��C��C�  C�  C�&fC��C��C��C��C�&fC��C��C��C��3C��C��C�  C�  C��3C��C��C��3C��C��C��C�  C��3C��3C��3C��C��C�  C�  C��fC��C�  C��3C��C��C�  C��C��C��C��C��3C��C�33C��3C�� C��3D ��D ��D�fD	s3DY�DL�D,�D�D  D� D��D L�D#  D%� D(�3D+@ D-�3D0��D3FfD5��D8��D;Y�D=��D@��DC` DF,�DI  DK�3DN��DQy�DT9�DWfDY��D\Y�D^��Dal�Dc��Dfs3Dh�fDkY�Dm��Dp  Dr� Dt�3Dw&fDys3D{ffD}�fD��D�I�D�� D��fD��D�  D�ffD��3D�� D�3D�Y�D���D��fD��D�VfD��3D�� D�3D�<�D�p D���D��3D�  D�Y�D�� D�ɚD� D�P D��3D���D��3D�)�D�` D��3D��fD���D�0 D�\�D��fD��fD��D�  D�S3D�� D�ɚD���D�0 D�` D���D��3D�fD�6fD�ffD��fD��3D���D�0 D�ffDƣ3D��3D�3D�0 D�c3Ď�D��3D��3D��D�P D҆fDӼ�D���D�)�D�` DؖfD�ɚD�3D�9�D�` Dޙ�D�ٚD� D�FfD� D乚D���D��D�L�D�fD�� D��fD�,�D�c3D��D�� D�	�D�9�D�p D��3D�ٚD��D�FfD�vfD��3D��fD�ٚD�  E  E �3E4�EɚE\�E�E�fE E��EC3EٚEq�E�E��E+3E��E	P E	�fE
nfE��E�3EH ET�Ed�E  E�E3E��E��E#3E+3E��E��EC3EK3E��E � E!��E#h E$c3E%�fE&�3E(X E)Y�E*��E+��E-A�E.�fE/�fE1&fE23E3� E4� E5�3E733E8�3E9��E:� E<  E?[3EB�3EE� EH��EL3EN��ERFfEU+3EX��E[� E^h Ea�fEd�fEg� Ek0 En�Eq$�Etx Ew�3Ez��E}��E�}�E��E���E� E��3E�E�E��fE�d E��fE�nfE� E�]�E���E� E�O3E�� E�� E�2fE���E��3E�2fE�p E�͚E�3E�h�E��3E�fE�[3E�� E��E�E�E���E�ٚE�2fE���E��E�&fE���E��fE�fE�[3E��3E�
fE�` E�� E���E�O3E���E��f=���=���=���=���>���>L��>L��=���=���>L��>L��>L��=���=���>L��>L��>L��=���=���>L��>L��>L��>���>L��>���>���>���?��?333?��?L��?L��?333?fff?�  ?���?�ff?ٙ�?�ff@��@   @9��@L��@fff@�33@�33@���@���@�ff@�  @���@ٙ�@���@�ffA��AffAffA��A#33A)��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441444414444144141141141441111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?333?�33@&ff@fff@���@���@�  @�33A  A33A+33AH  Al��A���A���A�33A�  A���A�  A�  A���BffB33BffB��B!��B*  B2ffB:��BB��BJ  BR��BZ��BbffBjffBr  Bz  B�  B���B�  B�33B���B�  B�33B�ffB�33B���B�  B�  B�33B�  B���B�33B�33B���Bș�B�  B�33B�33B�ffB�ffBᙚB���B�  B�  B�33B���B�  B�33C ��C� C� C�3C�3C
�fC� C� C��C��C�3CL�CffCffCL�C�3C �3C"��C$�3C&��C(� C*ffC,�3C.�3C0��C2� C4� C6ffC8ffC:�3C<��C>� C@��CB��CD��CF� CH� CJ�3CL��CNffCP��CR��CT� CV� CX� CZ��C\�3C^� C`� CbffCdffCf� ChffCj� Cl� CnffCp��Cr��Ct� Cv� CxffCz� C|ffC~�3C�Y�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�33C�33C�Y�C�L�C�L�C�@ C�33C�L�C�33C�&fC�@ C�Y�C�Y�C�33C�Y�C�@ C�33C�@ C�Y�C�@ C��C�33C�@ C�L�C�Y�C�ffC�ffC�s3C�L�C�33C�L�C�Y�C�ffC�Y�C�33C�L�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�@ C�L�C�L�C�ffC�Y�C�33C�@ C�ffC�Y�C�L�C�@ C�&fC�L�C�Y�C�Y�C�L�C�L�C�@ C�33C�33C�Y�C�Y�C�@ C�33C�33C�ffC�Y�C�L�C�@ C�@ C�ffC�Y�C�Y�C�Y�C�L�C�ffC�Y�C�L�C�L�C�33C�Y�C�Y�C�@ C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�L�C�@ C�33C�33C�33C�L�C�Y�C�@ C�@ C�&fC�L�C�@ C�33C�Y�C�L�C�@ C�Y�C�Y�C�L�C�L�C�33C�L�C�s3C�33C�  D �D ��D�D�fD	�3Dy�Dl�DL�D9�D  D� D��D l�D#@ D&  D(�3D+` D.3D0��D3ffD6�D8ٚD;y�D>�D@��DC� DFL�DI  DK�3DN��DQ��DTY�DW&fDYٚD\y�D_�Da��Dd�Df�3DifDky�Dm��Dp@ Dr� Dt�3DwFfDy�3D{�fD}�fD�)�D�Y�D�� D��fD���D�0 D�vfD��3D�� D�#3D�i�D���D��fD�)�D�ffD��3D�� D�3D�L�D�� D���D��3D�0 D�i�D�� D�ٚD�  D�` D��3D���D�3D�9�D�p D��3D��fD��D�@ D�l�D��fD��fD���D�0 D�c3D�� D�ٚD�	�D�@ D�p D���D��3D�fD�FfD�vfD��fD��3D�	�D�@ D�vfDƳ3D��3D�3D�@ D�s3D̜�D��3D�3D�,�D�` DҖfD���D���D�9�D�p DئfD�ٚD�3D�I�D�p Dީ�D��D�  D�VfD� D�ɚD���D�,�D�\�D�fD�� D�fD�<�D�s3D��D�� D��D�I�D� D��3D��D��D�VfD��fD��3D��fD��D� E  E �3E<�EњEd�E��E�fE  E��EK3E�Ey�E�E��E33E��E	X E	�fE
vfE��E�3EP E\�El�E E�E3E��E��E+3E33E��E��EK3ES3E��E � E!��E#p E$k3E%�fE&�3E(` E)a�E*��E+��E-I�E.�fE/�fE1.fE2#3E3� E4� E5�3E7;3E8�3E9��E:� E<( E?c3EB�3EE� EH��EL#3EN��ERNfEU33EX��E[� E^p Ea�fEd�fEg� Ek8 En�Eq,�Et� Ew�3Ez��E}��E���E��E���E�  E��3E�I�E��fE�h E��fE�rfE� E�a�E���E� E�S3E�� E�� E�6fE���E��3E�6fE�t E�њE�3E�l�E��3E�fE�_3E�� E��E�I�E���E�ݚE�6fE���E��E�*fE���E��fE�"fE�_3E��3E�fE�d E�� E���E�S3E���E��fG�O�G�O�G�O�?��G�O�G�O�G�O�G�O�?��G�O�G�O�G�O�G�O�?��G�O�G�O�G�O�G�O�?��G�O�G�O�?333G�O�?333?L��G�O�?fff?���G�O�?���G�O�G�O�?���?�33?�  ?ٙ�?�ff@��@33@,��@@  @Y��@l��@�33@�33@�33@���@���@�ff@�  @���@陚@���A33A��AffAffA$��A+33A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441444414444144141141141441111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ ^@ %@ V@ *@ �@ !s@ (�@ 0x@ 6�@ >�@ E�@ Q�@ a�@ n�@ |?@ ��@ �0@ ��@ �~@ �&@ �|@ �#@ �(@ �q@�@b@�@-@;d@I@UU@dZ@r@~�@��@��@�A@��@��@�7@ލ@��@�,@�@�@"�@/@=q@K@Yn@ff@s_@�d@�@�U@�M@�R@ƨ@�O@�@�L@��@	�@�@%�@3�@B�@N�@\�@k�@ww@�@�#@��@��@��@�c@׹@�`@�@��@�@�@'�@7�@E�@R�@`�@m�@z�@��@��@�5@�-@�&@��@��@�m@� @@@ �@.l@:�@G�@UU@dZ@qS@}�@��@�H@�A@��@@�C@�;@�@�,@%@�@""@/@=q@K@X@g@t�@��@�\@�U@��@��@�W@��@��@��@��@
=@�@%�@33@@�@M�@[z@i!@x�@��@�u@�m@�f@�k@ȴ@խ@�@�@^@�@�@(�@5�@DD@SI@_�@k.@z3@��@��@�5@��@�2@ψ@�#@�m@�q@	�@	@	 @	+�@	:�@	I@	V�@	dZ@	r�@	�W@	��@	�<@	�A@	��@	�>@	�C@	�;@	��@	�,@
�@
�@
"�@
/�@
<@
K�@
Z@
g�@
t�@
�d@
�\@
�U@
��@
��@
�W@
Ӡ@
��@
�@@
��@�@�@%�@33@B�@O�@]�@k.@x&@�+@�$@�@��@�@��@�h@�@�Y@�Q@@�@(G@7�@D�@R�@_�@l�@z3@��@��@�5@�~@�&@�o@�#@�@�@�@�@�@-�@;d@H]@V@bN@qS@�@�D@�0@��@�F@��@\�@�M@�~@I@��@�m@6�@��@�*@B@ff@�~@�9@DD@��@�\@g@i!@�9@��@C�@�P@�
@#�@p�@��@�@X@�(@�@9X@�@�W@�@O�@��@׹@�@]�@�@��@[@\�@��@��@�@S�@��@�
@B@Z�@�@�@&;@i�@�Y@��@5@@x�@��@]@D�@��@�@ �@ N�@ �i@ �O@!�@!Z�@!�@!��@"%�@"i�@"�Y@"�@@#0x@#r�@#��@#�q@$7�@$z3@$��@$��@%;d@%|?@%��@&  @&A�@&�@&�@'�@'K@'��@'��@(�@(SI@(�#@(��@)�@)V@)�<@)�t@*�@*`A@*�@*��@+""@+c�@+�(@+�`@,&;@,e�@,�A@,�y@-+�@-l�@-�!@-�Y@.4�@.v@.�@.�9@/:@/|�@/�2@0j@0E�@0��@0�o@1�@1M�@1��@1є@2{@2V�@2��@2�#@3
@3_�@3�z@3�T@4%�@4g@4�M@4��@5-�@5n�@5�Y@5�y@6'�@6ff@6��@6�@7 �@7`B@7�@7ލ@8	@8\)@8��@8��@9�@9\�@9��@9�#@:B@:Wb@:�0@:��@;b@;��@<�@<��@='�@=��@>K@>��@?-�@?խ@@D�@@�y@AZ@B@BqS@CB@C��@D.l@D�m@E@E�R@F#�@F�J@G1'@GӠ@HA�@H�T@IM$@I�4@J��@J�}@K��@K��@L�0@M/�@M�#@N*S@N��@OS�@O��@PDD@Q�4@S�@Tm:@U��@W�@XM$@Y��@Z�@\g@]�A@^�@`Ji@a�T@b�@dX�@e��@f�T@hN�@i��@j��@lI�@m�5@n��@pI@q�0@r�@tI@u��@v�Y@x5?@y��@z��@{1'@{m�@{��@{�Q@|6�@|��@|�1@}�@}M�@}��@}�7@~ @~T�@~��@~�@'�@s_@�&@�M@��@�C>@�\�@���@��M@���@���@�@�-f@�T�@�n�@��#@��X@���@��w@�[@�C�@�h�@��G�O�G�O�G�O�@  �G�O�G�O�G�O�G�O�@  �G�O�G�O�G�O�G�O�@  �G�O�G�O�G�O�G�O�@  �G�O�G�O�@ ^G�O�@ ^@ G�O�@ �@ G�O�@ G�O�G�O�@ �@ %@ �@ 1@ �@ �@ J@ @ @ �@ �@ �@ �@ g@ !s@ $.@ &�@ (�@ +�@ .l@ 2�@ 4�@ 8�@ <�@ @,@ B�@ E�@ H]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�x�A��A�x�A��A���A���A���A���A��jA���A���A�A�ƨA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ĜA�ĜA���A��jA���A���A�`BA�;dA��A�`BA��A�I�A���A���A�r�A��;A���A�hsA��HA��wA���A��hA�r�A�ĜA�ȴA�%A��A��;A���A�VA�Q�A�  A���A�+A��-A���A�9XA��hA�VA��A�~�A��HA��A�S�A���A�%A��/A���A��!A�;dA��^A�I�A�"�A�1'A�S�A���A��A�K�A���A��jA��TA��A�  A��`A�O�A��A��A���A��-A���A�(�A���A�XA��RA��A���A��#A�
=A��A�A�A��mA��A���A���A���A��+A��A���A�A��TA�ZA��FA��AC�A}p�A|-Azr�Aw�^AvJAqx�Aip�Ac�hAb�/Ab�Ab-A`��A^��A\��A[hsAZ{AU33ARv�AO�-AN��AL�RAK"�AI�AH1AG|�AG+AD�HA@��A?t�A?`BA?G�A?%A>�DA=ƨA;�TA9��A7?}A6�uA4�RA2��A0��A0=qA/�FA.ĜA-�A-C�A+�A*E�A)�A)A(��A'�-A&(�A%?}A$ffA 9XA{A��AĜAt�A�^A�PA��AQ�AJA��A�A�mA��A�FA�At�A&�A�#AoA��A1'A�#AO�Av�AK�A33Av�A��AC�A
��A
  A	�A��A�7AM�A�A��A-Ax�AO�A �/A z�@��@���@�@���@�&�@�C�@��@�=q@��`@�S�@�{@���@�dZ@ꟾ@�$�@蛦@�ff@��@�!@�-@�p�@���@�S�@�^5@���@���@�1@��@�O�@ج@�b@ׅ@֟�@�V@�j@Ӿw@���@�@� �@��@��@�Ĝ@�dZ@���@�\)@�%@�\)@��+@���@�1'@�{@���@�$�@��@� �@�o@�^5@�bN@�
=@�X@�;d@��7@���@��@�o@���@�@���@�33@�(�@���@��@��D@��@�~�@�`B@�I�@~�@|I�@z=q@y�@w+@u��@t�/@q%@pA�@o|�@nE�@mO�@l�D@j��@i��@ix�@h�9@g|�@f��@fv�@e�T@d1@a��@` �@_��@_+@\j@\1@[�
@[o@Y&�@XA�@X �@Vff@T�D@S33@QX@Q&�@P�u@O;d@M�@Jn�@I�@G�@F5?@B�@B�@@��@@r�@=�@<��@;��@:�@9�@7�;@5`B@4�@4Z@2�H@1�7@0��@0A�@/|�@.�y@-p�@+�@)��@)hs@)&�@'�@'l�@'+@&{@$��@$��@$Z@$j@$9X@#t�@"^5@!��@ �`@��@��@�+@��@��@C�@C�@"�@M�@��@�7@&�@��@�@�@Z@�F@33@@��@��@Q�@�w@
=@@�@(�@
�@	�^@	&�@�9@ �@l�@ȴ@�T@�D@I�@�F@@�!@�7@�@ �`@ bN?�\)?���?�v�?�{?�p�?�1?��9?�l�?��y?�9X?�t�?�w?�{?�D?�(�?���?���?�+?��T?�z�?�S�?�!?�Ĝ?�\)?�V?��?��m?�7L?׍P?�
=?ա�?�Z?���?�M�?�&�?Ͼw?θR?�p�?��?�1?�1?˅?ɺ^?�Q�?Ǯ?�ȴ?�ff?���?�t�?��?��7?�Ĝ?��?��?�p�?���?�j?�ƨ?�1?���?�ƨ?��?���?�dZ?�dZ?���?�C�?�"�?�C�?���?�1?�j?��?�O�?��?��R?�;d?�|�?�A�?�A�?�bN?��?���?���?�Ĝ?��`?�&�?�G�?�hs?��7?���?���?��?�-?�M�?\?�n�?°!?��?�o?�o?�S�?�t�?öF?öF?öF?���?��?�9X?�Z?ě�?ļj?���?���?�`B?�`B?ŁA�|�A�z�A�bNA�hsA�jA�p�A�r�A��A�t�A�r�A�r�A�hsA�jA�v�A�x�A�t�A�z�A��7A�~�A��\A���A���A���A��A���A��+A��A�~�A��A�z�A�t�A��PA�x�A�p�A�n�A�r�A�p�A�p�A��\A��+A�~�A�x�A�v�A�|�A��PA��7A���A���A���A���A���A���A���A���A���A��RA��wA���A��wA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�z�A�x�A��A�x�A��A���A���A���A���A��jA���A���A�A�ƨA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ĜA�ĜA���A��jA���A���A�`BA�;dA��A�`BA��A�I�A���A���A�r�A��;A���A�hsA��HA��wA���A��hA�r�A�ĜA�ȴA�%A��A��;A���A�VA�Q�A�  A���A�+A��-A���A�9XA��hA�VA��A�~�A��HA��A�S�A���A�%A��/A���A��!A�;dA��^A�I�A�"�A�1'A�S�A���A��A�K�A���A��jA��TA��A�  A��`A�O�A��A��A���A��-A���A�(�A���A�XA��RA��A���A��#A�
=A��A�A�A��mA��A���A���A���A��+A��A���A�A��TA�ZA��FA��AC�A}p�A|-Azr�Aw�^AvJAqx�Aip�Ac�hAb�/Ab�Ab-A`��A^��A\��A[hsAZ{AU33ARv�AO�-AN��AL�RAK"�AI�AH1AG|�AG+AD�HA@��A?t�A?`BA?G�A?%A>�DA=ƨA;�TA9��A7?}A6�uA4�RA2��A0��A0=qA/�FA.ĜA-�A-C�A+�A*E�A)�A)A(��A'�-A&(�A%?}A$ffA 9XA{A��AĜAt�A�^A�PA��AQ�AJA��A�A�mA��A�FA�At�A&�A�#AoA��A1'A�#AO�Av�AK�A33Av�A��AC�A
��A
  A	�A��A�7AM�A�A��A-Ax�AO�A �/A z�@��@���@�@���@�&�@�C�@��@�=q@��`@�S�@�{@���@�dZ@ꟾ@�$�@蛦@�ff@��@�!@�-@�p�@���@�S�@�^5@���@���@�1@��@�O�@ج@�b@ׅ@֟�@�V@�j@Ӿw@���@�@� �@��@��@�Ĝ@�dZ@���@�\)@�%@�\)@��+@���@�1'@�{@���@�$�@��@� �@�o@�^5@�bN@�
=@�X@�;d@��7@���@��@�o@���@�@���@�33@�(�@���@��@��D@��@�~�@�`B@�I�@~�@|I�@z=q@y�@w+@u��@t�/@q%@pA�@o|�@nE�@mO�@l�D@j��@i��@ix�@h�9@g|�@f��@fv�@e�T@d1@a��@` �@_��@_+@\j@\1@[�
@[o@Y&�@XA�@X �@Vff@T�D@S33@QX@Q&�@P�u@O;d@M�@Jn�@I�@G�@F5?@B�@B�@@��@@r�@=�@<��@;��@:�@9�@7�;@5`B@4�@4Z@2�H@1�7@0��@0A�@/|�@.�y@-p�@+�@)��@)hs@)&�@'�@'l�@'+@&{@$��@$��@$Z@$j@$9X@#t�@"^5@!��@ �`@��@��@�+@��@��@C�@C�@"�@M�@��@�7@&�@��@�@�@Z@�F@33@@��@��@Q�@�w@
=@@�@(�@
�@	�^@	&�@�9@ �@l�@ȴ@�T@�D@I�@�F@@�!@�7@�@ �`@ bN?�\)?���?�v�?�{?�p�?�1?��9?�l�?��y?�9X?�t�?�w?�{?�D?�(�?���?���?�+?��T?�z�?�S�?�!?�Ĝ?�\)?�V?��?��m?�7L?׍P?�
=?ա�?�Z?���?�M�?�&�?Ͼw?θR?�p�?��?�1?�1?˅?ɺ^?�Q�?Ǯ?�ȴ?�ff?���?�t�?��?��7?�Ĝ?��?��?�p�?���?�j?�ƨ?�1?���?�ƨ?��?���?�dZ?�dZ?���?�C�?�"�?�C�?���?�1?�j?��?�O�?��?��R?�;d?�|�?�A�?�A�?�bN?��?���?���?�Ĝ?��`?�&�?�G�?�hs?��7?���?���?��?�-?�M�?\?�n�?°!?��?�o?�o?�S�?�t�?öF?öF?öF?���?��?�9X?�Z?ě�?ļj?���?���?�`B?�`B?ŁA�|�A�z�A�bNA�hsA�jA�p�A�r�A��A�t�A�r�A�r�A�hsA�jA�v�A�x�A�t�A�z�A��7A�~�A��\A���A���A���A��A���A��+A��A�~�A��A�z�A�t�A��PA�x�A�p�A�n�A�r�A�p�A�p�A��\A��+A�~�A�x�A�v�A�|�A��PA��7A���A���A���A���A���A���A���A���A���A��RA��wA���A��wA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B>wB>wB?}B?}B?}B?}B>wB>wB?}B=qB=qB=qB=qB=qB=qB=qB>wB=qB=qB=qB=qB=qB>wB=qB=qB=qB>wB=qB>wB=qB>wBA�BE�BG�BH�BP�BVBjBu�B�oB��B�3B�sB�B�B�B)�B1'B2-B9XB=qB>wBH�BG�BE�BD�BD�BG�BG�BH�BC�B(�B#�B�B!�B"�B(�B+B$�B"�B�B#�B#�B$�BPBB��B�sB�`B�mB�B�B�B�B�sB�TB�B��B�!B��B��B��B��B�hB�PBt�BcTBbNB\)BZBVBT�BQ�BL�BF�B<jB �BB
�ZB
��B
ŢB
�jB
�'B
�B
��B
��B
�uB
�%B
v�B
^5B
N�B
;dB
"�B
�B
%B	�B	�fB	�HB	�)B	��B	�wB	�B	�DB	P�B	7LB	-B	(�B	"�B	{B	DB	
=B		7B��B�B�`B�B��B��BȴBĜBĜBĜBÖB��B��B��B��BȴBǮBĜBĜB�qB�'B�B��B��B��B�uB�hB�PB�7B�+B�By�Bv�Bq�Bp�Bm�BjBcTBbNBYBW
BT�BP�BN�BL�BF�BE�BE�BC�BA�BB�BA�BA�B@�B@�B@�B@�B?}B>wB>wB<jB<jB;dB:^B9XB7LB6FB6FB49B33B33B2-B/B/B.B.B-B-B+B,B)�B)�B)�B(�B(�B)�B+B(�B&�B)�B)�B(�B+B,B-B/B0!B0!B/B0!B49B49B49B33B33B5?B5?B49B49B2-B7LB8RB9XB9XB9XB8RB9XB9XB;dB=qB>wBD�BaHBe`Bx�B�=B��B��BȴB�B��B	\B	�B	'�B	0!B	@�B	C�B	_;B	hsB	l�B	x�B	�B	�VB	�hB	��B	��B	��B	��B	�9B	�XB	ɺB	��B	�#B	�BB	�mB	�B	�B	�B	��B	��B	��B
  B
B
B
B
+B
	7B
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
$�B
#�B
$�B
'�B
'�B
'�B
(�B
)�B
+B
+B
,B
.B
/B
33B
33B
49B
5?B
7LB
9XB
:^B
;dB
;dB
?}B
?}B
@�B
A�B
C�B
D�B
E�B
F�B
G�B
I�B
J�B
L�B
K�B
M�B
O�B
O�B
P�B
P�B
Q�B
S�B
T�B
W
B
W
B
W
B
XB
YB
YB
ZB
\)B
[#B
\)B
\)B
\)B
\)B
^5B
_;B
_;B
`BB
bNB
aHB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
gmB
gmB
iyB
k�B
k�B
k�B
m�B
l�B
m�B
n�B
p�B
p�B
p�B
r�B
t�B
s�B
u�B
w�B
w�B
w�B
x�B
x�B
z�B
{�B
|�B
|�B
}�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�+B
�7B
�7B
�DB
�JB
�JB
�JB
�PB
�PB
�VB
�\B
�bB
�hB
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
�B
�B
�B
�B
�B
�'B
�'B
�-B
�9B
�9B
�9B
�?B
�FB
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�^B
�^B
�^B
�^B
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
�jB
�jB
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
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�jB
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�^B
�jB
�jBC�B=qB@�B>wBA�BA�B@�B<jB?}B?}B?}B>wB>wB@�B>wB?}B?}B<jB=qB<jB;dB=qB<jB?}B:^B@�B<jB?}B<jB?}B9XB;dB>wB@�B@�B?}B?}BC�B>wB<jB?}B?}B?}B?}B>wB@�B@�B=qB>wB>wB=qB>wB>wB>wB?}B=qB=qB<jB=qB=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B>MB>MB?SB?TB?TB?TB>OB>OB?UB=JB=JB=JB=KB=LB=LB=MB>TB=NB=OB=OB=PB=QB>WB=RB=RB=SB>ZB=TB>[B=UB>\BAoBE�BG�BH�BP�BU�BjhBu�B�YB��B�B�_B�B�B�B)�B1B2B9HB=aB>hBH�BG�BE�BD�BD�BG�BG�BH�BC�B(�B#�B�B!�B"�B(�B*�B$�B"�B�B#�B#�B$�BLBB��B�qB�^B�lB�B�B�B�B�uB�VB�B��B�%B��B��B��B��B�nB�WBt�Bc\BbVB\2BZ&BVBUBQ�BL�BF�B<wB �B&B
�gB
��B
ŰB
�yB
�6B
�B
�B
��B
��B
�7B
v�B
^HB
N�B
;xB
"�B
�B
:B	��B	�|B	�^B	�@B	��B	��B	�3B	�\B	P�B	7dB	-'B	)B	"�B	�B	_B	
XB		RB�
B�B�|B�4B�B��B��BĻBĻBĻBöB��B��B��B��B��B��BĿB��B��B�LB�9B�B��B��B��B��B�xB�_B�TB�5BzBv�Bq�Bp�Bm�Bj�Bc�Bb{BYEBW8BU,BQBOBL�BF�BE�BE�BC�BA�BB�BA�BA�B@�B@�B@�B@�B?�B>�B>�B<�B<�B;�B:�B9�B7�B6�B6�B4uB3pB3pB2kB/YB/ZB.SB.TB-NB-OB+CB,JB*>B*?B*?B):B):B*AB+GB)<B'/B*CB*CB)>B+JB,QB-WB/eB0kB0lB/fB0mB4�B4�B4�B3�B3�B5�B5�B4�B4�B2~B7�B8�B9�B9�B9�B8�B9�B9�B;�B=�B>�BD�Ba�Be�By9B��B�TB��B�%B�yB�fB	�B	6B	(qB	0�B	AB	D!B	_�B	iB	mB	ymB	��B	��B	�	B	�+B	�rB	��B	��B	��B	�B	�rB	̃B	��B	�B	�2B	�MB	�iB	�B	��B	��B	��B
 �B
�B
�B
�B
B

B
QB
TB
^B
mB
vB
yB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
%�B
%�B
$�B
%�B
)B
)B
)B
* B
+)B
,2B
,5B
->B
/MB
0WB
4qB
4tB
5}B
6�B
8�B
:�B
;�B
<�B
<�B
@�B
@�B
A�B
B�B
D�B
FB
GB
HB
IB
K,B
L6B
NEB
MBB
OQB
Q_B
QbB
RkB
RnB
SxB
U�B
V�B
X�B
X�B
X�B
Y�B
Z�B
Z�B
[�B
]�B
\�B
]�B
]�B
]�B
]�B
_�B
`�B
`�B
bB
dB
cB
f%B
f(B
f+B
f.B
g7B
g:B
hCB
hFB
iPB
iSB
kbB
mqB
mtB
mvB
o�B
n�B
o�B
p�B
r�B
r�B
r�B
t�B
v�B
u�B
w�B
y�B
y�B
y�B
z�B
z�B
}B
~B
B
B
�!B
�*B
�-B
�5B
�8B
�;B
�DB
�GB
�PB
�XB
�[B
�^B
�hB
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
�B
�B
�B
�"B
�9B
�:B
�EB
�JB
�^B
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
�B
�B
�B
�B
�"B
�.B
�3B
�9B
�UB
�jB
��B
��B
��B
��B
��B
��B
�B
�B
�#B
�9B
�PB
�^B
�oB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�/B
�DB
�NB
�cB
�qB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�BClB=GB@YB>MBA_BA_B@YB<@B?SB?SB?SB>MB>MB@YB>MB?SB?SB<@B=GB<@B;:B=GB<@B?SB:4B@YB<@B?SB<@B?SB9.B;:B>MB@YB@YB?SB?SBClB>MB<@B?TB?TB?TB?TB>NB@ZB@ZB=HB>OB>OB=IB>OB>OB>OB?UB=JB=JB<CB=JB=JG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903020400252021061413560720210614135607202106171314152021061713141520210617131415201903020400252021061413560720210614135607202106171314152021061713141520210617131415PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019030204002520190302040025  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019030204002520190302040025QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019030204002520190302040025QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151520210617131515IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                