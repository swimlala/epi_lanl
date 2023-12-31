CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-08T07:03:20Z creation      
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
_FillValue                 4  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ɣ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ۘ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �       � Argo profile    3.1 1.2 19500101000000  20180808070320  20210722160151  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�u�q���@�u�q���11  @�u�ffpp@�u�ffpp@6�\(�@6�\(��cӍ���cӍ��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?fff?�33@@  @�33@�  @���@�  A   A��A$��A@  A`  A���A�  A���A���A�  A���AᙚA�  A�33B33B  BffB ffB(ffB0  B7��B@  BHffBP  BW��B_��Bh  BpffBxffB�  B�33B�33B�  B�33B�  B�33B�33B�ffB�33B�33B�  B�33B�ffB�33B�  B�  B�  B�  B�ffB���B�  B�ffBܙ�B�33B�  B�33B�  BB�  B�ffB�33C   C33C�C�fCL�C
33C33C�C�C�C  C  C�fCL�CL�C33C 33C!�fC#��C&L�C(33C*33C,�C.  C/�fC1��C4�C6L�C8�C:  C<33C>33C@  CB33CD�CE��CH  CJ�CL�CNL�CPL�CR33CS�fCU�fCX  CZ  C\  C^  C`�Ca�fCd  Cf  Ch�Cj�Cl33Cn  Co��Cq��Ct  Cv33Cx  Cy�3C{��C}��C�fC�  C��C�  C��fC��3C��C��C�&fC��C��3C��C�&fC��C�  C��C�  C��fC��3C��C�  C��fC��C��3C��fC��C��3C��3C�  C�  C��fC�  C�&fC��C��3C��C�  C�  C�  C��fC�  C��C��C��C�  C��3C��C��C��3C��C��C�  C��C��C�  C��C��C��3C��C�&fC��C��3C��C��C�  C��fC�  C��C�&fC�  C��fC��3C��C��C�&fC�&fC�  C��fC��fC��fC��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�&fC��C�&fC�&fC��C��fC��3C�  C��C�  C��fC��3C��C��C�  C��fC��3C��C��C��C��3C��C��C��C��fC��C��3C���D��D3D� D
L�D��D,�D� DfDs3D��DY�D� D @ D"��D%Y�D'��D*� D-9�D/ٚD2�fD5,�D7� D:S3D<�fD?y�DB�DD� DGL�DI�fDLy�DO  DQs3DT  DV��DYfD[y�D]��D`FfDb��De�Dgs3Di�fDl  Dny�Dp� DsFfDu��DxfDzffD|y�D~�fD���D�ٚD��D�P D��fD��3D�  D�@ D�� D���D��fD�0 D�ffD��3D�ٚD�	�D�@ D�vfD�� D��fD�  D�VfD�� D���D���D�)�D�c3D��3D��fD��fD�#3D�FfD�i�D�� D���D��3D� D�6fD�Y�D�y�D��3D���D��fD��3D�3D��D�0 D�FfD�` D�y�D���D���D��fD�� D�3D�6fD�c3DÌ�DĹ�D��3D�	�D�33D�VfD�y�Dˠ D��fD��3D�#3D�P Dу3DҶfD���D�33D�l�Dש�D��3D��D�P D܀ Dݼ�D���D�6fD�l�D⩚D��D��D�S3D�3D�fD��3D�fD�0 D�Y�D�3D�fD�ɚD�� D�3D�@ D�i�D��3D���D��fD� D��D�9�D�S3D�vfD��3E X E � Eq�E�fE�fE�E��EfE��E�E��E3E�E�E	k3E
�fE�3E Ec3E� E��E Ek3E^fE��E33E!�E�3E3E3E�3E|�E�E � E"X E#��E$�fE&!�E'� E(s3E)�fE+4�E,� E-i�E.� E0�E1[3E2� E3�E5fE6X E7��E8��E9�E;,�E<c3E?��EB�3EE�3EI8 ELfEOvfERX EU��EX�fE[� E^� Ea��EeC3Eh�Ek�3Ent�Eq�3Et� Ew�fEz�fE~fE��3E�0�E��fE�NfE��3E�\ E��3E�@ E�� E���E�L E�� E��E�.fE�w3E���E�3E�T E���E�fE�G3E�� E��E�O3E��3E��fE�( E�� E��3E��E�y�E���E�3E�\ E�� E��E�\�E��3E��E�H�E��3E���E�-�E�s3E��f?��?��?333?333?333?333?333?L��?L��?fff?fff?fff?���?���?�ff?�  ?���?ٙ�?�33@ff@��@&ff@333@L��@`  @l��@�33@���@���@�33@�33@���@ə�@�ff@�ff@�33A   AffA��A��A��A#33A,��A4��A;33AA��AK33AQ��AVffA`  AfffAnffAt��A|��A�ffA�ffA���A�ffA�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444141441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�33@��@`  @�33@�  @���@�  A  A��A,��AH  Ah  A���A�  A���A���A�  A���A噚A�  B��B	33B  BffB"ffB*ffB2  B9��BB  BJffBR  BY��Ba��Bj  BrffBzffB�  B�33B�33B�  B�33B�  B�33B�33B�ffB�33B�33B�  B�33B�ffB�33B�  B�  B�  B�  B�ffB���B�  B�ffBݙ�B�33B�  B�33B�  B�B�  B�ffB�33C � C�3C��CffC��C
�3C�3C��C��C��C� C� CffC��C��C�3C �3C"ffC$L�C&��C(�3C*�3C,��C.� C0ffC2L�C4��C6��C8��C:� C<�3C>�3C@� CB�3CD��CFL�CH� CJ��CL��CN��CP��CR�3CTffCVffCX� CZ� C\� C^� C`��CbffCd� Cf� Ch��Cj��Cl�3Cn� CpL�CrL�Ct� Cv�3Cx� Cz33C|L�C~L�C�33C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�ffC�L�C�33C�L�C�ffC�L�C�@ C�L�C�@ C�&fC�33C�Y�C�@ C�&fC�L�C�33C�&fC�L�C�33C�33C�@ C�@ C�&fC�@ C�ffC�L�C�33C�Y�C�@ C�@ C�@ C�&fC�@ C�Y�C�Y�C�L�C�@ C�33C�Y�C�L�C�33C�Y�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�Y�C�33C�L�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�@ C�&fC�33C�L�C�Y�C�ffC�ffC�@ C�&fC�&fC�&fC�33C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�Y�C�ffC�Y�C�ffC�ffC�Y�C�&fC�33C�@ C�L�C�@ C�&fC�33C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�L�C�33C�L�C�Y�C�L�C�&fC�L�C�33C��D��D33D� D
l�DٚDL�D� D&fD�3D�Dy�D� D ` D"��D%y�D(�D*� D-Y�D/��D2�fD5L�D7� D:s3D=fD?��DB,�DD� DGl�DJfDL��DO  DQ�3DT  DV��DY&fD[��D^�D`ffDb��De,�Dg�3Di�fDl@ Dn��Dq  DsffDu��Dx&fDz�fD|��DfD���D��D�)�D�` D��fD��3D� D�P D�� D�ɚD�fD�@ D�vfD��3D��D��D�P D��fD�� D��fD�0 D�ffD�� D���D�	�D�9�D�s3D��3D��fD�fD�33D�VfD�y�D�� D�ɚD��3D�  D�FfD�i�D���D��3D���D��fD��3D�3D�)�D�@ D�VfD�p D���D���D���D��fD�  D�#3D�FfD�s3DÜ�D�ɚD��3D��D�C3D�ffDʉ�D˰ D��fD�3D�33D�` Dѓ3D��fD�	�D�C3D�|�D׹�D��3D�,�D�` Dܐ D���D��D�FfD�|�D⹚D���D�,�D�c3D�3D�fD��3D�fD�@ D�i�D�3D�fD�ٚD�  D�#3D�P D�y�D��3D�ɚD��fD�  D�)�D�I�D�c3D��fD��3E ` E � Ey�EfE�fE�E��EfE��E�E��E3E�E	�E	s3E
�fE�3E Ek3E� E��E Es3EffE��E;3E)�E�3E3E3E�3E��E��E � E"` E#��E$�fE&)�E'� E({3E)�fE+<�E,� E-q�E.� E0�E1c3E2� E3�E5&fE6` E7��E8ɚE9��E;4�E<k3E?��EB�3EE�3EI@ EL&fEO~fER` EU��EX�fE[� E^� Eb�EeK3Eh�Ek�3En|�Eq�3Et� Ew�fE{fE~fE��3E�4�E��fE�RfE��3E�` E��3E�D E�� E���E�P E�� E��E�2fE�{3E�ŚE�3E�X E���E�fE�K3E�� E��E�S3E��3E��fE�, E�� E��3E� �E�}�E���E�3E�` E�� E�	�E�`�E��3E��E�L�E��3E���E�1�E�w3E��fG�O�?���G�O�G�O�G�O�G�O�?���G�O�?�ffG�O�G�O�?�33G�O�?���?�ff@   @ff@��@��@&ff@9��@Fff@S33@l��@�  @�ff@�33@���@���@�33@�33@ə�@ٙ�@�ff@�ffA��A  AffA��A��A$��A+33A4��A<��AC33AI��AS33AY��A^ffAh  AnffAvffA|��A�ffA�ffA�ffA���A�ffA�ffA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444141441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ %@ �@ {@ �@ ""@ (G@ /�@ 6�@ >@ FQ@ Q�@ _�@ m�@ z�@ �7@ ��@ ��@ �-@ ��@ ��@ ��@ �@ ��@@�@g@,`@9X@G�@V@b�@o�@}�@��@�H@��@��@�>@��@��@�4@�,@�@*@#�@0x@>@K@Yn@g�@t�@��@�\@�@��@��@�J@Ӡ@�@��@�E@
=@�@%�@1�@@�@O�@\�@i�@x�@��@�@�z@�r@�@�@׹@�`@�Y@  @�@[@+@7�@E�@Q=@^5@oF@|?@��@��@��@��@��@�|@܀@��@��@�@o@�@-�@:�@FQ@UU@c�@qS@�W@��@��@��@�9@@�7@��@�@��@%@{@""@0x@>@Lu@X�@e	@r�@��@��@�@��@��@Ĝ@��@�H@�L@��@�@6@&;@4�@B�@O0@[z@j@y�@��@��@�@�@�^@ȴ@�h@�@��@ �@�@�@)�@5�@C�@Q�@_�@k�@z�@��@��@�(@��@�&@��@�t@�@��@	�@	o@	g@	,`@	9X@	I@	V@	bN@	r@	~�@	��@	��@	��@	��@	��@	є@	�/@	�4@	�9@
�@
�@
"�@
1'@
=q@
I�@
X�@
g@
v@
��@
��@
�U@
�Y@
��@
�@
խ@
�H@
�@
�9@�@6@%�@3�@A�@N�@\)@i�@ww@�@��@�m@�@�k@�@׹@�@�e@^@�@[@*S@5?@C�@Q�@`B@m:@y�@��@��@�5@�~@��@�@�#@�y@�q@�@�@ @-@8�@H]@T�@`B@��@1�@z�@��@�@E�@��@�@J@O�@�@Ӡ@�@]�@�(@��@33@z3@��@
�@SI@��@��@&;@l�@��@�,@B8@�7@ψ@{@Wb@�@�@&;@i!@�@�4@-�@n�@�!@�@/�@o�@�~@�@4�@t�@��@�@@0x@r�@��@��@:@|?@��@j@G�@��@��@o@UU@��@׹@[@^5@�m@�@ %�@ g�@ ��@ ��@!/�@!s_@!��@!�e@"7L@"x&@"��@"��@#:�@#x�@#��@#��@$5@@$t�@$��@$�@%1�@%oF@%�Y@%�m@&#�@&`A@&��@&�@'{@'O�@'��@'�@(%@(C�@(�@(��@(��@)7�@)x&@)��@)��@*7L@*v@*��@*�@+1�@+p�@+�r@+�@,0x@,p�@,�-@,�@-8�@-{�@-�w@.@.D�@.��@.�c@/
=@/M�@/�@/��@06@0Z�@0�@0��@1"�@1c�@1��@1��@2#�@2b�@2�z@2��@3 @3^5@3�@3�#@4O@4Z�@4�H@4�@5B@5X�@5�i@5��@6
�@6I@6��@6@7  @7:�@7v�@7��@7�(@8"�@8[z@8�i@8�@8��@94�@9�@:�@:��@;9X@;�a@</�@<��@=UU@=��@>N�@>�@?M�@?�@@�@@��@A�|@B&�@B�#@C4�@C�@D> @D�A@EDD@E�T@FJi@F��@Gz�@G�;@Hv�@IJ@I�@I�E@J�\@Kg@K�@L6�@L��@MF�@M��@NP�@N�O@OV@O܀@P`�@Q�2@S&�@TdZ@U�t@W6@X�p@Y�&@[$/@\}�@]��@_�@`j@aє@c@d|?@e��@g @hX�@i�9@k6@li!@m�J@o�@pr�@q�1@s6@t\)@u�@u�,@v:@v{�@v��@w�@w]�@w�@w�/@x�@x[z@x��@x�@y.l@yi!@y�&@y� @zJi@z�@z��@{j@{Q�@{��@{�O@|#�@|V@|��@|�@},`@}ul@}��@}�@~;e@~�7@~�w@�@Lv@��@܀G�O�@ G�O�G�O�G�O�G�O�@ �G�O�@ vG�O�G�O�@ %G�O�@ �@ �@ 
=@ 
�@ �@ �@ V@ b@ �@ @ �@ �@ B@ �@ 
@  �@ "�@ &;@ '�@ +@ -�@ 1'@ 3�@ 6�@ 9X@ <@ ?}@ B�@ E�@ I�@ M$@ O�@ R�@ V�@ Yn@ [z@ _�@ bN@ e�@ hs@ k�@ oF@ r�@ uk@ y�@ |�@ �G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oA�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A�`BAύPA�VAάA�l�A�r�A�JA̡�A˾wA��`Aʕ�A�ZA�z�A��AēuA�%A�|�A��7A��yA�I�A�ƨA��A���A�&�A���A��A��A�=qA�5?A���A�I�A���A�XA��HA��A�
=A�E�A�z�A�$�A���A�/A�9XA��A���A��A�z�A��yA�ffA�ffA�(�A��9A�jA�VA��A���A���A�9XA�oA��A��
A��TA���A�|�A�jA�A���A�+A��HA��7A�E�A�5?A��9A�5?A�M�A��A��HA��A��;A���A�bA��A���A��A�^5A�Q�A��A���A��A��HA�z�A�-A�ȴA��\A��A�ƨA���A��A��hA��DA�A{"�AyAw��Au�AtZArAn�/Am%Aj�DAh=qAgx�Ag�Af��Ab��AbVAb �Ab  AahsA]x�A[��A[�AZ�AW�hASO�AQAQO�AO�#AN�ANv�AN{AM�hAL=qAJ��AHr�AGoAF��AF�AFȴAFjAD�/ACVA@r�A?A?&�A>�A=�#A=t�A=\)A<��A<�\A<bA;��A:��A:$�A9A9S�A8��A77LA5XA3�A2�A1��A0n�A/"�A.jA-��A,��A+t�A*�A(��A%�
A#�A"ĜA ffA��A�TA�FAI�A�A�RAr�A-A�;A�^Ap�A�A�`A1A��A�#A��A��At�A�A~�A�A��AS�A�
AO�AƨA7LA
1'A	�;A	�FA	+AI�Ax�A~�A?}A�#A\)A@��y@�^5@���@�o@�&�@��@���@��#@�7L@���@�bN@�P@�33@��@�7@�@�+@��@�v�@�O�@�"�@噚@���@��@��`@߶F@�/@�(�@�S�@ȴ9@�b@�|�@�"�@��@��-@��@�X@�o@���@��@�`B@��@�"�@�J@��@�S�@��\@�p�@��
@�S�@��@�@�x�@��/@��u@��@�o@��@�b@���@�E�@���@��F@��/@�z�@���@�O�@�%@� �@��@�M�@�n�@�@�%@��@���@���@���@+@|�D@z�H@yG�@x  @v@up�@r��@p��@o+@lZ@j�\@jJ@f��@c��@a��@`A�@^ȴ@\�@Z�\@X�9@W|�@W|�@Tj@S"�@Q��@Q&�@M�T@L�@K��@J�!@J~�@I7L@GK�@E��@D�@C�F@A��@@��@?�;@?K�@?K�@?K�@>ȴ@=��@;33@:�\@:-@9��@9%@8��@8Q�@5�@4�@4�@3��@3"�@2J@1��@1hs@0��@/�;@.�y@.$�@,z�@+�@*��@)��@)�7@(��@&��@$��@$Z@#�m@#�F@"=q@!hs@ ��@ ��@ bN@�w@E�@�@�@j@�
@dZ@�!@��@x�@%@��@�9@  @��@E�@��@p�@I�@�@33@��@�7@�@r�@  @;d@�@ff@�@1@
��@
M�@	�#@	x�@	X@��@��@l�@+@�+@�@��@%?�|�?�dZ?���?�ȴ?���?�hs?��?�p�?�ƨ?��#?�r�?�+?�`B?�33?��?��`?�v�?�/?ۅ?��#?�7L?׮?�?Լj?��?�n�?��?�%?�bN?��?��?͑h?�V?�(�?�?�=q?�7L?�r�?��?�l�?�?�z�?���?�G�?���?�|�?��?��?�/?��?�I�?�1?��?�dZ?�~�?���?�=q?�^5?�^5?�~�?��H?�C�?�dZ?�1?��D?�/?���?�{?�5??�V?�V?���?�v�?��R?��?��?��?�;d?�\)?�|�?�|�?���?��w?��;?�  ?� �?�A�?�bN?���?�Ĝ?��`?�%?�&�?�hs?��7?���?���?�J?�-?�M�?�n�?\?°!?��?�o?�33A�VA�oA�oA�{A�bA�{A�VA�VA��A�bA�bA�{A�oA�oA�oA�oA�bA�bA�VA�bA�{A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A�{A��A��A��A��A��A��A��A��A��A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A�oA�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A�oA��A�`BAύPA�VAάA�l�A�r�A�JA̡�A˾wA��`Aʕ�A�ZA�z�A��AēuA�%A�|�A��7A��yA�I�A�ƨA��A���A�&�A���A��A��A�=qA�5?A���A�I�A���A�XA��HA��A�
=A�E�A�z�A�$�A���A�/A�9XA��A���A��A�z�A��yA�ffA�ffA�(�A��9A�jA�VA��A���A���A�9XA�oA��A��
A��TA���A�|�A�jA�A���A�+A��HA��7A�E�A�5?A��9A�5?A�M�A��A��HA��A��;A���A�bA��A���A��A�^5A�Q�A��A���A��A��HA�z�A�-A�ȴA��\A��A�ƨA���A��A��hA��DA�A{"�AyAw��Au�AtZArAn�/Am%Aj�DAh=qAgx�Ag�Af��Ab��AbVAb �Ab  AahsA]x�A[��A[�AZ�AW�hASO�AQAQO�AO�#AN�ANv�AN{AM�hAL=qAJ��AHr�AGoAF��AF�AFȴAFjAD�/ACVA@r�A?A?&�A>�A=�#A=t�A=\)A<��A<�\A<bA;��A:��A:$�A9A9S�A8��A77LA5XA3�A2�A1��A0n�A/"�A.jA-��A,��A+t�A*�A(��A%�
A#�A"ĜA ffA��A�TA�FAI�A�A�RAr�A-A�;A�^Ap�A�A�`A1A��A�#A��A��At�A�A~�A�A��AS�A�
AO�AƨA7LA
1'A	�;A	�FA	+AI�Ax�A~�A?}A�#A\)A@��y@�^5@���@�o@�&�@��@���@��#@�7L@���@�bN@�P@�33@��@�7@�@�+@��@�v�@�O�@�"�@噚@���@��@��`@߶F@�/@�(�@�S�@ȴ9@�b@�|�@�"�@��@��-@��@�X@�o@���@��@�`B@��@�"�@�J@��@�S�@��\@�p�@��
@�S�@��@�@�x�@��/@��u@��@�o@��@�b@���@�E�@���@��F@��/@�z�@���@�O�@�%@� �@��@�M�@�n�@�@�%@��@���@���@���@+@|�D@z�H@yG�@x  @v@up�@r��@p��@o+@lZ@j�\@jJ@f��@c��@a��@`A�@^ȴ@\�@Z�\@X�9@W|�@W|�@Tj@S"�@Q��@Q&�@M�T@L�@K��@J�!@J~�@I7L@GK�@E��@D�@C�F@A��@@��@?�;@?K�@?K�@?K�@>ȴ@=��@;33@:�\@:-@9��@9%@8��@8Q�@5�@4�@4�@3��@3"�@2J@1��@1hs@0��@/�;@.�y@.$�@,z�@+�@*��@)��@)�7@(��@&��@$��@$Z@#�m@#�F@"=q@!hs@ ��@ ��@ bN@�w@E�@�@�@j@�
@dZ@�!@��@x�@%@��@�9@  @��@E�@��@p�@I�@�@33@��@�7@�@r�@  @;d@�@ff@�@1@
��@
M�@	�#@	x�@	X@��@��@l�@+@�+@�@��@%?�|�?�dZ?���?�ȴ?���?�hs?��?�p�?�ƨ?��#?�r�?�+?�`B?�33?��?��`?�v�?�/?ۅ?��#?�7L?׮?�?Լj?��?�n�?��?�%?�bN?��?��?͑h?�V?�(�?�?�=q?�7L?�r�?��?�l�?�?�z�?���?�G�?���?�|�?��?��?�/?��?�I�?�1?��?�dZ?�~�?���?�=q?�^5?�^5?�~�?��H?�C�?�dZ?�1?��D?�/?���?�{?�5??�V?�V?���?�v�?��R?��?��?��?�;d?�\)?�|�?�|�?���?��w?��;?�  ?� �?�A�?�bN?���?�Ĝ?��`?�%?�&�?�hs?��7?���?���?�J?�-?�M�?�n�?\?°!?��?�o?�33A�VA�oA�oA�{A�bA�{A�VA�VA��A�bA�bA�{A�oA�oA�oA�oA�bA�bA�VA�bA�{A�{A�{A�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A�{A��A��A��A��A��A��A��A��A��A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuBuBuBuBuBoBuBuBuBuBuBuBuB{B{B�B#�B��B
=B �B!�B(�B5?B33B;dBG�BYBXBP�B>wBN�BR�Bn�B^5B_;Be`Bk�B~�B~�B{�Bu�Bo�BgmBffBz�B�VB�\B�PB�bB�VB�=B�\B��B��B�B�B��B��B��B��B��B��B��B�oB� B|�By�Bu�Bt�By�B|�B�BjBm�BgmBbNBH�B8RB5?B2-B1'B)�B�B�BhBDB+B��B��B�yB�B��B�qB�B�VB�%B|�Bs�BiyBZBD�B:^B-B{BDB
��B
��B
�B
�B
��B
ɺB
B
�wB
�B
��B
�hB
gmB
[#B
J�B
E�B
6FB
/B
"�B
bB
	7B	�B	�B	�fB	�`B	�#B	ȴB	ŢB	ÖB	��B	�LB	��B	��B	��B	�\B	z�B	dZB	_;B	YB	O�B	I�B	G�B	D�B	?}B	:^B	6FB	+B	)�B	'�B	%�B	$�B	�B	�B	oB	
=B	1B	B��B��B��B��B��B��B�B�B�B�B�yB�mB�TB�/B��B��B��BŢB�}B�RB�?B�9B��B��B��B�7Bu�Bn�BhsB\)BR�BT�BR�BL�BL�BJ�BH�BF�BE�BC�BC�BB�BA�B?}B=qB;dB<jBB�BC�BD�BG�BG�BH�BD�BE�B@�BA�BA�B@�B@�B?}B>wB>wB<jB;dB7LB:^B8RB0!B1'B/B+B(�B(�B%�B&�B)�B'�B#�B#�B#�B#�B%�B0!B1'B2-B5?B49B1'B1'B-B0!B1'B2-B0!B:^B9XB;dBA�BffB�oB�%B��B�RBÖB��B��B��B�`B�B	VB	�B	 �B	&�B	=qB	G�B	XB	k�B	o�B	s�B	v�B	�B	�=B	�\B	��B	��B	��B	��B	��B	�B	�9B	�?B	�FB	�qB	�}B	�jB	�-B	�jB	��B	ǮB	�fB	�B	�B	��B	��B	��B	��B	��B
B
B
%B
%B
	7B

=B
VB
bB
hB
oB
{B
uB
�B
�B
�B
�B
 �B
!�B
$�B
%�B
&�B
'�B
+B
,B
-B
.B
2-B
2-B
33B
49B
49B
5?B
8RB
9XB
:^B
;dB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
A�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
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
P�B
P�B
R�B
R�B
R�B
T�B
S�B
VB
W
B
YB
YB
ZB
ZB
[#B
]/B
]/B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
hsB
hsB
iyB
jB
jB
k�B
jB
l�B
m�B
m�B
n�B
o�B
o�B
q�B
s�B
t�B
u�B
u�B
u�B
v�B
v�B
w�B
w�B
x�B
x�B
z�B
z�B
� B
�B
�B
�+B
�+B
�7B
�=B
�JB
�JB
�PB
�VB
�bB
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
��B
�B
�B
�B
�B
�'B
�-B
�-B
�3B
�9B
�?B
�?B
�?B
�FB
�LB
�LB
�RB
�XB
�^B
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�^B
�jB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
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
�dBuBoB{BuBhBoBoB{BuBuBuBoBuBuBuBuBuBuBuB{BuBuBuBuBuBuBuBuBuBuBoBuBoBuBuB{BuBuBuBoBuBoBuBoBuBuBoBuB{BuBuBuBuBuBuBuBuB{B{BuG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              BhBhBhBhBhBbBhBhBhBhBhBhBhBoBoB�B!�B��B1B�B�B&�B33B1'B9XBE�BW
BVBN�B<jBL�BP�Bl�B\)B]/BcTBiyB|�B|�By�Bs�Bm�Be`BdZBx�B�JB�PB�DB�VB�JB�1B�PB��B��B�B�B��B��B��B��B��B��B��B�bB}�Bz�Bw�Bs�Br�Bw�Bz�B~�BhsBk�Be`B`BBF�B6FB33B0!B/B'�B�B{B\B	7BB��B�B�mB�BɺB�dB�B�JB�Bz�Bq�BgmBXBB�B8RB+BoB	7B
��B
��B
�B
�sB
��B
ǮB
��B
�jB
�B
��B
�\B
e`B
YB
H�B
C�B
49B
-B
 �B
VB
+B	�B	�yB	�ZB	�TB	�B	ƨB	ÖB	��B	�}B	�?B	��B	��B	�uB	�PB	x�B	bNB	]/B	W
B	M�B	G�B	E�B	B�B	=qB	8RB	49B	(�B	'�B	%�B	#�B	#�B	�B	�B	bB		7B	+B	B��B��B��B��B��B��B�B�B�B�B�sB�fB�NB�)B��B��B��BĜB�wB�LB�9B�3B��B��B��B�1Bt�Bm�BgmB[#BQ�BS�BQ�BK�BK�BI�BG�BE�BD�BB�BB�BA�B@�B>wB<jB:^B;dBA�BB�BC�BF�BF�BG�BC�BD�B?}B@�B@�B?}B?}B>wB=qB=qB;dB:^B6FB9XB7LB/B0!B.B)�B'�B'�B$�B%�B(�B&�B"�B"�B"�B"�B$�B/B0!B1'B49B33B0!B0!B,B/B0!B1'B/B9XB8RB:^B@�Be`B�hB�B��B�LBB��BɺB��B�ZB�B	PB	�B	�B	%�B	<jB	F�B	W
B	jB	n�B	r�B	u�B	� B	�7B	�VB	��B	��B	��B	��B	��B	�B	�3B	�9B	�?B	�jB	�wB	�dB	�'B	�dB	�}B	ƨB	�`B	�yB	�B	�B	��B	��B	��B	��B
B
B
B
B
1B
	7B
PB
\B
bB
hB
uB
oB
�B
�B
�B
�B
�B
 �B
#�B
%�B
&�B
'�B
+B
,B
-B
.B
2-B
2-B
33B
49B
49B
5?B
8RB
9XB
:^B
;dB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
A�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
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
P�B
P�B
R�B
R�B
R�B
T�B
S�B
VB
W
B
YB
YB
ZB
ZB
[#B
]/B
]/B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
ffB
gmB
hsB
hsB
iyB
jB
jB
k�B
jB
l�B
m�B
m�B
n�B
o�B
o�B
q�B
s�B
t�B
u�B
u�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
{�B
{�B
�B
�B
�B
�1B
�1B
�=B
�DB
�PB
�PB
�VB
�\B
�hB
�uB
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
��B
��B
�B
�B
�B
�'B
�'B
�3B
�9B
�9B
�?B
�FB
�LB
�LB
�LB
�RB
�XB
�XB
�dB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�qB
�}B
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
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
�}BhBbBoBhB\BbBbBoBhBhBhBbBhBhBhBhBhBhBhBoBhBhBhBhBhBhBhBhBhBhBbBhBbBhBhBoBhBhBhBbBhBbBhBbBhBhBbBhBoBhBhBhBhBhBhBhBhBoBoBhG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808080703202021061413522620210614135226202106141746322021061417463220210614174632201808080703202021061413522620210614135226202106141746322021061417463220210614174632PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018080807032020180808070320  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080807032020180808070320QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080807032020180808070320QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015120210722160151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                