CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:58Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ´   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    $   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � $Argo profile    3.1 1.2 19500101000000  20180724220258  20210722161418  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�kF)��@�kF)��11  @�kDDE0@�kDDE0@*q"���S@*q"���S�cK����cK���11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?fff?�33@Fff@�33@�ff@�ff@�  A   A��A$��A>ffA^ffA���A���A���A���A�  A�  AᙚA���B   B��BffB��B ��B(ffB0ffB8��B@ffBH��BP��BX��B`��Bi33Bp��Bx��B�ffB�ffB�33B�33B�  B���B���B�  B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�33B�33B�ffB�ffB���B���B�33B�33B���B뙚BB�ffB�33B�33C �C�C�C�fC33C
�C  C33CL�C33C  C�C�C�fC  C  C��C"  C$33C&�C'�fC*33C,  C-��C0�C233C4�C5��C7�fC9�fC<  C>�C@�CB33CD33CFL�CH�CI��CK�fCN  CP  CR  CT�CV�CX�CZ�C\�C^�C`  Cb�Cd  Cf�Ch�Cj33Cl�Cn33Cp33Cr33Ct�Cv33Cx  Cz  C|  C~  C�fC��C�33C��C��C��C�  C��3C�  C��C��3C��C�&fC��C�  C��fC�  C��C��3C��fC��3C��C��3C�  C�  C��3C�  C��C��C�  C��3C��C�&fC��C��C�  C��3C�  C�&fC��C��C�  C��3C��fC�  C�&fC��C��C��C�  C�  C�  C�  C��3C��3C��fC��fC��C��C��C��C��3C��3C��fC��3C��C�33C��C��C�  C��3C��C�&fC��C��C�  C�  C��3C�  C��3C��3C�  C��3C�  C�  C��fC��3C��fC��C�&fC�&fC�&fC�&fC��C��C�&fC��C��C�&fC��C��C��C��C��C��C��C�  C��C�  C�  C�  C�  C��3C��C�&fC��C��C��C�  C��3C��C�&fC��3C�  C�@ C�&fC�&fC��3C��fD �3DfDy�DfD,�D
y�D�fDffDٚDY�D��Ds3DfD� D!&fD#� D&` D)fD+� D.@ D0�fD3L�D5�3D8` D:�fD=9�D?� DB�DDl�DF�3DH�3DK9�DMs3DO�3DQ�3DT33DVy�DX�3DZ��D]9�D_y�Da� Dc��Df9�Dhs3Dj��Dm  Do@ Dq� Ds� Dv�DxY�Dz��D|��DfD�� D���D�	�D�9�D�l�D���D��3D�  D�VfD��3D�� D��fD��fD��D�9�D�\�D�s3D���D���D��fD���D��3D��3D���D��fD�  D�fD��D��D�&fD�6fD�C3D�I�D�S3D�c3D�s3D��3D�� D���D��3D��fD�� D��3D���D�3D�#3D�@ D�P D�c3D�vfD���D��3D���D�� D���D�� D�� D��fD���D���D��3D³3Dé�DĠ Dœ3DƉ�Dǀ D�y�D�s3D�i�D�\�D�P D�@ D�33D�#3D�3D��D�	�D�3D���D���D��3D��fD�ٚD��fD�ɚD�� D۶fDܹ�Dݼ�D޹�D߳3D�fD�fD� D� D䩚D�fD� D��D��D�fD� D��D쉚D퉚D�fD�y�D�p D�i�D�\�D�S3D�L�D�@ D�6fD�33D�0 D�0 D�#3D�3D�� D��3D�� E [3EK3E�fEfE3E|�E�3E�fE
S3EK3E�3E0 E&fE�fE��EfE�E�fE��EfE E� E�fE��Es3Eh E ��E",�E#3E$l�E%��E&��E(8 E)p E*�fE+�3E-NfE.nfE/��E13E2�E3��E4� E6  E7;3E8Y�E9� E;3E<�E?!�EB��EE� EH�fEK�3EN� ER,�EUfEX;3E[t�E^nfEay�Ed��Eg��Ek�En)�En� Eo4�Eo��Ep��EqH Eq��Er�3Es�EsњEtS3Eu Eu�fEvNfEvɚEw��Ew�fEx��Eyp Ey� ?fff?fff?L��?L��?L��?333?L��?L��?L��?fff?fff?fff?fff?fff?L��?fff?fff?L��?L��?L��?fff?fff?fff?fff?L��?�  ?���?���?�ff?�ff?���?ٙ�?�33@ff@33@&ff@333@S33@`  @s33@�33@�  @���@���@�  @���@�  @�ff@�ff@�33A   A  A��AffAffA$��A,��A333A;33AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441444441444414444114141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?�33@��@fff@�33@�ff@�ff@�  A  A��A,��AFffAfffA���A���A���A���A�  A�  A噚A���B  B	��BffB��B"��B*ffB2ffB:��BBffBJ��BR��BZ��Bb��Bk33Br��Bz��B�ffB�ffB�33B�33B�  B���B���B�  B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�33B�33B�ffB�ffB���B���B�33B�33B���B왚B�B�ffB�33B�33C ��C��C��CffC�3C
��C� C�3C��C�3C� C��C��CffC� C� C L�C"� C$�3C&��C(ffC*�3C,� C.L�C0��C2�3C4��C6L�C8ffC:ffC<� C>��C@��CB�3CD�3CF��CH��CJL�CLffCN� CP� CR� CT��CV��CX��CZ��C\��C^��C`� Cb��Cd� Cf��Ch��Cj�3Cl��Cn�3Cp�3Cr�3Ct��Cv�3Cx� Cz� C|� C~� C�33C�L�C�s3C�Y�C�Y�C�L�C�@ C�33C�@ C�L�C�33C�L�C�ffC�L�C�@ C�&fC�@ C�L�C�33C�&fC�33C�L�C�33C�@ C�@ C�33C�@ C�Y�C�L�C�@ C�33C�L�C�ffC�Y�C�L�C�@ C�33C�@ C�ffC�Y�C�L�C�@ C�33C�&fC�@ C�ffC�Y�C�L�C�L�C�@ C�@ C�@ C�@ C�33C�33C�&fC�&fC�L�C�Y�C�Y�C�L�C�33C�33C�&fC�33C�L�C�s3C�Y�C�L�C�@ C�33C�L�C�ffC�Y�C�Y�C�@ C�@ C�33C�@ C�33C�33C�@ C�33C�@ C�@ C�&fC�33C�&fC�L�C�ffC�ffC�ffC�ffC�L�C�Y�C�ffC�Y�C�Y�C�ffC�Y�C�L�C�L�C�Y�C�L�C�Y�C�L�C�@ C�L�C�@ C�@ C�@ C�@ C�33C�L�C�ffC�Y�C�Y�C�L�C�@ C�33C�L�C�ffC�33C�@ C�� C�ffC�ffC�33D 3D �3D&fD��D&fDL�D
��DfD�fD��Dy�D�D�3D&fD� D!FfD#� D&� D)&fD+� D.` D0�fD3l�D5�3D8� D:�fD=Y�D?� DB,�DD��DF�3DI3DKY�DM�3DO�3DR3DTS3DV��DX�3D[�D]Y�D_��Da� Dd�DfY�Dh�3DjٚDm  Do` Dq� Ds� Dv,�Dxy�Dz��D|��D&fD�� D���D��D�I�D�|�D���D��3D�0 D�ffD��3D�� D��fD�fD�)�D�I�D�l�D��3D���D���D��fD���D��3D��3D���D�fD� D�fD��D�)�D�6fD�FfD�S3D�Y�D�c3D�s3D��3D��3D�� D���D��3D��fD�� D��3D�	�D�#3D�33D�P D�` D�s3D��fD���D��3D���D�� D���D�� D�� D��fD���D���D��3D��3Dù�Dİ Dţ3Dƙ�Dǐ Dȉ�DɃ3D�y�D�l�D�` D�P D�C3D�33D�#3D��D��D�3D�	�D�	�D�3D��fD��D��fD�ٚD�� D��fD�ɚD���D�ɚD��3D��fD��fD�� D�� D乚D�fD� D��D��D�fD� D��D왚D홚D�fDD�� D�y�D�l�D�c3D�\�D�P D�FfD�C3D�@ D�@ D�33D�3D�  D��3D�� E c3ES3E�fE&fE3E��E�3E�fE
[3ES3E�3E8 E.fE�fE��EfE�E�fE��EfE E� EfE�E{3Ep E ��E"4�E#3E$t�E%��E'�E(@ E)x E*�fE+�3E-VfE.vfE/��E13E2$�E3��E4� E6( E7C3E8a�E9� E;3E<$�E?)�EB��EE� EH�fEK�3EN� ER4�EU&fEXC3E[|�E^vfEa��Ed��Eg��Ek	�En1�En� Eo<�Ep�Ep��EqP Eq��Er�3Es�EsٚEt[3Eu Eu�fEvVfEvњEw��ExfEx��Eyx Ey� G�O�G�O�G�O�G�O�G�O�?���G�O�G�O�?�ffG�O�G�O�G�O�G�O�G�O�?�ffG�O�G�O�G�O�G�O�?�ffG�O�G�O�G�O�G�O�?�ff?�  G�O�?���G�O�?�ff@ff@��@��@&ff@333@Fff@S33@s33@�  @���@�33@�  @���@���@�  @���@�  @�ff@�ffA��A  A  A��AffA&ffA,��A4��A;33AC33AI��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441441444441444414444114141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ %@ �@ *@ �@ #�@ *S@ /�@ 6�@ >@ FQ@ Q=@ ^�@ m�@ {�@ �7@ ��@ ��@ �~@ ��@ �|@ �t@ �m@ �q@�@o@g@-@;d@H]@V�@dZ@r@�@��@��@��@�F@��@��@ލ@�@�~@%@{@"�@0x@>@K�@X�@ff@t@�d@�@��@�Y@�@�W@��@��@�@@�E@
�@6@$.@1�@B8@O0@\�@j@x&@��@�@��@��@��@��@�@�@�Y@ �@V@�@(�@6�@B�@Q�@`�@m�@z3@��@�0@�z@�-@��@�|@�@�m@�@j@�@g@-�@;d@I�@V@a�@o�@~K@��@��@��@��@�>@��@ލ@�4@�,@�@{@"�@0x@>�@K�@Z@g�@uk@�d@��@�@��@�R@��@��@��@�@��@�@�@%�@2�@@�@O0@[z@j@y�@��@��@�@�@�k@ȴ@խ@�@�@�Q@�@O@(G@6�@E�@R�@_�@l�@{�@��@��@��@�~@�w@��@܀@�y@�q@	j@	b@	[@	,`@	<@	I@	V@	c�@	p�@	~K@	��@	��@	��@	�9@	�2@	��@	ލ@	��@	��@
�@
�@
!s@
.l@
<�@
K�@
[z@
g�@
t�@
��@
��@
��@
��@
��@
�W@
Ӡ@
�H@
�@@
��@	�@6@%�@2�@@�@N�@Z�@i!@v@��@��@�y@�!@��@�@�h@�@�@^@�@�@)�@7L@E�@R�@`�@m�@z�@�7@�0@��@�~@�&@�@�#@�(@� @�@�@�@+�@:�@I�@T�@b�@t@�W@��@��@��@��@�>@ψ@K�@�|@�J@�@K�@��@��@B@^5@��@�@0x@ww@�&@�@N�@�0@�#@ @e	@��@�4@/@p�@��@�@1�@oF@�f@�(@'�@e	@�y@��@[@[z@��@�\@{@Q=@��@�o@	�@G�@�@@  @>�@}�@�@�@3�@t@�9@�e@5@@v�@�^@�E@@�@�@�>@j@B8@�@��@�9@9X@t�@��@�(@ #�@ ^�@ �H@ �O@!�@!E�@!~K@!�F@!�@@"'�@"`�@"��@"�O@#J@#D�@#~�@#�@#�@$,`@$e�@$��@$�h@%{@%O0@%��@%ƨ@& �@&=q@&ww@&�-@&��@''�@'`A@'��@'�7@(	�@(@�@(ww@(�@(�T@)�@)R�@)�7@)��@)�Y@*&;@*Z�@*�\@*Ĝ@*��@+.l@+bN@+�0@+�c@+�E@,0x@,c�@,��@,��@-@-8�@-oF@-��@-�h@.J@.B8@.v@.��@.�;@/�@/M�@/��@/�@/�L@0&�@0\)@0��@0�@0��@133@1i!@1��@1��@2
=@2@,@2v@2��@2�@3�@3K@3�W@3�9@3��@4
@4Q�@4�|@4�j@4�Y@5(�@5\�@5��@5�&@5�@6%�@6��@7*S@7�J@8^�@8�W@9a�@9��@:e�@;�@;n�@<@<��@=�@=�9@>g@>��@?.l@?��@@> @@��@AN�@A��@B��@B�9@C��@Dj@D��@E1�@E�#@F'�@F��@G?}@G��@HK@H�@ILu@I�@Jl�@J�@K�|@K��@L��@M{@M��@N-�@N��@ON�@O��@PB�@Q��@S �@TN�@U��@W�@XDD@Y��@Z�@@\B8@]�z@^�l@`3�@a�@b�(@dD�@e��@e�
@f�@fe	@f��@f�Y@g+@g�@g��@h�@h>�@h�\@h�J@i6@iK�@i�I@iψ@jg@jm:@j�mG�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�@ vG�O�G�O�G�O�G�O�G�O�@ vG�O�G�O�G�O�G�O�@ vG�O�G�O�G�O�G�O�@ v@ �G�O�@ �G�O�@ �@ 
�@ �@ �@ V@ �@ �@ @ �@ �@ �@ �@ �@ !s@ $.@ %�@ (G@ ,`@ -�@ 1'@ 3�@ 6�@ :@ <@ @,@ C�@ FQ@ I�@ Lu@ O�@ R�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aۏ\Aۏ\Aۏ\AۍPAۋDAۅAۏ\Aۇ+Aۉ7AۋDAۍPA�`BA�XA��A��A�ȴAڲ-AڮAڥ�Aڟ�Aڝ�Aڛ�Aڙ�Aڗ�Aڕ�AړuAڏ\Aڏ\AڍPAڍPAڍPAڃA�x�A�jA���A�K�A�(�A�=qA�x�A�S�A�VA��A���A�C�A��;A��wA�VA��A��
A�?}A��A�9XA�/A��-A��!A��
A��;A�|�A���A�G�A�5?A�ffA��A�x�A��9A��9A�bNA�VA�A��wA���A��hA���A���AO�Ay`BAt~�AqS�AlȴAhVAdbNAa"�A^jAZ  AW�AS�AQp�AO�-AMhsAI�FAE33ADz�ADbACoABr�ABVA@�A?S�A>ȴA=�wA;�A;;dA:�A8�A8=qA7�#A7�hA6�DA69XA5|�A4�9A3��A3��A3A2Q�A1��A1��A1S�A0�9A0ffA/C�A//A/�A.(�A-�PA-VA,�RA,�+A,Q�A+�;A+G�A*�\A*1'A)��A(�+A'�A&�9A%�A$�jA#�A#�A"ffA!A!�A ��A ��A Q�A �A��AdZA%AĜAI�A��Ax�A33A��A��An�AA��A�Az�A�mA�7A%A��A�+AZA  A�#A�;A�^A&�A�A��Av�A-A��A?}A
=A�`A��A(�A�hA&�A��A��AA�A��A�^Ax�Al�A��A�
A`BA%A��A(�AA\)A"�A�A�9A�\An�A�A��AC�A/A&�A�A
ȴA
n�A
A	��A	�A	�A�`A�9Ar�A$�A��A��AM�A(�A��A�^A`BA+A�yA��A~�AZA �AA�RAr�AjAI�A�AS�A7LA ��A �yA ��A ff@��F@���@�V@��T@�7L@�1'@��
@�;d@�hs@���@�j@� �@�K�@�=q@���@��@� �@��@���@�F@�z�@�^5@�@�S�@�Z@��@�"�@щ7@Ϯ@�&�@�  @�7L@�E�@�Z@��@��@��@��F@��@���@�-@���@�I�@���@��7@��@��!@��#@�;d@�A�@�n�@��m@�ȴ@���@�Q�@��
@�n�@�&�@�1@�5?@�G�@���@��+@���@�Q�@��@��@���@�l�@���@�&�@�j@�|�@�5?@�G�@�1@��@�{@�r�@�S�@���@���@���@��@}�@{�m@z��@xr�@w�@u`B@sƨ@r=q@p��@o�@lj@j�!@h�`@g\)@f{@dZ@b�@a�7@_�;@^�@]`B@\�j@Zn�@X��@W\)@Vff@T�@S��@R��@Q%@Ol�@M��@K�F@J-@I7L@G��@F��@D�@D�@B��@A�#@A�@@b@>�y@>5?@<��@;��@:��@9��@9�@7�@7\)@6V@5`B@3�
@3o@2M�@1G�@0Q�@/��@.v�@-@,��@+��@*�\@)x�@(�9@'�w@&��@%@$��@$�@#"�@"=q@!�@ �u@\)@
=@��@p�@�@t�@�@��@��@G�@Ĝ@;d@��@�-@V@Z@��@@��@�#@&�@bN@b@;d@��@�h@�@z�@��@o@
�\@
-@Ĝ@�P@ff@��@�F@�\@G�@ ��?���?�O�?�"�?�7L?�1'?��+?���?�Z?�t�?�n�?�hs?�bN??��?��?�I�?�dZ?�^5?�x�?���?�1'?���?��y?�ff?��?��/?�9X?�S�?�-?�&�?�A�?�\)?ޗ�?ݲ-?��?�(�?��m?�dZ?�~�?��#?�7L?�K�?Լj?�33?���?� �?��?�V?�j?��H?ɺ^?ȓu?���?�$�?�z�?�-?�  ?��;?�|�?��?��R?�{?��h?�V?��?���?��D?�1?�ƨ?���?��?�?�?���?�^5?��Aۏ\AۍPAۍPAۍPAۏ\Aۏ\AۍPAۍPAۋDAۍPAۍPAۏ\AۑhAۏ\AۑhAۋDAۋDAۍPAۍPAۏ\Aۏ\AۍPAۍPAۏ\Aۏ\AۍPAۑhAۑhAۏ\Aۏ\AۍPAۑhAۏ\AۍPAۍPAۍPAۑhAۏ\Aۏ\AۍPAۏ\AۍPAۍPAۋDAۉ7AۅAۅAۍPAۏ\AۍPAۃAۃAۉ7Aۉ7AۋDAۋDAۋDAۍPAۍPAۋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Aۏ\Aۏ\Aۏ\AۍPAۋDAۅAۏ\Aۇ+Aۉ7AۋDAۍPA�`BA�XA��A��A�ȴAڲ-AڮAڥ�Aڟ�Aڝ�Aڛ�Aڙ�Aڗ�Aڕ�AړuAڏ\Aڏ\AڍPAڍPAڍPAڃA�x�A�jA���A�K�A�(�A�=qA�x�A�S�A�VA��A���A�C�A��;A��wA�VA��A��
A�?}A��A�9XA�/A��-A��!A��
A��;A�|�A���A�G�A�5?A�ffA��A�x�A��9A��9A�bNA�VA�A��wA���A��hA���A���AO�Ay`BAt~�AqS�AlȴAhVAdbNAa"�A^jAZ  AW�AS�AQp�AO�-AMhsAI�FAE33ADz�ADbACoABr�ABVA@�A?S�A>ȴA=�wA;�A;;dA:�A8�A8=qA7�#A7�hA6�DA69XA5|�A4�9A3��A3��A3A2Q�A1��A1��A1S�A0�9A0ffA/C�A//A/�A.(�A-�PA-VA,�RA,�+A,Q�A+�;A+G�A*�\A*1'A)��A(�+A'�A&�9A%�A$�jA#�A#�A"ffA!A!�A ��A ��A Q�A �A��AdZA%AĜAI�A��Ax�A33A��A��An�AA��A�Az�A�mA�7A%A��A�+AZA  A�#A�;A�^A&�A�A��Av�A-A��A?}A
=A�`A��A(�A�hA&�A��A��AA�A��A�^Ax�Al�A��A�
A`BA%A��A(�AA\)A"�A�A�9A�\An�A�A��AC�A/A&�A�A
ȴA
n�A
A	��A	�A	�A�`A�9Ar�A$�A��A��AM�A(�A��A�^A`BA+A�yA��A~�AZA �AA�RAr�AjAI�A�AS�A7LA ��A �yA ��A ff@��F@���@�V@��T@�7L@�1'@��
@�;d@�hs@���@�j@� �@�K�@�=q@���@��@� �@��@���@�F@�z�@�^5@�@�S�@�Z@��@�"�@щ7@Ϯ@�&�@�  @�7L@�E�@�Z@��@��@��@��F@��@���@�-@���@�I�@���@��7@��@��!@��#@�;d@�A�@�n�@��m@�ȴ@���@�Q�@��
@�n�@�&�@�1@�5?@�G�@���@��+@���@�Q�@��@��@���@�l�@���@�&�@�j@�|�@�5?@�G�@�1@��@�{@�r�@�S�@���@���@���@��@}�@{�m@z��@xr�@w�@u`B@sƨ@r=q@p��@o�@lj@j�!@h�`@g\)@f{@dZ@b�@a�7@_�;@^�@]`B@\�j@Zn�@X��@W\)@Vff@T�@S��@R��@Q%@Ol�@M��@K�F@J-@I7L@G��@F��@D�@D�@B��@A�#@A�@@b@>�y@>5?@<��@;��@:��@9��@9�@7�@7\)@6V@5`B@3�
@3o@2M�@1G�@0Q�@/��@.v�@-@,��@+��@*�\@)x�@(�9@'�w@&��@%@$��@$�@#"�@"=q@!�@ �u@\)@
=@��@p�@�@t�@�@��@��@G�@Ĝ@;d@��@�-@V@Z@��@@��@�#@&�@bN@b@;d@��@�h@�@z�@��@o@
�\@
-@Ĝ@�P@ff@��@�F@�\@G�@ ��?���?�O�?�"�?�7L?�1'?��+?���?�Z?�t�?�n�?�hs?�bN??��?��?�I�?�dZ?�^5?�x�?���?�1'?���?��y?�ff?��?��/?�9X?�S�?�-?�&�?�A�?�\)?ޗ�?ݲ-?��?�(�?��m?�dZ?�~�?��#?�7L?�K�?Լj?�33?���?� �?��?�V?�j?��H?ɺ^?ȓu?���?�$�?�z�?�-?�  ?��;?�|�?��?��R?�{?��h?�V?��?���?��D?�1?�ƨ?���?��?�?�?���?�^5?��Aۏ\AۍPAۍPAۍPAۏ\Aۏ\AۍPAۍPAۋDAۍPAۍPAۏ\AۑhAۏ\AۑhAۋDAۋDAۍPAۍPAۏ\Aۏ\AۍPAۍPAۏ\Aۏ\AۍPAۑhAۑhAۏ\Aۏ\AۍPAۑhAۏ\AۍPAۍPAۍPAۑhAۏ\Aۏ\AۍPAۏ\AۍPAۍPAۋDAۉ7AۅAۅAۍPAۏ\AۍPAۃAۃAۉ7Aۉ7AۋDAۋDAۋDAۍPAۍPAۋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�bB	�bB	�bB	�bB	�\B	�bB	�bB	�\B	�\B	�\B	�\B	�VB	�VB	�PB	�JB	�DB	�DB	�DB	�DB	�=B	�=B	�7B	�7B	�7B	�7B	�7B	�7B	�1B	�1B	�1B	�1B	�1B	�1B	�+B	L�B	0!B	O�B	n�B	��B	ŢB
VB
1'B
W
B
hsB
iyB
`BB
XB
aHB
A�B
,B
.B
$�B
�B
O�B
�{B
��B
��BJ�BXBQ�B'�B
=B
�BB
ɺB
��B
l�B
I�B
-B
bB	�B	��B	��B	�oB	�DB	bNB	H�B	5?B	%�B	�B	JB��B��B�mB�yB�fB��B	
=B	
=B	�B	�B	 �B	49B	>wB	G�B	N�B	N�B	N�B	Q�B	R�B	YB	y�B	�=B	��B	�B	�^B	�}B	B	��B	�B	�B	�HB	�B	��B
+B
oB
�B
)�B
6FB
@�B
>wB
B�B
L�B
M�B
I�B
H�B
O�B
S�B
R�B
Q�B
N�B
K�B
L�B
N�B
O�B
L�B
L�B
L�B
O�B
O�B
M�B
N�B
O�B
N�B
P�B
P�B
O�B
P�B
R�B
S�B
VB
VB
VB
VB
XB
YB
YB
YB
XB
XB
XB
XB
XB
T�B
VB
T�B
S�B
R�B
R�B
S�B
S�B
VB
T�B
S�B
P�B
R�B
T�B
T�B
S�B
P�B
P�B
O�B
P�B
O�B
M�B
L�B
K�B
L�B
L�B
L�B
L�B
N�B
N�B
M�B
K�B
K�B
J�B
K�B
K�B
J�B
I�B
I�B
I�B
H�B
H�B
G�B
G�B
F�B
D�B
D�B
D�B
D�B
C�B
B�B
B�B
B�B
A�B
@�B
@�B
A�B
A�B
@�B
?}B
=qB
;dB
:^B
:^B
9XB
9XB
8RB
8RB
7LB
7LB
9XB
;dB
<jB
:^B
;dB
;dB
:^B
9XB
7LB
8RB
7LB
7LB
7LB
6FB
5?B
49B
49B
2-B
2-B
0!B
2-B
2-B
1'B
0!B
/B
-B
.B
+B
'�B
.B
-B
(�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
{B
oB
�B
hB
bB
VB
VB
\B
\B
VB
VB
\B
\B
\B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
$�B
%�B
'�B
'�B
)�B
)�B
)�B
+B
,B
-B
.B
/B
0!B
1'B
33B
6FB
7LB
6FB
7LB
9XB
:^B
:^B
<jB
<jB
=qB
>wB
@�B
@�B
B�B
C�B
C�B
F�B
G�B
G�B
I�B
H�B
K�B
L�B
L�B
M�B
N�B
O�B
O�B
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
ZB
ZB
[#B
\)B
\)B
]/B
^5B
^5B
_;B
`BB
`BB
aHB
aHB
bNB
aHB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
m�B
n�B
o�B
p�B
p�B
q�B
r�B
s�B
t�B
t�B
t�B
u�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
{�B
|�B
|�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�+B
�1B
�1B
�7B
�=B
�JB
�PB
�PB
�VB
�bB
�hB
�hB
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
�B
�B
�B
�!B
�!B
�'B
�'B
�'B
�3B
�9B
�9B
�9B
�?B
�FB
�LB
�XB
�RB
�RB
�XB
�XB
�^B
�^B
�XB
�^B
�RB
�^B
�XB
�XB
�^B
�XB
�^B
�XB
�dB
�dB
�dB	�bB	�hB	�hB	�bB	�bB	�\B	�bB	�bB	�bB	�bB	�bB	�bB	�\B	�bB	�\B	�bB	�hB	�hB	�bB	�bB	�bB	�bB	�hB	�bB	�bB	�bB	�bB	�\B	�bB	�hB	�bB	�bB	�bB	�bB	�bB	�bB	�\B	�bB	�\B	�bB	�bB	�bB	�\B	�bB	�\B	�\B	�bB	�bB	�\B	�\B	�VB	�bB	�\B	�\B	�\B	�\B	�\B	�\B	�\B	�\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B	�<B	�<B	�=B	�=B	�7B	�>B	�>B	�8B	�9B	�9B	�9B	�4B	�5B	�/B	�*B	�$B	�%B	�&B	�&B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	L�B	0B	O�B	n�B	��B	ŎB
BB
1B
V�B
h`B
igB
`0B
W�B
a6B
AxB
+�B
.B
$�B
�B
O�B
�kB
��B
��BJ�BXBQ�B'�B
1B
�6B
ɮB
��B
l�B
I�B
-B
XB	�B	�yB	��B	�fB	�;B	bEB	H�B	57B	%�B	B	BB��B��B�fB�rB�`B��B	
7B	
8B	�B	�B	 �B	45B	>tB	G�B	N�B	N�B	N�B	Q�B	R�B	YB	y�B	�>B	��B	�B	�aB	��B	B	�B	�	B	�"B	�NB	�B	��B
3B
wB
�B
*B
6PB
@�B
>�B
B�B
L�B
M�B
I�B
H�B
O�B
TB
SB
Q�B
N�B
K�B
L�B
N�B
O�B
L�B
L�B
L�B
O�B
O�B
M�B
N�B
O�B
N�B
P�B
P�B
O�B
P�B
SB
TB
VB
V B
V B
V!B
X.B
Y5B
Y6B
Y6B
X0B
X0B
X1B
X2B
X2B
U!B
V'B
U"B
TB
SB
SB
TB
TB
V+B
U&B
T B
QB
SB
U(B
U)B
T#B
QB
QB
PB
QB
PB
NB
L�B
K�B
L�B
L�B
L�B
L�B
OB
OB
NB
K�B
K�B
J�B
K�B
K�B
J�B
I�B
I�B
I�B
H�B
H�B
G�B
G�B
F�B
D�B
D�B
D�B
D�B
C�B
B�B
B�B
B�B
A�B
@�B
@�B
A�B
A�B
@�B
?�B
=�B
;�B
:�B
:�B
9�B
9�B
8�B
8�B
7�B
7�B
9�B
;�B
<�B
:�B
;�B
;�B
:�B
9�B
7�B
8�B
7�B
7�B
7�B
6�B
5�B
4�B
4�B
2�B
2�B
0vB
2�B
2�B
1~B
0xB
/sB
-fB
.mB
+[B
(JB
.oB
-iB
)RB
#2B
 !B
B
B
B
B
�B
B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
8B
;B
CB
@B
BB
KB
GB
JB
ZB
bB
qB
�B
|B
 �B
�B
 �B
!�B
"�B
#�B
$�B
%�B
&�B
(�B
(�B
*�B
*�B
*�B
+�B
,�B
.B
/B
0B
1!B
2*B
49B
7OB
8XB
7TB
8]B
:lB
;tB
;wB
=�B
=�B
>�B
?�B
A�B
A�B
C�B
D�B
D�B
G�B
H�B
H�B
J�B
I�B
MB
NB
NB
OB
P!B
Q*B
Q-B
R5B
S?B
UMB
UPB
URB
V[B
WcB
XlB
YtB
[�B
[�B
\�B
]�B
]�B
^�B
_�B
_�B
`�B
a�B
a�B
b�B
b�B
c�B
b�B
d�B
e�B
f�B
f�B
g�B
g�B
h B
i	B
jB
kB
kB
kB
k B
l)B
l+B
l-B
m6B
oDB
pMB
qVB
r^B
r`B
siB
tqB
uyB
v�B
v�B
v�B
w�B
x�B
y�B
y�B
y�B
z�B
|�B
|�B
}�B
~�B
~�B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�"B
�B
�-B
�/B
�1B
�4B
�6B
�>B
�FB
�IB
�QB
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
�B
�B
�B
� B
�4B
�9B
�LB
�QB
�XB
�eB
�qB
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
��B
��B
�B
�	B
�B
�B
�"B
�'B
�3B
�@B
�FB
�QB
�`B
�wB
��B
��B
��B
��B
��B
��B
��B
�B
�$B
�3B
�HB
�_B
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
��B
��B
��B
��B
��B
��B
��B	�<B	�BB	�BB	�<B	�<B	�6B	�<B	�<B	�<B	�<B	�<B	�<B	�6B	�<B	�6B	�<B	�BB	�BB	�<B	�<B	�<B	�<B	�BB	�<B	�<B	�<B	�<B	�6B	�<B	�BB	�<B	�<B	�<B	�<B	�=B	�=B	�7B	�=B	�7B	�=B	�=B	�=B	�7B	�=B	�7B	�8B	�>B	�>B	�8B	�8B	�2B	�>B	�8B	�9B	�9B	�9B	�9B	�9B	�9B	�9G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202582021061413572220210614135722202107221611222021072216112220210722161122201807242202582021061413572220210614135722202107221611222021072216112220210722161122PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025820180724220258  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025820180724220258QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025820180724220258QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141820210722161418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                