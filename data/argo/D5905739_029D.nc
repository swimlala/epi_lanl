CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-27T22:01:32Z creation      
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  dL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  t|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ѐ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ՜   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
tArgo profile    3.1 1.2 19500101000000  20180827220132  20210617131501  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�y��`4@�y��`411  @�y��z`@�y��z`@6�;�'@6�;�'�c�~��"��c�~��"�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@Fff@�  @�33@�33@�33A��A  A#33A@  Ac33A�  A�33A���A���A���A�  A�  A�  A�33BffB��B33B!33B)33B133B8��B@ffBG��BPffBXffB_��Bh  Bp��BxffB�  B���B�  B�33B�  B�  B�33B�33B�ffB�  B�33B�33B�33B�  B�33B�ffB�  B�33B�33B̙�B�ffB�ffB�33B�33B�33B�  B���B�ffB�  B�  B�ffB���C �C�fC33CL�C33C
�CL�C�C�fC33CL�CL�C�C�fC33C33C L�C"  C#��C%�fC(  C*  C,33C.  C/��C2  C4�C6L�C8�C9��C<�C>33C@�CB  CD33CF  CG�fCJ33CL�CM�fCP33CR�CS�fCV33CX�CZ  C\�C]�fC`L�Cb�Cd�Cf�Ch�Cj  Cl  CnL�Cp33Cr33Ct�Cv33Cx33Cz�C|33C~�C��C�  C�  C��fC��C�&fC�&fC�  C��C��C��3C��C��C�&fC��C��3C�  C��C�&fC�  C��C��C��3C��3C�  C�  C��C��C��C��C��C��C��C��C��C�&fC��C��C��C�&fC�&fC��C��C�33C�&fC�&fC��C��C��C�  C�  C�  C�  C��C��C�&fC�  C��fC��3C��3C�  C�  C��C��C��C��C�  C��fC��3C��C��C�  C��fC��C��C�  C��fC��C�  C��fC�  C�  C��fC��C�  C��3C��C�  C��3C��C��C�  C��3C��fC�  C��C��C��3C�  C��C��C�&fC�&fC�  C��C��C��3C��3C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��3C��C�  C��fC�&fC��D fD y�D ��D� D�fD
L�D� DS3DٚDy�D�D��D�fDL�D"  D%fD'�3D*�fD-y�D09�D2�3D5��D8FfD:�3D=y�D@fDB��DE�DG� DI�3DL` DN� DQ�DSs3DU� DX@ DZ� D\��D_S3Da��Dd�DfffDh� DkfDmY�Do��Dq��DtFfDv�3Dx�fD{�D|��D` D�� D�3D�FfD�� D��fD���D�0 D�p D��fD��3D�#3D�` D���D�ٚD�fD�S3D���D���D��D��D�L�D�|�D�� D�� D�	�D�0 D�Y�D�|�D��fD���D��3D�fD�<�D�c3D��3D���D�� D��3D��D�L�D��3D��3D��D�3D�C3D�p D��fD���D��D�P D�� D���D��fD�6fD�vfD¶fD��fD�33D�l�DǦfD���D� D�I�D̃3Dͳ3D��3D�3D�I�D�|�DӦfD��3D���D��D�<�D�Y�D�y�Dۓ3Dܳ3D�� D��fD��fD��fD�3D�	�D� D�fD��D�&fD�)�D�#3D�)�D�0 D�6fD�@ D�@ D�<�D�@ D�FfD�P D�S3D�Y�D�c3D�l�D�s3D�s3D�y�D�i�D�p D�vfD��fD��fD���E S3E �3Ea�E�3Eh E��Ep E�Eq�E~fEx E� E�3E
y�Ex E�fE�3EvfEt�E��E��Es3Et�E�3E�El�EnfE�3E�3El�Ep E � E!�3E#a�E$\�E%��E'<�E(4�E)�fE*��E+� E-S3E.� E/��E0�3E2;3E3~fE4��E6�E79�E8h E9�3E; E<!�E?d�EBNfEE�fEH�fEK�fEN��EQ�EU�EX,�E[NfE^ffEa�fEd�fEg�fEk$�En33EqK3Et;3Ewa�Ez�3E}�fE�zfE�  E�� E��E��3E��E�S3E��3E�� E�I�E�� E��fE��E��3E��fE� E�k3>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?��?��?333?fff?�  ?�ff?�33?���?�ff@   @��@   @333@Fff@`  @l��@�  @�ff@�33@���@�ff@�  @���@ə�@ٙ�@�33@�  @���A��A��A33A33A#33A+33A1��A9��AA��AH  ANffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141444444414411411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?fff?�33@&ff@fff@�  @�33@�33@�33A	��A  A+33AH  Ak33A�  A�33A���A���A���A�  A�  A�  B��B
ffB��B33B#33B+33B333B:��BBffBI��BRffBZffBa��Bj  Br��BzffB�  B���B�  B�33B�  B�  B�33B�33B�ffB�  B�33B�33B�33B�  B�33B�ffB�  B�33B�33B͙�B�ffB�ffB�33B�33B�33B�  B���B�ffB�  B�  B�ffB���C ��CffC�3C��C�3C
��C��C��CffC�3C��C��C��CffC�3C�3C ��C"� C$L�C&ffC(� C*� C,�3C.� C0L�C2� C4��C6��C8��C:L�C<��C>�3C@��CB� CD�3CF� CHffCJ�3CL��CNffCP�3CR��CTffCV�3CX��CZ� C\��C^ffC`��Cb��Cd��Cf��Ch��Cj� Cl� Cn��Cp�3Cr�3Ct��Cv�3Cx�3Cz��C|�3C~��C�L�C�@ C�@ C�&fC�L�C�ffC�ffC�@ C�Y�C�L�C�33C�L�C�Y�C�ffC�Y�C�33C�@ C�L�C�ffC�@ C�L�C�L�C�33C�33C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�L�C�L�C�Y�C�ffC�Y�C�Y�C�Y�C�ffC�ffC�L�C�L�C�s3C�ffC�ffC�Y�C�Y�C�Y�C�@ C�@ C�@ C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�&fC�L�C�L�C�@ C�&fC�L�C�@ C�&fC�@ C�@ C�&fC�L�C�@ C�33C�L�C�@ C�33C�Y�C�Y�C�@ C�33C�&fC�@ C�Y�C�L�C�33C�@ C�L�C�Y�C�ffC�ffC�@ C�Y�C�L�C�33C�33C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�L�C�L�C�Y�C�L�C�Y�C�33C�L�C�@ C�&fC�ffC�Y�D &fD ��D�D� DfD
l�D� Ds3D��D��D9�D��D�fDl�D"@ D%&fD'�3D*�fD-��D0Y�D33D5��D8ffD;3D=��D@&fDB��DE,�DG� DJ3DL� DN� DQ9�DS�3DV  DX` DZ� D]�D_s3Da��Dd,�Df�fDh� Dk&fDmy�Do��Dr�DtffDv�3Dx�fD{,�D}�D� D�� D�#3D�VfD�� D��fD���D�@ D�� D��fD��3D�33D�p D���D��D�&fD�c3D���D���D���D�)�D�\�D���D�� D�� D��D�@ D�i�D���D��fD���D�3D�&fD�L�D�s3D��3D���D�� D�3D�,�D�\�D��3D��3D���D�#3D�S3D�� D��fD���D�)�D�` D�� D���D�fD�FfD��fD��fD�fD�C3D�|�DǶfD���D�  D�Y�D̓3D��3D��3D�#3D�Y�DҌ�DӶfD��3D�	�D�,�D�L�D�i�Dډ�Dۣ3D��3D�� D��fD��fD�fD�3D��D�  D�&fD�,�D�6fD�9�D�33D�9�D�@ D�FfD�P D�P D�L�D�P D�VfD�` D�c3D�i�D�s3D�|�D��3D��3D���D�y�D�� D��fD��fD��fD���E [3E �3Ei�E�3Ep E��Ex E��Ey�E�fE� E  E	3E
��E� E�fE�3E~fE|�E��E��E{3E|�E�3E��Et�EvfE�3E�3Et�Ex E � E!�3E#i�E$d�E%��E'D�E(<�E)�fE*��E,  E-[3E.� E/��E0�3E2C3E3�fE4��E6	�E7A�E8p E9�3E; E<)�E?l�EBVfEE�fEH�fEK�fEN��EQ�EU�EX4�E[VfE^nfEa�fEd�fEg�fEk,�En;3EqS3EtC3Ewi�Ez�3E}�fE�~fE� E�� E� �E��3E��E�W3E��3E�� E�M�E�� E��fE� �E��3E��fE� E�o3G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��?fffG�O�?���?���?�33?�  ?�ff?�33@ff@33@   @9��@@  @S33@fff@�  @�ff@�  @�ff@�33@���@�ff@�  @���@ٙ�@陚@�33A   A��A��A��A33A#33A+33A333A9��AA��AI��AP  AVffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414141444444414411411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ �@ %@ V@ *@ O@ "�@ )�@ 0x@ 7L@ =q@ E�@ Q�@ `�@ m:@ z3@ �7@ ��@ ��@ �~@ �&@ ��@ ��@ ��@ � @v@@ �@.l@;d@H]@T�@c�@qS@}�@��@��@��@��@��@�7@ލ@�@�,@�@*@#�@/�@>@K�@Yn@ff@t�@�@�\@��@�Y@�^@�W@��@��@�@�E@
=@6@&�@33@@�@O�@^5@j@v�@�|@��@��@��@��@�@�[@�@�e@@V@�@*S@7�@FQ@Q�@^5@l�@z�@��@��@��@�!@�&@�|@܀@��@�e@@o@g@,`@;d@G�@T�@dZ@qS@}�@�P@�H@��@�F@�>@�7@ލ@��@�9@�@*@"�@0x@=q@K@Z�@g�@uk@�d@��@�a@�Y@��@ƨ@�O@�H@��@�9@
�@�@'�@33@B8@O0@[z@j@x�@�+@�$@��@�@�k@�o@�
@�`@�@�Q@�@O@(�@7L@D�@R�@`B@n�@|?@�7@��@�5@��@��@�*@��@�(@��@	@	�@	!s@	.l@	<@	I@	V�@	dZ@	p�@	~K@	��@	��@	��@	�F@	Ĝ@	�7@	܀@	��@	�~@
�@
{@
"�@
0x@
>�@
Lu@
X�@
e	@
s_@
�d@
��@
�@
�M@
�@
ƨ@
Ӡ@
��@
�@
��@�@�@%�@1�@A�@N�@[z@j@ww@�p@�$@��@�@�@�@�
@�@�@�Q@�@�@*S@8�@FQ@Q�@`�@m�@z3@��@�0@��@�-@��@�|@�#@�y@� @�@�@g@-�@:�@I@T�@c�@p�@|�@��@��@��@�9@��@@�@~�@��@j@I�@��@�\@
@g�@�-@��@K@�H@�@3�@�@�@�@^�@��@��@5�@{�@��@�@G�@��@��@�@M�@��@�7@@Q�@�@�C@o@SI@�u@Ӡ@�@Q=@��@ψ@V@M$@�7@�W@��@=q@~K@��@]@DD@�|@ȴ@�@Q�@�#@׹@�@_�@�(@�@*S@m�@��@�@ 1�@ r�@ �9@ �@!6�@!ww@!��@!��@"5@@"s_@"��@"�@#0x@#n�@#�f@#�4@$)�@$hs@$�A@$�`@%$�@%e�@%��@%��@&+@&j@&�Y@&�@'-�@'o�@'��@'��@(6�@(z3@(�@)]@)E�@)��@)�*@*�@*T�@*��@*��@+O@+^5@+�@+��@,"�@,c�@,��@,�m@-&�@-g@-��@-�@.!s@.^5@.��@.׹@/*@/Q�@/��@/��@/��@07L@0oF@0�A@0�;@16@1O�@1�+@1�j@1�e@2,`@2dZ@2�@2Ӡ@3	�@3@�@3x�@3�~@3��@4 �@4Yn@4�@4�@5 �@58�@5k�@5��@5��@6�@6O�@6��@6��@6��@73�@7k.@7��@7܀@8{@8K�@8�d@8�@9_�@:j@:r@;*@;��@<$�@<��@=5�@=�z@>FQ@>��@?V�@?Ĝ@@g�@@�O@Av@A�@B�|@B�@C�<@D�@D��@E�@E��@F �@F��@GZ�@GĜ@HbN@H�@I_�@I�@J��@J�(@K|?@L�@L��@M �@M��@N-@N�@O-�@O�7@PD�@Q�M@R�l@T^�@U��@V��@X5@@Y��@Z�T@\<@]�@^�@`X@a��@b�l@dS�@e��@f�@h4�@i��@j��@lM�@m�y@n��@p6�@q��@r��@s:�@sz2@s��@s�Y@tLv@t��@tě@u �@u[z@u�<@u�
@v	G�O�G�O�G�O�G�O�@ G�O�@ G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ @ �G�O�@ @ �@ %@ �@ �@ 	�@ 
�@ J@ �@ b@ @ @ *@ �@ B@ O@ �@ g@ !s@ #�@ %�@ (G@ +@ .l@ 0x@ 33@ 5?@ 8�@ <@ >�@ B8@ E�@ I@ K�@ O0@ R�@ UU@ XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�r�A� �AѾwA�I�A��A�jA��A���A��A��HA�ȴAϓuA�K�A�33A�&�A��A�bA�
=A�  A��AΛ�A΁A�r�A�ffA�VA�E�A�1'A�jA�S�A˸RAʮA�~�A�7LA���A�VA��
A�v�A��#A�l�A�x�A�1A�p�A��#A�E�A��A���A�`BA�A�A�1A��9A�{A�?}A�v�A��/A��-A�|�A���A���A��+A�bNA��yA��!A�bA���A���A�33A��
A�%A�x�A��A��RA���A�n�A�r�A��uA���A�G�A�|�A���A��A�p�A�"�A�r�A���A��A�S�A�n�A�^5A�/A�=qA�r�A��;A�7LA��A���A��DA���A��\A���A�E�A��`A�~�A�$�A�Q�A�C�A�%A�K�A��FA���A�A��AhsA33A%A~��A{`BAuAr��AqAo�PAl �Ai��Ah^5AhI�Ag%Af�Af�!Ae�
AehsAe7LAd�jAc�wAcS�Abv�A`��A_O�A\��A[�hAZ�RAXv�AW\)AV�uAU�#AUATbAR��AQ�
AO�AN$�AL�9AK��AK��AK\)AJbNAJbAI�FAH��AG�TAF��AF�AE��AD��ACAB �A?��A>�A=x�A<�!A;|�A: �A9�FA9`BA8~�A5�;A4��A3�A2�A2ZA1�hA0�/A/%A-\)A,A�A,5?A+��A+oA*^5A)K�A(��A(ZA( �A'�TA'|�A&  A%/A#��A"�!A!+A =qAjA
=Ar�A�
AdZA�A��AffA?}A�A��A%Ax�AhsA��A��Az�A��A$�A�mAĜA  A
��A	�7A	?}A��A�A�A�hA33A��A33A�A�mA ĜA E�@�t�@���@�r�@��F@��@�V@�^5@�1@���@�+@��@��@�&�@�~�@��@�u@���@�@�K�@�o@�t�@�-@�@�9X@�o@��/@���@ݲ-@Ӿw@�j@У�@�v�@�(�@�o@�r�@�p�@���@�Z@�b@�J@��@��+@�p�@�@��T@��9@���@�
=@�V@��j@�K�@��+@���@�%@�l�@�M�@�{@���@���@�
=@�M�@��@�`B@�1'@�dZ@�=q@���@�X@�Ĝ@���@���@�%@�"�@���@��-@�`B@��@�"�@�{@��9@}�T@|�@z�H@y&�@xb@v5?@u/@sC�@r=q@q��@p��@o�P@m�h@k�F@j�@i�@hA�@fV@d��@d9X@cC�@ax�@_�@]��@\�j@Z�!@Yx�@W+@Vv�@T�@S�m@R��@QG�@N��@K��@J~�@I�@G�P@E@D1@Bn�@A�@A�7@AG�@@A�@>v�@=��@<�@<�@:^5@9��@8A�@7l�@6��@5p�@3�m@2�!@17L@01'@/\)@-�-@,�j@+ƨ@*-@(Ĝ@'�w@'+@&��@&@%�-@$�/@$(�@#dZ@"J@!�#@!hs@ r�@\)@ȴ@�R@��@��@(�@S�@~�@��@�;@��@��@5?@�T@�j@�m@�@~�@��@��@ �@+@��@ff@/@�j@�@C�@
n�@	��@	x�@Ĝ@b@�;@|�@�@ȴ@��@/@�@�@�m@��@�7@�?�|�?�O�?���?���?�$�?��?�hs?��-?�V?�1?�^5?�l�?��?�?�t�?�\?��`?��?ݲ-?܋D?�C�?�?��?ش9?׮?�E�?Լj?�z�?��
?���?щ7?Ѓ?���?Η�?Ͳ-?�/?�(�?���?��#?��#?�7L?�1'?�?��?���?�Ĝ?� �?��?�V?�ƨ?�C�?���?���?���?���?��#?�X?�X?�r�?�Q�?�r�?��?���?�~�?�"�?���?�I�?�V?�/?�O�?�O�?�p�?��-?���?���?��?�5??�5??���?��RA�VA�S�A�^5A�dZA�l�A�ffA�hsA�l�A�r�A�hsA�l�A�r�A�t�A�r�A�z�A�z�A�z�A�|�A�z�A�z�A�z�A�~�A�x�A�x�A�r�A�r�A�t�A�^5A�VA�"�A�VA�bA�1A��mAѲ-Aѣ�A�|�A�A�A��A�JA���A��yA�ĜAЉ7A�K�A�&�A��A�
=A�A���A��A��A��mA��HA��A���A���A�ƨA�ĜAϼjG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�n�A�r�A� �AѾwA�I�A��A�jA��A���A��A��HA�ȴAϓuA�K�A�33A�&�A��A�bA�
=A�  A��AΛ�A΁A�r�A�ffA�VA�E�A�1'A�jA�S�A˸RAʮA�~�A�7LA���A�VA��
A�v�A��#A�l�A�x�A�1A�p�A��#A�E�A��A���A�`BA�A�A�1A��9A�{A�?}A�v�A��/A��-A�|�A���A���A��+A�bNA��yA��!A�bA���A���A�33A��
A�%A�x�A��A��RA���A�n�A�r�A��uA���A�G�A�|�A���A��A�p�A�"�A�r�A���A��A�S�A�n�A�^5A�/A�=qA�r�A��;A�7LA��A���A��DA���A��\A���A�E�A��`A�~�A�$�A�Q�A�C�A�%A�K�A��FA���A�A��AhsA33A%A~��A{`BAuAr��AqAo�PAl �Ai��Ah^5AhI�Ag%Af�Af�!Ae�
AehsAe7LAd�jAc�wAcS�Abv�A`��A_O�A\��A[�hAZ�RAXv�AW\)AV�uAU�#AUATbAR��AQ�
AO�AN$�AL�9AK��AK��AK\)AJbNAJbAI�FAH��AG�TAF��AF�AE��AD��ACAB �A?��A>�A=x�A<�!A;|�A: �A9�FA9`BA8~�A5�;A4��A3�A2�A2ZA1�hA0�/A/%A-\)A,A�A,5?A+��A+oA*^5A)K�A(��A(ZA( �A'�TA'|�A&  A%/A#��A"�!A!+A =qAjA
=Ar�A�
AdZA�A��AffA?}A�A��A%Ax�AhsA��A��Az�A��A$�A�mAĜA  A
��A	�7A	?}A��A�A�A�hA33A��A33A�A�mA ĜA E�@�t�@���@�r�@��F@��@�V@�^5@�1@���@�+@��@��@�&�@�~�@��@�u@���@�@�K�@�o@�t�@�-@�@�9X@�o@��/@���@ݲ-@Ӿw@�j@У�@�v�@�(�@�o@�r�@�p�@���@�Z@�b@�J@��@��+@�p�@�@��T@��9@���@�
=@�V@��j@�K�@��+@���@�%@�l�@�M�@�{@���@���@�
=@�M�@��@�`B@�1'@�dZ@�=q@���@�X@�Ĝ@���@���@�%@�"�@���@��-@�`B@��@�"�@�{@��9@}�T@|�@z�H@y&�@xb@v5?@u/@sC�@r=q@q��@p��@o�P@m�h@k�F@j�@i�@hA�@fV@d��@d9X@cC�@ax�@_�@]��@\�j@Z�!@Yx�@W+@Vv�@T�@S�m@R��@QG�@N��@K��@J~�@I�@G�P@E@D1@Bn�@A�@A�7@AG�@@A�@>v�@=��@<�@<�@:^5@9��@8A�@7l�@6��@5p�@3�m@2�!@17L@01'@/\)@-�-@,�j@+ƨ@*-@(Ĝ@'�w@'+@&��@&@%�-@$�/@$(�@#dZ@"J@!�#@!hs@ r�@\)@ȴ@�R@��@��@(�@S�@~�@��@�;@��@��@5?@�T@�j@�m@�@~�@��@��@ �@+@��@ff@/@�j@�@C�@
n�@	��@	x�@Ĝ@b@�;@|�@�@ȴ@��@/@�@�@�m@��@�7@�?�|�?�O�?���?���?�$�?��?�hs?��-?�V?�1?�^5?�l�?��?�?�t�?�\?��`?��?ݲ-?܋D?�C�?�?��?ش9?׮?�E�?Լj?�z�?��
?���?щ7?Ѓ?���?Η�?Ͳ-?�/?�(�?���?��#?��#?�7L?�1'?�?��?���?�Ĝ?� �?��?�V?�ƨ?�C�?���?���?���?���?��#?�X?�X?�r�?�Q�?�r�?��?���?�~�?�"�?���?�I�?�V?�/?�O�?�O�?�p�?��-?���?���?��?�5??�5??���?��RA�VA�S�A�^5A�dZA�l�A�ffA�hsA�l�A�r�A�hsA�l�A�r�A�t�A�r�A�z�A�z�A�z�A�|�A�z�A�z�A�z�A�~�A�x�A�x�A�r�A�r�A�t�A�^5A�VA�"�A�VA�bA�1A��mAѲ-Aѣ�A�|�A�A�A��A�JA���A��yA�ĜAЉ7A�K�A�&�A��A�
=A�A���A��A��A��mA��HA��A���A���A�ƨA�ĜAϼjG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�#B�B�
B�B�B�B�
B�B�B�B�B�B��B��B�B�B��B��B��B��B��B�B�B�B�B�B�B��B�B�mB�B�B�B�B�B��B�RB�dBBƨBƨB�dB�qB�LB�?B�B�B��B��B��B��B��B��B��B��B��B�B�!B�-B�3B�'B�3B�-B�9B�FB�9B�FB�FB�?B�-B�B�B��B��B��B��B�DBq�Bo�BbNBG�B@�B:^B33B"�B{B
=B��B�B�;B��B�-B��B�{B}�BhsBQ�BG�B8RB1'B(�B"�B�B�BDB
��B
�B
�
B
ȴB
�FB
��B
�uB
�JB
�7B
�+B
�B
aHB
;dB
)�B
�B
\B	�B	�;B	�NB	�HB	�B	�/B	�#B	��B	��B	��B	��B	ǮB	ƨB	�wB	�LB	�B	��B	��B	��B	�JB	�%B	�B	}�B	y�B	t�B	m�B	iyB	bNB	ZB	T�B	P�B	O�B	K�B	I�B	F�B	C�B	>wB	9XB	6FB	33B	/B	)�B	&�B	�B	�B	hB		7B	B��B��B��B�B�B�TB�;B�B�B��B��BÖB�?B�!B�B�B�B��B��B��B��B��B��B��B��B�{B�hB�=B�1B�=B�%B�B� B|�Bx�Bw�Bu�Bu�Bp�Bm�BgmBhsBdZB\)BVBQ�BL�BD�BC�BA�B@�B;dB;dB8RB>wB=qB7LB33B6FB9XB<jB;dB?}BC�BH�BG�BF�BE�BC�BC�BB�B@�B=qB;dB:^B9XB9XB8RB8RB7LB8RB9XB8RB8RB8RB7LB6FB9XB:^B9XB9XB8RB6FB8RB9XBF�BaHBv�Bs�B�=B�\B�VB��B��B��B�9BĜB�B�/B�B	
=B	�B	$�B	8RB	R�B	ZB	r�B	z�B	�B	�DB	�bB	��B	��B	��B	��B	�B	�3B	�RB	�qB	�qB	ÖB	ƨB	��B	��B	��B	�
B	�)B	�;B	�`B	�B	�B	�B	�B	��B	��B	��B
  B
B
B
%B
1B
	7B

=B
DB
PB
\B
\B
bB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
#�B
%�B
'�B
,B
,B
.B
/B
/B
1'B
49B
7LB
8RB
9XB
;dB
<jB
=qB
>wB
>wB
?}B
?}B
@�B
B�B
B�B
C�B
D�B
F�B
E�B
G�B
H�B
H�B
I�B
K�B
L�B
N�B
N�B
O�B
Q�B
R�B
S�B
VB
W
B
W
B
XB
YB
YB
ZB
[#B
\)B
]/B
^5B
]/B
^5B
_;B
`BB
`BB
aHB
bNB
cTB
cTB
dZB
dZB
dZB
ffB
gmB
gmB
gmB
hsB
iyB
jB
jB
jB
k�B
m�B
l�B
m�B
n�B
n�B
o�B
o�B
q�B
q�B
q�B
q�B
r�B
s�B
t�B
s�B
u�B
u�B
u�B
w�B
v�B
x�B
y�B
y�B
{�B
|�B
}�B
}�B
�B
�B
�B
�B
�%B
�+B
�7B
�1B
�=B
�=B
�\B
�\B
�\B
�hB
�hB
�uB
�uB
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
�B
�B
�B
�!B
�'B
�-B
�3B
�9B
�?B
�?B
�FB
�LB
�FB
�LB
�LB
�RB
�XB
�XB
�^B
�XB
�RB
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
�^B
�XB
�XB�/B�B�
B�/B�B�B�B�/B�B�)B�)B�B�)B�B�B�B�#B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�#B�
B�B�B�B�
B�B�B�B�B�B�
B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�SB�~B�B�B�B�B��B�:B�MB�xBƒBƒB�OB�\B�8B�+B��B�B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�B�(B�#B�/B�=B�0B�>B�?B�8B�'B�B�B��B��B��B��B�ABq�Bo�BbMBG�B@�B:^B34B"�B}B
?B��B�B�>B��B�1B��B��B}�BhyBQ�BG�B8YB1/B(�B"�B�B�BNB
��B
�B
�B
��B
�RB
��B
��B
�XB
�EB
�:B
�.B
aXB
;tB
*B
�B
mB	�B	�MB	�`B	�[B	�0B	�CB	�7B	�B	�B	��B	��B	��B	��B	��B	�eB	�4B	��B	��B	��B	�eB	�@B	�.B	~B	y�B	t�B	m�B	i�B	bmB	Z<B	UB	QB	P B	K�B	I�B	F�B	C�B	>�B	9|B	6jB	3XB	/@B	*"B	'B	�B	�B	�B		_B	BB�B��B��B��B�B�B�gB�DB�1B�B��B��B�nB�PB�8B�DB�?B�.B�B�
B�B�B�B�B��B��B��B�tB�hB�uB�]B�>B�9B}'ByBx	Bu�Bu�Bp�Bm�Bg�Bh�Bd�B\gBVCBR+BMBD�BC�BA�B@�B;�B;�B8�B>�B=�B7�B3xB6�B9�B<�B;�B?�BC�BH�BG�BF�BE�BC�BC�BB�B@�B=�B;�B:�B9�B9�B8�B8�B7�B8�B9�B8�B8�B8�B7�B6�B9�B:�B9�B9�B8�B6�B8�B9�BGBa�Bw,BtB��B��B��B�<B�2B�<B��B�BքBݳB�1B	
�B	B	%nB	8�B	S�B	Z�B	sNB	{�B	��B	��B	�B	�FB	�mB	�wB	��B	��B	��B	�B	�1B	�4B	�\B	�qB	̒B	ΡB	��B	��B	��B	�B	�<B	�dB	�fB	��B	�B	��B	��B	��B
 �B
B
B
B
	.B

7B
@B
JB
YB
hB
kB
tB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
%B
%B
'B
)/B
-JB
-LB
/[B
0eB
0hB
2vB
5�B
8�B
9�B
:�B
<�B
=�B
>�B
?�B
?�B
@�B
@�B
A�B
DB
DB
EB
FB
H(B
G$B
I3B
J<B
J?B
KHB
MXB
NaB
PpB
PsB
Q|B
S�B
T�B
U�B
W�B
X�B
X�B
Y�B
Z�B
Z�B
[�B
\�B
]�B
^�B
_�B
^�B
_�B
aB
bB
bB
cB
d%B
e-B
e0B
f8B
f:B
f=B
hKB
iUB
iWB
iZB
jbB
kkB
lsB
luB
lxB
m�B
o�B
n�B
o�B
p�B
p�B
q�B
q�B
s�B
s�B
s�B
s�B
t�B
u�B
v�B
u�B
w�B
w�B
w�B
y�B
x�B
{B
|B
|B
~ B
,B
�9B
�>B
�WB
�\B
�iB
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
�B
�B
� B
�'B
�,B
�?B
�JB
�QB
�VB
�iB
�pB
��B
��B
��B
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
�B
�!B
�3B
�IB
�]B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
�:B
�PB
�dB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�B
�)B
�&B
�)B
�2B
�4B
�8B
�5B
�>B
�@B
�DB
�MB
�JB
�MB�B��B��B�B��B��B��B�B��B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808272201322021061413553520210614135535202106171312422021061713124220210617131242201808272201322021061413553520210614135535202106171312422021061713124220210617131242PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018082722013220180827220132  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082722013220180827220132QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082722013220180827220132QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150120210617131501IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                