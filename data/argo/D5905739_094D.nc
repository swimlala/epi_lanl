CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-01-20T08:01:00Z creation      
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
resolution        =���   axis      Z        %�  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	`  a`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %�  j�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	`  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     %�  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %�  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	`  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %�  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	` �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %� �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %� B`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	` g�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %� q@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	` ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     %� �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � Š   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �H      � �HArgo profile    3.1 1.2 19500101000000  20200120080100  20210617131527  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ^   ^DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @����o �@����o �11  @����:P@����:P@2�u%F
�@2�u%F
��cv��)_�cv��)_11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >L��?�  ?�33@@  @�ff@�ff@�33@���A   A33A&ffAD��A`  A~ffA�33A���A���A�  A�  AᙚA�ffB ��BffBffBffB   B(  B0ffB7��B@  BH��BP  BX  Ba33Bh��BpffBx��B�33B�  B�  B�  B�33B�33B�33B�ffB�  B�33B�ffB�  B���B�  B���B�33B�ffB�33BǙ�B�  B�  B�  B�33B�ffB���B���B癚B�ffB�ffB�  B���B�  B���C33CL�C�fC�fC	�fC�fC�fC�fC  C  C  C  C�C�C�fC�fC"33C$33C&33C(33C*33C,�C.�C0  C1�fC4�C6L�C833C:�C<  C=�fC@�CB33CD�CF  CH33CJ33CL  CN�CP  CQ��CT  CV�CXL�CZ33C\  C^  C_�fCb�CdL�CfL�Ch�Cj33Cl�Cm�fCp33Cr�Ct  Cv33Cx�Cy��C|  C~33C��C��3C��3C��C��C�&fC��C��fC��3C�  C�&fC�  C��fC��3C�  C�  C��C�&fC��C��fC��C��C�&fC��C��fC�  C��C��C�33C�  C��fC��fC�  C�  C��C��C��C�&fC��C��fC��3C�  C�  C��C��C��C��C�&fC�&fC��C��C��C��C�&fC�&fC��C�  C��C��C��C��C�&fC��C��fC��fC��3C��3C��3C�  C�  C��C��C��C��C�  C��fC��3C��C�&fC�  C��fC�  C��C�  C��fC�  C��C�33C��C��3C��C��C��C��3C��C��C��C��3C�  C��C�33C��C��3C�  C��C��C��C�&fC�33C��C��fC��fC�  C��C��C��C��C��C�&fC��C��C�33C��C�&fC�&fC�&fC��C�&fD fD ��D�D�fDfD�fD��D� D�3Ds3D�3Dl�D3D�fDfD� D��D��D	3D	�fD
  D
s3D3D��D  Ds3D��D�fD��Dl�D�D��DfD� D�D�fD�3D��D�3Dl�DfD� D��D��D��Dl�DfDy�D�3D��D  Dy�D3D�fD  Dy�D��D��DfD� D�3Dl�DfDs3D�3D �fD!  D!s3D!��D"l�D#fD#y�D#��D$�3D%�D%�fD&  D&y�D&�3D's3D(�D(��D)�D)�fD*fD*y�D*��D+s3D+��D,s3D,��D-�3D.3D.�3D/�D/��D0fD0��D1�D1��D2fD2��D3fD3��D4fD4� D4��D5y�D5��D6y�D6��D7� D7��D8� D9fD9�fD:fD:�fD;  D;�fD<  D<�fD=fD=�fD>fD>��D?fD?��D@fD@��DA3DA�3DB�DB��DCfDC�fDD�DD�fDE�DE�fDFfDF� DF��DG� DH  DHy�DH��DIy�DI��DJs3DK�DK��DL3DL�3DM3DM��DN3DN��DO�DO�fDPfDP�fDQfDQy�DQ��DRs3DR�3DS�3DT3DT�3DU�DU�fDVfDV� DW  DWs3DX�DX�3DYfDY� DY�3DZ�3D[fD[� D[�3D\s3D]3D]��D^fD^�fD_  D_s3D_�3D`�3Da  Da�fDa��Dbs3Dc3Dc�fDd  Dd� Dd�3De��Df�Df�3Dg�Dg�fDh  Dh� Di  Diy�Dj�Dj��Dk�Dk�3Dl�Dl�fDmfDm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Duy�Dv  Dvy�Dv�3Dws3Dw�3Dx��Dy3Dys3Dy�3Dz�3D{3D{�3D|3D|�3D}�D}�3D~3D~��D�D�fD�  D�C3D�� D��3D�  D�C3D��3D��3D�fD�FfD��fD�ɚD�fD�C3D�� D�� D�  D�@ D�|�D���D��D�L�D�vfD��fD�	�D�FfD��fD��fD�3D�@ D�� D���D���D�@ D�|�D���D���D�L�D���D��fD��fD�FfD��3D��3D�  D�@ D�� D�� D���D�<�D�y�D���D��D�FfD���D��fD�fD�@ D�� D�� D�  D�9�D�y�D���D�	�D�FfD��3D�� D���D�<�D���D�ɚD�	�D�C3D��3D��fD�3D�<�D�� D���D���D�<�D�y�D���D��D�6fD�vfD�ɚD�	�D�FfD��3D��3D�  D�@ D�� D�� D���D�<�D�|�D���D���D�9�D���D���D��D�FfD���D��fD�3D�@ D�� D���D���D�L�D���D�ɚD�fD�C3D�� D���D��D�FfD�� D���D���D�I�D��fD��fD�  D�@ D�y�D��fD�	�D�C3D��3D��3D���D�9�D�y�D��fD�3D�C3D�� D���D�fD�C3D�|�D�ɚD�3D�<�D���D�� D�  D�FfD�� D���D�	�D�@ D�y�D��fD� D�I�D��fD�� D���D�FfD�� D��fD�  D�L�D��3D���D�fD�@ D�y�D��3D��D�C3D�|�D��3D�	�D�P D��fD���D�3D�L�D�� D�� D��fD�9�D�|�D�� D�3D�FfD���D�� D�3D�9�D�� D��3D�	�D�P D��3D���D���D�FfD���D�� D�  D�6fD�|�D�� D�3D�L�D�|�D��3D���D�@ D�� D��fD�	�D�P D��3D���D���D�C3D���D�� D�fD�9�D�� D�ɚD���D�6fD�|�D��fD� D�FfD�� D��fD� D�FfD�|�D��fD��D�FfD�|�D��fD�  D�9�D��3D���D�fD�@ D���D�� D���D�C3D�|�D��fD�3D�L�D��3D�� D��D�C3D�|�D�ɚD�3D�<�DÆfD��3D���D�FfDĀ Dļ�D�fD�C3D�|�D�ɚD���D�9�DƆfD�� D���D�L�DǆfD��3D���D�6fDȌ�D��3D�3D�@ D�|�D���D�	�D�FfDʃ3Dʼ�D��D�I�D˃3D�� D���D�I�D̆fD��fD�  D�<�D�y�DͶfD�3D�@ D�|�D���D�fD�C3D�|�DϹ�D��D�FfDІfD��3D�  D�<�D�vfD�ɚD�3D�@ D�|�DҶfD�fD�C3DӀ D�ɚD�3D�C3D�|�DԹ�D�fD�C3D�|�D���D�fD�@ D֌�D��3D���D�I�D׃3D׼�D�fD�<�D�vfD��fD� D�I�DنfDټ�D���D�FfD�|�DڶfD�  D�9�D�vfDۼ�D�	�D�<�D�vfDܼ�D�	�D�@ D�y�D��fD���D�6fDހ D��fD� D�FfD�|�D��3D�fD�L�D�� D๚D���D�C3D��D��D��fD�@ D�fD���D� D�@ D�vfD�� D�fD�I�D� D�fD�  D�C3D� D�� D��fD�<�D� D��fD��D�C3D�|�D�� D�fD�9�D�s3D蹚D�fD�L�D� D�fD�  D�C3D� D�� D���D�C3D��D��3D���D�FfD�|�D�fD���D�I�D� D��fD���D�FfD�|�D�fD�  D�I�D� D﹚D�3D�<�D�vfD�� D�	�D�@ D�vfD��3D�	�D�@ D� D��3D��D�C3D�y�D��3D� D�FfD�|�D��fD�  D�6fD�|�D��3D�	�D�@ D�vfD�� D�fD�L�D��3D���D�fD�@ D�vfD���D�	�D�9�D�vfD��3D� D�� D�  D��fD���D���D��3E ffE � E� EfE��E4�E��EH EњE�EnfE��E{3E3E��E	�3E
$�E
��E33E��EFfE[3E� Ek3E� E� E�E E��E  E� E,�EA�E�fENfE�fE` E�E3E�fE�E�3E�E!�E� EfE�fE!�E�E��E3E �E ��E � E!�E"[3E"��E#њE$D�E$�fE%�3E&3E&��E'{3E'�3E(Y�E)>fE)��E*� E*�fE+�E,L�E,��E-�fE.fE.�E/VfE09�E0��E1~fE1�3E2^fE3P E3��E4;3E5#3E5��E6� E6�E7h E8Q�E8�fE99�E:+3E:�fE;��E;��E<ffE=K3E=�3E>��E>�fE?��E@1�E@��EAY�EB#3EB�ECD�ED�ED��EE�EE��EF�3EG EG�EH[3EI1�EI� EJk3EJ��EK�3EK� EL�fEMs3EN0 EN�3EOFfEO�3EPP EQfEQ�fER�fER�ES�3ET4�EU EU� EV` EV�fEW33EX EXvfEYS3EY� EZ� EZ�fE[�fE\1�E]fE]� E^0 E^�fE_T�E`�E`�3Eay�Eb&fEbt�EcfEc��EdffEe Ee��EfQ�Ef�3Eg� Eh+3Eh� EiT�Ej,�Ej� EkQ�Ek�fEly�Em�Em�Ens3En�Eo�3EpL�EpњEq��ErfEr�3Esc3Es�3Et��>���>L��>L��>L��>L��=���>���>L��>L��>L��>L��>���>���>L��>L��>L��>L��>L��>���>���?��?��?fff?���?���?�  ?���?�33@��@&ff@@  @Y��@s33@�ff@�  @���@�  @���@�ff@�33@�33@�33A   A  AffAffAffA$��A.ffA8  A>ffAI��AP  AX  Aa��Ah  Aq��Ay��A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441444414444441414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?333?�  @��@`  @�ff@�ff@�33@���A  A33A.ffAL��Ah  A�33A�33A���A���A�  A�  A噚A�ffB��B
ffBffBffB"  B*  B2ffB9��BB  BJ��BR  BZ  Bc33Bj��BrffBz��B�33B�  B�  B�  B�33B�33B�33B�ffB�  B�33B�ffB�  B���B�  B���B�33B�ffB�33Bș�B�  B�  B�  B�33B�ffBᙚB���B虚B�ffB�ffB�  B���B�  C ffC�3C��CffCffC
ffCffCffCffC� C� C� C� C��C��CffC ffC"�3C$�3C&�3C(�3C*�3C,��C.��C0� C2ffC4��C6��C8�3C:��C<� C>ffC@��CB�3CD��CF� CH�3CJ�3CL� CN��CP� CRL�CT� CV��CX��CZ�3C\� C^� C`ffCb��Cd��Cf��Ch��Cj�3Cl��CnffCp�3Cr��Ct� Cv�3Cx��CzL�C|� C~�3C�L�C�33C�33C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�ffC�@ C�&fC�33C�@ C�@ C�Y�C�ffC�Y�C�&fC�L�C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�s3C�@ C�&fC�&fC�@ C�@ C�L�C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�ffC�ffC�Y�C�Y�C�Y�C�Y�C�ffC�ffC�Y�C�@ C�L�C�Y�C�Y�C�Y�C�ffC�L�C�&fC�&fC�33C�33C�33C�@ C�@ C�L�C�L�C�Y�C�Y�C�@ C�&fC�33C�L�C�ffC�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�s3C�Y�C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�s3C�L�C�33C�@ C�L�C�Y�C�Y�C�ffC�s3C�Y�C�&fC�&fC�@ C�L�C�L�C�Y�C�Y�C�Y�C�ffC�L�C�L�C�s3C�L�C�ffC�ffC�ffC�Y�C�ffD &fD ��D,�D�fD&fD�fD�D� D3D�3D3D��D33D�fD&fD� D�D��D	33D	�fD
  D
�3D33D��D  D�3D�D�fD�D��D,�D��D&fD� D9�D�fD3D��D3D��D&fD� D�D��D�D��D&fD��D3D��D  D��D33D�fD  D��D�D��D&fD� D3D��D&fD�3D 3D �fD!  D!�3D"�D"��D#&fD#��D$�D$�3D%,�D%�fD&  D&��D'3D'�3D(9�D(��D),�D)�fD*&fD*��D+�D+�3D,�D,�3D-�D-�3D.33D.�3D/,�D/��D0&fD0��D1,�D1��D2&fD2��D3&fD3��D4&fD4� D5�D5��D6�D6��D7�D7� D8�D8� D9&fD9�fD:&fD:�fD;  D;�fD<  D<�fD=&fD=�fD>&fD>��D?&fD?��D@&fD@��DA33DA�3DB,�DB��DC&fDC�fDD,�DD�fDE,�DE�fDF&fDF� DG�DG� DH  DH��DI�DI��DJ�DJ�3DK9�DK��DL33DL�3DM33DM��DN33DN��DO,�DO�fDP&fDP�fDQ&fDQ��DR�DR�3DS3DS�3DT33DT�3DU,�DU�fDV&fDV� DW  DW�3DX9�DX�3DY&fDY� DZ3DZ�3D[&fD[� D\3D\�3D]33D]��D^&fD^�fD_  D_�3D`3D`�3Da  Da�fDb�Db�3Dc33Dc�fDd  Dd� De3De��Df,�Df�3Dg,�Dg�fDh  Dh� Di  Di��Dj9�Dj��Dk9�Dk�3Dl,�Dl�fDm&fDm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr��Ds  Ds� Dt  Dt� Du  Du��Dv  Dv��Dw3Dw�3Dx3Dx��Dy33Dy�3Dz3Dz�3D{33D{�3D|33D|�3D},�D}�3D~33D~��D,�D�fD� D�S3D�� D��3D� D�S3D��3D��3D�fD�VfD��fD�ٚD�fD�S3D�� D�� D� D�P D���D�ɚD��D�\�D��fD��fD��D�VfD��fD��fD�3D�P D�� D���D��D�P D���D���D��D�\�D���D��fD�fD�VfD��3D��3D� D�P D�� D�� D��D�L�D���D���D��D�VfD���D��fD�fD�P D�� D�� D� D�I�D���D���D��D�VfD��3D�� D��D�L�D���D�ٚD��D�S3D��3D��fD�3D�L�D�� D���D��D�L�D���D���D��D�FfD��fD�ٚD��D�VfD��3D��3D� D�P D�� D�� D��D�L�D���D�ɚD�	�D�I�D���D���D��D�VfD���D��fD�3D�P D�� D���D��D�\�D���D�ٚD�fD�S3D�� D���D��D�VfD�� D�ɚD�	�D�Y�D��fD��fD� D�P D���D��fD��D�S3D��3D��3D��D�I�D���D��fD�3D�S3D�� D�ɚD�fD�S3D���D�ٚD�3D�L�D���D�� D� D�VfD�� D�ɚD��D�P D���D��fD�  D�Y�D��fD�� D�	�D�VfD�� D��fD� D�\�D��3D�ɚD�fD�P D���D��3D��D�S3D���D��3D��D�` D��fD���D�3D�\�D�� D�� D�fD�I�D���D�� D�3D�VfD���D�� D�3D�I�D�� D��3D��D�` D��3D�ɚD��D�VfD���D�� D� D�FfD���D�� D�3D�\�D���D��3D��D�P D�� D��fD��D�` D��3D�ɚD��D�S3D���D�� D�fD�I�D�� D�ٚD��D�FfD���D��fD�  D�VfD�� D��fD�  D�VfD���D��fD��D�VfD���D��fD� D�I�D��3D���D�fD�P D���D�� D��D�S3D���D��fD�3D�\�D��3D�� D��D�S3D�D�ٚD�3D�L�DÖfD��3D��D�VfDĐ D���D�fD�S3DŌ�D�ٚD��D�I�DƖfD�� D��D�\�DǖfD��3D��D�FfDȜ�D��3D�3D�P DɌ�D���D��D�VfDʓ3D���D��D�Y�D˓3D�� D�	�D�Y�D̖fD��fD� D�L�D͉�D��fD�3D�P DΌ�D���D�fD�S3Dό�D�ɚD��D�VfDЖfD��3D� D�L�DцfD�ٚD�3D�P DҌ�D��fD�fD�S3DӐ D�ٚD�3D�S3DԌ�D�ɚD�fD�S3DՌ�D���D�fD�P D֜�D��3D��D�Y�Dד3D���D�fD�L�D؆fD��fD�  D�Y�DٖfD���D�	�D�VfDڌ�D��fD� D�I�DۆfD���D��D�L�D܆fD���D��D�P D݉�D��fD��D�FfDސ D��fD�  D�VfDߌ�D��3D�fD�\�D�� D�ɚD��D�S3D��D���D�fD�P D�fD���D�  D�P D�fD�� D�fD�Y�D� D��fD� D�S3D� D�� D�fD�L�D� D��fD��D�S3D��D�� D�fD�I�D�3D�ɚD�fD�\�D� D��fD� D�S3D� D�� D�	�D�S3D��D��3D��D�VfD��D��fD��D�Y�D� D��fD��D�VfD��D��fD� D�Y�D� D�ɚD�3D�L�D��fD�� D��D�P D�fD��3D��D�P D� D��3D��D�S3D�D��3D�  D�VfD��D��fD� D�FfD���D��3D��D�P D��fD�� D�fD�\�D��3D���D�fD�P D��fD���D��D�I�D��fD��3D�  D�� D� D��fD���D���D��3E nfE  E� EfE��E<�E��EP EٚE�EvfE��E�3E3E��E	�3E
,�E
��E;3E��ENfEc3E� Es3E  E� E�E  E��E( E� E4�EI�E�fEVfE�fEh E�E3E�fE�E�3E!�E)�E� E&fE�fE)�E$�E��E3E �E ��E!  E!�E"c3E"��E#ٚE$L�E$�fE%�3E&#3E&��E'�3E'�3E(a�E)FfE)��E*� E+fE+�E,T�E,��E-�fE.fE.�E/^fE0A�E0��E1�fE1�3E2ffE3X E3��E4C3E5+3E5��E6� E6��E7p E8Y�E8�fE9A�E:33E:�fE;��E;��E<nfE=S3E=�3E>��E?fE?��E@9�EA�EAa�EB+3EB�ECL�ED�ED��EE!�EE��EF�3EG  EG��EHc3EI9�EI� EJs3EJ��EK�3EL  EL�fEM{3EN8 EN�3EONfEP3EPX EQfEQ�fER�fER�ES�3ET<�EU  EU� EVh EV�fEW;3EX EX~fEY[3EY� EZ� EZ�fE[�fE\9�E]fE]� E^8 E^�fE_\�E`�E`�3Ea��Eb.fEb|�Ec&fEcɚEdnfEe Ee��EfY�Ef�3Eg� Eh33Eh� Ei\�Ej4�Ej� EkY�Ek�fEl��Em�Em�En{3En��Eo�3EpT�EpٚEq��ErfEr�3Esk3Es�3EtɚG�O�G�O�G�O�G�O�G�O�?��G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�G�O�?333G�O�?fffG�O�?���?�33?���?ٙ�@   @ff@��@,��@Fff@`  @y��@���@�ff@�  @���@�  @���@�ff@�33@�33A��A  A  AffAffA&ffA,��A6ffA@  AFffAQ��AX  A`  Ai��Ap  Ay��A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441444414444441414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ ^@ �@ �@ {@ �@ #�@ )�@ /@ 6�@ >�@ F�@ S�@ _�@ l�@ z3@ �7@ ��@ ��@ �~@ ��@ ��@ ��@ ��@ �q@@@�@-@9X@G�@V�@b�@p�@�W@�P@�H@��@��@@�7@��@�4@��@�@�@""@0x@>�@K@X@ff@s_@�d@��@��@�M@�R@��@Ӡ@��@�L@��@	�@�@&�@4�@@�@M�@\)@i!@x�@�+@�@��@�f@�@ȴ@�[@�@�Y@  @�@�@)�@5�@C�@SI@`�@n�@|?@��@��@��@�~@�w@�|@܀@�y@�q@j@b@g@-�@:�@G�@V�@dZ@p�@~�@��@�<@�A@��@Ĝ@є@��@�@�~@�@�@$.@0x@>�@K�@X@g�@t�@��@��@��@�M@�R@�W@�O@��@�@@�E@�@�@&;@1�@@,@N�@^5@i�@v@�p@��@�m@�r@��@��@խ@�`@�@@V@�@(�@7L@E�@T�@_�@k�@y�@��@�0@��@�-@��@��@�#@�@�@	j@	@	g@	-@	;d@	I@	Wb@	e	@	r@	�@	�P@	��@	�M@	��@	��@	�7@	ލ@	��@	��@
1@
�@
"�@
.l@
<@
Ji@
X@
e�@
t@
��@
�@
��@
�@
��@
��@
�C@
��@
�@
��@
=@�@%�@4�@@�@M$@\)@k.@z3@�|@�@�@�r@�k@ȴ@׹@�@�@�Q@�@�@+�@7L@C�@Q�@`B@n�@|?@��@��@�5@�!@��@��@�#@��@� @�@o@ �@-@:�@Ji@V@e	@r�@�W@�P@��@��@�F@��@��@ލ@�4@�~@�@@ �@.l@;d@M$@Yn@g@t@�@�@�@�Y@�R@Ĝ@խ@�@��@�9@1@�@$�@1'@B8@Q=@\�@i�@z3@��@�h@��@��@��@�@�
@�@�@�Q@�@�@(G@5?@E�@Q�@^�@oF@{�@��@��@��@��@��@��@�@�@�q@@�@g@,`@8�@E�@SI@c�@o�@}�@��@��@��@��@��@��@܀@�@@��@1@*@"�@/@<�@I�@X@e	@r@��@�h@�@�@��@ƨ@��@�@�L@�E@�@�@&�@3�@@�@M�@[z@i!@v�@�p@��@��@�@�j@�@׹@�`@�Y@ �@�@�@)�@7L@D�@SI@`A@n�@{�@��@�<@��@��@��@�|@�#@�y@�q@�@�@g@,`@9X@G�@UU@bN@o�@}�@�D@�<@��@��@Ĝ@�C@��@��@�9@1@�@"�@0x@>@K�@X@e�@r�@�W@�h@�@��@��@ƨ@�O@�H@��@�9@�@�@&;@33@?}@P�@\�@i�@v@��@��@��@��@�j@�c@խ@�T@�e@  @V@�@'�@8�@D�@Q�@_�@k�@}�@��@�<@�5@�-@�&@��@�t@�m@�~@%@�@ �@-�@:�@H]@UU@b�@p�@~K@��@��@��@��@@�7@�/@�@�,@�@{@""@/@=q@Ji@Wb@e	@r�@�p@�h@��@�M@�^@�@խ@�T@��@��@J@�@&�@4�@A�@N�@\�@i�@x&@�@�u@�@��@�@��@�h@�@�@ �@�@O@(�@6�@C�@P�@bN@o�@x�@�|@�<@�4@��@��@�|@�t@�@�@�@@
@+�@9X@Ji@X@`�@n�@�@��@�H@�A@��@@�7@�/@��@��@%@6@#�@1�@>�@Lu@X�@ff@t@��@��@��@�f@�^@�W@�O@�H@�@@��@�@�@'�@3�@A�@O�@\�@i!@ww@�p@�@��@��@�w@�@��@�@�e@ @ @ �@ )�@ 6�@ DD@ Q�@ _�@ l�@ z3@ ��@ ��@ �z@ �!@ ��@ ψ@ �/@ �y@ �~@!�@!�@!�@!,`@!9X@!F�@!X@!e	@!r�@!�@!��@!��@!��@!��@!��@!�7@!܀@!�(@!�9@"1@"�@"""@"/�@"<@"I@"Z�@"g@"t�@"�d@"��@"��@"�M@"��@"ƨ@"�O@"�H@"�@"��@#
�@#6@#'�@#3�@#@,@#Q=@#\)@#i�@#x�@#�@#�i@#�z@#�@#�^@#��@#�t@#�@#�@$  @$J@$�@$(�@$4�@$DD@$T�@$`A@$k�@$|?@$��@$��@$��@$�9@$��@$�@$�#@$�(@$�,@%�@%b@%g@%/@%=q@%G�@%SI@%a�@%o�@%~K@%��@%��@%��@%�R@%�>@%��@%��@%�4@%�9@&
=@&*@& �@&/@&>�@&M�@&\)@&ff@&r@&�@&�\@&��@&�f@&��@&�>@&��@&�H@&��@&��@'J@'O@'&;@'1�@'@,@'O0@'^5@'m:@'x�@'��@'��@'�z@'�f@'��@'ȴ@'�h@'�@'�@(  @(@(�@(*S@(5�@(E�@(T�@(`�@(l�@(|?@(��@(��@(��@(�9@(��@(��@(�/@(�@(�@)@)b@)�@)-@)<�@)H]@)UU@)e�@)qS@)}�@)��@)�H@)��@)�F@)�>@)ψ@)�;@)�@)�~@*1@**@*!s@*1�@*<�@*I�@*Z@*ff@*s_@*�p@*��@*��@*��@*�F@*ȴ@*�O@*��@*��@*��@+�@+�@+&�@+3�@+@,@+Q=@+^5@+j@+ww@+��@+��@+��@+�r@+��@+ȴ@+խ@+�@+�@,  @,�@,
@,*S@,7L@,C�@,P�@,bN@,n�@,|?@,�7@,�0@,�(@,�r@,�2@,�|@,�t@,�m@,�@-�@-�@-�@-.l@-:�@-H]@-T�@-a�@-r@-~�@-�D@-�U@-��@-��@-�J@-��@-�/@-�@-��@.%@.�@.!s@.-�@.>�@.N�@.Z�@.g�@.s_@.�W@.��@.�U@.��@.�R@.Ĝ@.є@.��@.��@.��@/1@/6@/'�@/33@/?}@/O�@/[z@/g�@/ww@/�|@/�0@/��@/�f@/�j@/��@/��@/�@/��@/�Q@0V@0
@0(G@04�@0DD@0SI@0bN@0p�@0z�@0�|@0�0@0�4@0��@0�&@0��@0�t@0��@0�,@1j@1@1
@1,`@1;d@1Ji@1V@1bN@1p�@1�@1��@1��@1��@1�F@1�J@1�7@1��@1�@1��@2
=@2{@2 �@20x@2@,@2K�@2X@2g�@2s_@2�@2��@2�@2��@2�F@2�J@2��@2��@2��@2��@3J@3�@3$/@33�@3@,@3Lu@3\)@3k�@3ww@3�@3�u@3�z@3�@3��@3�@3��@3�`@3��@4 �@4@4�@4(G@47�@4DD@4O�@4^�@4m�@4|�@4��@4�#@4��@4��@4��@4�|@4��@4�y@4��@5]@5b@5 �@5+@57�@5H]@5X�@5p�@5��@5��@66@6Q=@6��@6ȴ@7�@7DD@7�@7��@7�9@85@@8p�@8�Z@9"�@9[z@9��@9�*@:1@:B�@:��@:��@;+@;dZ@;��@;��@<O�@<��@<��@=  @=:@=r�@=�@>g@>X�@>��@>�o@?A�@?z2@?�9@?�@@@(�@@c�@@��@A�@AM$@A�|@A��@B0x@Bff@B�T@B��@C
�@Cv@C�M@C�;@DI�@D{�@D�@E*@EE�@Ey�@E�`@F�@FJi@F�r@F�;@GV@Guk@G�4@G�O@H5�@He	@H��@H�@IV@I��@I��@J�@J@,@J�@Jψ@K0x@K\�@K�@K�y@L�@L��@L��@L�@MI@My�@M��@NV@N@�@N��@N�[@O�@On�@O��@P@P1�@PbN@P��@P�L@QP�@Q}�@Q�@R �@RWb@R~�@R��@S)�@SP�@S�z@S��@T�@ToF@T�W@T�Y@UO1@U|?@U׹@Vj@V]�@V�+@V�;@W�@WX@W��@W�,@X @Xo�@X�@X�H@Y/@Y}�@Y��@Y�E@Z]�@Z��@Z�@[�@[ww@[�(@[є@\/�@\[z@\��@\�@]@�@]l�@]ȴ@]�@^M�@^��@^��@_!s@_I�@_��@_�y@`3�@`}�@`�@`�l@a-@as_@a��@a��@bD�@b��@b��@c@cN�@c��@c�(@d(�@dg@d��@d�`@e$/@e�d@e�@e�@fLu@f�+@f��@g@gJi@g��@g�h@ho@hm�G�O�G�O�G�O�G�O�G�O�@  �G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�G�O�@ ^G�O�@ �G�O�@ @ %@ �@ 1@ 
=@ 
�@ �@ @ �@ {@ 6@ �@ �@ �@ !s@ %�@ (G@ *S@ -@ 0x@ 3�@ 6�@ :@ <�@ @,@ C�@ FQ@ Ji@ N�@ Q=@ V@ X�@ \)@ `B@ b�@ g@ j@ m�@ rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΉ7A΅A΋DA΍PAΏ\AΓuAΗ�AΛ�AΙ�AΙ�AΙ�AΗ�AΗ�AΛ�AΙ�AΛ�AΝ�AΝ�AΡ�AΣ�AΣ�AΥ�AΧ�AΧ�AΧ�AΧ�AΩ�AΩ�AάAάAήAΰ!Aΰ!Aβ-Aβ-Aδ9Aδ9AζFAζFAζFAθRAθRAκ^Aδ9AΣ�AΡ�AΡ�AΙ�A΋DA�p�A�r�A�hsA�A��A��mA��A��#A�Ać+A�z�AhA�ffA��hA�jA��A�
=A�z�A���A�
=A�z�A�ƨA�?}A��A��A���A�hsA��jA�;dA�ȴA�z�A�?}A��
A��A�A�A�ffA�A���A��#A�ƨA�v�A�G�A��FA��;A��A�
=A��HA�/A���A��#A��9A�;dA�dZA�(�A�-A���A�XA��mA��FA��`A�p�A�M�A��A�|�A���A���A�A��A�K�A��^A���A�E�A�\)A}�PAw�#Aux�Ap��An=qAkC�Ah �Act�A_��AY�^AV �AP�`AN�AK�mAH�uAG%AF�AE�#ADbNAB��AA��A?�A>9XA<A�A9��A97LA8I�A6�A4M�A3oA2bNA0ZA.z�A.(�A-��A,ȴA,~�A,1'A+�TA*~�A)x�A(Q�A&��A&=qA$�A#XA#�A#A"M�A!��A!�-A!l�A ��A ��A �A (�A��A33A�RA9XAx�A�HA��A��A��A��A�jA�jA�9A�!A��AVA��A��AA�At�A1'AJA�9A  A�AG�A^5A�AK�AA
z�A	"�A\)Av�An�A�
A��A�PAXA?}A�AA�A��A=qA�AhsA �A �jA ��A r�A ffA @��P@���@�$�@���@�X@�?}@���@�bN@��
@�"�@���@�Q�@��;@��H@���@�M�@�o@��;@���@�@���@�X@�z�@��m@��`@�X@���@�7@��@��@��`@��/@�j@�Ĝ@��/@�dZ@�V@�Z@�\)@�-@��
@���@�-@�O�@�D@��@���@�\@�^5@��@�G�@�&�@��@��@���@ޟ�@�v�@���@��@��
@�{@١�@� �@�ff@�x�@�ƨ@�\)@��y@�n�@�%@�t�@�33@υ@�\)@�@ΰ!@�Z@��@�C�@��@� �@�b@�Z@�r�@� �@�(�@�(�@���@�+@�-@�x�@���@�1@ǅ@�;d@�=q@��@���@��/@�b@Õ�@�l�@�+@��@���@���@���@���@��7@�hs@�%@��`@��@�b@��P@�;d@�33@��@���@���@��+@�n�@�M�@�J@�p�@���@���@��j@��j@���@��u@�  @���@�l�@�C�@��y@�J@�p�@���@� �@��w@��@���@�dZ@�S�@�C�@���@�v�@�hs@��@���@���@��@��u@�z�@�j@�r�@�1'@�ƨ@�33@���@��y@��@��@���@���@��-@���@�?}@��9@�Q�@��
@���@�\)@�+@��@��R@�{@���@�x�@��9@��D@�t�@��@�@��-@�?}@���@�Q�@�1@��;@��F@�dZ@�o@���@���@���@�A�@� �@�b@�1@��;@��P@�"�@��y@��y@��H@�ȴ@��!@�$�@��@�1'@��@�dZ@�K�@�S�@��@���@�^5@���@���@���@�r�@�1@��m@��w@��@�t�@�C�@�
=@���@���@�v�@�5?@��@��#@���@�hs@��j@�z�@�I�@��m@�"�@�ȴ@�v�@�M�@��@���@��@��^@���@��7@��@��@��j@�z�@���@��P@�|�@�dZ@�\)@��@��@��H@��@��R@�V@��T@���@�?}@���@���@�j@�b@�;d@�ff@�5?@��@��#@��-@��@�G�@��@�9X@���@�33@��@��@���@���@���@�E�@�@��#@���@���@���@��^@���@�Ĝ@�j@��@��@���@�|�@�K�@��@��H@���@��!@���@���@���@���@���@�~�@�^5@��@���@��@�&�@���@��D@�Z@�b@��
@�\)@���@�V@���@���@�/@��9@��D@��@�A�@�@K�@~��@~�y@~v�@}p�@}?}@|�@|1@{C�@{"�@{o@z�@z�H@z�!@z��@z�\@z=q@y�#@y�#@y�^@y��@y��@yx�@yhs@x�`@x�u@xb@wl�@u��@up�@t�@tz�@t1@s��@s�F@sS�@so@r��@r�\@r=q@r�@q��@q�^@q�7@qX@p�9@p�u@p�@pA�@pb@pb@o�@o�;@o��@o�@nv�@n{@m�T@mV@lz�@k�m@k33@jn�@j�@i�@i��@i�^@i��@i�7@hb@gl�@f�@fv�@fV@f5?@e@d�/@d9X@c��@cƨ@c��@co@a�^@a%@`�u@`r�@`bN@`1'@_�w@_
=@^v�@^@]�T@]�@]`B@]`B@]?}@[S�@Zn�@Y�#@Y��@Y�7@Yx�@YX@XA�@W|�@W;d@Vȴ@VE�@V$�@V{@V{@U�@U�T@U��@U?}@T�/@T9X@S��@So@R�@R�\@R^5@R^5@R-@Q��@Q��@Q�^@Q�7@Q�@PbN@O�@O�@O�;@O��@O�@O�@OK�@N��@Nȴ@N�R@N��@N��@N��@N�+@Nff@Nff@N@M�h@Mp�@MO�@M/@M�@MV@L��@L��@L9X@L1@Kƨ@K��@K�@Kt�@KS�@KC�@K@J�H@J�H@J�!@J�!@J~�@J�@I��@I��@I&�@H�u@H �@HA�@G��@FE�@E�@D��@D(�@C��@CC�@BM�@A��@A��@AX@A7L@A&�@@A�@?�@?�@?\)@?
=@>V@=�-@=p�@=O�@=V@<�@<��@<��@<�@<��@<�D@<Z@<I�@<9X@<1@;�
@;ƨ@;ƨ@;��@;C�@;33@;"�@;"�@;@:�@:��@:~�@:M�@:-@:�@:�@:�@9��@9��@9�^@9��@97L@8��@8��@8Ĝ@8�9@8�9@8�u@7��@7�P@6��@6��@6V@5�-@4�@4Z@49X@4�@41@3��@3ƨ@3�F@3t�@3C�@2��@1��@1��@1�@1�#@1��@0�9@/�@/�@/|�@/K�@/�@.�@.��@.�+@.5?@-O�@-V@,�D@+�m@+dZ@+S�@+C�@+33@+"�@+o@*��@*�!@*��@*��@*�\@*^5@)��@)&�@(�`@(��@(�9@(��@(��@(r�@'�;@'�P@'l�@';d@&�y@&�R@&��@%�@%@%�-@%/@%�@%�@%�@%�@$��@$��@$�@$�D@$z�@$1@#�@"�H@"��@"�!@"�\@"=q@!��@!G�@!%@ �@ Q�@ Q�@  �@   @�;@��@��@��@K�@;d@��@ȴ@v�@�-@V@��@�@j@�m@ƨ@ƨ@ƨ@�F@�F@��@S�@33@@M�@��@7L@�`@�9@Q�@�;@�@�P@;d@�@�y@ȴ@�R@��@��@v�@$�@�-@O�@?}@��@�/@�D@�D@z�@9X@�F@��@S�@C�@33@"�@�H@��@~�@�\@n�@�@��@��@�`@��@Ĝ@�9@�@�u@�u@r�@r�@r�@r�@r�@bN@r�@r�@A�@�@�;@�@�@�@�@�@�w@�@�P@K�@�@
=@��@�y@ȴ@��@�+@ff@5?@$�@$�@{@{@@@�T@@��@?}@�D@"�@
�H@
M�@	��@	��@	7L@��@ �@|�@|�@
=@�y@@��@?}@9X@ƨ@t�@�\@��@ �@ Q�?��;?�5??�5??�{?���?��D?���?���?���?��+?�?�`B?���?�t�?�n�?��?��;?�\)??�V?�~�?�^5?�u?�b?��y?�+?���?�j?��/?�j?䛦?�j?䛦?�Z?�Z?��?���?�n�?�Ĝ?߾w?ޗ�?�{?�ƨ?�dZ?���?���?���?ش9?ش9?���?ش9?ش9?ش9?�ff?��
?�S�?Ұ!?���?�J?Ѓ?� �?Ͼw?��?��?�{?��?Ͳ-?�p�?�O�?�ƨ?�dZ?�x�?ə�?�x�?�X?�7L?��?���?�r�?���?ǍP?�K�?�
=?Ł?š�?�?}?�?}?���?��/?�9X?��?��
?�S�?°!?°!?�n�?���?���?�G�?��7?�G�?�&�?�A�?�A�?�bN?�A�?��w?� �?�  ?���?��?�A�?�A�?��?�bN?���?��?�bN?���?��?��?�A�?�\)?�v�?�V?�5??�5??�5??�5??�{?���?��-?��h?��h?��h?��D?�1?�1?�1?�1?��m?��?�dZ?��?��?��?���?��?��?��?��?��?��#?��^?�7L?�X?�X?�7L?�X?�x�?�x�?�x�?���?��^?��^?���?��?�=q?�~�A΍PA΍PA΋DAΉ7AΉ7A΋DAΉ7AΉ7AΏ\A΋DA΋DAΉ7A΋DA΋DAΉ7A·+A΅A΁A�|�A�~�A΁A΃A΅A΅A΅AΏ\A΋DAΉ7A΋DA΍PA΍PAΏ\A΍PAΏ\AΑhAΏ\AΕ�AΕ�AΗ�AΙ�AΛ�AΙ�AΙ�AΙ�AΙ�AΗ�AΗ�AΙ�AΙ�AΙ�AΗ�AΗ�AΗ�AΗ�AΙ�AΗ�AΙ�AΛ�AΛ�AΛ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AΉ7A΅A΋DA΍PAΏ\AΓuAΗ�AΛ�AΙ�AΙ�AΙ�AΗ�AΗ�AΛ�AΙ�AΛ�AΝ�AΝ�AΡ�AΣ�AΣ�AΥ�AΧ�AΧ�AΧ�AΧ�AΩ�AΩ�AάAάAήAΰ!Aΰ!Aβ-Aβ-Aδ9Aδ9AζFAζFAζFAθRAθRAκ^Aδ9AΣ�AΡ�AΡ�AΙ�A΋DA�p�A�r�A�hsA�A��A��mA��A��#A�Ać+A�z�AhA�ffA��hA�jA��A�
=A�z�A���A�
=A�z�A�ƨA�?}A��A��A���A�hsA��jA�;dA�ȴA�z�A�?}A��
A��A�A�A�ffA�A���A��#A�ƨA�v�A�G�A��FA��;A��A�
=A��HA�/A���A��#A��9A�;dA�dZA�(�A�-A���A�XA��mA��FA��`A�p�A�M�A��A�|�A���A���A�A��A�K�A��^A���A�E�A�\)A}�PAw�#Aux�Ap��An=qAkC�Ah �Act�A_��AY�^AV �AP�`AN�AK�mAH�uAG%AF�AE�#ADbNAB��AA��A?�A>9XA<A�A9��A97LA8I�A6�A4M�A3oA2bNA0ZA.z�A.(�A-��A,ȴA,~�A,1'A+�TA*~�A)x�A(Q�A&��A&=qA$�A#XA#�A#A"M�A!��A!�-A!l�A ��A ��A �A (�A��A33A�RA9XAx�A�HA��A��A��A��A�jA�jA�9A�!A��AVA��A��AA�At�A1'AJA�9A  A�AG�A^5A�AK�AA
z�A	"�A\)Av�An�A�
A��A�PAXA?}A�AA�A��A=qA�AhsA �A �jA ��A r�A ffA @��P@���@�$�@���@�X@�?}@���@�bN@��
@�"�@���@�Q�@��;@��H@���@�M�@�o@��;@���@�@���@�X@�z�@��m@��`@�X@���@�7@��@��@��`@��/@�j@�Ĝ@��/@�dZ@�V@�Z@�\)@�-@��
@���@�-@�O�@�D@��@���@�\@�^5@��@�G�@�&�@��@��@���@ޟ�@�v�@���@��@��
@�{@١�@� �@�ff@�x�@�ƨ@�\)@��y@�n�@�%@�t�@�33@υ@�\)@�@ΰ!@�Z@��@�C�@��@� �@�b@�Z@�r�@� �@�(�@�(�@���@�+@�-@�x�@���@�1@ǅ@�;d@�=q@��@���@��/@�b@Õ�@�l�@�+@��@���@���@���@���@��7@�hs@�%@��`@��@�b@��P@�;d@�33@��@���@���@��+@�n�@�M�@�J@�p�@���@���@��j@��j@���@��u@�  @���@�l�@�C�@��y@�J@�p�@���@� �@��w@��@���@�dZ@�S�@�C�@���@�v�@�hs@��@���@���@��@��u@�z�@�j@�r�@�1'@�ƨ@�33@���@��y@��@��@���@���@��-@���@�?}@��9@�Q�@��
@���@�\)@�+@��@��R@�{@���@�x�@��9@��D@�t�@��@�@��-@�?}@���@�Q�@�1@��;@��F@�dZ@�o@���@���@���@�A�@� �@�b@�1@��;@��P@�"�@��y@��y@��H@�ȴ@��!@�$�@��@�1'@��@�dZ@�K�@�S�@��@���@�^5@���@���@���@�r�@�1@��m@��w@��@�t�@�C�@�
=@���@���@�v�@�5?@��@��#@���@�hs@��j@�z�@�I�@��m@�"�@�ȴ@�v�@�M�@��@���@��@��^@���@��7@��@��@��j@�z�@���@��P@�|�@�dZ@�\)@��@��@��H@��@��R@�V@��T@���@�?}@���@���@�j@�b@�;d@�ff@�5?@��@��#@��-@��@�G�@��@�9X@���@�33@��@��@���@���@���@�E�@�@��#@���@���@���@��^@���@�Ĝ@�j@��@��@���@�|�@�K�@��@��H@���@��!@���@���@���@���@���@�~�@�^5@��@���@��@�&�@���@��D@�Z@�b@��
@�\)@���@�V@���@���@�/@��9@��D@��@�A�@�@K�@~��@~�y@~v�@}p�@}?}@|�@|1@{C�@{"�@{o@z�@z�H@z�!@z��@z�\@z=q@y�#@y�#@y�^@y��@y��@yx�@yhs@x�`@x�u@xb@wl�@u��@up�@t�@tz�@t1@s��@s�F@sS�@so@r��@r�\@r=q@r�@q��@q�^@q�7@qX@p�9@p�u@p�@pA�@pb@pb@o�@o�;@o��@o�@nv�@n{@m�T@mV@lz�@k�m@k33@jn�@j�@i�@i��@i�^@i��@i�7@hb@gl�@f�@fv�@fV@f5?@e@d�/@d9X@c��@cƨ@c��@co@a�^@a%@`�u@`r�@`bN@`1'@_�w@_
=@^v�@^@]�T@]�@]`B@]`B@]?}@[S�@Zn�@Y�#@Y��@Y�7@Yx�@YX@XA�@W|�@W;d@Vȴ@VE�@V$�@V{@V{@U�@U�T@U��@U?}@T�/@T9X@S��@So@R�@R�\@R^5@R^5@R-@Q��@Q��@Q�^@Q�7@Q�@PbN@O�@O�@O�;@O��@O�@O�@OK�@N��@Nȴ@N�R@N��@N��@N��@N�+@Nff@Nff@N@M�h@Mp�@MO�@M/@M�@MV@L��@L��@L9X@L1@Kƨ@K��@K�@Kt�@KS�@KC�@K@J�H@J�H@J�!@J�!@J~�@J�@I��@I��@I&�@H�u@H �@HA�@G��@FE�@E�@D��@D(�@C��@CC�@BM�@A��@A��@AX@A7L@A&�@@A�@?�@?�@?\)@?
=@>V@=�-@=p�@=O�@=V@<�@<��@<��@<�@<��@<�D@<Z@<I�@<9X@<1@;�
@;ƨ@;ƨ@;��@;C�@;33@;"�@;"�@;@:�@:��@:~�@:M�@:-@:�@:�@:�@9��@9��@9�^@9��@97L@8��@8��@8Ĝ@8�9@8�9@8�u@7��@7�P@6��@6��@6V@5�-@4�@4Z@49X@4�@41@3��@3ƨ@3�F@3t�@3C�@2��@1��@1��@1�@1�#@1��@0�9@/�@/�@/|�@/K�@/�@.�@.��@.�+@.5?@-O�@-V@,�D@+�m@+dZ@+S�@+C�@+33@+"�@+o@*��@*�!@*��@*��@*�\@*^5@)��@)&�@(�`@(��@(�9@(��@(��@(r�@'�;@'�P@'l�@';d@&�y@&�R@&��@%�@%@%�-@%/@%�@%�@%�@%�@$��@$��@$�@$�D@$z�@$1@#�@"�H@"��@"�!@"�\@"=q@!��@!G�@!%@ �@ Q�@ Q�@  �@   @�;@��@��@��@K�@;d@��@ȴ@v�@�-@V@��@�@j@�m@ƨ@ƨ@ƨ@�F@�F@��@S�@33@@M�@��@7L@�`@�9@Q�@�;@�@�P@;d@�@�y@ȴ@�R@��@��@v�@$�@�-@O�@?}@��@�/@�D@�D@z�@9X@�F@��@S�@C�@33@"�@�H@��@~�@�\@n�@�@��@��@�`@��@Ĝ@�9@�@�u@�u@r�@r�@r�@r�@r�@bN@r�@r�@A�@�@�;@�@�@�@�@�@�w@�@�P@K�@�@
=@��@�y@ȴ@��@�+@ff@5?@$�@$�@{@{@@@�T@@��@?}@�D@"�@
�H@
M�@	��@	��@	7L@��@ �@|�@|�@
=@�y@@��@?}@9X@ƨ@t�@�\@��@ �@ Q�?��;?�5??�5??�{?���?��D?���?���?���?��+?�?�`B?���?�t�?�n�?��?��;?�\)??�V?�~�?�^5?�u?�b?��y?�+?���?�j?��/?�j?䛦?�j?䛦?�Z?�Z?��?���?�n�?�Ĝ?߾w?ޗ�?�{?�ƨ?�dZ?���?���?���?ش9?ش9?���?ش9?ش9?ش9?�ff?��
?�S�?Ұ!?���?�J?Ѓ?� �?Ͼw?��?��?�{?��?Ͳ-?�p�?�O�?�ƨ?�dZ?�x�?ə�?�x�?�X?�7L?��?���?�r�?���?ǍP?�K�?�
=?Ł?š�?�?}?�?}?���?��/?�9X?��?��
?�S�?°!?°!?�n�?���?���?�G�?��7?�G�?�&�?�A�?�A�?�bN?�A�?��w?� �?�  ?���?��?�A�?�A�?��?�bN?���?��?�bN?���?��?��?�A�?�\)?�v�?�V?�5??�5??�5??�5??�{?���?��-?��h?��h?��h?��D?�1?�1?�1?�1?��m?��?�dZ?��?��?��?���?��?��?��?��?��?��#?��^?�7L?�X?�X?�7L?�X?�x�?�x�?�x�?���?��^?��^?���?��?�=q?�~�A΍PA΍PA΋DAΉ7AΉ7A΋DAΉ7AΉ7AΏ\A΋DA΋DAΉ7A΋DA΋DAΉ7A·+A΅A΁A�|�A�~�A΁A΃A΅A΅A΅AΏ\A΋DAΉ7A΋DA΍PA΍PAΏ\A΍PAΏ\AΑhAΏ\AΕ�AΕ�AΗ�AΙ�AΛ�AΙ�AΙ�AΙ�AΙ�AΗ�AΗ�AΙ�AΙ�AΙ�AΗ�AΗ�AΗ�AΗ�AΙ�AΗ�AΙ�AΛ�AΛ�AΛ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
+B
}�B
��B
��B
��B
��B
�wB
��B
�B
�ZB
�BDB.BA�BA�BW
BjBu�B}�Bx�B�B�DB�bB��B�B�3B�?B�?B�RBÖB�B�yB�TB�)BɺB�#B�)B�sB��B  B��B�B�qB�B��B��B��B�{Bl�BE�B:^B5?B+B%�B#�BbB
��B
�fB
��B
��B
�hB
v�B
l�B
O�B
33B
,B
(�B
"�B
�B
DB	�sB	�
B	�3B	��B	��B	|�B	m�B	O�B	�B�B�mB�B�
B��B��B��BɺB��B��B��BŢBÖB�qB�qB�}B�RB�XB�FB�3B�!B�B�jB��B��B��BB��BBŢBŢB��B��B��B��B��B��B��B�B�#B�#B�#B�5B�HB�HB�ZB�mB�B�B��B	B	VB	bB	\B	\B	\B	bB	bB	bB	hB	hB	�B	�B	�B	!�B	#�B	&�B	�B	hB	hB	JB	
=B�B�B�B��B�B�B�B��B��B�B�B��B��B	B	B	B	B	B	PB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	(�B	,B	.B	.B	0!B	49B	2-B	33B	49B	33B	/B	,B	49B	+B	'�B	(�B	'�B	%�B	%�B	.B	>wB	A�B	D�B	K�B	_;B	n�B	y�B	z�B	z�B	{�B	|�B	|�B	z�B	z�B	z�B	x�B	}�B	}�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�=B	�7B	�7B	�+B	�%B	�+B	�7B	�DB	�7B	�=B	�DB	�bB	�hB	�{B	�uB	�VB	�DB	�DB	�PB	�bB	�oB	�oB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�'B	�!B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�'B	�-B	�-B	�?B	�FB	�RB	�^B	�jB	�qB	�qB	�qB	�wB	�}B	��B	��B	��B	B	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�
B	�
B	�B	�#B	�/B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�B	�B	�mB	�yB	�yB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B
	7B
DB
DB
JB
JB
JB
JB
JB
DB
PB
JB
PB
PB
PB
VB
VB
VB
VB
\B
bB
\B
bB
\B
bB
hB
hB
hB
uB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
#�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
,B
,B
,B
-B
-B
-B
-B
.B
-B
-B
.B
-B
.B
.B
.B
.B
.B
.B
/B
.B
/B
/B
/B
1'B
1'B
1'B
2-B
1'B
1'B
1'B
1'B
2-B
1'B
1'B
2-B
1'B
2-B
1'B
2-B
33B
2-B
33B
33B
33B
49B
33B
33B
49B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
:^B
:^B
;dB
:^B
:^B
<jB
<jB
=qB
=qB
<jB
=qB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
A�B
A�B
A�B
A�B
C�B
D�B
E�B
E�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
M�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
T�B
T�B
VB
VB
VB
W
B
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
dZB
dZB
ffB
gmB
gmB
gmB
hsB
gmB
gmB
hsB
gmB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
iyB
k�B
k�B
k�B
jB
k�B
jB
k�B
l�B
l�B
l�B
l�B
l�B
l�B
k�B
m�B
m�B
n�B
n�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
q�B
q�B
q�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
v�B
w�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
w�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
|�B
{�B
|�B
}�B
{�B
|�B
}�B
}�B
|�B
|�B
}�B
}�B
}�B
}�B
|�B
~�B
~�B
~�B
~�B
�B
� B
� B
� B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�=B
�7B
�DB
�=B
�=B
�DB
�JB
�JB
�PB
�JB
�VB
�PB
�VB
�\B
�\B
�hB
�bB
�hB
�hB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�!B
�!B
�'B
�'B
�'B
�3B
�-B
�-B
�-B
�9B
�3B
�?B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�RB
�RB
�LB
�LB
�LB
�XB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�qB
�qB
�wB
�}B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
ÖB
B
ÖB
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ĜB
ÖB
ÖB
ÖB
ĜB
ÖB
ĜB
ĜB
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ĜB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B	�HB	�HB	�HB	�BB	�BB	�BB	�BB	�;B	�BB	�BB	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�;B	�BB	�;B	�;B	�;B	�BB	�BB	�BB	�;B	�BB	�BB	�BB	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�5B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�TB
�B
l�B
�B
�7B
�VB
�uB
�B
�^B
ƨB
��B
�HB
��B�B0!B0!BE�BYBdZBl�BgmBp�By�B~�B��B��B��B��B��B��B�-BƨB�B��B��B�RBɺB��B�
B�mB�B�mBǮB�B��B��B��B�hB�B[#B49B(�B#�B�B{BoB
��B
�TB
��B
B
��B
� B
e`B
[#B
>wB
!�B
�B
�B
hB
PB	��B	�
B	ŢB	��B	��B	�=B	k�B	\)B	>wB	1B�NB�BǮBŢB�jB�jB�jB�RB�jB�XB�dB�9B�-B�B�B�B��B��B��B��B��B��B�B�!B�B�!B�'B�!B�'B�9B�9B�^B�dB�wB�qB��B��BBƨBɺBɺBɺB��B��B��B��B�B�B�BB�mB�B��B��B��B��B��B��B��B��B	  B	  B	B	+B	VB	bB	oB	�B	DB	  B	B��B��B�TB�5B�NB�mB�TB�BB�HB�`B�fB�BB�#B�fB�B�B�B�B�B��B��B	%B	1B	
=B	DB	
=B	
=B	
=B	
=B	VB	oB	{B	�B	�B	�B	�B	�B	�B	#�B	!�B	"�B	#�B	"�B	�B	�B	#�B	�B	�B	�B	�B	�B	�B	�B	.B	1'B	49B	;dB	N�B	^5B	iyB	jB	jB	k�B	l�B	l�B	jB	jB	jB	hsB	m�B	m�B	n�B	n�B	p�B	p�B	q�B	q�B	q�B	s�B	t�B	t�B	t�B	v�B	y�B	x�B	x�B	v�B	u�B	v�B	x�B	z�B	x�B	y�B	z�B	� B	�B	�B	�B	}�B	z�B	z�B	|�B	� B	�B	�B	{�B	|�B	�B	�=B	�JB	�PB	�bB	�hB	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�RB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	�}B	�}B	�}B	�}B	�}B	�wB	�wB	�jB	�qB	�qB	�qB	�qB	��B	B	ÖB	ĜB	ƨB	ǮB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�)B	�;B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�NB	�fB	�mB	�sB	�yB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B	��B
  B
B
B
B
B
B
B
B
B
+B
+B
%B
+B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
JB
JB
JB
JB
PB
PB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
oB
oB
oB
oB
uB
uB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
!�B
 �B
 �B
!�B
 �B
!�B
 �B
!�B
"�B
!�B
"�B
"�B
"�B
#�B
"�B
"�B
#�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
)�B
+B
)�B
)�B
,B
,B
-B
-B
,B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
1'B
1'B
1'B
1'B
33B
49B
5?B
5?B
49B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
8RB
:^B
:^B
;dB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
>wB
?}B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
T�B
T�B
T�B
W
B
XB
XB
XB
YB
XB
XB
YB
XB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
ZB
\)B
\)B
\)B
[#B
\)B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
\)B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
`BB
`BB
`BB
`BB
aHB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
bNB
bNB
bNB
cTB
dZB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
e`B
ffB
ffB
ffB
gmB
hsB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
hsB
jB
iyB
iyB
jB
k�B
k�B
k�B
m�B
l�B
m�B
n�B
l�B
m�B
n�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
z�B
y�B
{�B
z�B
z�B
{�B
|�B
|�B
}�B
|�B
~�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�1B
�7B
�DB
�DB
�DB
�DB
�JB
�PB
�PB
�PB
�\B
�\B
�\B
�bB
�hB
�uB
�oB
�{B
�{B
�{B
�{B
��B
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�B
�B
�!B
�!B
�'B
�-B
�'B
�'B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
�9B
�3B
�9B
�9B
�3B
�9B
�9B
�9B
�FB
�?B
�FB
�?B
�FB
�?B
�FB
�FB
�FB
�FB
�LB
�FB
�FB
�FB
�LB
�FB
�LB
�LB
�FB
�FB
�LB
�LB
�LB
�LB
�LB	�HB	�BB	�BB	�NB	�HB	�HB	�HB	�HB	�BB	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�BB	�NB	�HB	�HB	�NB	�HB	�HB	�BB	�HB	�HB	�BB	�HB	�HB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�;B	�;B	�BB	�BB	�BB	�;B	�BB	�BB	�;B	�;B	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�;B	�BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�202001200801002021061413564320210614135643202106141918432021061419184320210614191843202001200801002021061413564320210614135643202106141918432021061419184320210614191843PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9996 (+/-0.0002), vertically averaged dS = -0.017 (+/-0.007)                                                                                                                                                                                           surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9996 (+/-0.0002), vertically averaged dS = -0.017 (+/-0.007)                                                                                                                                                                                           Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2020012008010020200120080100  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020012008010020200120080100QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020012008010020200120080100QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713152720210617131527IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                