CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ,   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-11-22T23:00:36Z creation      
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
resolution        =���   axis      Z        )`  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  e@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )`  o�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )`  �P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )`  ̰   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )`  h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X )�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )` 4    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )` ]�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )` �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )` ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    |   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20191122230036  20210617131526  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               X   XDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @����q��@����q��11  @������`@������`@2�K]�c�@2�K]�c��c\&���c�c\&���c11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @Fff@�33@�  @�33@�33A��A��A(  AA��A`  A~ffA���A�ffA���A���A�  A�33A���B ��BffBffB  B   B(ffB0  B7��B@  BG��BP  BX��B`��BhffBp  Bx  B��B�  B�33B���B���B�33B�33B�  B�  B�ffB�ffB�33B���B�  B�33B�ffB�33B�33B�33B�ffBϙ�B���Bי�B���B���B㙚B�ffB�ffB�33B�33B�  B�ffC 33C�C�C�fC33C
�C�C  C  C  C  C  C�fC�fC  C�C 33C"L�C#�fC&  C(33C)�fC,  C.L�C/�fC2  C4�C5��C7�fC:  C<�C>ffC@�CB33CDL�CF  CH  CJ  CL  CN  CP�CR�CT�CV�CX�CZ�C\  C^  C_�fCbL�Cd33Cf�Ch  Cj  Cl  Cn�Cp�Cr�Ct33Cu��Cw�fCz  C|  C~�C��C��C��C��C��C��C��C��C��C�&fC��3C��3C��C��C��C�  C��3C��C��C�  C��C��C�  C��C��C��3C��C�  C�  C��C��C�  C�&fC��C��C��C�  C��3C��3C��C��C��3C��3C��fC��C��C��3C��3C��fC�  C�  C��fC�  C�&fC��C��C�  C��fC��C�  C��3C��C�  C��fC��C��C�  C��C�&fC��C�  C��C�  C��fC�  C��C�&fC��C��fC�  C�  C�  C�  C�  C�  C��C��C�&fC�&fC�&fC�33C�  C�ٚC�  C�&fC�&fC��C�ٚC��fC��3C�  C��C��C�&fC�&fC��C��fC��3C�  C��C��C�&fC�  C��fC��3C��C��C�&fC��C�  C�  C��C��C��3C�  C��C�ٚC��fC��3D ��D�Dy�D  D��D�3D� D�DffD��D� D3D� D��D��D  D� D	�D	��D	��D
�fD3Dl�D  D��D��Dy�DfDl�D  D��D��D�fD�Dl�D  D��D��Dy�DfDl�D��D� D�D� D  D�3D  Dy�D�D��D��D�fD3Ds3D��D��D3D� D  D�3D�Ds3D �D �3D ��D!y�D"�D"l�D"��D#�fD#� D$s3D$��D%��D&�D&s3D'  D'�fD(�D(��D(��D)�fD*3D*l�D+  D+��D+�fD,� D-�D-��D.&fD.� D/3D/� D/��D0��D13D1y�D2fD2�3D2��D3�fD43D4s3D5  D5��D5��D6� D7fD7ffD7��D8� D93D9� D:  D:��D;3D;s3D<  D<��D<��D=y�D>fD>l�D>�3D?�fD?� D@s3DA  DA��DB3DBl�DB��DC� DD3DD� DD�3DE�fDF�DFs3DG  DG��DG�fDHy�DIfDI�3DJ  DJy�DK  DK��DL3DL�fDL��DM� DN�DNl�DN��DO� DP�DP�3DP��DQs3DQ�3DR�fDSfDS�3DT�DTs3DT��DU�fDVfDV�fDW3DW�3DW�3DXy�DX�3DY� DZfDZ��D[3D[��D\�D\s3D]  D]� D^3D^�3D^�3D_� D`fD`��Da�Dal�Da�3Dbs3Db��Dc� Dd�Dd��Dd�3Des3De��Df�fDgfDgffDg�3Dhy�Dh��Di� DjfDj��Dk3Dk��Dk�3Dly�Dm  Dm�fDnfDn��Do3Dos3Dp  Dp� Dq�Dq��Dq�3Dry�Dr��Ds� DtfDt�fDu�Du��Dv�Dvl�Dv�3Dws3Dw��Dxy�Dx��Dyy�Dy��Dzy�D{  D{� D|fD|�fD}fD}��D~�D~�fD�D�fD�3D�C3D��fD��fD�fD�C3D��3D��3D�3D�C3D�� D��3D�3D�C3D��3D��3D�3D�C3D��3D�� D�  D�<�D�� D��3D�3D�@ D��fD��fD�	�D�I�D��fD��fD�	�D�FfD��fD��fD�	�D�I�D���D�ɚD�	�D�I�D�vfD���D�	�D�L�D�vfD���D���D�9�D�y�D���D���D�<�D�|�D���D�  D�@ D�� D��3D�3D�C3D��fD�ɚD�	�D�6fD�y�D���D�  D�@ D��3D��fD�	�D�FfD���D�� D���D�<�D��3D��3D�fD�I�D���D��3D��fD�I�D���D���D�	�D�L�D���D���D���D�<�D�� D�� D�3D�FfD��fD�ɚD��3D�6fD�vfD���D���D�C3D��3D��3D�fD�I�D���D��fD���D�9�D�|�D���D�  D�C3D��fD�ɚD��fD�9�D�|�D�� D�  D�C3D��3D�ɚD��D�9�D�y�D���D���D�@ D��3D��3D�	�D�I�D���D���D��D�L�D���D�ɚD��fD�6fD�vfD���D���D�<�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D���D���D�6fD���D�ɚD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�FfD��fD��fD�fD�C3D��3D�� D�3D�@ D�� D�� D�  D�<�D�y�D�� D��D�L�D���D�ɚD�	�D�I�D���D�ɚD�fD�FfD�� D��3D�  D�@ D�y�D���D���D�9�D�vfD���D��fD�6fD���D��fD�	�D�I�D��fD��fD�fD�C3D�� D���D�  D�<�D�|�D���D��D�L�D���D�ɚD�	�D�FfD��3D��3D�  D�C3D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD�3D�FfD��fD��fD�fD�FfD���D�ɚD�	�D�FfD��fD��fD�fD�I�D���D��fD�fD�I�D�D�ɚD�fD�FfDÃ3D��3D�3D�@ DĀ D�� D�  D�@ D�|�Dż�D���D�9�D�vfDƹ�D��fD�<�D�y�Dǹ�D���D�9�D�vfDȹ�D���D�9�D�y�Dɹ�D���D�9�Dʌ�D�� D��fD�9�D�y�D˹�D��D�L�Ď�D�ɚD�fD�FfD͉�D��fD�fD�C3D΀ D�� D���D�6fD�y�D��fD�fD�C3DЀ D�� D���D�9�Dь�D�ɚD�3D�@ DҀ D�� D���D�L�DӆfD��fD�3D�@ DԀ DԼ�D���D�9�DՌ�D��fD�3D�@ D�y�D���D�fD�C3D�y�D׹�D�	�D�FfD؆fD�� D�  D�@ D�y�D���D�	�D�FfDچfD��3D�  D�<�D�y�D۶fD�fD�FfD܃3Dܼ�D���D�9�D�vfD��fD�fD�C3D�|�D޹�D��D�L�D߉�D��fD�fD�C3D��3D�� D�3D�@ D� D��D���D�9�D�y�D���D�	�D�L�D㉚D�ɚD�  D�C3D�|�D��D���D�9�D剚D��fD�fD�C3D�3D��D���D�L�D牚D��3D�  D�9�D艚D��fD�  D�<�D�y�D�ɚD�3D�@ D�|�D�fD�fD�@ D�|�D���D�fD�FfD�3D��D���D�6fD�fD�� D�  D�9�D�y�D�ɚD�3D�@ D�vfD�fD�	�D�C3D�� D��D���D�FfD�3D��D��D�I�D�3D��D���D�I�D�3D��3D�  D�9�D�fD�� D�  D�L�D���D��fD�3D�@ D�y�D��fD�fD�@ D�|�D���D�	�D�FfD�|�D���D��D�I�D��fD�� D���D�	�D�,�D�VfE ^fE �3E� E  E�3EK3E�Ey�E�E�3EC3EٚEq�E�E�3E	0 E	��E
�3Ey�EfE��E,�E�3ED�EњE�fEs3E��E��E E��E��E3E��E!�E��E�fE+3E� E1�E6fE��E4�E��E>fEC3E��EI�E�3E�3EP E�fE Y�E ��E!��E"k3E"�3E#x E$��E%�E%��E&fE&� E'� E(&fE(��E).fE)��E*��E+A�E+�3E,D�E,� E-�fE.NfE.�fE/P E0VfE0��E1Q�E1�3E2�fE3VfE3�fE4VfE5VfE5� E6L�E6ɚE7�fE8FfE8��E9<�E:D�E:� E;A�E;��E<� E=<�E=��E>;3E?9�E?��E@33EA4�EA�3EB)�EB� EC��ED�ED�fEE�EF3EF��EGfEH�EH�3EI�EI�3EJ��EK EK�fELfEM�EM��EN�EN��EO�fEP EP� EQ�ER3ER|�ER� ES�fETs3ET� EUk3EVk3EV�fEWd�EW� EX�3EYP EY�3EZ�fE[33E[�3E\��E]fE]��E^q�E^��E_T�E`D�E`��Ea��Eb�Eb{3Ec\�Ec�fEd��Ee3Ee��Ef` Eg>fEg� Eh�Eh�EiY�Ej9�Ej��Ek�3Ek�fEl� Em;3En3En�fEn�3Eo��EpA�Eq( Eq� Erp Er�3Es��Et$�Et�fEuy�Eu� Ev�3Ew33Ex�Ex�3Eyc3Ey�fEz9�E{3E{�3E|^fE|�fE}� E~3E~�3E@ E�
fE�=�E��fE�ݚE�G3E�x�E���E�FfE�x E��fE��E�x�E��fE��E�u�E���E�	�E�9�E��3E� �E�/3E��3E��fE� �E���E�� E� E�| E��3E�
fE�h�E�� E��3E�T�E��3E��3E�E�E���E�� E�73E�� E��3E�, E�[3E��fE�  E�M�E���E�
fE�d E��fE���E�?3E��3E��E� E�p�E��3E�  E�vfE���E�!�E�I�E���E��3E�VfE���E�� E�8�E���E��E�D E�nfE�� E�fE�s3E��3E��E�FfE��fE��3E�E�E��3E�� E�5�E�� E�њE��E�ffE�� E�!�E�l�E��3E�  E�I�E��fE���E�D�E���E�՚E�H E��fE��fE�< E���E�� E��E�p E���E� �?   >���>���>���?   ?   >���>���>���?   ?   ?��?��?333?L��?fff?�  ?���?�ff?�33?���?�ff@ff@��@��@,��@9��@L��@`  @y��@�  @���@�ff@�33@���@�ff@�33@�  @���@�ff@�ffA   A  A��A  A   A&ffA0  A6ffA>ffAD��AL��AT��A\��Ac33Ak33As33Ay��A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441414111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�33@   @fff@�33@�  @�33@�33A	��A��A0  AI��Ah  A�33A���A�ffA���A���A�  A�33A���B��B
ffBffB  B"  B*ffB2  B9��BB  BI��BR  BZ��Bb��BjffBr  Bz  B���B�  B�33B���B���B�33B�33B�  B�  B�ffB�ffB�33B���B�  B�33B�ffB�33B�33B�33B�ffBЙ�B���Bؙ�B���B���B䙚B�ffB�ffB�33B�33B�  B�ffC �3C��C��CffC�3C
��C��C� C� C� C� C� CffCffC� C��C �3C"��C$ffC&� C(�3C*ffC,� C.��C0ffC2� C4��C6L�C8ffC:� C<��C>�fC@��CB�3CD��CF� CH� CJ� CL� CN� CP��CR��CT��CV��CX��CZ��C\� C^� C`ffCb��Cd�3Cf��Ch� Cj� Cl� Cn��Cp��Cr��Ct�3CvL�CxffCz� C|� C~��C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�Y�C�L�C�ffC�33C�33C�Y�C�Y�C�L�C�@ C�33C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�33C�Y�C�@ C�@ C�Y�C�L�C�@ C�ffC�Y�C�Y�C�Y�C�@ C�33C�33C�L�C�Y�C�33C�33C�&fC�L�C�L�C�33C�33C�&fC�@ C�@ C�&fC�@ C�ffC�Y�C�L�C�@ C�&fC�L�C�@ C�33C�L�C�@ C�&fC�L�C�L�C�@ C�Y�C�ffC�Y�C�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�@ C�@ C�@ C�@ C�@ C�L�C�Y�C�ffC�ffC�ffC�s3C�@ C��C�@ C�ffC�ffC�L�C��C�&fC�33C�@ C�L�C�Y�C�ffC�ffC�Y�C�&fC�33C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�L�C�Y�C�ffC�L�C�@ C�@ C�Y�C�L�C�33C�@ C�Y�C��C�&fD �D ��D9�D��D  D��D3D� D,�D�fD�D� D33D� D�D��D@ D� D	,�D	��D
�D
�fD33D��D  D��D�D��D&fD��D  D��D�D�fD,�D��D  D��D�D��D&fD��D�D� D,�D� D  D�3D@ D��D,�D��D�D�fD33D�3D�D��D33D� D  D�3D9�D�3D ,�D �3D!�D!��D",�D"��D#�D#�fD$  D$�3D%�D%��D&9�D&�3D'  D'�fD(,�D(��D)�D)�fD*33D*��D+  D+��D,fD,� D-,�D-��D.FfD.� D/33D/� D0�D0��D133D1��D2&fD2�3D3�D3�fD433D4�3D5  D5��D6�D6� D7&fD7�fD8�D8� D933D9� D:  D:��D;33D;�3D<  D<��D=�D=��D>&fD>��D?3D?�fD@  D@�3DA  DA��DB33DB��DC�DC� DD33DD� DE3DE�fDF,�DF�3DG  DG��DHfDH��DI&fDI�3DJ@ DJ��DK  DK��DL33DL�fDM�DM� DN,�DN��DO�DO� DP,�DP�3DQ�DQ�3DR3DR�fDS&fDS�3DT9�DT�3DU�DU�fDV&fDV�fDW33DW�3DX3DX��DY3DY� DZ&fDZ��D[33D[��D\9�D\�3D]  D]� D^33D^�3D_3D_� D`&fD`��Da,�Da��Db3Db�3Dc�Dc� Dd,�Dd��De3De�3Df�Df�fDg&fDg�fDh3Dh��Di�Di� Dj&fDj��Dk33Dk��Dl3Dl��Dm  Dm�fDn&fDn��Do33Do�3Dp  Dp� Dq,�Dq��Dr3Dr��Ds�Ds� Dt&fDt�fDu,�Du��Dv9�Dv��Dw3Dw�3Dx�Dx��Dy�Dy��Dz�Dz��D{  D{� D|&fD|�fD}&fD}��D~,�D~�fD,�D�fD�3D�S3D��fD��fD�fD�S3D��3D��3D�3D�S3D�� D��3D�3D�S3D��3D��3D�3D�S3D��3D�� D� D�L�D�� D��3D�3D�P D��fD��fD��D�Y�D��fD��fD��D�VfD��fD��fD��D�Y�D���D�ٚD��D�Y�D��fD�ɚD��D�\�D��fD�ɚD�	�D�I�D���D�ɚD�	�D�L�D���D���D� D�P D�� D��3D�3D�S3D��fD�ٚD��D�FfD���D�ɚD� D�P D��3D��fD��D�VfD���D�� D��D�L�D��3D��3D�fD�Y�D���D��3D�fD�Y�D���D���D��D�\�D���D�ɚD��D�L�D�� D�� D�3D�VfD��fD�ٚD�3D�FfD��fD���D��D�S3D��3D��3D�fD�Y�D���D��fD��D�I�D���D���D� D�S3D��fD�ٚD�fD�I�D���D�� D� D�S3D��3D�ٚD��D�I�D���D���D��D�P D��3D��3D��D�Y�D���D���D��D�\�D���D�ٚD�fD�FfD��fD�ɚD�	�D�L�D���D���D��D�L�D���D���D�	�D�L�D���D���D�	�D�FfD���D�ٚD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�VfD��fD��fD�fD�S3D��3D�� D�3D�P D�� D�� D� D�L�D���D�� D��D�\�D���D�ٚD��D�Y�D���D�ٚD�fD�VfD�� D��3D� D�P D���D���D�	�D�I�D��fD�ɚD�fD�FfD���D��fD��D�Y�D��fD��fD�fD�S3D�� D���D� D�L�D���D���D��D�\�D���D�ٚD��D�VfD��3D��3D� D�S3D���D���D� D�P D�� D�� D� D�P D�� D�� D� D�P D��3D��fD�3D�VfD��fD��fD�fD�VfD���D�ٚD��D�VfD��fD��fD�fD�Y�D���D��fD�fD�Y�D�D�ٚD�fD�VfDÓ3D��3D�3D�P DĐ D�� D� D�P DŌ�D���D�	�D�I�DƆfD�ɚD�fD�L�Dǉ�D�ɚD�	�D�I�DȆfD�ɚD�	�D�I�Dɉ�D�ɚD�	�D�I�Dʜ�D�� D�fD�I�Dˉ�D�ɚD��D�\�D̜�D�ٚD�fD�VfD͙�D��fD�fD�S3Dΐ D�� D��D�FfDω�D��fD�fD�S3DА D�� D��D�I�Dќ�D�ٚD�3D�P DҐ D�� D��D�\�DӖfD��fD�3D�P DԐ D���D�	�D�I�D՜�D��fD�3D�P D։�D���D�fD�S3D׉�D�ɚD��D�VfDؖfD�� D� D�P Dى�D���D��D�VfDږfD��3D� D�L�Dۉ�D��fD�fD�VfDܓ3D���D��D�I�D݆fD��fD�fD�S3Dތ�D�ɚD��D�\�Dߙ�D��fD�fD�S3D��3D�� D�3D�P D� D���D�	�D�I�D≚D���D��D�\�D㙚D�ٚD� D�S3D��D���D�	�D�I�D噚D��fD�fD�S3D�3D���D��D�\�D癚D��3D� D�I�D虚D��fD� D�L�D鉚D�ٚD�3D�P D��D��fD�fD�P D��D���D�fD�VfD�3D���D�	�D�FfD�fD�� D� D�I�DD�ٚD�3D�P D�fD��fD��D�S3D� D���D�	�D�VfD�3D���D��D�Y�D�3D���D��D�Y�D�3D��3D� D�I�D��fD�� D� D�\�D���D��fD�3D�P D���D��fD�fD�P D���D���D��D�VfD���D���D��D�Y�D��fD�� D�	�D��D�<�D�ffE ffE �3E� E( E�3ES3E�E��E�E�3EK3E�Ey�E�E�3E	8 E	��E
�3E��EfE��E4�E�3EL�EٚE�fE{3E�E��E E��E��E#3E��E)�E��E�fE33E� E9�E>fE��E<�E��EFfEK3E��EQ�E�3E�3EX E�fE a�E ��E!��E"s3E"�3E#� E$��E%�E%��E&fE&� E'� E(.fE(��E)6fE)��E*ɚE+I�E+�3E,L�E,� E-�fE.VfE.�fE/X E0^fE0��E1Y�E1�3E2�fE3^fE3�fE4^fE5^fE5� E6T�E6њE7�fE8NfE8ɚE9D�E:L�E:� E;I�E;��E<� E=D�E=��E>C3E?A�E?��E@;3EA<�EA�3EB1�EB� EC��ED$�ED�fEE$�EF#3EF��EGfEH!�EH�3EI�EI�3EJ��EK EK�fELfEM�EM��EN�EN��EO�fEP EP� EQ	�ER3ER��ES  ES�fET{3ET� EUs3EVs3EV�fEWl�EW� EX�3EYX EY�3EZ�fE[;3E[�3E\��E]fE]��E^y�E^��E_\�E`L�E`��Ea��Eb�Eb�3Ecd�Ec�fEd��Ee3Ee��Efh EgFfEg� Eh�Eh��Eia�EjA�Ej��Ek�3Ek�fEl� EmC3En#3En�fEn�3Eo��EpI�Eq0 Eq� Erx Er�3Es��Et,�Et�fEu��Eu� Ev�3Ew;3Ex!�Ex�3Eyk3Ey�fEzA�E{#3E{�3E|ffE|�fE}� E~3E~�3EH E�fE�A�E��fE��E�K3E�|�E���E�JfE�| E��fE��E�|�E��fE��E�y�E���E��E�=�E��3E��E�33E��3E��fE�$�E���E�� E� E�� E��3E�fE�l�E�� E��3E�X�E��3E��3E�I�E���E�� E�;3E�� E��3E�0 E�_3E��fE�$ E�Q�E���E�fE�h E��fE���E�C3E��3E��E� E�t�E��3E�$ E�zfE���E�%�E�M�E���E��3E�ZfE���E�� E�<�E���E��E�H E�rfE�� E�"fE�w3E��3E� �E�JfE��fE��3E�I�E��3E�� E�9�E�� E�՚E�!�E�jfE�� E�%�E�p�E��3E� E�M�E��fE��E�H�E���E�ٚE�L E��fE��fE�@ E���E�� E�!�E�t E���E��G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�?fffG�O�?�  G�O�?���?���?�ff?�33?�  ?ٙ�?�ff?�33@ff@33@&ff@,��@9��@L��@Y��@l��@�  @���@�  @���@�ff@�33@���@�ff@�33@�  @���@�ffA33A  A  A��A   A(  A.ffA8  A>ffAFffAL��AT��A\��Ad��Ak33As33A{33A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144441414111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ %@ �@ *@ �@ ""@ )�@ 0x@ 7L@ ?}@ G�@ R�@ _�@ l�@ {�@ ��@ ��@ ��@ �~@ �w@ �|@ ��@ ��@ �q@j@@g@,`@9X@G�@T�@b�@r@�@��@��@�A@�9@@��@�/@��@��@�@{@""@1'@>�@K�@X@ff@t�@�@�@��@�Y@��@Ĝ@��@��@�@@��@�@B@&�@3�@A�@N�@]�@k.@x&@��@�@��@��@�k@�c@�
@�@�Y@  @�@�@(�@7L@E�@S�@^�@m:@|?@��@�0@��@��@�&@�|@�@�m@��@@�@g@-�@<@G�@UU@b�@p�@~K@��@�H@��@��@�>@��@��@�@�~@�@�@"�@/�@=q@K@Yn@g@t�@�@��@�U@��@�R@ƨ@�O@��@�@��@
�@�@&;@4�@A�@P�@[z@i!@x�@�|@�u@�m@�f@�@��@�
@�@�@  @@�@(G@7�@DD@Q�@`�@m�@z�@��@��@�5@��@�&@�@��@��@� @	�@	b@	[@	-@	:�@	F�@	T�@	a�@	p�@	~K@	��@	��@	�M@	�F@	�>@	�7@	܀@	�4@	�,@
%@
*@
""@
.l@
>@
K�@
X�@
g�@
v@
�@
�\@
�a@
��@
��@
��@
�O@
�T@
�@
�9@
=@�@%�@33@@�@N�@\�@k.@y�@�+@��@�(@�@��@�c@�@�@�@��@J@�@(�@7L@E�@S�@a�@n�@y�@��@�0@��@��@�2@��@�@�m@�q@�@@g@,`@:@I@V@bN@p�@�@��@�<@��@�F@�J@ψ@��@��@��@�@�@g@/@=q@M$@\)@e�@uk@�@�\@�a@�f@��@ƨ@խ@�;@��@��@1@6@&;@1'@@�@O�@Z@j@x�@�@��@��@�@�@�@��@�@�Y@^@@O@+@:@C�@SI@bN@l�@{�@��@��@�(@��@�2@�7@�t@�(@�~@@o@ �@*S@9X@I@SI@bN@qS@z�@��@��@��@��@�2@�7@ލ@��@��@%@*@$.@-�@=q@Lu@V@ff@uk@�p@�u@�@��@��@�J@��@�T@�@@�E@J@6@&;@5?@?}@N�@]�@g�@ww@��@�@��@�@��@��@�
@�@�e@��@�@�@&�@5�@D�@O�@^5@m�@ww@�+@�0@�5@��@�@�@�t@�(@�,@@�@ @+@:@I@R�@bN@qS@�W@�\@��@�A@�F@Ĝ@�O@�/@�@��@�@�@""@1'@?}@I@Wb@e	@t�@�d@�h@��@�M@��@ƨ@�O@��@��@��@�@6@$.@33@A�@O�@^5@l�@z3@��@��@�m@�!@��@�@�
@�`@�@^@�@�@'�@5�@DD@SI@`�@k�@y�@��@��@��@��@��@�@��@�@�q@�@@!s@+@9X@G�@V@c�@r@�W@��@��@�A@�F@�J@��@�/@��@�,@�@*@#�@1'@@,@I@Wb@e	@s_@�@��@�U@��@��@��@Ӡ@��@�@�E@�@B@&;@4�@A�@O0@\�@k.@x�@�|@�u@�@��@�j@�@�
@�`@�@ �@V@�@)�@7L@D�@Q�@_�@l�@z�@�7@��@��@��@��@��@܀@�y@� @v@o@ @-�@<@I�@Wb@e	@r�@�W@��@�<@�M@��@��@��@܀@�(@��@v@@!s@/@<�@K@X�@ff@t�@�d@�@�a@��@�^@��@�C@��@��@��@
�@B@'�@4�@C�@Q�@[z@i!@x&@��@�#@�z@�!@�@�W@�@�@�@ @ b@ 
@ '�@ 5�@ C�@ Q�@ _�@ m�@ |?@ ��@ �<@ �@ �r@ �@ �@ ��@ ��@ �q@!@!o@! �@!.l@!7�@!F�@!S�@!bN@!o�@!~K@!��@!��@!�M@!��@!�2@!ψ@!��@!�@!��@"�@"�@"$�@".l@"<@"Ji@"X@"ff@"t�@"�d@"�i@"�@"��@"�@"ȴ@"�\@"�@"��@"��@#1@#�@#$/@#1�@#@,@#M$@#[z@#i!@#v�@#�p@#�@#�@#�f@#�@#ȴ@#խ@#�@#�@$@$@$�@$*S@$7�@$E�@$SI@$`�@$n�@$|?@$��@$��@$�4@$��@$��@$�|@$�t@$��@$��@%j@%@%�@%+�@%8�@%K@%X@%e�@%r�@%�W@%��@%��@%�M@%��@%��@%є@%��@%�4@%�,@&�@&@&!s@&.l@&<@&I@&Wb@&dZ@&r@&��@&��@&�@&��@&��@&�W@&��@&��@&��@&��@'
=@'6@'$�@'5�@'C�@'Q=@'^5@'k�@'y�@'�|@'�u@'�@'�@'�j@'ȴ@'�\@'�@'�Y@(  @(�@(O@((�@(6�@(DD@(Q�@(_�@(m�@(|?@(�7@(��@(�4@(��@(��@(�*@(܀@(�(@(��@)�@)o@) @)-�@)<@)I�@)V�@)dZ@)r�@)�W@)��@)��@)��@)��@)�>@)��@)��@)�@)�,@*�@*{@*!s@*/@*<@*I�@*V�@*e	@*r@*�@*��@*��@*�M@*��@*��@*�C@*��@*�@*�9@+�@+�@+$/@+5�@+DD@+Lu@+Z�@+hs@+v@+��@+��@+�(@+�!@+�@+��@+�@+�@+�@, �@,�@,O@,(G@,4�@,B�@,SI@,`�@,m�@,z�@,��@,��@,�z@,�9@,�2@,�|@,�t@,�@,��@-�@-�@- @--�@-:�@-G�@-UU@-bN@-oF@-|�@-��@-��@-��@-��@-�2@-��@-�;@-�4@-��@.v@.�@.#�@.1'@.=q@.K@.X�@.e	@.v�@.��@.��@.�a@.�Y@.�R@.�J@.�C@.�;@.�L@.��@/
�@/6@/$�@/1�@/>�@/O�@/]�@/j@/v�@/��@/��@/�(@/�!@/�@/��@/׹@/�`@/�Y@0 �@0�@0O@0(G@05@@0B�@0P�@0bN@0oF@0}�@0��@0�<@0��@0�-@0�w@0�@0�@0�@0��@1�@1o@1g@1-@19X@1F�@1X@1e	@1qS@1~K@1��@1��@1��@1��@1��@1��@1��@1�4@1�,@2%@2o@2#�@2/�@2<�@2M�@2Z@2g�@2t�@2�@2��@2��@2�@2�R@2��@2�C@2��@2��@2�E@3
=@3�@3#�@35@@3A�@3N�@3[z@3hs@3x�@3��@3�@3�(@3�!@3�j@3ȴ@3�\@3�@3�@4 �@4�@4�@4*S@46�@4DD@4T�@4a�@4n�@4{�@4��@4��@4��@4��@4�&@4�@4�/@4�(@4� @5�@5b@5!s@5.l@5;d@5G�@5S�@5Ĝ@6�@6B8@6�J@7�@7DD@7�@7��@8�@8D�@8��@8ƨ@91@9I@9�7@9�@:�@:I@:��@:�@;E�@;�d@;�w@;��@<<@<x�@<��@<�@=i!@=��@=܀@>*@>N�@>�+@>�9@?1'@?hs@?�@?�@@Ji@@�@@�R@@�@A^�@A��@A�o@Bv@B<�@B�@B�T@C�@CSI@C��@C�,@D2�@Dj@D�z@E�@ELu@E�|@E�&@F1�@Fhs@F�@F��@G{@G��@G�w@G�q@H/@Hg@H�#@I�@II@I�W@I�R@J(G@J^�@J��@J��@K<�@Kr�@K��@K�;@LM�@L�p@L�@L�@M^�@M��@M�@M�E@Nl�@N��@N�O@O�@Oy�@O�@O�`@P�@P��@P��@P�@Q*S@Q��@Q�o@R]@RoF@R��@R׹@S�@Sy�@S��@S�@T�@T�|@T�^@T�@U`B@U�#@U�o@V]@VoF@V��@V��@Wb@W~�@W��@W�(@X �@X��@X@X�,@Y-@Y��@Y��@Zj@Zo�@Z�4@Z�t@[@[|?@[��@[�@\O@\�|@\�R@\��@]T�@]�|@]��@^ �@^Q=@^��@^��@_�@_I�@_�!@_��@`B�@`r�@`��@a@a/@a�@a�@b[@bK@b��@b�
@cv@cc�@c�@c�@d[@d|?@d��@e
=@e7�@e��@e�J@e�@fS�@f�d@f�@g@gp�@g�a@g�E@h+@h[z@h�k@h�@iLu@ix�@i�#@j1@jg�@j��@j�>@k#�@kO�@k�f@k��@l6�@l`�@l�@l�@mB�@mn�@m�o@m� @nQ=@n{�@n�O@o+@oUU@o��@o׹@p0x@pZ�@p��@q1@q1'@q�}@q�r@r%@rYn@r�@r�[@s'�@sO1@s�5@s�,@t""@tww@t��@t��@uA�@u�\@u��@v
�@v^�@v�p@v�h@w-@wUU@w��@w�,@x!s@xww@x��@x�e@yG�@yn�@y��@z�@z\)@z�W@z�|@{6@{bN@{�	@{�7@|�@|i!@|�~@|�8@}D�@}�P@}�r@}��@~F�@~��@~��@�@V@��@��@�@�/@�UU@�z2@��a@��8@��@��}@�	@�B8@�e`@��1@���@���@��:@�V@�.�@�M�@�~K@���@���@�݆@���@��@�Ji@�h�@��+@���@���@���@�6@�9X@�]�@���@��0@�� @��H@� W@�G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�@ @ �@ v@ %@ �@ 1@ �@ 	�@ 
�@ J@ V@ @ b@ o@ �@ �@ �@ �@ O@ 
@  @ "�@ $.@ &�@ )�@ ,`@ /@ 1'@ 4�@ 6�@ :@ >@ @�@ DD@ F�@ K@ M�@ Q=@ S�@ Wb@ Z�@ ^5@ `�@ dZ@ g�@ j@ m�@ qSG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�AݓuAܩ�A�5?A���A��
A۾wAۮA۝�Aۉ7AۃA�^5A�33A��AھwAڸRAڬAڗ�Aڙ�AڍPAځA�t�A�l�A�jA�ffA�bNA�\)A�`BA�bNA�`BA�bNA�bNA�bNA�^5A�VA�S�A�M�A�E�A�A�/A׸RA֕�A��A� �A���A͉7A���A�z�Aʣ�A�A�Aȩ�A�ĜA�`BA�ZA�&�A��A��+A���A��A���A���A�hsA��A�ĜA��TA��mA�A�A�A��A�G�A�^5A�v�A��#A�I�A�|�A�K�A���A�E�A�ZA���A���A�\)A�5?A��DA�+A�A�A�A��`A�?}A��A��HA�z�A�oA��+A�v�A�K�A��mA��A�M�A�"�A���A�9XA�l�A�7LA�(�A���A�1A�
=A��uA�{A�`BA��/A��;A�^5A��+A��A�ĜA��uA~�A|�RAwC�ArVAoK�Am�-AlM�Ag��AehsAb��A_��A]�
A\M�AZ �AV�jAT��AR�!AQVAO7LAM��AKt�AHbNAGt�AF�jAEO�ADQ�AB�AA%A@E�A>�9A:�A8�+A7XA6��A6-A4��A3S�A0�HA/�TA.�/A.$�A)�hA%"�A#�;A"�A �DA��A�A�hA��A�\A��A�A5?A�yAZA(�A�#A;dAA�RA�RA�DA�DA��A��AbNA�A�FAx�A�A{A�
A�A�AXA�+A�hA`BAC�A�jA(�A�AVA��A
�/A
z�A
I�A
-A	��A	hsA�Ar�A�/AQ�AA�A(�A�yA|�A�HAA�A��A"�A �9A �@�\)@�S�@�33@��@��@�E�@��^@�hs@��@���@��@�j@���@��R@���@��9@���@�+@��@���@���@�1@��@�t�@�+@�=q@�V@�1'@@�"�@@��^@� �@�  @�@��@��@���@�!@��m@�|�@���@ް!@ݙ�@�?}@�Q�@۾w@ۍP@�dZ@�+@�J@�`B@���@��@�1'@�  @�^5@��@���@�{@�$�@�@�hs@Ԭ@җ�@��@�G�@д9@��H@�{@��@͙�@��@�S�@ʏ\@�^5@�x�@�&�@ȣ�@�  @��@Ɨ�@���@�7L@ģ�@Ĵ9@Ĵ9@���@�%@���@�Ĝ@�A�@��@Å@�C�@�+@���@�V@�=q@�=q@�E�@�J@��7@�?}@��@��@��@���@�I�@�  @��;@�o@�5?@���@��#@���@��#@���@���@�7L@��@���@�I�@�1'@�b@��m@���@��@�|�@�dZ@�+@�@��H@��R@��+@�ff@�M�@�5?@�=q@�$�@�J@���@��@��@�hs@�%@���@��@��;@��w@�"�@��\@�ff@��h@�p�@�hs@�7L@��/@�9X@�l�@��@���@���@���@��!@���@�{@��7@�O�@���@��u@��@�r�@� �@��m@���@�l�@���@�$�@�@���@��@��T@��T@��T@��T@��T@��#@���@��^@�%@�(�@��;@��@��@���@��P@�S�@���@�ff@�=q@�$�@�{@��@�p�@�Ĝ@�j@�A�@���@�ȴ@��+@��@��@��-@���@�X@���@��@��/@�Z@���@���@�l�@�C�@�o@���@���@�M�@��@��7@��9@��@� �@��@�S�@��H@�ȴ@���@�V@�$�@�@�&�@��/@�Z@� �@��m@�C�@��@��!@�o@��H@��T@�X@���@���@�z�@�I�@��w@�S�@��@���@���@�~�@�^5@�=q@�J@���@��T@�7L@�Ĝ@�Q�@� �@���@��;@�|�@�t�@�ȴ@��+@��#@�O�@��@�A�@���@��F@�t�@�+@��y@�v�@�5?@���@��#@��T@���@�@��^@��^@���@�X@���@��`@��D@�1@��m@�ƨ@��@�K�@�o@��+@�M�@���@�p�@��@�Ĝ@��@��@���@���@�t�@�K�@�@��R@�$�@���@��@��@���@�?}@�&�@��@���@��j@�I�@�b@��@��P@�t�@�l�@�S�@�"�@���@�V@�-@���@��@��-@���@��7@�x�@�G�@�/@��@���@��D@�Q�@�9X@�(�@;d@~��@~ff@~$�@~@}�@}�T@}@}`B@|�D@|1@{��@{S�@{o@z��@zJ@yX@x�9@x�@xbN@w�w@w|�@w\)@w
=@v��@u@t�@t��@t��@t��@t1@s��@s33@r��@r��@rn�@rM�@rM�@r=q@q��@p��@p�`@pĜ@p�u@p1'@o��@o;d@n�@n�R@n�R@n��@n��@nv�@nE�@n$�@m@m`B@l��@l�j@l�@lz�@lZ@l9X@l9X@l9X@k��@k�@k@j~�@j^5@i�#@ix�@ihs@i�@h�`@hbN@g�@g�P@g\)@f��@fȴ@f�+@f5?@f{@e�T@e��@eO�@e?}@e`B@dI�@c��@c�@cS�@c33@b~�@b=q@b-@a�@ahs@aG�@`�u@`b@_��@_�@_�P@_l�@_K�@^�y@^�+@^5?@^@]��@]�h@]�@]O�@]/@]�@\�j@\��@\z�@\I�@\I�@\(�@[�
@[dZ@Z��@Z=q@Y�^@W�@WK�@WK�@W;d@W+@V�R@VV@VE�@V@U`B@T�/@TI�@S�
@S��@SC�@So@R��@R��@R�!@R�\@R=q@RJ@Q�@Q��@Q%@P�`@PA�@P  @O�w@O\)@N��@N{@M�T@M�-@M/@L�D@LI�@L9X@KdZ@K@J�@J�!@I��@H�`@HĜ@H��@HbN@F�@FE�@E�T@E@E�-@E?}@D�@D�D@Dz�@Dj@D9X@CC�@C@B��@B�@A��@Ax�@A&�@@�`@@r�@?�@?�@?�@?��@?�@>ȴ@>�R@>��@>v�@>E�@=�@=�T@=@=�h@=?}@<�@<Z@<1@;�m@;�
@;�
@;ƨ@;S�@:�H@:��@:��@:�\@:=q@:=q@:J@9�7@9�@8 �@7�w@7l�@7+@6ȴ@6v�@65?@6{@5�T@5�T@5�T@5��@5@5�h@4��@49X@3��@3��@3S�@333@3"�@2�H@2~�@2�@1��@1�7@1hs@17L@0bN@/�w@/\)@.��@.��@.�+@.5?@-�T@-�-@-��@-p�@-O�@-/@-?}@-/@-?}@-V@,�@,�/@,�j@,�@,��@,I�@,�@,1@+��@+dZ@+33@+@+@+@*��@*n�@*=q@)��@)�^@)x�@)hs@(��@(Ĝ@(Ĝ@(�@'�;@'\)@'K�@&��@&ȴ@&v�@&V@&V@&{@%��@%�@%O�@%�@%V@$��@$�j@$z�@#�m@#"�@"�@"��@"��@"�@!�#@!��@!hs@!�@ ��@ 1'@�@l�@+@��@ȴ@��@E�@{@�-@�@?}@��@�j@�@��@z�@Z@9X@�
@�F@�@S�@C�@o@@�H@��@��@~�@^5@=q@��@�7@X@G�@&�@%@��@�9@��@Q�@b@�@�w@��@K�@+@+@+@+@��@�@ȴ@ȴ@��@ff@$�@{@�@��@�-@p�@`B@O�@?}@�@�@��@z�@I�@9X@(�@�@��@ƨ@�F@�@S�@"�@�@�!@�\@~�@^5@^5@~�@�\@~�@�@x�@7L@�`@�`@Ĝ@�@Q�@1'@ �@  @��@�P@�@?}@�@Z@9X@��@
�H@
n�@	�#@�`@A�@��@+@5?@{@��@�@��@Z@9X@��@~�@J@G�@%@ A�?�|�?��R?��?��?��m?�C�?�~�?���?�E�?���?��
?���?��?��`?�A�?�\)??��-?��-?�/?��m?��?��?��?�1'?�K�?�?�?�Z?�F?�!?�%?�bN?ߝ�?߾w?�;d?ޗ�?��?܋D?�dZ?���?��?ٺ^?���?ش9?�1'?���?׍P?և+?�?Լj?��
?��
?�S�?��?�J?щ7?�&�?� �?Ͼw?�|�?�;d?�v�?�5??Ͳ-?͑h?̬?�1?�1?��m?˥�?�C�?�^5?��?ɺ^?ə�?�x�?�7L?��?�x�?�X?���?ȓu?�r�?Ǯ?��?�+?�ȴ?Ƨ�?�?�?�?�?��T?��T?Ł?�?}?��?���?�?}?ļj?�z�?�9X?öF?Õ�?Õ�?�S�?�33?�33?�o?°!?�J?�J?��?���?�hs?�%?��`?�%?��`?���?���?�A�?�A�?�bN?�A�?�  ?��;?�|�?�\)?�;d?���?��?��R?��R?��?��R?�v�?���?���?�V?�5??�V?�5??�V?�5??�V?�V?�v�?���?�V?�v�?�v�?�{?��?�5??��?��-?��-?���?��h?�p�?�p�?�/?�V?��?��?��?��?�j?��D?��m?�1?��m?��m?�ƨ?�ƨ?�ƨ?�ƨ?���?���?�ƨ?�ƨ?�ƨ?��m?�1?�I�?�I�?�I�?�I�?�I�?�I�?��D?�I�?�(�?�I�?�j?�I�?�j?�I�?��D?��D?�j?��D?��?��?��?�j?�j?�j?�j?��D?�j?��D?��?��?���?���?��?�/?�V?�O�?�O�?�p�?��h?��h?��h?��-?��-?��h?���?���?���?�{?��?�{?�5??�V?�v�?�V?�V?�5??�V?�{?�5??�v�?�{?�{?�V?�{?�5??�V?�V?���?�v�?�v�?�v�?���?��R?��?��?��?���?���?��?��?��?�;d?�;d?�;d?�\)?�\)?�|�?���?���?���?��w?��;?��w?��w?�|�?��w?�  ?�  ?�  ?��w?��;?��;?� �?�bN?�bN?��;AށAޓuAޅA�z�A�n�A�z�A�ZAޅA�bNA�n�A�VA�E�A�33A��Aݰ!Aݗ�A�^5A�-A�{A���A��/A���Aܰ!A܉7A�r�A�S�A�=qA�+A��A�
=A���A��yA��;A��
A���A�ƨA۾wA۶FA۰!A۩�Aۥ�A۟�AۓuAۋDAۉ7Aۉ7AۃA�z�A�t�A�dZA�VA�E�A�=qA�33A�1'A�(�A��A�A��;A�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A�t�AݓuAܩ�A�5?A���A��
A۾wAۮA۝�Aۉ7AۃA�^5A�33A��AھwAڸRAڬAڗ�Aڙ�AڍPAځA�t�A�l�A�jA�ffA�bNA�\)A�`BA�bNA�`BA�bNA�bNA�bNA�^5A�VA�S�A�M�A�E�A�A�/A׸RA֕�A��A� �A���A͉7A���A�z�Aʣ�A�A�Aȩ�A�ĜA�`BA�ZA�&�A��A��+A���A��A���A���A�hsA��A�ĜA��TA��mA�A�A�A��A�G�A�^5A�v�A��#A�I�A�|�A�K�A���A�E�A�ZA���A���A�\)A�5?A��DA�+A�A�A�A��`A�?}A��A��HA�z�A�oA��+A�v�A�K�A��mA��A�M�A�"�A���A�9XA�l�A�7LA�(�A���A�1A�
=A��uA�{A�`BA��/A��;A�^5A��+A��A�ĜA��uA~�A|�RAwC�ArVAoK�Am�-AlM�Ag��AehsAb��A_��A]�
A\M�AZ �AV�jAT��AR�!AQVAO7LAM��AKt�AHbNAGt�AF�jAEO�ADQ�AB�AA%A@E�A>�9A:�A8�+A7XA6��A6-A4��A3S�A0�HA/�TA.�/A.$�A)�hA%"�A#�;A"�A �DA��A�A�hA��A�\A��A�A5?A�yAZA(�A�#A;dAA�RA�RA�DA�DA��A��AbNA�A�FAx�A�A{A�
A�A�AXA�+A�hA`BAC�A�jA(�A�AVA��A
�/A
z�A
I�A
-A	��A	hsA�Ar�A�/AQ�AA�A(�A�yA|�A�HAA�A��A"�A �9A �@�\)@�S�@�33@��@��@�E�@��^@�hs@��@���@��@�j@���@��R@���@��9@���@�+@��@���@���@�1@��@�t�@�+@�=q@�V@�1'@@�"�@@��^@� �@�  @�@��@��@���@�!@��m@�|�@���@ް!@ݙ�@�?}@�Q�@۾w@ۍP@�dZ@�+@�J@�`B@���@��@�1'@�  @�^5@��@���@�{@�$�@�@�hs@Ԭ@җ�@��@�G�@д9@��H@�{@��@͙�@��@�S�@ʏ\@�^5@�x�@�&�@ȣ�@�  @��@Ɨ�@���@�7L@ģ�@Ĵ9@Ĵ9@���@�%@���@�Ĝ@�A�@��@Å@�C�@�+@���@�V@�=q@�=q@�E�@�J@��7@�?}@��@��@��@���@�I�@�  @��;@�o@�5?@���@��#@���@��#@���@���@�7L@��@���@�I�@�1'@�b@��m@���@��@�|�@�dZ@�+@�@��H@��R@��+@�ff@�M�@�5?@�=q@�$�@�J@���@��@��@�hs@�%@���@��@��;@��w@�"�@��\@�ff@��h@�p�@�hs@�7L@��/@�9X@�l�@��@���@���@���@��!@���@�{@��7@�O�@���@��u@��@�r�@� �@��m@���@�l�@���@�$�@�@���@��@��T@��T@��T@��T@��T@��#@���@��^@�%@�(�@��;@��@��@���@��P@�S�@���@�ff@�=q@�$�@�{@��@�p�@�Ĝ@�j@�A�@���@�ȴ@��+@��@��@��-@���@�X@���@��@��/@�Z@���@���@�l�@�C�@�o@���@���@�M�@��@��7@��9@��@� �@��@�S�@��H@�ȴ@���@�V@�$�@�@�&�@��/@�Z@� �@��m@�C�@��@��!@�o@��H@��T@�X@���@���@�z�@�I�@��w@�S�@��@���@���@�~�@�^5@�=q@�J@���@��T@�7L@�Ĝ@�Q�@� �@���@��;@�|�@�t�@�ȴ@��+@��#@�O�@��@�A�@���@��F@�t�@�+@��y@�v�@�5?@���@��#@��T@���@�@��^@��^@���@�X@���@��`@��D@�1@��m@�ƨ@��@�K�@�o@��+@�M�@���@�p�@��@�Ĝ@��@��@���@���@�t�@�K�@�@��R@�$�@���@��@��@���@�?}@�&�@��@���@��j@�I�@�b@��@��P@�t�@�l�@�S�@�"�@���@�V@�-@���@��@��-@���@��7@�x�@�G�@�/@��@���@��D@�Q�@�9X@�(�@;d@~��@~ff@~$�@~@}�@}�T@}@}`B@|�D@|1@{��@{S�@{o@z��@zJ@yX@x�9@x�@xbN@w�w@w|�@w\)@w
=@v��@u@t�@t��@t��@t��@t1@s��@s33@r��@r��@rn�@rM�@rM�@r=q@q��@p��@p�`@pĜ@p�u@p1'@o��@o;d@n�@n�R@n�R@n��@n��@nv�@nE�@n$�@m@m`B@l��@l�j@l�@lz�@lZ@l9X@l9X@l9X@k��@k�@k@j~�@j^5@i�#@ix�@ihs@i�@h�`@hbN@g�@g�P@g\)@f��@fȴ@f�+@f5?@f{@e�T@e��@eO�@e?}@e`B@dI�@c��@c�@cS�@c33@b~�@b=q@b-@a�@ahs@aG�@`�u@`b@_��@_�@_�P@_l�@_K�@^�y@^�+@^5?@^@]��@]�h@]�@]O�@]/@]�@\�j@\��@\z�@\I�@\I�@\(�@[�
@[dZ@Z��@Z=q@Y�^@W�@WK�@WK�@W;d@W+@V�R@VV@VE�@V@U`B@T�/@TI�@S�
@S��@SC�@So@R��@R��@R�!@R�\@R=q@RJ@Q�@Q��@Q%@P�`@PA�@P  @O�w@O\)@N��@N{@M�T@M�-@M/@L�D@LI�@L9X@KdZ@K@J�@J�!@I��@H�`@HĜ@H��@HbN@F�@FE�@E�T@E@E�-@E?}@D�@D�D@Dz�@Dj@D9X@CC�@C@B��@B�@A��@Ax�@A&�@@�`@@r�@?�@?�@?�@?��@?�@>ȴ@>�R@>��@>v�@>E�@=�@=�T@=@=�h@=?}@<�@<Z@<1@;�m@;�
@;�
@;ƨ@;S�@:�H@:��@:��@:�\@:=q@:=q@:J@9�7@9�@8 �@7�w@7l�@7+@6ȴ@6v�@65?@6{@5�T@5�T@5�T@5��@5@5�h@4��@49X@3��@3��@3S�@333@3"�@2�H@2~�@2�@1��@1�7@1hs@17L@0bN@/�w@/\)@.��@.��@.�+@.5?@-�T@-�-@-��@-p�@-O�@-/@-?}@-/@-?}@-V@,�@,�/@,�j@,�@,��@,I�@,�@,1@+��@+dZ@+33@+@+@+@*��@*n�@*=q@)��@)�^@)x�@)hs@(��@(Ĝ@(Ĝ@(�@'�;@'\)@'K�@&��@&ȴ@&v�@&V@&V@&{@%��@%�@%O�@%�@%V@$��@$�j@$z�@#�m@#"�@"�@"��@"��@"�@!�#@!��@!hs@!�@ ��@ 1'@�@l�@+@��@ȴ@��@E�@{@�-@�@?}@��@�j@�@��@z�@Z@9X@�
@�F@�@S�@C�@o@@�H@��@��@~�@^5@=q@��@�7@X@G�@&�@%@��@�9@��@Q�@b@�@�w@��@K�@+@+@+@+@��@�@ȴ@ȴ@��@ff@$�@{@�@��@�-@p�@`B@O�@?}@�@�@��@z�@I�@9X@(�@�@��@ƨ@�F@�@S�@"�@�@�!@�\@~�@^5@^5@~�@�\@~�@�@x�@7L@�`@�`@Ĝ@�@Q�@1'@ �@  @��@�P@�@?}@�@Z@9X@��@
�H@
n�@	�#@�`@A�@��@+@5?@{@��@�@��@Z@9X@��@~�@J@G�@%@ A�?�|�?��R?��?��?��m?�C�?�~�?���?�E�?���?��
?���?��?��`?�A�?�\)??��-?��-?�/?��m?��?��?��?�1'?�K�?�?�?�Z?�F?�!?�%?�bN?ߝ�?߾w?�;d?ޗ�?��?܋D?�dZ?���?��?ٺ^?���?ش9?�1'?���?׍P?և+?�?Լj?��
?��
?�S�?��?�J?щ7?�&�?� �?Ͼw?�|�?�;d?�v�?�5??Ͳ-?͑h?̬?�1?�1?��m?˥�?�C�?�^5?��?ɺ^?ə�?�x�?�7L?��?�x�?�X?���?ȓu?�r�?Ǯ?��?�+?�ȴ?Ƨ�?�?�?�?�?��T?��T?Ł?�?}?��?���?�?}?ļj?�z�?�9X?öF?Õ�?Õ�?�S�?�33?�33?�o?°!?�J?�J?��?���?�hs?�%?��`?�%?��`?���?���?�A�?�A�?�bN?�A�?�  ?��;?�|�?�\)?�;d?���?��?��R?��R?��?��R?�v�?���?���?�V?�5??�V?�5??�V?�5??�V?�V?�v�?���?�V?�v�?�v�?�{?��?�5??��?��-?��-?���?��h?�p�?�p�?�/?�V?��?��?��?��?�j?��D?��m?�1?��m?��m?�ƨ?�ƨ?�ƨ?�ƨ?���?���?�ƨ?�ƨ?�ƨ?��m?�1?�I�?�I�?�I�?�I�?�I�?�I�?��D?�I�?�(�?�I�?�j?�I�?�j?�I�?��D?��D?�j?��D?��?��?��?�j?�j?�j?�j?��D?�j?��D?��?��?���?���?��?�/?�V?�O�?�O�?�p�?��h?��h?��h?��-?��-?��h?���?���?���?�{?��?�{?�5??�V?�v�?�V?�V?�5??�V?�{?�5??�v�?�{?�{?�V?�{?�5??�V?�V?���?�v�?�v�?�v�?���?��R?��?��?��?���?���?��?��?��?�;d?�;d?�;d?�\)?�\)?�|�?���?���?���?��w?��;?��w?��w?�|�?��w?�  ?�  ?�  ?��w?��;?��;?� �?�bN?�bN?��;AށAޓuAޅA�z�A�n�A�z�A�ZAޅA�bNA�n�A�VA�E�A�33A��Aݰ!Aݗ�A�^5A�-A�{A���A��/A���Aܰ!A܉7A�r�A�S�A�=qA�+A��A�
=A���A��yA��;A��
A���A�ƨA۾wA۶FA۰!A۩�Aۥ�A۟�AۓuAۋDAۉ7Aۉ7AۃA�z�A�t�A�dZA�VA�E�A�=qA�33A�1'A�(�A��A�A��;A�ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
bB
DB

=B
PB
PB
PB
PB
PB
JB
JB
JB
+B
+B
B
B
%B
+B
1B

=B
DB
DB
JB
PB
PB
VB
VB
VB
bB
hB
oB
{B
{B
�B
�B
{B
{B
�B
{B
oB
�B
8RB
L�B
Q�B
[#B
t�B
��B
��B
��B
�B
�B
ÖB
�TB"�B:^BXBhsBz�B�BƨB�TB�ZB�TB�TB�HB�/B�/B�fB�B  BBbB�B!�B!�B.B+B'�B"�B�B�BuB	7B�#B��BÖB�RB�qB�XB��B�\B�1B� Bw�Bo�BdZBR�BM�BG�BE�BA�B>wB+B�BJB
=BB
�B
�TB
��B
ȴB
��B
��B
�bB
hsB
-B
�B
\B	�mB	�wB	�-B	�B	iyB	VB	Q�B	A�B	,B	#�B	PB	PB	B��B��B�mB�fB�/B�
B��BȴBȴBǮBɺBĜBÖBɺBǮB��B��BɺB�wB��BȴB��B��B�NB�B��B	B	oB	�B�sB��B��BB�LB�LB�FB�?B�?B�?B�FB�qB�qBÖBƨBǮBǮB��B��B�B�NB�B�B�B��B��B	B		7B	\B	uB	�B	�B	�B	�B	�B	�B	+B	0!B	1'B	2-B	1'B	6FB	8RB	9XB	>wB	?}B	?}B	>wB	=qB	<jB	:^B	8RB	:^B	;dB	;dB	;dB	9XB	:^B	;dB	<jB	>wB	@�B	D�B	G�B	J�B	J�B	J�B	J�B	K�B	M�B	O�B	Q�B	R�B	S�B	S�B	T�B	W
B	ZB	]/B	aHB	bNB	bNB	cTB	gmB	l�B	o�B	q�B	q�B	r�B	s�B	w�B	x�B	z�B	z�B	{�B	|�B	~�B	�B	�B	~�B	� B	� B	|�B	z�B	y�B	y�B	x�B	v�B	v�B	t�B	u�B	t�B	t�B	s�B	r�B	r�B	r�B	w�B	z�B	|�B	�B	�B	�%B	�=B	�=B	�DB	�DB	�7B	�DB	�=B	�7B	�B	�+B	�1B	�+B	�+B	�B	�7B	�7B	�=B	�=B	�JB	�DB	�PB	�hB	�uB	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�-B	�3B	�3B	�3B	�3B	�-B	�3B	�-B	�3B	�9B	�9B	�?B	�FB	�LB	�LB	�LB	�^B	�^B	�^B	�^B	�jB	�wB	�wB	�wB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�5B	�;B	�;B	�BB	�BB	�HB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�`B	�fB	�fB	�`B	�`B	�fB	�fB	�fB	�sB	�mB	�sB	�sB	�mB	�sB	�sB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
  B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
%B
B
B
B
%B
+B
1B
	7B
DB
DB
JB
JB
PB
JB
PB
PB
VB
\B
VB
PB
VB
PB
VB
VB
VB
\B
\B
\B
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
uB
uB
uB
�B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
#�B
#�B
#�B
"�B
#�B
#�B
#�B
#�B
%�B
&�B
&�B
%�B
%�B
&�B
&�B
&�B
(�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
0!B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
@�B
B�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
D�B
E�B
F�B
G�B
G�B
G�B
F�B
H�B
H�B
H�B
G�B
J�B
I�B
K�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
O�B
P�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
T�B
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
XB
YB
YB
YB
YB
YB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
[#B
ZB
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
\)B
]/B
]/B
^5B
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
_;B
_;B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
aHB
aHB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
gmB
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
iyB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
k�B
jB
k�B
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
s�B
s�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
w�B
x�B
w�B
w�B
x�B
w�B
x�B
y�B
x�B
z�B
y�B
z�B
{�B
|�B
}�B
~�B
}�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�%B
�+B
�1B
�+B
�1B
�1B
�=B
�=B
�=B
�=B
�=B
�DB
�JB
�PB
�VB
�VB
�\B
�VB
�\B
�bB
�oB
�oB
�oB
�oB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�!B
�'B
�'B
�3B
�-B
�3B
�3B
�3B
�-B
�9B
�9B
�3B
�9B
�9B
�9B
�9B
�?B
�9B
�?B
�9B
�9B
�?B
�?B
�?B
�9B
�?B
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�FB
�LB
�LB
�LB
�LB
�RB
�RB
�LB
�LB
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�LB
�RB
�RB
�LB
�RB
�XB
�RB
�LB
�RB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�^B
�dB
�^B
�dB
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
�jB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�dB
�jB
�jB
�jB
�qB
�jB
�jB
�qB
�qB
�qB
�qB
�jB
�jB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�wB
�wB
�}B
�wB
�}B
�wB
�wB
�wB
�wB
�qB
�qB
�wB
�qB
�wB
�qB
�wB
�}B
�wB
�}B
�}B
�}B
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
��B
��B
��B
��B
�}B
��B
��B
��B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
B
��B
B
B
��B
��B
B
B
�B
DB
{B
hB
�B
oB
bB
VB
bB
uB
VB
JB
JB
PB
+B
+B

=B
VB
PB

=B

=B
PB
1B

=B
PB
DB
PB
VB
PB
VB
PB
PB
PB
JB
PB
PB
PB
PB
PB
PB
JB
JB
DB
PB
PB
DB
DB

=B
+B
%B
%B
B
+B
+B
+B
B
B
B
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
DB
%B
B
1B
1B
1B
1B
1B
+B
+B
+B
B
B	��B	��B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B
	7B
DB
JB
PB
\B
\B
bB
bB
\B
\B
bB
\B
PB
�B
33B
G�B
L�B
VB
o�B
��B
��B
��B
��B
��B
�wB
�5B�B5?BR�BcTBu�B��B��B�5B�;B�5B�5B�)B�B�B�HB�B��B��BDB�B�B�B(�B%�B"�B�BuBbBVBB�BƨB�wB�3B�RB�9B��B�=B�Bz�Br�BjB_;BM�BH�BB�B@�B<jB9XB%�BuB+BB
��B
�B
�5B
��B
ÖB
�dB
��B
�DB
cTB
'�B
�B

=B	�NB	�XB	�B	{�B	dZB	P�B	L�B	<jB	&�B	�B	1B	1B��B��B�B�NB�HB�B��B��BÖBÖBBĜB�}B�wBĜBBȴBȴBĜB�XB�jBÖBɺB��B�/B�B��B��B	PB	hB�TB��BɺB�qB�-B�-B�'B�!B�!B�!B�'B�RB�RB�wB��BBBƨBɺB��B�/B�mB�B�B�B��B��B	B	
=B	VB	�B	�B	�B	�B	�B	�B	%�B	+B	,B	-B	,B	1'B	33B	49B	9XB	:^B	:^B	9XB	8RB	7LB	5?B	33B	5?B	6FB	6FB	6FB	49B	5?B	6FB	7LB	9XB	;dB	?}B	B�B	E�B	E�B	E�B	E�B	F�B	H�B	J�B	L�B	M�B	N�B	N�B	O�B	Q�B	T�B	XB	\)B	]/B	^5B	_;B	cTB	gmB	k�B	m�B	m�B	n�B	o�B	s�B	t�B	v�B	v�B	w�B	x�B	z�B	~�B	~�B	z�B	{�B	{�B	x�B	v�B	u�B	u�B	t�B	r�B	r�B	p�B	q�B	p�B	p�B	o�B	n�B	n�B	n�B	s�B	v�B	x�B	}�B	~�B	�B	�%B	�%B	�+B	�+B	�B	�+B	�%B	�B	�B	�B	�B	�B	�B	� B	�B	�B	�%B	�%B	�1B	�+B	�7B	�PB	�\B	�hB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�3B	�3B	�FB	�FB	�FB	�FB	�RB	�^B	�^B	�^B	�^B	�dB	�jB	�jB	�jB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ƨB	ƨB	ǮB	ȴB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�#B	�#B	�)B	�)B	�/B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�;B	�HB	�NB	�NB	�HB	�HB	�NB	�NB	�NB	�ZB	�TB	�ZB	�ZB	�TB	�ZB	�ZB	�fB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�yB	�yB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
	7B
1B
	7B
	7B

=B
DB

=B
	7B

=B
	7B

=B

=B

=B
DB
DB
DB

=B

=B
DB
DB
DB
JB
JB
JB
JB
JB
PB
\B
\B
\B
hB
bB
bB
bB
bB
bB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
!�B
!�B
"�B
"�B
"�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
,B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
/B
.B
.B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
7LB
7LB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
:^B
:^B
;dB
;dB
;dB
:^B
:^B
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
>wB
=qB
=qB
>wB
>wB
=qB
?}B
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
C�B
E�B
E�B
E�B
D�B
G�B
F�B
H�B
H�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
L�B
M�B
N�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
VB
VB
VB
VB
VB
W
B
VB
W
B
W
B
W
B
W
B
XB
W
B
YB
YB
YB
YB
YB
ZB
ZB
ZB
YB
ZB
ZB
[#B
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
`BB
_;B
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
dZB
cTB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
ffB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
hsB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
t�B
u�B
t�B
t�B
u�B
t�B
u�B
v�B
u�B
w�B
v�B
w�B
x�B
y�B
z�B
{�B
z�B
|�B
}�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�+B
�+B
�1B
�7B
�=B
�DB
�JB
�JB
�PB
�JB
�PB
�VB
�bB
�bB
�bB
�bB
�hB
�hB
�oB
�uB
�uB
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�!B
�!B
�-B
�'B
�-B
�-B
�-B
�'B
�3B
�3B
�-B
�3B
�3B
�3B
�3B
�9B
�3B
�9B
�3B
�3B
�9B
�9B
�9B
�3B
�9B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�?B
�FB
�FB
�FB
�FB
�LB
�LB
�FB
�FB
�?B
�FB
�FB
�LB
�LB
�LB
�LB
�LB
�FB
�LB
�LB
�FB
�LB
�RB
�LB
�FB
�LB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
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
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�dB
�dB
�jB
�jB
�jB
�jB
�dB
�jB
�jB
�dB
�jB
�jB
�jB
�qB
�jB
�jB
�qB
�qB
�qB
�qB
�jB
�jB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�wB
�wB
�}B
�wB
�}B
�wB
�wB
�wB
�wB
�qB
�qB
�wB
�qB
�wB
�qB
�wB
�}B
�wB
�}B
�}B
�}B
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
��B
��B
��B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
B
��B
��B
��B
��B
��B
��B
B
B
B
B
��B
��B
B
B
B
B
B
B
B
B
B
B
ÖB
ÖB
B
ÖB
ÖB
B
B
ÖB	��B
oB
%B
\B
JB
bB
PB
DB
	7B
DB
VB
	7B
+B
+B
1B
B
B
B
	7B
1B
B
B
1B
B
B
1B
%B
1B
	7B
1B
	7B
1B
1B
1B
+B
1B
1B
1B
1B
1B
1B
+B
+B
%B
1B
1B
%B
%B
B
B
B
B
  B
B
B
B
  B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201911222300362021061413563720210614135637202106141918362021061419183620210614191836201911222300362021061413563720210614135637202106141918362021061419183620210614191836PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0001), vertically averaged dS = -0.005 (+/-0.004)                                                                                                                                                                                           surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0.0001), vertically averaged dS = -0.005 (+/-0.004)                                                                                                                                                                                           Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019112223003620191122230036  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019112223003620191122230036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019112223003620191122230036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713152620210617131526IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                