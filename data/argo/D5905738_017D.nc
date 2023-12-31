CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:37Z creation      
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
_FillValue                 $  Lp   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  eH   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ą   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   8   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   @   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       $   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � p      � pArgo profile    3.1 1.2 19500101000000  20180724220237  20210722160150  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�ojm	�<@�ojm	�<11  @�ojff{`@�ojff{`@6����D�@6����D��c��l�<�c��l�<11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	AB  AA  AA  ?   ?L��?�33@9��@�  @�33@�33@�33A33A��A#33AA��A`  A~ffA���A���A�  A���A���A�  A���B ffBffB��B  B   B(��B0��B8  B@  BH  BPffBXffB`ffBh��Bo��BxffB�33B�  B�33B�ffB���B�  B�  B�  B���B�33B�ffB�ffB�33B���B�33B�ffB�  B�  B�  B�ffB�33B�ffB�33B�33B�33B�  B���B�ffB�ffB�  B�  B�  C L�C�C�fC33C�C	�fC�C  C��C�C  C�fC33C�C  C33C �C"�C$33C&�C(  C)�fC+��C.  C0  C1�fC433C633C8�C:�C;�fC>�C@L�CB�CD  CF33CH  CI��CL�CNL�CP33CR  CT33CVL�CX�CY��C[�fC^  C`�CbL�Cd�Ce�fCh�Cj33Cl  Cm��Co�fCr�CtL�Cv�Cx  Cz�C|�C~  C��C��C��3C�  C�&fC��C�  C�&fC��C��C��C��3C�&fC��C��C��C��fC��C��C��3C��C�&fC��C�  C��C�  C��fC��3C��C�&fC��C�  C��C�&fC��C��fC�  C��C��C��C��3C�  C��C�  C��fC�  C��C�&fC��C�  C��C�  C��3C��C��C�&fC��C��3C��C�&fC��C��fC�  C��C�  C��fC�  C��C��C��3C��C��C�  C��C�  C��3C��C�&fC��C�  C�&fC�  C��3C�  C��C�&fC�&fC��C�&fC��C��C�  C��3C�&fC��C��3C��C�  C��3C��C�&fC��C�  C��C�&fC��C�  C��C�&fC��C��3C�  C��C��C�  C��fC��3C�  C��C�&fC��C�  C��fC��C�&fC��fC��3C�  D fD ��D�D�fD  DY�D` D	��D� DFfD��D�fD@ D� D� D&fD!��D$S3D&�3D)@ D+�fD.FfD0�3D3s3D5��D8�3D;  D=�3D@L�DB��DEs3DHfDJ�3DMffDPfDR�3DUY�DX  DZ� D]9�D_ٚDbl�De  Dg��Dj3Dl�fDo9�Dq�fDtL�Dv��DyS3D{Y�D}� D�6fD�vfD���D���D�<�D�y�D�� D�	�D�L�D�� D�ٚD�  D�` D���D��fD�33D�y�D��3D� D�VfD�� D�� D�#3D�i�D���D�� D� D�L�D��3D��3D�� D�3D�P D�� D��3D��fD��D�33D�\�D�|�D���D��fD���D�  D�fD�,�D�FfD�VfD�i�D�� D��fD���D���D��3D���D� D�,�D�FfD�` D�y�Dǜ�Dȹ�D�� D���D� D�,�D�FfD�i�DЉ�DѠ DҰ D��3D���D�fD�9�D�c3DنfDڦfD��fD�� D� D�6fD�VfD�|�D� D�ɚD��3D��D�<�D�l�D��D��3D�� D�  D�L�D�y�D�fD��D��D�P D�y�D���D�� D�3D�6fD�L�D�vfD���D��fD�ٚE ~fEfE� E.fE��EQ�E� Eh E�3E{3E3E��E E��E��E
#3E$�E�3E�fE,�E( E�3E�3E.fE1�E�fE��E6fE9�E��E��EɚEX E ` E!� E"�E$p E%nfE&�E'�fE)X E*S3E+�3E-1�E.)�E/�3E0�fE1��E3A�E4��E5��E6� E8H E9�3E:�3E;�fE?( EA��EE4�EHi�EK�3ENx EQ��EU EX<�E[d�E^~fEa� EdvfEg��Ej��Em�3Ep��Et1�Ew�EzQ�E}s3E�=�E��3E�e�E��E�� E�fE�� E�L E��fE�]�E���E�@�E��3E�͚E��E�|�E���E��E�jfE��3E��3E�X E��3E� �E�BfE�� E��E�%�E���?   ?   >���?   ?333?   ?   ?   ?   ?��?��?��?333?333?L��?�  ?���?�ff?�33?ٙ�?�ff@   @��@&ff@,��@@  @S33@fff@�  @���@�ff@�33@���@���@�ff@�ff@�33@���@���AffA��A��A33A#33A)��A333A9��AC33AI��AP  AVffA\��Ac33Ak33Aq��Ax  A~ffA�ffA�ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441144441441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?�  ?�ff@��@Y��@�  @�33@�33@�33A33A��A+33AI��Ah  A�33A���A���A�  Ař�A���A�  A���BffB
ffB��B  B"  B*��B2��B:  BB  BJ  BRffBZffBbffBj��Bq��BzffB�33B�  B�33B�ffB���B�  B�  B�  B���B�33B�ffB�ffB�33B���B�33B�ffB�  B�  B�  B�ffB�33B�ffB�33B�33B�33B�  B���B�ffB�ffB�  B�  B�  C ��C��CffC�3C��C
ffC��C� CL�C��C� CffC�3C��C� C�3C ��C"��C$�3C&��C(� C*ffC,L�C.� C0� C2ffC4�3C6�3C8��C:��C<ffC>��C@��CB��CD� CF�3CH� CJL�CL��CN��CP�3CR� CT�3CV��CX��CZL�C\ffC^� C`��Cb��Cd��CfffCh��Cj�3Cl� CnL�CpffCr��Ct��Cv��Cx� Cz��C|��C~� C�Y�C�L�C�33C�@ C�ffC�L�C�@ C�ffC�Y�C�L�C�L�C�33C�ffC�Y�C�L�C�L�C�&fC�L�C�L�C�33C�L�C�ffC�Y�C�@ C�L�C�@ C�&fC�33C�L�C�ffC�Y�C�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�Y�C�@ C�Y�C�@ C�33C�L�C�Y�C�ffC�Y�C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�L�C�L�C�33C�Y�C�L�C�@ C�Y�C�@ C�33C�L�C�ffC�Y�C�@ C�ffC�@ C�33C�@ C�L�C�ffC�ffC�L�C�ffC�Y�C�L�C�@ C�33C�ffC�L�C�33C�Y�C�@ C�33C�Y�C�ffC�Y�C�@ C�Y�C�ffC�Y�C�@ C�L�C�ffC�Y�C�33C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�Y�C�@ C�&fC�Y�C�ffC�&fC�33C�@ D &fD ��D9�D�fD  Dy�D� D
�D� DffD�D�fD` D  D� DFfD!ٚD$s3D&�3D)` D+�fD.ffD0�3D3�3D6�D8�3D;@ D=�3D@l�DC�DE�3DH&fDJ�3DM�fDP&fDR�3DUy�DX  DZ� D]Y�D_��Db��De  Dg��Dj33Dl�fDoY�Dq�fDtl�Dv��Dys3D{y�D~  D�FfD��fD�ɚD��D�L�D���D�� D��D�\�D�� D��D�0 D�p D���D��fD�C3D���D��3D�  D�ffD�� D�� D�33D�y�D���D�� D�  D�\�D��3D��3D�� D�#3D�` D�� D��3D��fD��D�C3D�l�D���D���D��fD���D� D�&fD�<�D�VfD�ffD�y�D�� D��fD���D���D��3D�	�D�  D�<�D�VfD�p DƉ�DǬ�D�ɚD�� D���D�  D�<�D�VfD�y�DЙ�DѰ D�� D��3D��D�&fD�I�D�s3DٖfDڶfD��fD�  D�  D�FfD�ffD��D� D�ٚD�3D�,�D�L�D�|�D��D��3D�  D�0 D�\�DD��fD���D�,�D�` D���D���D�� D�3D�FfD�\�D��fD���D��fD��E �fEfE� E6fEɚEY�E� Ep E�3E�3E3E��E E��E��E
+3E,�E�3E�fE4�E0 E�3E�3E6fE9�E�fE��E>fEA�E��E��EњE` E h E!� E"��E$x E%vfE&�E'�fE)` E*[3E+�3E-9�E.1�E/�3E0�fE1��E3I�E4��E5��E6� E8P E9�3E:�3E;�fE?0 EB�EE<�EHq�EK�3EN� EQ��EU EXD�E[l�E^�fEa� Ed~fEg��Ej��Em�3Ep��Et9�Ew!�EzY�E}{3E�A�E��3E�i�E��E�� E�fE�� E�P E��fE�a�E���E�D�E��3E�њE��E���E���E��E�nfE��3E��3E�\ E��3E��E�FfE�� E��E�)�E���G�O�G�O�?fff?�  G�O�G�O�G�O�G�O�?�  G�O�G�O�?���G�O�?���?�ff?�  ?���?�ff?�33@��@33@   @,��@Fff@L��@`  @s33@�33@�  @���@�ff@�33@���@���@�ff@�ff@�33@���AffAffA��A��A#33A+33A1��A;33AA��AK33AQ��AX  A^ffAd��Ak33As33Ay��A�  A�33A�ffA�ffA�ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441144441441411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @ j@ v@ �@ �@ O@ "�@ )�@ 0x@ 7�@ >@ E�@ R�@ _�@ l�@ |?@ �7@ �0@ �5@ �-@ �&@ �|@ �#@ ��@ � @j@@ @-�@:@G�@UU@c�@qS@~�@�P@��@��@��@@��@�;@�@�,@�@{@!s@0x@>�@Lu@Yn@e�@t�@�@�\@�@��@��@ƨ@��@��@�@�E@
=@6@&�@4�@@�@N�@\)@k�@x&@�p@�#@�@�f@�k@�c@խ@�`@�Y@�Q@@�@(�@7�@D�@R�@`�@m�@z�@��@��@��@�~@�w@�*@��@��@�q@�@�@ �@-@:@I@UU@a�@qS@�W@�P@��@��@��@�>@��@�/@�@��@�@*@!s@0x@>�@K@Wb@e�@t�@��@�@�@�Y@�@��@��@��@�@@��@J@�@%�@5?@B8@O0@\�@i!@y�@�|@�u@�@��@�k@�@�[@�`@�e@^@�@�@(�@5?@C�@R�@a�@n�@z�@�7@�<@��@�!@�&@�|@��@��@�@	j@	o@	�@	+@	:@	H]@	Wb@	dZ@	p�@	�@	��@	��@	��@	�F@	Ĝ@	є@	�/@	�4@	�9@
�@
@
""@
1'@
=q@
I�@
X�@
g@
t�@
�@
��@
��@
��@
��@
��@
��@
��@
��@
��@
=@�@%�@2�@@�@O0@^5@k�@x&@�+@�$@�@�@�@�o@׹@�@�@  @�@�@+@7�@DD@SI@a�@n�@z�@�7@�<@�5@��@�&@�|@��@�@�e@�@@g@.l@;d@G�@S�@dZ@r�@|�@�D@��@��@�F@�J@�O@��@�m@p�@��@  @H]@��@׹@""@i�@�~@��@@,@�+@�o@�@R�@��@܀@$.@i!@�!@��@<@�@��@�@V@�@��@0x@y�@��@
=@Q�@��@��@&�@m:@��@��@>@�p@�@@SI@�<@ψ@{@Z@�a@�T@(G@l�@�!@��@<@�@��@J@Q�@�0@��@ @g@��@�@ :@ �@ ��@!
=@!O0@!��@!�@"O@"\)@"��@"��@#"�@#b�@#��@#�@$(�@$j@$�@$��@%)�@%i!@%��@%�T@& @&^�@&�@&�h@'�@'O�@'��@'Ĝ@(  @(;d@(v�@(�9@(�@)+@)ff@)�(@)�;@*O@*Wb@*��@*�C@+�@+Ji@+��@+�J@,]@,?}@,|�@,�R@,�Y@-0x@-o�@-�@-�(@.)�@.g�@.�4@.�@/""@/_�@/�a@/��@0�@0X�@0�<@0׹@16@1T�@1��@1�\@2*@2UU@2�0@2�\@3�@3Z@3��@3�/@4�@4^5@4�@4��@5g@5`�@5�U@5��@6�@6V�@6��@6��@7b@7N�@7�C@7�@8�@8DD@8~K@8��@8�@9-�@9g@9�m@9�@:K�@:�L@;^5@<�@<s_@=�@=��@>&�@>�#@?9X@?��@@M�@@�^@A^�@A�|@BqS@B�T@CR�@C��@Dm:@E�@E��@F(�@F��@G7L@G�(@H@�@H�@II@I�`@JO1@J�@K}�@K�T@L{�@M�@MqS@N
=@N�m@O7�@O��@P/@Q�\@R�>@T$/@U�d@V�@X�@Y�W@Z�@\B�@]��@^�@@`:@az2@b�(@d �@euk@fĜ@h0x@im�@j�|@l#�@mn�@n�/@p @q��@r��@t$.@u��@v��@x1�@y|?@z�k@{�@{T�@{��@{�|@|&;@|`B@|�I@|��@}+�@}e�@}��@}�@~K�@~��@~��@@E�@��G�O�G�O�@ �@ jG�O�G�O�G�O�G�O�@ jG�O�G�O�@ G�O�@ �@ v@ �@ �@ �@ 	�@ �@ J@ �@ @ �@ o@ {@ �@ �@ O@ [@  @ "�@ $�@ (G@ *S@ -�@ 0x@ 2�@ 5�@ 9X@ <@ ?}@ B8@ E�@ H]@ Lu@ O0@ SI@ V@ X�@ [z@ ^5@ `�@ dZ@ g@ i�@ l�@ oF@ r�@ vG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A��A��A��A��A��A��A�$�A�"�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�(�A�
=A�Ȧ+A˟�Aɺ^Aǟ�A�l�A���A�K�A��yA�hsA��+A�%A��A��/A���A�r�A�E�A�+A��;A�z�A�-A��^A�JA��A�-A���A�p�A�$�A��#A�5?A�oA�hsA�K�A�"�A���A��A��TA���A��A�~�A�n�A�7LA��A�ĜA�=qA�+A��uA��+A���A�l�A�G�A�/A��A��A�G�A��\A�ĜA�33A��hA�&�A�ȴA�ƨA��7A�JA��DA�dZA���A��yA�bNA��A��#A��+A�1'A�M�A�1A�A�A�A��A���A�Q�A�A�l�A��-A�=qA�33A���A�K�A�`BA��A��wA�bA�(�A�oA��RA�oA��#A�
=A�A��TA�^A~ĜA~v�A}��A|�DAz�Ay�AxȴAvĜAq�;AoC�Akl�Ag��Ac�AaO�A_��A]|�A\I�A[��AZ��AW%AT�ATffAR��APffAN��AMp�AK��AK|�AJ��AI��AIoAGC�AD��AC�AC�PAA`BA?%A>~�A>A�A=��A=A;��A:jA8��A7XA6{A5�A4z�A2��A1hsA0jA/�;A/p�A-�A,�\A+G�A*��A)�A(Q�A&�+A%&�A$5?A"�HA!l�A JA��A`BAZAdZAoA�\A�A��A��A�A"�A��A�AI�A�9A��AJA��AA�A�A��AZA�A�AK�AA
��A	\)A��AȴA��A{AVA�AS�A+A ��A  �@�+@�V@��9@���@���@�5?@��j@��@��\@�dZ@�@�9@�Z@��m@�;d@�J@�+@�ȴ@柾@�n�@�{@�X@�u@�@�@�t�@���@�E�@��T@ܴ9@ܛ�@۶F@�=q@�p�@׮@�Z@�bN@�Z@� �@�$�@��@�{@��@�hs@϶F@�$�@�ȴ@�V@�I�@�5?@��@�%@��`@��/@��@���@�-@���@�%@�dZ@���@�G�@��R@���@���@��`@�l�@��\@��@��`@��;@��H@�O�@��u@��@�{@�r�@��R@���@��m@�dZ@�+@���@��@�@���@���@|��@~�+@}O�@z�H@y&�@xr�@vv�@v{@t��@s��@rn�@n�+@k��@j��@i%@g�;@e��@dz�@b-@`��@_;d@]@]?}@[33@ZM�@Xb@V$�@T�/@T�@R~�@Q�7@Q&�@O�@N��@M��@K�F@J��@HQ�@E�T@Dz�@B��@A�7@A7L@?\)@>$�@=��@<�@<z�@;t�@8r�@7|�@6��@4��@3t�@2-@1G�@/��@.E�@-�@,�j@+��@*�!@)&�@(�u@'�@&E�@%�h@$��@$Z@$1@#t�@"�\@!�@ r�@l�@�@5?@�@�@(�@�
@o@M�@  @l�@�@�h@�@�j@�@�H@��@�@r�@�;@;d@�@/@�D@��@C�@
~�@	��@	G�@��@A�@  @
=@��@�T@�@�D@�F@o@n�@��@��@X@ �`@ 1'?�|�?��?���?��m?�x�?�r�?���?���?���?�bN?��?���?�?��#?�1'?�+?���?�o?�G�?�A�?���?���?܋D?�?���?�7L?�Q�?և+?��?���?���?ҏ\?ѩ�?��`?��;?Η�?͑h?�O�?�ƨ?�C�?�=q?ɺ^?ȴ9?�r�?��?�K�?Ƈ+?��
?�33?�M�?�&�?�\)?��?�V?��-?��?�(�?��m?��?�"�?�?�"�?�C�?��?�"�?��?��m?�(�?�j?��?�/?��-?�5??���?�\)?�  ?��?���?��`?�%?�&�?�hs?�hs?��7?���?��?��?�-?�M�?�n�?\?°!?��?�o?�S�A� �A�"�A�bA�1A�
=A�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�$�A�"�A�$�A�"�A�$�A�$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      A��A��A��A��A��A��A��A��A��A��A��A�$�A�"�A�$�A�$�A�&�A�&�A�&�A�(�A�(�A�(�A�(�A�&�A�(�A�
=A�Ȧ+A˟�Aɺ^Aǟ�A�l�A���A�K�A��yA�hsA��+A�%A��A��/A���A�r�A�E�A�+A��;A�z�A�-A��^A�JA��A�-A���A�p�A�$�A��#A�5?A�oA�hsA�K�A�"�A���A��A��TA���A��A�~�A�n�A�7LA��A�ĜA�=qA�+A��uA��+A���A�l�A�G�A�/A��A��A�G�A��\A�ĜA�33A��hA�&�A�ȴA�ƨA��7A�JA��DA�dZA���A��yA�bNA��A��#A��+A�1'A�M�A�1A�A�A�A��A���A�Q�A�A�l�A��-A�=qA�33A���A�K�A�`BA��A��wA�bA�(�A�oA��RA�oA��#A�
=A�A��TA�^A~ĜA~v�A}��A|�DAz�Ay�AxȴAvĜAq�;AoC�Akl�Ag��Ac�AaO�A_��A]|�A\I�A[��AZ��AW%AT�ATffAR��APffAN��AMp�AK��AK|�AJ��AI��AIoAGC�AD��AC�AC�PAA`BA?%A>~�A>A�A=��A=A;��A:jA8��A7XA6{A5�A4z�A2��A1hsA0jA/�;A/p�A-�A,�\A+G�A*��A)�A(Q�A&�+A%&�A$5?A"�HA!l�A JA��A`BAZAdZAoA�\A�A��A��A�A"�A��A�AI�A�9A��AJA��AA�A�A��AZA�A�AK�AA
��A	\)A��AȴA��A{AVA�AS�A+A ��A  �@�+@�V@��9@���@���@�5?@��j@��@��\@�dZ@�@�9@�Z@��m@�;d@�J@�+@�ȴ@柾@�n�@�{@�X@�u@�@�@�t�@���@�E�@��T@ܴ9@ܛ�@۶F@�=q@�p�@׮@�Z@�bN@�Z@� �@�$�@��@�{@��@�hs@϶F@�$�@�ȴ@�V@�I�@�5?@��@�%@��`@��/@��@���@�-@���@�%@�dZ@���@�G�@��R@���@���@��`@�l�@��\@��@��`@��;@��H@�O�@��u@��@�{@�r�@��R@���@��m@�dZ@�+@���@��@�@���@���@|��@~�+@}O�@z�H@y&�@xr�@vv�@v{@t��@s��@rn�@n�+@k��@j��@i%@g�;@e��@dz�@b-@`��@_;d@]@]?}@[33@ZM�@Xb@V$�@T�/@T�@R~�@Q�7@Q&�@O�@N��@M��@K�F@J��@HQ�@E�T@Dz�@B��@A�7@A7L@?\)@>$�@=��@<�@<z�@;t�@8r�@7|�@6��@4��@3t�@2-@1G�@/��@.E�@-�@,�j@+��@*�!@)&�@(�u@'�@&E�@%�h@$��@$Z@$1@#t�@"�\@!�@ r�@l�@�@5?@�@�@(�@�
@o@M�@  @l�@�@�h@�@�j@�@�H@��@�@r�@�;@;d@�@/@�D@��@C�@
~�@	��@	G�@��@A�@  @
=@��@�T@�@�D@�F@o@n�@��@��@X@ �`@ 1'?�|�?��?���?��m?�x�?�r�?���?���?���?�bN?��?���?�?��#?�1'?�+?���?�o?�G�?�A�?���?���?܋D?�?���?�7L?�Q�?և+?��?���?���?ҏ\?ѩ�?��`?��;?Η�?͑h?�O�?�ƨ?�C�?�=q?ɺ^?ȴ9?�r�?��?�K�?Ƈ+?��
?�33?�M�?�&�?�\)?��?�V?��-?��?�(�?��m?��?�"�?�?�"�?�C�?��?�"�?��?��m?�(�?�j?��?�/?��-?�5??���?�\)?�  ?��?���?��`?�%?�&�?�hs?�hs?��7?���?��?��?�-?�M�?�n�?\?°!?��?�o?�S�A� �A�"�A�bA�1A�
=A�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A��A��A��A�{A��A��A��A��A��A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A�"�A�"�A�$�A�"�A�$�A�"�A�$�A�$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
��B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B%B[#B��B�3BǮB�;B�3B�dB�B��B��B��B+BoBoB�B�B�B�B�B!�B$�B-B:^BI�BK�BL�BA�BF�BJ�BF�BT�B��B��B��B��B��B��B�B�B�-B�-B�'B�'B�-B�B�B��B��B��B��B��B�+B�%B�B~�Bw�Bu�Bp�B^5BT�BN�BH�BE�B?}B'�B�BbBB��B�B�B�sB��B��BB�3B��B�{B�=Bp�BbNBYBM�BH�B;dB1'B"�B
�B
�fB
�BB
��B
B
��B
�PB
�B
{�B
jB
[#B
Q�B
D�B
C�B
?}B
7LB
)�B
�B
hB
1B	�B	��B	��B	{�B	Q�B	49B	�B	&�B	&�B	+B	1'B	;dB	�B	�B	�B	uB	B��B�B�B�B�yB�`B�#B��BŢBĜBB�XB�FB�3B�-B�'B�B��B��B��B�oB�hB�VB�+B�B�B}�B~�B}�By�Bu�Bt�Bs�Bq�Bo�Bl�Bn�Bm�Bl�Bk�Bm�Bn�Bn�Bn�Bp�Bo�Bo�Bm�Bk�BhsBffBgmBffBcTB^5B]/BYBS�BS�BR�BP�BO�BM�BF�BH�BF�BE�BA�B=qB>wB<jB:^B9XB8RB8RB8RB6FB5?B5?B5?B5?B5?B5?B49B33B5?B49B33B33B5?B:^B8RB7LB7LB49B:^B9XB8RB8RB8RB9XB9XB<jB=qBF�BG�BH�BI�BK�BQ�BVBXBW
BVBjBjBk�Bk�Bq�B�B�B�qB�B��B	B	DB��B�BB�
B�BB�#B	  B		7B		7B��B	hB	B�B	:^B	XB	_;B	iyB	w�B	m�B	t�B	r�B	{�B	�B	�PB	��B	��B	��B	�3B	�XB	��B	ŢB	ǮB	��B	��B	��B	�
B	�5B	�;B	�BB	�`B	�yB	�B	�ZB	�B	��B	��B	��B	��B	��B
B
B
+B
	7B

=B
	7B
bB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
#�B
&�B
(�B
)�B
)�B
,B
-B
-B
/B
0!B
1'B
49B
33B
6FB
8RB
8RB
:^B
:^B
;dB
>wB
=qB
>wB
@�B
@�B
A�B
C�B
D�B
D�B
F�B
G�B
H�B
I�B
K�B
K�B
L�B
L�B
N�B
N�B
P�B
P�B
R�B
T�B
S�B
T�B
VB
VB
YB
YB
ZB
[#B
]/B
^5B
]/B
`BB
`BB
aHB
bNB
bNB
cTB
ffB
ffB
ffB
hsB
hsB
iyB
jB
k�B
m�B
m�B
m�B
n�B
n�B
p�B
q�B
q�B
r�B
r�B
s�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
y�B
z�B
z�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�+B
�1B
�7B
�=B
�DB
�DB
�VB
�PB
�\B
�bB
�oB
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
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�9B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�RB
�XB
�XB
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
�jB
�jB
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
�jB
�dB
�dB
�^B
��B
�B
�B
��B
�B
�B
��B
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
��B
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
��B
�B
��B
��B
��B
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B
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
�BBYB��B�'BŢB�/B�'B�XB�
B�B��B��BBbBbB{B�B�B�B�B�B"�B+B8RBG�BI�BJ�B?}BD�BH�BD�BR�B�uB��B��B��B��B��B��B�B�!B�!B�B�B�!B�B��B��B��B��B��B�uB�B�B�B}�Bv�Bt�Bo�B]/BS�BM�BG�BD�B>wB&�B�B\BB��B�B�B�mB��B��B��B�-B��B�uB�7Bo�BaHBXBL�BG�B:^B0!B!�B
�B
�`B
�;B
��B
��B
��B
�JB
� B
z�B
iyB
ZB
P�B
C�B
B�B
>wB
6FB
(�B
�B
bB
+B	�B	��B	��B	z�B	P�B	33B	�B	%�B	%�B	)�B	0!B	:^B	�B	�B	�B	oB	  B��B�B�B�B�sB�ZB�B��BĜBÖB��B�RB�?B�-B�'B�!B�B��B��B��B�hB�bB�PB�%B�B�B|�B}�B|�Bx�Bt�Bs�Br�Bp�Bn�Bk�Bm�Bl�Bk�BjBl�Bm�Bm�Bm�Bo�Bn�Bn�Bl�BjBgmBe`BffBe`BbNB]/B\)BXBR�BR�BQ�BO�BN�BL�BE�BG�BE�BD�B@�B<jB=qB;dB9XB8RB7LB7LB7LB5?B49B49B49B49B49B49B33B2-B49B33B2-B2-B49B9XB7LB6FB6FB33B9XB8RB7LB7LB7LB8RB8RB;dB<jBE�BF�BG�BH�BJ�BP�BT�BW
BVBT�BiyBiyBjBjBp�B�B�B�jB�B��B	B	
=B��B�;B�B�;B�B��B	1B	1B��B	bB	A�B	9XB	W
B	^5B	hsB	v�B	l�B	s�B	q�B	z�B	�B	�JB	��B	��B	��B	�-B	�RB	��B	ĜB	ƨB	��B	��B	��B	�B	�/B	�5B	�;B	�ZB	�sB	�B	�TB	�B	��B	��B	��B	��B	��B
B
B
+B
	7B

=B
	7B
bB
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
"�B
#�B
&�B
(�B
)�B
)�B
,B
-B
-B
/B
0!B
1'B
49B
33B
6FB
8RB
8RB
:^B
:^B
;dB
>wB
=qB
>wB
@�B
@�B
A�B
C�B
D�B
D�B
F�B
G�B
H�B
I�B
K�B
K�B
L�B
L�B
N�B
N�B
P�B
P�B
R�B
T�B
S�B
T�B
VB
VB
YB
YB
ZB
[#B
]/B
^5B
]/B
`BB
`BB
aHB
bNB
bNB
cTB
ffB
ffB
ffB
hsB
hsB
iyB
jB
k�B
m�B
m�B
m�B
n�B
n�B
p�B
r�B
r�B
s�B
s�B
t�B
v�B
v�B
v�B
w�B
x�B
x�B
x�B
z�B
{�B
{�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�7B
�=B
�DB
�JB
�JB
�\B
�VB
�bB
�hB
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
�B
�B
�B
�B
�B
�B
�'B
�-B
�3B
�9B
�?B
�?B
�FB
�LB
�RB
�^B
�^B
�dB
�dB
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�wB
�qB
�wB
�}B
��B
��B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
�wB
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
�B
�B
�B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202372021061413522020210614135220202106141746272021061417462720210614174627201807242202372021061413522020210614135220202106141746272021061417462720210614174627PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023720180724220237  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023720180724220237QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023720180724220237QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015020210722160150IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                