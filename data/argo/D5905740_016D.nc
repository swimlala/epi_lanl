CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-20T07:05:24Z creation      
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
_FillValue                 4  �|   TEMP_ADJUSTED            
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
_FillValue                 4  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ƈ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ی   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20180820070524  20210722161420  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�sx�+@�sx�+11  @�sx`�@�sx`�@*��R~R@*��R~R�cI�3]$��cI�3]$�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?fff@   @@  @y��@�33@�33@�  A��A  A$��AC33A`  A�  A���A���A�  A���A���A�33A�  B ��B��B��BffB ffB'��B0  B8��B@ffBHffBPffBX  B`ffBh  Bo��Bx  B�ffB�  B�  B�ffB�  B���B�33B�  B�  B�33B�33B�  B�ffB�ffB�33B�33B���B�  B�  B̙�B�  B�33Bؙ�B�33Bߙ�B�  B�33B���B���B�33B�33B���B���C�C33C�fC�C
L�C�fC�C�C33CL�C  C  C33C�fC�3C�fC"  C$�C&33C'��C)�fC,�C.�C0�C233C4L�C6  C8�C:33C;��C=�fC?�fCB  CD  CF�CH33CI�fCL  CN�CP�CR�CT�CV33CX33CZL�C[�fC^  C`  Cb�Cd33Cf33Ch33Cj�Cl�Cn�Cp�Cr  Ct  Cv  Cx33Cz�C|  C~�C��C��3C��C�  C��fC�  C��C�  C��fC�  C��C��C��3C��C�&fC�  C��fC�  C��C��C��fC��3C��C��C��C�  C��fC��3C��C�&fC�  C��fC��3C��C��C��C��C�&fC�  C��fC��3C�  C��C��C�&fC��C�  C��C�&fC��C��fC��fC�  C��C�&fC��C��3C�  C��C��C�  C��fC��3C�  C��C��C�&fC�33C�&fC�  C��C�&fC�  C��3C�  C�  C��C��3C�ٚC��fC�  C��C�&fC��C��fC��3C�  C��C�&fC�&fC��C��fC��fC��3C��3C�  C��C�  C��fC��3C�  C��C�&fC��C��fC�  C��C��C��C��fC��3C��C��C��C��fC�  C��C��C��3C�  C��C��C��C��3Dy�D�D� D
��Dl�D  DٚD�fD� DL�D�D �fD#�3D&` D)FfD,�D.��D1��D4ffD79�D:fD<� D?�3DBl�DE33DHfDJ��DM��DPL�DS3DU��DX�fD[� D^y�DaFfDd  Df��Di��Dly�DoFfDrfDt�fDw�fDz9�D|� D@ D�3D�` D�� D�	�D�` D��fD�	�D�S3D���D�� D�9�D�|�D�� D� D�VfD���D�  D�FfD�� D�� D�33D��3D�ٚD�6fD���D��fD�VfD�� D�fD�p D�ɚD��D�s3D��3D�&fD�� D��3D�,�D�� D��3D�)�D�vfD�� D� D�VfD��fD��3D�3D�\�D�� D��3D�,�D�i�DƬ�D���D�,�D�p Dˬ�D��3D��D�P DІfDѶfD���D�)�D�S3D։�D׼�D��D�fD�I�D�vfDݩ�D��3D���D�&fD�S3D�3D� D�ٚD�	�D�0 D�VfD�vfD��D��3D��D� D�6fD�S3D�vfD��D�� D�� D�fD�<�D�p D�|�D��fD��3D���D�)�E +3E �fENfE�3Ex EfE�fE<�EњEa�E�fE�3E#3E�fEK3E� E	q�E
 E
��E9�E�fE` E� E�fE�fE EɚE�3E�EFfEs3E��EњE��E� E��E#3E S3E!~fE"� E#њE$�fE&�3E'�fE)fE*C3E+t�E,�3E-ɚE/�fE0��E1��E2��E4 E54�E6�E7��E9 E:8 E;�fE>� EA��ED�EH EK&fEN��EQ�3ETt�EW��EZ�3E^1�E`�Edd�Eg[3EjFfEm� Ep� Es��Ev�3Ey��E}\�E�)�E��fE�@ E���E�q�E�fE�� E��E��fE�8�E��3E�FfE�՚E�� E��E��fE��E�ɚE�H�E��fE�\�E���E�� E�fE���E�0 E��fE�P E��3E���E�73E�� E�՚E�5�E�t E���E��E�l�E��3E��E�\�E�� E��E�RfE���E��E�6fE���E���E�%�E�vfE��3E��?333?fff?L��?L��?333?333?L��?L��?333?L��?fff?L��?L��?L��?333?L��?L��?fff?�  ?�  ?���?���?�33?�  ?���?ٙ�@   @��@33@&ff@9��@Fff@S33@fff@y��@�ff@�33@���@���@�  @���@ə�@�ff@�  @���@���A33A33A33A33A   A&ffA0  A4��A@  AFffAP  AT��A\��Ac33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441441144441411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ?�33@   @`  @���@�33@�33@�  A	��A  A,��AK33Ah  A�  A���A���A�  A���A���A�33A�  B��B
��B��BffB"ffB)��B2  B:��BBffBJffBRffBZ  BbffBj  Bq��Bz  B�ffB�  B�  B�ffB�  B���B�33B�  B�  B�33B�33B�  B�ffB�ffB�33B�33B���B�  B�  B͙�B�  B�33Bٙ�B�33B���B�  B�33B���B���B�33B�33B���C ffC��C�3CffC��C
��CffC��C��C�3C��C� C� C�3CffC33C ffC"� C$��C&�3C(L�C*ffC,��C.��C0��C2�3C4��C6� C8��C:�3C<L�C>ffC@ffCB� CD� CF��CH�3CJffCL� CN��CP��CR��CT��CV�3CX�3CZ��C\ffC^� C`� Cb��Cd�3Cf�3Ch�3Cj��Cl��Cn��Cp��Cr� Ct� Cv� Cx�3Cz��C|� C~��C�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�ffC�@ C�&fC�@ C�Y�C�L�C�&fC�33C�L�C�L�C�Y�C�@ C�&fC�33C�L�C�ffC�@ C�&fC�33C�L�C�L�C�L�C�L�C�ffC�@ C�&fC�33C�@ C�Y�C�Y�C�ffC�L�C�@ C�L�C�ffC�L�C�&fC�&fC�@ C�Y�C�ffC�Y�C�33C�@ C�L�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�ffC�s3C�ffC�@ C�L�C�ffC�@ C�33C�@ C�@ C�Y�C�33C��C�&fC�@ C�Y�C�ffC�L�C�&fC�33C�@ C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�33C�@ C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�L�C�&fC�33C�L�C�Y�C�L�C�&fC�@ C�Y�C�L�C�33C�@ C�L�C�Y�C�L�C�33D��D9�D  D
��D��D@ D��D�fD� Dl�D,�D �fD#�3D&� D)ffD,,�D.��D1��D4�fD7Y�D:&fD=  D?�3DB��DES3DH&fDJ��DM��DPl�DS33DV�DX�fD[� D^��DaffDd@ Dg�Di��Dl��DoffDr&fDt�fDw�fDzY�D|� D` D�3D�p D�� D��D�p D��fD��D�c3D���D�  D�I�D���D�� D�  D�ffD���D� D�VfD�� D�� D�C3D��3D��D�FfD���D�fD�ffD�� D�&fD�� D�ٚD�)�D��3D��3D�6fD�� D��3D�<�D�� D��3D�9�D��fD�� D�  D�ffD��fD��3D�#3D�l�D�� D��3D�<�D�y�DƼ�D���D�<�Dʀ D˼�D��3D�)�D�` DЖfD��fD���D�9�D�c3D֙�D���D���D�&fD�Y�D܆fDݹ�D��3D��D�6fD�c3D�3D�� D��D��D�@ D�ffD�fD��D��3D���D�  D�FfD�c3D�fD��D�� D�  D�&fD�L�D�� D���D��fD��3D��D�9�E 33E �fEVfE�3E� EfE�fED�EٚEi�E�fE�3E+3E�fES3E� E	y�E
 E
��EA�E�fEh E� E�fE�fE EњE�3E!�ENfE{3E��EٚE�E� E��E+3E [3E!�fE"� E#ٚE%fE&�3E'�fE)fE*K3E+|�E,�3E-њE/�fE0��E1��E3�E4  E5<�E6�E8�E9  E:@ E;�fE>� EA��ED��EH EK.fEN��EQ�3ET|�EW��E[3E^9�E`�Edl�Egc3EjNfEm� Ep� Es��Ew3Ez�E}d�E�-�E��fE�D E���E�u�E�fE�� E��E��fE�<�E��3E�JfE�ٚE�� E��E��fE��E�͚E�L�E��fE�`�E���E�� E�fE���E�4 E��fE�T E��3E���E�;3E�� E�ٚE�9�E�x E���E��E�p�E��3E��E�`�E�� E��E�VfE���E��E�:fE���E���E�)�E�zfE��3E��?���G�O�G�O�G�O�G�O�?���G�O�G�O�?���?�ffG�O�G�O�G�O�G�O�?���G�O�?�ff?�33G�O�?�  G�O�?ٙ�?�33@   @ff@��@   @,��@333@Fff@Y��@fff@s33@�33@���@�ff@�33@���@���@�  @ə�@ٙ�@�ff@�  @���AffA33A33A33A#33A(  A.ffA8  A<��AH  ANffAX  A\��Ad��Ak33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144441441144441411414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              @ %@ �@ {@ �@ "�@ )�@ /�@ 7L@ =q@ FQ@ SI@ _�@ m:@ {�@ �7@ �0@ ��@ �-@ �w@ ��@ ��@ �y@ � @@�@
@,`@;d@H]@V@c�@p�@~�@��@��@�A@�F@@�7@�;@�@�~@�@{@""@0x@>@K@Z@g�@t�@�d@��@�@��@�^@��@�O@�T@�@�9@
=@�@$�@2�@A�@O0@^5@i!@x&@�|@�@�@�!@�@�@׹@�@�e@  @�@�@(G@4�@C�@Q�@`B@n�@y�@��@��@��@�-@��@��@�t@��@� @@b@
@,`@:@H]@V�@bN@p�@~�@��@�H@��@�F@��@�C@�/@�@�,@�@�@#�@1'@>@K�@Yn@g@t@��@�\@�a@�Y@�R@ƨ@�O@��@�L@��@�@�@&�@33@?}@N�@]�@j@v�@��@��@�m@��@��@��@׹@�T@�@ �@V@�@(�@5?@C�@R�@a�@m:@y�@��@��@��@�-@��@��@�t@�@�@	j@	o@	 @	.l@	:�@	G�@	V@	e	@	qS@	|�@	��@	��@	��@	��@	��@	ψ@	��@	�4@	��@
�@
@
!s@
/�@
>@
Lu@
Z�@
i!@
v@
��@
�@
�@
��@
��@
��@
Ӡ@
�@
�@@
��@�@�@&�@5?@A�@M$@[z@i�@x�@�+@��@�@��@�^@ȴ@�[@�@�@  @J@�@(�@7L@FQ@R�@^5@m:@{�@��@��@�y@��@��@�*@�#@�@��@�@�@
@,`@:�@I@V@bN@��@2�@~K@��@�@_�@��@��@DD@��@��@&;@r�@�&@V@Z@�5@�L@>@�D@׹@%�@r�@�@�@V@��@��@7�@��@��@g@m:@�@�@UU@��@��@9X@��@��@�@g@��@��@:@��@�7@�@a�@��@�@<@�d@�@@Wb@�U@�H@(�@n�@��@   @ E�@ ��@ Ӡ@!�@!c�@!��@!� @"B�@"�P@"�h@#""@#n�@#�R@$@$I�@$�u@$�#@%&�@%p�@%�@&�@&K@&�u@&܀@'#�@'i�@'�~@'� @(;d@(~�@(�>@)	�@)N�@)�u@)��@*[@*bN@*��@*��@+/�@+s_@+��@+��@,:@,|?@,�@,�Q@-B�@-�d@-Ĝ@.%@.FQ@.�|@.�@/1@/I�@/�7@/ȴ@01@0H]@0�7@0�c@1�@1I�@1��@1�W@2�@2C�@2�d@2�2@3  @3>�@3{�@3��@3�~@46�@4ww@4�F@4�@56�@5o�@5�r@5�@6/@6oF@6�r@6�@@7+�@7k.@7��@7��@8+�@8k�@8�Z@8��@9(G@9g�@9��@9�l@:&�@:ff@:��@:�@;&;@;g@;��@;�@<ff@<�@=bN@=�H@>�@?�@?��@@	@@�a@A �@A�@B �@B�@Ce	@C�@Dg�@D�l@Eff@E�`@Fe�@G&�@G��@H$�@H�4@I'�@I�4@J&;@J�@Ka�@K��@L\�@L�
@MP�@N�@N~�@N�,@Ot@P$�@QN�@R��@T�@U\)@V�f@X-@Yk.@Z��@\�@]n�@^�|@_�q@ar�@b�F@c�@e_�@f�c@h@i`�@j��@l�@m]�@n�@p  @q\�@r��@t�@ub�@v�r@w�e@y\�@z�w@{��@}Lv@~�&@��@���@�R�@�
�@��l@�Q=@���@���@�X�@��Q@��@�UU@��2@���@��W@���@�P@�6�@�P�@�y�@��{@��@�׹@���@�B@�?}@�e	@��=@��@���@���@�V@�/@�P�@�r�@��*@���@��@��E@ �G�O�G�O�G�O�G�O�@ �G�O�G�O�@ �@ vG�O�G�O�G�O�G�O�@ �G�O�@ v@ %G�O�@ �G�O�@ 1@ 	�@ 
=@ 
�@ �@ �@ @ �@ �@ �@ *@ �@ �@ �@ �@ g@ !s@ $.@ %�@ '�@ +@ -�@ /�@ 2�@ 5�@ 7�@ ;d@ >�@ B8@ DD@ F�@ K@ M$@ Q�@ T�@ X�@ Z�@ ^5@ `�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A݉7A݇+A݉7A݋DA݋DA݁A݅A�~�A�x�A�v�A�r�A�t�A�~�Aݏ\Aݡ�Aݺ^A��#A�A�$�A�&�A�$�A� �A��A��A�JA���A��yA�ƨAݬAݑhA�+A�ȴA�K�A�
=A�bA�dZAЏ\A�M�AƲ-A�bA�^5A��hA�;dA�5?A���A���A���A�1'A��+A�VA�t�A�  A�7LA�I�A��uA�%A�Q�A�-A��A�~�A���A���A�M�A�ffA��A��uA�M�A�^5A�
=A�33A�VA��/A�`BA��A�A��A��wA��/A�7Az~�Au|�Aqx�Ak�7Ad~�A^�/AZI�AX��AU��AR�HAO�7ALZAKK�AJM�AH�AEK�AD�AC�AAK�A>�yA=%A;G�A8��A6��A5ƨA5A45?A2ZA0��A.��A-A,�A-33A-�hA-�A,1'A*ffA(�/A'�#A'XA'��A'x�A&  A%��A%\)A$�/A$A�A#�A#|�A#`BA"�HA"^5A!�;A!dZA!/AAv�A$�A��A�A�#A�wA
=A/AE�AJAbNAffAM�AS�A�^A"�A�!AI�AM�AA
=An�A�Ar�AA�A  A��A�DAVA�AAK�A��AE�A�AffA�uAjA�A��AA�A1A��A��A�TA{A�+A�\A�AƨA�PA33A
�A
�RA
jA	�hA	`BA	?}A�RA��A�wAK�A"�A%A�A��A��AbNA��A�
A�A|�A`BA7LAVAz�A�7A��A�/A��Az�A=qA��AG�A ��A �uA E�@�\)@�M�@��@��u@���@���@���@���@�b@��@�
=@�O�@�  @�K�@�-@�O�@�@�r�@�(�@��;@�|�@@�J@�/@�A�@�F@�@�=q@��#@�x�@�V@蛦@�A�@�|�@㝲@ݺ^@�@��@�33@�r�@Χ�@̴9@��#@�"�@ũ�@Ý�@�5?@��@�@�9X@���@�9X@�$�@��@���@���@��j@�$�@�ƨ@���@��@�o@�%@�
=@���@���@���@�@���@�"�@�J@�?}@�ƨ@�=q@�&�@�(�@�V@��^@�r�@�"�@��^@�1'@�\)@�M�@��-@���@�@���@�Q�@~$�@|1@yX@x1'@vV@s�
@r�\@p�`@oK�@m@l(�@i�7@gK�@dZ@b=q@`�9@^��@]�@[��@Y��@W�;@U`B@S"�@Q&�@NV@L�D@J�!@Hr�@G
=@EV@C�
@C@A�7@@�`@@�u@?\)@>V@<9X@:n�@8��@7��@6@4�D@3��@2n�@0�`@0  @.�y@.{@-�@,j@+C�@)hs@'�@&��@%O�@#��@"~�@!��@ A�@�@$�@�/@(�@C�@�!@n�@G�@ �@�@�R@�-@��@�@ƨ@"�@��@��@�u@�@�@5?@�@(�@ƨ@
�@
�@	��@�9@  @��@l�@�@�R@@p�@�j@�@��@@�\@^5@��@x�@ �9@ Q�?�|�?���?�5??��-?�I�?��m?�"�?���?��^?�1'?��P?��T?��j?�n�?�Ĝ?�  ??���?�j?��?���?���?�7L?�u?��?�K�?�ff?��?�`B?䛦?��
?�33?�-?�hs?�Ĝ?��;?��?�v�?�{?�O�?�V?�I�?�C�?���?���?�X?���?��?�$�?ԛ�?�33?ѩ�?�|�?�V?�/?�1?���?ȴ9?�K�?š�?�Z?�o?�&�?� �?���?��h?��D?�?�=q?�7L?��u?��?�+?�E�?��?���?��F?�S�?���?�n�?�n�?�M�?��?�-?�-?��\?�o?�t�?��F?�Z?��?��?�ff?�+?��?��9?���?���?��?�X?�X?���?��^?���?��?�=q?�=q?�~�?���?��H?��H?�C�?�dZ?��?�ƨ?�ƨ?��m?�(�?�j?��D?��A݇+A݉7A݉7A݇+A݉7A݉7A݉7A݇+A݇+A݉7A݉7A݋DA݉7A݋DA݇+A݉7A݉7A݉7A݉7A݋DA݋DA݋DA݉7A݇+A݇+A݅A݇+A݉7A݋DA݉7A݉7A݉7A݉7A݉7A݋DA݋DA݋DA݋DA݋DA݉7A݁A݁A݅A݅A݃A�~�A�~�A�z�A�v�A�v�A�t�A�v�A�v�A�r�A�r�A�r�A�r�A�p�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              A݉7A݇+A݉7A݋DA݋DA݁A݅A�~�A�x�A�v�A�r�A�t�A�~�Aݏ\Aݡ�Aݺ^A��#A�A�$�A�&�A�$�A� �A��A��A�JA���A��yA�ƨAݬAݑhA�+A�ȴA�K�A�
=A�bA�dZAЏ\A�M�AƲ-A�bA�^5A��hA�;dA�5?A���A���A���A�1'A��+A�VA�t�A�  A�7LA�I�A��uA�%A�Q�A�-A��A�~�A���A���A�M�A�ffA��A��uA�M�A�^5A�
=A�33A�VA��/A�`BA��A�A��A��wA��/A�7Az~�Au|�Aqx�Ak�7Ad~�A^�/AZI�AX��AU��AR�HAO�7ALZAKK�AJM�AH�AEK�AD�AC�AAK�A>�yA=%A;G�A8��A6��A5ƨA5A45?A2ZA0��A.��A-A,�A-33A-�hA-�A,1'A*ffA(�/A'�#A'XA'��A'x�A&  A%��A%\)A$�/A$A�A#�A#|�A#`BA"�HA"^5A!�;A!dZA!/AAv�A$�A��A�A�#A�wA
=A/AE�AJAbNAffAM�AS�A�^A"�A�!AI�AM�AA
=An�A�Ar�AA�A  A��A�DAVA�AAK�A��AE�A�AffA�uAjA�A��AA�A1A��A��A�TA{A�+A�\A�AƨA�PA33A
�A
�RA
jA	�hA	`BA	?}A�RA��A�wAK�A"�A%A�A��A��AbNA��A�
A�A|�A`BA7LAVAz�A�7A��A�/A��Az�A=qA��AG�A ��A �uA E�@�\)@�M�@��@��u@���@���@���@���@�b@��@�
=@�O�@�  @�K�@�-@�O�@�@�r�@�(�@��;@�|�@@�J@�/@�A�@�F@�@�=q@��#@�x�@�V@蛦@�A�@�|�@㝲@ݺ^@�@��@�33@�r�@Χ�@̴9@��#@�"�@ũ�@Ý�@�5?@��@�@�9X@���@�9X@�$�@��@���@���@��j@�$�@�ƨ@���@��@�o@�%@�
=@���@���@���@�@���@�"�@�J@�?}@�ƨ@�=q@�&�@�(�@�V@��^@�r�@�"�@��^@�1'@�\)@�M�@��-@���@�@���@�Q�@~$�@|1@yX@x1'@vV@s�
@r�\@p�`@oK�@m@l(�@i�7@gK�@dZ@b=q@`�9@^��@]�@[��@Y��@W�;@U`B@S"�@Q&�@NV@L�D@J�!@Hr�@G
=@EV@C�
@C@A�7@@�`@@�u@?\)@>V@<9X@:n�@8��@7��@6@4�D@3��@2n�@0�`@0  @.�y@.{@-�@,j@+C�@)hs@'�@&��@%O�@#��@"~�@!��@ A�@�@$�@�/@(�@C�@�!@n�@G�@ �@�@�R@�-@��@�@ƨ@"�@��@��@�u@�@�@5?@�@(�@ƨ@
�@
�@	��@�9@  @��@l�@�@�R@@p�@�j@�@��@@�\@^5@��@x�@ �9@ Q�?�|�?���?�5??��-?�I�?��m?�"�?���?��^?�1'?��P?��T?��j?�n�?�Ĝ?�  ??���?�j?��?���?���?�7L?�u?��?�K�?�ff?��?�`B?䛦?��
?�33?�-?�hs?�Ĝ?��;?��?�v�?�{?�O�?�V?�I�?�C�?���?���?�X?���?��?�$�?ԛ�?�33?ѩ�?�|�?�V?�/?�1?���?ȴ9?�K�?š�?�Z?�o?�&�?� �?���?��h?��D?�?�=q?�7L?��u?��?�+?�E�?��?���?��F?�S�?���?�n�?�n�?�M�?��?�-?�-?��\?�o?�t�?��F?�Z?��?��?�ff?�+?��?��9?���?���?��?�X?�X?���?��^?���?��?�=q?�=q?�~�?���?��H?��H?�C�?�dZ?��?�ƨ?�ƨ?��m?�(�?�j?��D?��A݇+A݉7A݉7A݇+A݉7A݉7A݉7A݇+A݇+A݉7A݉7A݋DA݉7A݋DA݇+A݉7A݉7A݉7A݉7A݋DA݋DA݋DA݉7A݇+A݇+A݅A݇+A݉7A݋DA݉7A݉7A݉7A݉7A݉7A݋DA݋DA݋DA݋DA݋DA݉7A݁A݁A݅A݅A݃A�~�A�~�A�z�A�v�A�v�A�t�A�v�A�v�A�r�A�r�A�r�A�r�A�p�A�r�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	ȴB	�HB
bB
>wB
gmB
o�B
p�B
p�B
p�B
n�B
l�B
jB
hsB
`BB
]/B
W
B
H�B
<jB
0!B
VB	�B	��B	��B	�B
$�B
�%B
�-B
ȴB
��B49B5?B33B?}BP�BA�B8RB7LB!�B:^BJ�Bz�B�VB�7B�DB�\B�Bn�BS�B-B"�BoB
��B
�B
�#B
��B
�B
bNB
F�B
"�B
{B
1B	�B	�`B	��B	�B	[#B	L�B	)�B��B�BǮB��B�'B��B��B�VB�VB�VB��B��B�B�XBƨB��B�NB�B	B�B�B�B��B��B	oB	0!B	<jB	@�B	I�B	q�B	�DB	�=B	�B	r�B	m�B	q�B	z�B	�%B	�PB	��B	��B	��B	��B	�?B	�^B	�}B	��B	��B	��B	��B	��B	�#B	��B	��B	��B	�;B	�TB	�NB	�sB	�NB	�B	�
B	�TB	�yB	�B	�B	�yB	�ZB	�sB	�sB	�B	��B	��B	�B	��B
  B	��B
B
B	��B
  B
B
B
B
B
  B	��B	��B	��B
B
B
B
JB
�B
�B
�B
hB
�B
�B
%�B
'�B
&�B
'�B
(�B
(�B
+B
+B
(�B
'�B
+B
,B
)�B
)�B
)�B
)�B
)�B
(�B
)�B
)�B
(�B
'�B
&�B
'�B
&�B
&�B
&�B
&�B
%�B
�B
�B
�B
�B
!�B
"�B
"�B
 �B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
bB
\B
PB
JB

=B

=B

=B
	7B
1B
+B
%B
B
%B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B
  B
B
B
%B
%B
1B
	7B
DB
JB
PB
PB
PB
bB
\B
bB
uB
uB
{B
uB
uB
hB
oB
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
%�B
'�B
(�B
+B
,B
.B
/B
2-B
5?B
6FB
8RB
8RB
8RB
;dB
;dB
=qB
@�B
?}B
?}B
@�B
B�B
B�B
E�B
G�B
I�B
H�B
I�B
J�B
K�B
M�B
N�B
O�B
R�B
S�B
VB
XB
XB
YB
YB
[#B
\)B
]/B
]/B
]/B
]/B
^5B
_;B
aHB
cTB
dZB
e`B
dZB
gmB
gmB
hsB
jB
k�B
k�B
l�B
l�B
l�B
n�B
m�B
o�B
p�B
p�B
q�B
s�B
s�B
t�B
u�B
w�B
w�B
x�B
x�B
z�B
z�B
z�B
z�B
|�B
}�B
|�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�1B
�1B
�1B
�=B
�DB
�DB
�DB
�JB
�DB
�JB
�PB
�VB
�VB
�\B
�\B
�bB
�bB
�hB
�hB
�oB
�hB
�uB
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�-B
�'B
�-B
�3B
�9B
�3B
�9B
�?B
�FB
�FB
�RB
�RB
�XB
�^B
�dB
�jB
�jB
�qB
�wB
�wB
�}B
�}B
��B
��B
B
B
ÖB
ÖB
ĜB
ĜB
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ƨB
ƨB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ƨB
ŢB
ŢB
ŢB
ŢB
ƨB
ƨB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              B	�tB	�nB	�nB	�nB	�oB	�vB	�oB	�vB	�}B	��B	��B	��B	��B	�=B	ȔB	�)B
CB
>YB
gOB
o�B
p�B
p�B
p�B
n}B
lqB
jfB
hZB
`*B
]B
V�B
H�B
<TB
0B
AB	�B	˳B	��B	�B
$�B
�B
�B
ȡB
��B4'B5-B3"B?lBP�BAyB8BB7<B!�B:OBJ�Bz�B�HB�*B�7B�PB�Bn�BS�B-B"�BeB
��B
�{B
�B
�zB
�B
bFB
F�B
"�B
sB
*B	�B	�ZB	��B	�B	[B	L�B	)�B��B�BǨB�~B�"B��B��B�RB�RB�SB��B��B�B�VBƧB��B�NB�B	B�B�B�B��B��B	rB	0%B	<nB	@�B	I�B	q�B	�KB	�DB	�&B	r�B	m�B	q�B	z�B	�/B	�[B	��B	��B	��B	��B	�LB	�lB	��B	��B	��B	��B	�B	��B	�5B	�B	�B	��B	�OB	�iB	�cB	�B	�dB	�4B	�!B	�lB	�B	�B	��B	�B	�tB	�B	�B	��B	��B	��B	��B	��B
 B	�B
&B
-B	�B
 "B
(B
5B
<B
0B
 %B	�B	��B	��B
9B
9B
4B
rB
�B
�B
�B
�B
�B
�B
&B
(B
'B
(B
)%B
)%B
+2B
+2B
)'B
("B
+4B
,;B
*/B
*0B
*0B
*1B
*2B
),B
*3B
*3B
).B
((B
'"B
(*B
'#B
'$B
'$B
'%B
&B
�B
�B
�B
�B
"
B
#B
#B
!B
�B
!B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B

�B

�B
	�B
B
zB
tB
oB
uB
pB
jB
dB
eB
lB
`B
fB
gB
hB
bB
nB	�FB	�=B	�LB	�1B
 kB
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
B
B
9B
7B
:B
=B
FB
PB
;B
PB
TB
]B
yB
�B
�B
 �B
"�B
#�B
$�B
&�B
(�B
)�B
+�B
,�B
/B
0B
3-B
6BB
7LB
9[B
9^B
9aB
<vB
<yB
>�B
A�B
@�B
@�B
A�B
C�B
C�B
F�B
H�B
J�B
I�B
J�B
K�B
MB
OB
P!B
Q*B
T@B
UIB
WXB
YgB
YkB
ZuB
ZxB
\�B
]�B
^�B
^�B
^�B
^�B
_�B
`�B
b�B
d�B
e�B
f�B
e�B
h�B
h�B
jB
lB
mB
m B
n)B
n,B
n/B
p?B
o;B
qKB
rTB
rWB
s`B
unB
uqB
vzB
w�B
y�B
y�B
z�B
z�B
|�B
|�B
|�B
|�B
~�B
�B
~�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
� B
�/B
�2B
�;B
�=B
�@B
�OB
�YB
�[B
�^B
�gB
�dB
�mB
�uB
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
��B
��B
��B
��B
��B
� B
�B
�B
�&B
�2B
�=B
�CB
�IB
�\B
�aB
�gB
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
��B
��B
��B
�B
�B
�B
�B
�B
�*B
�2B
�=B
�=B
�HB
�PB
�cB
�zB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�0B
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
�B
�&B
�6B
�KB
�[B
�vB
ƅB
ǙB
ǪB
��B
��B
��B
��B
�B
�B
�'B
�8B
�GB
�[B
�kB
�{B
˄B
̙B
ˣB
̸B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B	�tB	�tB	�mB	�tB	�mB	�tB	�tB	�tB	�mB	�tB	�mB	�mB	�tB	�mB	�tB	�tB	�tB	�tB	�tB	�tB	�tB	�mB	�mB	�tB	�nB	�uB	�uB	�nB	�nB	�nB	�nB	�nB	�uB	�uB	�nB	�nB	�uB	�oB	�oB	�oB	�vB	�vB	�vB	�oB	�oB	�vB	�vB	�}B	�}B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808200705242021061413574020210614135740202107221611392021072216113920210722161139201808200705242021061413574020210614135740202107221611392021072216113920210722161139PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018082007052420180820070524  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082007052420180820070524QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018082007052420180820070524QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216142020210722161420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                