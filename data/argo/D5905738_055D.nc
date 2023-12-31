CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-01T05:00:50Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   `   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20190201050050  20210722160157  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               7   7DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؟��۝	@؟��۝	11  @؟����P@؟����P@5�Q���@5�Q����c�=[�!��c�=[�!�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@ff@@  @y��@�  @�  @�33A��A  A$��AA��Aa��A���A���A�ffA���A�  A�  A�  A�  B   B  B  B��B   B(ffB0ffB8ffB@  BH  BO��BX  B`  Bh  BpffBxffB�ffB�33B�  B�  B�ffB�  B�33B�ffB�  B�33B�ffB�  B�33B���B�ffB�33B���B�  BǙ�B�33BЙ�B�  B�  B�ffB�ffB�ffB�33B왚B�B�33B�33B���C �C�fC��C�fC�C
33C  C�3C�fC�fC�C33C33CL�C�C  C ffC"L�C$L�C&33C(33C*33C,33C.�C0  C1�fC3�fC5��C8  C:L�C<33C>33C@�CB�CC�fCE��CH�CJL�CL33CN�CP�CR�CT�CV�CX33CZ33C\33C^  C_��Ca�fCd33Cf  Cg��Ci�fCl  Cn33Cp  Cq��Ct  Cv�Cx33Cz�C{�3C}�fC�  C��C��C�&fC�&fC�&fC�&fC�  C�  C�&fC�&fC��C��C��C�  C�  C��3C��3C��C�&fC��C�  C��C��C�  C��C��C�  C��fC�  C��C�  C��fC��fC��C�&fC��C��C��C��C�  C��C��C��C�  C��fC��3C��3C�  C��C��C��C�&fC�  C��C��C��fC��3C��3C��3C��3C��3C��fC�ٚC��C�&fC�&fC��C��C��C�  C��3C�  C��C�  C��3C��fC�  C��C��3C�  C�  C��C��C�  C��C��C��C��3C��3C�  C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��3C��3C�  C��C��C��C��C��3C��C��C��3C�  C�  C��C�&fC�  C��C��C��D�fD� D33D� D
ٚD,�D� DٚD,�Dy�D� D33D��D�3D"&fD$��D&� D)33D+�fD-� D09�D2�fD5fD7` D9�fD<3D>Y�D@�fDB�fDE  DG9�DIffDK��DM��DO� DQٚDS��DV�DX  DZ33D\L�D^S3D`` Dbl�Ddl�DfffDhy�Dj�fDl� Dn�fDp� Dr��Dt�3Dv�3Dx�3D{�D|��D~��D�p D�|�D��fD���D���D��fD��fD�ɚD��fD��D��fD� D�&fD�@ D�Y�D�p D��fD���D��3D���D��3D�ɚD��fD���D�� D���D�fD��D�fD��D�)�D�33D�C3D�FfD�VfD�` D�ffD�c3D�\�D�\�D�\�D�` D�\�D�\�D�S3D�I�D�@ D�6fD�&fD� D���D���D���D��3D���D�s3D�VfD�33D�3D��fD��fD��3D��fD�y�D�VfD�<�D�#3D�	�D��D���Dų3DƖfDǃ3D�vfD�l�D�S3D�I�D�@ D�6fD�,�D�&fD�  D�3D�3D��3D�� D��fD�ɚD��3D׬�Dؙ�Dك3D�y�D�i�D�S3D�<�D�)�D��D�3D��D⹚D�3D�c3D�9�D��3D�� D��D�s3D�FfD���D��fD��3D�FfD��3D��D�i�D�6fD���D��fD�S3D�ٚD�S3E ^fE�fE� E�E@ E��E�3E	3E
I�E� E�fE�fEfE� E�3E�3EP Ea�E�3E�3EfE\�E�3EfE ,�E#3E&s3E)` E,�3E/��E2�E5�fE8�fE<�E<�fE=i�E=��E>{3E?H E?��E@��EA3EA�fEB\�EB� EC��ED,�ED��EE��EF EF� EG[3EG� EH�fEI!�EI�fEJi�EK33EK��ELC3EM3EM�fENK3EN�3EOvfEP!�EP� EQ>fEQ��ER�fES&fES�fETa�>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?��>���>���>���>���?   ?��?333?333?L��?fff?�  ?���?�  ?�33?�ff?�33@ff@   @333@@  @S33@fff@y��@�ff@�33@�  @���@�33@�  @���@ٙ�@�33@�  A   A  A33A33A��A!��A(  A.ffA8  A>ffAC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444444441414411141111141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?fff?�33@&ff@`  @���@�  @�  @�33A��A  A,��AI��Ai��A���A���A�ffA���A�  A�  A�  A�  B  B
  B  B��B"  B*ffB2ffB:ffBB  BJ  BQ��BZ  Bb  Bj  BrffBzffB�ffB�33B�  B�  B�ffB�  B�33B�ffB�  B�33B�ffB�  B�33B���B�ffB�33B���B�  Bș�B�33Bљ�B�  B�  B�ffB�ffB�ffB�33B홚B�B�33B�33B���C ��CffCL�CffC��C
�3C� C33CffCffC��C�3C�3C��C��C� C �fC"��C$��C&�3C(�3C*�3C,�3C.��C0� C2ffC4ffC6L�C8� C:��C<�3C>�3C@��CB��CDffCFL�CH��CJ��CL�3CN��CP��CR��CT��CV��CX�3CZ�3C\�3C^� C`L�CbffCd�3Cf� ChL�CjffCl� Cn�3Cp� CrL�Ct� Cv��Cx�3Cz��C|33C~ffC�@ C�Y�C�Y�C�ffC�ffC�ffC�ffC�@ C�@ C�ffC�ffC�Y�C�Y�C�L�C�@ C�@ C�33C�33C�L�C�ffC�L�C�@ C�L�C�Y�C�@ C�L�C�Y�C�@ C�&fC�@ C�L�C�@ C�&fC�&fC�L�C�ffC�Y�C�L�C�L�C�L�C�@ C�L�C�Y�C�Y�C�@ C�&fC�33C�33C�@ C�L�C�L�C�Y�C�ffC�@ C�L�C�L�C�&fC�33C�33C�33C�33C�33C�&fC��C�L�C�ffC�ffC�Y�C�Y�C�L�C�@ C�33C�@ C�Y�C�@ C�33C�&fC�@ C�L�C�33C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�Y�C�33C�33C�@ C�@ C�L�C�L�C�Y�C�@ C�L�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�33C�33C�@ C�L�C�L�C�Y�C�Y�C�33C�L�C�L�C�33C�@ C�@ C�Y�C�ffC�@ C�L�C�Y�C�L�D�fD  DS3D� D
��DL�D� D��DL�D��D  DS3D��D�3D"FfD$��D'  D)S3D+�fD.  D0Y�D2�fD5&fD7� D9�fD<33D>y�D@�fDB�fDE  DGY�DI�fDK��DM��DO� DQ��DT�DV,�DX@ DZS3D\l�D^s3D`� Db��Dd��Df�fDh��Dj�fDl� Dn�fDp� Dr��Dt�3Dv�3Dy3D{,�D|ٚD~��D�� D���D��fD���D���D��fD��fD�ٚD��fD���D�fD�  D�6fD�P D�i�D�� D��fD���D��3D���D��3D�ٚD��fD���D�  D�	�D�fD��D�&fD�,�D�9�D�C3D�S3D�VfD�ffD�p D�vfD�s3D�l�D�l�D�l�D�p D�l�D�l�D�c3D�Y�D�P D�FfD�6fD�  D��D���D�ɚD��3D���D��3D�ffD�C3D�#3D�fD��fD��3D��fD���D�ffD�L�D�33D��D���D���D��3DƦfDǓ3DȆfD�|�D�c3D�Y�D�P D�FfD�<�D�6fD�0 D�#3D�3D�3D�� D��fD�ٚD��3D׼�Dة�Dٓ3Dډ�D�y�D�c3D�L�D�9�D�,�D�3D���D�ɚD�3D�s3D�I�D�3D�� D��D�3D�VfD�	�D��fD�3D�VfD�3D���D�y�D�FfD���D��fD�c3D��D�c3E ffE�fE� E	�EH E��E�3E	3E
Q�E� E�fE�fEfE� E�3E�3EX Ei�E�3E�3EfEd�E�3EfE 4�E#3E&{3E)h E,�3E/��E2�E5�fE8�fE<�E<�fE=q�E=��E>�3E?P E?��E@��EA3EA�fEBd�EB� EC��ED4�EE�EE��EF EF� EGc3EG� EH�fEI)�EI�fEJq�EK;3EK��ELK3EM3EM�fENS3EN�3EO~fEP)�EP� EQFfEQ��ER�fES.fES�fETi�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�?L��G�O�G�O�?fff?�  ?���G�O�?���?�ff?�33?�  ?���G�O�?�33@33@��@&ff@@  @S33@`  @s33@�33@���@�ff@�33@�  @���@�33@�  @���@陚@�33A   A  A  A33A33A$��A)��A0  A6ffA@  AFffAK33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414441444444441414411141111141111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ �@ %@ V@ {@ �@ ""@ (�@ 0x@ 8�@ =q@ FQ@ R�@ `B@ m�@ {�@ ��@ ��@ ��@ �~@ �&@ ��@ �t@ �@ ��@�@@g@-@:�@G�@UU@bN@p�@~K@��@�H@��@�F@�>@�7@��@��@�,@�@�@""@0x@>�@K@Yn@hs@uk@�d@��@�@�M@�@�@Ӡ@�H@�L@��@�@�@'�@5?@A�@O0@[z@j@v�@��@�@�@�r@��@�W@�[@�@�@^@@[@)�@6�@F�@S�@a�@n�@|?@��@��@��@�~@�w@�@�@�@��@�@o@g@-@9X@FQ@V@e	@r@~�@��@�H@��@��@��@є@�;@�@��@%@�@""@.l@<�@K@Z@ff@r�@��@�@�a@�Y@�F@�J@Ӡ@�@�L@��@J@�@'�@33@@�@P�@^5@k.@x�@��@��@�m@�f@�@�@�@�`@�Y@ �@@O@)�@7�@DD@P�@_�@m�@z�@�+@��@��@��@��@�|@�#@��@��@	@	o@	 @	,`@	8�@	F�@	T�@	b�@	qS@	~�@	�P@	��@	�A@	��@	�>@	��@	�/@	��@	�~@
%@
�@
 �@
-�@
>@
M$@
Z�@
g�@
uk@
�d@
�\@
�U@
��@
��@
��@
��@
��@
��@
�E@	�@�@%�@3�@A�@N�@\�@j@x�@�p@�@�m@�@�k@�@�h@�@�@^@@�@)�@7L@D�@R�@^�@l�@z�@�7@��@�5@��@�w@�|@�#@�m@��@j@o@ �@,`@:�@I@V@�O@@P�@�\@ψ@@N�@��@�*@�@N�@��@�*@J@K�@�P@��@J@K�@��@�@V@O0@�\@��@�@M�@�7@ƨ@j@@,@{�@�F@�L@(�@bN@��@��@�@FQ@�@��@��@&�@]�@�u@�@@:@qS@�A@�;@�@S�@��@�W@�@-�@ff@��@�h@b@Ji@�@�@��@1'@k�@�4@�H@�@X�@��@�7@�@F�@~�@��@�@ +@ dZ@ �U@ �
@!�@!I@!�@!��@!�@"+@"c�@"��@"��@#@#G�@#�@#��@#��@$!s@$X@$�\@$�J@$��@%0x@%e	@%��@%�*@&]@&33@&e�@&��@&Ĝ@&�q@'(G@'Yn@'��@'�@'��@(B@(I@(x&@(��@(�@)1@)9X@)j@)��@)�o@)��@*-@*]�@*�@*��@*�~@+)�@+^5@+��@+�W@+��@,1'@,ff@,�H@,�|@- �@-33@-g�@-��@-��@.�@.5@@.g@.��@.��@/ �@/2�@/e	@/��@/�@/�9@0^5@0��@0�@1[@1{�@1��@1��@2:�@2g�@2Ĝ@2�L@3O0@3x�@3�O@3�Q@4Z�@4�|@4��@5�@5g@5�^@6A�@6�J@7Ji@7��@8R�@8�t@9c�@9��@:x�@; �@;�@<	�@<��@=�@=��@>1'@>��@?G�@?�k@@SI@@��@AM�@A܀@Bn�@B�Q@DWb@E�#@G�@HDD@I��@K@LR�@M�@N��@P8�@P��@P��@QJ@QE�@Q�@Qխ@R)�@R`�@R��@R�@S%�@Sy�@S��@TJ@TH]@T��@T�
@U@UG�@U��@U��@V$�@V\�@V��@V�4@W&�@W|?@W�9@X�@X7�@X�p@X�|@Y  @YF�@Y��@Y��@Z6@Z[z@Z��G�O�@ G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�@ G�O�G�O�@ �@ j@ G�O�@ �@ v@ %@ �@ �G�O�@ 	�@ J@ �@ V@ @ @ {@ �@ �@ �@ �@ g@ ""@ $.@ &;@ (�@ +�@ .l@ 0x@ 33@ 6�@ :@ ;d@ >�@ B�@ D�@ G�@ Ji@ N�@ Q=@ SIG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��
A��
A��#A��/A��TA��mA��`A��mA��HA��HA��`A��`A��TA��mA��yA��yA��`A��mA��TA��;A���A�A���AüjAð!Aç�AÝ�AÑhAÃA�\)A�=qA���A��A��A��
A��`A��A���A¬A�$�A��+A��jA��#A�S�A�JA��7A��A�p�A��\A�x�A��
A�\)A��yA���A�5?A���A�-A�G�A��A��\A���A���A�z�A�C�A�1'A��
A�{A�ffA�K�A�;dA�&�A��mA��^A�S�A��wA��FA�
=A���A��7A��PA�ffA�&�A��A��HA�r�A���A�M�A���A�1A��^A���A���A�ȴA���A��7A��A�M�A��PA��DA��/A��uA���A�S�A�9XA�M�A��HA���A��A��A��!A�33A~�A|ZA{;dAzbAyXAw�mArȴAohsAn�+Al�AkG�AfI�Ac\)Aa�TAap�A`��A^1A\ffA[K�AX�`AVA�AU7LATn�AR��ARv�ARZAR�AQ`BAPjAN��AN5?AM�7ALA�AL1AK�mAK�-AKXAJ~�AIp�AFE�ADbABE�A?�mA=�wA<�9A<ZA;�A;C�A:�+A6��A4��A2�!A0-A.bNA-�A+�A)�A(jA'p�A&jA%��A$�A#�#A"�uA!��A ~�A  A/A"�A�A(�A�7A�A�^A�AO�Ar�A��AoAƨA�A��A9XAO�A
�jA
��A
^5A
 �A	�A	t�A�A��A|�A��A�\A9XA{AO�AS�@��R@��`@��@�~�@�v�@�Ĝ@�@�I�@��y@��@�
=@���@�Ĝ@�1'@���@�dZ@�o@���@�@��@�P@�P@�ȴ@��@�%@�@�  @�"�@��#@��@�5?@�+@�E�@�-@���@�t�@� �@׮@�Q�@��@��@�X@��@�X@��/@�ff@���@��7@�1'@���@�r�@��y@�7L@�S�@�%@�"�@��D@���@�@��-@��@��h@�j@�"�@��R@��F@�$�@���@�@��7@��@���@���@�E�@���@���@�+@���@�X@�"�@��#@��`@�V@���@�hs@���@��;@�ff@�X@���@�ff@��#@�/@�&�@��@���@��/@���@�;d@��@�ȴ@�ff@�@�Z@+@}��@|�@{��@z=q@x��@wK�@t�@r~�@q��@n�R@l��@k�m@j�H@i%@fȴ@e@e`B@e�@b��@a��@`�9@^��@^v�@]�@\9X@[�@Z-@Y7L@W�@Vv�@Up�@T�@S�F@Q��@O�@Nff@M�h@MV@L�D@K�@H�`@HA�@F{@EV@D1@CS�@B�!@A��@@A�@>ȴ@<j@;33@:�H@:^5@9�7@8r�@7�@7��@6�R@5`B@4�D@3t�@2^5@1G�@17L@/\)@.�@.�+@-��@-/@,Z@+C�@)�#@(A�@'�@&v�@%`B@$�/@#�
@#��@#33@#"�@"~�@ ��@\)@V@@�-@z�@S�@=q@��@X@��@r�@b@�@�w@�P@;d@;d@��@z�@"�@-@��@1'@�;@��@�T@/@�D@
��@
-@	��@Ĝ@�w@;d@�h@��@�@M�@7L@ 1'?��?�C�?���?�
=?���?�-?��;?��?�V?�?�9?�?�?�Z?�33?�  ?޸R?ݑh?�O�?�~�?�Q�?��?�J?ϝ�?���?��m?��#?�7L?�+?�ȴ?�$�?��T?��?���?���?��/?��/?ě�?�z�?�Z?�Z?��?��
?öF?Õ�?�t�?�S�?�S�?�o?�o?�n�?�M�?�M�?�-?�J?��7?���?���?��7?��7?�hs?�hs?�hs?�%?�Ĝ?���?�A�?� �A���A���A�ȴA���A���A���A���A���A���A���A���A�ƨA�ĜA���A�ƨA���A�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��#A��
A��A��
A���A��A��/A��/A��#A��/A��HA��HA��TA��`A��mA��mA��mA��`A��`A��mA��mA��`A��TA��;A��#A��/A��;A��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   A���A���A��
A��
A��#A��/A��TA��mA��`A��mA��HA��HA��`A��`A��TA��mA��yA��yA��`A��mA��TA��;A���A�A���AüjAð!Aç�AÝ�AÑhAÃA�\)A�=qA���A��A��A��
A��`A��A���A¬A�$�A��+A��jA��#A�S�A�JA��7A��A�p�A��\A�x�A��
A�\)A��yA���A�5?A���A�-A�G�A��A��\A���A���A�z�A�C�A�1'A��
A�{A�ffA�K�A�;dA�&�A��mA��^A�S�A��wA��FA�
=A���A��7A��PA�ffA�&�A��A��HA�r�A���A�M�A���A�1A��^A���A���A�ȴA���A��7A��A�M�A��PA��DA��/A��uA���A�S�A�9XA�M�A��HA���A��A��A��!A�33A~�A|ZA{;dAzbAyXAw�mArȴAohsAn�+Al�AkG�AfI�Ac\)Aa�TAap�A`��A^1A\ffA[K�AX�`AVA�AU7LATn�AR��ARv�ARZAR�AQ`BAPjAN��AN5?AM�7ALA�AL1AK�mAK�-AKXAJ~�AIp�AFE�ADbABE�A?�mA=�wA<�9A<ZA;�A;C�A:�+A6��A4��A2�!A0-A.bNA-�A+�A)�A(jA'p�A&jA%��A$�A#�#A"�uA!��A ~�A  A/A"�A�A(�A�7A�A�^A�AO�Ar�A��AoAƨA�A��A9XAO�A
�jA
��A
^5A
 �A	�A	t�A�A��A|�A��A�\A9XA{AO�AS�@��R@��`@��@�~�@�v�@�Ĝ@�@�I�@��y@��@�
=@���@�Ĝ@�1'@���@�dZ@�o@���@�@��@�P@�P@�ȴ@��@�%@�@�  @�"�@��#@��@�5?@�+@�E�@�-@���@�t�@� �@׮@�Q�@��@��@�X@��@�X@��/@�ff@���@��7@�1'@���@�r�@��y@�7L@�S�@�%@�"�@��D@���@�@��-@��@��h@�j@�"�@��R@��F@�$�@���@�@��7@��@���@���@�E�@���@���@�+@���@�X@�"�@��#@��`@�V@���@�hs@���@��;@�ff@�X@���@�ff@��#@�/@�&�@��@���@��/@���@�;d@��@�ȴ@�ff@�@�Z@+@}��@|�@{��@z=q@x��@wK�@t�@r~�@q��@n�R@l��@k�m@j�H@i%@fȴ@e@e`B@e�@b��@a��@`�9@^��@^v�@]�@\9X@[�@Z-@Y7L@W�@Vv�@Up�@T�@S�F@Q��@O�@Nff@M�h@MV@L�D@K�@H�`@HA�@F{@EV@D1@CS�@B�!@A��@@A�@>ȴ@<j@;33@:�H@:^5@9�7@8r�@7�@7��@6�R@5`B@4�D@3t�@2^5@1G�@17L@/\)@.�@.�+@-��@-/@,Z@+C�@)�#@(A�@'�@&v�@%`B@$�/@#�
@#��@#33@#"�@"~�@ ��@\)@V@@�-@z�@S�@=q@��@X@��@r�@b@�@�w@�P@;d@;d@��@z�@"�@-@��@1'@�;@��@�T@/@�D@
��@
-@	��@Ĝ@�w@;d@�h@��@�@M�@7L@ 1'?��?�C�?���?�
=?���?�-?��;?��?�V?�?�9?�?�?�Z?�33?�  ?޸R?ݑh?�O�?�~�?�Q�?��?�J?ϝ�?���?��m?��#?�7L?�+?�ȴ?�$�?��T?��?���?���?��/?��/?ě�?�z�?�Z?�Z?��?��
?öF?Õ�?�t�?�S�?�S�?�o?�o?�n�?�M�?�M�?�-?�J?��7?���?���?��7?��7?�hs?�hs?�hs?�%?�Ĝ?���?�A�?� �A���A���A�ȴA���A���A���A���A���A���A���A���A�ƨA�ĜA���A�ƨA���A�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A��
A��#A��
A��A��
A���A��A��/A��/A��#A��/A��HA��HA��TA��`A��mA��mA��mA��`A��`A��mA��mA��`A��TA��;A��#A��/A��;A��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be`Be`Be`BdZBe`BdZBdZBdZBe`BdZBe`Be`BdZBcTBdZBdZBdZBdZBdZBdZBdZBcTBcTBcTBcTBcTBcTBcTBcTBdZBcTBbNBaHB_;BdZBjBn�B�B�=B�DB�bB��B��B��B	7B1B#�B?}BA�BC�BB�BL�BN�BO�BP�BO�BK�BS�BP�BR�BR�BQ�BP�BB�B<jB;dB9XB49B,B+B(�B(�B'�B&�B%�B#�B �B!�B�B�B�B �B�B�BhB��B�B�HB�
BɺBĜB��B�RB��B��Bw�BiyBcTBP�BM�BD�B@�B>wB33BuB
�B
ȴB
�-B
�JB
y�B
[#B
7LB
�B	��B	�sB	�/B	��B	��B	�jB	�hB	�%B	{�B	k�B	_;B	@�B	1'B	-B	'�B	 �B	bB	DB	  B�B�5B�B��B��B��B��B��BƨBB��B��B��BĜBĜBȴB��B��B��BB�9B�B�?B�B��B��B��B��B��B��Bw�Bs�Bo�BdZBdZB_;B_;B]/BaHB_;B]/BZB[#BXBVBXBT�BS�BP�BM�BJ�BH�BE�B9XB>wB:^B9XB8RB6FB33B0!B2-B1'B.B.B.B,B,B,B+B)�B,B/B,B+B&�B(�B.B49B,B$�B!�B%�B%�B&�B%�B$�B!�B"�B"�B#�B"�B#�B$�B$�B$�B$�B&�B2-B49B6FB7LB6FB8RBK�BQ�BXBZB^5B\)BZBYBYBXBZBhsBp�Br�Be`B`BBe`BhsBk�Bp�B��B��B�BƨB�B�TB�B��B��B	+B	uB	�B	+B	49B	8RB	:^B	=qB	L�B	Q�B	aHB	e`B	n�B	x�B	�B	�%B	�{B	��B	��B	��B	�B	�-B	�3B	�RB	�dB	B	��B	��B	�B	�BB	�HB	�fB	�B	�B	�B	�B	��B	��B	��B
B
  B
B
B
B
B
%B
%B
+B
+B
	7B
JB
VB
\B
bB
oB
uB
{B
�B
�B
�B
�B
�B
!�B
"�B
%�B
&�B
)�B
+B
+B
,B
.B
-B
.B
/B
/B
/B
1'B
2-B
33B
49B
5?B
6FB
7LB
8RB
9XB
;dB
=qB
>wB
>wB
>wB
?}B
@�B
B�B
B�B
C�B
D�B
F�B
F�B
G�B
G�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
N�B
N�B
O�B
Q�B
Q�B
R�B
T�B
T�B
VB
W
B
W
B
W
B
YB
YB
YB
XB
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
aHB
bNB
bNB
cTB
bNB
cTB
dZB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
gmB
hsB
gmB
jB
k�B
l�B
m�B
m�B
n�B
o�B
q�B
r�B
s�B
s�B
t�B
u�B
v�B
w�B
x�B
x�B
z�B
{�B
~�B
~�B
�B
�B
�B
�B
�%B
�+B
�1B
�=B
�DB
�JB
�PB
�VB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�-B
�'B
�-B
�-B
�-B
�3B
�-B
�-B
�3B
�3BdZBe`BiyBe`BcTBe`BffBe`Be`BdZBe`BcTBffBe`BdZBe`BffBe`BffBe`Be`Be`BdZBe`Be`Be`Be`Be`BdZBe`Be`BdZBdZBe`BdZBdZBdZBffBe`Be`BdZBe`BdZBdZBdZBe`BdZBe`BdZBe`Be`BdZBdZBe`Be`Be`Be`Be`Be`Be`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   BbNBbNBbNBaHBbNBaHBaHBaHBbNBaHBbNBbNBaHB`BBaHBaHBaHBaHBaHBaHBaHB`BB`BB`BB`BB`BB`BB`BB`BBaHB`BB_;B^5B\)BaHBgmBk�B�B�+B�1B�PB��BɺB��B%BB �B<jB>wB@�B?}BI�BK�BL�BM�BL�BH�BP�BM�BO�BO�BN�BM�B?}B9XB8RB6FB1'B(�B'�B%�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�BVB�B�mB�5B��BƨB��B�qB�?B��B��Bt�BffB`BBM�BJ�BA�B=qB;dB0!BbB
�B
ŢB
�B
�7B
v�B
XB
49B
�B	�B	�`B	�B	��B	ɺB	�XB	�VB	�B	x�B	hsB	\)B	=qB	.B	)�B	$�B	�B	PB	1B��B�B�#B��B��B��B��BɺBǮBÖB�}BǮBɺBǮB��B��BŢB��B��BȴB�}B�'B�B�-B�B��B��B��B��B��B�oBt�Bp�Bl�BaHBaHB\)B\)BZB^5B\)BZBW
BXBT�BR�BT�BQ�BP�BM�BJ�BG�BE�BB�B6FB;dB7LB6FB5?B33B0!B-B/B.B+B+B+B(�B(�B(�B'�B&�B(�B,B(�B'�B#�B%�B+B1'B(�B!�B�B"�B"�B#�B"�B!�B�B�B�B �B�B �B!�B!�B!�B!�B#�B/B1'B33B49B33B5?BH�BN�BT�BW
B[#BYBW
BVBVBT�BW
Be`Bm�Bo�BbNB]/BbNBe`BhsBn�B��B��B��BĜB�B�HB�B�B��B	B	hB	�B	(�B	2-B	6FB	8RB	;dB	J�B	O�B	_;B	cTB	l�B	v�B	� B	�B	�oB	��B	��B	��B	�B	�!B	�'B	�FB	�XB	��B	ɺB	��B	�B	�5B	�;B	�ZB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
+B

=B
JB
PB
VB
bB
hB
oB
{B
�B
�B
�B
�B
�B
 �B
#�B
$�B
'�B
(�B
(�B
)�B
,B
+B
,B
-B
-B
-B
/B
0!B
1'B
2-B
33B
49B
5?B
6FB
7LB
9XB
;dB
<jB
<jB
<jB
=qB
>wB
@�B
@�B
A�B
B�B
E�B
E�B
F�B
F�B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
M�B
M�B
N�B
P�B
P�B
Q�B
S�B
S�B
T�B
VB
VB
VB
XB
XB
XB
W
B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
`BB
aHB
aHB
bNB
aHB
bNB
cTB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
ffB
gmB
ffB
iyB
jB
k�B
l�B
l�B
m�B
n�B
p�B
q�B
r�B
r�B
s�B
t�B
u�B
v�B
w�B
w�B
y�B
z�B
}�B
}�B
� B
�B
�B
�B
�B
�%B
�+B
�7B
�=B
�JB
�PB
�VB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�B
�!B
�!B
�!B
�!B
�'B
�'B
�'B
�'B
�'B
�-B
�-B
�-B
�3B
�-B
�3B
�3B
�3B
�9B
�3B
�3B
�9B
�9BaHBbNBffBbNB`BBbNBcTBbNBbNBaHBbNB`BBcTBbNBaHBbNBcTBbNBcTBbNBbNBbNBaHBbNBbNBbNBbNBbNBaHBbNBbNBaHBaHBbNBaHBaHBaHBcTBbNBbNBaHBbNBaHBaHBaHBbNBaHBbNBaHBbNBbNBaHBaHBbNBbNBbNBbNBbNBbNBbNG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201902010500502021061413531320210614135313202106141747062021061417470620210614174706201902010500502021061413531320210614135313202106141747062021061417470620210614174706PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019020105005020190201050050  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019020105005020190201050050QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019020105005020190201050050QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015720210722160157IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                