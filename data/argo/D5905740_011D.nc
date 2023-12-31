CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:03:02Z creation      
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
_FillValue                 ,  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  a`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ŀ   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   |   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        `   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220302  20210722161419  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�mb�>DC@�mb�>DC11  @�mb�� `@�mb�� `@*K�5�Xy@*K�5�Xy�cMeәo��cMeәo�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?fff@ff@Fff@�  @�  @�33@�33A   A��A!��A@  A`  A�  A�  A�  A���A���A���A���A���B   B  BffBffB   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBxffB�33B�33B�  B�  B�  B�  B�33B�  B���B�  B�33B�ffB�ffB�  B���B�  B�  B�  BǙ�B�33B���B�33Bי�B�  B���B�33B癚B�  B�ffB�  B���B���C �C�fC  C33C�C
33C� C33C�fC  C�C��C  CffC33C�fC��C"  C$L�C&�C'�fC*  C,L�C.  C/�3C2  C433C6  C7�3C:  C<33C>  C?�fCB  CDL�CF�CG��CJ  CL33CM�fCP�CR33CS�fCV  CX33CY��C\  C^  C_�fCa�fCc�fCf33Ch33Cj�Cl  Cm�fCp�Cr  Cs��Cv�Cx33Cz�C{�fC~  C��C�33C��C��3C��C��C�33C�&fC�  C��C�&fC��C��fC��3C�  C��C��C�  C�ٚC�ٚC��3C��3C�  C�  C��C�&fC��C�ٚC��3C�  C��C��C�&fC��C��3C�  C��C��C��C��C�&fC�&fC�  C��C�33C��C��fC��3C��3C�  C�  C�  C��C��C��C�&fC�&fC��C��fC��3C�  C�  C�  C��C��C��C��3C�ٚC��3C�  C��C�&fC��C��3C�  C��C�&fC��C�  C�&fC��C�  C��C��C��3C��C��C��C�  C��fC��C�  C��3C��C��C��3C��C��C��fC��C�  C��3C��C�&fC��C�  C��C�  C��3C��C�&fC��C��C��C�  C�&fC�&fC��C�  C��fC��C��C�ٚC�&fC�L�C��C�� C��3D fD� D��D��D�fD� D��D��D�3Dl�D 9�D"��D%� D(9�D*��D-,�D/��D1� D4�D6FfD8� D:� D=  D?S3DA� DC�fDFL�DH��DK3DM�3DP�DR� DUY�DW�3DZ��D]S3D`  Db�3De� Dh��Dky�DnL�Dq&fDs�3Dv� Dy�fD|9�D&fD�  D�` D���D�fD�s3D�� D�&fD�y�D��3D�	�D�VfD�� D��D�9�D�y�D�� D��D�VfD���D�� D��D�ffD���D�3D�L�D�� D��fD��D�\�D�� D���D�<�D���D�� D�  D�p D��3D�fD�S3D���D��D�6fD�y�D��3D��D�S3D��3D�� D�0 D�l�D��3D�  D�I�DņfD���D��D�i�Dʳ3D���D�FfDΆfD�� D� D�VfDӖfD�� D��D�P Dؓ3D�� D�fD�Y�Dݜ�D��3D�0 D�s3D�fD���D�C3D�3D��3D�  D�@ D� D��D��3D�33D�vfD��D��fD�  D�S3D���D��fD��fD�,�D�I�D�|�D���D��E  E � E@ E�fEnfE�E�fE,�E� EQ�E� E|�E E� E4�E��E	Y�E	�fE
� E3E�fED�E�3Es3E��E��E E6fEk3E� EњE E<�Ei�E,�EX Ex E�fE��E!h E"��E#�3E$�fE&NfE'T�E(c3E)�E*�E,l�E-nfE.��E/�3E1p E2s3E3�3E4��E6k3E7d�E8� E9�E;\�E<�3E?��EBɚEE��EI�EK��EOL�ER��EUњEX��E[�3E^�Eb	�Ee#3EhNfEkp En�fEq��Et��Ew� E{�E~6fE��3E�#3E�͚E�H E���E��fE���E�� E�fE��fE�+3E�� E�T E��fE�h�E�3E��fE�#3E�� E�S3E��3E�d�E��fE���E� E�]�E�ŚE� E�Q�E���E�� E�@ E���E�� E�1�E�u�E�� E�3E��fE�ŚE��E�m�E���?333?333?333?333?L��?333?333?L��?L��?333?333?L��?L��?L��?fff?fff?L��?L��?fff?fff?�  ?fff?�  ?�  ?���?���?���?�33?�  ?�33?ٙ�?�ff@ff@��@   @   @333@Fff@Y��@fff@y��@�33@���@�ff@�  @���@���@�33@���@���@�ff@�33A   AffAffA33A��A!��A)��A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144144414414441414141411141111141111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?�33@&ff@fff@�  @�  @�33@�33A  A��A)��AH  Ah  A�  A�  A�  A���A���A���A���A���B  B
  BffBffB"  B*  B2  B:  BB  BJ  BR  BZ  Bb  BjffBrffBzffB�33B�33B�  B�  B�  B�  B�33B�  B���B�  B�33B�ffB�ffB�  B���B�  B�  B�  Bș�B�33B���B�33Bؙ�B�  BᙚB�33B虚B�  B�ffB�  B���B���C ��CffC� C�3C��C
�3C  C�3CffC� C��CL�C� C�fC�3CffC L�C"� C$��C&��C(ffC*� C,��C.� C033C2� C4�3C6� C833C:� C<�3C>� C@ffCB� CD��CF��CHL�CJ� CL�3CNffCP��CR�3CTffCV� CX�3CZL�C\� C^� C`ffCbffCdffCf�3Ch�3Cj��Cl� CnffCp��Cr� CtL�Cv��Cx�3Cz��C|ffC~� C�Y�C�s3C�L�C�33C�L�C�Y�C�s3C�ffC�@ C�L�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�@ C��C��C�33C�33C�@ C�@ C�Y�C�ffC�L�C��C�33C�@ C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�L�C�L�C�Y�C�ffC�ffC�@ C�L�C�s3C�L�C�&fC�33C�33C�@ C�@ C�@ C�L�C�Y�C�Y�C�ffC�ffC�L�C�&fC�33C�@ C�@ C�@ C�L�C�L�C�Y�C�33C��C�33C�@ C�L�C�ffC�L�C�33C�@ C�Y�C�ffC�L�C�@ C�ffC�L�C�@ C�Y�C�L�C�33C�Y�C�Y�C�L�C�@ C�&fC�L�C�@ C�33C�L�C�L�C�33C�L�C�L�C�&fC�Y�C�@ C�33C�L�C�ffC�L�C�@ C�Y�C�@ C�33C�L�C�ffC�Y�C�L�C�L�C�@ C�ffC�ffC�L�C�@ C�&fC�L�C�L�C��C�ffC���C�L�C�  C�33D &fD  DٚDٚD�fD� D��D��D�3D��D Y�D#�D%� D(Y�D*ٚD-L�D/��D2  D49�D6ffD8� D:� D=  D?s3DA� DDfDFl�DH��DK33DM�3DP9�DR� DUy�DX3DZ��D]s3D`@ Dc3De� Dh��Dk��Dnl�DqFfDt3Dw  Dy�fD|Y�DFfD� D�p D���D�&fD��3D�� D�6fD���D��3D��D�ffD�� D���D�I�D���D�� D��D�ffD���D�� D�,�D�vfD�ɚD�3D�\�D�� D��fD�)�D�l�D�� D���D�L�D���D�� D�0 D�� D��3D�fD�c3D���D���D�FfD���D��3D��D�c3D��3D�� D�@ D�|�D��3D� D�Y�DŖfD���D�)�D�y�D��3D��D�VfDΖfD�� D�  D�ffDӦfD�� D��D�` Dأ3D�� D�&fD�i�Dݬ�D��3D�@ D�3D��fD��D�S3D�3D��3D� D�P D� D���D�3D�C3D��fD��D��fD�0 D�c3D���D��fD�fD�<�D�Y�D���D�ɚD���E  E � EH E�fEvfE	�E�fE4�E� EY�E� E��E E� E<�E��E	a�E	�fE
� E#3E�fEL�E�3E{3E��E��E E>fEs3E� EٚE ED�Eq�E4�E` E� E�fEɚE!p E"��E#�3E$�fE&VfE'\�E(k3E)�E*�E,t�E-vfE.��E/�3E1x E2{3E3�3E4��E6s3E7l�E8� E9�E;d�E<�3E?��EBњEE��EI!�EL�EOT�ER��EUٚEX��E[�3E^�Eb�Ee+3EhVfEkx En�fEq��Eu�Ew� E{!�E~>fE��3E�'3E�њE�L E���E��fE���E�� E�
fE��fE�/3E�� E�X E��fE�l�E�3E��fE�'3E�� E�W3E��3E�h�E��fE���E� E�a�E�ɚE� E�U�E���E�  E�D E���E�� E�5�E�y�E�� E�#3E��fE�ɚE��E�q�E���G�O�G�O�G�O�?���G�O�G�O�?���G�O�G�O�G�O�?���G�O�G�O�?�ffG�O�G�O�G�O�?�ffG�O�?�33G�O�?�33G�O�?�  G�O�?���?ٙ�?�33G�O�?�33@��@33@&ff@,��G�O�@@  @S33@fff@y��@�33@���@�33@���@�ff@�  @���@ə�@�33@���@���@�ffA��A  AffAffA33A!��A)��A1��A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144144414414441414141411141111141111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ %@ V@ *@ O@ ""@ )�@ 0x@ 6�@ >@ D�@ Q�@ _�@ m:@ z�@ ��@ ��@ ��@ �-@ ��@ �|@ �t@ �@ �q@@@�@,`@:@G�@UU@b�@p�@~K@��@�H@��@��@�>@�7@��@�@�,@�@{@!s@/�@>@Lu@Z@ff@s_@��@�\@�@�M@�@ȴ@�O@��@��@��@
�@�@%�@4�@@�@M$@[z@j@v�@�@�#@�@�r@�&@��@�[@�@�@��@�@
@*S@5�@B�@Q�@a�@m�@z3@��@�<@��@�r@�&@�*@�t@�@��@�@@
@,`@<@H]@S�@b�@r@}�@��@��@��@��@��@��@��@�@�~@%@�@#�@1'@>@K@X@g@t@�W@�@�a@�Y@��@��@��@�@�@��@
�@B@(G@5?@@�@O0@^5@j@v@�p@��@�@�r@��@�W@��@�@�@  @�@�@+@7L@B8@Q=@_�@m�@|?@��@��@�(@�~@��@�|@�#@�y@��@	v@	@	g@	/@	:�@	FQ@	T�@	bN@	p�@	~K@	��@	�H@	��@	�F@	Ĝ@	�C@	ލ@	�(@	�~@
�@
{@
""@
0x@
>@
Lu@
X@
dZ@
s_@
��@
�@
�@
�Y@
��@
��@
��@
�T@
�@
��@J@�@%�@4�@A�@M�@]�@k.@x&@�@�h@�@�@�@�@׹@�@�@ �@J@�@(�@5�@D�@S�@`B@m:@|?@��@��@��@��@��@�|@�#@�@��@v@�@�@+@:�@H]@SI@e	@t�@~�@��@��@��@G�@��@�m@7L@��@��@+�@|�@��@6@bN@��@�@5�@x�@��@�,@5�@qS@�@�@(�@hs@�A@�`@&�@g�@�M@�@2�@z�@��@�@Q=@��@�@5?@��@ψ@
@k.@�@v@UU@��@�m@7L@�@�7@�@dZ@��@�,@B8@��@��@�@]�@��@�(@1�@v@��@ �@ I@ ��@ Ӡ@!6@!]�@!��@!�4@"2�@"ww@"�@#@#F�@#��@#��@$�@$a�@$�A@$��@%6�@%{�@%��@&
�@&S�@&�<@&�;@'$/@'j@'��@'�q@(:�@(��@(�c@)�@)R�@)��@)��@*#�@*i!@*�!@*��@+>@+�p@+��@,@,UU@,��@,�;@-#�@-ff@-��@-��@.3�@.ww@.�@/@/F�@/��@/Ӡ@0�@0]�@0�(@0��@1-@1qS@1��@1�,@2=q@2�@2�>@3�@3Lu@3��@3є@4{@4V@4��@4��@5�@5^�@5��@5�/@6 �@6a�@6��@6�@7%�@7e�@7��@7�`@8$�@8e	@8��@8��@9""@9a�@9�m@9��@:[@:Z�@:�I@:��@;�@;Z@;��@;�@<B@<Z@<܀@=^5@=�H@>bN@>�@?i�@?�4@@p�@@�e@At�@B5@@B��@C/�@C�f@D)�@D��@EWb@Eє@FF�@F�@Ge	@G�h@H~�@H�4@I�i@I�Q@J�z@K@K��@L#�@L�W@M1�@M��@N?}@N�@OO1@O��@P��@Qխ@S�@T\�@U�|@W�@Xr�@Yխ@[:�@\hs@]��@_�@`qS@a��@c	@dt@e�J@gV@h�+@i@k"�@lv�@m��@o�@px�@q��@s�@t|�@u�@wO@xWa@y� @{%@|_�@}�R@�@�,�@��;@��C@�1'@��H@��P@�6F@�܀@���@�9@���@���@�'�@�E�@�c�@���@��`@��c@��@��@�0x@�M|@�y,@���@��,@���@���@�$�@�?�G�O�G�O�G�O�@ �G�O�G�O�@ �G�O�G�O�G�O�@ �G�O�G�O�@ vG�O�G�O�G�O�@ vG�O�@ %G�O�@ %G�O�@ �G�O�@ �@ 1@ 	�G�O�@ 	�@ �@ J@ V@ G�O�@ @ @ *@ 6@ �@ �@ �@ 
@  @ ""@ $�@ '�@ )�@ +�@ /@ 1'@ 3�@ 6�@ 9X@ <�@ >�@ A�@ D�@ H]@ K�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AؑhAؗ�Aؙ�A؝�A؝�A؝�A؛�A؝�Aؙ�Aؕ�Aؕ�A؝�A؟�A؟�Aء�Aذ!AجAذ!AؾwAؼjAؾwA���Aذ!Aذ!Aء�A�ffA�9XA��A� �A� �A�/A�=qA�?}A�?}A��A�A�l�A��A���A�oA�jA�VA�VAÁA��A��A�E�A��DA�G�A���A�XA�
=A�|�A�M�A��#A��9A���A�C�A�+A���A��A�oA���A��A�%A�p�A�%A��A��A��
A�K�A��A��AXAxE�Ar�Ao"�Ai��Aa��AaVA^�AY�wAU�ASG�AO33ALE�AI�FAH$�AG`BAE"�AA�#A?�A=��A<�DA:��A9�FA8��A8�`A8�DA7��A6�A6$�A6JA5�mA5C�A3��A0�yA0��A2(�A2��A2��A2M�A2�A1��A/��A-�TA-�7A.�A-�;A,5?A,1A+�A+�TA+�#A+�;A+l�A*��A)��A)�A)+A(�`A(�\A(  A$��A#A#O�A"��A!�;A#K�A$(�A%�A%�A%��A%�A%�wA$��A#��A#O�A#&�A"�`A"��A"(�A!�
A!VA {A��A�A��AĜA�!AM�A�;A7LA�RA�A�-Al�AĜA��A��A�+A��A�yAbNAA��AC�A
=A�A~�A�A��A��AhsA;dA7LA�A�+A9XA�A��A(�A�PA�AoAVAQ�A�wA/A~�AVA(�AA�-A\)AAffAM�A$�A�#A�A&�AVA
�A
VA
  A	\)A	�A	%A��A��Ap�A�AffA�A��A�-Ax�AG�A�A�uA��AA��Al�A\)A33A�A��A�PA�A �A ^5A -@���@���@�$�@�G�@��@�%@��@���@��@�9X@��
@�=q@�Ĝ@�b@�|�@�;d@���@��@�t�@��@�7@���@�\)@�bN@��@Ӆ@�Ĝ@���@�$�@�  @�X@��@�t�@�j@�9X@�9X@��P@��h@�b@��!@��/@�  @�J@���@��@�  @�~�@��@�dZ@���@��@��@��#@�Q�@�33@�-@�&�@��@�{@���@�dZ@�V@�?}@���@���@�`B@��
@�33@�~�@�hs@���@�n�@���@~ff@|��@{�@z��@x��@w�@uO�@r��@p�@mp�@kS�@i7L@g\)@d�/@c�@a7L@` �@_\)@]�@\(�@Z-@X�@W��@U�T@T�D@Q�#@Q&�@N�y@MO�@L�@J�H@Ix�@H1'@F@Dz�@C33@BJ@A7L@?�w@>�R@=/@;�
@9�@8�@7�@6E�@4�j@4Z@3t�@1hs@0  @.E�@,�@*-@)X@(bN@'+@&5?@$�/@$1@#t�@!��@ ��@+@�+@��@z�@S�@M�@��@�`@1'@;d@ff@V@�@dZ@n�@X@Ĝ@��@K�@��@@�h@�/@z�@ƨ@
�@
^5@
�@	�@��@bN@�w@+@��@$�@��@?}@z�@(�@��@S�@�!@��@��@&�@ A�?��;?���?�{?�V?��?�ƨ?�^5?��9?�ȴ?���?���?��?�-?�Ĝ?�|�?�R?�h?�(�?�?�C�?�=q?��?��?�+?�+?�?�?}?��/?��
?�o?�n�?��?�bN?�|�?�{?ݲ-?�V?�j?�dZ?���?ٙ�?ش9?�b?�+?�?}?�S�?�-?�Ĝ?θR?��?̋D?��H?ɺ^?�1'?�ȴ?���?��
?�J?���?���?���?��?��m?�dZ?�^5?�X?���?���?��y?�E�?���?���?���?���?�-?�hs?�G�?�G�?�G�?�G�?�hs?���?�n�?���?�t�?��?��/?�`B?�$�?�ȴ?��y?�+?�+?�K�?�l�?��P?��?���?��?�1'?�Q�?�r�?��9?��9?���?��?�X?�x�A؍PA؋DA؍PA؉7A؇+A؏\A؏\A؏\AؑhAؑhA؍PA؍PAؓuAؓuAؗ�A؛�Aؗ�Aؙ�AؑhA؏\AؓuAؓuAؓuAؓuAؕ�Aؕ�Aؕ�Aؗ�Aؗ�Aؗ�Aؗ�Aؕ�Aؕ�Aؗ�Aؗ�Aؙ�Aؗ�Aؗ�A؛�A؝�A؝�A؝�A؝�A؝�A؛�A؝�A؟�A؝�A؛�Aؙ�A؛�A؝�A؟�A؛�Aؙ�Aؗ�Aؕ�Aؕ�Aؕ�Aؗ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          AؑhAؗ�Aؙ�A؝�A؝�A؝�A؛�A؝�Aؙ�Aؕ�Aؕ�A؝�A؟�A؟�Aء�Aذ!AجAذ!AؾwAؼjAؾwA���Aذ!Aذ!Aء�A�ffA�9XA��A� �A� �A�/A�=qA�?}A�?}A��A�A�l�A��A���A�oA�jA�VA�VAÁA��A��A�E�A��DA�G�A���A�XA�
=A�|�A�M�A��#A��9A���A�C�A�+A���A��A�oA���A��A�%A�p�A�%A��A��A��
A�K�A��A��AXAxE�Ar�Ao"�Ai��Aa��AaVA^�AY�wAU�ASG�AO33ALE�AI�FAH$�AG`BAE"�AA�#A?�A=��A<�DA:��A9�FA8��A8�`A8�DA7��A6�A6$�A6JA5�mA5C�A3��A0�yA0��A2(�A2��A2��A2M�A2�A1��A/��A-�TA-�7A.�A-�;A,5?A,1A+�A+�TA+�#A+�;A+l�A*��A)��A)�A)+A(�`A(�\A(  A$��A#A#O�A"��A!�;A#K�A$(�A%�A%�A%��A%�A%�wA$��A#��A#O�A#&�A"�`A"��A"(�A!�
A!VA {A��A�A��AĜA�!AM�A�;A7LA�RA�A�-Al�AĜA��A��A�+A��A�yAbNAA��AC�A
=A�A~�A�A��A��AhsA;dA7LA�A�+A9XA�A��A(�A�PA�AoAVAQ�A�wA/A~�AVA(�AA�-A\)AAffAM�A$�A�#A�A&�AVA
�A
VA
  A	\)A	�A	%A��A��Ap�A�AffA�A��A�-Ax�AG�A�A�uA��AA��Al�A\)A33A�A��A�PA�A �A ^5A -@���@���@�$�@�G�@��@�%@��@���@��@�9X@��
@�=q@�Ĝ@�b@�|�@�;d@���@��@�t�@��@�7@���@�\)@�bN@��@Ӆ@�Ĝ@���@�$�@�  @�X@��@�t�@�j@�9X@�9X@��P@��h@�b@��!@��/@�  @�J@���@��@�  @�~�@��@�dZ@���@��@��@��#@�Q�@�33@�-@�&�@��@�{@���@�dZ@�V@�?}@���@���@�`B@��
@�33@�~�@�hs@���@�n�@���@~ff@|��@{�@z��@x��@w�@uO�@r��@p�@mp�@kS�@i7L@g\)@d�/@c�@a7L@` �@_\)@]�@\(�@Z-@X�@W��@U�T@T�D@Q�#@Q&�@N�y@MO�@L�@J�H@Ix�@H1'@F@Dz�@C33@BJ@A7L@?�w@>�R@=/@;�
@9�@8�@7�@6E�@4�j@4Z@3t�@1hs@0  @.E�@,�@*-@)X@(bN@'+@&5?@$�/@$1@#t�@!��@ ��@+@�+@��@z�@S�@M�@��@�`@1'@;d@ff@V@�@dZ@n�@X@Ĝ@��@K�@��@@�h@�/@z�@ƨ@
�@
^5@
�@	�@��@bN@�w@+@��@$�@��@?}@z�@(�@��@S�@�!@��@��@&�@ A�?��;?���?�{?�V?��?�ƨ?�^5?��9?�ȴ?���?���?��?�-?�Ĝ?�|�?�R?�h?�(�?�?�C�?�=q?��?��?�+?�+?�?�?}?��/?��
?�o?�n�?��?�bN?�|�?�{?ݲ-?�V?�j?�dZ?���?ٙ�?ش9?�b?�+?�?}?�S�?�-?�Ĝ?θR?��?̋D?��H?ɺ^?�1'?�ȴ?���?��
?�J?���?���?���?��?��m?�dZ?�^5?�X?���?���?��y?�E�?���?���?���?���?�-?�hs?�G�?�G�?�G�?�G�?�hs?���?�n�?���?�t�?��?��/?�`B?�$�?�ȴ?��y?�+?�+?�K�?�l�?��P?��?���?��?�1'?�Q�?�r�?��9?��9?���?��?�X?�x�A؍PA؋DA؍PA؉7A؇+A؏\A؏\A؏\AؑhAؑhA؍PA؍PAؓuAؓuAؗ�A؛�Aؗ�Aؙ�AؑhA؏\AؓuAؓuAؓuAؓuAؕ�Aؕ�Aؕ�Aؗ�Aؗ�Aؗ�Aؗ�Aؕ�Aؕ�Aؗ�Aؗ�Aؙ�Aؗ�Aؗ�A؛�A؝�A؝�A؝�A؝�A؝�A؛�A؝�A؟�A؝�A؛�Aؙ�A؛�A؝�A؟�A؛�Aؙ�Aؗ�Aؕ�Aؕ�Aؕ�Aؗ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�mB�mB�mB�yB�sB�sB�mB�sB�`B�`B�ZB�fB�fB�mB�sB�B�B�B��B��B��B	  B	�B	�B	�B	1'B	49B	6FB	7LB	9XB	?}B	C�B	C�B	E�B	B�B	G�B	M�B	L�B	I�B	8RB	(�B	P�B	�\B	�dB	�yB
� B
��B
�fB  B49B�B
��B
�
B
��B
��B
��B
�qB
ƨB  B49B'�BJB
��B
�B
�NB
�BB
�dB
t�B
P�B

=B	�/B	�qB	�bB	VB	0!B		7B	\B�B�B��B�^B��B��B�FB�dB�wB�wBBÖB�FB�LB�LB�HB�fB��B	JB	bB	5?B	E�B	F�B	N�B	N�B	[#B	jB	ffB	S�B	dZB	r�B	��B	�-B	�?B	�?B	�FB	�?B	��B	��B	�B	�jB	�jB	�?B	�jB	ÖB	ƨB	ȴB	��B	�B	�HB	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B
�B
&�B
?}B
N�B
P�B
Q�B
P�B
P�B
P�B
Q�B
Q�B
O�B
O�B
N�B
N�B
L�B
L�B
M�B
K�B
M�B
M�B
M�B
O�B
O�B
L�B
P�B
P�B
XB
VB
VB
YB
^5B
`BB
\)B
[#B
XB
T�B
VB
S�B
Q�B
XB
VB
VB
YB
XB
W
B
ZB
[#B
[#B
[#B
ZB
S�B
O�B
L�B
J�B
M�B
Q�B
T�B
P�B
O�B
L�B
J�B
J�B
L�B
L�B
K�B
L�B
J�B
H�B
K�B
J�B
J�B
G�B
G�B
H�B
H�B
G�B
E�B
B�B
B�B
B�B
?}B
<jB
@�B
<jB
=qB
;dB
=qB
<jB
<jB
;dB
9XB
7LB
6FB
6FB
5?B
49B
49B
49B
33B
49B
0!B
1'B
33B
5?B
5?B
49B
49B
2-B
33B
33B
33B
33B
49B
5?B
49B
33B
1'B
.B
-B
,B
/B
/B
&�B
!�B
�B
�B
�B
�B
�B
�B
�B
hB
oB
�B
{B
bB
bB
�B
�B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
"�B
"�B
#�B
#�B
#�B
%�B
&�B
'�B
(�B
,B
-B
.B
/B
0!B
2-B
1'B
33B
49B
7LB
8RB
:^B
<jB
<jB
>wB
>wB
?}B
@�B
A�B
C�B
D�B
E�B
F�B
H�B
I�B
K�B
K�B
L�B
M�B
N�B
N�B
P�B
R�B
S�B
S�B
T�B
VB
W
B
XB
YB
ZB
[#B
[#B
\)B
]/B
^5B
_;B
_;B
`BB
`BB
aHB
bNB
cTB
cTB
e`B
ffB
gmB
ffB
gmB
hsB
iyB
jB
l�B
m�B
o�B
p�B
q�B
r�B
r�B
q�B
t�B
t�B
u�B
v�B
v�B
w�B
x�B
x�B
z�B
{�B
|�B
|�B
}�B
}�B
� B
� B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�1B
�1B
�7B
�=B
�DB
�JB
�DB
�JB
�PB
�JB
�JB
�PB
�VB
�\B
�VB
�VB
�bB
�bB
�hB
�hB
�hB
�oB
�oB
�oB
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
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�-B
�'B
�-B
�9B
�9B
�?B
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�dB
�dB
�jB
�qB
�wB
�}B
�}B
��B
��B
B
B
B
ĜB
ÖB
ÖB
ĜB
ŢB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ƨB
ĜB
ŢB
ŢB
ŢB
ƨB
ŢB
ƨB
ŢB
ŢB
ŢB
ŢB
ŢB
ƨB
ŢB
ŢB
ŢB
ƨB�fB�fB�fB�fB�fB�fB�sB�mB�`B�fB�mB�mB�fB�fB�fB�mB�sB�yB�mB�mB�mB�mB�sB�mB�mB�mB�mB�mB�mB�mB�mB�fB�mB�fB�mB�fB�fB�mB�sB�sB�yB�yB�sB�mB�sB�yB�sB�sB�mB�fB�sB�yB�sB�fB�fB�ZB�`B�`B�`B�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B�GB�GB�HB�TB�NB�NB�IB�OB�<B�=B�7B�DB�DB�LB�SB��B�yB��B��B��B��B��B	�B	�B	�B	1B	4 B	6-B	74B	9@B	?fB	C�B	C�B	E�B	BzB	G�B	M�B	L�B	I�B	8@B	(�B	P�B	�JB	�RB	�hB
�B
��B
�VB
��B4*B�B
��B
��B
��B
��B
�{B
�dB
ƛB
��B4-B'�B?B
��B
�B
�DB
�8B
�ZB
t�B
P�B

3B	�%B	�hB	�YB	U�B	0B		.B	SB�B�B��B�VB��B��B�?B�]B�pB�qBBÑB�AB�GB�HB�DB�cB��B	GB	`B	5>B	E�B	F�B	N�B	N�B	[$B	j�B	fhB	S�B	d]B	r�B	��B	�2B	�EB	�EB	�MB	�FB	�B	��B	�B	�tB	�tB	�JB	�uB	âB	ƴB	��B	�B	�+B	�WB	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B
�B
&�B
?�B
N�B
P�B
RB
P�B
P�B
P�B
RB
RB
O�B
O�B
N�B
N�B
L�B
L�B
M�B
K�B
M�B
M�B
M�B
PB
PB
L�B
Q	B
Q	B
X5B
V)B
V*B
Y=B
^\B
`jB
\QB
[LB
X9B
U(B
V.B
T#B
RB
X<B
V0B
V1B
YEB
X>B
W9B
ZLB
[SB
[TB
[TB
ZOB
T*B
PB
M B
J�B
NB
R!B
U4B
QB
PB
MB
J�B
J�B
MB
MB
LB
MB
J�B
H�B
LB
J�B
J�B
G�B
G�B
H�B
H�B
G�B
E�B
B�B
B�B
B�B
?�B
<�B
@�B
<�B
=�B
;�B
=�B
<�B
<�B
;�B
9�B
7�B
6�B
6�B
5�B
4�B
4�B
4�B
3�B
4�B
0oB
1vB
3�B
5�B
5�B
4�B
4�B
2B
3�B
3�B
3�B
3�B
4�B
5�B
4�B
3�B
1~B
.lB
-fB
,aB
/tB
/uB
'JB
"/B
B

B
B
B
B
�B
B
�B
�B
B
B
�B
�B
B
B
B
B
!B
#B
2B
<B
>B
AB
=B
GB
IB
RB
aB
dB
mB
!�B
 B
!�B
#�B
#�B
$�B
$�B
$�B
&�B
'�B
(�B
)�B
,�B
-�B
/B
0B
1B
3#B
2 B
40B
59B
8OB
9XB
;gB
=vB
=yB
?�B
?�B
@�B
A�B
B�B
D�B
E�B
F�B
G�B
I�B
J�B
L�B
L�B
NB
OB
PB
PB
R+B
T;B
UDB
UGB
VPB
WYB
XbB
YlB
ZvB
[B
\�B
\�B
]�B
^�B
_�B
`�B
`�B
a�B
a�B
b�B
c�B
d�B
d�B
f�B
g�B
i B
g�B
iB
jB
kB
l!B
n0B
o9B
qIB
rRB
s[B
tdB
tgB
sdB
vyB
v|B
w�B
x�B
x�B
y�B
z�B
z�B
|�B
}�B
~�B
~�B
�B
�B
��B
��B
��B
��B
�B
�	B
�B
�B
�%B
�(B
�0B
�3B
�<B
�?B
�HB
�QB
�[B
�dB
�aB
�jB
�rB
�oB
�rB
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
�B
�B
�#B
�)B
�5B
�AB
�GB
�XB
�hB
�nB
�yB
�yB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�"B
�0B
�4B
�HB
�FB
�NB
�UB
�pB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�3B
�BB
�XB
�gB
�|B
��B
��B
��B
��B
��B
��B
��B
�B
�+B
�@B
�PB
�fB
�zB
ǐB
ǞB
ǭB
��B
��B
��B
��B
�B
�B
�'B
�;B
�KB
�[B
�jB
�yB
̎B
˘B
˧B
̰B
ʨB
˱B
˳B
˶B
��B
˼B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B�@B�@B�@B�@B�@B�@B�MB�GB�:B�@B�GB�GB�@B�@B�@B�GB�MB�SB�GB�GB�GB�GB�MB�GB�GB�GB�GB�GB�GB�GB�GB�@B�GB�@B�GB�@B�AB�HB�NB�NB�TB�TB�NB�HB�NB�TB�NB�NB�IB�BB�OB�UB�OB�BB�BB�6B�=B�=B�=B�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242203022021061413573120210614135731202107221611312021072216113120210722161131201807242203022021061413573120210614135731202107221611312021072216113120210722161131PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422030220180724220302  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030220180724220302QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422030220180724220302QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141920210722161419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                