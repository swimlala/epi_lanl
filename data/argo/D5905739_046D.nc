CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-16T18:02:32Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _D   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ̈́   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   <   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    `   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181116180232  20210617131508  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               .   .DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؎�3�g8@؎�3�g811  @؎�-��@؎�-��@6��Ew�U@6��Ew�U�c��>l�c��>l11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
AC  AA  AA  >���?L��@ff@@  @�  @�33@�  @�  A   AffA!��A@  Aa��A�  A�33A�  A���A�  A�33A���A�ffB ��B  B��B33B ��B(ffB0  B7��B?��BH  BPffBW��B`  Bh  Bp  Bx  B�33B�33B�  B�33B�33B�ffB�33B���B���B�  B�  B�  B�  B�  B�33B�ffB�  B���BǙ�B�33B�ffB���B���B���Bߙ�B�33B�33B�33B�33B�ffB�33B�33C 33C�C�fC  C33C
�C  C�CL�C  C��C  C33C  C�fC�C   C!�fC$33C&33C(�C*  C+�fC.L�C033C2�C4�C6  C7�fC9��C<�C>  C?�fCA�fCC�fCF33CH�CI�fCLL�CN33CP33CR�CT  CVL�CXL�CZ33C\33C^33C`�Cb  Cc�fCe�fCh�Cj  Ck��Cn�CpL�Cr33Ct  Cv�CxL�Cz�C{�fC~  C��C�33C��C��3C�  C��C��C�&fC��C�  C��C��C��C��3C�  C�&fC��C�  C��C��C��3C��C��C�  C��C�&fC��C�  C��C��C�  C��C��C�  C��C��C�  C��3C��fC��C�&fC��C�  C��C��C��3C��C�  C��3C�  C��C�33C��C�  C��C��C��3C�  C�&fC��C��3C�  C��C��C��fC�  C��C�&fC��C��3C��C��C�&fC��C��3C��C�  C�ٚC�  C��C��C��3C��C��3C��fC�  C��C�  C��fC��3C��C�  C��fC��C��C��3C��C��C�  C�&fC��C��C��C�  C��C��C�  C��C�&fC��C��C��3C��C�  C�  C��C�&fC��C�  C�  C��C��C��fC���D�3DY�D�D
��D&fD�3D,�D�3D33D��D33D��D!S3D$�D&ٚD)� D,ffD/33D2  D4�3D7�fD:Y�D=�D?��DBL�DD�fDGy�DI�3DL` DNٚDQ@ DS��DV�DXl�DZ�fD]  D_� Da�fDdS3Df��Di9�Dk��Dn,�Dp�fDs&fDu�fDx&fDz��D|��D9�D�� D��D�i�D�� D��fD�C3D��3D��fD�33D��3D���D�3D�Y�D�� D��3D�,�D�y�D���D�3D�C3D��fD��3D�fD�I�D���D��fD�3D�<�D�|�D��fD��fD�0 D�s3D�� D��3D�,�D�c3D���D��3D��D�<�D�i�D�� D���D�ٚD���D��D�@ D�` D�� D�� D���D�ٚD���D�fD�6fD�` D�|�Dǜ�Dȼ�D��fD�3D�@ D�i�DΖfD��fD��3D�  D�P Dԃ3Dճ3D�� D��D�<�D�ffDۖfD��3D���D�)�D�\�D�3D⹚D��fD�3D�9�D�i�D虚D���D�fD�<�D�l�DD�ɚD���D�,�D�S3D��D���D��D�fD�@ D�i�D�s3D���D�� D��3E   E ��E3E��E9�E�3EQ�E��EffE� Ex E  E� E�E�3EfE	&fE
)�E��E��E+3E( E��E� E  E!�E�3E�3E33E9�EɚE� E�fEt�Ex E!�E"	�E#�E$��E%� E'$�E($�E)��E*�fE,>fE-A�E.�3E/�fE0�fE2c3E3h E4� E5��E7nfE8k3E9� E:�fE<FfE?��EB��EE� EH��EK��EO	�ER4�EUa�EX!�E[I�E^nfEa�3Ed��Eg�Ej��EnVfEq;3Et` EwvfEz�fE}�E�t�E��E���E��E�� E�RfE��3E�Y�E���E�q�E��E���>���>���>���>���>���?   >���?��>���>���>���>���>���>L��?   >���>���>���?   >���>���>���>���>���?��?   ?333?333?L��?�  ?���?�  ?ٙ�?�33@��@��@,��@@  @Y��@l��@�  @���@���@�33@���@���@�33@�33@�  @���@���A33A	��A��A��A!��A)��A1��A8  A@  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414144444414441444411414111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?fff?�ff@&ff@`  @�  @�33@�  @�  A  AffA)��AH  Ai��A�  A�33A�  A���A�  A�33A���A�ffB��B
  B��B33B"��B*ffB2  B9��BA��BJ  BRffBY��Bb  Bj  Br  Bz  B�33B�33B�  B�33B�33B�ffB�33B���B���B�  B�  B�  B�  B�  B�33B�ffB�  B���Bș�B�33B�ffB���B���B���B���B�33B�33B�33B�33B�ffB�33B�33C �3C��CffC� C�3C
��C� C��C��C� CL�C� C�3C� CffC��C � C"ffC$�3C&�3C(��C*� C,ffC.��C0�3C2��C4��C6� C8ffC:L�C<��C>� C@ffCBffCDffCF�3CH��CJffCL��CN�3CP�3CR��CT� CV��CX��CZ�3C\�3C^�3C`��Cb� CdffCfffCh��Cj� ClL�Cn��Cp��Cr�3Ct� Cv��Cx��Cz��C|ffC~� C�Y�C�s3C�L�C�33C�@ C�L�C�Y�C�ffC�Y�C�@ C�Y�C�Y�C�L�C�33C�@ C�ffC�Y�C�@ C�Y�C�L�C�33C�Y�C�L�C�@ C�L�C�ffC�Y�C�@ C�Y�C�L�C�@ C�Y�C�L�C�@ C�Y�C�L�C�@ C�33C�&fC�L�C�ffC�Y�C�@ C�Y�C�L�C�33C�L�C�@ C�33C�@ C�Y�C�s3C�Y�C�@ C�Y�C�Y�C�33C�@ C�ffC�L�C�33C�@ C�Y�C�L�C�&fC�@ C�L�C�ffC�L�C�33C�L�C�Y�C�ffC�L�C�33C�L�C�@ C��C�@ C�Y�C�L�C�33C�L�C�33C�&fC�@ C�Y�C�@ C�&fC�33C�Y�C�@ C�&fC�Y�C�L�C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�@ C�Y�C�L�C�@ C�Y�C�ffC�L�C�L�C�33C�L�C�@ C�@ C�Y�C�ffC�L�C�@ C�@ C�Y�C�Y�C�&fC��D�3Dy�D,�D
��DFfD�3DL�D�3DS3DٚDS3DٚD!s3D$,�D&��D)� D,�fD/S3D2  D4�3D7�fD:y�D=9�D?��DBl�DEfDG��DJ3DL� DN��DQ` DSٚDV9�DX��DZ�fD]@ D_� DbfDds3Df��DiY�Dk��DnL�Dp�fDsFfDu�fDxFfDz��D|ٚDY�D�� D�,�D�y�D�� D�fD�S3D��3D��fD�C3D��3D���D�#3D�i�D�� D��3D�<�D���D���D�3D�S3D��fD��3D�fD�Y�D���D��fD�3D�L�D���D��fD�fD�@ D��3D�� D�3D�<�D�s3D���D��3D��D�L�D�y�D�� D�ɚD��D�	�D�)�D�P D�p D�� D�� D���D��D�	�D�&fD�FfD�p Dƌ�DǬ�D���D��fD�#3D�P D�y�DΦfD��fD�3D�0 D�` Dԓ3D��3D�� D��D�L�D�vfDۦfD��3D�	�D�9�D�l�D�3D�ɚD��fD�#3D�I�D�y�D詚D���D�fD�L�D�|�DD�ٚD��D�<�D�c3D���D���D���D�&fD�P D�y�D��3D���D�� D��3E  E ��E#3E��EA�E�3EY�E��EnfE� E� E E� E�E�3EfE	.fE
1�E��E��E33E0 E��E� E( E)�E�3E�3E;3EA�EњE� E�fE|�E� E!�E"�E#�E$��E%� E',�E(,�E)��E*�fE,FfE-I�E.�3E/�fE0�fE2k3E3p E4� E5��E7vfE8s3E9� E:�fE<NfE?��EB��EE� EH��EK��EO�ER<�EUi�EX)�E[Q�E^vfEa�3Ed��Eg�Ek�En^fEqC3Eth Ew~fEz�fE}��E�x�E��E���E�!�E�� E�VfE��3E�]�E� �E�u�E��E���G�O�G�O�?L��G�O�?L��G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?L��G�O�G�O�G�O�G�O�?L��?fffG�O�?�  G�O�?���?�ff?�  ?ٙ�@   @��@��@,��@9��@L��@`  @y��@�ff@�  @���@���@�33@���@ə�@�33@�33@�  @���A��A33A��A��A!��A)��A1��A9��A@  AH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441414144444414441444411414111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            @ �@ v@ V@ {@ O@ "�@ (�@ /�@ 6�@ <�@ D�@ Q�@ `B@ m:@ z3@ ��@ ��@ ��@ ��@ ��@ ��@ ��@ �@ � @v@o@g@,`@9X@F�@UU@c�@o�@~K@��@��@�A@��@�>@�7@ލ@�4@��@�@�@!s@/�@=q@K@X�@ff@t�@�@�\@�U@�M@�@�W@��@��@�@@�9@
�@�@&;@3�@B8@O0@\�@k.@x&@�p@��@��@��@��@�@�@�@��@  @@O@(G@7L@DD@Q=@`�@n�@{�@��@��@��@��@��@�|@�t@�m@�e@@@
@+�@9X@I@V@bN@r�@�@�P@�H@�A@��@Ĝ@є@�;@��@��@�@�@!s@0x@=q@I�@Yn@hs@uk@��@�@�@�Y@��@��@��@�@�@��@
=@�@&�@5?@B8@N�@]�@k.@x&@�p@��@�y@�r@��@��@׹@�@�@ �@�@�@+@7�@DD@SI@`B@m:@|?@�7@�0@�5@�-@�&@�@�@��@��@	�@	@	 @	-@	9X@	H]@	UU@	bN@	p�@	�@	��@	��@	�A@	�F@	��@	ψ@	��@	�@	��@
%@
{@
#�@
0x@
<@
K@
Yn@
hs@
t�@
�@
�@
�a@
��@
�@
�J@
�O@
�H@
��@
��@�@�@$�@3�@@,@M$@\)@k.@ww@��@�@��@�@�^@��@׹@�@�@ �@�@[@*S@7L@D�@Q�@`�@m�@z�@��@�<@��@�-@�w@�|@�t@�@� @v@�@�@,`@;d@I@S�@`B@��@9X@�@ȴ@V@S�@��@܀@ �@e�@�M@�@@5?@�@�@�@c�@�!@��@I�@��@��@+�@r@��@ �@F�@��@��@b@Q�@��@�\@�@V@�0@�
@�@Z�@�a@��@#�@g�@�Y@�@3�@x&@�@�@9X@~K@��@�@N�@�#@�#@"�@k.@�-@��@@,@��@�o@@V@�U@�T@ (G@ m�@ �-@ � @!:�@!�@!Ĝ@"�@"Lu@"�@"��@#6@#Z@#�a@#�H@$&;@$i�@$��@$�@%3�@%v�@%�@%��@&<�@&|�@&��@&�9@'8�@'v@'��@'�Y@(/�@(m:@(��@(�m@)$/@)a�@)�a@)��@*O@*X@*��@*��@+o@+R�@+��@+�C@,o@,SI@,�u@,Ӡ@-{@-V@-��@-�
@.6@.X@.��@.�h@/�@/Z�@/��@/�/@0�@0^5@0�a@0ލ@1[@1^5@1�@1��@2#�@2e�@2��@2�@3'�@3i!@3��@3��@4+�@4l�@4��@4��@5,`@5k�@5��@5�T@6""@6`A@6�@6�@7�@7R�@7�@7��@8�@8B�@8}�@8�R@8�Y@9,`@9ff@9�@9�h@:b@:�p@:�@;��@<@<��@={@=��@>""@>��@?3�@?�h@@I@@�L@A`B@B
�@B~K@B�@C��@D
=@D��@E"�@E�u@F:�@F��@GP�@G��@Hg@H�[@I}�@I�4@J�#@K�@Ks_@L�@L��@M/�@M�@NC�@N�r@OQ�@O�@PT�@Q��@S�@Tb�@U�!@V��@XV@Y�!@[
�@\7L@]�@^�l@`7�@a�M@b�@dB�@e��@f��@hDD@i��@j�@lYn@m��@o �@p;e@q��@r�,@tS�@u��@v�y@xO1@y�P@z�4@|7�G�O�G�O�@ G�O�@ G�O�@ �G�O�G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�@ @ �G�O�@ jG�O�@ �@ v@ �@ 1@ 
=@ �@ �@ @ b@ o@ {@ 6@ B@ O@ 
@  �@ "�@ $.@ '�@ )�@ -@ /�@ 2�@ 5?@ 7�@ :�@ >@ A�@ D�@ H]@ K�@ N�@ Q�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AӁAӃA�|�A�p�A�bNA�G�A�C�A�A�A�?}A�E�A�9XA��A�JA���A���A���A��A��A��mA��A�33A�S�A�=qA�$�A��A�bA�A���A��A��
A���A�G�A�"�A���A�;dA�G�A�\)A��PA�&�A�dZA���A�5?A��jA���A�A�%A��
A���A�Q�A��+A�Q�A�9XA���A��
A�7LA���A�+A�ƨA�v�A���A�p�A�l�A��wA�&�A��/A���A��7A��uA��
A�JA�  A�v�A���A�=qA�ȴA�S�A�?}A��A��A�oA��^A�ZA�JA��!A�|�A�(�A�A���A�E�A��A�VA�C�A�%A��FA�l�A���A��DA�;dA�ƨA�XA�(�A� �A�bNA��DA�E�A��7A�K�A�A�-A���A���A�M�A�x�A��/A~VA}�A{hsAz$�Ayx�Ax��AwK�Au�
As�As&�ArI�ApȴAnbNAm��Al�!Ak�TAj�Ag�AeAd�+Ac%AahsA`�A`{A_�A]�#A\�A\v�A[\)AYp�AX�jAXQ�AW�AW
=AT��AS��AO�AN��AMl�AK�AJ~�AIXAG�AGp�AE�AD  ACABbAA��A@�A@�A?��A?C�A>��A>{A<��A;��A;�A:��A:(�A9�mA9ƨA933A7�-A6�+A5oA3�hA3`BA2n�A0��A0VA0=qA/�A.��A.M�A.A-��A-�;A-`BA,��A+�mA+;dA*r�A*  A(ffA&A�A$ĜA#|�A"�DA �+A��A��AM�A/A{A��A|�A~�AAdZA��A~�A��A��A��AA��AZAA�PA�9AM�A1A�
A�PA�uA�;A�hA
�+AK�A{A�A~�A��A��A  A%@�33@�Q�@���@�X@�bN@�b@�l�@��h@�bN@��@�@��@�5?@��@�V@��@���@׍P@�%@�ƨ@Ƈ+@�ff@���@�/@�(�@�J@�A�@�%@��-@�X@�K�@��^@���@��
@���@���@�|�@���@��@�@���@�
=@��7@���@�z�@�o@�-@�7L@��/@�z�@��;@���@�-@���@��j@�@��^@�A�@���@�@�G�@��`@��u@�9X@��H@��@��`@�1@;d@}�T@|��@y�@xA�@w+@u�h@sƨ@q�@p�u@nE�@mp�@j-@ihs@g��@fV@c�
@b^5@`��@^��@\�@Z�@X�9@UV@T1@Q��@P  @N��@M�-@MO�@Lj@J�@I�@H1'@F�R@F$�@EO�@B�@A�7@@  @>ȴ@=/@;@8�`@6�y@6V@5�@4�@3dZ@2M�@1�@0A�@.v�@-�T@-��@,�@+�@*��@)��@)7L@(b@'l�@&v�@%�T@$�@#"�@"^5@!7L@ �9@ A�@\)@��@/@��@�F@�@��@G�@Q�@+@$�@@��@1@dZ@�!@�7@��@A�@�w@��@�-@�/@j@Z@ƨ@C�@
��@	�@	7L@��@�w@\)@�@�@/@��@I�@�
@�H@J@��@ r�?�;d?�{?�/?�/?�ƨ?�dZ?���?��#?��?��+?���?��`?��?�ƨ?�^5?�Q�?�
=?�Z?�t�?�!?��?ߝ�?ݑh?�(�?ۥ�?��H?��#?�b?�K�?�?�z�?��?�33?щ7?�&�?�bN?��;?��?���?�v�?͑h?��?˥�?˥�?ʟ�?ɺ^?ȓu?�1'?�K�?�`B?���?���?�A�?��w?�5??�p�?�1?��?���?�~�?��?���?��^?���?��^?���?�X?��^?��#?�=q?���?�?�dZ?��m?�j?��?��-?�V?���?���?�bN?�&�A�z�A�|�A�|�AӃA�~�A�|�A�~�A�~�A�~�AӁAӁA�|�A�~�AӃA�~�AӃAӁAӇ+AӁAӃAӅAӃAӃAӇ+AӇ+AӅA�~�AӁA�~�AӃAӁAӇ+AӁAӁA�x�A�v�A�p�A�p�A�r�A�r�A�dZA�Q�A�K�A�G�A�C�A�C�A�C�A�A�A�A�A�A�A�?}A�=qA�C�A�E�A�C�A�9XA�-A�"�A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            AӁAӃA�|�A�p�A�bNA�G�A�C�A�A�A�?}A�E�A�9XA��A�JA���A���A���A��A��A��mA��A�33A�S�A�=qA�$�A��A�bA�A���A��A��
A���A�G�A�"�A���A�;dA�G�A�\)A��PA�&�A�dZA���A�5?A��jA���A�A�%A��
A���A�Q�A��+A�Q�A�9XA���A��
A�7LA���A�+A�ƨA�v�A���A�p�A�l�A��wA�&�A��/A���A��7A��uA��
A�JA�  A�v�A���A�=qA�ȴA�S�A�?}A��A��A�oA��^A�ZA�JA��!A�|�A�(�A�A���A�E�A��A�VA�C�A�%A��FA�l�A���A��DA�;dA�ƨA�XA�(�A� �A�bNA��DA�E�A��7A�K�A�A�-A���A���A�M�A�x�A��/A~VA}�A{hsAz$�Ayx�Ax��AwK�Au�
As�As&�ArI�ApȴAnbNAm��Al�!Ak�TAj�Ag�AeAd�+Ac%AahsA`�A`{A_�A]�#A\�A\v�A[\)AYp�AX�jAXQ�AW�AW
=AT��AS��AO�AN��AMl�AK�AJ~�AIXAG�AGp�AE�AD  ACABbAA��A@�A@�A?��A?C�A>��A>{A<��A;��A;�A:��A:(�A9�mA9ƨA933A7�-A6�+A5oA3�hA3`BA2n�A0��A0VA0=qA/�A.��A.M�A.A-��A-�;A-`BA,��A+�mA+;dA*r�A*  A(ffA&A�A$ĜA#|�A"�DA �+A��A��AM�A/A{A��A|�A~�AAdZA��A~�A��A��A��AA��AZAA�PA�9AM�A1A�
A�PA�uA�;A�hA
�+AK�A{A�A~�A��A��A  A%@�33@�Q�@���@�X@�bN@�b@�l�@��h@�bN@��@�@��@�5?@��@�V@��@���@׍P@�%@�ƨ@Ƈ+@�ff@���@�/@�(�@�J@�A�@�%@��-@�X@�K�@��^@���@��
@���@���@�|�@���@��@�@���@�
=@��7@���@�z�@�o@�-@�7L@��/@�z�@��;@���@�-@���@��j@�@��^@�A�@���@�@�G�@��`@��u@�9X@��H@��@��`@�1@;d@}�T@|��@y�@xA�@w+@u�h@sƨ@q�@p�u@nE�@mp�@j-@ihs@g��@fV@c�
@b^5@`��@^��@\�@Z�@X�9@UV@T1@Q��@P  @N��@M�-@MO�@Lj@J�@I�@H1'@F�R@F$�@EO�@B�@A�7@@  @>ȴ@=/@;@8�`@6�y@6V@5�@4�@3dZ@2M�@1�@0A�@.v�@-�T@-��@,�@+�@*��@)��@)7L@(b@'l�@&v�@%�T@$�@#"�@"^5@!7L@ �9@ A�@\)@��@/@��@�F@�@��@G�@Q�@+@$�@@��@1@dZ@�!@�7@��@A�@�w@��@�-@�/@j@Z@ƨ@C�@
��@	�@	7L@��@�w@\)@�@�@/@��@I�@�
@�H@J@��@ r�?�;d?�{?�/?�/?�ƨ?�dZ?���?��#?��?��+?���?��`?��?�ƨ?�^5?�Q�?�
=?�Z?�t�?�!?��?ߝ�?ݑh?�(�?ۥ�?��H?��#?�b?�K�?�?�z�?��?�33?щ7?�&�?�bN?��;?��?���?�v�?͑h?��?˥�?˥�?ʟ�?ɺ^?ȓu?�1'?�K�?�`B?���?���?�A�?��w?�5??�p�?�1?��?���?�~�?��?���?��^?���?��^?���?�X?��^?��#?�=q?���?�?�dZ?��m?�j?��?��-?�V?���?���?�bN?�&�A�z�A�|�A�|�AӃA�~�A�|�A�~�A�~�A�~�AӁAӁA�|�A�~�AӃA�~�AӃAӁAӇ+AӁAӃAӅAӃAӃAӇ+AӇ+AӅA�~�AӁA�~�AӃAӁAӇ+AӁAӁA�x�A�v�A�p�A�p�A�r�A�r�A�dZA�Q�A�K�A�G�A�C�A�C�A�C�A�A�A�A�A�A�A�?}A�=qA�C�A�E�A�C�A�9XA�-A�"�A��A�{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�!B�!B�B�B�B�B�B�B�B�B�B�B�B�!B�!B�!B�'B�-B�9B�dB�5B��B��B��B��B��B��B��B��BB�B.B1'B33B=qBs�B}�B��B��B��B�hB��B��B��B��B��B��B��B��B��B��B�PB�\B�DB�+B�+B�7B�DB�7B�B|�Bx�Bs�Br�Bp�Bm�B`BBW
BO�BH�B9XB5?B'�B�B�BB��B�mB�BB�B�B��B��BŢB�LB�dB�XB�?B�B�B�B��B��B��B�hB�+Bw�Bt�Bl�BiyBgmBW
BA�B6FB�B
=B
��B
�mB
�)B
��B
�FB
��B
�hB
o�B
aHB
[#B
L�B
F�B
@�B
8RB
/B
 �B
{B
VB
1B
B	��B	��B	��B	��B	�yB	�#B	��B	ɺB	�wB	�LB	�9B	�B	��B	��B	��B	��B	�hB	�=B	�+B	�B	�B	x�B	r�B	aHB	S�B	L�B	I�B	C�B	=qB	33B	.B	'�B	 �B	�B	{B	oB	VB	
=B	+B	B	B��B��B�B�B�ZB�HB�;B�5B�)B�B�B��BɺBǮBǮB�qB�?B�3B�3B�B��B��B��B��B��B��B��B��B��B��B��B�oB�=B� B~�Bx�Bt�Br�Bn�Bk�BdZBdZB[#BZBW
BW
BS�BM�BM�BK�BL�BL�BK�BJ�BI�BI�BG�BG�BG�BF�BE�BD�BC�BF�BG�BA�BD�BA�B=qB=qB:^B;dB8RB5?B33B5?B2-B49B5?B49B49B49B7LB6FB6FB49B8RB9XB7LB6FB?}B>wBC�BJ�BS�Bq�B��B��B�qB�wB�
B�B��B�B�B��B	�B	�B	#�B	&�B	E�B	[#B	cTB	hsB	u�B	v�B	�B	�oB	��B	��B	��B	��B	�!B	�-B	�FB	�jB	��B	ĜB	ɺB	��B	�B	�B	�5B	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B
  B	��B
B
B
+B
	7B

=B
DB
VB
hB
oB
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
%�B
'�B
,B
-B
/B
1'B
2-B
33B
33B
49B
6FB
6FB
8RB
9XB
9XB
:^B
=qB
=qB
?}B
@�B
A�B
D�B
F�B
H�B
I�B
J�B
J�B
L�B
M�B
N�B
N�B
P�B
P�B
P�B
P�B
S�B
T�B
T�B
VB
XB
W
B
YB
ZB
ZB
\)B
^5B
^5B
^5B
^5B
`BB
`BB
bNB
bNB
cTB
dZB
e`B
e`B
gmB
hsB
jB
iyB
jB
k�B
k�B
l�B
m�B
n�B
o�B
p�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
w�B
x�B
y�B
y�B
z�B
z�B
{�B
{�B
|�B
}�B
~�B
~�B
� B
� B
�B
� B
�B
�B
�B
�B
�%B
�B
�1B
�=B
�=B
�JB
�VB
�\B
�bB
�oB
�oB
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
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�'B
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
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�^B
�^B
�^B
�^B�!B�'B�!B�B�!B�!B�!B�!B�!B�B�!B�!B�!B�B�!B�B�!B�B�'B�!B�B�B�-B�B�B�!B�'B�!B�!B�'B�!B�B�B�B�B�B�B�B�B�!B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�EB�B��B��B��B��B��B��B��B��B�B�B-�B1B3B=ZBs�B}�B�}B��B�xB�SB��B��B��B��B��B�uB��B��B�|B�|B�@B�LB�5B�B�B�)B�7B�*B�B|�Bx�Bs�Br�Bp�Bm�B`9BWBO�BH�B9QB59B'�B�B�BB��B�jB�?B�B�B��B��BŢB�LB�eB�YB�AB�B�B�B��B��B��B�nB�2Bw�Bt�Bl�Bi�BgvBWBA�B6PB�B
HB
� B
�yB
�5B
��B
�SB
��B
�vB
o�B
aWB
[2B
L�B
F�B
@�B
8cB
/-B
 �B
�B
iB
EB
B	�B	��B	��B	��B	�B	�:B	�	B	��B	��B	�eB	�RB	�.B	�B	��B	��B	��B	��B	�ZB	�HB	�7B	�$B	x�B	r�B	ahB	TB	L�B	I�B	C�B	=�B	3UB	.7B	(B	 �B	�B	�B	�B	|B	
cB	RB	9B	4B�B��B��B�B�B�sB�fB�aB�UB�DB�EB�B��B��B��B��B�oB�dB�dB�:B�B�B�B�B� B��B��B��B��B��B��B��B�vB�9B4ByBt�Br�Bn�Bk�Bd�Bd�B[`BZ[BWHBWIBT7BNBNBLBMBMBL	BKBI�BI�BG�BG�BG�BF�BE�BD�BC�BF�BG�BA�BD�BA�B=�B=�B:�B;�B8�B5�B3�B5�B2{B4�B5�B4�B4�B4�B7�B6�B6�B4�B8�B9�B7�B6�B?�B>�BC�BK$BT]BrB��B�jB��B��BׁB؉B�[B�B�2B�[B	#B	2B	$gB	'|B	F8B	[�B	c�B	iB	vfB	woB	��B	�B	�BB	�pB	��B	��B	��B	��B	�B	�-B	�IB	�eB	ʆB	ѴB	��B	��B	�B	�GB	�\B	�_B	�tB	�~B	�B	��B	��B	��B	��B
 �B	��B

B
B
/B

>B
GB
QB
fB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%B
'B
)'B
-BB
.KB
0[B
2jB
3sB
4|B
4B
5�B
7�B
7�B
9�B
:�B
:�B
;�B
>�B
>�B
@�B
A�B
B�B
FB
HB
J+B
K3B
L=B
L@B
NNB
OWB
P`B
PcB
RqB
RtB
RwB
RyB
U�B
V�B
V�B
W�B
Y�B
X�B
Z�B
[�B
[�B
]�B
_�B
_�B
_�B
_�B
bB
bB
dB
dB
eB
f'B
g0B
g2B
iBB
jKB
lZB
kWB
l`B
mhB
mkB
ntB
o}B
p�B
q�B
r�B
s�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
w�B
x�B
y�B
y�B
z�B
{�B
{�B
}B
}B
~B
~B
B
�#B
�+B
�.B
�7B
�9B
�BB
�>B
�MB
�OB
�XB
�]B
�uB
�vB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�	B
�B
�"B
�'B
�2B
�?B
�JB
�YB
�dB
�iB
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
�	B
�B
�!B
�&B
�4B
�VB
�kB
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�9B
�PB
�eB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�/B
�?B
�MB
�bB
�lB
�{B
��B
��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B��B��B��B�B��B��B��B� B��B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811161802322021061413555220210614135552202106171313302021061713133020210617131330201811161802322021061413555220210614135552202106171313302021061713133020210617131330PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018111618023220181116180232  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111618023220181116180232QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018111618023220181116180232QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150820210617131508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                