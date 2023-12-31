CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-20T08:00:36Z creation      
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
_FillValue                 $  L\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  a   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  u�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �<   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   |   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                      	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � Argo profile    3.1 1.2 19500101000000  20190220080036  20210617131515  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               <   <DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؤ�C0�@ؤ�C0�11  @ؤ���`@ؤ���`@6'�`�N�@6'�`�N��c͑hr�!�c͑hr�!11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@@  @�33@�33@�ff@�  @���AffA!��AA��A`  A�  A�  A�  A���A���A���A�  A�  B ffB  BffBffB ffB(ffB0  B8  B@  BH  BP  BX  B`��Bh  BpffBxffB�  B�  B�33B�33B���B�  B�33B�  B�33B�33B�33B�ffB�  B�  B���B���B�  B�33BǙ�B˙�BЙ�B�ffB�33B�33B�  B�ffB�33B�  B�ffB�ffB�33B�  B���C33C33C�C  C	��C�fC�C�C�fC33CL�C�C�fC�fC�C L�C"33C$  C&33C(�C)��C,  C.33C0L�C2�C3�fC633C8  C9��C<  C>33C@L�CB33CD  CF33CH  CI��CL�CN�CP�CR  CS�fCV  CW��CZ�C\�C^  C_�fCa�fCd�Cf�Cg�fCj33Cl33Cn  Cp33Cr33Ct  Cv  Cw�fCz33C|�C~�C�  C��3C��C�  C��fC��C�  C��fC��C�  C��3C�  C��fC��C�  C��3C�  C�&fC��C��3C��C�  C��fC�  C��C��C��fC��3C�  C��C��C��fC�  C��C��C��3C��C��C��3C��C��C�  C��C��C��3C�  C��3C��fC��C��3C��fC��C��C�  C��3C��3C�  C�  C��3C��C��C��3C��C��C��C��C�  C��C��C��C�&fC��C�  C�&fC��C�  C��C��C�  C�&fC��C��3C��C�  C��fC�  C��C�  C��fC��3C��C��C��3C�ٚC��fC�  C��C�  C��fC��3C�  C��C��C�&fC��C��3C��C�&fC��C��3C�  C��C��C�&fC��C��fC��3C�  C�  C��C��C�&fC��C��fC��3D&fD�fDs3D,�D�fD�fDs3D&fD��D� D  D �fD#Y�D%��D(� D+3D-�fD0@ D2�fD5�fD8@ D:�3D=� D@L�DCfDE� DH��DKS3DN  DP��DS� DV&fDX�fD[y�D^&fD`��DcY�De��Dh��Dk  Dm�fDpl�Ds�Du�fDxY�Dz�3D}�D�fD�6fD���D���D�@ D��3D�� D�@ D��3D�� D�,�D��fD��fD�#3D�l�D��3D�  D�C3D���D�ٚD�  D�i�D���D���D�C3D��fD���D��D�` D��3D�� D�6fD��3D��fD�	�D�P D��fD��fD�FfD���D��D�<�D���D�ٚD�#3D�ffD�� D��fD�FfD��3D��3D�&fD�l�Dİ D��fD�0 D�i�Dɳ3D�� D�)�D�c3DΦfD��D�)�D�ffDӬ�D��fD�#3D�\�Dؠ D�� D�&fD�ffDݩ�D��fD�#3D�` D� D���D��D�S3D� D��3D���D�@ D� D���D���D�<�D�y�D� D��D�&fD�\�D��3D���D� D�0 D�p D��3D��fE fE � EL�E�E�fE#3E��ET�E��E��E�E�3EI�EٚEi�E��E	�fE
!�E
�3ED�E��E^fE� El�E~fE�E	�E�3E��E��E�3E[3E�fE��E3E�3E�3E�E �E!��E"� E$&fE%&fE&��E'�fE)3E*�E+��E,� E-ɚE/T�E0\�E1�fE2�3E4fE5�fE6��E83E9�E:{3E;p E<�3E@)�ECQ�EFT�EIt�ELt�EO{3ER�3EU�3EY3E[� E_>fEb1�Ee��Ehy�EkٚEn�fEq�fEu3ExA�E{a�E~{3E�͚E�ffE�� E�t E���E�� E�3E�� E�;3E��E�[3E��fE���E�C3E��fE��E�O3E��fE��3E�0�E�x�E��3E��E�vfE��fE�  E�h E��fE��3E�\ E���E��fE�I�E���E���E�73E�|�E��3E�%�E�h >���>L��>L��>L��>L��=���>L��>���>���>���>���>���>���>L��>���>���>L��>���>���>L��>L��>L��>L��>���>���>���>���>���>���>���?��?L��?�  ?�  ?�ff?�  ?ٙ�?�33@33@   @333@@  @S33@s33@�  @�  @���@�33@�  @�  @���@�ff@�ff@�33A��A	��A33A��A!��A)��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441114444414414444414441441114111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?L��?�  @&ff@`  @�33@�33@�ff@�  AffAffA)��AI��Ah  A�  A�  A�  A���A���A���A�  A�  BffB
  BffBffB"ffB*ffB2  B:  BB  BJ  BR  BZ  Bb��Bj  BrffBzffB�  B�  B�33B�33B���B�  B�33B�  B�33B�33B�33B�ffB�  B�  B���B���B�  B�33Bș�B̙�Bљ�B�ffB�33B�33B�  B�ffB�33B�  B�ffB�ffB�33B�  C ffC�3C�3C��C� C
L�CffC��C��CffC�3C��C��CffCffC��C ��C"�3C$� C&�3C(��C*L�C,� C.�3C0��C2��C4ffC6�3C8� C:L�C<� C>�3C@��CB�3CD� CF�3CH� CJL�CL��CN��CP��CR� CTffCV� CXL�CZ��C\��C^� C`ffCbffCd��Cf��ChffCj�3Cl�3Cn� Cp�3Cr�3Ct� Cv� CxffCz�3C|��C~��C�@ C�33C�L�C�@ C�&fC�L�C�@ C�&fC�L�C�@ C�33C�@ C�&fC�Y�C�@ C�33C�@ C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�L�C�&fC�33C�@ C�Y�C�L�C�&fC�@ C�Y�C�L�C�33C�L�C�L�C�33C�Y�C�L�C�@ C�Y�C�L�C�33C�@ C�33C�&fC�L�C�33C�&fC�L�C�L�C�@ C�33C�33C�@ C�@ C�33C�Y�C�L�C�33C�Y�C�Y�C�L�C�Y�C�@ C�Y�C�Y�C�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�Y�C�L�C�@ C�ffC�L�C�33C�Y�C�@ C�&fC�@ C�Y�C�@ C�&fC�33C�L�C�L�C�33C��C�&fC�@ C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�ffC�L�C�33C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�@ C�L�C�Y�C�ffC�L�C�&fC�33DFfD�fD�3DL�DfD�fD�3DFfD��D� D@ D �fD#y�D&�D(� D+33D-�fD0` D3fD5�fD8` D;3D=� D@l�DC&fDE� DH��DKs3DN@ DP��DS� DVFfDYfD[��D^FfD`ٚDcy�Df�Dh��Dk@ Dm�fDp��Ds,�Du�fDxy�D{3D}9�D�fD�FfD���D���D�P D��3D�  D�P D��3D�� D�<�D��fD��fD�33D�|�D��3D� D�S3D���D��D�0 D�y�D���D��D�S3D��fD���D�)�D�p D��3D�  D�FfD��3D��fD��D�` D��fD�fD�VfD���D���D�L�D���D��D�33D�vfD�� D�fD�VfD��3D��3D�6fD�|�D�� D�fD�@ D�y�D��3D�  D�9�D�s3DζfD���D�9�D�vfDӼ�D��fD�33D�l�Dذ D�� D�6fD�vfDݹ�D��fD�33D�p D� D���D��D�c3D� D��3D��D�P D� D���D�	�D�L�D�D�� D���D�6fD�l�D��3D���D�  D�@ D�� D��3D��fE fE � ET�E�E�fE+3E��E\�E��E��E!�E�3EQ�E�Eq�E	�E	�fE
)�E
�3EL�E��EffE� Et�E�fE�E�E�3E��E�E�3Ec3E�fE��E3E�3E�3E�E �E!��E"� E$.fE%.fE&��E'�fE)#3E*!�E+��E,� E-њE/\�E0d�E1�fE33E4fE5�fE6��E83E9�E:�3E;x E<�3E@1�ECY�EF\�EI|�EL|�EO�3ER�3EU�3EY#3E[� E_FfEb9�Ee��Eh��Ek�En�fEq�fEu3ExI�E{i�E~�3E�њE�jfE�� E�x E���E�� E�#3E�� E�?3E��E�_3E��fE���E�G3E��fE��E�S3E��fE��3E�4�E�|�E��3E��E�zfE��fE� E�l E��fE��3E�` E���E��fE�M�E���E���E�;3E���E��3E�)�E�l G�O�G�O�G�O�G�O�G�O�?��?333?L��G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�?333G�O�G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?L��G�O�G�O�?fff?���?�ffG�O�?�  ?�ff@   @��@��@333@@  @S33@`  @s33@���@�  @�  @���@�33@�  @�  @���@�ff@�ffA��A	��A��A33A!��A)��A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444441114444414414444414441441114111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ @ �@ V@ {@ �@ "�@ *S@ /�@ 5�@ <�@ D�@ R�@ _�@ m:@ z�@ ��@ ��@ ��@ �-@ �&@ ��@ �#@ �@ �q@@�@g@,`@:@G�@UU@b�@p�@�@��@�H@��@��@@��@ލ@��@�,@�@{@"�@0x@>@Lu@X�@ff@s_@�@�\@��@�M@��@�@��@��@�@��@�@�@%�@4�@B8@O0@\)@i!@x�@�|@�u@�m@��@�@�@׹@�@�@@V@�@(G@7L@FQ@SI@_�@n�@{�@�+@�0@�5@��@��@�@��@�@�e@j@o@ �@-�@:@I@UU@a�@qS@~�@��@��@��@��@�2@��@ލ@�@�~@%@*@"�@/@>�@Lu@X�@g�@uk@��@�\@�U@�@�@ƨ@Ӡ@��@�@��@�@�@%�@1�@A�@N�@[z@i�@v@�|@��@��@�@��@�@�[@�@�Y@��@�@�@)�@5?@C�@Q�@`�@m�@y�@��@��@��@��@��@�|@��@�y@�q@	j@	o@	g@	+�@	:@	F�@	S�@	c�@	o�@	|�@	��@	�H@	�A@	�9@	��@	�7@	��@	��@	��@
�@
�@
#�@
1'@
>@
Lu@
X�@
g�@
uk@
�d@
�h@
�a@
��@
�^@
�W@
Ӡ@
�@
�@
��@J@�@$�@4�@@�@M$@\)@k.@ww@��@�@�@��@�@�W@խ@�@�@  @J@�@(�@7L@E�@S�@`B@l�@{�@��@��@�(@�~@��@�*@܀@��@�e@�@@�@-@;d@I�@V@a�@o�@�E@D�@��@�h@"�@m�@�^@@Lu@�0@��@&;@l�@��@�,@?}@��@��@*@\�@�A@��@:@�@�|@�@dZ@�!@��@E�@�\@׹@"�@i!@�-@�~@@,@�|@�*@{@\�@�5@��@7L@}�@Ĝ@�Q@H]@��@��@$�@m:@��@  @G�@�@�
@
@g�@�r@�q@<�@�d@�c@ V@ T�@ ��@ �H@!'�@!l�@!�9@!��@">�@"�p@"�o@#@#V@#�@#�@$)�@$n�@$��@$�,@%B8@%��@%є@&�@&a�@&��@&�@'8�@'~�@'��@(
=@(O�@(��@(ލ@)&;@)k.@)��@)��@*;d@*~K@*�2@+�@+K@+��@+��@,�@,Z�@,�@,�@-(G@-k.@-��@-�@.6�@.z�@.��@/�@/I�@/�P@/��@0{@0UU@0��@0�/@1"�@1ff@1��@1��@2/�@2t@2��@2�9@3@,@3��@3��@4�@4Lu@4��@4��@5�@5X�@5�0@5�t@6g@6`�@6�(@6�@7+@7m�@7��@7�@85@@8v@8��@8�q@97L@9x�@9�@9�q@:3�@:qS@:��@:�@;-�@;k�@;�M@;�@<�@<Wb@<�@=r�@=��@>�|@>��@?�i@?��@@�#@A+�@A�i@B*S@B��@C7�@C܀@DI�@D�@Eb�@F	�@Fv�@G*@G�@H&�@H�u@I> @I�~@J&;@J��@K?}@K��@L]�@Lψ@Mv�@M�T@N�|@N��@O��@O�,@P�#@Q�E@SV@T�@U�e@W<@X�|@Y��@[I@\��@]�7@_@,@`�d@a�e@c0x@d�@e�[@g(G@h�P@i�4@kA�@l�#@m�y@oFP@p��@q�H@s)�@t��@u�/@w:@x�p@y��@{/@{r�@{�-@{�@|5?@|��@|��@}�@}[z@}�I@}׹@~6@~V@~� @~�'@%�@~K@�^@�@�'A@�D�@�bN@���@��M@��Q@��@��@�,@�W�@�tG�O�G�O�G�O�G�O�G�O�@  �@ ^@ G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�G�O�@ �@ @ vG�O�@ �@ �@ 
=@ �@ �@ �@ @ @ {@ �@ �@ O@ �@  �@ "�@ %�@ (�@ +�@ -�@ 1'@ 3�@ 7L@ :�@ >�@ A�@ D�@ H]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�t�A�z�A�x�A�z�A��A�~�A��A��A��A��+A��7A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��-A��9A��9A��FA��RA��^A�ĜA�A�ƨA�ȴA���A���A�ĜA�A��jA��jA��RA��9A��!A��A���A�x�A�A�bNA���A�
=A�r�A��A�"�A���A�oA��PA�
=A��/A�VA�M�A���A�t�A�VA�5?A���A��A���A��A�O�A���A���A�n�A�S�A� �A���A��A��!A�XA�C�A�(�A��HA��uA�+A��!A�M�A��+A�ȴA�bA�;dA���A�I�A��A��A��`A�"�A�A��A�|�A��A���A�A��`A��`A��/A��A��/A��uA���A��9A�ffA�A�oA�O�A���A��\A���A�G�A�ƨA~bNAzE�Av��AtArA�Aqx�ApffAo�An�RAm�Am�AlI�Ael�AcAa�TA_�A]�A\�\A[��A[|�A[p�A[7LAY�mAWC�AV�AT^5AR=qAOXAL �AKS�AH�`AE��AE|�AD��ADz�ACK�A@��A?\)A=�A;�A8��A6��A6bA5ƨA5��A5XA4�`A3�;A3%A2{A1A/�hA,��A*�A*��A*�DA*z�A)p�A&�!A&VA%�A$�A"�A" �A M�A^5AAVA\)A�
Av�A��AA��Ax�AVA$�A��A�9AdZA��AE�AĜAVA=qAhsA
��A	��A �AoA��A�^A�A�AAC�A ��@��P@�{@���@�b@��F@�;d@���@���@���@�@�F@�=q@��D@�@�S�@�&�@�E�@���@�P@��@�^@�Z@��@�33@��@�bN@�v�@���@��#@�`B@�r�@�
=@�9X@ӥ�@�l�@��@ҟ�@��T@�
=@+@�ƨ@�1'@�|�@���@���@��@��@�ȴ@�5?@�|�@��@���@�1'@���@�ƨ@���@��w@��@��!@�Z@���@���@��@��@�/@��@�j@���@��/@���@�C�@���@��+@�/@�ƨ@��R@��@��H@��\@���@��/@~�+@}p�@{�@{o@z�!@y�7@v��@t��@s��@r�H@rJ@q7L@o|�@m�-@l9X@k33@j�\@e�@cS�@`Ĝ@\�D@\1@X�`@X1'@W�@V{@U��@Up�@R�\@Q�@P��@O�;@N��@N5?@I&�@G�@G+@Fȴ@C"�@B�H@A7L@@  @<�@:��@9��@9�7@8r�@7��@5�h@3�m@3@2M�@1�@0�@0  @.��@,9X@*J@(bN@'��@'�@&@$j@#��@#33@"�@ �u@�;@�@�@��@S�@=q@�7@��@��@
=@�R@v�@�D@o@�`@�`@A�@��@E�@�@@�-@1@
�@
��@
n�@	X@	X@��@ �@�w@�+@@`B@�@z�@�
@ƨ@��@��@��@��@hs@7L@ ��@ �u?�|�?��D?���?��9?��?��?�E�?�`B?���?�Z?�-?�J?�&�?�  ?�;d??�p�?�dZ?��H?��#?�^?�u?�1'?�+?��?�%?�O�?�(�?�=q?��?�ȴ?�?}?���?�33?��?�&�?У�?�  ?�|�?��?�p�?���?�j?˅?�C�?�"�?�~�?ɺ^?�X?ǍP?�+?��y?��y?��?öF?�hs?��;?���?��?���?��#?�x�?�r�?�1'?�Q�?�Q�?��u?�r�?���?���?�X?���?�=q?�^5?���?�"�?�dZ?�1?��D?�/?��h?�{?�v�?�;d?��w?��?���?�Ĝ?��`?�%?�%?�G�?�G�?�hs?���?���?���?���?���?��?�J?�-?�-?�M�?�n�?�n�?°!?°!?°!?��?�o?�S�?�S�?�t�?Õ�A�p�A�ffA�ffA�ffA�dZA�dZA�dZA�bNA�`BA�`BA�jA�|�A�t�A�n�A�|�A�x�A�t�A�p�A�dZA�`BA�r�A�x�A�z�A�z�A�z�A�|�A�z�A�z�A�z�A�t�A�x�A�x�A�v�A�r�A�n�A�t�A�|�A�|�A�z�A�x�A�z�A�x�A�v�A�v�A�v�A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��+A��A��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�p�A�t�A�z�A�x�A�z�A��A�~�A��A��A��A��+A��7A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��-A��9A��9A��FA��RA��^A�ĜA�A�ƨA�ȴA���A���A�ĜA�A��jA��jA��RA��9A��!A��A���A�x�A�A�bNA���A�
=A�r�A��A�"�A���A�oA��PA�
=A��/A�VA�M�A���A�t�A�VA�5?A���A��A���A��A�O�A���A���A�n�A�S�A� �A���A��A��!A�XA�C�A�(�A��HA��uA�+A��!A�M�A��+A�ȴA�bA�;dA���A�I�A��A��A��`A�"�A�A��A�|�A��A���A�A��`A��`A��/A��A��/A��uA���A��9A�ffA�A�oA�O�A���A��\A���A�G�A�ƨA~bNAzE�Av��AtArA�Aqx�ApffAo�An�RAm�Am�AlI�Ael�AcAa�TA_�A]�A\�\A[��A[|�A[p�A[7LAY�mAWC�AV�AT^5AR=qAOXAL �AKS�AH�`AE��AE|�AD��ADz�ACK�A@��A?\)A=�A;�A8��A6��A6bA5ƨA5��A5XA4�`A3�;A3%A2{A1A/�hA,��A*�A*��A*�DA*z�A)p�A&�!A&VA%�A$�A"�A" �A M�A^5AAVA\)A�
Av�A��AA��Ax�AVA$�A��A�9AdZA��AE�AĜAVA=qAhsA
��A	��A �AoA��A�^A�A�AAC�A ��@��P@�{@���@�b@��F@�;d@���@���@���@�@�F@�=q@��D@�@�S�@�&�@�E�@���@�P@��@�^@�Z@��@�33@��@�bN@�v�@���@��#@�`B@�r�@�
=@�9X@ӥ�@�l�@��@ҟ�@��T@�
=@+@�ƨ@�1'@�|�@���@���@��@��@�ȴ@�5?@�|�@��@���@�1'@���@�ƨ@���@��w@��@��!@�Z@���@���@��@��@�/@��@�j@���@��/@���@�C�@���@��+@�/@�ƨ@��R@��@��H@��\@���@��/@~�+@}p�@{�@{o@z�!@y�7@v��@t��@s��@r�H@rJ@q7L@o|�@m�-@l9X@k33@j�\@e�@cS�@`Ĝ@\�D@\1@X�`@X1'@W�@V{@U��@Up�@R�\@Q�@P��@O�;@N��@N5?@I&�@G�@G+@Fȴ@C"�@B�H@A7L@@  @<�@:��@9��@9�7@8r�@7��@5�h@3�m@3@2M�@1�@0�@0  @.��@,9X@*J@(bN@'��@'�@&@$j@#��@#33@"�@ �u@�;@�@�@��@S�@=q@�7@��@��@
=@�R@v�@�D@o@�`@�`@A�@��@E�@�@@�-@1@
�@
��@
n�@	X@	X@��@ �@�w@�+@@`B@�@z�@�
@ƨ@��@��@��@��@hs@7L@ ��@ �u?�|�?��D?���?��9?��?��?�E�?�`B?���?�Z?�-?�J?�&�?�  ?�;d??�p�?�dZ?��H?��#?�^?�u?�1'?�+?��?�%?�O�?�(�?�=q?��?�ȴ?�?}?���?�33?��?�&�?У�?�  ?�|�?��?�p�?���?�j?˅?�C�?�"�?�~�?ɺ^?�X?ǍP?�+?��y?��y?��?öF?�hs?��;?���?��?���?��#?�x�?�r�?�1'?�Q�?�Q�?��u?�r�?���?���?�X?���?�=q?�^5?���?�"�?�dZ?�1?��D?�/?��h?�{?�v�?�;d?��w?��?���?�Ĝ?��`?�%?�%?�G�?�G�?�hs?���?���?���?���?���?��?�J?�-?�-?�M�?�n�?�n�?°!?°!?°!?��?�o?�S�?�S�?�t�?Õ�A�p�A�ffA�ffA�ffA�dZA�dZA�dZA�bNA�`BA�`BA�jA�|�A�t�A�n�A�|�A�x�A�t�A�p�A�dZA�`BA�r�A�x�A�z�A�z�A�z�A�|�A�z�A�z�A�z�A�t�A�x�A�x�A�v�A�r�A�n�A�t�A�|�A�|�A�z�A�x�A�z�A�x�A�v�A�v�A�v�A��A��A��A��A�~�A�~�A��A��A��A��A��A��A��+A��A��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B]/B]/B^5B]/B]/B]/B]/B]/B]/B]/B]/B]/B^5B^5B^5B^5B^5B]/B]/B^5B]/B^5B^5B_;B`BB`BB_;BaHB`BBcTBcTBcTBcTBcTBdZBffBffBgmBhsBk�Bm�Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Bu�B�BB
=B�B�B�B�B&�B/B49BA�BI�BG�BG�BI�BH�BF�BE�BB�B<jB6FB0!B-B/B2-B5?B7LB6FB9XB8RB:^B<jB;dB:^B9XB8RB8RB6FB49B.B&�B�B�BPB1B�B�yB�fB�BŢB�'B��B�{B�=Bv�BR�B33B�B
�B
��B
�9B
�\B
l�B
YB
R�B
?}B
5?B
&�B
JB
	7B	��B	�`B	��B	�?B	��B	�B	|�B	w�B	o�B	l�B	iyB	hsB	gmB	]/B	-B	'�B	�B	hB	�B	JB	\B	bB	oB	oB	%B��B��B�yB��B�jB�9B�jB�'B�dB�dB��BĜB�?B�-B�?B�B��B�VB�1B�B�B�B}�B{�Bx�Bx�Bw�Bt�Bn�BhsBiyBffBdZBcTB]/B_;B\)B[#BW
BYBVBW
BW
BVBS�BR�BM�BK�BI�BH�BG�BE�BB�BC�BA�B<jB=qB=qB9XB8RB49B5?B49B33B1'B/B0!B.B,B-B(�B+B)�B'�B'�B&�B'�B&�B&�B%�B#�B&�B%�B%�B#�B%�B$�B'�B'�B'�B,B,B.B.B.B1'B0!B/B0!B2-B49B5?B49B49B7LB:^BD�BE�BE�BF�BH�BK�BgmBm�Bu�B�B��BB��B�5B�B��B��B	\B	uB	0!B	9XB	B�B	K�B	O�B	]/B	jB	p�B	�B	�=B	�bB	��B	��B	��B	��B	��B	�9B	�dB	�^B	ÖB	ŢB	ƨB	��B	�B	�B	�/B	�yB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
+B
1B
	7B
DB
JB
VB
bB
oB
uB
{B
�B
�B
�B
"�B
 �B
&�B
&�B
%�B
+B
+B
)�B
.B
-B
/B
0!B
/B
0!B
7LB
8RB
8RB
8RB
<jB
;dB
=qB
>wB
D�B
F�B
F�B
F�B
H�B
I�B
K�B
M�B
M�B
M�B
M�B
N�B
P�B
Q�B
S�B
W
B
YB
YB
YB
[#B
\)B
\)B
]/B
_;B
`BB
`BB
aHB
bNB
cTB
e`B
ffB
e`B
gmB
gmB
hsB
hsB
hsB
jB
k�B
m�B
m�B
n�B
o�B
q�B
q�B
r�B
q�B
s�B
t�B
t�B
t�B
v�B
u�B
w�B
w�B
w�B
z�B
y�B
z�B
|�B
|�B
|�B
|�B
|�B
}�B
� B
�B
� B
� B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�1B
�1B
�7B
�DB
�=B
�DB
�DB
�JB
�PB
�VB
�bB
�bB
�hB
�hB
�oB
�oB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�LB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�dB
�^B
�dB
�^B
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�^B
�dB
�^B
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
�jB
�dB
�dB
�jB
�dB
�jB
�jB
�dB
�jB
�dB
�dB
�dB
�jB\)B]/B]/B^5B^5B^5B]/B^5B^5B^5B_;B]/B]/BbNB^5B[#B^5B\)B\)B`BB_;B]/B]/B]/B\)B\)B]/B\)B\)B^5B]/B\)B]/B\)B]/B]/B^5B]/B]/B^5B]/B]/B]/B]/B]/B]/B\)B]/B]/B\)B^5B]/B]/B]/B]/B\)B]/B]/B]/B\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B]B]B^B]B]B]B]B]B]B]B]B]	B^B^B^B^B^B]B]B^B]B^B^B_B`$B`$B_Ba+B`&Bc9Bc9Bc:Bc:Bc;BdBBfNBfOBgWBh]BkpBm|Bq�Bq�Bq�Bq�Bq�Br�Br�Br�Br�Bu�B��BB
0BzB�B�B�B&�B/B4/BA�BI�BG�BG�BI�BH�BF�BE�BB�B<eB6BB0B-B/B2*B5=B7KB6EB9XB8RB:_B<kB;fB:aB9[B8VB8VB6KB4>B.B&�B�B�BXB9B�B�B�pB�BŭB�2B��B��B�JBv�BR�B3AB�B
��B
��B
�HB
�kB
l�B
Y'B
SB
?�B
5PB
&�B
\B
	JB	�B	�tB	��B	�SB	��B	�'B	}B	w�B	o�B	l�B	i�B	h�B	g�B	]HB	-'B	(	B	�B	�B	�B	eB	xB	~B	�B	�B	CB�B��B�B�B��B�YB��B�HB��B��B��BĿB�bB�QB�cB�8B��B�{B�VB�EB�3B�-B~B|Bx�Bx�Bw�Bt�Bn�Bh�Bi�Bf�Bd�Bc�B]]B_iB\WB[RBW:BYGBV4BW;BW;BV6BT*BS%BNBK�BI�BH�BG�BE�BB�BC�BA�B<�B=�B=�B9�B8�B4sB5zB4tB3oB1cB/WB0^B.QB,FB-LB)5B+AB*<B(0B(1B'*B(2B'+B',B&&B$B'-B&(B&)B$B&)B%$B(7B(8B(8B,QB,QB.^B.^B._B1rB0mB/hB0nB2{B4�B5�B4�B4�B7�B:�BD�BE�BE�BF�BIBLBg�Bm�Bv#B�|B�+B��B�^BޤB�B�8B�GB	�B	�B	0�B	9�B	CB	LQB	PlB	]�B	kB	q;B	��B	��B	�B	�1B	�AB	�uB	�sB	��B	��B	�B	�B	�TB	�cB	�lB	ЦB	��B	��B	��B	�LB	�[B	�rB	�B	��B	��B	��B	��B	��B	��B
B
!B
	*B

3B
CB
LB
[B
jB
{B
�B
�B
�B
�B
�B
#�B
!�B
(B
(B
'B
,/B
,2B
+0B
/KB
.HB
0XB
1aB
0^B
1gB
8�B
9�B
9�B
9�B
=�B
<�B
>�B
?�B
E�B
HB
HB
HB
J"B
K+B
M;B
OJB
OMB
OPB
OTB
P]B
RlB
SvB
U�B
X�B
Z�B
Z�B
Z�B
\�B
]�B
]�B
^�B
`�B
a�B
a�B
b�B
dB
eB
gB
h#B
g B
i0B
i3B
j<B
j?B
jBB
lQB
mZB
oiB
olB
pvB
qB
s�B
s�B
t�B
s�B
u�B
v�B
v�B
v�B
x�B
w�B
y�B
y�B
y�B
|�B
{�B
|�B
B
B
B

B
B
�B
�%B
�.B
�+B
�.B
�7B
�:B
�CB
�RB
�aB
�dB
�gB
�jB
�lB
�{B
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
�B
�(B
�5B
�IB
�TB
�aB
�lB
�zB
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
� B
�B
�B
�$B
�+B
�6B
�IB
�_B
�tB
��B
��B
��B
��B
��B
� B
�B
�*B
�;B
�IB
�`B
�tB
�~B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�(B
�1B
�HB
�PB
�fB
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
��B
��B
��B[�B]B]B^B^B^B]B^B^B^B_B]B]Bb$B^BZ�B^B[�B[�B`B_B]B]B]B[�B[�B]B[�B[�B^B]B[�B]B[�B]B]B^B]B]B^B]B]B]B]B]B]B\B]B]B\B^B]B]B]B]B\B]B]B]B\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201902200800362021061413560620210614135606202106171314122021061713141220210617131412201902200800362021061413560620210614135606202106171314122021061713141220210617131412PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019022008003620190220080036  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019022008003620190220080036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019022008003620190220080036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151520210617131515IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                