CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-05T15:00:27Z creation      
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
resolution        =���     �  Op   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                      HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 4Argo profile    3.1 1.2 19500101000000  20181205150027  20210617131510  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               2   2DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؓ�h���@ؓ�h���11  @ؓ�`� @ؓ�`� @6��e��O@6��e��O�c���7"�c���7"11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @   @@  @�  @���@�  @�  A   A��A$��A@  A`  A���A���A���A���A�  A�  A�33A�ffB ffB��B��BffB ffB'��B/��B733B@  BH��BPffBX  B`  Bh  Bp  Bw��B33B�  B�33B�  B���B���B�ffB�ffB�  B�  B�  B�33B�  B�33B�  B���B�  B�33B�  B�33B���B�  B�ffB�33B���B�ffB�  B뙚B�  B�ffB�  B���C   C  C33C�C�fC
  C�CL�C�C��C  C�C�C33C�C�fC   C"L�C$33C&  C(33C*�C,  C.33C0�C2  C3�fC5��C7��C9��C<33C>33C@33CB33CD�CF�CH  CJ  CK��CN33CP�CR  CT  CU�fCX33CZ�C\  C]�fC_�fCb33Cd  Ce��Ch  Cj�Cl33CnL�Cp  Cq��Cs�fCv  Cx  Cz�C|33C~33C��C��C��C�&fC�&fC�&fC�33C��C�  C��C�&fC��C��fC�  C��C�33C��C��3C��C�&fC��C��3C��C�&fC��C�  C��C�33C��C��C�&fC��C�  C��C�  C��fC��3C��C��C��3C��fC��3C�  C��C��C��C��fC��3C�  C��C��C��C��3C�  C��C�&fC��C��3C�  C��C�&fC��C��3C�  C��C�&fC��C��fC�  C��C��C��C��fC�  C��C�  C��3C��C�  C��3C��C�&fC��C�  C��C�  C��3C��C��C��C��3C��C�&fC��C��fC��3C�  C��C��C��C��fC�  C�  C��C�&fC��C��fC�  C��C��C��C��C��C��C�&fC��C��C��C�  C��3C��fC��C��3C��C�  C��3C�33C�&fD fD � D ��D��D��D
&fD� D�fD` D�3D�3D@ D  D�3D!�3D$Y�D'&fD)��D,� D/��D29�D4�fD7� D:fD<��D>�3DAFfDC�fDE��DG�fDJ�DL33DNS3DP� DR��DT�3DW@ DY� D\  D^s3D`� DcL�DeٚDhS3Dj�fDmy�Dp�Dr�3DuFfDw�3DzFfD|Y�D~� D��fD�ٚD�3D�L�D�|�D��3D�� D�fD�<�D�i�D���D�ɚD��3D�fD�0 D�` D��3D��fD�ɚD��D��D�33D�\�D�� D���D��3D�fD�6fD�i�D���D���D���D�,�D�` D��fD�� D�  D�,�D�` D��3D�� D��D�#3D�S3D�|�D��3D��fD�  D�VfD�� D�ɚD��D�L�D��3D��fD�3D�6fD�s3Dũ�D��fD�fD�<�D�l�D˜�D��3D��D�@ D�l�Dљ�D�� D���D�3D�9�D�` D؀ Dٜ�DڶfD�ɚD��3D��3D� D�&fD�@ D�\�D�y�D�3D� D��3D��D��D�33D�\�D쉚D��fD���D�fD�FfD�|�D�3D��3D�fD�L�D��3D��fD��fD� D�FfD�y�D��fE p E	�E�3E8 EњEi�E3E� E.fE�fEY�E��E{3E3E	0 E
D�ET�E�3E�Ec3EVfE� E�Eh E�3E�3E� E$�El�E��E  EL�E��E � E!� E#4�E$��E%�fE&��E(  E)p E*�3E,3E-Y�E.��E/��E0�fE2)�E3q�E4��E63E7^fE8�fE9��E:�fE<X E?y�EB�fEE�3EH� EK�EO	�ER�EUT�EX9�E[t�E^� Ea�fEd� Eg�Ej�fEn$�Eq!�EtA�Ew` Ez�fE}��E�w3E��f>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   ?��?333?L��?���?���?���?�33?ٙ�?�33@��@33@&ff@333@Fff@`  @l��@�  @���@�33@���@�ff@�33@�  @ə�@ٙ�@�33@�33A   A  A��A��A��A$��A,��A6ffA<��AC33AI��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444441441444414114111114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?fff?�  @   @`  @�  @���@�  @�  A  A��A,��AH  Ah  A���A���A���A���A�  A�  A�33A�ffBffB
��B��BffB"ffB)��B1��B933BB  BJ��BRffBZ  Bb  Bj  Br  By��B���B�  B�33B�  B���B���B�ffB�ffB�  B�  B�  B�33B�  B�33B�  B���B�  B�33B�  B�33B���B�  B�ffB�33B���B�ffB�  B왚B�  B�ffB�  B���C � C� C�3C��CffC
� C��C��C��CL�C� C��C��C�3C��CffC � C"��C$�3C&� C(�3C*��C,� C.�3C0��C2� C4ffC6L�C8L�C:L�C<�3C>�3C@�3CB�3CD��CF��CH� CJ� CLL�CN�3CP��CR� CT� CVffCX�3CZ��C\� C^ffC`ffCb�3Cd� CfL�Ch� Cj��Cl�3Cn��Cp� CrL�CtffCv� Cx� Cz��C|�3C~�3C�L�C�L�C�Y�C�ffC�ffC�ffC�s3C�Y�C�@ C�Y�C�ffC�L�C�&fC�@ C�Y�C�s3C�Y�C�33C�Y�C�ffC�Y�C�33C�Y�C�ffC�Y�C�@ C�Y�C�s3C�Y�C�L�C�ffC�L�C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�33C�&fC�33C�@ C�L�C�Y�C�L�C�&fC�33C�@ C�L�C�Y�C�L�C�33C�@ C�L�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�&fC�@ C�L�C�Y�C�L�C�&fC�@ C�L�C�@ C�33C�L�C�@ C�33C�L�C�ffC�Y�C�@ C�Y�C�@ C�33C�L�C�Y�C�L�C�33C�L�C�ffC�L�C�&fC�33C�@ C�L�C�Y�C�L�C�&fC�@ C�@ C�Y�C�ffC�L�C�&fC�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�L�C�L�C�L�C�@ C�33C�&fC�L�C�33C�L�C�@ C�33C�s3C�ffD &fD � D�D��D��D
FfD� DfD� D3D�3D` D  D�3D!�3D$y�D'FfD*�D,� D/��D2Y�D5fD7� D:&fD<��D?3DAffDC�fDEٚDHfDJ,�DLS3DNs3DP� DR��DU3DW` DY� D\  D^�3Da  Dcl�De��Dhs3DkfDm��Dp9�Dr�3DuffDw�3DzffD|y�D  D��fD��D�#3D�\�D���D��3D�� D�&fD�L�D�y�D���D�ٚD�3D�&fD�@ D�p D��3D��fD�ٚD���D��D�C3D�l�D�� D���D��3D�fD�FfD�y�D���D���D�	�D�<�D�p D��fD�� D� D�<�D�p D��3D�� D���D�33D�c3D���D��3D��fD�0 D�ffD�� D�ٚD��D�\�D��3D��fD�3D�FfDă3DŹ�D��fD�fD�L�D�|�Dˬ�D��3D��D�P D�|�Dѩ�D�� D���D�#3D�I�D�p Dؐ D٬�D��fD�ٚD��3D�3D�  D�6fD�P D�l�D㉚D�3D�� D��3D���D��D�C3D�l�D왚D��fD���D�&fD�VfD��D��3D��3D�&fD�\�D��3D��fD��fD�  D�VfD���D��fE x E�E�3E@ EٚEq�E3E� E6fE�fEa�E��E�3E3E	8 E
L�E\�E�3E�Ek3E^fE� E�Ep E�3E�3E� E,�Et�E��E ET�E��E � E!� E#<�E$��E%�fE&��E(( E)x E*�3E,3E-a�E.��E/��E0�fE21�E3y�E4��E63E7ffE8�fE9��E;fE<` E?��EB�fEE�3EH� EK�EO�ER�EU\�EXA�E[|�E^� Ea�fEd� Eg�Ej�fEn,�Eq)�EtI�Ewh Ez�fE}��E�{3E��fG�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�G�O�?L��G�O�?L��?fffG�O�?�  ?���?���?�ff?���G�O�?ٙ�?�33@��@��@,��@333@Fff@S33@fff@�  @�ff@�  @���@�33@���@�ff@�33@�  @ٙ�@陚@�33A��A  A  A��A��A$��A,��A4��A>ffAD��AK33AQ��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444441441444414114111114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ �@ �@ �@ {@ O@ !s@ (�@ /�@ 6�@ >@ FQ@ Q�@ _�@ n�@ {�@ �7@ ��@ ��@ �~@ �w@ �o@ �#@ �y@ � @@�@
@+�@8�@G�@V�@c�@p�@~K@��@��@��@��@@��@��@�(@�~@1@�@""@/�@=q@K�@X�@g@t@�@�\@��@��@�@�J@Ӡ@�@�@��@�@�@$.@33@B8@N�@Z�@i�@ww@�|@�u@��@�@�k@�o@׹@�T@�Y@ �@V@�@)�@5�@DD@S�@`�@m:@|?@�7@�0@�5@�-@�&@�@�@�@�e@�@o@ @-�@:�@H]@UU@b�@oF@�@��@��@�A@�9@��@��@��@��@�~@1@{@ �@/�@>@Lu@Z�@ff@r�@�@�\@�@�Y@��@�W@�O@��@�L@��@J@�@(G@4�@@�@O�@^5@j@v@�@�$@�(@�r@�@��@�@�@�@^@�@�@(�@7�@F�@SI@`B@oF@{�@��@��@��@�!@�w@�|@��@�m@�e@	�@	@	g@	-�@	:�@	FQ@	T�@	b�@	qS@	�@	��@	��@	�A@	��@	Ĝ@	��@	�/@	�@	��@
�@
*@
!s@
/�@
>@
M$@
Yn@
e	@
t@
�d@
��@
��@
�M@
�R@
ƨ@
Ӡ@
��@
�@
��@	�@�@'�@4�@@�@O�@\)@i!@x&@�|@�u@��@��@��@�@խ@�@�Y@ �@@�@'�@6�@DD@SI@a�@m�@y�@��@��@��@��@��@�*@��@�(@�q@@�@�@+�@8�@H]@T�@c�@p�@}�@��@��@��@��@��@@,@|?@�j@��@>@��@�@�@X�@��@��@<@��@�O@!s@m:@��@�@K�@��@׹@�@^5@��@�#@6@R�@�P@�@@=q@x�@��@��@6�@ww@�^@��@>�@�p@�@V@T�@�U@�T@)�@oF@�-@��@/�@r@��@�q@9X@z3@�j@��@>�@}�@��@��@?}@~�@�@�,@:@x&@�F@�e@ 1�@ o�@ ��@ �@@!,`@!l�@!�Y@!��@"-�@"oF@"�!@"�@#1�@#s_@#��@#� @$:@$z�@$�@$��@%>@%~K@%��@& �@&A�@&�@&�>@'�@'G�@'��@'��@(�@(T�@(��@(�#@) @)c�@)�4@)��@*+@*k.@*�@*�@@+/@+o�@+�-@+�@,6�@,v�@,��@,��@-5�@-t�@-��@-�Y@./�@.l�@.��@.�T@/g@/Yn@/�0@/є@0�@0Ji@0�+@0�>@1  @1>@1y�@1��@1�q@25�@2v@2�F@2�@37�@3x�@3�@3�E@4>@4�@4��@5@5E�@5�@5��@61@6I�@6��@6��@7V@7O�@7�\@7��@8�@8SI@8��@8��@9�@9R�@9�i@9�*@:�@:��@:��@;r�@<�@<��@=-�@=��@>/�@>�2@?Q�@?�;@@>�@@��@AWb@A�T@BqS@B��@C��@D�@D��@E�@E�z@F5@@Fȴ@G+@G��@HK@H�h@Ig�@I�q@J�+@J��@Kv�@L@L�@M�@M�@N<�@Nψ@O3�@Oȴ@P\)@Q�-@S�@TSI@U��@V�E@XV@Y�@[v@\A�@]�z@_j@`<�@a�0@b�@d@,@e��@f��@h7L@i��@j�}@lA�@m��@n�TG�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�G�O�G�O�@ G�O�@ @ �G�O�@ j@ @ �@ v@ �G�O�@ 1@ 	�@ �@ �@ @ �@ �@ @ *@ �@ B@ O@ [@ g@ !s@ #�@ &;@ (�@ +@ .l@ 0x@ 3�@ 6�@ :@ <@ ?}@ B�@ FQ@ I�@ M�@ P�@ SI@ VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�r�A�p�A�ƨA�7LA���Aʣ�Aʉ7A�p�A�ffA�\)A�Q�A�G�A�A�A�=qA�9XA�5?A�33A�/A�(�A�&�A�&�A�$�A�$�A�$�A�"�A�"�A��A��A��A��A��A��A��A��A��A��A�{A�bA�VA���A��/A�-A���A�9XA��^A��DA��RA�5?A�A���A�C�A��/A�dZA�jA�ƨA�A�33A���A�A�A�9XA�ĜA�?}A���A�XA��yA�XA� �A��A�O�A�VA��/A���A�5?A��9A�JA���A�VA��A���A�5?A���A�n�A�ȴA�XA���A�"�A���A�(�A���A��yA�VA��A�I�A��;A�z�A�$�A��^A��/A�x�A�C�A�ƨA�~�A��HA�=qA�`BA�A�Q�A��A�ZA�r�A���A���A�"�A��A�$�A���A�ƨA���A��+A�{A���A���A���A���A���A��PA�S�A�  A���A�VA�33A}�FA|A�A{|�Az�Aw��Av1'As7LAp��Ao��Am��AkAiVAh1'Ag��Af�jAdI�A`�\A_�^A]�TA[��AZ��AYhsAX��AWhsAU�ASG�AR�AO;dAJ�+AF��ADĜAC;dAA�A?oA=/A<1A:  A8��A7�wA7K�A77LA6�!A5S�A4z�A3\)A2JA0jA/33A.�HA/&�A.�A.��A.~�A-ƨA-C�A,Q�A)��A)VA'`BA%�A#�A#t�A#dZA#+A!�A Q�A bA��Al�A��A�A��AG�A�;A�A�+AA��A��A��AƨA�AK�A+AȴA�A��At�AK�A�A�\A{A��AXA
=A  AO�A
��A
z�A	�
A�A�DA�A��A;dAhsAn�A1'AdZA�/AƨA M�@�p�@��@�G�@�l�@�=q@�\)@�Q�@��@�hs@���@��@�u@�Z@�b@��@⟾@�^@�/@�9@�b@�C�@�z�@��
@ɡ�@�V@��;@�hs@�j@���@�I�@���@��@�J@���@��h@�A�@�ff@��^@��@�O�@���@�+@�5?@���@���@�b@�o@�@�7L@��/@�l�@��@�E�@���@��m@�S�@�=q@��`@�1@�+@��+@�7L@���@��+@��@��
@�"�@�@���@~@|�D@z~�@y�7@x��@v��@t�D@s��@q�@n�@lZ@j^5@hb@f�+@d9X@a��@`��@^ff@Z�!@Yhs@X��@V@U`B@S��@R��@P��@O��@O��@O�@M�-@K��@J��@IX@G�w@F�R@EO�@DZ@C�F@B=q@@��@?�;@?K�@>v�@=O�@<(�@;��@8Ĝ@8b@7+@6@41@3��@2�\@1��@01'@/|�@/�@.�R@-@,j@+C�@)��@)G�@(Q�@(b@'�w@&v�@%�@$�@$�D@#t�@#C�@"-@!�7@ bN@|�@��@��@��@�F@o@��@=q@G�@Ĝ@�@v�@�@p�@�
@dZ@o@^5@X@r�@�@ȴ@v�@�-@�@��@
��@
-@	��@	x�@	�@�9@bN@��@K�@�@V@�@�@�@��@S�@o@^5@��@ ��@ A�?��w?�v�?��?�C�?���?�K�?�`B?�!?�Ĝ?�\)?�R?�I�?��#?�P?�?�S�?���?�|�?�p�?�(�?�?ؓu?�K�?��T?�Z?ӕ�?ҏ\?��?�&�?�|�?�|�?�v�?�p�?��?�1?�ƨ?���?�7L?�Q�?���?Ƨ�?Ƨ�?�?Ł?�z�?��?�9X?��
?\?���?��7?��;?�;d?��?�O�?��?��?��?��?�~�?�~�?�^5?�^5?���?�^5?�?�dZA�`BA�`BA�K�A�M�A�ZA�^5A�dZA�ffA�ffA�hsA�hsA�ffA�dZA�\)A�dZA�ZA�^5A�`BA�bNA�ffA�ffA�n�A�r�A�t�A�v�A�t�A�n�A�n�A�p�A�n�A�r�A�p�A�n�A�hsA��A�x�A�I�A�E�A�9XA�&�A��A���AʸRAʬAʥ�Aʛ�Aʏ\AʁA�t�A�n�A�l�A�hsA�bNA�^5A�ZA�ZA�VA�S�A�Q�A�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�`BA�r�A�p�A�ƨA�7LA���Aʣ�Aʉ7A�p�A�ffA�\)A�Q�A�G�A�A�A�=qA�9XA�5?A�33A�/A�(�A�&�A�&�A�$�A�$�A�$�A�"�A�"�A��A��A��A��A��A��A��A��A��A��A�{A�bA�VA���A��/A�-A���A�9XA��^A��DA��RA�5?A�A���A�C�A��/A�dZA�jA�ƨA�A�33A���A�A�A�9XA�ĜA�?}A���A�XA��yA�XA� �A��A�O�A�VA��/A���A�5?A��9A�JA���A�VA��A���A�5?A���A�n�A�ȴA�XA���A�"�A���A�(�A���A��yA�VA��A�I�A��;A�z�A�$�A��^A��/A�x�A�C�A�ƨA�~�A��HA�=qA�`BA�A�Q�A��A�ZA�r�A���A���A�"�A��A�$�A���A�ƨA���A��+A�{A���A���A���A���A���A��PA�S�A�  A���A�VA�33A}�FA|A�A{|�Az�Aw��Av1'As7LAp��Ao��Am��AkAiVAh1'Ag��Af�jAdI�A`�\A_�^A]�TA[��AZ��AYhsAX��AWhsAU�ASG�AR�AO;dAJ�+AF��ADĜAC;dAA�A?oA=/A<1A:  A8��A7�wA7K�A77LA6�!A5S�A4z�A3\)A2JA0jA/33A.�HA/&�A.�A.��A.~�A-ƨA-C�A,Q�A)��A)VA'`BA%�A#�A#t�A#dZA#+A!�A Q�A bA��Al�A��A�A��AG�A�;A�A�+AA��A��A��AƨA�AK�A+AȴA�A��At�AK�A�A�\A{A��AXA
=A  AO�A
��A
z�A	�
A�A�DA�A��A;dAhsAn�A1'AdZA�/AƨA M�@�p�@��@�G�@�l�@�=q@�\)@�Q�@��@�hs@���@��@�u@�Z@�b@��@⟾@�^@�/@�9@�b@�C�@�z�@��
@ɡ�@�V@��;@�hs@�j@���@�I�@���@��@�J@���@��h@�A�@�ff@��^@��@�O�@���@�+@�5?@���@���@�b@�o@�@�7L@��/@�l�@��@�E�@���@��m@�S�@�=q@��`@�1@�+@��+@�7L@���@��+@��@��
@�"�@�@���@~@|�D@z~�@y�7@x��@v��@t�D@s��@q�@n�@lZ@j^5@hb@f�+@d9X@a��@`��@^ff@Z�!@Yhs@X��@V@U`B@S��@R��@P��@O��@O��@O�@M�-@K��@J��@IX@G�w@F�R@EO�@DZ@C�F@B=q@@��@?�;@?K�@>v�@=O�@<(�@;��@8Ĝ@8b@7+@6@41@3��@2�\@1��@01'@/|�@/�@.�R@-@,j@+C�@)��@)G�@(Q�@(b@'�w@&v�@%�@$�@$�D@#t�@#C�@"-@!�7@ bN@|�@��@��@��@�F@o@��@=q@G�@Ĝ@�@v�@�@p�@�
@dZ@o@^5@X@r�@�@ȴ@v�@�-@�@��@
��@
-@	��@	x�@	�@�9@bN@��@K�@�@V@�@�@�@��@S�@o@^5@��@ ��@ A�?��w?�v�?��?�C�?���?�K�?�`B?�!?�Ĝ?�\)?�R?�I�?��#?�P?�?�S�?���?�|�?�p�?�(�?�?ؓu?�K�?��T?�Z?ӕ�?ҏ\?��?�&�?�|�?�|�?�v�?�p�?��?�1?�ƨ?���?�7L?�Q�?���?Ƨ�?Ƨ�?�?Ł?�z�?��?�9X?��
?\?���?��7?��;?�;d?��?�O�?��?��?��?��?�~�?�~�?�^5?�^5?���?�^5?�?�dZA�`BA�`BA�K�A�M�A�ZA�^5A�dZA�ffA�ffA�hsA�hsA�ffA�dZA�\)A�dZA�ZA�^5A�`BA�bNA�ffA�ffA�n�A�r�A�t�A�v�A�t�A�n�A�n�A�p�A�n�A�r�A�p�A�n�A�hsA��A�x�A�I�A�E�A�9XA�&�A��A���AʸRAʬAʥ�Aʛ�Aʏ\AʁA�t�A�n�A�l�A�hsA�bNA�^5A�ZA�ZA�VA�S�A�Q�A�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�oB�hB�hB�B�1B�1B�1B�+B�1B�+B�+B�+B�%B�%B�%B�%B�%B�%B�%B�B�%B�B�%B�B�B�B�B�%B�B�%B�%B�B�%B�B�B�%B�B�B�B�B�B�Bv�BbNBW
BdZBbNBbNB^5BbNBcTBe`BhsBjBo�Bs�By�B|�B}�B|�B{�B~�B{�Bz�B|�B}�B~�B~�B~�B|�B�B�B�B�B�B�DB�7B�7B�PB�bB��B��B�oB�uB�\B�1B}�Bt�BhsBP�B@�B49B!�BuBJB+BB��B�B�`B�ZB�BȴB��B�?B��B��B��B�uB�Bx�Bq�BZBF�B<jB8RB49B1'B�BDBB
�B
�TB
��B
��B
��B
�B
v�B
n�B
W
B
K�B
;dB
(�B
�B
�B
JB	��B	�B	��B	��B	�qB	�-B	��B	��B	�hB	�\B	�%B	m�B	\)B	VB	D�B	9XB	/B	&�B	�B	uB	B��B�B��B�3B��B��B�=Bz�Bu�Bl�Bl�BhsBn�Bo�Bt�Bw�B� B�7B�DB�1B|�Br�BjBt�B�1B�JB�DB�JB�JB�=B�%B|�Bw�BiyBo�Bp�Bq�Bp�Bm�BjBjBiyBjBk�BhsBe`BcTB`BBXBR�BL�BN�BL�BI�BC�BG�BF�BD�BD�BB�BD�BD�BC�BC�BC�BB�BB�BA�B@�B?}B@�B>wB;dB;dB9XB:^B:^B9XB8RB5?B5?B7LB6FB5?B5?B2-B33B5?B33B2-B33B1'B1'B33B33B33B2-B2-B1'B?}B=qB=qBA�BF�BJ�BN�BXB�{B��B��B�1B}�B�bB�RB�B��B��B��B�XBĜB��B�#B�B	JB	9XB	K�B	aHB	z�B	� B	�=B	�JB	��B	��B	��B	��B	�B	�B	�9B	�FB	�}B	ƨB	��B	��B	��B	�B	�#B	�NB	�`B	�sB	�B	�B	��B	��B	��B	��B	��B
  B	��B
B
1B
	7B
DB
VB
\B
hB
{B
�B
�B
�B
�B
�B
�B
�B
!�B
%�B
&�B
&�B
+B
+B
-B
-B
0!B
1'B
1'B
1'B
1'B
33B
49B
5?B
7LB
9XB
:^B
;dB
<jB
>wB
?}B
@�B
A�B
A�B
B�B
C�B
C�B
G�B
G�B
G�B
I�B
I�B
J�B
K�B
M�B
N�B
O�B
O�B
O�B
Q�B
Q�B
S�B
T�B
VB
W
B
W
B
W
B
YB
ZB
ZB
ZB
[#B
[#B
]/B
]/B
_;B
_;B
`BB
`BB
bNB
cTB
cTB
cTB
e`B
e`B
ffB
ffB
iyB
hsB
iyB
k�B
k�B
k�B
l�B
m�B
n�B
o�B
p�B
q�B
r�B
s�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
x�B
y�B
y�B
y�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�=B
�DB
�DB
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
�B
�B
�B
�B
�B
�B
�B
�'B
�'B
�-B
�3B
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
�XB
�^B�hB�bB�uB�oB��B�uB�oB�hB�hB�oB�hB�hB�bB��B�\B�hB�uB�hB�oB�hB�oB�uB�hB�hB�bB�bB�hB�oB�bB�hB�hB�hB�bBz�Bz�B�=B�DB�DB�7B�B�+B�1B�7B�7B�1B�1B�+B�+B�1B�1B�+B�+B�+B�%B�+B�+B�+B�+B�%B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          B�FB�@B�@B��B�	B�
B�
B�B�B�B�B�B� B�B�B�B�B�B�B��B�B� B�B�B�B�B�B�
B�B�B�B�B�B�B�B�B�	B�
B�
B�B�B�Bv�Bb<BV�BdHBb<Bb=B^$Bb>BcDBeQBhdBjqBo�Bs�By�B|�B}�B|�B{�B~�B{�Bz�B|�B}�B~�B~�B~�B|�B�B�B�B�B�B�BB�5B�6B�OB�bB��B��B�pB�wB�^B�4B}�Bt�BhxBP�B@�B4?B!�B|BQB3BB��B�B�jB�eB�B��B��B�LB��B��B��B��B�.Bx�Bq�BZ.BF�B<{B8dB4LB1:B�BXB&B
�B
�iB
��B
��B
��B
�0B
v�B
n�B
W"B
K�B
;}B
)B
�B
�B
eB	��B	�B	�B	��B	��B	�JB	��B	��B	��B	�{B	�EB	m�B	\IB	V%B	D�B	9zB	/=B	'B	�B	�B	0B��B�B�B�XB��B��B�cB{Bu�Bl�Bl�Bh�Bn�Bo�Bt�Bw�B�+B�bB�pB�]B}Br�Bj�Bt�B�`B�yB�tB�{B�{B�oB�WB}!BxBi�Bo�Bp�Bq�Bp�Bm�Bj�Bj�Bi�Bj�Bk�Bh�Be�Bc�B`|BXKBS-BM	BOBM
BI�BC�BG�BF�BD�BD�BB�BD�BD�BC�BC�BC�BB�BB�BA�B@�B?�B@�B>�B;�B;�B9�B:�B:�B9�B8�B5�B5�B7�B6�B5�B5�B2{B3�B5�B3�B2}B3�B1xB1xB3�B3�B3�B2�B2�B1{B?�B=�B=�BA�BF�BKBO1BXiB��B��B�(B��B~\B��B��B�B�+B�"B�PB��B�B�\B۩B�9B	�B	9�B	LZB	a�B	{zB	��B	��B	��B	�&B	�YB	��B	��B	��B	��B	��B	��B	�6B	�dB	͌B	ϛB	ұB	��B	��B	�B	�0B	�FB	�VB	�~B	��B	��B	��B	��B	��B
 �B	��B
B
	%B

.B
=B
RB
[B
jB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
' B
(	B
(B
,'B
,*B
.9B
.;B
1QB
2ZB
2]B
2`B
2cB
4qB
5zB
6�B
8�B
:�B
;�B
<�B
=�B
?�B
@�B
A�B
B�B
B�B
C�B
D�B
D�B
IB
IB
I B
K/B
K2B
L<B
MEB
OTB
P]B
QfB
QiB
QkB
S{B
S~B
U�B
V�B
W�B
X�B
X�B
X�B
Z�B
[�B
[�B
[�B
\�B
\�B
^�B
^�B
`�B
`�B
bB
bB
dB
eB
e!B
e#B
g2B
g5B
h=B
h@B
kVB
jRB
k[B
mjB
mlB
moB
nxB
o�B
p�B
q�B
r�B
s�B
t�B
u�B
w�B
w�B
w�B
x�B
x�B
x�B
y�B
y�B
z�B
{�B
{�B
{�B
~B
~B
~B
B
�&B
�)B
�,B
�5B
�>B
�FB
�IB
�RB
�WB
�bB
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
�B
�B
�B
�&B
�2B
�?B
�EB
�XB
�cB
�pB
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
�B
�B
�B
�(B
�-B
�3B
�@B
�UB
�dB
�yB
��B
��B
��B
��B
��B
��B
�	B
�B
�3B
�HB
�_B
�nB
��B
��B
��B
��B
��B
��B
��B
� B�?B�9B�LB�FB�XB�LB�FB�?B�?B�FB�?B�?B�9B�XB�3B�?B�LB�?B�FB�?B�FB�LB�?B�@B�:B�:B�@B�GB�:B�@B�@B�@B�:Bz�Bz�B�B�B�B�B��B�B�
B�B�B�
B�
B�B�B�
B�B�B�B�B��B�B�B�B�B� B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812051500272021061413555620210614135556202106171313412021061713134120210617131341201812051500272021061413555620210614135556202106171313412021061713134120210617131341PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018120515002720181205150027  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018120515002720181205150027QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018120515002720181205150027QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151020210617131510IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                