CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-26T06:00:27Z creation      
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
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �,   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181126060027  20210617131509  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               0   0DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؑ'���@ؑ'���11  @ؑ'��A@ؑ'��A@6��xB0�@6��xB0��c�@c�^J�c�@c�^J11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @Fff@�  @�  @�  @�33A��A��A&ffAA��A`  A���A���A���A���A�  A�  A�  A�  B ffB��B  BffB   B'��B0ffB933B@ffBHffBP��BX��B`��Bh��BpffBw��B�  B�33B�33B�33B�33B�33B�  B���B�33B�33B�33B�33B�  B�  B�  B�  B�  B�33B�  B�33B�  B�ffB�ffB�  B�ffB�33B癚B�  B�ffB�  B���B�33C L�C33C�C  C  C	�fC�fC�CL�C�C33C33C33C33C33C�C �C"  C$�C&  C(�C*�C,  C-�fC/�fC1�fC433C6�C8�C:L�C<�C=�fC@�CBL�CD33CF  CH�CJL�CL  CM��CP�CR33CT�CU�fCX  CZ�C\33C^L�C`�Ca��Cc�fCf  Ch�Cj33Cl  Cm��Co�fCq�fCt  Cv�Cx�CzL�C|�C}��C�  C��C��3C��fC�  C��C��C�  C��C��C��3C��C��C��3C��C��C��C��3C��fC�  C��C��C�  C��C�  C��3C��C��3C��fC��3C��C��C��C��fC��3C��C�&fC��C��fC��3C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C�  C��3C��fC��C��C�  C�&fC�&fC�&fC�&fC�33C��C��fC��fC��3C��3C�  C��C��C��C�33C��C��fC�  C��C�  C��fC��C��C��C�  C��C�  C��fC�  C��C�  C��3C��C��C��3C��C��C��C��3C��C�  C�  C�&fC�&fC��C�&fC��C��3C��C��C��C��fC�  C��C�  C��fC�  C�ٚC�&fC��C�ٚC��fC��fD   D � DfD��D�D�D	��D3D�fD  Ds3D�fDl�D��D` D�3D"Y�D$� D'l�D*  D,�fD/  D1�3D4,�D6�3D933D;�3D>33D@�fDCS3DE� DHs3DK  DM� DP  DRl�DT�3DWl�DY�fD\` D^ٚDaL�Dc� Df33Dh�fDk&fDm��Dp33Dr��Du33Dw��DzFfD|Y�D~�fD��fD��fD�<�D��3D���D��fD�6fD�i�D���D�� D�� D�	�D�0 D�S3D�p D��3D�� D��fD��3D�  D�#3D�L�D�i�D�� D��fD���D�fD�9�D�s3D��fD�ٚD��D�VfD��fD��fD��D�` D�� D��3D�0 D�|�D�ɚD�fD�ffD��fD�  D�FfD�� D��fD��D�i�D�� D��D�&fD�ffDã3D��fD��D�FfD�y�DɦfD��3D��fD��D�@ D�Y�D�vfDь�DҜ�Dө�DԳ3Dչ�D�� D��3D��3D��fD�� D�ٚD�� D�� D��fD��fD���D�3D�3D�  D�0 D�9�D�C3D�VfD�c3D�p D�s3D�y�D�|�D�|�D�3D��fD��D��D�3D���D�� D��fD�� D�� D�ɚD��3D��3D��fD��3D�fE fE ��E8 E�3E` E� E�3E�E� EFfE�fEfE33E	^fE
��E��Ed�E��E� E�3E^fEq�E~fE�E�E��E� E E�Ep Ec3EɚE!1�E"�E#y�E$�E%�E'S3E(�3E)�fE+;3E,33E-� E.� E00 E14�E2��E3��E5A�E6C3E7��E8��E:>fE;;3E<��E?� EBٚEF�EI�EL&fEO.fER� EU� EX�3E[� E^��Eb!�Ee!�Eh EkS3Enc3Eqq�Et��Ew�3Ez�fE}�3E�� E�, E���E�4 E���E�e�E� >���>���>���>���>L��>���>���>���>���>���>L��>L��>���>���>���>���>L��>���>���>���>���>���?   ?   ?��?333?L��?fff?���?�ff?���?ٙ�?�33@��@   @333@Fff@`  @l��@�  @���@�33@�  @���@�33@�  @ə�@�ff@�  @�  @���A��A	��A33A  A   A&ffA.ffA4��A>ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441441144411444141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ?L��?�33@   @fff@�  @�  @�  @�33A	��A��A.ffAI��Ah  A���A���A���A���A�  A�  A�  A�  BffB	��B  BffB"  B)��B2ffB;33BBffBJffBR��BZ��Bb��Bj��BrffBy��B�  B�33B�33B�33B�33B�33B�  B���B�33B�33B�33B�33B�  B�  B�  B�  B�  B�33B�  B�33B�  B�ffB�ffB�  B�ffB�33B虚B�  B�ffB�  B���B�33C ��C�3C��C� C� C
ffCffC��C��C��C�3C�3C�3C�3C�3C��C ��C"� C$��C&� C(��C*��C,� C.ffC0ffC2ffC4�3C6��C8��C:��C<��C>ffC@��CB��CD�3CF� CH��CJ��CL� CNL�CP��CR�3CT��CVffCX� CZ��C\�3C^��C`��CbL�CdffCf� Ch��Cj�3Cl� CnL�CpffCrffCt� Cv��Cx��Cz��C|��C~L�C�@ C�L�C�33C�&fC�@ C�Y�C�L�C�@ C�L�C�L�C�33C�L�C�L�C�33C�Y�C�L�C�L�C�33C�&fC�@ C�Y�C�L�C�@ C�L�C�@ C�33C�Y�C�33C�&fC�33C�L�C�Y�C�L�C�&fC�33C�L�C�ffC�L�C�&fC�33C�33C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�L�C�Y�C�Y�C�L�C�@ C�33C�&fC�L�C�L�C�@ C�ffC�ffC�ffC�ffC�s3C�L�C�&fC�&fC�33C�33C�@ C�L�C�L�C�Y�C�s3C�L�C�&fC�@ C�L�C�@ C�&fC�L�C�Y�C�Y�C�@ C�Y�C�@ C�&fC�@ C�L�C�@ C�33C�L�C�L�C�33C�L�C�Y�C�L�C�33C�L�C�@ C�@ C�ffC�ffC�L�C�ffC�Y�C�33C�L�C�Y�C�L�C�&fC�@ C�Y�C�@ C�&fC�@ C��C�ffC�Y�C��C�&fC�&fD   D � D&fD��D,�D9�D	��D33D�fD  D�3DfD��D�D� D�3D"y�D%  D'��D*  D,�fD/@ D1�3D4L�D6�3D9S3D;�3D>S3D@�fDCs3DF  DH�3DK  DM� DP  DR��DU3DW��DZfD\� D^��Dal�Dc� DfS3Dh�fDkFfDm��DpS3DrٚDuS3DwٚDzffD|y�DfD��fD�fD�L�D��3D���D�fD�FfD�y�D���D�� D�� D��D�@ D�c3D�� D��3D�� D��fD��3D� D�33D�\�D�y�D�� D��fD���D�fD�I�D��3D��fD��D�)�D�ffD��fD��fD�)�D�p D�� D��3D�@ D���D�ٚD�&fD�vfD��fD� D�VfD�� D��fD�,�D�y�D�� D���D�6fD�vfDó3D��fD��D�VfDȉ�DɶfD��3D�fD�,�D�P D�i�DІfDќ�DҬ�Dӹ�D��3D�ɚD�� D��3D��3D��fD�� D��D�� D�� D��fD�fD�	�D�3D�#3D�0 D�@ D�I�D�S3D�ffD�s3D� D�3D쉚D��D��D�3D�fD��D��D�3D���D�� D��fD�� D�� D�ٚD��3D��3D��fD�3D�fE fE ��E@ E�3Eh E� E�3E!�E� ENfE�fEfE;3E	ffE
��E��El�E��E� E�3EffEy�E�fE�E�E��E� E E	�Ex Ek3EњE!9�E"!�E#��E$�E%�E'[3E(�3E)�fE+C3E,;3E-� E.� E08 E1<�E2��E3ɚE5I�E6K3E7ɚE8ɚE:FfE;C3E<��E?� EB�EF	�EI!�EL.fEO6fER� EU� EX�3E[� E^��Eb)�Ee)�Eh Ek[3Enk3Eqy�Et��Ew�3E{fE}�3E�� E�0 E���E�8 E���E�i�E� G�O�G�O�G�O�G�O�?333G�O�G�O�G�O�?L��G�O�G�O�?333?L��G�O�G�O�G�O�?333?L��G�O�G�O�G�O�?fffG�O�?�  ?���?���?�ff?�33?���?�ff@ff@��@��@,��@@  @S33@fff@�  @�ff@�  @���@�33@�  @���@�33@�  @ٙ�@�ff@�  A   AffA��A��A33A   A(  A.ffA6ffA<��AFffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414441441144411444141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  @ @ %@ �@ *@ O@ ""@ (�@ 0x@ 7L@ >@ F�@ R�@ _�@ m�@ {�@ �7@ ��@ ��@ �~@ �&@ ��@ �#@ �m@ ��@@@
@-@<@H]@V@dZ@r@�@�P@�H@��@��@�>@��@ލ@�4@��@�@�@"�@0x@>@K�@X�@ff@t@��@�\@��@��@�@��@��@�@��@��@
�@�@%�@4�@@�@M$@\�@k�@x�@��@��@�m@�f@�@�@�@�`@�@^@@�@*S@7L@D�@Q�@`B@m:@{�@�7@�0@�(@��@�w@�*@�#@��@��@@b@g@.l@;d@G�@V@e	@p�@|�@��@��@��@�9@@��@�;@�@��@v@�@""@0x@>�@K@Wb@e�@s_@��@�@��@��@�@Ĝ@Ӡ@��@�@@�9@
=@B@&;@33@A�@O0@[z@j@x&@�p@�$@�@��@�@�@�
@�@�@  @V@O@(G@7�@C�@P�@^�@m�@|?@�7@��@�(@�-@�2@�|@�@�m@�@	j@	@	g@	,`@	:@	G�@	UU@	b�@	p�@	~K@	��@	�H@	�A@	��@	�>@	є@	�;@	�4@	�,@
%@
@
"�@
0x@
=q@
M$@
Z�@
hs@
v@
�p@
�@
��@
�M@
��@
�J@
Ӡ@
��@
�@
��@�@�@$.@33@A�@N�@Z�@j@x�@�|@��@��@�@�^@�c@׹@�@�@ �@V@�@)�@7�@D�@Q=@`B@m:@z�@��@�<@��@��@��@�@�#@�y@�q@@@ @,`@8�@G�@SI@e	@r@|?@��@�<@�A@��@�>@є@�;@i!@�@��@3�@ww@�^@�E@B8@�|@�c@J@Q=@�0@��@""@g@�@�e@7�@|�@�2@v@I�@�@խ@O@a�@�A@�@/�@r@��@��@>@��@�J@1@K@��@��@*@Z@�@�@'�@l�@�-@��@0x@uk@��@�Q@D�@��@��@@P�@�i@�7@�@M$@��@�@�@D�@��@�@��@ 6�@ t�@ �9@ ��@!/�@!n�@!�f@!��@".l@"qS@"��@"�e@#8�@#|?@#��@$�@$I�@$�\@$Ӡ@%�@%_�@%��@%�@&4�@&|?@&��@'
=@'O�@'�0@'��@(!s@(hs@(�@(��@)4�@)x�@)�j@)��@*@,@*�@*Ĝ@+�@+D�@+�@+��@,  @,<@,x�@,�9@,�@@-'�@-`A@-�<@-�7@.�@.A�@.x�@.�@.�@/�@/UU@/�P@/�W@/��@07L@0qS@0��@0�@1[@1V@1��@1�@2j@2:�@2r�@2��@2��@3�@3O�@3��@3�w@3�q@4.l@4ff@4�a@4�
@5@5I�@5~�@5�@5�@6-@6g�@6��@6�T@7""@7`�@7�m@7��@8�@8\�@8�@8�/@9	@9��@:�@:�T@;�@;��@<S�@<��@=G�@=��@>s_@>��@?[z@@�@@v@A�@A�@B%�@B�@C,`@C�#@D-@Dƨ@E)�@E��@F\�@Fƨ@GdZ@H�@Ho�@I@Ix�@JO@J��@K,`@K��@LB�@L�-@MV@M��@Ng@N�O@Ov�@O�@P�d@Q��@S"�@T{�@U�|@W�@Xe�@Y�7@[%�@\r@]@_J@`{�@a�>@cj@dg�@e�F@g@hYn@i�@k6@lV�@m�@o{@pqS@q��@s*@tdZ@u��G�O�G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ G�O�G�O�@ ^@ G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�@ �G�O�@ j@ @ �@ v@ %@ �@ �@ 
�@ �@ �@ @ @ @ *@ �@ B@ O@ [@ g@ ""@ $.@ &;@ (�@ +@ -�@ /�@ 33@ 5�@ 8�@ :�@ >�@ @�@ DD@ F�@ Ji@ M$@ Q=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aͩ�A͕�A͝�Aʹ9A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA���A���A�A�A;wAͼjAͼjAͼjAͼjAͼjAͼjA;wAͼjA͍PA̗�A�ƨA�`BA�r�A��A�ƨA�v�A���A�r�A�ĜA��A�v�A�M�A�A�^5A��A��-A��A��A��A�p�A��mA��7A�{A�r�A�VA��A��hA�G�A��^A�x�A��A��hA�G�A���A� �A��A�ƨA���A�l�A�`BA�bA��A��FA�z�A�K�A���A�~�A�`BA� �A�&�A�/A�r�A��HA��A�K�A���A���A��A��A��`A�O�A�Q�A���A��A�
=A�%A�ƨA�\)A�-A�M�A�1'A��A��A�%A�33A�jA��A�$�A�A}K�A|bNA{ƨAzM�Aw��AuXAq;dAm��Al^5Ak;dAiVAf��Ac`BAa7LA_��A^�A^{A]x�A\��A\�RA\��A\~�AZ��AXA�AT  ARI�AQƨAP��AN=qAL�RALVAK��AI�TAH=qAG�7AFȴAFAD1'ABv�A@�\A?XA=7LA:��A9��A8�A8n�A65?A5VA4��A3`BA2=qA1x�A.��A-��A,  A(�\A&�+A$A�A#t�A"�!A!C�A�A�A��A�hAt�AG�AbNA��A(�A?}AĜA �AA�AO�AVA��AZA �A�A��A�AG�A;dA"�A
=A��AM�A1'A$�A �AA��Ax�A"�A
=A�AȴAQ�AJA�A�
A��AS�AQ�Ap�A��AbNAbA��A��A�A�A
z�A	�-A1'A�A�PA��A�A��A�RA��AQ�A �A�Ap�A�TA �`@���@���@�?}@�M�@�Z@��;@��@�v�@�M�@���@�j@@�@�w@�p�@�b@�t�@��@��@��`@θR@���@�p�@��7@��w@��R@�?}@�&�@���@��7@���@���@�|�@��^@�|�@�-@�p�@�@�hs@��@�V@��@�G�@�  @��y@��^@���@�Ĝ@���@�I�@��;@��@��@�V@�t�@���@�Q�@�1@���@��@�A�@���@�?}@���@��@�=q@�x�@��@~�y@}�T@}V@zJ@y�^@w;d@u�T@u`B@rn�@q&�@o�@o;d@n�+@m�@j�@i��@i�^@h�u@fȴ@eV@c�
@c@_�@]p�@["�@Xr�@U��@U/@T(�@R��@Q7L@PQ�@O
=@N@Mp�@L1@J�\@H�`@Gl�@F@Dz�@C�@Bn�@A�@?�P@=�T@<�@;��@:^5@9%@7�;@6�+@5�@4�@3o@1%@0��@0  @/
=@-��@,�D@+dZ@*�!@)7L@(�9@(bN@'�@&V@$�@$Z@#�F@"��@!�#@!7L@ 1'@l�@�@@�@�@�!@=q@Ĝ@r�@A�@��@�-@/@1@S�@=q@-@X@&�@�u@�@ȴ@$�@�@z�@�F@
��@
�@
J@	��@	%@Ĝ@A�@�;@�P@��@�+@$�@p�@9X@�@33@�!@-@�#@�7@ ��@  �?���?�(�?��?�ȴ?��j?��?��?�;d?�R?�j?ꟾ?��?�ff?��?�9X?�S�?�7?��;?���?�V?ڟ�?���?�1'?֧�?և+?��/?ӶF?ӕ�?�-?��`?Ͼw?�;d?��?�p�?��m?˅?�"�?���?�x�?�Q�?�1'?Ǯ?�
=?�?�z�?Õ�?�o?\?��`?�A�?�  ?�|�?��h?�1?�ƨ?�~�?�=q?��#?���?���?�X?�x�?�x�?��^?��?�~�?��H?�dZ?�1?��?�p�?�{Aͩ�Aͩ�Aͧ�Aͩ�Aͩ�Aͧ�Aͩ�Aͩ�Aͧ�AͬAͮAͩ�AͶFAͮAͰ!Aʹ9AͶFAͰ!AͮAͰ!Aͧ�A͝�A͙�A͙�A͗�A͗�A͕�A͕�A͑hA͗�A͙�A͟�A͟�A͟�A͝�A͡�A͸RA�ĜA�ȴA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Aͩ�A͕�A͝�Aʹ9A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA�ĜA���A���A�A�A;wAͼjAͼjAͼjAͼjAͼjAͼjA;wAͼjA͍PA̗�A�ƨA�`BA�r�A��A�ƨA�v�A���A�r�A�ĜA��A�v�A�M�A�A�^5A��A��-A��A��A��A�p�A��mA��7A�{A�r�A�VA��A��hA�G�A��^A�x�A��A��hA�G�A���A� �A��A�ƨA���A�l�A�`BA�bA��A��FA�z�A�K�A���A�~�A�`BA� �A�&�A�/A�r�A��HA��A�K�A���A���A��A��A��`A�O�A�Q�A���A��A�
=A�%A�ƨA�\)A�-A�M�A�1'A��A��A�%A�33A�jA��A�$�A�A}K�A|bNA{ƨAzM�Aw��AuXAq;dAm��Al^5Ak;dAiVAf��Ac`BAa7LA_��A^�A^{A]x�A\��A\�RA\��A\~�AZ��AXA�AT  ARI�AQƨAP��AN=qAL�RALVAK��AI�TAH=qAG�7AFȴAFAD1'ABv�A@�\A?XA=7LA:��A9��A8�A8n�A65?A5VA4��A3`BA2=qA1x�A.��A-��A,  A(�\A&�+A$A�A#t�A"�!A!C�A�A�A��A�hAt�AG�AbNA��A(�A?}AĜA �AA�AO�AVA��AZA �A�A��A�AG�A;dA"�A
=A��AM�A1'A$�A �AA��Ax�A"�A
=A�AȴAQ�AJA�A�
A��AS�AQ�Ap�A��AbNAbA��A��A�A�A
z�A	�-A1'A�A�PA��A�A��A�RA��AQ�A �A�Ap�A�TA �`@���@���@�?}@�M�@�Z@��;@��@�v�@�M�@���@�j@@�@�w@�p�@�b@�t�@��@��@��`@θR@���@�p�@��7@��w@��R@�?}@�&�@���@��7@���@���@�|�@��^@�|�@�-@�p�@�@�hs@��@�V@��@�G�@�  @��y@��^@���@�Ĝ@���@�I�@��;@��@��@�V@�t�@���@�Q�@�1@���@��@�A�@���@�?}@���@��@�=q@�x�@��@~�y@}�T@}V@zJ@y�^@w;d@u�T@u`B@rn�@q&�@o�@o;d@n�+@m�@j�@i��@i�^@h�u@fȴ@eV@c�
@c@_�@]p�@["�@Xr�@U��@U/@T(�@R��@Q7L@PQ�@O
=@N@Mp�@L1@J�\@H�`@Gl�@F@Dz�@C�@Bn�@A�@?�P@=�T@<�@;��@:^5@9%@7�;@6�+@5�@4�@3o@1%@0��@0  @/
=@-��@,�D@+dZ@*�!@)7L@(�9@(bN@'�@&V@$�@$Z@#�F@"��@!�#@!7L@ 1'@l�@�@@�@�@�!@=q@Ĝ@r�@A�@��@�-@/@1@S�@=q@-@X@&�@�u@�@ȴ@$�@�@z�@�F@
��@
�@
J@	��@	%@Ĝ@A�@�;@�P@��@�+@$�@p�@9X@�@33@�!@-@�#@�7@ ��@  �?���?�(�?��?�ȴ?��j?��?��?�;d?�R?�j?ꟾ?��?�ff?��?�9X?�S�?�7?��;?���?�V?ڟ�?���?�1'?֧�?և+?��/?ӶF?ӕ�?�-?��`?Ͼw?�;d?��?�p�?��m?˅?�"�?���?�x�?�Q�?�1'?Ǯ?�
=?�?�z�?Õ�?�o?\?��`?�A�?�  ?�|�?��h?�1?�ƨ?�~�?�=q?��#?���?���?�X?�x�?�x�?��^?��?�~�?��H?�dZ?�1?��?�p�?�{Aͩ�Aͩ�Aͧ�Aͩ�Aͩ�Aͧ�Aͩ�Aͩ�Aͧ�AͬAͮAͩ�AͶFAͮAͰ!Aʹ9AͶFAͰ!AͮAͰ!Aͧ�A͝�A͙�A͙�A͗�A͗�A͕�A͕�A͑hA͗�A͙�A͟�A͟�A͟�A͝�A͡�A͸RA�ĜA�ȴA�ƨA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbNBaHBaHBbNB`BB`BBaHBaHBaHBaHBaHBaHBaHBbNBbNBbNBbNBbNBcTBbNBe`Bk�Bk�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bm�BhsBdZB`BBVBE�Bl�BW
BhsBq�Br�Bm�Bp�B�B�B�+B�+B�%B�B�1B�7B�=B�=B�=B�=B�+B�PB�=B�7B�1B�B�B� B~�B}�Bw�Bw�Bu�Bt�Bp�B`BBW
BQ�BG�B;dB9XB33B.B$�BoBB�B�fB�)B��BƨB�?B��B��B�+Bz�BhsBL�BA�B;dB)�B�BVB
�B
��B
��B
�!B
��B
��B
�7B
x�B
jB
`BB
XB
I�B
7LB
0!B
+B
"�B
�B
B	�B	��B	�wB	�3B	�B	��B	�bB	|�B	u�B	o�B	k�B	e`B	aHB	]/B	ZB	YB	W
B	G�B	1'B	�B	hB	
=B��B�B�B�B��B�B��B�B�B�B�`B�mB�BB�B��BȴBÖB�wB�^B�B�B��B��B��B��B�+B�Bw�BffB]/BO�BK�BD�B8RB(�B#�B �B�B�B�B�BuBbB\BPBJBPBPBVB\BhBuB{B{B�B�B�B�B�B�B"�B%�B&�B'�B'�B(�B,B/B33B49B49B5?B9XB;dB;dB;dB;dB=qB?}BA�BB�BB�BB�BA�BA�BA�B@�B@�B=qB=qB<jB8RB;dB9XB8RB8RB7LB7LB7LB6FB49B2-B2-B/B1'B0!B0!B1'B/B.B.B/B.B-B.B0!B33B8RB7LB9XB;dB;dBv�BiyBZBdZBjBl�Bn�B�B�'B�B��B�`B�`B��B��B��B��B	B��B	DB	PB	\B	!�B	,B	J�B	VB	ffB	u�B	~�B	�B	�DB	�VB	��B	��B	�B	�9B	��B	ȴB	��B	��B	��B	�)B	�5B	�fB	�mB	�B	�B	�B	��B	��B	��B	��B
B
B
%B
1B
+B

=B
JB
PB
VB
VB
hB
uB
{B
uB
�B
�B
�B
�B
�B
�B
 �B
#�B
'�B
)�B
+B
,B
-B
.B
.B
/B
1'B
2-B
33B
5?B
6FB
8RB
:^B
;dB
>wB
=qB
?}B
A�B
A�B
C�B
D�B
D�B
F�B
F�B
H�B
I�B
I�B
K�B
N�B
N�B
O�B
P�B
Q�B
Q�B
S�B
S�B
W
B
VB
W
B
XB
YB
ZB
[#B
ZB
[#B
]/B
]/B
^5B
^5B
_;B
`BB
bNB
dZB
dZB
dZB
e`B
ffB
ffB
iyB
iyB
iyB
jB
k�B
m�B
l�B
m�B
m�B
n�B
p�B
q�B
q�B
r�B
r�B
t�B
u�B
u�B
u�B
v�B
w�B
v�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
|�B
{�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�7B
�7B
�=B
�DB
�DB
�JB
�\B
�VB
�hB
�oB
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
�B
�B
�B
�B
�B
�!B
�!B
�'B
�3B
�9B
�?B
�FB
�FB
�FB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�^B
�^BaHBaHBaHBaHBaHBbNBbNBaHBbNBaHBcTBaHBcTBbNBdZBcTBaHB_;BaHBaHBaHBbNBbNBcTBaHBaHBaHBbNBbNBaHBaHBaHBbNBaHBaHBcTBbNBaHB`BBaHB`BB`BB`BB`BBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHBaHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Bb&Ba Ba Bb&B`B`Ba!Ba"Ba"Ba"Ba#Ba#Ba$Bb*Bb+Bb,Bb,Bb-Bc3Bb.BeABkfBkgBlnBlnBloBloBmvBmwBmwBmxBmyBmyBn�Bn�Bn�Bm{Bh^BdFB`.BU�BE�BlwBV�Bh`Bq�Br�BmBp�B�B�	B�B�B�B�B�#B�*B�0B�1B�1B�2B� B�FB�3B�.B�(B�B�
B�B~�B}�Bw�Bw�Bu�Bt�Bp�B`?BWBQ�BG�B;cB9XB34B.B$�BqBB�B�iB�-B��BƭB�DB��B��B�2Bz�Bh{BL�BA�B;mB*B�B`B
�B
��B
��B
�-B
��B
��B
�DB
x�B
j�B
`QB
XB
I�B
7\B
01B
+B
"�B
�B
$B	�B	��B	��B	�GB	�B	��B	�wB	}B	u�B	o�B	k�B	ewB	a`B	]GB	Z6B	Y0B	W$B	G�B	1BB	�B	�B	
YB�
B��B�B��B��B��B��B��B��B�B�B�B�dB�2B�B��BúB��B��B�@B�-B�B��B��B��B�SB�GBw�Bf�B]XBP	BK�BD�B8}B)!B$B �B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B#
B&B'#B(+B(,B)2B,EB/XB3qB4xB4xB5B9�B;�B;�B;�B;�B=�B?�BA�BB�BB�BB�BA�BA�BA�B@�B@�B=�B=�B<�B8�B;�B9�B8�B8�B7�B7�B7�B6�B4�B2}B2}B/lB1xB0sB0sB1zB/nB.hB.hB/pB.iB-dB.jB0xB3�B8�B7�B9�B;�B;�Bw(Bi�BZ�Bd�Bj�Bl�BoB��B��B��B�NB��B��B�FB�UB�XB��B	�B�yB	�B	�B	�B	"iB	,�B	KeB	V�B	gB	vpB	�B	��B	��B	�B	�tB	��B	��B	��B	�KB	�B	͛B	��B	��B	� B	�B	�CB	�LB	�gB	�qB	�B	��B	��B	��B	��B
�B
 B
"B
	1B
.B
CB
RB
[B
dB
fB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
%B
)#B
+2B
,;B
-DB
.MB
/VB
/YB
0bB
2qB
3{B
4�B
6�B
7�B
9�B
;�B
<�B
?�B
>�B
@�B
B�B
B�B
EB
FB
FB
H$B
H'B
J6B
K?B
KBB
MRB
PgB
PiB
QrB
R{B
S�B
S�B
U�B
U�B
X�B
W�B
X�B
Y�B
Z�B
[�B
\�B
[�B
\�B
^�B
^�B
_�B
_�B
a B
b
B
dB
f'B
f)B
f,B
g4B
h<B
h?B
kUB
kWB
kZB
lbB
mkB
oyB
nvB
o~B
o�B
p�B
r�B
s�B
s�B
t�B
t�B
v�B
w�B
w�B
w�B
x�B
y�B
x�B
y�B
z�B
z�B
{�B
{�B
{�B
}B
B
~B
B
�!B
�*B
�-B
�5B
�DB
�MB
�SB
�_B
�kB
�wB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�(B
�4B
�9B
�FB
�SB
�WB
�kB
�rB
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
�B
�B
�B
�B
�0B
�5B
�BB
�\B
�lB
�{B
��B
��B
��B
��B
��B
�B
�B
�+B
�CB
�QB
�`B
�|B
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�0B
�?B
�OBa Ba Ba Ba Ba Bb&Bb&Ba Bb&Ba Bc,Ba Bc,Bb&Bd2Bc,Ba B_Ba Ba Ba Bb&Bb&Bc,Ba Ba Ba Bb&Bb&Ba Ba Ba Bb&Ba Ba Bc,Bb&Ba!B`Ba!B`B`B`B`Ba!Ba!Ba!Ba"Ba"Ba"Ba"Ba"Ba"Ba"Ba"Ba#Ba#Ba#Ba#Ba#G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201811260600272021061413555420210614135554202106171313362021061713133620210617131336201811260600272021061413555420210614135554202106171313362021061713133620210617131336PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018112606002720181126060027  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112606002720181126060027QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018112606002720181126060027QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150920210617131509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                