CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-13T17:01:36Z creation      
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
_FillValue                 �  _X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                      HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                      HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � <Argo profile    3.1 1.2 19500101000000  20181013170136  20210617131505  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               '   'DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؆+����@؆+����11  @؆+�>��@؆+�>��@6�d2�n@6�d2�n�c�FI�l��c�FI�l�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@   @Fff@�ff@�ff@�ff@���@���A��A$��A@  Aa��A���A�  A�  A�  A���A�  A�  A�B ffB  B��BffB!33B(��B0ffB7��B@  BHffBPffBW��B`  BhffBo��Bx  B�33B�33B�33B�  B�ffB�ffB�  B�33B�ffB�33B�ffB�33B�33B�33B�33B�33B�ffB�33BǙ�B�  B�  B���Bؙ�Bܙ�B�ffB�ffB�33B�33B�33B�33B�  B���B���C�C33C�C33C
33C�C�fC�C  C��C33C33C  C  C�fC�fC!��C$�C&�C(  C*  C+�fC.33C0�C2  C433C633C8�C:L�C<�C=�fC@33CB  CD  CF33CH33CJffCL33CM�fCP�CR33CT  CU��CX  CZ33C\�C]�fC`�Cb�Cc��Cf  Ch33Cj�Ck��Cn�CpL�Cr33Ct�Cv  Cw�fCz33C|33C~  C�  C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��3C��fC��C��C��C��C��3C��C��C�  C�  C��3C��C��C�  C�  C��fC��C��C��C��C��C��C�  C�  C��C�  C�  C�  C��3C��C��C�  C��C��C��C�  C�  C��3C��fC��C�  C��3C��C��C�  C�&fC��C�  C��C��C�  C�&fC��C��3C��C��C�  C��fC�  C��C�&fC��C��3C��C��C��C��fC��C��C��C�&fC��C��3C�  C��C�&fC�  C��fC��3C�  C��C��C�&fC�&fC�33C��C��fC��3C�  C�  C��C��C�&fC�&fC�&fC��C��fC�  C��C��C��C��fC��3C�  C��fC�� Dy�D33DfD
�fD��D��D` D&fDٚDy�D&fD � D#S3D%�fD(ffD*��D-,�D/� D1ٚD4&fD6s3D8�fD;3D=s3D?ٚDB33DD� DG,�DI��DL&fDN�fDQ33DS� DV��DY9�D[�3D^�fDa` Dd�Df��Di� Dl33Dn�3Dqy�Dt  Dv�3Dy9�D{Y�D}�3D�33D�s3D���D�� D�3D�FfD�y�D��3D���D���D��D�)�D�C3D�Y�D�p D���D��fD���D��fD��fD��fD��D��D�0 D�FfD�` D�|�D��fD��3D�ٚD�	�D�9�D�ffD���D��fD��fD�  D�I�D�y�D���D��fD�� D� D�33D�Y�D�s3D��fD���D��fD�� D��D�#3D�<�D�P D�i�D�y�D�� D��fD��3D��3D���D�  D�6fD�P D�i�Dɉ�DʦfD��3D��fD�	�D�0 D�S3D�|�DҰ D��fD�	�D�9�D�c3Dؓ3D�� D��fD��D�L�D�vfDߣ3D���D�� D�3D�9�D�c3D�fD��D�ɚD��3D�3D��D�9�D�I�D�c3D�y�D��D�fD�3D��3D���D���D��fD�� D���D�� D���D��fD�|�D�vfD�s3E 8 E � E9�E�fE8 E�fE1�E,�E��E��E�E	��E
�3E� EA�EL�E��E�fE��Et�Et�E3E E� E��E�3E4�E@ E�fE��E �E"� E#��E$�fE&9�E'D�E(ٚE)�E*�E,p E-vfE/�E0 E1�fE2��E4fE5�E6�3E7vfE8�E:K3E;6fE<� E?�3EB� EF  EH��ELK3EO@ ERA�EUY�EXs3E[�3E^�fEa�fEe>fEh@ EkD�En��Eq�3Et�3Ew�3Ez� E}��E��3E�)�E���E�X E��3E�bfE��E�� E�,�E�W3>���>���>���>���>���>���?   >���>���>���>���>���>���?   >���?   ?   >���?��?333?333?fff?���?���?�33?���?�33@��@��@,��@9��@S33@`  @y��@�33@�  @���@�ff@�  @�  @ə�@�ff@�33@�33@���A��A��A��A��A$��A,��A4��A<��AD��AK33AT��A\��Ad��AnffAvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414441141441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               ?fff?�33@   @fff@�ff@�ff@�ff@���AffA��A,��AH  Ai��A���A�  A�  A�  A���A�  A�  A���BffB
  B��BffB#33B*��B2ffB9��BB  BJffBRffBY��Bb  BjffBq��Bz  B�33B�33B�33B�  B�ffB�ffB�  B�33B�ffB�33B�ffB�33B�33B�33B�33B�33B�ffB�33Bș�B�  B�  B���Bٙ�Bݙ�B�ffB�ffB�33B�33B�33B�33B�  B���C L�C��C�3C��C�3C
�3C��CffC��C� CL�C�3C�3C� C� CffC ffC"L�C$��C&��C(� C*� C,ffC.�3C0��C2� C4�3C6�3C8��C:��C<��C>ffC@�3CB� CD� CF�3CH�3CJ�fCL�3CNffCP��CR�3CT� CVL�CX� CZ�3C\��C^ffC`��Cb��CdL�Cf� Ch�3Cj��ClL�Cn��Cp��Cr�3Ct��Cv� CxffCz�3C|�3C~� C�@ C�@ C�Y�C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�33C�&fC�Y�C�Y�C�Y�C�L�C�33C�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�@ C�@ C�&fC�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�@ C�@ C�@ C�33C�Y�C�Y�C�@ C�Y�C�Y�C�L�C�@ C�@ C�33C�&fC�L�C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�@ C�Y�C�Y�C�@ C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�Y�C�33C�L�C�Y�C�L�C�&fC�L�C�Y�C�Y�C�ffC�L�C�33C�@ C�Y�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�ffC�ffC�s3C�Y�C�&fC�33C�@ C�@ C�L�C�Y�C�ffC�ffC�ffC�L�C�&fC�@ C�L�C�Y�C�L�C�&fC�33C�@ C�&fC�  D��DS3D&fDfDٚD��D� DFfD��D��DFfD � D#s3D&fD(�fD*��D-L�D/� D1��D4FfD6�3D8�fD;33D=�3D?��DBS3DD� DGL�DI��DLFfDN�fDQS3DT  DV��DYY�D\3D^�fDa� Dd9�Df��Di� DlS3Dn�3Dq��Dt@ Dv�3DyY�D{y�D}�3D�C3D��3D���D�� D�#3D�VfD���D��3D���D���D��D�9�D�S3D�i�D�� D���D��fD���D��fD��fD�fD��D�,�D�@ D�VfD�p D���D��fD��3D��D��D�I�D�vfD���D��fD�fD�0 D�Y�D���D���D��fD�  D�  D�C3D�i�D��3D��fD�ɚD��fD�  D��D�33D�L�D�` D�y�D���D�� D��fD��3D��3D��D�0 D�FfD�` D�y�Də�DʶfD��3D��fD��D�@ D�c3Dь�D�� D��fD��D�I�D�s3Dأ3D�� D�fD�)�D�\�DކfD߳3D���D�  D�#3D�I�D�s3D�fD��D�ٚD��3D�3D�,�D�I�D�Y�D�s3D���D��D�fD�3D��3D�ɚD���D��fD�� D���D�� D���D��fD���D��fD��3E @ E � EA�E�fE@ E�fE9�E4�E��E��E!�E	��E
�3E� EI�ET�E��E�fE��E|�E|�E3E E� E��E�3E<�EH E�fE��E ��E"� E#��E$�fE&A�E'L�E(�E)�E*�E,x E-~fE/	�E0 E1�fE2��E4fE5�E6�3E7~fE8�E:S3E;>fE<� E?�3EB� EF EH��ELS3EOH ERI�EUa�EX{3E[�3E^�fEa�fEeFfEhH EkL�En��Eq�3Et�3Ew�3Ez� E}��E��3E�-�E���E�\ E��3E�ffE��E�� E�0�E�[3?L��G�O�?L��G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�?L��?fffG�O�?L��G�O�G�O�?fff?���G�O�?���?�33?���?ٙ�?�33@ff@��@,��@9��@L��@Y��@s33@�  @���@�33@�  @���@�ff@�  @�  @ٙ�@�ff@�33A��AffA��A��A��A$��A,��A4��A<��AD��AL��AS33A\��Ad��Al��AvffA~ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414441141441141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               @ �@ %@ �@ *@ �@ #�@ *S@ /@ 5�@ >@ FQ@ Q�@ `B@ m�@ z�@ ��@ �0@ ��@ �~@ �&@ �*@ �#@ �@ �@@@ @-@9X@G�@V@c�@o�@~K@��@��@�A@��@�>@��@��@��@��@�@*@#�@0x@>�@K�@Yn@g@t�@�d@��@��@�M@�R@��@��@�T@��@��@�@�@&;@3�@A�@N�@[z@hs@x&@�|@�u@��@�r@�k@ȴ@׹@�@��@^@@O@(�@5�@C�@P�@`B@m�@z�@��@��@�5@�-@�&@�*@��@��@��@@b@ @,`@:@I@V�@e�@r@}�@��@��@�A@��@@є@ލ@��@��@�@@""@1'@>@I�@Yn@hs@uk@�d@�\@�U@�@��@��@Ӡ@�H@�L@�E@
�@�@&;@4�@A�@O0@\�@j@x&@��@�u@�m@�@�@�@�h@�@�@ �@�@�@)�@6�@DD@Q=@`�@n�@z�@��@��@��@�-@��@�|@�#@��@��@	j@	�@	�@	,`@	:@	F�@	V�@	dZ@	p�@	�@	�P@	�H@	�A@	��@	��@	��@	ލ@	�@	�~@
1@
*@
""@
1�@
>�@
K@
Z@
g�@
t@
��@
�@
�U@
�Y@
��@
��@
�C@
�H@
�@
��@�@6@&;@4�@A�@M$@\�@k.@x�@�+@�u@��@�@�@�o@�
@�T@�@  @V@�@+@8�@F�@SI@^5@l�@z�@��@��@�5@��@�2@��@�#@�@��@@o@g@+@9X@G�@S�@_�@��@5?@�d@��@
@k.@�R@@M�@��@ލ@%�@k�@�-@�q@7�@x�@�R@�~@7L@v@��@�e@5?@v�@��@�,@>�@�@ƨ@
�@P�@��@�@+�@v@��@
=@T�@�a@�@1�@y�@��@
=@P�@��@ψ@@Yn@��@��@""@c�@�4@�@&;@e�@�(@��@[@Yn@��@�7@J@I@�p@��@��@ 4�@ o�@ ��@ �@! @!\)@!��@!��@"�@"P�@"�i@"�C@#o@#S�@#�#@#��@${@$S�@$��@$խ@%o@%Q�@%�\@%�|@&J@&H]@&�|@&Ĝ@']@'=q@'z3@'��@'�@(,`@(hs@(�z@(��@)B@)V@)�u@)ψ@*�@*I@*�@*�2@*��@+;d@+x&@+�F@+�e@,33@,qS@,��@,�Y@-1'@-r�@-��@-�@.3�@.t@.�F@.�e@/5�@/uk@/��@/�@033@0qS@0�!@0�@1-�@1l�@1�M@1�`@2"�@2^�@2��@2խ@3�@3M$@3��@3��@3��@43�@4k�@4�(@4�h@5�@5C�@5t@5��@5�;@6�@6I@6~�@6��@6�@7"�@7X@7�\@7�J@7��@8e	@9@9oF@:�@:��@;&�@;��@<D�@<��@=a�@=��@>B�@>�(@?Wb@@]@@p�@A�@A�7@A��@B��@C�@C�J@D8�@D�Z@EX�@E�o@F@,@F�4@G^5@H
�@H{�@H�4@I��@J�@J�Z@KO@K��@L1�@L�[@M?}@M�;@NF�@N��@O|?@O��@Pt@Q�W@S@Tz�@U�!@W*S@Xm:@Y��@[�@\Z@]�r@_%@`X@aψ@c�@da�@e�@g�@hdZ@i��@k@lZ�@m�w@oo@p\�@q�c@s�@ta�@u�|@w�@xx&@x�T@ G�O�@ G�O�G�O�@ G�O�@ G�O�G�O�G�O�@ @ �G�O�@ G�O�G�O�@ �@ G�O�@ �@ %@ �@ 1@ 	�@ 
�@ �@ @ b@ o@ �@ �@ �@ �@ �@ �@ !s@ #�@ %�@ (�@ +@ -�@ 0x@ 3�@ 5�@ 8�@ <@ ?}@ B�@ FQ@ I�@ M$@ P�@ S�@ V�@ Z�@ ^5@ a�@ e�@ i!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA�ĜA�ƨA���A���A��
A��A��#A��/A��/A��;A��HA��TA��mA��mA���AؓuA��A�n�A�ZA�A�C�A��AՍPAӡ�Aҙ�A�VAϼjA�XA�bA͏\A��;A��HA��mA�O�A�n�A�^5A�\)A��#A���A�hsA��A�hsA�K�A�?}A�p�A��A��/A��A��A�n�A��jA�
=A���A�l�A�I�A�oA���A��DA�hsA�G�A��wA��+A�t�A�jA�I�A���A��A���A�9XA�z�A��A���A�I�A�
=A�"�A���A�1A��^A��A�A�A�n�A���A��A��uA�ffA�"�A���A��9A��PA�O�A���A��+A�$�A��A�XA�VA���A���A��
A�r�A�A��A��jA��A���A�~�A�XA�ƨA��wA�oA�VA��FA�"�A��TA�-A�z�A���A�ƨA��PA�K�A���A�bA�+A�5?AC�A};dAy`BAw33AvJAu�At�jAs��Ar�Ao��Am��Ak?}AiK�Ah~�Ag��Ag��Ag�AfbNAe�
Ad�yAd �A`^5A^�A[�A[oAZ�HAZ��AZ��AZ�uAZ�DAZAV��AQ��APVAOANVANAKAH��AF1'AB��AA�wA@�A?��A?�A>��A=K�A<Q�A:�RA:JA8Q�A7�A6��A6�uA4n�A3ƨA3�A3%A1��A0jA-��A,�A,�\A+A*ĜA)ƨA(��A(M�A'l�A&�A&1A$ĜA$jA$�A#O�A"�uA!�-A"1A!�7A �9A I�A
=A�;A��A�uA/A�\A�wAA^5Ap�A�HA�A��Az�A�A��A�TA��A^5A�-A�Ar�A  At�A�+AK�A
1'A	dZA	?}A	;dA	?}A��A�A�AM�A�FA�AE�Ax�A5?A �A ��A =q@��P@���@�Ĝ@�Q�@�-@�A�@��@�x�@���@�&�@թ�@���@�5?@�1@�~�@��@�I�@�@��@��F@�
=@�@���@��@�9X@���@��T@��
@�J@��D@�`B@��P@�$�@�p�@�&�@��7@�|�@�bN@��P@�-@�+@�33@�o@��@�7L@�t�@���@�ȴ@�ff@���@�(�@�v�@�7L@��@�  @�C�@�@���@�%@\)@|�/@z~�@v��@uV@t9X@q��@q7L@p  @m�@l��@kƨ@ko@j^5@h��@f�y@cƨ@aG�@_�;@^v�@\��@[��@Y�@X�`@W�@V@T�@S�
@Q�7@P�@O�@MV@Lz�@J�!@HA�@G��@E@D�j@C��@B-@A��@@�`@@  @>E�@=�@;dZ@:n�@9hs@8Q�@7|�@6V@6@5p�@4�@4�@2�\@1��@0Ĝ@/�@/
=@-�T@,1@*�\@)7L@(Q�@'��@';d@&v�@%�@%O�@$(�@#S�@"��@"n�@!��@ �9@ b@��@|�@5?@��@��@j@"�@J@��@��@�P@��@5?@��@�
@S�@�@^5@�7@G�@��@  @\)@ȴ@��@O�@��@�m@33@
^5@	��@	%@�;@;d@�R@�@/@�@��@"�@��@�!@�@&�?���?��?���?�r�?�ȴ?�9X?�7?�bN?�\)?��-?�I�?�^5?���?��?�`B?���?�o?�G�?�|�?ݲ-?��m?���?��#?���?��?�+?��T?�?}?��
?��?ҏ\?�M�?�G�?�&�?��;?�v�?�{?Ͳ-?�O�?��?�I�?˅?�^5?���?��?�1'?�
=?��T?�9X?�S�?��?�Ĝ?�  ?���?�p�?�j?��?�C�?���?��9?���?��?�7L?�7L?��#?��#?�~�?�?��?��m?��?�/?��?�V?��?�;dA���A���A���A�ȴA���A���A�ƨA�ƨA�ƨA�ĜA�ĜA�A�A�ĜA���A�AؾwA�A�A�A�A�A�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA���A���A���A���A���A���A���A���A��
A��
A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��;A��HA��HA��HA��HA��TA��TA��HA��`A��`A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               A�ƨA�ĜA�ƨA���A���A��
A��A��#A��/A��/A��;A��HA��TA��mA��mA���AؓuA��A�n�A�ZA�A�C�A��AՍPAӡ�Aҙ�A�VAϼjA�XA�bA͏\A��;A��HA��mA�O�A�n�A�^5A�\)A��#A���A�hsA��A�hsA�K�A�?}A�p�A��A��/A��A��A�n�A��jA�
=A���A�l�A�I�A�oA���A��DA�hsA�G�A��wA��+A�t�A�jA�I�A���A��A���A�9XA�z�A��A���A�I�A�
=A�"�A���A�1A��^A��A�A�A�n�A���A��A��uA�ffA�"�A���A��9A��PA�O�A���A��+A�$�A��A�XA�VA���A���A��
A�r�A�A��A��jA��A���A�~�A�XA�ƨA��wA�oA�VA��FA�"�A��TA�-A�z�A���A�ƨA��PA�K�A���A�bA�+A�5?AC�A};dAy`BAw33AvJAu�At�jAs��Ar�Ao��Am��Ak?}AiK�Ah~�Ag��Ag��Ag�AfbNAe�
Ad�yAd �A`^5A^�A[�A[oAZ�HAZ��AZ��AZ�uAZ�DAZAV��AQ��APVAOANVANAKAH��AF1'AB��AA�wA@�A?��A?�A>��A=K�A<Q�A:�RA:JA8Q�A7�A6��A6�uA4n�A3ƨA3�A3%A1��A0jA-��A,�A,�\A+A*ĜA)ƨA(��A(M�A'l�A&�A&1A$ĜA$jA$�A#O�A"�uA!�-A"1A!�7A �9A I�A
=A�;A��A�uA/A�\A�wAA^5Ap�A�HA�A��Az�A�A��A�TA��A^5A�-A�Ar�A  At�A�+AK�A
1'A	dZA	?}A	;dA	?}A��A�A�AM�A�FA�AE�Ax�A5?A �A ��A =q@��P@���@�Ĝ@�Q�@�-@�A�@��@�x�@���@�&�@թ�@���@�5?@�1@�~�@��@�I�@�@��@��F@�
=@�@���@��@�9X@���@��T@��
@�J@��D@�`B@��P@�$�@�p�@�&�@��7@�|�@�bN@��P@�-@�+@�33@�o@��@�7L@�t�@���@�ȴ@�ff@���@�(�@�v�@�7L@��@�  @�C�@�@���@�%@\)@|�/@z~�@v��@uV@t9X@q��@q7L@p  @m�@l��@kƨ@ko@j^5@h��@f�y@cƨ@aG�@_�;@^v�@\��@[��@Y�@X�`@W�@V@T�@S�
@Q�7@P�@O�@MV@Lz�@J�!@HA�@G��@E@D�j@C��@B-@A��@@�`@@  @>E�@=�@;dZ@:n�@9hs@8Q�@7|�@6V@6@5p�@4�@4�@2�\@1��@0Ĝ@/�@/
=@-�T@,1@*�\@)7L@(Q�@'��@';d@&v�@%�@%O�@$(�@#S�@"��@"n�@!��@ �9@ b@��@|�@5?@��@��@j@"�@J@��@��@�P@��@5?@��@�
@S�@�@^5@�7@G�@��@  @\)@ȴ@��@O�@��@�m@33@
^5@	��@	%@�;@;d@�R@�@/@�@��@"�@��@�!@�@&�?���?��?���?�r�?�ȴ?�9X?�7?�bN?�\)?��-?�I�?�^5?���?��?�`B?���?�o?�G�?�|�?ݲ-?��m?���?��#?���?��?�+?��T?�?}?��
?��?ҏ\?�M�?�G�?�&�?��;?�v�?�{?Ͳ-?�O�?��?�I�?˅?�^5?���?��?�1'?�
=?��T?�9X?�S�?��?�Ĝ?�  ?���?�p�?�j?��?�C�?���?��9?���?��?�7L?�7L?��#?��#?�~�?�?��?��m?��?�/?��?�V?��?�;dA���A���A���A�ȴA���A���A�ƨA�ƨA�ƨA�ĜA�ĜA�A�A�ĜA���A�AؾwA�A�A�A�A�A�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA���A���A���A���A���A���A���A���A��
A��
A��A��#A��#A��#A��/A��/A��/A��/A��/A��/A��;A��HA��HA��HA��HA��TA��TA��HA��`A��`A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�sB
�sB
�`B
�mB
�mB
�mB
�sB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�B
�B
��BDBJB\B{BoBPB#�B<jBT�B[#Bo�Bo�Bz�B�1B�7B��B�XBB��B�)B�yB��B��BB  BbB�B�B"�B,B/B6FB8RB;dBA�BF�BE�BE�BE�BG�BI�BI�BJ�BN�BP�BP�BP�BP�BR�BYB]/B^5BbNBcTBhsBjBhsBk�Bo�Bu�Bv�Bw�Bv�Bt�Bz�Bw�Bu�B`BBcTBiyBk�BjBhsBgmB^5BW
BT�BO�BL�B<jB!�BuBbB	7B�B�B��B~�Bq�BYBC�B7LB,B�B�BbB
=B
��B
�sB
�#B
��B
�jB
��B
��B
�DB
t�B
L�B
6FB
�B
JB	��B	��B	�B	�B	�TB	�;B	��B	ƨB	�-B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�1B	x�B	hsB	bNB	_;B	\)B	ZB	ZB	ZB	\)B	7LB	�B	VB	%B	B	  B�B�TB��B�wB�FB�B��B��B��B��B��B��B��B��B��B��B�bB�=B�1B�%B�B|�Bv�Bp�Bm�Bk�BjBjBcTBcTBbNB_;BcTBcTB_;B_;B_;BbNBcTBdZBr�Br�Bo�Bo�BiyBk�BiyBdZBe`Be`BaHB`BB^5B\)BZBVBR�BM�BK�BI�BG�BH�BG�BF�BD�BD�BD�BC�BA�B@�BA�B@�BG�BH�BO�BO�BO�BM�BK�BK�BI�BG�BF�BA�BE�BB�BA�BA�B?}B;dB<jB;dB:^B:^B8RB5?B33B49B5?B5?BB�BO�BS�Bl�Bx�B�%B�bB��B��B�'B�dB��B��B�
B�B��B	1B	)�B	49B	9XB	:^B	H�B	aHB	x�B	�uB	��B	��B	��B	��B	��B	�B	�B	�9B	�wB	ÖB	��B	��B	��B	�B	�/B	�BB	�TB	�fB	�sB	�B	�B	�B	��B	��B
B
B
B
B
%B
+B
DB
JB
\B
bB
bB
oB
�B
�B
�B
�B
 �B
!�B
"�B
&�B
&�B
'�B
(�B
+B
)�B
,B
-B
.B
1'B
1'B
33B
6FB
6FB
8RB
8RB
9XB
;dB
;dB
=qB
=qB
?}B
?}B
A�B
B�B
C�B
D�B
D�B
F�B
F�B
I�B
I�B
I�B
L�B
L�B
N�B
O�B
O�B
P�B
R�B
S�B
S�B
VB
W
B
XB
ZB
ZB
[#B
\)B
\)B
]/B
]/B
]/B
_;B
`BB
_;B
`BB
aHB
bNB
cTB
bNB
e`B
e`B
ffB
gmB
gmB
gmB
gmB
jB
k�B
k�B
l�B
l�B
m�B
m�B
n�B
o�B
p�B
p�B
q�B
r�B
r�B
s�B
t�B
u�B
u�B
v�B
w�B
x�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
|�B
}�B
� B
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
�JB
�PB
�VB
�\B
�\B
�hB
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
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�-B
�9B
�9B
�?B
�FB
�RB
�LB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�XB
�XB
�sB
�sB
�mB
�yB
�sB
�mB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�mB
�yB
�mB
�mB
�sB
�sB
�sB
�sB
�sB
�mB
�mB
�/B
�mB
�yB
�sB
�mB
�mB
�mB
�mB
�sB
�mB
�mB
�mB
�mB
�sB
�mB
�mB
�fB
�mB
�mB
�mB
�mB
�mB
�sB
�mB
�mB
�mB
�mB
�mB
�mB
�mB
�sB
�mB
�mB
�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               B
�LB
�LB
�:B
�GB
�GB
�HB
�NB
�HB
�IB
�IB
�IB
�JB
�JB
�KB
�LB
�^B
�~B
��B%B+B>B^BRB4B#�B<OBT�B[Bo�Bo�Bz�B�B�B��B�AB�xBͽB�B�dB��B��B �B��BOB�B�B"�B+�B/B66B8CB;UBA{BF�BE�BE�BE�BG�BI�BI�BJ�BN�BP�BP�BP�BP�BR�BYB]*B^0BbJBcPBhpBj|BhqBk�Bo�Bu�Bv�Bw�Bv�Bt�Bz�Bw�Bu�B`FBcXBi~Bk�Bj�BhyBgtB^<BWBUBO�BL�B<tB!�B�BmB	CB��B�B��BBq�BY%BC�B7[B,B�B�BsB
NB
��B
�B
�5B
��B
�}B
�B
��B
�YB
t�B
L�B
6[B
�B
`B	�B	��B	�B	�B	�lB	�TB	�B	��B	�GB	�B	��B	��B	��B	��B	��B	��B	��B	�{B	�PB	x�B	h�B	bnB	_\B	\JB	Z?B	Z@B	Z@B	\MB	7pB	�B	{B	JB	8B	 &B��B�{B��B��B�nB�CB�%B�B�B�B��B��B��B��B��B��B��B�kB�`B�TB�BB}Bv�Bp�Bm�Bk�Bj�Bj�Bc�Bc�Bb�B_pBc�Bc�B_rB_rB_sBb�Bc�Bd�Br�Br�Bo�Bo�Bi�Bk�Bi�Bd�Be�Be�Ba�B`�B^uB\jBZ^BVEBS4BNBL
BI�BG�BH�BG�BF�BD�BD�BD�BC�BA�B@�BA�B@�BG�BH�BP*BP+BP+BN BLBLBJ	BG�BF�BA�BE�BB�BA�BA�B?�B;�B<�B;�B:�B:�B8�B5�B3�B4�B5�B5�BB�BPLBTgBl�ByKB��B��B�*B�dB��B��B�B�~BיB�#B�oB	�B	*�B	4�B	9�B	; B	IYB	a�B	y�B	�$B	�EB	�[B	�?B	�sB	��B	��B	��B	�B	�BB	�dB	̙B	��B	��B	��B	�B	�#B	�8B	�MB	�]B	�rB	��B	��B	��B	��B
B
B
B
#B
+B
4B
PB
XB
mB
uB
xB
�B
�B
�B
�B
�B
!�B
"�B
#�B
(B
(B
)"B
*+B
,:B
+6B
-EB
.NB
/WB
2mB
2oB
4~B
7�B
7�B
9�B
9�B
:�B
<�B
<�B
>�B
>�B
@�B
@�B
B�B
C�B
EB
FB
FB
H!B
H$B
K9B
K;B
K>B
NSB
NVB
PeB
QmB
QpB
RyB
T�B
U�B
U�B
W�B
X�B
Y�B
[�B
[�B
\�B
]�B
]�B
^�B
^�B
^�B
`�B
bB
`�B
bB
cB
dB
e"B
dB
g4B
g6B
h?B
iIB
iLB
iNB
iQB
lfB
mnB
mqB
nzB
n}B
o�B
o�B
p�B
q�B
r�B
r�B
s�B
t�B
t�B
u�B
v�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
}B
}B
~B
B
B
B
�"B
�3B
�FB
�KB
�XB
�lB
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
��B
�B
�B
�"B
�.B
�;B
�FB
�RB
�YB
�^B
�qB
�xB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�!B
�(B
�DB
�YB
�oB
��B
��B
��B
��B
��B
��B
�
B
�%B
�4B
�KB
�aB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�!B
�*B
�?B
�PB
�^B
�hB
�jB
�LB
�LB
�FB
�RB
�LB
�FB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�FB
�RB
�FB
�FB
�LB
�LB
�LB
�MB
�MB
�GB
�GB
�	B
�GB
�SB
�MB
�GB
�GB
�GB
�GB
�MB
�HB
�HB
�HB
�HB
�NB
�HB
�HB
�AB
�HB
�IB
�IB
�IB
�IB
�OB
�IB
�IB
�JB
�JB
�JB
�JB
�JB
�PB
�KB
�KB
�KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810131701362021061413554520210614135545202106171313132021061713131320210617131313201810131701362021061413554520210614135545202106171313132021061713131320210617131313PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018101317013620181013170136  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101317013620181013170136QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018101317013620181013170136QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150520210617131505IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                