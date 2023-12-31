CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-05-08T22:00:50Z creation      
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
resolution        =���   axis      Z        $  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	  _�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  h�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  �,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	  �D   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  �L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	 d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $ l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $ 8�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	 \�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $ e�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 	 ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $ ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Ä   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � ۄArgo profile    3.1 1.2 19500101000000  20190508220050  20210617131518  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               D   DDD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ط��ǵ@ط��ǵ11  @ط���@ط���@5Կ	���@5Կ	����c�:���c�:��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  ?�33@@  @�33@�33@�ff@�  @���A  A$��AFffAd��A���A�  A�  A�33A���Aљ�A���A�33B   BffB  B��B   B(��B0ffB8  B?��BHffBP��BXffB`  Bg��Bp  BxffB�  B�  B�ffB�33B�  B�33B�ffB�  B���B�  B�33B�33B�  B�  B���B�  B���B���B���B�ffB�33B�33B�  B���B�33B䙚B�33B���B�33B�ffB�33B�  C �CL�C33C  C  C	�fC�CffC33C�C  C��C  C33C  C  C   C!��C$�C&L�C(33C*�C,  C.  C0�C233C4  C5�fC8�C:33C<�C>  C?�fCA��CC�3CF  CH�CJ�CL  CM�fCO�fCR  CS�fCV  CX33CZ33C\�C]�fC_�fCb�Cd�Cf�Ch  Ci��Cl�Cn33Cp  Cq�fCt33CvL�CxL�CzL�C|�C}��C��C��C�&fC��C��C�  C��fC��fC��3C�  C��C��C��3C�  C��C��3C�  C��C��C�&fC��C�  C��fC�  C��C�  C��3C��C�&fC�&fC�&fC��C��C��C��C��C��C��C��C��C��C�  C�  C��3C��3C�  C�&fC��C��C�  C��fC�ٚC�  C��C�  C��3C�ٚC�  C��C�  C��3C�  C��C��3C��C��C��C��C�  C��C��C�  C��C��C��C��C�  C��fC�  C�  C�  C��C��C��C��3C�  C�  C��C��C��3C�  C�  C��3C��C��C�  C��C�&fC��C�  C��C��C�  C��C�&fC��C��3C�  C��C��3C��C��C�  C��C�&fC��C��3C�  C�  C��C��C��3C��fC��C�  C�ٚC��C�33D �D � D�D�3DfD� D�D�fD��Ds3D�D��D  Dy�D�3D�3D3D��D	�D	�fD
fD
� D  Dl�D3D�3D�D��D�D� DfD� D  D� D��Ds3D�3D��D�D�3D�D��DfD�fDfD�fDfD�fD  D�fD  D� DfD� D  D� D��D� D�D�3D�D�fD  Dy�D�3D ��D!�D!�fD"  D"y�D#�D#��D$  D$��D%�D%� D&  D&�3D&��D'�3D(fD(y�D)�D)� D)�3D*��D*��D+ffD+��D,�3D-  D-l�D.  D.�3D.��D/l�D0  D0�fD13D1� D2�D2y�D3�D3��D4  D4l�D4��D5�fD6�D6�fD6��D7�fD8�D8� D9�D9y�D:fD:��D;3D;� D;��D<y�D=�D=�3D>  D>�fD>��D?y�D@  D@��DAfDAl�DA�3DB�fDC3DC� DC��DDy�DE�DE��DFfDFl�DG  DG��DH�DH� DH��DIs3DJfDJ�3DK�DK� DL�DLs3DM  DM��DN�DN��DOfDOl�DP  DP��DQ�DQy�DQ��DRs3DS  DS�fDT�DT��DU3DU��DV  DVffDV�3DW�fDX�DX��DY  DYl�DY�3DZy�D[fD[��D\3D\��D]  D]�fD]��D^y�D_fD_��D`fD`s3Da  Da��Db�Db� Db��Dcy�Dd�Dd��DefDel�De��Df� Dg3Dg��DhfDhffDh�3Di� DjfDj��Dk�Dk� Dl�Dll�Dl��Dm� Dn  Dn�fDo3Do��Dp�Dp� Dq  Dql�Dq�3Drs3Ds  Ds� Dt  Dt�fDu�Du�3Dv�Dv� Dv��Dws3Dw�3Dx� DyfDy�3Dz�Dz� Dz�fD{l�D{�3D|y�D}  D}�fD~3D~�3D�D� D�fD�9�D�|�D��3D�fD�I�D���D�� D�3D�9�D�y�D���D���D�C3D��fD�ɚD� D�C3D�s3D��fD���D�<�D�|�D��3D�fD�FfD���D�� D��fD�9�D�� D��3D�	�D�I�D�� D��3D���D�<�D�� D��fD��D�C3D�vfD���D�3D�I�D�� D��fD�  D�I�D�y�D��3D���D�@ D���D�� D��fD�C3D���D��3D���D�FfD�|�D��3D�  D�I�D�� D���D�  D�I�D�� D���D�  D�L�D��3D���D�fD�9�D�vfD�� D��D�<�D�vfD���D�fD�L�D�� D���D�  D�I�D�� D��fD�3D�I�D��3D���D�3D�L�D��3D���D�3D�L�D��3D���D�3D�I�D�� D��fD���D�FfD���D��3D��fD�9�D�� D��fD�	�D�P D�� D�ɚD���D�@ D���D���D�  D�6fD�y�D�� D�3D�FfD���D�� D�3D�6fD�y�D���D���D�@ D��3D��fD�fD�I�D���D���D��3D�33D�s3D��fD���D�9�D�|�D���D�  D�@ D��fD��fD��D�L�D�� D��fD���D�<�D�� D��3D�fD�I�D���D�� D�  D�6fD�vfD�� D�3D�I�D�� D�� D�fD�9�D��3D�ɚD� D�C3D�|�D���D�3D�L�D�|�D���D�  D�FfD�� D�� D��fD�<�D��3D���D�3D�6fD�|�D��fD��D�L�D�� D��3D���D�@ D��fD�ɚD���D�33D�|�D�� D��D�C3D�y�D��3D�	�D�@ D�y�D��3D���D�6fD�� D�ɚD�  D�<�D��fD���D�3D�<�D��fD�� D�fD�<�D���D��3D���D�I�D�� D���D�	�D�@ D�y�D��fD���D�9�D��3D�� D�fD�C3D�y�D���D�fD�@ D�vfD��3D�  D�6fD��3D�� D�	�D�@ D���D��3D�  D�I�D�|�D¶fD�fD�L�D�|�D�� D��D�FfD�y�D��3D� D�FfDŀ D���D�fD�<�DƉ�D�� D���D�C3Dǐ D��3D���D�C3DȌ�D��fD�  D�I�D�|�Dɹ�D�  D�L�Dʀ Dʼ�D�fD�@ D�vfD��3D��D�@ D�y�D��3D�	�D�@ D�s3Dͼ�D�  D�FfDΉ�D�� D�  D�6fD�y�D�� D�fD�L�DЃ3Dг3D��fD�<�DцfD�ɚD���D�33D�s3DҶfD���D�<�DӃ3D��3D�3D�C3Dԃ3D�ɚD�	�D�L�DՐ D�� D�  D�S3Dփ3Dֳ3D��fD�33D�p D׳3D��3D�6fD�s3DضfD��fD�6fD�s3D��3D� D�L�Dډ�D�ɚD�	�D�FfDۆfD��3D�  D�<�D�y�D��3D� D�FfD݃3D�� D��fD�<�DކfD�� D�3D�I�D߀ D�� D�fD�<�D��3D��fD�fD�I�D�|�D��3D�fD�FfD≚D�� D�3D�C3D�fD��fD���D�@ D� D��3D�  D�C3D�3D�� D�  D�C3D� D�� D�fD�C3D�3D�� D�  D�<�D�y�D��fD�fD�9�D�|�D��fD�3D�<�D� D��3D�fD�C3D�fD��D���D�9�D�y�D�fD���D�9�D�vfD���D���D�FfD�3D��3D�3D�C3DD�� D��fD�<�D��fD��D���D�C3D�|�D��3D��D�FfD�|�D�D�  D�I�D�3D�� D���D�@ D�D��fD�  D�9�D�vfD�� D��D�L�D���D���D�	�D�I�D��fD��fD�fD�FfD��fD��fD�	�D�FfD���D���D�	�D�� D�� D�p E +3E ��E�E�Eq�E�E�3E0 E Ek3E8 E��E` E�Ey�E	<�E	�E
�fE  E�3Ec3E3E��E E�3E��EFfE�3Ed�E#3E� E@ EfEk3E0 E�3ET�E#3E� E^fE�3E��E\�E��E�3E� E� Ek3E��E��ENfE��E [3E!�E!�3E"S3E"�3E#�3E$#3E$� E%H E%� E&��E';3E'ɚE(` E)  E)�3E*A�E*�fE+t�E,3E,��E-�3E.0 E.�3E/Y�E/� E0�fE1!�E1� E2VfE2�E3� E4s3E5fE5�fE69�E6� E7d�E7�fE8�fE9p E:  E:��E;�E;ٚE<X E=fE=��E>I�E>�fE?ffE@ E@��EAL�EA�3EB�3EC+3ECњEDk3EE3EE�fEFNfEF�3EG�fEHA�EH�3EI��EJ EJ�fEKh ELfEL��EM1�EM��ENy�EO( EO��EPI�EP�fEQ��ERH ER��ES��ET.fET��EUh EV  EV�3EW  EWٚEX` EY EY��EZS3>L��>L��>���>���>L��>���>L��>���=���>L��>���>���>L��>L��>���>L��>L��>���>���>���>���>���>���?��?L��?L��?�  ?���?�33?���@   @ff@&ff@333@Fff@Y��@l��@�33@�  @���@�33@�  @�  @ə�@ٙ�@�ff@�ffA��A33A33A��A!��A)��A0  A;33AA��AI��AP  AY��A`  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411414141144414414414411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?L��?�  @��@`  @�33@�33@�ff@�  AffA  A,��ANffAl��A���A�  A�  A�33A���Aՙ�A���A�33B  B
ffB  B��B"  B*��B2ffB:  BA��BJffBR��BZffBb  Bi��Br  BzffB�  B�  B�ffB�33B�  B�33B�ffB�  B���B�  B�33B�33B�  B�  B���B�  B���B���B���B�ffB�33B�33B�  B���B�33B噚B�33B���B�33B�ffB�33B�  C ��C��C�3C� C� C
ffC��C�fC�3C��C� CL�C� C�3C� C� C � C"L�C$��C&��C(�3C*��C,� C.� C0��C2�3C4� C6ffC8��C:�3C<��C>� C@ffCBL�CD33CF� CH��CJ��CL� CNffCPffCR� CTffCV� CX�3CZ�3C\��C^ffC`ffCb��Cd��Cf��Ch� CjL�Cl��Cn�3Cp� CrffCt�3Cv��Cx��Cz��C|��C~L�C�&fC�L�C�ffC�Y�C�L�C�@ C�&fC�&fC�33C�@ C�L�C�L�C�33C�@ C�L�C�33C�@ C�L�C�L�C�ffC�L�C�@ C�&fC�@ C�Y�C�@ C�33C�L�C�ffC�ffC�ffC�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�33C�33C�@ C�ffC�Y�C�L�C�@ C�&fC��C�@ C�Y�C�@ C�33C��C�@ C�Y�C�@ C�33C�@ C�L�C�33C�L�C�L�C�Y�C�Y�C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�Y�C�@ C�&fC�@ C�@ C�@ C�L�C�L�C�Y�C�33C�@ C�@ C�L�C�Y�C�33C�@ C�@ C�33C�L�C�L�C�@ C�L�C�ffC�L�C�@ C�L�C�Y�C�@ C�Y�C�ffC�L�C�33C�@ C�L�C�33C�L�C�L�C�@ C�L�C�ffC�L�C�33C�@ C�@ C�Y�C�Y�C�33C�&fC�L�C�@ C��C�L�C�s3D ,�D � D9�D�3D&fD� D,�D�fD�D�3D9�D��D  D��D3D�3D33D��D	,�D	�fD
&fD
� D  D��D33D�3D,�D��D,�D� D&fD� D  D� D�D�3D3D��D,�D�3D,�D��D&fD�fD&fD�fD&fD�fD  D�fD  D� D&fD� D  D� D�D� D9�D�3D,�D�fD  D��D 3D ��D!,�D!�fD"  D"��D#9�D#��D$  D$��D%,�D%� D&@ D&�3D'�D'�3D(&fD(��D),�D)� D*3D*��D+�D+�fD,�D,�3D-  D-��D.  D.�3D/�D/��D0  D0�fD133D1� D2,�D2��D3,�D3��D4  D4��D5�D5�fD69�D6�fD7�D7�fD89�D8� D9,�D9��D:&fD:��D;33D;� D<�D<��D=,�D=�3D>@ D>�fD?�D?��D@  D@��DA&fDA��DB3DB�fDC33DC� DD�DD��DE,�DE��DF&fDF��DG  DG��DH9�DH� DI�DI�3DJ&fDJ�3DK9�DK� DL,�DL�3DM  DM��DN,�DN��DO&fDO��DP  DP��DQ9�DQ��DR�DR�3DS  DS�fDT,�DT��DU33DU��DV  DV�fDW3DW�fDX,�DX��DY  DY��DZ3DZ��D[&fD[��D\33D\��D]@ D]�fD^�D^��D_&fD_��D`&fD`�3Da  Da��Db9�Db� Dc�Dc��Dd,�Dd��De&fDe��Df�Df� Dg33Dg��Dh&fDh�fDi3Di� Dj&fDj��Dk9�Dk� Dl,�Dl��Dm�Dm� Dn  Dn�fDo33Do��Dp9�Dp� Dq  Dq��Dr3Dr�3Ds  Ds� Dt  Dt�fDu,�Du�3Dv9�Dv� Dw�Dw�3Dx3Dx� Dy&fDy�3Dz9�Dz� D{fD{��D|3D|��D}  D}�fD~33D~�3D9�D� D�3D�I�D���D��3D�fD�Y�D���D�� D�3D�I�D���D���D��D�S3D��fD�ٚD�  D�S3D��3D��fD�	�D�L�D���D��3D�fD�VfD���D�� D�fD�I�D�� D��3D��D�Y�D�� D��3D�	�D�L�D�� D��fD��D�S3D��fD���D�3D�Y�D�� D��fD� D�Y�D���D��3D�	�D�P D���D�� D�fD�S3D���D��3D��D�VfD���D��3D� D�Y�D�� D�ɚD� D�Y�D�� D�ɚD� D�\�D��3D���D�fD�I�D��fD�� D��D�L�D��fD���D�fD�\�D�� D�ɚD� D�Y�D�� D��fD�3D�Y�D��3D���D�3D�\�D��3D�ɚD�3D�\�D��3D�ɚD�3D�Y�D�� D��fD��D�VfD���D��3D�fD�I�D�� D��fD��D�` D�� D�ٚD��D�P D���D���D� D�FfD���D�� D�3D�VfD���D�� D�3D�FfD���D���D��D�P D��3D��fD�fD�Y�D���D���D�3D�C3D��3D��fD�	�D�I�D���D���D� D�P D��fD��fD��D�\�D�� D��fD�	�D�L�D�� D��3D�fD�Y�D���D�� D� D�FfD��fD�� D�3D�Y�D�� D�� D�fD�I�D��3D�ٚD�  D�S3D���D���D�3D�\�D���D�ɚD� D�VfD�� D�� D�fD�L�D��3D���D�3D�FfD���D��fD��D�\�D�� D��3D�	�D�P D��fD�ٚD��D�C3D���D�� D��D�S3D���D��3D��D�P D���D��3D�	�D�FfD�� D�ٚD� D�L�D��fD���D�3D�L�D��fD�� D�fD�L�D���D��3D�	�D�Y�D�� D���D��D�P D���D��fD��D�I�D��3D�� D�fD�S3D���D�ɚD�fD�P D��fD��3D� D�FfD��3D�� D��D�P D���D��3D� D�Y�D�D��fD�fD�\�DÌ�D�� D��D�VfDĉ�D��3D�  D�VfDŐ D���D�fD�L�Dƙ�D�� D�	�D�S3DǠ D��3D�	�D�S3DȜ�D��fD� D�Y�DɌ�D�ɚD� D�\�Dʐ D���D�fD�P DˆfD��3D��D�P D̉�D��3D��D�P D̓3D���D� D�VfDΙ�D�� D� D�FfDω�D�� D�fD�\�DГ3D��3D�fD�L�DіfD�ٚD��D�C3D҃3D��fD�	�D�L�Dӓ3D��3D�3D�S3Dԓ3D�ٚD��D�\�Dՠ D�� D� D�c3D֓3D��3D�fD�C3D׀ D��3D�3D�FfD؃3D��fD�fD�FfDك3D��3D�  D�\�Dڙ�D�ٚD��D�VfDۖfD��3D� D�L�D܉�D��3D�  D�VfDݓ3D�� D�fD�L�DޖfD�� D�3D�Y�Dߐ D�� D�fD�L�D��3D��fD�fD�Y�D��D��3D�fD�VfD♚D�� D�3D�S3D�fD��fD��D�P D� D��3D� D�S3D�3D�� D� D�S3D� D�� D�fD�S3D�3D�� D� D�L�D艚D��fD�fD�I�D��D��fD�3D�L�D� D��3D�fD�S3D�fD���D�	�D�I�D쉚D��fD�	�D�I�D�fD���D��D�VfD�3D��3D�3D�S3DD�� D�fD�L�D�fD���D��D�S3D��D��3D��D�VfD��D�ɚD� D�Y�D�3D�� D�	�D�P D���D��fD� D�I�D��fD�� D��D�\�D���D���D��D�Y�D��fD��fD�fD�VfD��fD��fD��D�VfD���D���D��D�� D�� D�� E 33E ��E�E	�Ey�E�E�3E8 E Es3E@ E��Eh E$�E��E	D�E	��E
�fE E�3Ek3E3E��E  E�3E��ENfE�3El�E+3E� EH EfEs3E8 E�3E\�E+3E� EffE�3E��Ed�E��E�3E� E� Es3E��E��EVfE��E c3E!�E!�3E"[3E"�3E#�3E$+3E$� E%P E%� E&��E'C3E'њE(h E) E)�3E*I�E*�fE+|�E,3E,��E-�3E.8 E.�3E/a�E/� E0�fE1)�E1� E2^fE2��E3� E4{3E5fE5�fE6A�E6� E7l�E8fE8�fE9x E: E:��E;�E;�E<` E=fE=��E>Q�E>�fE?nfE@ E@��EAT�EA�3EB�3EC33ECٚEDs3EE3EE�fEFVfEF�3EG�fEHI�EH�3EI��EJ EJ�fEKp ELfEL��EM9�EM��EN��EO0 EO��EPQ�EP�fEQ��ERP ER��ES��ET6fET��EUp EV EV�3EW( EW�EXh EY  EY��EZ[3G�O�?333?L��G�O�?333G�O�?333G�O�?��?333G�O�G�O�G�O�?333G�O�G�O�?333G�O�G�O�?L��G�O�G�O�?fff?���G�O�?�ff?�  ?ٙ�?�33@ff@   @&ff@Fff@S33@fff@y��@�ff@�33@�  @���@�33@�  @�  @ٙ�@陚@�ffA33A	��A33A33A!��A)��A1��A8  AC33AI��AQ��AX  Aa��Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411414141144414414414411411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @ @ �@ �@ {@ �@ "�@ *S@ /�@ 5�@ =q@ FQ@ T�@ a�@ m�@ z�@ ��@ ��@ ��@ ��@ ��@ �@ �t@ ��@ ��@�@@ @-@:@F�@V@dZ@qS@~K@�D@��@��@��@@є@ލ@�@��@1@{@!s@/�@>@K�@X�@ff@s_@��@��@�U@��@��@ƨ@�O@�H@�@@�E@J@�@$�@3�@B8@O0@\)@j@y�@�|@��@�m@�f@�k@�@�h@�`@�Y@��@�@�@(�@6�@DD@P�@`B@oF@|?@�7@�0@��@�-@��@��@��@��@� @@@
@+@7�@G�@V@c�@p�@}�@�D@��@��@��@��@є@ލ@��@�~@�@*@"�@/�@<@K�@Z@ff@s_@�@�h@�@��@�@Ĝ@�C@��@��@��@
�@�@$.@1�@@,@N�@\�@j@v�@�@�u@��@�@�k@�@�@�`@�Y@��@�@�@(�@5�@D�@S�@a�@oF@|?@�7@��@��@�-@��@�|@�#@��@�q@	j@	@	
@	+�@	:@	I�@	V�@	c�@	p�@	|�@	��@	��@	��@	��@	��@	�*@	��@	��@	�,@
%@
{@
"�@
/@
>@
K�@
Z@
g�@
t@
�d@
�@
�@
�Y@
�@
ƨ@
��@
�H@
�@
��@
=@�@&;@3�@B8@M�@\)@i�@x&@�|@�@�m@�@�@�@׹@�@�@@V@O@)�@7�@DD@SI@a�@m�@z3@��@��@�(@�-@��@��@�#@�(@�q@�@@�@-�@;d@F�@S�@c�@p�@|?@��@�U@��@��@�J@�C@ލ@��@��@�@�@ �@2�@>�@K@X@e	@v@��@��@�a@�Y@�@��@Ӡ@�;@��@��@�@B@&�@33@A�@N�@\)@i�@v�@��@�h@�(@�r@��@��@�h@�`@�@ �@V@�@)�@6�@D�@Q�@_�@m�@z�@��@�0@�(@�~@��@��@��@��@��@�@�@!s@-�@:�@G�@T�@e�@r@~K@��@��@�A@�R@Ĝ@ψ@��@�4@�~@1@{@ �@1'@<�@H]@X@hs@t@�@�\@�@��@�F@��@�O@�T@�Y@��@	�@B@(G@33@>�@M�@\�@l�@x&@�p@�u@�(@�~@�@ȴ@׹@�@�e@  @�@�@*S@8�@G�@R�@]�@l�@z�@��@��@��@�!@��@��@�t@�@�@�@�@g@*S@:@I@X@b�@n�@|�@��@��@��@�R@��@��@��@��@��@	�@*@ @/�@>�@M�@X@dZ@r�@��@�@�a@�@�^@ȴ@Ӡ@ލ@�@�E@�@�@%�@1'@?}@M�@\�@k.@y�@��@�0@�@�@�@�@��@�`@��@  @@
@(�@4�@C�@SI@bN@m�@x�@��@�0@��@�9@��@�@�@�@�q@�@�@""@-�@7�@F�@UU@b�@qS@�W@��@�U@��@��@��@��@܀@�@�,@�@*@#�@1�@@,@K@V�@e	@r�@��@�@�@�f@�R@�>@є@��@�@@��@
�@�@'�@5�@@�@K�@Z�@i!@x&@�|@��@�(@�~@�j@�@խ@�@�@ �@@[@,`@7L@A�@O�@^5@l�@z3@�7@��@�4@�9@�&@��@�@�@�q@v@@""@-@8�@F�@UU@dZ@s_@~�@��@��@��@��@@�*@��@�@��@@@""@1�@=q@I@Yn@i!@t�@�@��@�U@��@�R@�@Ӡ@��@��@��@
=@�@%�@5�@A�@M�@]�@hs@uk@�@��@��@�@�@��@��@�@��@   @ �@ O@ &�@ 7L@ FQ@ R�@ ^�@ m�@ }�@ �7@ ��@ ��@ �9@ ��@ �o@ �#@ �(@ ��@!]@!b@! @!/@!:�@!E�@!S�@!b�@!r@!�W@!�\@!�@!�M@!�9@!@!�C@!��@!�@!� @"v@"{@""�@"1'@"@,@"N�@"Yn@"dZ@"r�@"�@"��@"�@"�Y@"��@"�W@"խ@"�@"�@@"��@#�@#*@##�@#1�@#?}@#M�@#[z@#i�@#ww@#�|@#�#@#�(@#��@#��@#�W@#խ@#�@#�Y@$ �@$@$[@$+�@$:@$DD@$O�@$]�@$m:@${�@$��@$��@$�A@$��@$��@$�|@$܀@$�@$�q@%�@%b@%g@%/@%9X@%FQ@%UU@%dZ@%t@%~K@%��@%��@%��@%��@%�>@%�*@%�/@%��@%��@&	�@&{@&g@&.l@&=q@&Lu@&Z�@&e�@&qS@&�@&�\@&��@&�Y@&��@&ƨ@&խ@&�H@&�@&�E@'�@'�@'%�@'5@@'@�@'M�@']�@'l�@'x&@'�p@'�#@'��@'�r@'�@'�o@'׹@'�T@'�e@(  @(�@([@((�@(5@@(E�@(Q=@(^5@(m�@(~K@(��@(��@(�z@(�!@(��@(��@(�h@(��@(��@)]@)�@)""@).l@):@)Ji@)V@)b�@)r�@)}�@)��@)��@)��@)�9@)@)��@)�;@)�(@)��@*
=@*�@*""@*2�@*>�@*Ji@*Z�@*ff@*r�@*�d@*��@*��@*�M@*�@*ȴ@*��@*�H@*��@*��@+�@+�@+(G@+33@+@,@+O�@+\)@+g�@+x&@+��@+��@+�@+��@+��@+�c@+�O@+�@+�Y@,]@,�@,�@,(�@,4�@,B�@,Q�@,`�@,o�@,{�@,��@,�#@,�(@,��@,�2@,�@,׹@,�`@,�@-@-b@-g@--@-:�@-H]@-V@-e	@-r�@-�@-�\@-��@-�A@-�@-�>@-�|@-��@-��@-��@.@.�@. @.-@.;d@.I@.V�@.c�@.t�@.�@.�@.�@.��@.�^@.�W@.��@.��@.��@.��@/�@/�@/(�@/4�@/A�@/N�@/Z@/i!@/x�@/�@/�u@/�z@/�@/��@/��@/�\@/�`@/�@0]@0�@0�@0)�@07�@0E�@0S�@0_�@0m�@0{�@0��@0��@0�(@0�~@0�&@0�|@0�t@0��@0�q@1j@1@1g@1,`@1:@1I@1V@1c�@1p�@1~K@1�D@1�<@1��@1�F@1�2@1ψ@1�;@1�4@1�~@2�@2*@2#�@20x@2>�@2Ji@2Wb@2e	@2r�@2�@2��@2��@2��@2��@2�J@2��@2��@2�@2�E@3
�@3�@3%�@31'@3@,@3O�@3[z@3i!@3x&@3�p@3�u@3�(@3�r@3�@3�@3�
@3�@3�@4  @4J@4O@4+@47�@4DD@4P�@4]�@4m:@4}�@4�D@4��@4��@4��@4�2@4�*@4��@4�y@4� @5�@5o@5 �@5-�@5<@5Ji@5Wb@5�@6{@6G�@6�r@6��@7@7x&@7��@7׹@87�@8ff@8@8��@9DD@9m�@9@:@::�@:��@:�#@;(G@;N�@;��@;�@<1'@<|�@<�m@<�L@=> @=��@=�F@>�@>Z@>��@>Ӡ@?(G@?SI@?�A@?��@@$/@@|?@@�A@A�@A-�@A��@A܀@Bv@BZ@B��@B�
@C*S@CS�@C��@C�}@D[@Dk.@D��@D��@EB8@E�|@E�W@F1@FG�@F�@F@G[@GZ@G��@G�
@HO@H`�@H��@H�l@I'�@Ig�@I��@J@JQ�@J��@J��@K@KQ=@K�u@K�
@L6@LYn@L��@L��@M@,@M�@M��@N  @N?}@N�@N��@O�@O\)@O�<@O�C@P&;@P\)@P�f@P�T@Q0x@Qz2@Q��@Q�Y@R3�@Ry�@R�@S]@SE�@S��@S�*@T@TT�@T�T@T�@U(G@UqS@U��@Vv@V6�@V�@V�c@Wb@WB�@W��@W�C@X�@Xb�@X��@Xލ@Y$�@Ym�@Y�R@Y��@ZB�@Z��@Z�o@[�@[N�@[�P@[�c@\�@\Q�@\�m@\�#@]&�G�O�@ ^@ G�O�@ ^G�O�@ ^G�O�@  �@ ^G�O�G�O�G�O�@ ^G�O�G�O�@ ^G�O�G�O�@ G�O�G�O�@ �@ G�O�@ v@ �@ 1@ 	�@ 
�@ �@ V@ �@ @ *@ 6@ B@ �@ �@  �@ "�@ %�@ (�@ +@ .l@ 1'@ 4�@ 7L@ ;d@ >�@ A�@ D�@ H]@ K@ O�@ R�@ V@ X�@ \�@ _�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A��A�z�A�|�A��A��+A��7A��7A��7A��7A��DA��PA��PA��PA��\A��\A���A���A���A���A��A�;dA���A��A�l�A�=qA�(�A�"�A�bA���A��/A���A��9A�jA�33A�%A��A��
A���A�=qA�&�A���A�-A��RA�\)A� �A�VA��A��-A�A�A���A��/A��A�I�A��A�9XA���A���A��A��A�n�A�JA���A�;dA��A�~�A�ffA��FA��7A�n�A�;dA��A��A��A�p�A��TA���A�%A��jA��A���A��#A�\)A��9A�?}A��;A�hsA�~�A�K�A�;dA�7LA�33A�+A�1A���A�JA�JA���A�5?A���A��A�VA��uA��uA���A��TA�oA���A���A��!A�
=A���A��A�ȴA�VA��`A��A���A�oA��FA�dZA�
=A��+A���A�?}A��9A�E�A~��A|VA{O�Ay�
Aw|�Av  Au|�Ar�An�!An�Ak�PAi��Ah �Ac�PA_hsA]�hAZQ�AV�+ATM�ASS�AP5?AN�9AM�AK�AI��AF�AA��A?�A>5?A=`BA<ZA<VA<^5A<bNA<^5A:�\A9��A7�#A4�A3�
A3?}A1�A0�A.��A-VA+�
A+p�A*n�A(�!A't�A&��A%VA$~�A#O�A"JA!`BA bA��A��A�TA�A��AffA�hA�`A�A�A%A�AȴA�A��A��A��A�A��AA
��A
Q�A	�-A	K�A�FA�/A��AE�A�AG�A�+A��A�PA�@��P@��@�Z@�+@�~�@�z�@��@�@�!@�dZ@�@�5?@��/@�K�@�-@�@�1@��m@�|�@�+@�
=@��y@�n�@�-@�h@�A�@�O�@�?}@�p�@��@�@�^5@���@܋D@�z�@��`@�+@�I�@���@Ցh@ԋD@�ƨ@�l�@��H@�-@�X@�l�@�M�@�hs@���@�+@�V@�J@ɩ�@�`B@�\)@���@�^5@ũ�@ŉ7@őh@�@ư!@Ǯ@Ǯ@�;d@�;d@��y@��H@�v�@�5?@��@ź^@�X@���@Ĭ@ă@�1'@���@�@�`B@���@�Ĝ@�Q�@�|�@��H@�V@��h@��P@���@�n�@�@�x�@���@��7@�p�@�O�@�?}@��@���@���@�  @��@��@���@�9X@��j@�bN@��`@�/@�O�@���@��7@���@�Ĝ@���@���@�V@���@� �@���@�b@�1@���@���@�|�@�l�@�"�@���@�O�@�?}@��@�9X@�ȴ@�~�@�=q@�=q@�n�@�=q@�@��@�%@��@��@�+@���@���@���@���@�z�@�9X@�  @��w@��y@���@�`B@��@��@�Q�@�1'@�b@�  @��m@��P@�33@�{@�?}@��`@�Ĝ@��@�b@�b@��w@���@���@�|�@�K�@�33@��!@�@��@���@��h@��@��@�l�@�C�@�33@�"�@��@��y@��!@�$�@��^@�p�@�?}@���@��u@�Q�@�1'@���@��w@�;d@�
=@���@���@�?}@�j@�1@�ƨ@���@�dZ@�C�@���@���@���@���@��+@�v�@�^5@�=q@�$�@�{@�@��@��@��@��T@��#@��#@���@��#@��-@�x�@�G�@�7L@��@��`@��/@�Ĝ@���@��@�(�@��
@��@���@���@�l�@�K�@��H@�v�@�E�@�-@���@��-@��h@�?}@�/@�/@�/@�&�@��@���@�9X@���@��m@��
@�ƨ@���@���@���@���@��@�33@���@��+@��@���@��-@�p�@�?}@���@��@��P@�l�@�;d@�
=@���@�V@�E�@�-@�$�@�@�@���@��@��@��@��T@��#@��^@�hs@��D@� �@��;@��@�;d@�;d@���@���@�=q@���@�hs@�%@��@��u@��D@�9X@���@�@�~�@�V@��@���@���@��@�p�@�p�@�hs@�X@��u@�@�P@\)@\)@+@~��@~ff@}�@|�/@{��@{�m@{��@{t�@z�@zM�@yX@x�9@x�u@x�@x�@xr�@xA�@w��@w;d@w+@v�y@v��@vE�@v{@u@u�@up�@uV@tj@s��@sdZ@s"�@r��@r=q@r-@r-@rJ@q�7@qX@qG�@qG�@qG�@qG�@q&�@q&�@q&�@q&�@q%@p��@pĜ@pr�@pb@o�@o�;@o�w@o+@n�y@nv�@nv�@nff@nV@nff@nE�@m�T@mp�@m�@l��@l�/@lI�@kt�@kC�@j��@j��@jn�@j-@j�@i��@i&�@h��@h�`@h�9@h��@hQ�@g�w@g
=@f��@f�R@fff@f5?@e�@e`B@eV@d�j@dj@d�@c�
@c��@c33@b~�@a�#@a7L@`Ĝ@`A�@_��@_
=@^{@]?}@\�/@\�@\�@\Z@\�@[��@[�@[33@[o@Z�H@Z-@Yhs@X��@XĜ@XĜ@X��@XbN@XA�@X �@W��@W��@V��@V�+@V{@U�-@Up�@T�j@Tj@T1@S"�@R~�@RM�@Q�^@Qx�@Q%@P�9@PA�@P  @Ol�@N5?@L��@L(�@K��@K@J�!@Jn�@J=q@J�@J�@I�@I��@Ihs@I7L@I�@HbN@G;d@F�R@F��@F��@F��@F��@F��@F��@F��@F�R@F�R@F�R@F�R@F�R@Fv�@E��@EO�@E�@EV@D��@D�j@Dj@Cƨ@C�@CS�@C@C@B�@B��@B��@B��@B��@B�\@B��@B�\@B^5@BM�@B^5@BJ@A��@Ax�@A7L@A�@@A�@?�@?�@?\)@?�@>�y@>ȴ@>�R@>v�@>V@=��@<�@<��@<(�@;�F@;��@9��@9��@9�^@9��@9��@9G�@8�`@8��@8�@81'@8  @7�@7|�@7�P@7�w@7\)@7�@7
=@6�R@6V@5`B@4�@3�m@2�@2��@2�!@2�\@2n�@2~�@2�\@2�@1�^@1�7@1X@1&�@0r�@/�@.��@.ff@.E�@.5?@.5?@-@-V@,��@,�@,�D@,z�@,�D@,��@,�@,�@,�@,�@,�@,�@,�@,�@,�@,�@,(�@+33@*M�@)��@)G�@)G�@)G�@)7L@)&�@)%@(Ĝ@(��@(��@( �@&�y@&$�@%�-@%�h@%�h@%`B@%�@%V@$��@$�@$1@#��@#�
@#�@#C�@#"�@"�H@"��@"�\@"~�@"^5@"M�@"-@!��@!X@!7L@!�@!&�@!%@ �u@ �u@ r�@  �@�w@
=@��@�+@V@V@{@�-@p�@`B@O�@V@j@�@�m@t�@"�@=q@��@&�@ �@+@�R@��@v�@V@5?@$�@�@��@��@O�@�@�@�D@z�@j@I�@I�@I�@1@ƨ@��@��@C�@C�@C�@�@�\@�\@�\@�\@�\@�\@~�@~�@�\@^5@M�@-@�^@��@��@��@hs@&�@��@�@�@�P@��@|�@;d@�y@E�@$�@$�@$�@$�@5?@@�T@��@��@`B@/@/@?}@�@�@�@�@V@��@�@�@(�@(�@�
@��@t�@o@@
�H@
��@
�\@	��@  @\)@K�@K�@K�@+@;d@;d@;d@;d@+@�@+@+@+@;d@;d@��@�+@5?@V@(�@S�@�@n�@�@�#@&�@&�@%@ 1'@ A�?�;d?�O�?��?�(�?���?��u?�Q�?�Q�?�ff?��j?�?��?�!?�7?���?�v�?���?�p�?�V?�I�?��?���?���?��#?�Q�?�ȴ?��T?�`B?�?��?�j?�j?��?�F?��?��?�hs?��`?߾w?�;d?��?ݲ-?�(�?ۅ?ڟ�?ٺ^?��?�r�?�1'?և+?�E�?�?��?�Z?���?ӕ�?ӕ�?�o?�n�?�M�?�J?��?ѩ�?�hs?� �?�  ?�  ?�  ?��;?ϝ�?�\)?Η�?�5??Ͳ-?Ͳ-?���?�V?�(�?˥�?˥�?�?�~�?�=q?�=q?�X?�X?ȴ9?ȴ9?ȓu?�r�?�Q�?�b?Ƨ�?�E�?�?Ł?Ł?���?�z�?ě�?�Z?�9X?Õ�?���?�n�?�-?�hs?��`?�bN?�A�?�A�?�bN?�bN?��?�bN?��?�bN?�bN?��?��?�  ?�  ?��;?���?���?�|�?���?�|�?�;d?��R?��?���?�v�?�v�A�ffA�l�A�n�A�l�A�jA�l�A�hsA�n�A�r�A�p�A�p�A�t�A�r�A�p�A�jA�l�A�jA�jA�n�A�n�A�v�A�v�A��A�~�A��+A��A��A��A��A�|�A�v�A�z�A�x�A�z�A�~�A�~�A��A��A��+A��+A��+A��7A��7A��7A��7A��+A��7A��7A��7A��7A��+A��7A��PA��PA��DA��DA��PA��\A��PA��DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A�n�A��A�z�A�|�A��A��+A��7A��7A��7A��7A��DA��PA��PA��PA��\A��\A���A���A���A���A��A�;dA���A��A�l�A�=qA�(�A�"�A�bA���A��/A���A��9A�jA�33A�%A��A��
A���A�=qA�&�A���A�-A��RA�\)A� �A�VA��A��-A�A�A���A��/A��A�I�A��A�9XA���A���A��A��A�n�A�JA���A�;dA��A�~�A�ffA��FA��7A�n�A�;dA��A��A��A�p�A��TA���A�%A��jA��A���A��#A�\)A��9A�?}A��;A�hsA�~�A�K�A�;dA�7LA�33A�+A�1A���A�JA�JA���A�5?A���A��A�VA��uA��uA���A��TA�oA���A���A��!A�
=A���A��A�ȴA�VA��`A��A���A�oA��FA�dZA�
=A��+A���A�?}A��9A�E�A~��A|VA{O�Ay�
Aw|�Av  Au|�Ar�An�!An�Ak�PAi��Ah �Ac�PA_hsA]�hAZQ�AV�+ATM�ASS�AP5?AN�9AM�AK�AI��AF�AA��A?�A>5?A=`BA<ZA<VA<^5A<bNA<^5A:�\A9��A7�#A4�A3�
A3?}A1�A0�A.��A-VA+�
A+p�A*n�A(�!A't�A&��A%VA$~�A#O�A"JA!`BA bA��A��A�TA�A��AffA�hA�`A�A�A%A�AȴA�A��A��A��A�A��AA
��A
Q�A	�-A	K�A�FA�/A��AE�A�AG�A�+A��A�PA�@��P@��@�Z@�+@�~�@�z�@��@�@�!@�dZ@�@�5?@��/@�K�@�-@�@�1@��m@�|�@�+@�
=@��y@�n�@�-@�h@�A�@�O�@�?}@�p�@��@�@�^5@���@܋D@�z�@��`@�+@�I�@���@Ցh@ԋD@�ƨ@�l�@��H@�-@�X@�l�@�M�@�hs@���@�+@�V@�J@ɩ�@�`B@�\)@���@�^5@ũ�@ŉ7@őh@�@ư!@Ǯ@Ǯ@�;d@�;d@��y@��H@�v�@�5?@��@ź^@�X@���@Ĭ@ă@�1'@���@�@�`B@���@�Ĝ@�Q�@�|�@��H@�V@��h@��P@���@�n�@�@�x�@���@��7@�p�@�O�@�?}@��@���@���@�  @��@��@���@�9X@��j@�bN@��`@�/@�O�@���@��7@���@�Ĝ@���@���@�V@���@� �@���@�b@�1@���@���@�|�@�l�@�"�@���@�O�@�?}@��@�9X@�ȴ@�~�@�=q@�=q@�n�@�=q@�@��@�%@��@��@�+@���@���@���@���@�z�@�9X@�  @��w@��y@���@�`B@��@��@�Q�@�1'@�b@�  @��m@��P@�33@�{@�?}@��`@�Ĝ@��@�b@�b@��w@���@���@�|�@�K�@�33@��!@�@��@���@��h@��@��@�l�@�C�@�33@�"�@��@��y@��!@�$�@��^@�p�@�?}@���@��u@�Q�@�1'@���@��w@�;d@�
=@���@���@�?}@�j@�1@�ƨ@���@�dZ@�C�@���@���@���@���@��+@�v�@�^5@�=q@�$�@�{@�@��@��@��@��T@��#@��#@���@��#@��-@�x�@�G�@�7L@��@��`@��/@�Ĝ@���@��@�(�@��
@��@���@���@�l�@�K�@��H@�v�@�E�@�-@���@��-@��h@�?}@�/@�/@�/@�&�@��@���@�9X@���@��m@��
@�ƨ@���@���@���@���@��@�33@���@��+@��@���@��-@�p�@�?}@���@��@��P@�l�@�;d@�
=@���@�V@�E�@�-@�$�@�@�@���@��@��@��@��T@��#@��^@�hs@��D@� �@��;@��@�;d@�;d@���@���@�=q@���@�hs@�%@��@��u@��D@�9X@���@�@�~�@�V@��@���@���@��@�p�@�p�@�hs@�X@��u@�@�P@\)@\)@+@~��@~ff@}�@|�/@{��@{�m@{��@{t�@z�@zM�@yX@x�9@x�u@x�@x�@xr�@xA�@w��@w;d@w+@v�y@v��@vE�@v{@u@u�@up�@uV@tj@s��@sdZ@s"�@r��@r=q@r-@r-@rJ@q�7@qX@qG�@qG�@qG�@qG�@q&�@q&�@q&�@q&�@q%@p��@pĜ@pr�@pb@o�@o�;@o�w@o+@n�y@nv�@nv�@nff@nV@nff@nE�@m�T@mp�@m�@l��@l�/@lI�@kt�@kC�@j��@j��@jn�@j-@j�@i��@i&�@h��@h�`@h�9@h��@hQ�@g�w@g
=@f��@f�R@fff@f5?@e�@e`B@eV@d�j@dj@d�@c�
@c��@c33@b~�@a�#@a7L@`Ĝ@`A�@_��@_
=@^{@]?}@\�/@\�@\�@\Z@\�@[��@[�@[33@[o@Z�H@Z-@Yhs@X��@XĜ@XĜ@X��@XbN@XA�@X �@W��@W��@V��@V�+@V{@U�-@Up�@T�j@Tj@T1@S"�@R~�@RM�@Q�^@Qx�@Q%@P�9@PA�@P  @Ol�@N5?@L��@L(�@K��@K@J�!@Jn�@J=q@J�@J�@I�@I��@Ihs@I7L@I�@HbN@G;d@F�R@F��@F��@F��@F��@F��@F��@F��@F�R@F�R@F�R@F�R@F�R@Fv�@E��@EO�@E�@EV@D��@D�j@Dj@Cƨ@C�@CS�@C@C@B�@B��@B��@B��@B��@B�\@B��@B�\@B^5@BM�@B^5@BJ@A��@Ax�@A7L@A�@@A�@?�@?�@?\)@?�@>�y@>ȴ@>�R@>v�@>V@=��@<�@<��@<(�@;�F@;��@9��@9��@9�^@9��@9��@9G�@8�`@8��@8�@81'@8  @7�@7|�@7�P@7�w@7\)@7�@7
=@6�R@6V@5`B@4�@3�m@2�@2��@2�!@2�\@2n�@2~�@2�\@2�@1�^@1�7@1X@1&�@0r�@/�@.��@.ff@.E�@.5?@.5?@-@-V@,��@,�@,�D@,z�@,�D@,��@,�@,�@,�@,�@,�@,�@,�@,�@,�@,�@,(�@+33@*M�@)��@)G�@)G�@)G�@)7L@)&�@)%@(Ĝ@(��@(��@( �@&�y@&$�@%�-@%�h@%�h@%`B@%�@%V@$��@$�@$1@#��@#�
@#�@#C�@#"�@"�H@"��@"�\@"~�@"^5@"M�@"-@!��@!X@!7L@!�@!&�@!%@ �u@ �u@ r�@  �@�w@
=@��@�+@V@V@{@�-@p�@`B@O�@V@j@�@�m@t�@"�@=q@��@&�@ �@+@�R@��@v�@V@5?@$�@�@��@��@O�@�@�@�D@z�@j@I�@I�@I�@1@ƨ@��@��@C�@C�@C�@�@�\@�\@�\@�\@�\@�\@~�@~�@�\@^5@M�@-@�^@��@��@��@hs@&�@��@�@�@�P@��@|�@;d@�y@E�@$�@$�@$�@$�@5?@@�T@��@��@`B@/@/@?}@�@�@�@�@V@��@�@�@(�@(�@�
@��@t�@o@@
�H@
��@
�\@	��@  @\)@K�@K�@K�@+@;d@;d@;d@;d@+@�@+@+@+@;d@;d@��@�+@5?@V@(�@S�@�@n�@�@�#@&�@&�@%@ 1'@ A�?�;d?�O�?��?�(�?���?��u?�Q�?�Q�?�ff?��j?�?��?�!?�7?���?�v�?���?�p�?�V?�I�?��?���?���?��#?�Q�?�ȴ?��T?�`B?�?��?�j?�j?��?�F?��?��?�hs?��`?߾w?�;d?��?ݲ-?�(�?ۅ?ڟ�?ٺ^?��?�r�?�1'?և+?�E�?�?��?�Z?���?ӕ�?ӕ�?�o?�n�?�M�?�J?��?ѩ�?�hs?� �?�  ?�  ?�  ?��;?ϝ�?�\)?Η�?�5??Ͳ-?Ͳ-?���?�V?�(�?˥�?˥�?�?�~�?�=q?�=q?�X?�X?ȴ9?ȴ9?ȓu?�r�?�Q�?�b?Ƨ�?�E�?�?Ł?Ł?���?�z�?ě�?�Z?�9X?Õ�?���?�n�?�-?�hs?��`?�bN?�A�?�A�?�bN?�bN?��?�bN?��?�bN?�bN?��?��?�  ?�  ?��;?���?���?�|�?���?�|�?�;d?��R?��?���?�v�?�v�A�ffA�l�A�n�A�l�A�jA�l�A�hsA�n�A�r�A�p�A�p�A�t�A�r�A�p�A�jA�l�A�jA�jA�n�A�n�A�v�A�v�A��A�~�A��+A��A��A��A��A�|�A�v�A�z�A�x�A�z�A�~�A�~�A��A��A��+A��+A��+A��7A��7A��7A��7A��+A��7A��7A��7A��7A��+A��7A��PA��PA��DA��DA��PA��\A��PA��DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
�}B
��B
ÖBhBZBbNBl�B�B�oB��B��B��B��B��B�B�-B�^B�^B�^B�^B�dB�jBƨB�#B�B��B%BDBVBVB\BoBoBhB\BVBB��B%BbBbB�B�B,B49B<jBA�BI�BN�BT�B\)B^5B_;B_;BaHBaHB^5B_;B_;B_;BZBN�BK�BG�B>wB6FB1'B/B,B$�B�B�B�B�B�B�B�B�BPB  B�B�ZB�BB�B��B��B�}B�3B��B|�BVBD�B?}B,B!�BoBPBB
��B
�B
�B
��B
�}B
��B
��B
�oB
�7B
~�B
y�B
q�B
ffB
S�B
M�B
>wB
(�B
!�B
�B
  B	�B	�`B	��B	��B	�B	�%B	ffB	XB	<jB	#�B	�B	(�B	�B	�B	PB��B�BɺB�-B��B��B��B��B��B��B��B��B��B�uB�B~�B{�Bw�Bt�Bp�BiyBgmBdZBbNB]/B^5B`BB]/B[#BXBR�BS�BQ�BM�BM�BK�BJ�BH�BH�BK�BJ�BG�BH�BI�BH�BG�BF�B?}B?}B=qB=qB:^B:^B:^B;dB;dB>wB>wB?}BA�B<jB>wB>wB<jB<jB<jB<jB;dB<jB<jB9XB7LB7LB8RB<jB9XB0!B2-B0!B0!B/B1'B2-B33B5?B7LB9XB:^B;dB;dB9XB?}BI�B@�BA�B@�BA�BF�BR�B[#BW
BYBS�B^5Bv�B{�B|�B~�B�B�B�B�B�B�B�B�B�B�B�+B�1B�1B�7B�JB��B��B��B��B��B��B��B�B�LB�wB��BÖBĜBƨBȴBɺB��B��B��B��B��B��B�B�
B�)B�HB�TB�NB�HB�HB�;B�;B�B�5B�HB�BB�NB�`B�sB�B�B�B�B�B��B��B	B	B	DB	bB	�B	�B	#�B	,B	/B	33B	7LB	8RB	8RB	8RB	8RB	<jB	<jB	=qB	<jB	@�B	A�B	B�B	D�B	E�B	G�B	H�B	H�B	J�B	M�B	L�B	N�B	O�B	P�B	O�B	O�B	P�B	R�B	XB	\)B	cTB	dZB	ffB	ffB	iyB	l�B	o�B	q�B	q�B	q�B	s�B	s�B	s�B	v�B	y�B	z�B	{�B	|�B	� B	� B	� B	� B	�B	�B	�B	�B	�7B	�DB	�=B	�bB	�\B	�\B	�bB	�bB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�3B	�?B	�9B	�FB	�LB	�RB	�^B	�dB	��B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�;B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B
	7B
	7B
DB
PB
PB
PB
PB
VB
VB
VB
\B
\B
hB
hB
hB
hB
hB
oB
uB
{B
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
$�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
&�B
'�B
(�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
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
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
:^B
9XB
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
@�B
@�B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
C�B
D�B
C�B
C�B
D�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
D�B
D�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
N�B
M�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
P�B
P�B
P�B
Q�B
P�B
P�B
Q�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
S�B
VB
VB
VB
VB
T�B
VB
VB
VB
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
VB
VB
W
B
XB
YB
XB
XB
YB
XB
XB
XB
XB
YB
XB
YB
ZB
ZB
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
]/B
]/B
\)B
\)B
]/B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
_;B
_;B
`BB
_;B
_;B
aHB
aHB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
ffB
e`B
ffB
ffB
gmB
hsB
iyB
iyB
iyB
iyB
jB
iyB
jB
iyB
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
k�B
l�B
l�B
l�B
l�B
m�B
l�B
m�B
n�B
m�B
n�B
n�B
m�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
p�B
t�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
v�B
u�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
z�B
z�B
z�B
y�B
y�B
z�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
y�B
y�B
y�B
{�B
z�B
{�B
|�B
}�B
}�B
~�B
� B
� B
� B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�%B
�1B
�+B
�+B
�7B
�=B
�DB
�DB
�DB
�JB
�PB
�PB
�VB
�VB
�\B
�\B
�hB
�bB
�bB
�bB
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�oB
�oB
�oB
�uB
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�!B
�'B
�!B
�'B
�'B
�!B
�!B
�!B
�'B
�!B
�'B
�'B
�-B
�'B
�-B
�-B
�-B
�-B
�-B
�-B
�3B
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
��B
B
��B
��B
��B
��B
��B
��B
B
��B
��B
�wB
�}B
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
�}B
��B
�}B
��B
��B
��B
��B
��B
��B
��B
�}B
�}B
��B
��B
�}B
�}B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B
�_B
�ZB
�ZB
�`B
�ZB
�[B
�[B
�[B
�\B
�\B
�\B
�]B
�^B
�^B
�_B
�_B
�ZB
�[B
�aB
�uBGBY�Bb.BllB��B�QB�vB��B��B��B��B��B�B�EB�EB�FB�FB�MB�TBƒB�B��B��BB1BCBDBJB^B_BXBMBGBB��BBVBVB�B�B+�B4/B<aBA�BI�BN�BT�B\"B^/B_6B_6BaDBaDB^2B_8B_9B_9BZBN�BK�BG�B>xB6GB1)B/B,B$�B�B�B�B�B�B�B�B�BXB 	B�B�dB�LB�(B�B��B��B�@B��B|�BVBD�B?�B,B!�BB`B*B
�B
�B
�B
��B
��B
�	B
��B
��B
�LB
B
y�B
q�B
f}B
TB
M�B
>�B
)B
!�B
�B
 B	�B	�zB	��B	��B	�B	�AB	f�B	X,B	<�B	#�B	�B	)B	�B	�B	oB��B�B��B�MB��B��B��B��B��B��B��B��B��B��B�EB B|Bw�Bt�Bp�Bi�Bg�Bd�BbxB]YB^`B`mB][B[OBX=BSBT%BRBNBNBK�BJ�BH�BH�BK�BJ�BG�BH�BI�BH�BG�BF�B?�B?�B=�B=�B:�B:�B:�B;�B;�B>�B>�B?�BA�B<�B>�B>�B<�B<�B<�B<�B;�B<�B<�B9�B7�B7�B8�B<�B9�B0eB2qB0fB0fB/aB1mB2tB3zB5�B7�B9�B:�B;�B;�B9�B?�BJB@�BA�B@�BA�BF�BSBB[sBW[BYhBTJB^�BwB|:B}BBNB�aB�oB�hB�iB�cB�jB�jB�xB�xB�sB��B��B��B��B��B��B��B��B��B�B�1B�QB�oB��B��B��B��B�B�B�B� B�4B�:B�MB�[B�aB�hB�nB�uBܔB�B��B�B�B�BߪBߪBڍBޥB�B�B��B��B��B��B�B�B�B� B�RB�^B	~B	�B	�B	�B	B	'B	$SB	,�B	/�B	3�B	7�B	8�B	8�B	8�B	8�B	<�B	<�B	=�B	<�B	AB	BB	CB	E!B	F(B	H5B	I;B	I<B	KIB	N\B	MVB	OcB	PjB	QpB	PkB	PkB	QrB	SB	X�B	\�B	c�B	d�B	f�B	f�B	jB	mB	p1B	r=B	r>B	r>B	tKB	tLB	tLB	w`B	zrB	{yB	|�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	� B	�EB	�dB	�eB	�eB	�fB	�TB	�tB	�nB	�oB	�vB	�|B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�9B	�@B	�MB	�TB	�UB	�[B	�bB	�iB	�iB	�pB	�pB	�qB	�wB	�B	̆B	͌B	ΓB	ΓB	ϚB	СB	СB	ТB	УB	УB	ѪB	ұB	ҲB	ӹB	ӹB	ӺB	ӺB	��B	��B	ӼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	� B	�&B	�'B	�(B	�.B	�5B	�5B	�<B	�<B	�=B	�>B	�DB	�EB	�LB	�SB	�ZB	�`B	�aB	�zB	�{B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
B
�B
�B
B
B
B
B
B
B
&B
'B
	-B
	.B
	.B

5B

6B
CB
PB
PB
QB
RB
XB
YB
YB
`B
aB
mB
nB
nB
oB
pB
wB
~B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
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
�B
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
�B
�B
�B
�B
�B
 �B
 �B
!�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
%B
%B
&B
%B
%B
&B
&B
&B
'B
'B
'B
'B
'B
(B
(B
(B
)&B
( B
( B
(!B
)(B
))B
)*B
)*B
($B
($B
(%B
'B
(&B
).B
*4B
+;B
,AB
,BB
-IB
-IB
.PB
.PB
.QB
.QB
.RB
/YB
/YB
/ZB
/ZB
0bB
0cB
1iB
2pB
2qB
2qB
2rB
2rB
3yB
4B
4�B
5�B
5�B
6�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
:�B
;�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
>�B
?�B
?�B
?�B
A�B
A�B
@�B
@�B
A�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
D�B
F B
D�B
D�B
FB
D�B
D�B
D�B
FB
D�B
D�B
E B
FB
FB
EB
GB
GB
GB
GB
GB
GB
GB
HB
HB
IB
I B
I!B
I!B
I"B
I"B
I#B
I$B
I$B
I%B
I%B
I&B
I'B
I'B
J.B
J.B
J/B
K6B
J0B
K7B
K7B
K8B
L@B
MFB
MGB
MGB
LBB
MIB
MIB
NPB
NPB
OWB
OXB
P^B
OYB
QeB
QfB
QgB
QgB
QhB
QhB
RoB
RpB
RpB
RqB
RqB
RrB
SyB
SzB
RtB
RtB
RuB
S}B
RvB
RwB
S~B
RxB
RxB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
U�B
W�B
W�B
W�B
W�B
V�B
W�B
W�B
W�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
W�B
W�B
X�B
Y�B
Z�B
Y�B
Y�B
Z�B
Y�B
Y�B
Y�B
Y�B
Z�B
Y�B
Z�B
[�B
[�B
\�B
\�B
\�B
]�B
\�B
]�B
]�B
]�B
^�B
^�B
]�B
]�B
^�B
_�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
bB
`�B
`�B
bB
a B
a B
cB
cB
dB
dB
dB
dB
eB
dB
eB
eB
e B
e B
f'B
g-B
g.B
g/B
h5B
g0B
h6B
h7B
i?B
jEB
kLB
kLB
kMB
kNB
lTB
kOB
lUB
kPB
lWB
lWB
lXB
lXB
m_B
m`B
m`B
maB
maB
mbB
mcB
mcB
njB
njB
nkB
nlB
orB
nmB
otB
p{B
ouB
p|B
p}B
ovB
p~B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
r�B
v�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
x�B
w�B
x�B
x�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
|�B
|�B
|�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
|�B
{�B
{�B
{�B
{�B
{�B
}�B
|�B
~B
B
�B
�B
�"B
�+B
�-B
�1B
�9B
�=B
�9B
�IB
�KB
�\B
�_B
�aB
�kB
�tB
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�
B
�B
�B
�B
�B
�B
�(B
�*B
�.B
�2B
�?B
�CB
�LB
�UB
�_B
�bB
�kB
�nB
�wB
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�"B
�+B
�.B
�2B
�4B
�8B
�@B
�JB
�GB
�IB
�LB
�[B
�^B
�aB
�dB
�mB
�jB
�sB
�|B
�B
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
��B
��B
��B
��B
��B
� B
�	B
�_B
�_B
�YB
�YB
�YB
�_B
�eB
�_B
�YB
�_B
�_B
�YB
�_B
�_B
�eB
�_B
�_B
�_B
�_B
�_B
�YB
�eB
�YB
�_B
�MB
�SB
�ZB
�ZB
�TB
�TB
�`B
�`B
�`B
�`B
�ZB
�ZB
�ZB
�ZB
�[B
�[B
�[B
�[B
�UB
�[B
�UB
�[B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�WB
�]B
�]B
�WB
�WB
�]B
�]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201905082200502021061413561420210614135614202106171314302021061713143020210617131430201905082200502021061413561420210614135614202106171314302021061713143020210617131430PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019050822005020190508220050  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019050822005020190508220050QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019050822005020190508220050QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713151820210617131518IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                