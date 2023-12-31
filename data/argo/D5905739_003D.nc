CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  i   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:41Z creation      
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
resolution        =���   axis      Z        H  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  U8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  cT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  qp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ׼   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ؀   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ؠ   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ب   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ذ   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ظ   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��Argo profile    3.1 1.2 19500101000000  20180724220241  20210617131451  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�cp�s�@�cp�s�11  @�cp�-�0@�cp�-�0@6�R�J��@6�R�J���c��䣃'�c��䣃'11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  ?�33@@  @�33@�ff@�33@�33A��A��A#33A@  Aa��A�  A���A�ffA���A�  A�  A�33A�33A�33B33B  B33B!33B)33B0��B8ffB@  BH  BO��BW33B`  Bh��Bp  BxffB�ffB�  B�  B�33B�  B�  B���B�  B�  B�  B�33B�  B�33B�33B�33B�33B�33B�ffB�  B�ffB�  B�33B�  B�  B���B�  B虚B�ffB�33B�33B�33B�33C �C�C  C�C�C
�C  C  C�C33C  C  C�fC�C�C�fC �C"33C$�C&  C'�fC*�C,�C.�C0�C1�fC3�fC5�fC8�C:�C<  C>  C@  CB  CD  CF�CH33CJ�CL  CN�CP�CR  CS�fCV�CX�CZ�C\�C^�C_�fCb�Cd33Cf33Ch  Ci�fCl  Cn�Cp  Cq�fCs�fCv  Cx�Cz  C|�C~33C��C��C��C��C��C��C��C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��C�  C��3C��3C�  C��C��C�  C�  C��C��C��C��C��C��C��C��C��C�  C�  C��C��C��C��C�  C��C��C��C�  C�  C��C��C��C�  C��C��C��C�  C��C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C��C��C��C�  C��C�  C��C��C��C�  C�  C��C��C�  C�  C�  C��C��C�  C��C��C��C��C��C��C�  C�  C�  C��3C��C��C��C�  C��3C�  C��C�  C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C��C��C��fC�  C��C��C�  C���DS3DY�D` Dl�Ds3Dy�D��D��D	��D
�fD��D� Dy�Ds3Dl�DY�DY�D@ D  DfDٚD�3D�fD33D��D�3D @ D!3D"� D#��D$��D&l�D'@ D)3D)� D+�3D,S3D-� D.��D/��D1FfD2� D4  D5ffD6�D7�3D9S3D:&fD;��D<� D>9�D>��D@s3DA� DB�fDD  DEl�DFٚDG�3DI&fDJ�fDKffDM�DM�fDO��DPffDQ��DS� DT33DU��DW3DW� DY�DZL�D[s3D]  D^�D_s3D`S3Da� Db�3DdY�DeL�Df��Dg�3Di` DjFfDk�3DlٚDn  DoffDpl�Dq�fDs&fDtffDu�3Dv��Dx&fDyL�Dzy�>L��>���>���>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���>���>���>���?   ?��?333?fff?�  ?���?�ff?���?ٙ�@   @��@&ff@333@Fff@Y��@l��@�33@���@���@�ff@�  @���@ə�@�ff@�33@�  @���A��A��A��A��A#33A)��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411444414141414144444141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               ?L��?�  @��@`  @�33@�ff@�33@�33A	��A��A+33AH  Ai��A�  A���A�ffA���A�  A�  A�33A�33B��B	33B  B33B#33B+33B2��B:ffBB  BJ  BQ��BY33Bb  Bj��Br  BzffB�ffB�  B�  B�33B�  B�  B���B�  B�  B�  B�33B�  B�33B�33B�33B�33B�33B�ffB�  B�ffB�  B�33B�  B�  B���B�  B陚B�ffB�33B�33B�33B�33C ��C��C� C��C��C
��C� C� C��C�3C� C� CffC��C��CffC ��C"�3C$��C&� C(ffC*��C,��C.��C0��C2ffC4ffC6ffC8��C:��C<� C>� C@� CB� CD� CF��CH�3CJ��CL� CN��CP��CR� CTffCV��CX��CZ��C\��C^��C`ffCb��Cd�3Cf�3Ch� CjffCl� Cn��Cp� CrffCtffCv� Cx��Cz� C|��C~�3C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�Y�C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�@ C�33C�33C�@ C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�L�C�L�C�@ C�@ C�L�C�L�C�Y�C�L�C�@ C�L�C�L�C�L�C�@ C�@ C�Y�C�L�C�L�C�@ C�L�C�L�C�L�C�@ C�Y�C�@ C�@ C�L�C�L�C�L�C�L�C�Y�C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�L�C�@ C�L�C�@ C�L�C�Y�C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�33C�Y�C�Y�C�L�C�@ C�33C�@ C�L�C�@ C�@ C�L�C�@ C�33C�@ C�L�C�L�C�Y�C�@ C�@ C�@ C�L�C�L�C�&fC�@ C�L�C�Y�C�@ C���Ds3Dy�D� D��D�3D��D��D��D	��D
�fD��D� D��D�3D��Dy�Dy�D` D@ D&fD��D�3D�fDS3D��D�3D ` D!33D#  D#ٚD$��D&��D'` D)33D*  D+�3D,s3D.  D.��D0�D1ffD2� D4  D5�fD69�D7�3D9s3D:FfD;��D<� D>Y�D?�D@�3DB  DB�fDD  DE��DF��DG�3DIFfDJ�fDK�fDM9�DNfDO��DP�fDR�DS� DTS3DU��DW33DW� DY,�DZl�D[�3D]@ D^9�D_�3D`s3Da� Dc3Ddy�Del�Df��Dg�3Di� DjffDk�3Dl��Dn  Do�fDp��DrfDsFfDt�fDu�3Dw�DxFfDyl�Dz��?333G�O�G�O�G�O�?333?L��G�O�G�O�G�O�G�O�?L��G�O�?L��G�O�?L��G�O�?L��G�O�?333G�O�G�O�G�O�G�O�G�O�?L��G�O�?fff?�  ?���?���?�33?�  ?ٙ�?�ff@ff@��@   @,��@Fff@S33@fff@y��@�ff@�33@���@���@�ff@�  @���@ٙ�@�ff@�33A   AffA��A��A��A$��A+33A1��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411444414141414144444141111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               @ @ �@ �@ {@ �@ #�@ )�@ 0x@ 7L@ >@ E�@ Q�@ `B@ m:@ {�@ ��@ ��@ ��@ �~@ �w@ �@ ��@ �@ ��@v@@ �@-�@:�@G�@UU@bN@oF@~K@�P@��@��@�F@@�7@ލ@�@�,@%@{@""@/�@>@K@Yn@g@t�@�d@�@�a@��@��@��@�O@�H@��@��@
=@�@&�@3�@A�@O0@\�@j@x&@�@�u@�@��@��@�c@׹@�@�Y@  @�@�@)�@5�@D�@SI@`B@m:@z3@�7@��@��@�-@�w@�@��@��@�q@j@@�@,`@:@H]@V�@c�@p�@~�@��@��@��@��@�>@��@ލ@�4@�~@�@�@#�@/�@<�@K@Yn@ff@s_@�@�\@��@��@�@�W@�O@��@�@�E@
�@�@&;@33@A�@O�@\)@i�@ww@�@�u@�m@�@��@�@׹@�@�@  @�@�@(�@7L@D�@Q�@_�@m�@{�@�7@��@��@��@��@�|@�#@�@��@	@	�@	 @	-@	:@	H]@	V@	c�@	p�@	~K@	�P@	�H@	��@	��@	�>@	��@	ލ@	�@	��@
�@
{@
"�@
0x@
>@
K�@
Z@
g@
t�@
��@
�\@
�@
�Y@
�@
ƨ@
Ӡ@
��@
��@
�E@�@�@%�@33@A�@O0@\)@i�@ww@��@�u@�m@��@�k@�@׹@�`@�@  @�@O@(G@7�@E�@R�@_�@l�@z�@�7@�0@��@�-@�&@�@�t@��@�q@�@@�@,`@:�@H]@S�@b�@qS@�@��@�u@�o@�m@j@ @<@X@uk@��@�@ƨ@��@�@2�@M$@�@�U@��@�@j@�@M�@e	@{�@�M@��@�(@�@.l@_�@v�@��@��@�
@�@�@M$@a�@��@�a@�J@��@�@33@Yn@l�@�<@Ĝ@�#@1@�@Ji@]�@�+@�@��@�@@5�@I�@t�@��@�-@��@�q@$�@:�@e�@�\@�y@��@��@j@&�@I@hs@�0@��@խ@�@@5?@[z@uk@�a@�^@�@�E@ �@C�@b�@�7@�5@�|@�@�@5?@X@x&@��@��@ ^G�O�G�O�G�O�@ ^@ G�O�G�O�G�O�G�O�@ G�O�@ G�O�@ G�O�@ G�O�@ ^G�O�G�O�G�O�G�O�G�O�@ G�O�@ �@ j@ @ �@ %@ �@ 1@ �@ 
�@ �@ �@ @ �@ @ *@ 6@ B@ �@ 
@  �@ #�@ %�@ (G@ +@ -�@ 0x@ 33@ 5�@ 8�@ <@ ?}@ B�@ E�@ H]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aŕ�AœuAŕ�AœuAőhAœuAŕ�AœuAŏ\AŋDAŋDAŃA�|�A�x�A�n�A�VA�XA�=qA�(�A�bA��HA���AĬA�l�A�%Aá�A�|�A�I�A���A��^A�-A�\)A���A���A��A�XA�O�A��A���A�p�A�7LA���A�bNA��A�p�A���A��yA��\A�\)A�Q�A�I�A�I�A�?}A�&�A��A��7A�?}A�&�A��RA��uA�1A�t�A�33A���A�"�A���A���A�33A��A��
A�%A�1A���A���A��A���A���A��;A�?}A��`A�&�A��A�A�ȴA��PA���A�ffA�A��jA��+A�E�A�ƨA�?}A�M�A��FA��A�oA��A�A�ȴA��PA�`BA��7A��-A���A�C�A�%A�ffA�G�A�t�A��mA�A�A�x�A�A�A��9A�dZA�1A��wA���A}hsA{��A{O�Az�9AyS�Aw/Av��Av��Aut�As�Aq��Ap�jAn��Al�/Ak`BAj �AiVAg7LAf-AeC�Ad�+Ac�7AbĜAb  AaK�A`�`A_�;A]oAZ  AX��AX�AW��AV�+AU�PATȴARQ�AQ��AP�AO�PANn�AM��AM/AL�+AL�AKƨAK&�AJAI��AIXAH�AF�AFAE��AEoADZADA�AC�ACG�AA��A=�A:E�A7\)A5ƨA3�#A2=qA0��A/�A.�A.bNA-ƨA+��A* �A)oA'��A&�A$��A#A"��A"ffA!�#A!�A 9XA~�A�A=qA7LAVA��AȴAz�A��A�#A�#A�#A�;A�wAC�A%AA��AI�A�#Al�A?}A��A�#A�Ap�A�RA�
A�`A"�A�7AO�A	��A�#A��A;dA�A��A"�AJAS�AC�A7LA�A�\AA~�A ��@���@�Q�@��j@���@�1@�\)@�R@��@�j@�u@��T@�r�@�33@�!@��#@� �@֧�@�(�@�b@�;d@���@��@ˍP@���@š�@þw@�J@�%@��@���@�G�@�K�@�ff@�Q�@�9X@�-@�`B@��
@�/@�bN@���@��y@�J@���@���@�\)@��y@��@�r�@�=q@�$�@���@��@���@�E�@�@�Z@��\@�Q�@��m@�"�@��\@�`B@�`B@�`B@��u@�z�@�ƨ@�33@��y@���@�$�@�/@���@�bN@�Q�@��@��m@��y@���@�~�@�@�?}@�V@�Ĝ@��@�r�@���@�ȴ@�J@��#@���@��m@�^5@���@��h@�x�@�G�@���@���@���@��@�J@���@K�@~�R@}�@|�j@|j@|(�@{S�@{Aŗ�Aŏ\AœuAœuAŕ�Aŕ�Aŗ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�AœuAœuAœuAœuAœuAœuAőhAőhAőhAœuAœuAœuAőhAŕ�Aŕ�Aŕ�Aŕ�Aŕ�AœuAőhAŏ\AőhAœuAőhAŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�AőhAŏ\AōPAōPAŉ7AŋDAŉ7AŋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               Aŕ�AœuAŕ�AœuAőhAœuAŕ�AœuAŏ\AŋDAŋDAŃA�|�A�x�A�n�A�VA�XA�=qA�(�A�bA��HA���AĬA�l�A�%Aá�A�|�A�I�A���A��^A�-A�\)A���A���A��A�XA�O�A��A���A�p�A�7LA���A�bNA��A�p�A���A��yA��\A�\)A�Q�A�I�A�I�A�?}A�&�A��A��7A�?}A�&�A��RA��uA�1A�t�A�33A���A�"�A���A���A�33A��A��
A�%A�1A���A���A��A���A���A��;A�?}A��`A�&�A��A�A�ȴA��PA���A�ffA�A��jA��+A�E�A�ƨA�?}A�M�A��FA��A�oA��A�A�ȴA��PA�`BA��7A��-A���A�C�A�%A�ffA�G�A�t�A��mA�A�A�x�A�A�A��9A�dZA�1A��wA���A}hsA{��A{O�Az�9AyS�Aw/Av��Av��Aut�As�Aq��Ap�jAn��Al�/Ak`BAj �AiVAg7LAf-AeC�Ad�+Ac�7AbĜAb  AaK�A`�`A_�;A]oAZ  AX��AX�AW��AV�+AU�PATȴARQ�AQ��AP�AO�PANn�AM��AM/AL�+AL�AKƨAK&�AJAI��AIXAH�AF�AFAE��AEoADZADA�AC�ACG�AA��A=�A:E�A7\)A5ƨA3�#A2=qA0��A/�A.�A.bNA-ƨA+��A* �A)oA'��A&�A$��A#A"��A"ffA!�#A!�A 9XA~�A�A=qA7LAVA��AȴAz�A��A�#A�#A�#A�;A�wAC�A%AA��AI�A�#Al�A?}A��A�#A�Ap�A�RA�
A�`A"�A�7AO�A	��A�#A��A;dA�A��A"�AJAS�AC�A7LA�A�\AA~�A ��@���@�Q�@��j@���@�1@�\)@�R@��@�j@�u@��T@�r�@�33@�!@��#@� �@֧�@�(�@�b@�;d@���@��@ˍP@���@š�@þw@�J@�%@��@���@�G�@�K�@�ff@�Q�@�9X@�-@�`B@��
@�/@�bN@���@��y@�J@���@���@�\)@��y@��@�r�@�=q@�$�@���@��@���@�E�@�@�Z@��\@�Q�@��m@�"�@��\@�`B@�`B@�`B@��u@�z�@�ƨ@�33@��y@���@�$�@�/@���@�bN@�Q�@��@��m@��y@���@�~�@�@�?}@�V@�Ĝ@��@�r�@���@�ȴ@�J@��#@���@��m@�^5@���@��h@�x�@�G�@���@���@���@��@�J@���@K�@~�R@}�@|�j@|j@|(�@{S�@{Aŗ�Aŏ\AœuAœuAŕ�Aŕ�Aŗ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŗ�Aŕ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�Aŗ�AœuAœuAœuAœuAœuAœuAőhAőhAőhAœuAœuAœuAőhAŕ�Aŕ�Aŕ�Aŕ�Aŕ�AœuAőhAŏ\AőhAœuAőhAŕ�Aŕ�Aŕ�Aŕ�Aŕ�Aŕ�AőhAŏ\AōPAōPAŉ7AŋDAŉ7AŋDG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ɺB
ɺB
ɺB
ȴB
ɺB
ɺB
ɺB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
��B
��B
��B
��B
�BB
��B%BbB&�BL�B}�B�uB��B��B��B��B�-BB��B�HB�By�B�DB�uB��B��B��B��B��B��B��B��B��B��B�B�B�-B�'B�-B�3B�9B�XB�RB�LB�9B�RB�dB�wB��B�}B�dB�FB�FB�3B�B��B��B��B��B��B��B�\B�\B�\B�VBz�Bw�Bs�Bq�Bq�Bm�BZBM�B>wB9XB1'B �BuB\B�TB�^B��B�{B�Bu�Bk�B`BBXB-B�BuB
=B
�B
��B
ǮB
ÖB
�}B
�wB
�JB
_;B
L�B
F�B
?}B
2-B
'�B
%�B
$�B
 �B
�B
{B
1B	��B	�B	�ZB	�5B	�B	��B	ÖB	�wB	�XB	�3B	�B	��B	��B	��B	��B	�B	o�B	cTB	aHB	YB	N�B	H�B	F�B	C�B	B�B	=qB	7LB	1'B	.B	,B	+B	(�B	&�B	"�B	�B	�B	�B	hB	1B	%B	B��B��B��B��B��B�B�BB��BǮB�jB�FB�B�B��B��B��B��B��B��B�oB�\B�=B�B�B�B�B~�B}�Bz�Bw�Bs�Bn�Bo�Bn�Bm�Bm�BjBk�Bl�Bl�Bl�Bl�Bl�Bn�Bl�Bk�Br�Bu�Bx�B{�B{�B{�Bx�Bw�Bx�Bx�Bx�Bt�BdZB]/BffBVBG�BA�BS�BbNB`BBffBn�Bk�Bk�Bk�BjBffBdZBn�BhsBW
BL�BA�B>wB=qBB�BB�BC�BI�BYBffBdZBF�BN�BL�BG�B=qB>wB?}B?}BC�BH�BM�BP�BT�BW
BW
BYBZBcTBffBo�B� B�B�B�%B�=B�\B��B��B��B��B��B��B�B�HB�`B�B�B��B��B��B	B	,B	;dB	E�B	E�B	J�B	bNB	e`B	r�B	y�B	�%B	�+B	�1B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�FB	�RB	�wB	�}B	�}B	��B	��B	ÖB	ǮB	��B	��B	��B	��B	�B	�5B	�;B	�HB	�HB	�HB	�ZB	�mB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
ɺB
��B
ȴB
ɺB
ɺB
ɺB
ȴB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ɺB
ȴB
ɺB
ɺB
ɺB
ɺB
ȴB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ɺB
ɺB
ȴB
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               B
ɑB
ɑB
ɒB
ȌB
ɒB
ɓB
ɓB
ȍB
ɓB
ȎB
ȎB
ȏB
ȏB
ȐB
ȐB
ȑB
ȒB
ȒB
əB
əB
˧B
ʹB
κB
��B
�$B
��BBFB&�BL�B}�B�[B��B��B��B��B�B�xB˱B�2B�By�B�/B�aB��B��B��B��B��B��B��B��B��B��B��B�B�!B�B�"B�(B�/B�NB�IB�DB�1B�KB�]B�qB�}B�xB�`B�BB�CB�0B�B��B��B��B��B��B��B�]B�^B�^B�YBz�Bw�Bs�Bq�Bq�Bm�BZ#BM�B>~B9`B1/B �B~BfB�^B�hB��B��B�*Bu�Bk�B`OBXB-B�B�B
LB
��B
��B
ǾB
çB
��B
��B
�\B
_MB
L�B
F�B
?�B
2AB
(B
%�B
$�B
 �B
�B
�B
HB	�B	��B	�rB	�NB	�B	��B	ðB	��B	�sB	�OB	�0B	�B	��B	��B	��B	�8B	o�B	ctB	ahB	Y8B	N�B	H�B	F�B	C�B	B�B	=�B	7pB	1KB	.9B	,.B	+(B	)B	'B	"�B	�B	�B	�B	�B	[B	PB	=B�&B� B�!B�B��B��B�qB�'B��B��B�vB�KB�9B�!B�B�	B��B��B��B��B��B�rB�UB�HB�=B�=B2B~,B{BxBs�Bn�Bo�Bn�Bm�Bm�Bj�Bk�Bl�Bl�Bl�Bl�Bl�Bn�Bl�Bk�Br�BvByB|+B|,B|,ByBxByByByBuBd�B]xBf�BVNBG�BA�BTCBb�B`�Bf�Bn�Bk�Bk�Bk�Bj�Bf�Bd�Bn�Bh�BW\BMBA�B>�B=�BB�BB�BC�BJBYnBf�Bd�BGBO4BM(BH
B=�B>�B?�B?�BC�BIBN8BQKBUeBWrBWtBY�BZ�Bc�Bf�BpB�rB��B��B��B��B��B�,B�-B�TB�gB�{B�UBڠB��B��B�B�B�FB�NB��B	�B	,�B	;�B	F6B	F6B	KWB	b�B	e�B	sKB	zwB	��B	��B	��B	�
B	�B	�,B	�GB	�NB	�UB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�1B	�9B	�9B	�AB	�IB	�WB	�qB	˅B	ΙB	ϠB	��B	��B	� B	�B	�B	�B	�B	�,B	�AB	�HB	�VB	�iB	�~B	��B	��B	��B	��B	��B	��B	��B	��B
ɑB
ʘB
ȋB
ɑB
ɑB
ɑB
ȋB
ȋB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ɑB
ȋB
ɑB
ɑB
ɑB
ɑB
ɑB
ȋB
ɑB
ȋB
ɑB
ɑB
ɑB
ɑB
ȋB
ɑB
ɑB
ɒB
ȌB
ȌB
ȌB
ȌB
ȌB
ɒB
ɒB
ȌB
ɒB
ɓB
ɓB
ɓB
ɓB
ȍB
ȍB
ȍB
ȍB
ɓB
ȎB
ȎB
ȎB
ȎB
ȎG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                               <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202412021061413551020210614135510202106171311272021061713112720210617131127201807242202412021061413551020210614135510202106171311272021061713112720210617131127PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024120180724220241  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024120180724220241QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024120180724220241QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145120210617131451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                