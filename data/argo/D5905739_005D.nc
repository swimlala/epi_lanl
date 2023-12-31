CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:42Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        b�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  m�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        p�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  {�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ~�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ֬   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �LArgo profile    3.1 1.2 19500101000000  20180724220242  20210617131452  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c�*�@�c�*�11  @�c�""9 @�c�""9 @6�9.�<@6�9.�<�c�o?R�&�c�o?R�&11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @Fff@�33@�33@�ff@�ffA��A  A$��AD��Ac33A���A�33A�  A���A�  A�33A�  A�B ��BffB  B��B   B(��B0ffB8ffB?��BH  BP��BX  B_��Bh  BpffBxffB�  B�  B�ffB�ffB�ffB�  B�  B�  B�  B�33B�ffB�33B�  B�  B�  B�  B�  B�  B���B�ffB�ffB�33B���B�33B�33B�  B�33B�33B�33B�ffB�33B�33C �C�fC  C�C33C
�C�fC�fC�fC  C�C  C�fC  C  C�C �C"  C$  C%�fC'�fC*�C,33C.�C0�C233C4�C633C8�C:�C<  C=�fC@�CB  CD  CF�CH  CJ�CL  CM�fCO�fCR  CT�CV  CX  CZ  C\�C^�C`  Cb�Cd�Cf�Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cw�fCz  C|�C~�C��C��C��C�  C�  C��C�  C��3C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C��C��C�  C��C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C��C��C�  C��C��C�  C�  C�  C�  C��C��C��C��C��C�  C��C��C�  C��C��C��C��C�  C��C��C��C��C��3C��3C�  C��C��C��C��C��C�  C��C��C��C��C��C�  C��3C��3C��3C�  C��C�  C��C��C��C��C�  C�  C��C��C��C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C��C�  C�  C��3C��3C�  C��C��C��C��C���D�DffD��D��DL�D��D�fD	��D&fDL�DY�DY�D@ D�D3D��D� DٚD� D3D&fDS3D�fD��DٚD   D!&fD"FfD$ffD%l�D&s3D's3D(s3D*l�D+` D,Y�D-` D/FfD0,�D1�D2�fD3�3D5,�D6��D7s3D8��D:�fD;��D<y�D>ffD?Y�D@S3DBffDCffDDffDE` DG@ DH�DH�3DJ� DKY�DL� DN�DOS3DPy�DQ�3DR��DT` DU� DV�fDW�3DY3DZY�D[�3D]3D^�fD_9�D`�3Db  Dc�fDd33De��Dg3Dh�fDi9�Dj�3Dl33Dm��DnFfDo��Dq�DrffDs�3Dt� DvfDw�Dx� Dy` Dz��>���>���>���>���>���>���>���>���>���>���>���>���?   >���>���?   ?   ?   ?   ?   >���?   ?��?   ?333?333?fff?�  ?���?���?�ff?���?�ff?�33@��@��@,��@@  @L��@Y��@l��@�33@���@���@�33@���@���@�ff@�  @���@���@���A33A33A33A��A   A(  A0  A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414411441444441141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ?fff?�33@   @fff@�33@�33@�ff@�ffA	��A  A,��AL��Ak33A���A�33A�  A���A�  A�33A�  A���B��B
ffB  B��B"  B*��B2ffB:ffBA��BJ  BR��BZ  Ba��Bj  BrffBzffB�  B�  B�ffB�ffB�ffB�  B�  B�  B�  B�33B�ffB�33B�  B�  B�  B�  B�  B�  B���B�ffB�ffB�33B���B�33B�33B�  B�33B�33B�33B�ffB�33B�33C ��CffC� C��C�3C
��CffCffCffC� C��C� CffC� C� C��C ��C"� C$� C&ffC(ffC*��C,�3C.��C0��C2�3C4��C6�3C8��C:��C<� C>ffC@��CB� CD� CF��CH� CJ��CL� CNffCPffCR� CT��CV� CX� CZ� C\��C^��C`� Cb��Cd��Cf��Ch� Cj� Cl��Cn��Cp� Cr� Ct� Cv� CxffCz� C|��C~��C�Y�C�Y�C�L�C�@ C�@ C�L�C�@ C�33C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�@ C�L�C�L�C�L�C�Y�C�L�C�@ C�L�C�33C�33C�@ C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�33C�@ C�L�C�L�C�L�C�@ C�@ C�Y�C�Y�C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�L�C�L�C�L�C�Y�C�L�C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�33C�33C�@ C�Y�C�Y�C�Y�C�Y�C�L�C�@ C�L�C�L�C�L�C�L�C�Y�C�@ C�33C�33C�33C�@ C�L�C�@ C�L�C�Y�C�L�C�L�C�@ C�@ C�L�C�Y�C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�L�C�@ C�@ C�L�C�@ C�@ C�L�C�@ C�@ C�33C�33C�@ C�Y�C�L�C�L�C�L�C���D,�D�fD��D�Dl�D��D�fD
�DFfDl�Dy�Dy�D` D9�D33D�D  D��D  D33DFfDs3D�fD��D��D   D!FfD"ffD$�fD%��D&�3D'�3D(�3D*��D+� D,y�D-� D/ffD0L�D19�D2�fD3�3D5L�D6ٚD7�3D9�D:�fD;��D<��D>�fD?y�D@s3DB�fDC�fDD�fDE� DG` DH9�DI3DJ� DKy�DL� DN9�DOs3DP��DQ�3DR��DT� DU� DV�fDW�3DY33DZy�D[�3D]33D^�fD_Y�D`�3Db@ Dc�fDdS3De��Dg33Dh�fDiY�Dj�3DlS3Dm��DnffDoٚDq9�Dr�fDs�3Du  Dv&fDw,�Dx� Dy� Dz��G�O�G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�?L��?fffG�O�G�O�?fffG�O�G�O�G�O�G�O�G�O�?fff?�  G�O�?�  G�O�?���?�33?�  ?���?ٙ�?�ff@ff@33@��@,��@9��@L��@`  @l��@y��@�ff@�33@���@���@�33@���@ə�@�ff@�  @���@���AffA33A33A33A!��A(  A0  A8  AA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414411441444441141411111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        @ �@ %@ �@ *@ �@ "�@ *S@ 1'@ 7L@ =q@ FQ@ S�@ `�@ m�@ z3@ ��@ ��@ ��@ ��@ �&@ �*@ ��@ ��@ ��@�@@ @-@:�@F�@UU@dZ@p�@}�@��@�H@��@��@@є@�;@��@�,@�@{@""@0x@>�@K�@X�@ff@t@��@�\@�@��@��@�W@�O@��@�@�E@
=@�@&;@3�@B8@O0@\�@j@v�@�@�u@��@��@�@ȴ@�[@�@�@  @�@O@(�@7L@D�@Q�@_�@l�@z3@�7@��@��@�-@��@�|@��@��@�q@j@b@g@,`@:@H]@UU@c�@p�@}�@�D@��@��@��@@�7@ލ@�4@�,@�@*@"�@/�@=q@K�@Yn@ff@t@��@�\@�U@��@�@ƨ@��@�@�@��@
=@�@%�@2�@A�@N�@\)@i�@ww@�@��@��@�@��@�@׹@�`@�@ �@�@�@(G@5�@DD@Q�@_�@m:@{�@�7@�0@��@�~@�&@�@�t@��@�q@	@	@	�@	-�@	;d@	G�@	V@	c�@	p�@	~K@	��@	��@	��@	��@	�>@	є@	ލ@	�@	��@
�@
{@
"�@
0x@
>@
K�@
X�@
g@
t�@
�d@
�@
�U@
��@
�R@
�W@
��@
�@
�L@
�E@
=@�@&;@3�@A�@O�@\)@i!@v�@�p@��@�@�@�k@��@׹@�`@�Y@  @V@�@)�@6�@DD@R�@`B@m:@z�@�7@�0@��@�-@�&@��@�#@�@��@�@b@�@-�@:�@H]@V@�@��@��@�@/@SI@uk@��@��@׹@� @�@/@b�@z3@��@�W@�H@��@�@S�@qS@�h@�-@є@�@@0x@O0@�7@�5@�2@܀@��@-�@G�@bN@~K@�-@��@�@�@'�@SI@}�@�h@�@��@^@�@O0@i!@��@�j@׹@�@�@@�@X@oF@�@��@�
@��@[@<�@Z�@x�@�A@��@�`@v@'�@Ji@oF@��@�j@ψ@��@�@D�@Wb@�@��@�|@��@�@1�@X@j@�@��@�#@��@�@>@Z@��@��@��G�O�G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�@ @ �G�O�G�O�@ �G�O�G�O�G�O�G�O�G�O�@ �@ jG�O�@ jG�O�@ �@ %@ �@ �@ 1@ �@ 
�@ J@ �@ @ b@ o@ {@ �@ 6@ B@ �@ 
@  �@ "�@ $�@ '�@ *S@ ,`@ /@ 2�@ 5�@ 7�@ ;d@ >�@ A�@ DD@ G�@ K@ O0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��yA��A��A��yA��yA��TA��#A��
A��`A��/AǴ9A�~�A��/AƅA�33Aũ�A�/A���Aĩ�A�v�A�(�AÙ�A��A���A��`A�AA��A�bNA���A�?}A��A���A��A��uA���A�~�A�r�A�~�A�r�A�^5A�ffA��#A��;A���A�33A�bA��-A��yA�O�A�K�A�{A���A��A��A���A�XA� �A���A��/A��wA���A�A��HA���A��9A���A�`BA��A���A��A�Q�A��A�  A�-A�|�A�1A�^5A��A�oA�%A��-A��A�?}A���A�t�A�bA�K�A�+A�&�A���A���A�`BA�{A��A��A�?}A��TA���A���A��jA�hsA�1'A���A�7LA�;dA���A��;A�ȴA��!A�1'A�%A��
A�E�A��A��9A�|�A�ZA~�`A{?}AyAxn�Au7LAr�yAoO�Ak��Aj�9Aj1Ai
=Ah��AhffAg�Ag"�Ae7LAd �AcS�A`z�A]�A]+A\��AZ��AZI�AX��AX(�AVjAS�^AR�HARZAR1'AR�AQ��APZAO�^ANn�AL�`ALffAK��AK�FAK;dAJ�AJE�AJ1'AJ  AI��AI/AH$�AFĜAFE�AEAD��ABn�AA�TA?��A>jA=?}A;��A:��A9S�A7��A6E�A4^5A3dZA37LA3�A2�/A2�A1t�A0�!A0-A/�wA/\)A.{A-C�A,JA*M�A)��A(��A(9XA'�-A'\)A&�RA&^5A%�A$ �A"�A!��A!"�A Q�A�AbNA��AE�Al�A%AĜA  AȴAffAp�AbA��A�A��A�hAn�AA7LA�PA^5A�A�^A+A1AA�hA
��A��A�;A&�A�A��A��A�;A��A%@�C�@�;d@�O�@���@��m@�ff@�&�@���@�@�X@���@�@�hs@�bN@���@�l�@��T@أ�@�Z@���@Ӿw@���@Ϯ@�hs@��T@�O�@���@��P@�@�bN@��+@�ff@��7@�\)@�-@�I�@�p�@��`@�b@�z�@���@�=q@��^@�V@�@�o@��h@���@��@�/@�1@��+@�J@�&�@���@��@��@���@���@�"�@�1'@�Q�@��;@�1@���@��@���@�n�@�M�@�@��D@�j@��F@�\)@���@�=q@���@��@��`@�j@�|�@��H@�$�@���@�I�@���@�K�@�;d@�n�@��@�hs@�O�@�I�@�ƨ@���@�|�@�K�@�33@���@��+@�J@���@�V@���@�1'@�b@��@�
=@�~�@���A��A��A��A��A��A��yA��mA��mA��yA��`A��TA��HA��TA��mA��yA��mA��mA��yA��mA��mA��mA��mA��yA��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��yA��yA��yA��yA��`A��#A��#A��A��A���A��;A��yA��A��yA���A�ƨAǾwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        A��yA��yA��A��A��yA��yA��TA��#A��
A��`A��/AǴ9A�~�A��/AƅA�33Aũ�A�/A���Aĩ�A�v�A�(�AÙ�A��A���A��`A�AA��A�bNA���A�?}A��A���A��A��uA���A�~�A�r�A�~�A�r�A�^5A�ffA��#A��;A���A�33A�bA��-A��yA�O�A�K�A�{A���A��A��A���A�XA� �A���A��/A��wA���A�A��HA���A��9A���A�`BA��A���A��A�Q�A��A�  A�-A�|�A�1A�^5A��A�oA�%A��-A��A�?}A���A�t�A�bA�K�A�+A�&�A���A���A�`BA�{A��A��A�?}A��TA���A���A��jA�hsA�1'A���A�7LA�;dA���A��;A�ȴA��!A�1'A�%A��
A�E�A��A��9A�|�A�ZA~�`A{?}AyAxn�Au7LAr�yAoO�Ak��Aj�9Aj1Ai
=Ah��AhffAg�Ag"�Ae7LAd �AcS�A`z�A]�A]+A\��AZ��AZI�AX��AX(�AVjAS�^AR�HARZAR1'AR�AQ��APZAO�^ANn�AL�`ALffAK��AK�FAK;dAJ�AJE�AJ1'AJ  AI��AI/AH$�AFĜAFE�AEAD��ABn�AA�TA?��A>jA=?}A;��A:��A9S�A7��A6E�A4^5A3dZA37LA3�A2�/A2�A1t�A0�!A0-A/�wA/\)A.{A-C�A,JA*M�A)��A(��A(9XA'�-A'\)A&�RA&^5A%�A$ �A"�A!��A!"�A Q�A�AbNA��AE�Al�A%AĜA  AȴAffAp�AbA��A�A��A�hAn�AA7LA�PA^5A�A�^A+A1AA�hA
��A��A�;A&�A�A��A��A�;A��A%@�C�@�;d@�O�@���@��m@�ff@�&�@���@�@�X@���@�@�hs@�bN@���@�l�@��T@أ�@�Z@���@Ӿw@���@Ϯ@�hs@��T@�O�@���@��P@�@�bN@��+@�ff@��7@�\)@�-@�I�@�p�@��`@�b@�z�@���@�=q@��^@�V@�@�o@��h@���@��@�/@�1@��+@�J@�&�@���@��@��@���@���@�"�@�1'@�Q�@��;@�1@���@��@���@�n�@�M�@�@��D@�j@��F@�\)@���@�=q@���@��@��`@�j@�|�@��H@�$�@���@�I�@���@�K�@�;d@�n�@��@�hs@�O�@�I�@�ƨ@���@�|�@�K�@�33@���@��+@�J@���@�V@���@�1'@�b@��@�
=@�~�@���A��A��A��A��A��A��yA��mA��mA��yA��`A��TA��HA��TA��mA��yA��mA��mA��yA��mA��mA��mA��mA��yA��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��yA��yA��yA��yA��yA��`A��#A��#A��A��A���A��;A��yA��A��yA���A�ƨAǾwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�}B
�}B
�}B
�}B
�}B
��B
��B
B
B
��B
��B
ĜB
ǮB
��B
�B
�)B
�HB
�ZB
�mB
�sB
�B
��BB\BbBhB{B!�B5?BR�Bz�B�JB�hB��B��B��B�!B�?B�}BÖB��B��B�B�BS�B�B�B�B�7B�JB��B��B�B�B��B��B��B�B�B�B�!B�-B�?B�LB�LB�LB�LB�RB�RB�wB��BB�}B�FB�!B��B��B��B��B��B��B��B�{B�hB�+Bx�Bs�Bn�Bl�Bk�BYB<jB0!B+B(�B'�B$�B�B�B��B�?B��B��B��B�{Bs�B^5BR�BI�BG�BE�B,B#�B�B
�B
��B
ƨB
�'B
�1B
iyB
M�B
C�B
7LB
�B
JB	�B	�/B	�B	��B	��B	��B	��B	ǮB	��B	�LB	�!B	�B	��B	�bB	�=B	�B	{�B	w�B	q�B	l�B	YB	D�B	<jB	9XB	8RB	7LB	8RB	49B	0!B	(�B	'�B	$�B	"�B	!�B	�B	�B	�B	�B	�B	�B	�B	\B	
=B	+B	B��B��B�B�B�fB�HB�#B�B��B��BB�^B�FB�9B�3B�'B�B�B��B��B��B��B��B��B��B��B�uB�hB�VB�PB�DB�=B�7B�1B�B� B~�B|�By�Bw�Bt�Bs�Bs�Bm�Bm�Bm�Bm�Bn�Bl�Bl�Bo�Bo�Bn�Bn�Br�Bt�Bs�Bu�Bs�Bv�Bu�Bw�Bw�Bu�Bt�Bs�Bs�BiyBm�Bo�BjBdZB\)BM�BB�B<jB7LB;dB9XB8RB7LB6FB7LB6FB6FBD�BYBz�Bu�Bq�BcTBL�BE�BA�BB�B@�B>wBC�BB�BL�BQ�BR�BW
B[#B_;BbNBq�Br�Br�B~�B�B�JB�PB�PB�7B�PB�bB�{B��B��B��B�LB�wBƨB�
B�B�B�)B�;B�`B�B��B	{B	�B	 �B	7LB	M�B	^5B	gmB	o�B	p�B	x�B	{�B	~�B	�B	�B	�VB	�bB	��B	��B	��B	��B	��B	�B	�!B	�?B	�^B	�jB	��B	��B	ĜB	ǮB	ȴB	ȴB	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�;B	�HB	�TB	�NB	�fB	�mB	�mB	�yB	�B	�B
��B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
��B
�}B
��B
��B
�}B
�}B
�}B
��B
��B
�}B
�wB
�}B
�}B
�}B
��B
��B
��B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
��B
�}B
��B
��B
��B
��B
B
��B
��B
B
��B
�}B
�}B
�}B
��B
B
ÖG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        B
�TB
�UB
�UB
�UB
�UB
�\B
�\B
�hB
�iB
�]B
�]B
�wB
ǉB
��B
��B
�B
�&B
�8B
�LB
�RB
�kB
��B�B=BDBKB^B!�B5#BR�Bz�B�0B�OB�zB�iB��B�
B�(B�gBÀB��B��BB�BS�B��B�B�B�&B�:B�}B��B��B�B��B��B��B��B�B�B�B�$B�6B�DB�DB�EB�FB�LB�MB�rB�BB�zB�CB�B��B��B��B��B��B��B��B�}B�jB�.Bx�Bs�Bn�Bl�Bk�BYB<pB0'B+	B(�B'�B$�B�B� B��B�IB��B��B��B��Bs�B^BBR�BI�BG�BE�B,B#�B�B
�B
�B
ƹB
�8B
�BB
i�B
M�B
C�B
7_B
�B
^B	��B	�CB	�,B	�B	��B	��B	��B	��B	��B	�dB	�:B	�'B	��B	�|B	�XB	�4B	|B	w�B	q�B	l�B	Y4B	D�B	<�B	9wB	8qB	7lB	8sB	4ZB	0CB	)B	(B	% B	"�B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	
fB	UB	CB�%B��B��B�B�B�uB�PB�2B�B��B¾B��B�vB�jB�dB�YB�MB�AB�/B�*B�B�B��B��B��B��B��B��B��B��B�}B�wB�qB�lB�ZB�<B6B}+BzBxBt�Bs�Bs�Bm�Bm�Bm�Bm�Bn�Bl�Bl�Bo�Bo�Bn�Bn�Br�BuBs�Bv
Bs�BwBvBxBxBvBuBtBtBi�Bm�Bo�Bj�Bd�B\wBN"BB�B<�B7�B;�B9�B8�B7�B6�B7�B6�B6�BD�BYoB{;BvBrBc�BM,BFBA�BB�B@�B>�BC�BB�BM6BRVBS]BWwB[�B_�Bb�BrBs$Bs%BqB��B��B��B��B��B��B��B��B�B�*B�WB��B��B�2BזB؝B؞BܹB��B��B�8B��B	B	KB	!_B	7�B	NqB	^�B	hB	pAB	qHB	y{B	|�B	�B	��B	��B	�B	�B	�IB	�]B	��B	��B	��B	��B	��B	��B	�B	�'B	�AB	�CB	�]B	�qB	�yB	�yB	͔B	ΜB	ѯB	ѰB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�0B	�,B	�EB	�MB	�OB	�\B	�oB	�wB
�ZB
�TB
�TB
�NB
�TB
�TB
�TB
�TB
�TB
�ZB
�TB
�ZB
�ZB
�TB
�TB
�TB
�ZB
�ZB
�TB
�NB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�UB
�UB
�UB
�UB
�UB
�UB
�OB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�UB
�\B
�\B
�VB
�\B
�\B
�\B
�\B
�hB
�bB
�cB
�iB
�cB
�WB
�WB
�WB
�cB
�jB
�qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202422021061413551220210614135512202106171311342021061713113420210617131134201807242202422021061413551220210614135512202106171311342021061713113420210617131134PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024220180724220242  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024220180724220242QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024220180724220242QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145220210617131452IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                