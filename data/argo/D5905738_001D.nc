CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  T   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:26Z creation      
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
resolution        =���   axis      Z        
�  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
�  I(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
�  Vp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  k�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  nX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  {�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �x   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    є   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     ќ   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         Ѽ   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �      �  �Argo profile    3.1 1.2 19500101000000  20180724220226  20210722160147  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�b�t>E�@�b�t>E�11  @�b�l� @�b�l� @6����R~@6����R~�c�9��8q�c�9��8q11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @Fff@�33@�  @�  @�ffA��A��A$��AA��Aa��A���A���A���A���A���A���A���A�  A�33BffB  B  B ffB(  B0  B8ffB@��BHffBP  BXffB`  Bh��BpffBw��B��B���B���B���B�33B�  B�  B�33B�  B�33B�ffB���B���B�  B�33B�33B�33B�33B���B�  Bϙ�B�  B�33B�ffB���B�33B���B�33B�33B���B�  B�  B���C�fC  C  C�C
33C33C  C  C�C33C33C�fC�fC�fC�fC 33C"33C$�C&  C(  C*  C,  C.33C0  C1�fC4�C633C833C:�C;�fC>  C@�CB33CD  CE�fCG��CJ�CL33CN�CP  CR  CS�fCU�fCX�CZ33C\�C^33C`�Cb�Cc�fCe�fCg��Ci��Cl�CnL�Cp�Cq��Cs�fCv�Cx  Cy�fC|  C~�C��C��C��C��C��C��C��C�  C�  C��C��C�&fC��C��3C��C�&fC�&fC��C��C�  C�  C��C��C�  C�  C��3C�  C��C��C��C��C�  C�  C��C�  C�  C�  C��3C��C��C��C��C��C��C�  C��C��C��C��3C��3C��3C�  C�&fC��C��C�  C�  C��C��C��C��C�  C��3C��3C��3C��3C�  C��C��C�&fC��C��3C�  C�  C��C��C�  C��3C��3C��C�&fC��C��C��C��C�  C��fC�  C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��C��C�  C�  C��C��C��C�  C��3C��3C��C�&fC��C��C�  C��3C��C��C�  C��C��C��C�ٚC�  C�33C��C�  C��3D fD y�D�3D,�D��D��D
� DL�D3DٚD� D` D,�D�fD�3DL�DfD��Dl�D!  D"�3D$�fD&@ D'�3D)�3D+l�D-�D.�3D0y�D2  D3�3D5� D7,�D8�fD:Y�D;��D=� D?3D@�3DB  DC�3DE@ DF��DHS3DI� DKl�DL��DN� DP9�DQٚDSy�DU�DV� DX` DY��D[��D]  D^�fD`,�Da�fDc&fDd�3Df  DgY�Dh� Dj,�Dk��Dl�3DnS3Do�3Dq3Drs3Ds�3Du,�Dv�fDw� Dy9�Dz��D{� >���>���>���>���>���>���>���>���>���>L��>���>���>���>���?   >���>���?   ?   ?��?333?fff?�  ?�  ?���?�ff?�  ?ٙ�?�33@ff@33@,��@9��@L��@`  @s33@�33@�  @���@�ff@�33@�  @���@���@���@���AffA��AffA��A&ffA,��A4��A<��AD��AL��AVffA^ffAd��AnffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141444144114414111141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        ?fff?�  @   @fff@�33@�  @�  @�ffA	��A��A,��AI��Ai��A���A���A���A���A���A���A���A�  B��B
ffB  B  B"ffB*  B2  B:ffBB��BJffBR  BZffBb  Bj��BrffBy��B���B���B���B���B�33B�  B�  B�33B�  B�33B�ffB���B���B�  B�33B�33B�33B�33B���B�  BЙ�B�  B�33B�ffBᙚB�33B���B�33B�33B���B�  B�  C L�CffC� C� C��C
�3C�3C� C� C��C�3C�3CffCffCffCffC �3C"�3C$��C&� C(� C*� C,� C.�3C0� C2ffC4��C6�3C8�3C:��C<ffC>� C@��CB�3CD� CFffCHL�CJ��CL�3CN��CP� CR� CTffCVffCX��CZ�3C\��C^�3C`��Cb��CdffCfffChL�CjL�Cl��Cn��Cp��CrL�CtffCv��Cx� CzffC|� C~��C�Y�C�L�C�L�C�Y�C�L�C�L�C�Y�C�@ C�@ C�L�C�L�C�ffC�L�C�33C�L�C�ffC�ffC�Y�C�L�C�@ C�@ C�Y�C�L�C�@ C�@ C�33C�@ C�Y�C�L�C�Y�C�Y�C�@ C�@ C�Y�C�@ C�@ C�@ C�33C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�Y�C�L�C�33C�33C�33C�@ C�ffC�Y�C�L�C�@ C�@ C�L�C�L�C�L�C�Y�C�@ C�33C�33C�33C�33C�@ C�L�C�L�C�ffC�L�C�33C�@ C�@ C�L�C�L�C�@ C�33C�33C�L�C�ffC�Y�C�Y�C�L�C�L�C�@ C�&fC�@ C�Y�C�L�C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�L�C�Y�C�@ C�@ C�Y�C�L�C�Y�C�@ C�33C�33C�L�C�ffC�Y�C�L�C�@ C�33C�L�C�Y�C�@ C�L�C�Y�C�L�C��C�@ C�s3C�Y�C�@ C�33D &fD ��D�3DL�D�DٚD
� Dl�D33D��D� D� DL�DfD�3Dl�D&fDٚD��D!@ D"�3D$�fD&` D(3D)�3D+��D-9�D.�3D0��D2@ D3�3D5� D7L�D8�fD:y�D<�D=� D?33D@�3DB@ DC�3DE` DF��DHs3DJ  DK��DM�DN� DPY�DQ��DS��DU9�DV� DX� DZ�D[��D]@ D^�fD`L�Da�fDcFfDd�3Df  Dgy�Dh� DjL�Dk��Dm3Dns3Do�3Dq33Dr�3Ds�3DuL�Dv�fDx  DyY�Dz��D{� G�O�G�O�G�O�?L��G�O�?L��G�O�G�O�G�O�?333G�O�G�O�?L��?fffG�O�G�O�?fffG�O�?�  ?���?���?�33G�O�?�  ?ٙ�?�ff@   @��@��@&ff@333@L��@Y��@l��@�  @���@�33@�  @���@�ff@�33@�  @���@���@���A��AffA��AffA$��A.ffA4��A<��AD��AL��AT��A^ffAfffAl��AvffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444141444144114414111141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        @ �@ �@ �@ *@ �@ ""@ (�@ 1'@ 7L@ >@ FQ@ R�@ `B@ m�@ |?@ ��@ ��@ ��@ �-@ ��@ ��@ ��@ ��@ ��@j@�@�@,`@:�@I@V@b�@qS@~K@�P@�H@��@�9@��@ψ@�/@�4@�,@�@*@""@0x@>�@Ji@X@ff@t�@�d@�@��@��@�R@Ĝ@Ӡ@��@�L@��@
�@6@&;@3�@@,@N�@\)@hs@v�@�@��@�@�r@�@�c@�
@�`@�@^@�@�@(G@5�@E�@SI@`B@m:@z�@��@�0@�5@�~@�w@�|@��@�y@�q@�@@g@-�@:@F�@S�@c�@r@~�@��@��@��@�9@�>@є@ލ@��@��@�@�@!s@.l@<@K�@Z�@g@r�@�@�@�@��@�R@ƨ@��@��@�@��@
�@�@&�@33@@�@O0@\�@k�@x&@�p@�u@�y@�!@�@�@�
@�@�@ �@�@O@(G@6�@E�@R�@`�@n�@z�@��@��@��@�~@�&@�@�#@��@�q@	@	�@	g@	,`@	:�@	I@	V@	bN@	o�@	}�@	��@	��@	��@	��@	@	�7@	ލ@	�4@	��@
1@
{@
!s@
/@
<�@
Ji@
X�@
g@
t�@
��@
�@
�U@
��@
�R@
ƨ@
�O@
�H@
�@@
��@
�@�@&�@4�@A�@O0@\)@hs@ww@�|@�u@�m@�f@�@ȴ@�[@�@�@�Q@�@�@)�@7�@DD@Q�@`�@m�@|?@��@��@�(@�-@�2@�*@�#@�@�@@o@�@-@;d@H]@SI@b�@s_@�@��@��@��@�9@�@4�@dZ@��@��@� @'�@X@��@�R@�y@�@FQ@uk@��@��@^@/�@^5@��@��@�(@�@I@v�@��@��@  @.l@\)@��@��@��@�@6�@a�@��@��@��@
=@4�@^5@��@��@�/@
=@5�@bN@��@�@�@{@@,@k.@�0@��@�y@�@:�@a�@��@�f@Ӡ@��@ @FQ@k�@�h@��@܀@@&�@K�@p�@��@�@�tG�O�G�O�G�O�@ G�O�@ G�O�G�O�G�O�@ ^G�O�G�O�@ @ �G�O�G�O�@ �G�O�@ j@ @ �@ %G�O�@ �@ 1@ �@ 
=@ �@ �@ V@ �@ o@ �@ �@ �@ �@ �@ �@  �@ #�@ &;@ (�@ +�@ /@ 2�@ 5?@ 9X@ <@ @,@ B�@ F�@ I�@ M$@ P�@ S�@ Wb@ [z@ ^�@ a�@ e�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��TA��A���A��A��A��mA��TA��HA��`A��`A��A��A��A��A���A���A���A���A��A�S�A��A�`BA��mA°!A�ADA��HA�E�A�  A��^A�ZA���A�"�A��A�A���A��yA�/A���A��DA�n�A��^A���A��A��\A��jA��;A��DA���A�"�A�ƨA�\)A��A���A���A�~�A�G�A��;A�$�A��A��
A�K�A���A���A���A�$�A�?}A��!A�O�A�|�A�v�A�
=A���A�=qA��A��A��/A�^5A�S�A�t�A��mA��A���A���A�S�A���A�E�A��wA�+A�A��mA�A�ZA��A��A�"�A�oA��mA��A���A���A��wA��
A��A��DA�v�A�G�A�M�A�A��mA��A�p�A��A�-A���A�K�A���A�A~ȴA{%Ay�AvI�As�AohsAnz�AnffAm\)AhZAe33Ac��A`�A^��A^�jA^ȴA^r�A]�A]C�A\1'AZM�AY�AX~�AW�^AW7LAWoAV��AUt�AT=qAP�+AM?}AJ��AJ  AI�AIS�AH�/AH�AGO�AE��AD�RADz�AC?}AA��A?�A=�A<(�A<bA;�mA;l�A;"�A;VA:�A:��A9\)A7�A7hsA6�9A5p�A4�A4M�A2�`A2��A29XA1�A1�A1`BA0�`A/;dA.�RA-�
A,�A*�+A*(�A*JA(�9A'l�A%�hA$�+A#��A#%A"�\A"M�A"bA!�hA VA�An�A��A|�A��AG�A�TA��A��Av�At�AĜAI�AO�A��AE�AA�uAdZA��An�AA�AhsA;dAoA�A�RA �A�#AS�A�HAffAQ�A�#A
n�A	�
A	��A	x�Ar�A��A%A�DAv�AZA�A��A �AbA �\A �A ��A (�@�v�@� �@�@���@�5?@�l�@�o@�Z@�z�@ڗ�@�p�@�b@�I�@��H@��@���@��;@�S�@�  @�@�G�@�C�@�X@���@�1@���@���@��@�t�@�O�@�Z@��T@��7@�V@���@�V@���@��@��@�$�@��^@��u@���@�Ĝ@��@�ƨ@��@���@���@�
=@�J@�X@��@�Ĝ@�A�@���@��#@���@�X@��@���@���@�|�@�C�@���@��D@�b@�C�@��!@�v�@�5?@���@���@�/@�b@���@��!@�ff@�J@�@�7L@��j@�bA���A���A��A��yA��TA���A�ȴA���A�ĜA�ȴA�ȴA���A���A���A���A���A���A���A��A���A��
A��A��A��A��yA��yA��A��yA��A��A��A��A��A���A���A���A��A��A��A��A��A��`A��`A��TA��HA��;A��TA��TA��mA��mA��`A��`A��mA��yA��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        A��
A��TA��A���A��A��A��mA��TA��HA��`A��`A��A��A��A��A���A���A���A���A��A�S�A��A�`BA��mA°!A�ADA��HA�E�A�  A��^A�ZA���A�"�A��A�A���A��yA�/A���A��DA�n�A��^A���A��A��\A��jA��;A��DA���A�"�A�ƨA�\)A��A���A���A�~�A�G�A��;A�$�A��A��
A�K�A���A���A���A�$�A�?}A��!A�O�A�|�A�v�A�
=A���A�=qA��A��A��/A�^5A�S�A�t�A��mA��A���A���A�S�A���A�E�A��wA�+A�A��mA�A�ZA��A��A�"�A�oA��mA��A���A���A��wA��
A��A��DA�v�A�G�A�M�A�A��mA��A�p�A��A�-A���A�K�A���A�A~ȴA{%Ay�AvI�As�AohsAnz�AnffAm\)AhZAe33Ac��A`�A^��A^�jA^ȴA^r�A]�A]C�A\1'AZM�AY�AX~�AW�^AW7LAWoAV��AUt�AT=qAP�+AM?}AJ��AJ  AI�AIS�AH�/AH�AGO�AE��AD�RADz�AC?}AA��A?�A=�A<(�A<bA;�mA;l�A;"�A;VA:�A:��A9\)A7�A7hsA6�9A5p�A4�A4M�A2�`A2��A29XA1�A1�A1`BA0�`A/;dA.�RA-�
A,�A*�+A*(�A*JA(�9A'l�A%�hA$�+A#��A#%A"�\A"M�A"bA!�hA VA�An�A��A|�A��AG�A�TA��A��Av�At�AĜAI�AO�A��AE�AA�uAdZA��An�AA�AhsA;dAoA�A�RA �A�#AS�A�HAffAQ�A�#A
n�A	�
A	��A	x�Ar�A��A%A�DAv�AZA�A��A �AbA �\A �A ��A (�@�v�@� �@�@���@�5?@�l�@�o@�Z@�z�@ڗ�@�p�@�b@�I�@��H@��@���@��;@�S�@�  @�@�G�@�C�@�X@���@�1@���@���@��@�t�@�O�@�Z@��T@��7@�V@���@�V@���@��@��@�$�@��^@��u@���@�Ĝ@��@�ƨ@��@���@���@�
=@�J@�X@��@�Ĝ@�A�@���@��#@���@�X@��@���@���@�|�@�C�@���@��D@�b@�C�@��!@�v�@�5?@���@���@�/@�b@���@��!@�ff@�J@�@�7L@��j@�bA���A���A��A��yA��TA���A�ȴA���A�ĜA�ȴA�ȴA���A���A���A���A���A���A���A��A���A��
A��A��A��A��yA��yA��A��yA��A��A��A��A��A���A���A���A��A��A��A��A��A��`A��`A��TA��HA��;A��TA��TA��mA��mA��`A��`A��mA��yA��A��A��A��A��A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�!B
�!B
�B
�!B
�B
�!B
�!B
�'B
�'B
�'B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�!B
�XB
��B
��BBJB\BhB"�B7LB@�BI�B\)B�+B�3B"�B7LBL�BgmBm�Bw�Bv�Bt�BhsBhsB�DB�DB�+B��B��B��B��B�B�FB�jB�dB�dB�^B�XB�^B�9B�'B��B��B�oB�hB^5BP�BgmB�{B��B��B��B�bB�DB�%B|�B|�Bx�Bq�Bm�BdZB^5BXBS�BJ�B9XB0!B(�B�BbB�sB��B��B��B��B��By�BW
BR�BQ�BP�BP�BO�BD�B<jB)�B+B
��B
�ZB
��B
��B
�!B
��B
�1B
�B
|�B
t�B
k�B
hsB
[#B
?}B
.B
uB	��B	�#B	�
B	��B	ɺB	��B	�uB	�\B	� B	s�B	�B	�DB	�VB	�VB	�JB	�+B	�B	z�B	w�B	q�B	m�B	k�B	jB	^5B	T�B	D�B	/B	(�B	"�B	�B	�B	�B	uB	JB	B��B��B��B�B�mB�/B�B�B�B��B��B��B��B��B��BȴBŢBB�wB�jB�RB�9B�3B�!B�B�B�B��B��B��B��B��B��B��B�{B�bB�JB�B� B|�Bz�Bx�Bw�Bv�Bu�Bt�Bq�Bp�Bo�Bn�Bk�Bk�BhsBiyBjBjBn�Br�Bm�Be`BbNB]/B]/B\)B\)B_;B^5B`BB^5B]/B]/B]/B`BBaHBaHBdZBbNB]/B`BBiyBjBs�Bs�Br�Br�Bn�Bn�Bo�Bp�Bp�Bq�Bp�Bn�BhsBbNB]/B_;B_;B`BB^5BXBQ�BL�BJ�BD�B2-B=qB9XB8RB?}BD�BG�BR�BS�BS�BVBbNBaHBgmBjBr�Bq�Bx�B�B�JB�uB��B��B��B�LBɺB��B�#B�ZB��B	VB	�B	.B	<jB	I�B	K�B	T�B	XB	[#B	bNB	l�B	p�B	u�B	|�B	�1B	�\B	�oB	��B	��B	��B	��B	��B	��B	�!B	�3B	�FB	�LB	�jB	ĜB	ĜB	ƨB	��B	��B	��B	��B	��B	�B	�
B	�B	�5B	�BB	�BB	�HB	�HB	�ZB	�ZB	�mB
�!B
�B
�B
�B
�!B
�!B
�-B
�'B
�'B
�!B
�-B
�3B
�'B
�B
�!B
�-B
�!B
�-B
�!B
�!B
�'B
�!B
�!B
�B
�!B
�B
�B
�B
�B
�B
�!B
�B
�'B
�!B
�B
�B
�B
�B
�!B
�B
�!B
�!B
�!B
�'B
�'B
�'B
�!B
�'B
�'B
�!B
�!B
�'B
�!B
�!B
�!B
�!B
�B
�'B
�!B
�!G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�RB
��B
��BBDBVBbB!�B6FB?}BH�B[#B�%B�-B!�B6FBK�BffBl�Bv�Bu�Bs�BgmBgmB�=B�=B�%B��B��B��B��B�B�?B�dB�^B�^B�XB�RB�XB�3B�!B��B��B�hB�bB]/BO�BffB�uB��B��B��B�\B�=B�B{�B{�Bw�Bp�Bl�BcTB]/BW
BR�BI�B8RB/B'�B�B\B�mB��B��B��B��B��Bx�BVBQ�BP�BO�BO�BN�BC�B;dB(�B%B
��B
�TB
��B
�}B
�B
��B
�+B
�B
{�B
s�B
jB
gmB
ZB
>wB
-B
oB	��B	�B	�B	��B	ȴB	��B	�oB	�VB	~�B	r�B	�B	�=B	�PB	�PB	�DB	�%B	�B	y�B	v�B	p�B	l�B	jB	iyB	]/B	S�B	C�B	.B	'�B	!�B	�B	�B	�B	oB	DB	B��B��B��B�B�fB�)B�B�
B��B��B��B��B��B��B��BǮBĜB��B�qB�dB�LB�3B�-B�B�B�B�B��B��B��B��B��B��B�{B�uB�\B�DB�B~�B{�By�Bw�Bv�Bu�Bt�Bs�Bp�Bo�Bn�Bm�BjBjBgmBhsBiyBiyBm�Bq�Bl�BdZBaHB\)B\)B[#B[#B^5B]/B_;B]/B\)B\)B\)B_;B`BB`BBcTBaHB\)B_;BhsBiyBr�Br�Bq�Bq�Bm�Bm�Bn�Bo�Bo�Bp�Bo�Bm�BgmBaHB\)B^5B^5B_;B]/BW
BP�BK�BI�BC�B1'B=qB9XB8RB?}BD�BG�BR�BS�BS�BVBbNBaHBgmBjBr�Bq�Bx�B�B�JB�uB��B��B��B�LBɺB��B�#B�ZB��B	VB	�B	.B	<jB	I�B	K�B	T�B	XB	[#B	bNB	l�B	p�B	u�B	|�B	�1B	�\B	�oB	��B	��B	��B	��B	��B	��B	�!B	�3B	�FB	�LB	�jB	ĜB	ĜB	ƨB	��B	��B	��B	��B	��B	�B	�
B	�B	�5B	�BB	�BB	�HB	�HB	�ZB	�ZB	�mB
�B
�B
�B
�B
�B
�B
�'B
�!B
�!B
�B
�'B
�-B
�!B
�B
�B
�'B
�B
�'B
�B
�B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�B
�!B
�!B
�B
�B
�!B
�B
�B
�B
�B
�B
�!B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202262021061413515320210614135153202106141746092021061417460920210614174609201807242202262021061413515320210614135153202106141746092021061417460920210614174609PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422022620180724220226  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022620180724220226QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022620180724220226QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014720210722160147IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                