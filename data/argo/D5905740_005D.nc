CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  d   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:58Z creation      
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
_FillValue                  �  �LArgo profile    3.1 1.2 19500101000000  20180724220258  20210722161418  5905740 5905740 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7220                            7220                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12003                           12003                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�j�o��V@�j�o��V11  @�j�l� @�j�l� @*o�1P��@*o�1P���cJ��޽��cJ��޽�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  ?L��@   @Fff@�ff@�ff@�33@�  A��A  A$��A@  A`  A�  A���A���A�  A���Aљ�A�33A�  B ffBffB��B��B ��B(ffB0ffB8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�33B���B���B���B���B���B�  B�  B�  B�33B�ffB�ffB�ffB�33B�33B�ffB�ffB�33BǙ�B˙�B���Bә�B�  B�  B�33B䙚B�ffB�33B�33B�ffB�33B�33C 33C  C�fC�fC�C
L�C33C33CL�CL�C33C33C33C�C�C33C �C"  C#�fC%�fC(  C*  C,  C.  C0  C2�C4�C5�fC8  C:33C<�C>  C@�CB�CD�CF�CH�CJ�CL  CN  CP�CR  CT�CV�CW�fCY�fC\  C^�C`�Cb�Cd  Cf�Ch33Cj�Cl�Cn  Cp  Cr  Ct�Cv�Cx33Cz33C|  C~  C��C�  C��C�&fC��C��C�&fC�&fC��C�  C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��C�  C��C��C��C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C��C��C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C��3C�  C�  C�  C��C��C�  C��C��C��C��C��C��C�  C��C��C��C��C��C�  C�  C�  C��C��C��C�  C��C��C�  C�  C��C�  C��C��C��C��C��3C��3C��C��C�  C��C��C��C��C�  C��C��C�  C��3C���D y�D�fD� D3DFfDy�D�fD	�3D
��D�D33DS3Dl�D��D�3D��D� D� D��D��D�fD��D� D��D�3D�fD!�D#ffD$�3D%�fD'fD(@ D)y�D*� D,  D-,�D.L�D/l�D0� D1�fD2�fD4ffD59�D6�D7��D9,�D9� D;l�D=fD=�fD?s3D@L�DB�DCfDC��DE�3DF�fDGٚDH�3DJ�3DK�3DL�3DN� DO�3DP�3DQ��DSٚDT��DU�fDV��DX� DYS3D[  D[��D]ffD^  D_�3Da  DbS3Db�3Dd,�Del�Dg9�Dhs3Di�fDk�Dk�3Dm&fDn� Dp  Dp� Dr� DsL�Du�Du�fDw��Dx�fDzY�D{�?333?333?333?��?333?333?333?333?333?333?L��?333?333?��?333?333?333?333?333?L��?333?L��?L��?L��?fff?���?���?�ff?���?�ff@   @��@   @333@Fff@`  @y��@�ff@�  @�  @���@���@�33@�  @ٙ�@�ff@���A��A��A33A��A$��A,��A333A<��AC33AK33AT��A\��Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144444144414444141441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ?�ff@   @fff@�ff@�ff@�33@�  A	��A  A,��AH  Ah  A�  A���A���A�  A���Aՙ�A�33A�  BffB
ffB��B��B"��B*ffB2ffB:  BB  BJ  BRffBZffBb  Bj  Br  Bz  B�33B���B���B���B���B���B�  B�  B�  B�33B�ffB�ffB�ffB�33B�33B�ffB�ffB�33Bș�B̙�B���Bԙ�B�  B�  B�33B噚B�ffB�33B�33B�ffB�33B�33C �3C� CffCffC��C
��C�3C�3C��C��C�3C�3C�3C��C��C�3C ��C"� C$ffC&ffC(� C*� C,� C.� C0� C2��C4��C6ffC8� C:�3C<��C>� C@��CB��CD��CF��CH��CJ��CL� CN� CP��CR� CT��CV��CXffCZffC\� C^��C`��Cb��Cd� Cf��Ch�3Cj��Cl��Cn� Cp� Cr� Ct��Cv��Cx�3Cz�3C|� C~� C�Y�C�@ C�L�C�ffC�L�C�L�C�ffC�ffC�L�C�@ C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�L�C�L�C�L�C�Y�C�L�C�L�C�L�C�@ C�Y�C�@ C�@ C�@ C�33C�@ C�L�C�@ C�@ C�L�C�@ C�L�C�Y�C�L�C�@ C�L�C�@ C�@ C�@ C�33C�@ C�L�C�@ C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�33C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�Y�C�L�C�@ C�L�C�@ C�33C�@ C�@ C�@ C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�L�C�Y�C�Y�C�L�C�L�C�@ C�@ C�@ C�L�C�L�C�Y�C�@ C�L�C�L�C�@ C�@ C�L�C�@ C�L�C�Y�C�L�C�L�C�33C�33C�L�C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�L�C�L�C�@ C��3C��D ��D�fD  D33DffD��D�fD	�3D�D9�DS3Ds3D��D��D�3D��D� D� D��D��D�fD��D� DٚD�3D fD!,�D#�fD$�3D%�fD'&fD(` D)��D*� D,  D-L�D.l�D/��D0� D1�fD2�fD4�fD5Y�D6,�D7��D9L�D:  D;��D=&fD=�fD?�3D@l�DB9�DC&fDD�DF3DGfDG��DH�3DJ�3DK�3DL�3DO  DO�3DP�3DQ��DS��DT��DU�fDV��DX� DYs3D[  D[��D]�fD^@ D_�3Da  Dbs3Dc3DdL�De��DgY�Dh�3Di�fDk,�Dk�3DmFfDn� Dp@ Dq  Dr� Dsl�Du,�DvfDw��Dx�fDzy�D{9�G�O�G�O�G�O�?���G�O�G�O�G�O�G�O�G�O�?���G�O�G�O�G�O�?���G�O�G�O�G�O�G�O�?���G�O�?���G�O�G�O�?�ff?�33?���?ٙ�?�ff@ff@33@   @,��@@  @S33@fff@�  @���@�ff@�  @�  @���@ə�@�33@�  @陚@�ffA��A��A��A33A$��A,��A4��A;33AD��AK33AS33A\��Ad��Ai��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144444144414444141441111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        @ v@ �@ *@ �@ #�@ )�@ /�@ 7L@ =q@ FQ@ Q�@ _�@ m:@ |?@ ��@ �0@ ��@ ��@ �w@ ��@ �#@ ��@ � @�@o@g@-@:@G�@UU@c�@qS@~K@��@��@�A@��@��@ψ@܀@��@�~@�@{@""@0x@>�@Lu@Z@g@t�@�@��@��@�M@��@�J@�C@�H@��@�E@J@B@&;@3�@B8@O0@\�@k.@ww@�p@�@�@�!@�@��@�@�@�@^@@�@)�@7�@D�@Q�@^�@l�@z�@��@�0@��@�~@��@�|@��@�@� @@@g@-@:�@H]@V@c�@p�@~K@��@��@��@��@��@ψ@��@�4@��@�@{@"�@1'@>@K�@X�@ff@t@�d@�@�a@�@�R@��@��@�H@�@��@
�@�@'�@5?@A�@N�@\�@i�@x&@��@�u@�@��@�k@��@�h@�`@�@ �@@�@)�@7L@DD@SI@_�@m:@z�@��@�0@��@�~@�&@�|@�t@��@� @	@	@	g@	,`@	:@	G�@	T�@	b�@	qS@	~K@	��@	��@	��@	��@	@	�7@	��@	�@	�,@
%@
*@
"�@
/�@
=q@
K@
X�@
ff@
t@
�@
�@
�@
�Y@
�R@
�J@
Ӡ@
�H@
��@
�E@
�@�@&;@3�@A�@O0@\�@j@ww@��@�$@��@��@�k@�c@�
@�@�@ �@@O@)�@7L@DD@Q�@`B@m:@{�@��@��@��@��@�w@�|@�#@�@�q@@�@g@,`@:�@H]@UU@^�@��@�9@�@@1�@R�@s_@�u@��@��@�@�@.l@Lu@��@�y@�w@�t@��@-�@I@c�@}�@�R@��@��@V@-�@m�@��@��@��@�Y@�@6�@X�@x�@��@�F@Ӡ@�@
�@>@T�@k.@��@��@Ӡ@��@)�@>@k�@�@�9@�|@�m@[@7L@Q=@k�@�y@��@�@@+@FQ@`�@��@��@�|@�@�@.l@\)@r@��@�~@�@  @$.@5?@V�@x�@��@�o@�@o@$.@K�@t@�@�~@��@�@#�@:�@k.@�d@�9@ȴG�O�G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�@ G�O�G�O�G�O�G�O�@ �G�O�@ �G�O�G�O�@ v@ %@ �@ 1@ �@ 
�@ J@ �@ @ @ @ *@ �@ �@ �@ �@ ""@ $.@ '�@ )�@ ,`@ .l@ 1'@ 5?@ 8�@ <@ >�@ B�@ FQ@ I�@ Lu@ P�@ SI@ V�@ Z�@ ^5@ `BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�oA��A��A�"�A�+A�1'A�1'A�-A�1'A�=qA�I�A�K�A�K�A�K�A�M�A�C�A�A�A�G�A�E�A�K�A�I�A�K�A�K�A�K�A�Q�A�S�A�VA�S�A�VA�XA�XA�ZA�ZA�ZA�XA�M�A��A�ZA�^5Aİ!A�oA�
=A�^5A��A��-A���A�ȴA�9XA�jA��A�G�A�\)A�hsA��RA���A��
A�~�A���A�^5A���A��/A�O�A�M�A��A��A��A�A�A�E�A���A��A�oA��!A�E�A�33A|��Au�mAp��An��Al�\AjAg�Ad��Aa�A`ffA^{AYS�AUx�AP�ANM�AK��AJ�AG\)ADE�AB~�A?�A>-A=33A<��A<JA;
=A9t�A97LA9�A8�`A8r�A8�A8{A7��A7G�A6�/A6A�A5;dA3��A3|�A2��A2bA1\)A0E�A/��A.�HA.�!A.M�A.VA-�A,bNA+�
A+�PA+l�A*�RA)hsA(��A(��A($�A'C�A&��A&�!A& �A%�FA$�HA$  A#�PA"��A!��A!��A!��A!G�A �A �9A z�A bA7LA��A�uAVA1'A�A��Al�A33A��A^5A�
A7LA�jA1'AXAoA�`A�\AffAI�A�TA��AoA��A1'A|�A��A�DAI�A�A��AO�A�A��AVA��A�7AXA/A��Av�A1'A�
A�7AXA�AA��A5?A��Al�AS�A�A�`A�RA��A9XA��AS�A33AoA
�jA
�\A
 �A	��A	%A�9AjA�A\)A�\A=qA{A�A�wAx�A�A�!A=qA��A�TAoA�A�uAffA{A�A��Ap�A �`A ~�@���@�ȴ@�$�@��^@���@�?}@���@���@�I�@��@���@���@�|�@�j@�33@��h@���@�+@�x�@�u@@��@�G�@�K�@�J@�@���@�+@�9@ޟ�@���@ܴ9@۝�@��@���@�33@ԓu@ӕ�@ҧ�@�=q@�X@��@�%@�\)@�~�@�-@�O�@�S�@Ɨ�@��@�Z@��
@�S�@�hs@�9X@�V@��`@�Z@���@���@�l�@���@��@�~�@�V@���@���@�@�v�@��`@�r�@��@�ƨ@���@�?}@�V@��P@�\)@��@���@��@� �@���@�33@���@�p�@���@��@��^@��@�9X@�"�@���@�x�@�b@���@�=q@�hs@��u@��P@�33@���@���@���@��u@�\)@�K�@���@��\@�`B@�V@�9X@��A�-A�-A�1'A�(�A��A�$�A�$�A��A�"�A��A��A��A�oA�1A�{A�{A�bA�
=A�VA�VA�A�A�1A�%A�
=A�VA��A��A�bA�bA�{A�{A�oA��A��A��A��A��A�"�A�"�A�"�A�&�A�-A�/A�/A�5?A�33A�1'A�-A�-A�+A�1'A�9XA�9XA�5?A�?}A�G�A�E�A�I�A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        A��A�oA��A��A�"�A�+A�1'A�1'A�-A�1'A�=qA�I�A�K�A�K�A�K�A�M�A�C�A�A�A�G�A�E�A�K�A�I�A�K�A�K�A�K�A�Q�A�S�A�VA�S�A�VA�XA�XA�ZA�ZA�ZA�XA�M�A��A�ZA�^5Aİ!A�oA�
=A�^5A��A��-A���A�ȴA�9XA�jA��A�G�A�\)A�hsA��RA���A��
A�~�A���A�^5A���A��/A�O�A�M�A��A��A��A�A�A�E�A���A��A�oA��!A�E�A�33A|��Au�mAp��An��Al�\AjAg�Ad��Aa�A`ffA^{AYS�AUx�AP�ANM�AK��AJ�AG\)ADE�AB~�A?�A>-A=33A<��A<JA;
=A9t�A97LA9�A8�`A8r�A8�A8{A7��A7G�A6�/A6A�A5;dA3��A3|�A2��A2bA1\)A0E�A/��A.�HA.�!A.M�A.VA-�A,bNA+�
A+�PA+l�A*�RA)hsA(��A(��A($�A'C�A&��A&�!A& �A%�FA$�HA$  A#�PA"��A!��A!��A!��A!G�A �A �9A z�A bA7LA��A�uAVA1'A�A��Al�A33A��A^5A�
A7LA�jA1'AXAoA�`A�\AffAI�A�TA��AoA��A1'A|�A��A�DAI�A�A��AO�A�A��AVA��A�7AXA/A��Av�A1'A�
A�7AXA�AA��A5?A��Al�AS�A�A�`A�RA��A9XA��AS�A33AoA
�jA
�\A
 �A	��A	%A�9AjA�A\)A�\A=qA{A�A�wAx�A�A�!A=qA��A�TAoA�A�uAffA{A�A��Ap�A �`A ~�@���@�ȴ@�$�@��^@���@�?}@���@���@�I�@��@���@���@�|�@�j@�33@��h@���@�+@�x�@�u@@��@�G�@�K�@�J@�@���@�+@�9@ޟ�@���@ܴ9@۝�@��@���@�33@ԓu@ӕ�@ҧ�@�=q@�X@��@�%@�\)@�~�@�-@�O�@�S�@Ɨ�@��@�Z@��
@�S�@�hs@�9X@�V@��`@�Z@���@���@�l�@���@��@�~�@�V@���@���@�@�v�@��`@�r�@��@�ƨ@���@�?}@�V@��P@�\)@��@���@��@� �@���@�33@���@�p�@���@��@��^@��@�9X@�"�@���@�x�@�b@���@�=q@�hs@��u@��P@�33@���@���@���@��u@�\)@�K�@���@��\@�`B@�V@�9X@��A�-A�-A�1'A�(�A��A�$�A�$�A��A�"�A��A��A��A�oA�1A�{A�{A�bA�
=A�VA�VA�A�A�1A�%A�
=A�VA��A��A�bA�bA�{A�{A�oA��A��A��A��A��A�"�A�"�A�"�A�&�A�-A�/A�/A�5?A�33A�1'A�-A�-A�+A�1'A�9XA�9XA�5?A�?}A�G�A�E�A�I�A�I�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	}�B	}�B	}�B	}�B	}�B	}�B	|�B	|�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	z�B	$�B	|�B	�FB	�`B
33B
e`B
`BB
x�B
\)B
@�B
7LB
I�B
F�B
/B
5?B
8RB
5?B
5?B
[#B
�)BbNBq�Bn�BffB`BBJ�B33B�BB
�B
�wB
~�B
J�B
�B	�fB	�RB	�{B	k�B	ZB	2-B	$�B	hB	B��B��B��B��B�B�B��B��B	DB	
=B	+B	B	{B	+B	+B	2-B	C�B	YB	hsB	u�B	�7B	��B	��B	�B	�'B	�jB	��B	��B	��B	�/B	�mB	�mB	�fB	�B	��B
�B
/B
5?B
<jB
@�B
D�B
K�B
J�B
L�B
Q�B
P�B
M�B
K�B
J�B
J�B
N�B
Q�B
Q�B
P�B
O�B
M�B
L�B
N�B
P�B
O�B
Q�B
P�B
P�B
Q�B
R�B
Q�B
R�B
R�B
R�B
R�B
Q�B
O�B
O�B
O�B
O�B
Q�B
R�B
YB
ZB
[#B
YB
VB
S�B
R�B
R�B
R�B
R�B
S�B
R�B
S�B
T�B
W
B
W
B
W
B
W
B
VB
T�B
Q�B
O�B
N�B
O�B
O�B
Q�B
P�B
P�B
O�B
N�B
N�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
L�B
L�B
K�B
K�B
J�B
J�B
I�B
H�B
H�B
G�B
F�B
E�B
D�B
D�B
D�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
A�B
>wB
;dB
:^B
:^B
9XB
9XB
9XB
9XB
9XB
<jB
<jB
=qB
<jB
;dB
:^B
:^B
9XB
9XB
8RB
8RB
7LB
6FB
49B
2-B
1'B
0!B
0!B
0!B
0!B
/B
/B
/B
0!B
0!B
0!B
0!B
/B
,B
&�B
%�B
%�B
#�B
"�B
"�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
oB
hB
\B
hB
hB
VB
\B
JB
PB
VB
PB
VB
VB
VB
PB
PB
JB
VB
\B
bB
bB
hB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
$�B
$�B
$�B
&�B
%�B
&�B
'�B
(�B
'�B
)�B
(�B
)�B
(�B
+B
+B
-B
-B	{�B	|�B	}�B	}�B	� B	|�B	|�B	~�B	}�B	|�B	|�B	|�B	}�B	~�B	}�B	~�B	~�B	�B	|�B	}�B	~�B	~�B	}�B	� B	{�B	~�B	}�B	}�B	|�B	~�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	}�B	|�B	~�B	}�B	|�B	}�B	|�B	|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        B	}�B	}�B	}�B	}�B	}�B	}�B	|�B	|�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	z�B	$�B	|�B	�2B	�MB
3 B
eMB
`0B
x�B
\B
@qB
7;B
I�B
F�B
/
B
5/B
8BB
50B
50B
[B
�BbABq�Bn�BfZB`7BJ�B3(B�BB
�B
�nB
~�B
J�B
�B	�]B	�IB	�rB	k|B	ZB	2%B	$�B	`B	B��B��B��B��B�B�B��B��B	?B	
9B	'B	B	xB	*�B	+ B	2+B	C�B	YB	hsB	u�B	�8B	��B	��B	�B	�*B	�mB	��B	��B	��B	�5B	�sB	�tB	�mB	�B	��B
�B
/$B
5IB
<tB
@�B
D�B
K�B
J�B
L�B
Q�B
P�B
M�B
K�B
J�B
J�B
N�B
Q�B
Q�B
P�B
O�B
M�B
L�B
N�B
P�B
O�B
RB
P�B
P�B
RB
SB
RB
SB
SB
SB
SB
RB
O�B
O�B
O�B
O�B
RB
SB
Y7B
Z>B
[DB
Y9B
V&B
TB
SB
SB
SB
SB
TB
SB
TB
U%B
W2B
W2B
W3B
W4B
V.B
U)B
RB
PB
OB
PB
PB
RB
QB
QB
PB
O	B
O
B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
MB
MB
K�B
L B
J�B
J�B
I�B
H�B
H�B
G�B
F�B
E�B
D�B
D�B
D�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
A�B
>�B
;�B
:�B
:�B
9�B
9�B
9�B
9�B
9�B
<�B
<�B
=�B
<�B
;�B
:�B
:�B
9�B
9�B
8�B
8�B
7�B
6�B
4�B
2~B
1yB
0sB
0tB
0tB
0uB
/oB
/pB
/qB
0wB
0xB
0xB
0zB
/vB
,eB
'HB
&CB
&DB
$:B
#5B
#6B
!,B
!-B
!B
B
B
B
B
�B
B
�B
B
B

B
B
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
8B
-B
4B
1B
2B
3B
.B
6B
7B
3B
AB
BB
CB
JB
TB
[B
\B
]B
eB
lB
nB
oB
wB
~B
 �B
 �B
!�B
!�B
"�B
#�B
%�B
%�B
%�B
'�B
&�B
'�B
(�B
)�B
(�B
*�B
)�B
*�B
)�B
+�B
+�B
-�B
-�B	{�B	|�B	}�B	}�B	�B	|�B	|�B	~�B	}�B	|�B	|�B	|�B	}�B	~�B	}�B	~�B	~�B	��B	|�B	}�B	~�B	~�B	}�B	�B	{�B	~�B	}�B	}�B	|�B	~�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	}�B	|�B	}�B	}�B	|�B	|�B	|�B	|�B	|�B	|�B	|�B	}�B	~�B	}�B	|�B	~�B	}�B	|�B	}�B	|�B	|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202582021061413572020210614135720202107221611212021072216112120210722161121201807242202582021061413572020210614135720202107221611212021072216112120210722161121PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025820180724220258  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025820180724220258QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025820180724220258QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216141820210722161418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                