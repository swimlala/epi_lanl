CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  F�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  b�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  p�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ~�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ք   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ֌   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ɬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  լArgo profile    3.1 1.2 19500101000000  20180724220242  20210617131451  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c�E�4{@�c�E�4{11  @�c�""*0@�c�""*0@6�6����@6�6�����c���7"�c���7"11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@@  @�33@�ff@�  @�33A��A  A&ffAA��Aa��A�  A�33A�  A�33A���A�  A���A�B   BffB33BffB��B(ffB0ffB8ffB@  BH  BPffBXffB`  Bh  Bp  Bw��B��B�33B�  B���B�  B�ffB�  B���B�  B�33B�ffB�  B���B�  B�  B�33B�33B�33BǙ�B���B�ffB�33B���B�33B���B䙚B�ffB�33B�  B�33B���B�  B���C  C33C�C�C
33C33C33C  C�fC  C��C�C33C  C�fC   C"�C$  C&  C(�C*  C,  C.  C0�C2�C4  C6�C8�C:�C<33C>33C@�CB�CD  CF�CH�CJ  CL  CN�CP33CR  CT  CV  CX�CZ�C\  C^  C`�Ca�fCd  Cf�Ch�Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz�C|  C~�C�fC�  C��C��C��C��C��C�  C�  C��C��C��C��C�  C��3C�  C��C��C��C�  C�  C��C��C�  C��3C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C��C��C��C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��C�  C��C�  C�  C��C��C��C�  C�  C�  C��C�  C�  C�  C��3C��C�  C�  C�  C��C�  C��3C�  C��C��C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C��C�  C��3C�  C��C�  C��C��C�  C�  C��C��C��C��C��C��C��C��C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C��fC�ٚC�33D ��D��D��D�fD�fD�fD�fD	��D  D�D33DS3Dy�D�3D  D9�DffD�fDٚD3D@ Dl�D�fD�fD�fD� D ��D#3D$  D%�D&  D'�fD(��D)��D+�fD,�3D-� D/y�D0l�D1ffD2l�D4l�D5ffD6S3D8  D8��D9�3D;�3D<` D>  D>��D@��DA��DCL�DD�DE� DF� DH  DIl�DJ�DK��DM�DM�fDOffDQ33DRfDR�3DT�fDU�fDW` DX  DY� DZs3D[��D]�D^,�D_S3D`�fDa�fDc�Dds3DeٚDg9�Dh��DiY�DjٚDl9�Dm��Dn9�Do�fDp� Dq�fDsy�Dtl�Du� Dw�Dx9�Dy` Dz� >L��>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>���>L��>���>���>���>���>���?   >���?   ?��?333?L��?�  ?�  ?�ff?�  ?ٙ�@   @��@   @,��@@  @S33@l��@�  @�ff@�  @���@���@�33@�  @ə�@�ff@�ff@�  @���A��A��A33A33A!��A)��A0  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444144414414441444114111114111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ?L��?�  @&ff@`  @�33@�ff@�  @�33A	��A  A.ffAI��Ai��A�  A�33A�  A�33A���A�  A���A���B  B
ffB33BffB!��B*ffB2ffB:ffBB  BJ  BRffBZffBb  Bj  Br  By��B���B�33B�  B���B�  B�ffB�  B���B�  B�33B�ffB�  B���B�  B�  B�33B�33B�33Bș�B���B�ffB�33B���B�33BᙚB噚B�ffB�33B�  B�33B���B�  C ffC� C�3C��C��C
�3C�3C�3C� CffC� CL�C��C�3C� CffC � C"��C$� C&� C(��C*� C,� C.� C0��C2��C4� C6��C8��C:��C<�3C>�3C@��CB��CD� CF��CH��CJ� CL� CN��CP�3CR� CT� CV� CX��CZ��C\� C^� C`��CbffCd� Cf��Ch��Cj�3Cl�3Cn�3Cp�3Cr�3Ct�3Cv�3Cx�3Cz��C|� C~��C�33C�@ C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�@ C�33C�@ C�L�C�L�C�Y�C�@ C�@ C�L�C�L�C�@ C�33C�@ C�L�C�@ C�@ C�@ C�@ C�L�C�Y�C�@ C�@ C�@ C�@ C�L�C�Y�C�L�C�L�C�@ C�33C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�33C�L�C�@ C�@ C�@ C�L�C�@ C�33C�@ C�Y�C�L�C�@ C�@ C�@ C�L�C�L�C�L�C�Y�C�L�C�@ C�@ C�@ C�@ C�L�C�@ C�33C�@ C�L�C�@ C�L�C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�@ C�@ C�L�C�L�C�L�C�@ C�33C�@ C�@ C�@ C�@ C�@ C�L�C�L�C�@ C�&fC��C�s3D ��DٚD��D�fD�fD�fD	fD
�D  D9�DS3Ds3D��D�3D  DY�D�fD�fD��D33D` D��D�fD�fD�fD   D!�D#33D$  D%,�D&  D(fD(��D)ٚD+�fD,�3D-� D/��D0��D1�fD2��D4��D5�fD6s3D8@ D9�D9�3D;�3D<� D>@ D?�D@ٚDA��DCl�DD9�DE� DF� DH  DI��DJ9�DK��DM,�DM�fDO�fDQS3DR&fDS3DT�fDU�fDW� DX@ DY� DZ�3D[��D],�D^L�D_s3D`�fDa�fDc9�Dd�3De��DgY�Dh��Diy�Dj��DlY�Dm��DnY�Do�fDp� DrfDs��Dt��Dv  Dw9�DxY�Dy� Dz� ?333G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�?333G�O�G�O�G�O�?L��?fffG�O�?fff?�  ?���?���?�ffG�O�?�  ?�ff@   @��@   @,��@@  @L��@`  @s33@�ff@�  @�ff@�  @���@���@�33@�  @ٙ�@�ff@�ffA   AffA��A��A33A#33A)��A1��A8  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444144414414441444114111114111111111111111111111111111111                                                                                                                                                                                                                                                                                                           @ @ �@ V@ {@ �@ #�@ (�@ 0x@ 7L@ =q@ F�@ R�@ `B@ m:@ z3@ ��@ ��@ ��@ �~@ ��@ �*@ �t@ ��@ ��@@b@g@-@:�@G�@UU@c�@qS@~K@��@��@��@�9@�>@�7@�/@�@��@�@�@""@0x@>�@K@X@ff@t@�d@�@��@�M@��@�W@�O@��@�@��@J@B@&;@33@A�@P�@\)@i!@ww@�|@�u@�@�r@�@��@�
@�@�Y@��@V@�@(�@5�@DD@R�@_�@m:@{�@��@�0@��@�-@��@��@�#@��@�q@�@o@g@-@:@H]@V@b�@p�@~�@�P@��@�A@��@�>@��@��@�@��@%@{@"�@0x@>�@Lu@Z@g�@uk@�@��@�a@�Y@�R@ƨ@��@�H@�@�E@
�@�@&;@33@@�@O0@\�@j@x&@�@�@�m@��@�k@��@�
@�@�@ �@�@�@(�@7L@DD@Q�@_�@m:@{�@��@�0@��@�~@�&@�|@��@��@�q@	j@	b@	�@	-@	:�@	G�@	UU@	b�@	p�@	~K@	��@	��@	��@	��@	@	��@	ލ@	�4@	�,@
�@
{@
"�@
/�@
=q@
K@
X@
g@
t@
��@
�\@
��@
��@
��@
��@
��@
��@
��@
��@
=@�@&;@3�@B8@O0@\)@i�@ww@�@�u@�m@�f@��@�@�
@�`@�@  @�@�@)�@7L@D�@R�@`B@n�@{�@��@�0@��@�-@��@��@��@�@��@j@@�@-@:�@G�@S�@`�@�U@��@�\@@)�@D�@`A@�H@�F@Ӡ@�@�@.l@M�@��@�@ψ@�@�@2�@S�@t@�$@�-@��@�@�@+�@e	@~K@��@��@��@^@�@O0@hs@��@��@є@�4@1@>�@Yn@r�@��@�@�C@@�@G�@^�@��@��@��@��@�@,`@UU@|?@��@��@�;@�@g@P�@g@�W@�-@�@�,@�@:@M$@r@�$@��@�C@�@*@9X@^5@�p@��@є@�@�@2�@X@i!@��@�@�|@�~@o@:@[z@z3@��@�R@ ^G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�G�O�@ ^G�O�G�O�G�O�@ @ �G�O�@ �@ j@ @ �@ vG�O�@ �@ �@ 
=@ �@ �@ @ @ o@ {@ �@ B@ O@ �@ �@ !s@ $�@ &;@ (�@ +@ -�@ 1'@ 33@ 5�@ 8�@ <@ >�@ B8@ D�@ H]@ KG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ZA�ZA�\)A�`BA�dZA�hsA�hsA�n�A�r�A�t�A�t�A�(�A�JA��yA���Aĺ^Ağ�AċDA�ffA�Q�A�$�A��yAÉ7A��A��;A´9A�A�%A�(�A�oA�S�A��-A���A��A��A��hA�~�A��A�XA�ƨA�dZA�
=A��jA�O�A��wA� �A���A���A��PA�t�A�
=A��9A���A��7A�hsA�9XA��\A�C�A�33A��A��;A��`A�`BA�1'A�E�A��A��#A��wA��^A�ffA�/A���A�1A�\)A��A��A���A���A��A��wA�`BA��A��A��A��#A�ȴA���A��A�;dA���A��A�XA���A�v�A���A�v�A��jA��A�VA��DA�;dA��A�z�A��FA�I�A��A���A�l�A��A�&�A�A�\)A��A��A�33A��yA��!A�%A��A�t�A���A�5?A�ĜA��hA�+A�|�A�K�A���A��A�+A|�AC�A�A~n�A}�
A}hsA}A{�mAy�hAvAt�AqoAl�Ai��Ae�AcS�Aa��A^5?A[\)AW�;AS�TAPbNAOO�AN��AM�AL�DAIp�AG`BAD�RAC7LABn�A?�;A>�DA<n�A<1A;�A;��A;�A:M�A9�A7��A6�!A4r�A3�hA3
=A1hsA/�A.�HA.��A.��A.M�A-/A+�^A+�A+`BA++A*ĜA)��A)S�A)&�A(��A(��A(=qA'�A'�A&��A&ffA%�#A%A#`BA"=qA!�^A!%A33A�mA5?AK�A�yAffAA�;At�A/A�uA��A�-A�PAAVA�^AG�AȴA9XA��A�hA�7Ap�A;dA�/A^5A��AXA��A�A/A	|�A��A�DA5?AVA�TAS�A^5A�A�`A�A�@���@��P@�/@�C�@���@�ƨ@�K�@��@��@�R@�x�@�?}@@�(�@��@���@�33@���@���@�^5@��@��@\@��D@�ff@�V@�7L@��@���@��y@��!@�@��/@�33@�ff@���@�z�@��@���@�I�@�A�@�Ĝ@�+@��@���@�v�@�@��R@���@��F@��
@���@��@��^@��@��h@��`@�`B@�@���@�^5@��+@�V@��@���@�dZ@���@�^5@�=q@�@��@�`B@��/@�Z@���@��T@���@�G�@��/@��j@��@�33@��\@�M�@���@��@�j@�\)@��\@���@�Z@�dZ@�ȴ@���@��!@���@�n�@�@�p�@�hs@�Ĝ@�I�@���@�K�@�"�@�^5@���@��@��j@���A�ZA�XA�ZA�VA�XA�XA�bNA�ZA�XA�Q�A�ZA�ZA�ZA�ZA�\)A�ZA�ZA�XA�\)A�ZA�XA�\)A�ZA�VA�XA�XA�XA�XA�XA�ZA�ZA�\)A�^5A�\)A�^5A�^5A�ZA�\)A�\)A�ffA�dZA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�jA�l�A�n�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           A�ZA�ZA�\)A�`BA�dZA�hsA�hsA�n�A�r�A�t�A�t�A�(�A�JA��yA���Aĺ^Ağ�AċDA�ffA�Q�A�$�A��yAÉ7A��A��;A´9A�A�%A�(�A�oA�S�A��-A���A��A��A��hA�~�A��A�XA�ƨA�dZA�
=A��jA�O�A��wA� �A���A���A��PA�t�A�
=A��9A���A��7A�hsA�9XA��\A�C�A�33A��A��;A��`A�`BA�1'A�E�A��A��#A��wA��^A�ffA�/A���A�1A�\)A��A��A���A���A��A��wA�`BA��A��A��A��#A�ȴA���A��A�;dA���A��A�XA���A�v�A���A�v�A��jA��A�VA��DA�;dA��A�z�A��FA�I�A��A���A�l�A��A�&�A�A�\)A��A��A�33A��yA��!A�%A��A�t�A���A�5?A�ĜA��hA�+A�|�A�K�A���A��A�+A|�AC�A�A~n�A}�
A}hsA}A{�mAy�hAvAt�AqoAl�Ai��Ae�AcS�Aa��A^5?A[\)AW�;AS�TAPbNAOO�AN��AM�AL�DAIp�AG`BAD�RAC7LABn�A?�;A>�DA<n�A<1A;�A;��A;�A:M�A9�A7��A6�!A4r�A3�hA3
=A1hsA/�A.�HA.��A.��A.M�A-/A+�^A+�A+`BA++A*ĜA)��A)S�A)&�A(��A(��A(=qA'�A'�A&��A&ffA%�#A%A#`BA"=qA!�^A!%A33A�mA5?AK�A�yAffAA�;At�A/A�uA��A�-A�PAAVA�^AG�AȴA9XA��A�hA�7Ap�A;dA�/A^5A��AXA��A�A/A	|�A��A�DA5?AVA�TAS�A^5A�A�`A�A�@���@��P@�/@�C�@���@�ƨ@�K�@��@��@�R@�x�@�?}@@�(�@��@���@�33@���@���@�^5@��@��@\@��D@�ff@�V@�7L@��@���@��y@��!@�@��/@�33@�ff@���@�z�@��@���@�I�@�A�@�Ĝ@�+@��@���@�v�@�@��R@���@��F@��
@���@��@��^@��@��h@��`@�`B@�@���@�^5@��+@�V@��@���@�dZ@���@�^5@�=q@�@��@�`B@��/@�Z@���@��T@���@�G�@��/@��j@��@�33@��\@�M�@���@��@�j@�\)@��\@���@�Z@�dZ@�ȴ@���@��!@���@�n�@�@�p�@�hs@�Ĝ@�I�@���@�K�@�"�@�^5@���@��@��j@���A�ZA�XA�ZA�VA�XA�XA�bNA�ZA�XA�Q�A�ZA�ZA�ZA�ZA�\)A�ZA�ZA�XA�\)A�ZA�XA�\)A�ZA�VA�XA�XA�XA�XA�XA�ZA�ZA�\)A�^5A�\)A�^5A�^5A�ZA�\)A�\)A�ffA�dZA�dZA�dZA�dZA�ffA�hsA�hsA�hsA�jA�l�A�n�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�v�A�t�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�fB
�`B
�fB
�fB
�fB
�fB
�fB
�mB
�mB
�mB
�mB
�HB
�NB
�BB
�;B
�;B
�;B
�BB
�TB
�mB
�B
�B  B	7BVBoB�BH�Bs�B�\B��BɺB��B1B�BP�Bv�B�B�1B��B�B�B�B��B��B��B�B�-B�9B�?B�FB�RB�RB�XB�^B�jB�wB��B�}B�}B�jB�jB�wB�qB�?B�?B�'B��B��B��B��B��B��B�{B�VB�%Bx�Bw�Br�Bn�Bk�Be`BD�B7LB33B33B33B5?B49B1'B)�BDB��B�FB��B��B�DBl�B]/BP�BM�BI�BA�B8RB33B33B0!B+B�BB
��B
�fB
�5B
��B
��B
ɺB
ƨB
��B
�}B
�^B
�-B
�B
��B
��B
��B
�\B
�DB
�B
y�B
t�B
k�B
jB
gmB
`BB
ZB
VB
Q�B
H�B
5?B
$�B
�B
1B	�fB	��B	�dB	��B	��B	�B	y�B	ffB	Q�B	>wB	6FB	2-B	,B	%�B	�B	JB	  B��B��B�B�mB�;B�5B�/B�#B�
B��B��BƨBB�RB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�bB�\B�VB�DB�=B�=B�+B�%B�B~�B|�B|�Bv�Bt�Bo�Bn�Bn�Bp�Bt�Bw�Bv�Bt�Bs�Bv�Bx�Bx�Bw�Bu�Bv�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bv�Bv�Be`BK�BD�BC�BB�BB�BJ�BO�BL�BK�BJ�BO�BT�BN�B@�B:^B8RB6FB5?B6FB6FB6FB49B33B7LBG�Bp�BXBH�BC�B=qBD�BJ�BJ�BS�BS�BW
BXB^5B^5BbNBffBjBr�Bq�Bs�Bt�B|�B�B�B�=B�=B�JB��B��B��B��B��B�B��B��B�B��B��B�TB�sB�B�B�HB��B	B	JB	!�B	.B	:^B	=qB	H�B	S�B	ffB	jB	y�B	|�B	�B	�B	�+B	�VB	�{B	��B	��B	��B	��B	��B	�!B	�!B	�FB	�RB	�qB	�wB	��B	B	ÖB	ȴB	��B	��B	��B	�B	�)B	�#B	�/B	�/B	�/B	�;B	�HB	�HB	�TB	�ZB	�`B	�yB	�yB	�B	�B	�B	�B	�B
�mB
�fB
�fB
�fB
�fB
�fB
�mB
�fB
�sB
�mB
�fB
�`B
�fB
�fB
�`B
�fB
�`B
�fB
�`B
�`B
�fB
�`B
�`B
�fB
�fB
�fB
�fB
�`B
�fB
�`B
�`B
�fB
�`B
�`B
�fB
�`B
�fB
�fB
�fB
�fB
�`B
�fB
�fB
�`B
�fB
�fB
�fB
�fB
�`B
�mB
�mB
�mB
�fB
�mB
�fB
�mB
�fB
�mB
�mB
�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           B
�=B
�7B
�>B
�>B
�>B
�>B
�?B
�FB
�FB
�GB
�GB
�#B
�)B
�B
�B
�B
�B
� B
�3B
�LB
�_B
�B
��B	B8BRB�BH�Bs�B�AB��BɟB��BBzBP�Bv�B�B�B��B��B�B�B��B��B��B��B�B�(B�.B�6B�BB�CB�IB�PB�]B�jB�wB�qB�rB�`B�`B�nB�hB�7B�7B� B��B��B��B��B��B��B�wB�SB�"Bx�Bw�Br�Bn�Bk�Be`BD�B7MB35B35B36B5BB4=B1,B*BIB��B�LB��B��B�KBl�B]7BP�BM�BI�BA�B8]B3>B3?B0-B+B�B'B
��B
�tB
�DB
�B
��B
��B
ƹB
��B
��B
�qB
�@B
�B
��B
��B
��B
�rB
�ZB
�0B
y�B
t�B
k�B
j�B
g�B
`\B
Z8B
VB
RB
H�B
5\B
$�B
�B
OB	�B	�B	��B	�B	��B	�>B	y�B	f�B	RB	>�B	6gB	2OB	,*B	&B	�B	mB	 #B�B�B��B�B�aB�[B�VB�JB�2B�B��B��B¹B�|B�jB�XB�:B�B�
B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�sB�tB�bB�]B�DB3B}'B}(BwBt�Bo�Bn�Bn�Bp�Bt�BxBwBt�Bs�Bw	ByByBxBvBwBxBxBxBxBxBxBxBxBxByBxBwBwBe�BLBD�BC�BB�BB�BKBP-BMBLBKBP/BUNBO*B@�B:�B8�B6�B5�B6�B6�B6�B4�B3�B7�BHBp�BXlBIBC�B=�BD�BK#BK$BT\BT]BWqBXxB^�B^�Bb�Bf�Bj�Bs!BrBt*Bu1B}eB�~B��B��B��B��B�JB�WB�-B�ZB�gB��B�[BՇB֎B�~B�B��B�B�B�B��B��B	�B	�B	"eB	.�B	:�B	>B	IUB	T�B	g
B	k$B	z�B	}�B	��B	��B	��B	�B	�+B	�DB	�_B	��B	��B	��B	��B	��B	�B	�B	�0B	�7B	�JB	�RB	�ZB	�yB	ˈB	ϡB	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�-B	�4B	�<B	�VB	�XB	�kB	�zB	�B	�B	�B
�DB
�=B
�=B
�=B
�=B
�=B
�DB
�=B
�JB
�DB
�=B
�7B
�=B
�=B
�7B
�=B
�7B
�=B
�7B
�7B
�=B
�7B
�7B
�=B
�=B
�=B
�=B
�7B
�=B
�7B
�7B
�=B
�7B
�7B
�>B
�8B
�>B
�>B
�>B
�>B
�8B
�>B
�>B
�8B
�>B
�?B
�?B
�?B
�9B
�FB
�FB
�FB
�?B
�FB
�@B
�GB
�@B
�GB
�GB
�GG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202422021061413551120210614135511202106171311302021061713113020210617131130201807242202422021061413551120210614135511202106171311302021061713113020210617131130PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024220180724220242  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024220180724220242QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024220180724220242QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145120210617131451IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                