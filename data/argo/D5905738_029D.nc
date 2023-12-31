CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:46Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                       HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                      HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                      HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �     HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � @      � @Argo profile    3.1 1.2 19500101000000  20180917231346  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�~>|e�V@�~>|e�V11  @�~>q�;�@�~>q�;�@6[���@6[����c�7��~�c�7��~11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
AB  AA  AA  >���?L��@��@L��@�  @���@���@�33A33A  A!��A@  Aa��A�  A�33A�  A�  A�33A���AᙚA���B   B��B33B  B ffB(ffB0ffB8  B?��BH  BP��BXffB`  BhffBpffBxffB�33B�33B�  B�  B�ffB�ffB�33B���B���B�  B�  B���B�  B�33B�  B�  B�33B�33B�  B̙�B�ffB�33B�33B�33B���B�ffB�ffB�ffB�33B�33B�  B�  C   C  C�C�CL�C
  C  C�C�C�C�C33C�fC�fC  C  C   C!�fC$  C&  C(33C*�C,  C.  C/��C2  C433C6L�C8�C9��C;��C=�fC?�fCB  CD  CF�CH�CJ�CL33CNL�CP�CQ��CS�fCV�CX33CZ�C[��C^�C`  Ca��Cd33Cf�Ch�Ci�fCk�fCn  Co�fCr  Ct  Cv�Cx�CzL�C|  C~�C��C�  C��C��C�  C��C�&fC�  C��C��C�  C��C�33C��C��3C��C�33C��C��3C��C�&fC�  C��C�33C��C��3C�  C�&fC��3C�  C��C��C��C��C��C��C�&fC��C�&fC�&fC��C�  C�  C��3C��fC�  C��C��C�  C��C��C��C��3C��C��C�&fC��C�  C��C�&fC��C��3C��3C��3C�  C��3C��3C�  C��C��C��C��C�ٚC��fC��fC�ٚC��3C��3C��3C�  C�  C��C�&fC��C��fC�  C��C�  C�  C��C��C��3C��C��C��C��3C��C��C�  C��3C��fC��C�  C��3C��C��3C��3C��3C��fC�  C��C��C�  C��C��C�  C��C��C�  C��C��C�  C��fD� DFfDfD
� D�fDFfD  D��DffD3D��D Y�D"��D%s3D'�3D*s3D,ٚD/@ D1��D4�D6� D8��D;ffD=� D@` DB� DEs3DG�3DJs3DL��DO�3DR9�DTٚDW� DZ3D\��D_FfDa�fDd��Dg@ Di� Dl�3DoS3Dr  Dt��Dws3Dz  D|Y�D~��D�ɚD�fD�c3D��3D���D�)�D�ffD��3D��fD�3D�C3D�y�D�� D��D�  D�VfD���D��3D��fD�33D�s3D���D��3D�  D�` D���D��3D�33D�|�D�� D���D�@ D�|�D���D�� D�3D�I�D�vfD��3D���D���D�&fD�S3D�vfD���D��3D��3D�	�D�)�D�P D�p D���D�� D��fD�3D�<�D�l�Dœ3D�� D�� D�  D�S3Dˉ�D��3D���D�,�D�` DіfD�� D�  D�33D�c3Dד3D��fD���D�&fD�VfD݀ Dް D�� D�fD�,�D�VfD�vfD��D�fD�ɚD��fD��D��3D�  D�3D�  D�0 D�@ D�I�D�Y�D�ffD�l�D�vfD��3D���D���D��fD��fD�� D�� D���D���D��fE k3E �E{3E�E� E�E�3E$�E� E;3E�fEP Ei�E	�E
)�EA�Ec3E E0 EP Et�E�fED�Ec3E� E�3E1�E@ ENfE� E��E3E ��E!��E#fE$fE%��E&��E'� E)c3E*VfE+� E-8 E.1�E/�fE0��E2.fE3)�E4��E5��E7 E8�E9h E:� E;�3E?( EBnfEE1�EHq�EK�fEN�fEQ�fET� EX<�E[$�E^c3Ea4�EdVfEg�3Ej�3Em�3Eq  Et6fEwK3EzY�E}� E�H�E���E�T�E�� E��fE�fE���E�9�E��3E�d E���>���?��>���?��?��?   ?��?��?333?333?L��?L��?fff?�  ?���?�33?�  ?ٙ�?�ff@ff@33@   @   @9��@Fff@`  @l��@�  @���@�33@���@�ff@�33@���@ə�@ٙ�@�33@�  A   AffA33A33A33A#33A(  A1��A9��A@  AFffAL��AVffA\��Ad��Al��As33A{33A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414141111111111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?fff?�ff@,��@l��@�  @���@���@�33A33A  A)��AH  Ai��A�  A�33A�  A�  A�33A���A噚A���B  B	��B33B  B"ffB*ffB2ffB:  BA��BJ  BR��BZffBb  BjffBrffBzffB�33B�33B�  B�  B�ffB�ffB�33B���B���B�  B�  B���B�  B�33B�  B�  B�33B�33B�  B͙�B�ffB�33B�33B�33B���B�ffB�ffB�ffB�33B�33B�  B�  C � C� C��C��C��C
� C� C��C��C��C��C�3CffCffC� C� C � C"ffC$� C&� C(�3C*��C,� C.� C0L�C2� C4�3C6��C8��C:L�C<L�C>ffC@ffCB� CD� CF��CH��CJ��CL�3CN��CP��CRL�CTffCV��CX�3CZ��C\L�C^��C`� CbL�Cd�3Cf��Ch��CjffClffCn� CpffCr� Ct� Cv��Cx��Cz��C|� C~��C�Y�C�@ C�Y�C�Y�C�@ C�L�C�ffC�@ C�L�C�Y�C�@ C�L�C�s3C�L�C�33C�L�C�s3C�L�C�33C�L�C�ffC�@ C�L�C�s3C�L�C�33C�@ C�ffC�33C�@ C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�ffC�L�C�ffC�ffC�L�C�@ C�@ C�33C�&fC�@ C�Y�C�L�C�@ C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�Y�C�@ C�L�C�ffC�L�C�33C�33C�33C�@ C�33C�33C�@ C�L�C�Y�C�Y�C�L�C��C�&fC�&fC��C�33C�33C�33C�@ C�@ C�Y�C�ffC�Y�C�&fC�@ C�Y�C�@ C�@ C�Y�C�L�C�33C�L�C�Y�C�Y�C�33C�Y�C�L�C�@ C�33C�&fC�L�C�@ C�33C�L�C�33C�33C�33C�&fC�@ C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�Y�C�L�C�@ C�&fD� DffD&fD
� D�fDffD  DٚD�fD33DٚD y�D#�D%�3D(3D*�3D,��D/` D1��D4,�D6� D9�D;�fD>  D@� DC  DE�3DH3DJ�3DM�DO�3DRY�DT��DW� DZ33D\��D_ffDbfDd��Dg` Dj  Dl�3Dos3Dr  DtٚDw�3Dz@ D|y�D�D�ٚD�&fD�s3D��3D���D�9�D�vfD��3D��fD�3D�S3D���D�� D���D�0 D�ffD���D��3D�fD�C3D��3D���D��3D�0 D�p D���D�3D�C3D���D�� D��D�P D���D���D�� D�#3D�Y�D��fD��3D���D�	�D�6fD�c3D��fD���D��3D��3D��D�9�D�` D�� D���D�� D��fD�#3D�L�D�|�Dţ3D�� D�  D�0 D�c3D˙�D��3D��D�<�D�p DѦfD�� D� D�C3D�s3Dף3D��fD�	�D�6fD�ffDݐ D�� D�� D�fD�<�D�ffD�fD��D��fD�ٚD��fD���D�3D� D�#3D�0 D�@ D�P D�Y�D�i�D�vfD�|�D��fD��3D���D���D��fD��fD�� D�� D���D�ɚD��fE s3E ��E�3E�E� E�E�3E,�E� EC3E�fEX Eq�E	�E
1�EI�Ek3E E8 EX E|�E�fEL�Ek3E� E�3E9�EH EVfE� E�E3E ��E!��E#fE$fE%��E&��E(  E)k3E*^fE+� E-@ E.9�E/�fE0��E26fE31�E4��E5��E7 E8	�E9p E:� E;�3E?0 EBvfEE9�EHy�EK�fEN�fEQ�fET� EXD�E[,�E^k3Ea<�Ed^fEg�3Ej�3Em�3Eq( Et>fEwS3Eza�E}� E�L�E���E�X�E�� E��fE�fE���E�=�E��3E�h E���?fffG�O�?fffG�O�G�O�?�  G�O�?���G�O�?���G�O�?�ff?�33?�  ?ٙ�?�33@   @��@33@&ff@333G�O�@@  @Y��@fff@�  @�ff@�  @���@�33@���@�ff@�33@���@ٙ�@陚@�33A   A  AffA33A33A#33A+33A0  A9��AA��AH  ANffAT��A^ffAd��Al��At��A{33A���A���A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141441414141111111111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ �@ v@ @ �@ O@ !s@ (G@ 0x@ 7�@ =q@ D�@ Q�@ `B@ m:@ z3@ ��@ �0@ �(@ �-@ ��@ �|@ �t@ �m@ �e@j@�@g@-@:@F�@UU@dZ@qS@~K@��@�H@��@��@�>@�7@��@��@��@�@�@!s@/�@=q@Ji@X�@g@t@��@�@��@��@�^@�W@�O@��@�@��@�@B@&�@3�@A�@N�@\)@i�@ww@��@�u@�z@�@��@�@׹@�`@�@^@�@�@(�@6�@DD@Q=@_�@m:@|?@�7@�0@��@�!@�&@�*@܀@��@�e@@b@
@,`@:@H]@V@c�@r@�W@��@�<@��@��@��@��@܀@�4@�,@v@�@"�@0x@<�@Ji@X�@e�@t@��@�@��@��@�R@ƨ@��@�H@�L@��@
=@�@'�@33@A�@O�@\)@j@z3@��@�@�@��@�k@ȴ@׹@�@�Y@ �@b@�@(G@6�@FQ@Q=@_�@m�@{�@��@��@�5@��@�2@�|@܀@�(@�q@	j@	@	
@	+@	:@	I@	V@	b�@	qS@	�@	��@	��@	��@	�F@	Ĝ@	є@	��@	�4@	�9@
�@
�@
!s@
/@
=q@
Ji@
X@
ff@
t�@
�@
��@
��@
��@
��@
Ĝ@
є@
��@
�@@
��@
=@�@&�@5?@B8@M$@\)@k.@ww@�@�$@�@�f@�k@��@�h@�@�@ �@�@�@'�@7L@DD@Q=@`B@l�@z3@��@��@��@��@��@��@��@�y@��@�@o@�@-�@:�@G�@S�@��@7L@�d@��@�@c�@�@�~@A�@��@��@�@`�@��@�(@.l@o�@�~@�@4�@ww@��@�E@@�@�@�c@�@S�@�<@�/@$.@l�@�9@��@B�@��@��@�@`�@��@�Y@<@�+@�7@�@e	@�@��@2�@y�@��@�@K�@�@խ@B@Yn@��@ލ@"�@e	@�A@�(@,`@n�@��@�@ 4�@ x&@ �j@ ��@!A�@!�@!�c@"�@"V@"�H@"��@#%�@#i!@#�@#�@$2�@$t@$��@$��@%7�@%x&@%��@%��@&7�@&x&@&�F@&�@'3�@'qS@'�!@'�@(,`@(i�@(�M@(�@)&�@)g@)��@)�m@*&;@*ff@*�A@*�@+)�@+k�@+��@+�@,2�@,t@,�F@,�,@-:@-{�@-�j@-�E@.>�@.�W@.��@/]@/@�@/��@/@0]@0@,@0�@0�@0��@17�@1r�@1�@1�@2g@2X�@2�u@2��@3�@3@�@3y�@3��@3��@4$�@4]�@4��@4ψ@5	�@5B8@5uk@5�@5�@6
@6Wb@6��@6��@7@7>�@7y�@7�~@7�4@8&�@8a�@8�@8�h@9�@9N�@9ƨ@:y�@:�@;j@;�@<��@=�@=��@>�@>�C@?B�@?�@@6�@@�@A\�@A�7@BC�@B�Y@Cg@C�h@D�W@D��@E��@F�@F�z@G
�@G��@HE�@H�f@IK@I�@JR�@J�}@Ke	@L%@LqS@M�@M|�@NO@N�d@OO@O�R@P @Q�\@R�@T"�@U��@V�@X5�@Yz2@Z�t@\B�@]�W@^�@`�@al�@b��@dB@e�@f�H@h2�@i�@j��@l6�@mx&@n�#@p�@qww@r��@t$.@u{�@v�+@x+�@y��@z��@ �G�O�@ �G�O�G�O�@ jG�O�@ G�O�@ �G�O�@ v@ %@ �@ 1@ 	�@ 
=@ �@ J@ V@ �G�O�@ @ �@ *@ �@ B@ O@ [@ g@ !s@ #�@ &;@ (G@ +@ .l@ 0x@ 33@ 6�@ 9X@ ;d@ >�@ B8@ E�@ G�@ K�@ O0@ Q�@ T�@ Wb@ [z@ ^5@ a�@ e	@ g�@ k.@ n�@ r@ t�@ x&G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�dZA�n�A�jA�jA�p�A�ffA�Q�A�?}A�=qA�=qA�O�A�A�A�/A�/A�&�A��A�%A�  A���A���A���A���A���A��A��A��A��mA��#A�A�A��;A��/AήA���Aʝ�Aɛ�A�Q�A�z�A��TA�p�A���A���A�dZA�%A���A��hA���A�K�A��A���A���A�|�A�1A���A���A��A���A���A���A���A�JA�?}A���A�G�A�`BA���A�K�A��9A�  A�~�A��A��-A�v�A�ZA�$�A��!A��
A��wA��-A�O�A���A�I�A���A�=qA�JA�jA�t�A�l�A���A��7A��^A���A�v�A�;dA���A��9A�ZA�dZA��FA���A��A��RA�|�A�ƨA��+A�VA��;A���A��7A�A��/A�A�A��DA��
A�;dA��TA7LA}��A{�hAx�\Av�Au��At��Aq��An�HAnQ�Am�AkƨAkoAjĜAj~�Ai�#Ag�Af�RAf9XAe�Ae�Ae/AdM�AcoAb  A_VA\�A\1AZ�/AY��AX��AXffAWp�AV�DAUAT~�AS7LARAP�AO�^AMl�AJ��AJ-AI�AIp�AI33AH��AHĜAG/AD��AC�AA�-A?7LA=��A<��A;`BA:-A8~�A7p�A6�9A6bA5dZA5�A4�yA4�uA3XA2ZA1K�A.��A-A-�A+��A)�A(�/A(v�A'�;A'+A&~�A&  A%&�A#��A#VA"-A!\)A �A E�A JA �A�RA�TA+A�HA~�AAn�A�RA�FA�!A$�AA�7A�A�PA
^5A	�A	x�A�wA��AA��A��A��A ��@��@��D@�z�@�bN@�bN@�  @�7L@�|�@�ȴ@���@���@�
=@��@���@�@��m@�7L@�V@�=q@�9@���@�{@�=q@�V@�n�@�=q@�X@���@�E�@��
@�C�@Ӆ@�dZ@ř�@��m@�K�@�x�@��@���@��P@��@�A�@��@��@���@�1@���@��`@��u@�&�@�$�@�M�@�n�@��@��w@�x�@�1'@�|�@�^5@���@���@��@�n�@�?}@���@�o@�$�@��7@��u@�S�@��+@��j@��@�|�@��R@�=q@�V@|Z@{"�@yX@vv�@s�F@rM�@n��@l�/@k�@i��@g��@f��@d1@cS�@aX@_�w@^ff@\I�@YX@Xb@V$�@U/@T��@TZ@R�H@Q��@OK�@M�@K�m@K@J-@I�@HA�@F��@Ep�@C�
@A��@@b@=�T@=@<Z@;t�@:=q@97L@8��@7�;@7�w@6��@5@4j@3�F@2^5@1�^@01'@.��@.5?@-�-@,��@,Z@+@*-@)x�@)%@'�;@'+@&�R@%@$��@#�m@#�F@"M�@!�^@ �u@�@v�@�@��@��@@^5@�#@hs@��@�@�w@�P@��@��@z�@S�@�\@Ĝ@1'@�R@�@��@ƨ@dZ@
��@
=q@	hs@	hs@�@�@V@{@`B@V@I�@��@C�@n�@�@��@ �`@  �?��?�{?�/?��D?���?�~�?���?���?���?��?�/?�ƨ?�^5?�r�?�ff?�S�?�33?���?�G�?���?�{?��?��m?�"�?�7L?�1'?׮?�$�?ա�?��?�M�?�&�?�bN?��;?θR?Ͳ-?�p�?��?�I�?�I�?��m?�ƨ?�C�?���?�7L?ȓu?��?��?�`B?�?�t�?�Ĝ?�v�?�V?��?�I�?���?�"�?�?��?�^5?�^5?��^?�X?���?�7L?�X?�X?��^?�=q?���?�C�?�ƨ?��D?�/?��h?�5??��?���?�A�A�jA�dZA�^5A�bNA�bNA�dZA�dZA�dZA�\)A�dZA�ffA�ffA�ffA�ffA�hsA�jA�jA�l�A�l�A�n�A�n�A�p�A�l�A�l�A�jA�jA�hsA�jA�l�A�l�A�p�A�r�A�l�A�ffA�bNA�\)A�O�A�K�A�A�A�=qA�=qA�=qA�=qA�=qA�=qA�?}A�O�A�S�A�XA�VA�I�A�?}A�?}A�9XA�33A�/A�/A�/A�1'A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          A�dZA�dZA�n�A�jA�jA�p�A�ffA�Q�A�?}A�=qA�=qA�O�A�A�A�/A�/A�&�A��A�%A�  A���A���A���A���A���A��A��A��A��mA��#A�A�A��;A��/AήA���Aʝ�Aɛ�A�Q�A�z�A��TA�p�A���A���A�dZA�%A���A��hA���A�K�A��A���A���A�|�A�1A���A���A��A���A���A���A���A�JA�?}A���A�G�A�`BA���A�K�A��9A�  A�~�A��A��-A�v�A�ZA�$�A��!A��
A��wA��-A�O�A���A�I�A���A�=qA�JA�jA�t�A�l�A���A��7A��^A���A�v�A�;dA���A��9A�ZA�dZA��FA���A��A��RA�|�A�ƨA��+A�VA��;A���A��7A�A��/A�A�A��DA��
A�;dA��TA7LA}��A{�hAx�\Av�Au��At��Aq��An�HAnQ�Am�AkƨAkoAjĜAj~�Ai�#Ag�Af�RAf9XAe�Ae�Ae/AdM�AcoAb  A_VA\�A\1AZ�/AY��AX��AXffAWp�AV�DAUAT~�AS7LARAP�AO�^AMl�AJ��AJ-AI�AIp�AI33AH��AHĜAG/AD��AC�AA�-A?7LA=��A<��A;`BA:-A8~�A7p�A6�9A6bA5dZA5�A4�yA4�uA3XA2ZA1K�A.��A-A-�A+��A)�A(�/A(v�A'�;A'+A&~�A&  A%&�A#��A#VA"-A!\)A �A E�A JA �A�RA�TA+A�HA~�AAn�A�RA�FA�!A$�AA�7A�A�PA
^5A	�A	x�A�wA��AA��A��A��A ��@��@��D@�z�@�bN@�bN@�  @�7L@�|�@�ȴ@���@���@�
=@��@���@�@��m@�7L@�V@�=q@�9@���@�{@�=q@�V@�n�@�=q@�X@���@�E�@��
@�C�@Ӆ@�dZ@ř�@��m@�K�@�x�@��@���@��P@��@�A�@��@��@���@�1@���@��`@��u@�&�@�$�@�M�@�n�@��@��w@�x�@�1'@�|�@�^5@���@���@��@�n�@�?}@���@�o@�$�@��7@��u@�S�@��+@��j@��@�|�@��R@�=q@�V@|Z@{"�@yX@vv�@s�F@rM�@n��@l�/@k�@i��@g��@f��@d1@cS�@aX@_�w@^ff@\I�@YX@Xb@V$�@U/@T��@TZ@R�H@Q��@OK�@M�@K�m@K@J-@I�@HA�@F��@Ep�@C�
@A��@@b@=�T@=@<Z@;t�@:=q@97L@8��@7�;@7�w@6��@5@4j@3�F@2^5@1�^@01'@.��@.5?@-�-@,��@,Z@+@*-@)x�@)%@'�;@'+@&�R@%@$��@#�m@#�F@"M�@!�^@ �u@�@v�@�@��@��@@^5@�#@hs@��@�@�w@�P@��@��@z�@S�@�\@Ĝ@1'@�R@�@��@ƨ@dZ@
��@
=q@	hs@	hs@�@�@V@{@`B@V@I�@��@C�@n�@�@��@ �`@  �?��?�{?�/?��D?���?�~�?���?���?���?��?�/?�ƨ?�^5?�r�?�ff?�S�?�33?���?�G�?���?�{?��?��m?�"�?�7L?�1'?׮?�$�?ա�?��?�M�?�&�?�bN?��;?θR?Ͳ-?�p�?��?�I�?�I�?��m?�ƨ?�C�?���?�7L?ȓu?��?��?�`B?�?�t�?�Ĝ?�v�?�V?��?�I�?���?�"�?�?��?�^5?�^5?��^?�X?���?�7L?�X?�X?��^?�=q?���?�C�?�ƨ?��D?�/?��h?�5??��?���?�A�A�jA�dZA�^5A�bNA�bNA�dZA�dZA�dZA�\)A�dZA�ffA�ffA�ffA�ffA�hsA�jA�jA�l�A�l�A�n�A�n�A�p�A�l�A�l�A�jA�jA�hsA�jA�l�A�l�A�p�A�r�A�l�A�ffA�bNA�\)A�O�A�K�A�A�A�=qA�=qA�=qA�=qA�=qA�=qA�?}A�O�A�S�A�XA�VA�I�A�?}A�?}A�9XA�33A�/A�/A�/A�1'A�33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
R�B
P�B
O�B
P�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
_;B
�=B
��BI�Bt�B�B�dB��B$�B1'B5?B6FB?}BG�BK�BM�BO�BS�BXB^5B]/BdZBhsBjBiyBm�Bp�Bt�Bt�Bs�Bs�B{�B� B�B�+B�DB�\B�VB��B�oB�JB�1B�B�B�B� Bp�Bm�BjBiyBdZB`BB_;BZBD�B<jB33B'�B�B1B��B��B�B�B�B�mB�ZB�5BB�B��B�{Bu�BZBE�B0!B �B
=B
��B
��B
�B
�;B
��B
�RB
��B
��B
��B
�B
t�B
jB
S�B
L�B
A�B
=qB
 �B
{B
PB
B	��B	��B	�B	�B	�mB	�)B	��B	��B	��B	ɺB	ǮB	�}B	�RB	�B	��B	�bB	�1B	� B	u�B	p�B	m�B	cTB	aHB	[#B	Q�B	J�B	B�B	;dB	.B	�B	�B	�B	�B	{B	oB	bB	\B	B	  B��B�sB�TB�HB�#B�B��B��B��BɺBǮBǮBƨBŢBÖBÖB��B�jB�^B�LB�3B�B��B��B��B��B��B��B��B��B�hB�bB�DB�DB�+B�+B�B}�Bz�Bx�Bu�Bs�Bq�BhsBdZBcTB_;B^5B\)B[#BXBS�BJ�BF�BC�B@�B9XB9XB7LB5?B6FB9XB49B>wBA�B@�BA�B@�B<jBC�BK�BL�BI�BG�BL�BJ�BN�BS�BZBP�BM�BK�BJ�B\)BbNBffBjBjBjBm�B�uB��B��B��B�-B�'B�'BBǮB��B�
B�BB��B	JB	�B	.B	.B	>wB	G�B	L�B	N�B	N�B	^5B	dZB	o�B	s�B	�+B	�JB	��B	��B	��B	��B	�B	�3B	�dB	��B	ŢB	ǮB	�B	�#B	�NB	�fB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
1B
	7B
JB
uB
{B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
%�B
'�B
(�B
,B
-B
-B
.B
0!B
2-B
5?B
5?B
7LB
8RB
8RB
:^B
:^B
<jB
=qB
?}B
A�B
B�B
D�B
D�B
D�B
E�B
F�B
H�B
G�B
I�B
I�B
I�B
K�B
L�B
M�B
N�B
N�B
P�B
Q�B
Q�B
R�B
T�B
S�B
VB
VB
W
B
XB
YB
ZB
ZB
\)B
[#B
^5B
]/B
_;B
_;B
`BB
bNB
bNB
cTB
dZB
e`B
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
jB
k�B
k�B
l�B
k�B
m�B
l�B
n�B
p�B
o�B
q�B
p�B
q�B
r�B
t�B
t�B
w�B
w�B
x�B
x�B
z�B
z�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�1B
�1B
�=B
�JB
�VB
�\B
�bB
�hB
�oB
�uB
�uB
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
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�?B
�?B
�FB
�LB
�LB
�LB
�RB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�jB
�jB
�dB
�jB
Q�B
P�B
R�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
P�B
Q�B
P�B
Q�B
P�B
P�B
O�B
P�B
O�B
O�B
O�B
N�B
O�B
P�B
P�B
O�B
O�B
P�B
O�B
P�B
P�B
N�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
O�B
O�B
N�B
O�B
O�B
N�B
O�B
N�B
O�B
P�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
N�B
N�B
N�B
N�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          B
P�B
N�B
M�B
N�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
]/B
�1B
��BG�Br�B��B�XB��B"�B/B33B49B=qBE�BI�BK�BM�BQ�BVB\)B[#BbNBffBhsBgmBk�Bn�Br�Br�Bq�Bq�By�B}�B�B�B�7B�PB�JB�uB�bB�=B�%B�B�B� B}�Bn�Bk�BhsBgmBbNB^5B]/BXBB�B:^B1'B%�BuB%B��B�B�B�B�yB�`B�NB�)B��B��B��B�oBs�BXBC�B.B�B1B
��B
��B
�sB
�/B
��B
�FB
��B
��B
�uB
�B
r�B
hsB
Q�B
J�B
?}B
;dB
�B
oB
DB
B	��B	�B	�B	�B	�`B	�B	��B	��B	��B	ǮB	ŢB	�qB	�FB	�B	��B	�VB	�%B	}�B	s�B	n�B	k�B	aHB	_;B	YB	O�B	H�B	@�B	9XB	,B	�B	�B	�B	uB	oB	bB	VB	PB	B��B�B�fB�HB�;B�B�B��B��B��BǮBŢBŢBĜBÖB��B��B�}B�^B�RB�?B�'B�B��B��B��B��B��B��B��B�uB�\B�VB�7B�7B�B�B�B{�Bx�Bv�Bs�Bq�Bo�BffBbNBaHB]/B\)BZBYBVBQ�BH�BD�BA�B>wB7LB7LB5?B33B49B7LB2-B<jB?}B>wB?}B>wB:^BA�BI�BJ�BG�BE�BJ�BH�BL�BQ�BXBN�BK�BI�BH�BZB`BBdZBhsBhsBhsBk�B�hB�{B��B��B�'B�!B�!B��BƨBɺB�B�;B��B	DB	�B	-B	-B	=qB	F�B	K�B	M�B	M�B	]/B	cTB	n�B	r�B	�%B	�DB	��B	��B	��B	��B	�B	�-B	�^B	��B	ĜB	ƨB	�
B	�B	�HB	�`B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
B
+B
1B
DB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
!�B
$�B
&�B
'�B
+B
,B
,B
-B
/B
1'B
49B
49B
6FB
7LB
7LB
9XB
9XB
;dB
<jB
>wB
@�B
A�B
C�B
C�B
C�B
D�B
E�B
G�B
G�B
I�B
I�B
I�B
K�B
L�B
M�B
N�B
N�B
P�B
Q�B
Q�B
R�B
T�B
S�B
VB
VB
W
B
XB
YB
ZB
ZB
\)B
[#B
^5B
]/B
_;B
_;B
`BB
bNB
bNB
cTB
dZB
e`B
dZB
e`B
e`B
e`B
ffB
gmB
gmB
gmB
jB
k�B
k�B
l�B
k�B
m�B
l�B
n�B
p�B
o�B
q�B
p�B
q�B
r�B
t�B
t�B
w�B
w�B
x�B
x�B
z�B
z�B
|�B
|�B
}�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�1B
�1B
�=B
�JB
�VB
�\B
�bB
�oB
�uB
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
�B
�B
�B
�B
�B
�-B
�3B
�9B
�9B
�?B
�LB
�LB
�RB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�jB
�qB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�}B
�}B
�wB
�}B
O�B
N�B
P�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
N�B
O�B
N�B
N�B
M�B
N�B
M�B
M�B
M�B
L�B
M�B
N�B
N�B
M�B
M�B
N�B
M�B
N�B
N�B
L�B
L�B
M�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
M�B
M�B
L�B
M�B
L�B
M�B
N�B
L�B
M�B
M�B
L�B
L�B
L�B
M�B
L�B
L�B
L�B
L�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313462021061413523420210614135234202106141746402021061417464020210614174640201809172313462021061413523420210614135234202106141746402021061417464020210614174640PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134620180917231346  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134620180917231346QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                