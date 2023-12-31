CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:47Z creation      
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
resolution        =���   axis      Z        0  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     0  dL   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  t|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ѐ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     0  ՜   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   <   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar            HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       (   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 
tArgo profile    3.1 1.2 19500101000000  20180917231347  20210617131503  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               !   !DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�~�g��p@�~�g��p11  @�~�`��@�~�`��@6֥�L�X@6֥�L�X�c�jUp�c�jUp11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@Fff@�  @���@�  @�  A��A33A&ffAC33Aa��A~ffA�33A�33A���A���A�  AᙚA�B   B  B��B  B ffB(��B0  B8ffBA33BHffBO��BX  B`ffBg��BpffBxffB��B�  B�  B�33B�33B�  B���B�  B�ffB�33B�33B�ffB�  B�  B�  B�  B�33B�  BǙ�B�  B�  B�33B�33B�33B�33B���B�ffB�  B���B�33B���B���C   C33C  C��C�fC
�CL�C33C�fC  C33CL�C�C��C��C��C �C"�C$33C&L�C(  C)�3C+��C-�fC0  C2�C433C6L�C8�C9�fC<�C>L�C@33CB�CD  CF  CH33CJ33CL33CN33CP�CR�CT�CV�CX  CZ  C[�fC^33C`�Cb  Cc�fCe�fCh33Cj33Cl�Cn  Co�fCr33Ct�Cv�CxL�Cz33C|  C~33C�  C��fC��3C��3C��C��C��C��C��fC��3C��C��C��C��fC��3C��3C�  C�  C�  C��C��C��C�&fC��C��3C��3C�  C��C��C��C�&fC��C��3C��C��C�&fC�33C��C�ٚC��fC��3C�  C��C�  C��fC�  C��C��C��C��3C�  C��C�  C��3C��C��C��C��3C��C��C�&fC��C��fC��3C�  C��C��C��C��C��C�  C��fC�  C��C��C�&fC��C��3C�  C��C��C��C��C��fC�  C��C�  C��fC��3C��3C�  C��3C�  C��C��C��C��C��C�&fC��C��fC��fC��fC��fC��3C�  C�  C�  C��C��C�&fC�&fC��C��fC��3C�  C�  C�  C��C�  C��C�&fC�  D&fD��D�D	��D�D��DS3D�3D��DFfD��D�fD!S3D$fD&�fD)33D+��D.,�D0� D3fD5l�D7�fD:,�D<��D>�3DA` DC�fDF` DH��DKl�DM��DPffDR� DUffDW��DZs3D\�fD_S3Da��Dd@ Df��Di  Dk�fDm�3DpFfDr�3Du3DwY�Dy� D{��D}��D��D�9�D�Y�D�|�D�� D�ɚD�� D�fD�FfD�y�D�� D���D�)�D�\�D���D���D�fD�L�D��3D��fD��D��D�VfD���D�� D�� D�  D�P D�|�D�� D�ɚD�� D�fD�<�D�ffD���D���D��fD�fD�<�D�` D���D���D���D��fD�  D�FfD�p D�� D���D���D���D�#3D�FfD�l�D��3D���D�ɚD��fD���D�  D�@ D�Y�D�|�Dʙ�D˼�D�ٚD���D��D�9�D�VfD�s3DӆfDԣ3Dռ�D�� D���D�fD��D�)�D�33D�9�D�@ D�C3D�FfD�FfD�I�D�P D�S3D�P D�L�D�P D�VfD�Y�D�Y�D�\�D�` D�\�D�` D�i�D�p D�|�D�fD��D��3D��3D���D�� D�ɚD��fD��fD��fD���D�� D�� D��3E ~fE�E�fE3E�fE�E� E� E��E�E	� E
�fE3E� El�Ec3E� EA�E6fE��E��E+3E)�E�3E�3EI�EVfE�fE� E ��E"X E#�fE$�3E&&fE'��E(y�E)��E+)�E,y�E-\�E.�3E/�fE1)�E2i�E3��E4��E6#3E7X E8� E9�fE;0 E<y�E?y�EB� EE�3EI( EL�EOFfERh EU��EX��E[�fE^�fEa�fEe3EhfEki�EnK3EqnfEt� Ew��E{  E~)�E���E��E���E�H�E���E�)�E�m�E���E�3E�T E�� E�� E�W3E���E���E�@ E�� E��3E�(�E���E���E�( >���>���>���>L��>���>���>���>���?   >L��>���>L��>���>���>���>���>���>���>���>���>���>���>���>���>���?��?L��?�  ?���?�33?ٙ�?�33@��@   @333@@  @Y��@l��@�  @���@�33@���@���@�33@�  @ə�@�ff@�  @陚@���A��A  A  AffA��A#33A(  A0  A4��A<��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144414141144444441444111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ?L��?�  @&ff@fff@�  @���@�  @�  A	��A33A.ffAK33Ai��A�33A�33A�33A���A���A�  A噚A���B  B
  B��B  B"ffB*��B2  B:ffBC33BJffBQ��BZ  BbffBi��BrffBzffB���B�  B�  B�33B�33B�  B���B�  B�ffB�33B�33B�ffB�  B�  B�  B�  B�33B�  Bș�B�  B�  B�33B�33B�33B�33B���B�ffB�  B���B�33B���B���C � C�3C� CL�CffC
��C��C�3CffC� C�3C��C��CL�CL�CL�C ��C"��C$�3C&��C(� C*33C,L�C.ffC0� C2��C4�3C6��C8��C:ffC<��C>��C@�3CB��CD� CF� CH�3CJ�3CL�3CN�3CP��CR��CT��CV��CX� CZ� C\ffC^�3C`��Cb� CdffCfffCh�3Cj�3Cl��Cn� CpffCr�3Ct��Cv��Cx��Cz�3C|� C~�3C�@ C�&fC�33C�33C�L�C�Y�C�Y�C�L�C�&fC�33C�L�C�L�C�L�C�&fC�33C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�ffC�L�C�33C�33C�@ C�L�C�Y�C�Y�C�ffC�Y�C�33C�L�C�Y�C�ffC�s3C�L�C��C�&fC�33C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�33C�@ C�Y�C�@ C�33C�L�C�Y�C�L�C�33C�L�C�L�C�ffC�L�C�&fC�33C�@ C�L�C�L�C�L�C�Y�C�Y�C�@ C�&fC�@ C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�Y�C�L�C�&fC�@ C�L�C�@ C�&fC�33C�33C�@ C�33C�@ C�L�C�L�C�L�C�L�C�Y�C�ffC�L�C�&fC�&fC�&fC�&fC�33C�@ C�@ C�@ C�L�C�Y�C�ffC�ffC�L�C�&fC�33C�@ C�@ C�@ C�L�C�@ C�Y�C�ffC�@ DFfD��D,�D	��D9�DٚDs3D3D��DffD�D�fD!s3D$&fD&�fD)S3D+��D.L�D0� D3&fD5��D7�fD:L�D<��D?3DA� DDfDF� DI�DK��DN�DP�fDS  DU�fDX�DZ�3D]fD_s3Da��Dd` DfٚDi@ Dk�fDn3DpffDr�3Du33Dwy�Dy� D{��D~�D�)�D�I�D�i�D���D�� D�ٚD�  D�&fD�VfD���D�� D���D�9�D�l�D���D���D�&fD�\�D��3D��fD���D�,�D�ffD���D�� D�  D�0 D�` D���D�� D�ٚD�  D�&fD�L�D�vfD���D�ɚD��fD�&fD�L�D�p D���D���D���D�fD�0 D�VfD�� D�� D�ɚD���D��D�33D�VfD�|�D��3D���D�ٚD��fD�	�D�0 D�P D�i�DɌ�Dʩ�D���D��D�	�D�)�D�I�D�ffD҃3DӖfDԳ3D���D�� D���D�fD�)�D�9�D�C3D�I�D�P D�S3D�VfD�VfD�Y�D�` D�c3D�` D�\�D�` D�ffD�i�D�i�D�l�D�p D�l�D�p D�y�D�� D��D�fD��D��3D��3D���D�� D�ٚD��fD��fD��fD���D�� D�  E �E �fE	�E�fE3E�fE�E� E� E��E$�E	� E
�fE3E  Et�Ek3E� EI�E>fE��E��E33E1�E�3E�3EQ�E^fE�fE� E ��E"` E#�fE$�3E&.fE'��E(��E)��E+1�E,��E-d�E.�3E/�fE11�E2q�E3��E4��E6+3E7` E8� E9�fE;8 E<��E?��EB� EE�3EI0 EL�EONfERp EU��EX��E[�fE^�fEa�fEe3Eh&fEkq�EnS3EqvfEt� Ew��E{ E~1�E���E��E���E�L�E���E�-�E�q�E���E�3E�X E�� E�� E�[3E���E��E�D E�� E��3E�,�E���E���E�, G�O�?L��G�O�?333G�O�G�O�G�O�?L��G�O�?333G�O�?333?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?fff?���?�ff?�  ?ٙ�?�33@��@��@,��@@  @S33@`  @y��@�ff@�  @���@�33@���@���@�33@�  @ٙ�@�ff@�  @���A��A	��A  A  AffA$��A+33A0  A8  A<��AD��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144414141144444441444111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          @ @ �@ V@ *@ O@ !s@ (�@ /�@ 7L@ >�@ F�@ SI@ `B@ l�@ z3@ ��@ ��@ ��@ �~@ ��@ �*@ �t@ �@ � @j@�@ @,`@:�@I�@V@bN@p�@~�@�D@�H@��@�9@@�7@ލ@�4@�,@%@{@#�@0x@>@Lu@X�@ff@t@��@�@�@�M@�R@��@�O@��@�@�E@	�@B@%�@2�@A�@M�@Z�@i�@x�@�@�h@��@��@��@��@�[@�@�@@V@�@'�@5?@D�@R�@`�@oF@z�@�|@��@�(@�~@��@�*@܀@��@�@@@ @-@:@G�@V�@dZ@r@�@��@�H@��@��@@�7@�/@��@��@�@�@!s@1'@>�@K�@X�@e�@uk@�d@�@�@�@�R@�W@Ӡ@��@�@@��@
�@B@&�@3�@?}@M�@\�@j@x&@��@�@��@�@��@�c@׹@�`@�@@V@�@(G@6�@D�@SI@`�@oF@|?@��@��@�5@��@��@�|@�h@�@�@	j@	o@	�@	+@	:@	H]@	V�@	c�@	o�@	~K@	�P@	��@	��@	��@	��@	��@	�/@	�4@	��@
�@
*@
 �@
/@
=q@
K�@
Yn@
g@
uk@
�@
�\@
��@
��@
�@
�W@
խ@
��@
�@@
��@
�@B@&�@3�@?}@N�@\�@i�@v@�p@�@�m@�f@��@�@׹@�`@�@^@�@�@'�@5?@B�@P�@^�@m:@z�@��@��@�5@��@�2@�|@�@�m@��@j@@g@,`@;d@I�@UU@��@#�@g�@�@�@9X@�W@�@b@Yn@�(@�4@5?@~�@ƨ@J@O�@�$@�
@�@Z@�H@��@�@^5@�m@�`@(�@n�@��@� @:�@~K@�>@1@M$@�@�C@�@X�@�U@��@g@a�@�@�T@$.@bN@�m@��@�@S�@�i@��@�@K@��@�c@1@I@��@��@b@S�@��@�@
@`�@�(@�`@ &�@ hs@ ��@ ��@!/@!p�@!�~@!�Y@"33@"s_@"�~@"��@#/�@#n�@#�f@#��@$+�@$k�@$�@$��@%+�@%i�@%�M@%�m@&$�@&dZ@&��@&�@'""@'_�@'�@'�/@(�@(Yn@(��@(�\@)*@)Q=@)��@)��@*v@*DD@*��@*��@*��@+8�@+v�@+��@+��@,.l@,k�@,��@,�`@- @-\�@-��@-Ӡ@.b@.Lu@.�+@.�2@.��@/1�@/i�@/�@/�h@0@0FQ@0~K@0��@0�@1!s@1X�@1��@1�@1��@25�@2m:@2�(@2�t@3@3K@3�p@3�@3�@4-@4g@4��@4�t@5@5Lu@5�@5��@5�@6(�@6b�@6�H@6��@7
�@7C�@7|?@7�9@7�4@8�0@9j@9qS@:@:��@;!s@;�@<%�@<Ĝ@=-�@=�c@>g@>ψ@?m�@?�#@@�@@�4@A�#@B�@B��@C!s@Cȴ@D6�@D�z@EDD@E�@FLu@F�@G|�@G��@Hv@I�@I��@I��@J�@K�@K��@Lg@L�M@M.l@M�F@N:@N@OM�@O��@Pj@Q�-@S(�@Tg�@UӠ@W�@Xo�@Y��@[�@\a�@]ȴ@_�@`^�@a�F@c	�@dqS@e�@g�@hm:@i��@k�@lqS@m�Z@o^@pV@q�k@s�@sV�@s��@s�`@t	@tUU@t�A@t��@u2�@uk.@u��@u�,@vN�@v��@v��@w*@wK�@w��G�O�@ G�O�@ ^G�O�G�O�G�O�@ G�O�@ ^G�O�@ ^@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ �@ @ v@ �@ 1@ 	�@ �@ �@ @ @ @ {@ 6@ B@ O@ 
@ g@ !s@ $.@ &;@ (�@ +@ -�@ /�@ 1�@ 5?@ 7L@ :@ =q@ @,@ B�@ E�@ G�@ K@ M$@ P�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aѧ�Aѧ�Aѩ�Aѧ�Aѧ�Aѧ�Aѧ�AѬAѲ-AѴ9Aѕ�Aѡ�Aѥ�Aѡ�Aџ�Aѣ�Aѣ�Aѣ�A�-A��A��A��TA��/A��#A��A��
A���A���A���A�ȴAоwAв-AЗ�AЗ�A�/A��mA��;A�?}Aė�A��#A�ƨA�ZA��A�K�A�bA�v�A���A��yA��DA�jA���A��A���A�O�A�JA�`BA��A���A� �A��\A�v�A���A��A��uA�E�A�;dA�S�A�oA��mA�A�A�=qA�r�A�ƨA�M�A��HA�{A���A�x�A���A�VA�n�A��A�z�A���A�A�A��#A�dZA��jA��uA��9A��A�\)A�x�A�`BA�O�A���A� �A���A�/A��\A��;A�t�A��A��DA�A��A��A�  A�Q�A��wA�1A�Q�A�jA�^AoA~ZA}"�Az-Ax�DAv�yAt��Ar��Aq�wAq�Ap$�An��AnI�Ak��Ai�AfVAe�PAd  Aa�-A`Q�A_+A^bA\�A[��AY/AV�jAVE�AU�AU��AV�AU�AU�AU;dAT�AT�AS?}AQ��AP�AO��AO+AN{AK�AJ�AI�AI�AG��AF�uAEx�ADQ�AC+AA�;A@��A>�A>(�A<�A:z�A9��A9XA8��A6z�A4��A3�A1;dA0v�A.��A.z�A,�`A,(�A+�wA+XA*�/A*ȴA*�9A*A(��A'�#A'"�A&��A&�A$�9A#��A#�A!�A!K�A!O�A!A $�A+A~�Al�A��A
=A1A�AAȴAA�Al�AjA?}Ar�A|�AA�A;dA-A|�A�+AG�A
��A	��A�jA$�At�AVA�9A(�A�PA(�A��AjAƨA -@�ƨ@���@�Z@��@��/@��m@��y@�?}@�@�V@�V@��@�K�@�9X@�33@��#@��@���@�7L@�C�@Ӿw@�-@� �@�1'@�z�@��m@�Z@��j@�?}@�  @��@��7@���@�~�@��P@�G�@�j@�r�@�%@��-@��#@��-@���@�V@�C�@���@��h@��@�z�@��@�^5@���@�X@���@���@���@���@��w@��h@���@���@��m@�\)@�{@�hs@��@���@� �@�
=@��\@��-@�r�@~@|�@|Z@z=q@xA�@v�+@r��@q�@o�@o��@lI�@jn�@g�;@fV@d�j@ct�@a�^@`bN@^E�@\�/@\1@Z=q@Y��@Xb@W�@Up�@T(�@R=q@PA�@N��@N@M/@J�!@J��@I��@HĜ@G�;@F��@E�@D1@B�\@A��@AX@?��@>�y@>$�@=�T@=p�@;��@;33@:M�@9hs@8�@8  @7�@6V@5`B@4�@4z�@3t�@2��@2n�@1�7@0��@0  @.��@-�-@,��@*�\@)��@)7L@(A�@&��@%�@#�m@#@!�#@ Ĝ@ Q�@��@��@�@`B@�D@dZ@�@=q@&�@��@��@�P@�T@?}@�j@�m@C�@��@J@��@X@�`@��@1'@�w@��@ff@�h@�@1@t�@
��@
�\@	�^@	G�@��@bN@�@\)@;d@��@?}@�
@@G�?�;d?�5??�/?�dZ?��?�?�S�?��?�|�?�D?�?���?�+?�`B?���?�9X?��?���?߾w?�{?�V?�?��?�r�?�+?�$�?�?��?ӕ�?�33?щ7?�  ?�;d?��?θR?��?͑h?�(�?�ƨ?�?�^5?�x�?ȓu?�E�?��/?���?�o?�J?��`?�|�?�\)?�V?��?�O�?��D?�I�?�C�?�~�?��^?�X?�X?��#?��?�~�?�"�?���?�I�?���?��?�V?�O�?��h?��h?���?���?�{?�{?�V?�v�?��R?��?���?��?�\)?�\)AѬAѬAѬAѩ�Aѩ�AѬAѬAѬAѩ�Aѩ�Aѧ�Aѩ�AѬAѩ�AѬAѧ�Aѧ�Aѣ�Aѧ�Aѣ�Aѣ�Aѡ�Aџ�Aџ�Aљ�Aѩ�Aѩ�Aѧ�Aѣ�Aѣ�Aѧ�Aѧ�Aѩ�Aѩ�Aѩ�Aѧ�Aѧ�Aѧ�Aѩ�Aѥ�Aѧ�Aѥ�Aѧ�Aѧ�Aѧ�Aѥ�Aѧ�Aѩ�AѴ9AѲ-AѰ!AѴ9AѼjAѬAљ�Aљ�Aѕ�AэPAэPAѡ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Aѧ�Aѧ�Aѩ�Aѧ�Aѧ�Aѧ�Aѧ�AѬAѲ-AѴ9Aѕ�Aѡ�Aѥ�Aѡ�Aџ�Aѣ�Aѣ�Aѣ�A�-A��A��A��TA��/A��#A��A��
A���A���A���A�ȴAоwAв-AЗ�AЗ�A�/A��mA��;A�?}Aė�A��#A�ƨA�ZA��A�K�A�bA�v�A���A��yA��DA�jA���A��A���A�O�A�JA�`BA��A���A� �A��\A�v�A���A��A��uA�E�A�;dA�S�A�oA��mA�A�A�=qA�r�A�ƨA�M�A��HA�{A���A�x�A���A�VA�n�A��A�z�A���A�A�A��#A�dZA��jA��uA��9A��A�\)A�x�A�`BA�O�A���A� �A���A�/A��\A��;A�t�A��A��DA�A��A��A�  A�Q�A��wA�1A�Q�A�jA�^AoA~ZA}"�Az-Ax�DAv�yAt��Ar��Aq�wAq�Ap$�An��AnI�Ak��Ai�AfVAe�PAd  Aa�-A`Q�A_+A^bA\�A[��AY/AV�jAVE�AU�AU��AV�AU�AU�AU;dAT�AT�AS?}AQ��AP�AO��AO+AN{AK�AJ�AI�AI�AG��AF�uAEx�ADQ�AC+AA�;A@��A>�A>(�A<�A:z�A9��A9XA8��A6z�A4��A3�A1;dA0v�A.��A.z�A,�`A,(�A+�wA+XA*�/A*ȴA*�9A*A(��A'�#A'"�A&��A&�A$�9A#��A#�A!�A!K�A!O�A!A $�A+A~�Al�A��A
=A1A�AAȴAA�Al�AjA?}Ar�A|�AA�A;dA-A|�A�+AG�A
��A	��A�jA$�At�AVA�9A(�A�PA(�A��AjAƨA -@�ƨ@���@�Z@��@��/@��m@��y@�?}@�@�V@�V@��@�K�@�9X@�33@��#@��@���@�7L@�C�@Ӿw@�-@� �@�1'@�z�@��m@�Z@��j@�?}@�  @��@��7@���@�~�@��P@�G�@�j@�r�@�%@��-@��#@��-@���@�V@�C�@���@��h@��@�z�@��@�^5@���@�X@���@���@���@���@��w@��h@���@���@��m@�\)@�{@�hs@��@���@� �@�
=@��\@��-@�r�@~@|�@|Z@z=q@xA�@v�+@r��@q�@o�@o��@lI�@jn�@g�;@fV@d�j@ct�@a�^@`bN@^E�@\�/@\1@Z=q@Y��@Xb@W�@Up�@T(�@R=q@PA�@N��@N@M/@J�!@J��@I��@HĜ@G�;@F��@E�@D1@B�\@A��@AX@?��@>�y@>$�@=�T@=p�@;��@;33@:M�@9hs@8�@8  @7�@6V@5`B@4�@4z�@3t�@2��@2n�@1�7@0��@0  @.��@-�-@,��@*�\@)��@)7L@(A�@&��@%�@#�m@#@!�#@ Ĝ@ Q�@��@��@�@`B@�D@dZ@�@=q@&�@��@��@�P@�T@?}@�j@�m@C�@��@J@��@X@�`@��@1'@�w@��@ff@�h@�@1@t�@
��@
�\@	�^@	G�@��@bN@�@\)@;d@��@?}@�
@@G�?�;d?�5??�/?�dZ?��?�?�S�?��?�|�?�D?�?���?�+?�`B?���?�9X?��?���?߾w?�{?�V?�?��?�r�?�+?�$�?�?��?ӕ�?�33?щ7?�  ?�;d?��?θR?��?͑h?�(�?�ƨ?�?�^5?�x�?ȓu?�E�?��/?���?�o?�J?��`?�|�?�\)?�V?��?�O�?��D?�I�?�C�?�~�?��^?�X?�X?��#?��?�~�?�"�?���?�I�?���?��?�V?�O�?��h?��h?���?���?�{?�{?�V?�v�?��R?��?���?��?�\)?�\)AѬAѬAѬAѩ�Aѩ�AѬAѬAѬAѩ�Aѩ�Aѧ�Aѩ�AѬAѩ�AѬAѧ�Aѧ�Aѣ�Aѧ�Aѣ�Aѣ�Aѡ�Aџ�Aџ�Aљ�Aѩ�Aѩ�Aѧ�Aѣ�Aѣ�Aѧ�Aѧ�Aѩ�Aѩ�Aѩ�Aѧ�Aѧ�Aѧ�Aѩ�Aѥ�Aѧ�Aѥ�Aѧ�Aѧ�Aѧ�Aѥ�Aѧ�Aѩ�AѴ9AѲ-AѰ!AѴ9AѼjAѬAљ�Aљ�Aѕ�AэPAэPAѡ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B� B}�B�B� B� B~�B� B~�B~�Bs�Br�Bo�Bo�Bn�Bn�Bn�Bn�Bo�Bn�Bn�Bn�Bm�Bm�Bo�Bs�Bv�Bw�BdZBgmB�=B�\B�DB�{B�oB��B��B��B��B�LB��B��B��B�BĜB�dB�3B�B��B��B��B��B��B��B��B��B�7B�=B�DB�DB�7B~�Bl�B[#BVBO�BF�B>wB<jB+B&�B�B�B{BVB��B�B�B�sB�HB��B�jB�FB�!B��Bp�B[#BO�BG�B7LB/B�B
��B
�TB
�B
�B
��B
�jB
�'B
��B
��B
��B
�PB
�=B
�\B
�JB
�B
~�B
q�B
Q�B
D�B
49B
+B
�B
uB
\B
B	��B	��B	�fB	�
B	�jB	�?B	��B	�=B	~�B	x�B	p�B	gmB	`BB	W
B	L�B	P�B	N�B	_;B	k�B	p�B	p�B	p�B	o�B	p�B	n�B	hsB	aHB	]/B	ZB	R�B	H�B	B�B	=qB	:^B	0!B	,B	%�B	 �B	�B	�B	VB	DB	+B��B��B��B��B�B�fB�TB�
B��B��BȴBĜB�wB�jB�^B�RB�LB�LB�FB�!B�B��B��B��B��B��B�uB�bB�PB�JB�7B�1B�B� B|�B{�Bw�Bt�Bp�Bm�Bl�BjBhsBffBdZBbNB^5BW
BW
BW
B\)BZBXBW
BVBVBO�BO�BM�BK�BJ�BI�BH�BE�BC�BB�BA�B=qB33B8RB9XB6FB33B2-B2-B1'B33B49B5?B49B6FB6FB9XB9XB7LB7LB33B49B?}BD�BE�BH�BM�BS�B[#Be`Bt�B}�B�VB��B�B�XB��B��B	PB	hB	DB	oB	$�B	>wB	R�B	bNB	jB	q�B	y�B	�B	�uB	��B	��B	�-B	�9B	�FB	�dB	��B	ȴB	��B	��B	��B	�B	�)B	�BB	�ZB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
+B
	7B
1B
%B
	7B
VB
oB
�B
uB
�B
�B
�B
 �B
"�B
#�B
%�B
&�B
(�B
+B
+B
-B
.B
/B
1'B
33B
49B
5?B
5?B
6FB
8RB
7LB
8RB
8RB
9XB
:^B
<jB
<jB
>wB
?}B
>wB
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
E�B
F�B
F�B
H�B
I�B
I�B
J�B
K�B
L�B
M�B
M�B
N�B
O�B
P�B
P�B
Q�B
R�B
T�B
VB
W
B
XB
ZB
[#B
\)B
]/B
^5B
_;B
_;B
`BB
`BB
bNB
aHB
bNB
dZB
dZB
dZB
e`B
ffB
hsB
gmB
iyB
jB
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
o�B
p�B
r�B
r�B
t�B
t�B
u�B
t�B
u�B
v�B
v�B
w�B
w�B
x�B
w�B
x�B
z�B
|�B
}�B
� B
�B
�B
�B
�B
�B
�+B
�1B
�=B
�DB
�JB
�JB
�PB
�\B
�bB
�\B
�bB
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
�B
�B
�B
�B
�B
�B
�!B
�-B
�-B
�3B
�9B
�9B
�FB
�FB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�RB
�XB
�RB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�RB
�XB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B�B�B�B�B�B�B�B�B}�B~�B~�B}�B|�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          B��B��B��B��B��B��B��B��B��B�B}�B��B�B�B~�B�B~�B~�Bs�Br�BoBo�BnzBn{Bn|Bn|Bo�Bn}Bn~BnBmxBmyBo�Bs�Bv�Bw�BdEBgXB�(B�GB�0B�gB�[B��B��B��B��B�;BʰB��B��B�BčB�VB�%B��B��B��B��B��B��B��B��B�~B�.B�4B�<B�<B�0B~�Bl�B[BU�BO�BF�B>sB<gB*�B&�B�B�BzBVB��B�B�B�uB�JB��B�mB�JB�%B��Bp�B[(BO�BG�B7SB/"B�B
��B
�\B
� B
�B
��B
�tB
�2B
��B
��B
��B
�]B
�JB
�jB
�XB
�!B
	B
q�B
Q�B
D�B
4JB
+B
�B
�B
oB
2B	�B	�B	�{B	�B	��B	�UB	��B	�TB	B	x�B	p�B	g�B	`[B	W$B	L�B	Q B	N�B	_WB	k�B	p�B	p�B	p�B	o�B	p�B	n�B	h�B	aiB	]PB	Z?B	SB	H�B	B�B	=�B	:�B	0EB	,-B	&B	 �B	�B	�B	}B	lB	SB�#B�B��B��B��B�B�B�6B�$B�B��B��B��B��B��B��B�|B�}B�wB�SB�:B�#B�
B��B��B��B��B��B��B��B�oB�iB�XB�9B}'B|!Bx	Bt�Bp�Bm�Bl�Bj�Bh�Bf�Bd�Bb�B^tBWJBWJBWKB\jBZ_BXRBWMBVGBVHBP#BP$BNBLBKBJBH�BE�BC�BB�BA�B=�B3}B8�B9�B6�B3B2zB2zB1uB3�B4�B5�B4�B6�B6�B9�B9�B7�B7�B3�B4�B?�BD�BFBIBN7BT_B[�Be�Bu,B~gB��B�B�~B��B�[B�lB	�B	�B	�B	 B	%pB	?B	S�B	b�B	kB	rLB	z�B	��B	� B	�HB	�oB	��B	��B	� B	�!B	�IB	�wB	ΙB	ѮB	ΞB	��B	��B	�B	�1B	�YB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B

4B
	1B
'B

<B
^B
zB
�B
�B
�B
�B
 �B
!�B
#�B
$�B
'B
(B
*!B
,0B
,3B
.BB
/JB
0TB
2cB
4rB
5zB
6�B
6�B
7�B
9�B
8�B
9�B
9�B
:�B
;�B
=�B
=�B
?�B
@�B
?�B
A�B
A�B
B�B
B�B
DB
EB
EB
FB
G#B
H,B
H.B
J=B
KFB
KHB
LRB
M[B
NcB
OlB
OoB
PwB
Q�B
R�B
R�B
S�B
T�B
V�B
W�B
X�B
Y�B
[�B
\�B
]�B
^�B
_�B
aB
aB
bB
bB
dB
cB
d#B
f1B
f4B
f6B
g>B
hGB
jVB
iSB
kaB
ljB
mrB
n{B
n}B
nB
o�B
o�B
p�B
p�B
p�B
q�B
p�B
q�B
r�B
t�B
t�B
v�B
v�B
w�B
v�B
w�B
x�B
x�B
y�B
y�B
z�B
y�B
{B
}B
(B
�3B
�FB
�YB
�^B
�eB
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
�
B
�B
�)B
�4B
�EB
�LB
�_B
�kB
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
�	B
�B
�!B
�(B
�CB
�`B
�tB
��B
��B
��B
��B
��B
��B
�B
�B
�.B
�DB
�SB
�pB
�~B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�1B
�-B
�0B
�4B
�0B
�9B
�6B
�?B
�CB
�KB
�IB
�KB
�OB
�RB
�TB
�XB
�UB
�^B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B}�B~�B~�B}�B|�B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313472021061413553920210614135539202106171312552021061713125520210617131255201809172313472021061413553920210614135539202106171312552021061713125520210617131255PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134720180917231347  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134720180917231347QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134720180917231347QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150320210617131503IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                