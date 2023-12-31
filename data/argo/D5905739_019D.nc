CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:51Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  L|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  aP   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  e|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 ,  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   <   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�           HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    (   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � lArgo profile    3.1 1.2 19500101000000  20180724220251  20210617131457  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�m�_��@�m�_��11  @�m�UU]�@�m�UU]�@6��}�9�@6��}�9��c���: �c���: 11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?fff@   @Fff@�  @�  @�33@�  A��A  A$��AC33Aa��A�  A�  A���A���A���A�33A�33A���B33BffB  B  B ��B(  B0  B8  B@  BHffBPffBXffB_��Bg��BpffBw��B�  B�33B�33B�ffB�33B�  B�  B�  B�ffB�33B�33B�ffB���B���B�  B�  B�33B�33BǙ�B�  BЙ�Bԙ�B�ffBܙ�B�ffB䙚B���B뙚B�ffB�33B�  B���B���C�fC�fC  C�C
  C�fC  C�fC  C  C33C33C�C�C  C   C!�fC$�C&�C(  C*33C,�C-�fC0�C2  C3��C633C8�C9��C;�fC>  C@�CBL�CD  CE��CG�fCJ  CL�CN33CPL�CR�CS��CU�fCX  CZ�C\33C^L�C`33Ca�fCd33Cf�Cg�fCj33Cl  Cm��Cp33Cr�Ct  CvL�Cx33Cz�C|�C~  C�fC�  C��3C��3C��3C��3C��C��C��C�  C��3C��C��C�  C�&fC��C��C��C�  C�  C��3C��C��C�  C��3C��fC��C��C��3C��C�  C��fC��C��C�  C��C��C�  C��C��C�  C��C��C�  C��C�  C��3C��C�&fC��C��3C��C�&fC��C��fC�  C��C�  C��fC�  C��C��C�  C��C�  C��fC�  C�&fC��C�  C�&fC��C�  C��C��C��C��fC�  C��C��C�&fC�  C��fC��3C��3C��C��C�&fC��C��fC��fC��3C��3C��3C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��fC��fC��fC��fC��fC�ٚC��C�&fC�&fC�&fC��C�&fC��C��C��3C��C�&fC�ٚC��fC��fD   D � D�D��D3DfD	� D  Dy�D��DffD��Dl�D  D� D L�D"�3D%��D(9�D*��D-�3D0ffD3  D5��D8@ D:� D=@ D?�3DB  DD�3DG  DIs3DK��DNY�DPٚDSFfDU�3DX,�DZ� D]�D_��Db  Dd�3Dg9�Di�fDlFfDn�3Dql�Ds�3Dvs3Dx��D{l�D}�fD��D�VfD���D���D�&fD�c3D���D���D�6fD�vfD���D�fD�L�D���D��fD�  D�6fD�l�D���D�� D���D�  D�@ D�VfD�l�D�� D��fD���D�� D��3D��3D��3D�fD�  D�6fD�FfD�Y�D�s3D�� D���D�� D��fD�� D�  D�3D�)�D�@ D�P D�` D�s3D���D��3D��3D�ɚD��3D��fD��D�)�D�<�D�\�DĀ Dœ3Dư D�� D���D�  D�I�D�vfD͠ D���D�  D�)�D�Y�DӀ DԳ3D��fD��D�P Dـ Dڰ D��fD� D�<�D�l�D���D��fD���D�fD�FfD�i�D��D��D���D��3D�3D��D�6fD�P D�c3D�vfD� D��D��D���D��3D�� D���D���D��fD��fD��3D��fD��3D��3E H E ��EFfE��EFfE�fEH E�3EQ�E� E�3E��E	�fE
�3E�3E[3Et�E�3E��EVfEnfE�3E( E9�ENfE� E��EVfEK3E�3E� E!3E"i�E#� E$�fE%� E'H E(��E)�3E+33E,|�E-�3E.�3E/�3E1FfE2� E4  E4�fE6H E7��E93E9�fE;T�E<��E?{3EB��EE��EIfEL EOK3ERvfEU�3EX�3E[��E^�Ea��EeFfEh!�EkX En�3Eq�3Et�fEw��Ez�fE~$�E��3E��E���E�M�E��3E�l E���E�@ E���E��E�A�E���E���E��E�w3E���E��E�l�E���E��E�ZfE��3E��E�H E���E��3E�;3E�x�E��3E�, >���=���>���>L��>���>���>���>���>���>���>���>���>L��>���>���>���?   ?   ?   ?��?   ?333?333?L��?L��?fff?�  ?���?���?�  ?ٙ�?�ff@ff@��@,��@Fff@`  @s33@�33@�33@�  @�  @���@�ff@�33@�  @�  @���AffA33A��A��A#33A,��A6ffA>ffAFffANffAT��A\��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441444144144141414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?L��?�33@   @fff@�  @�  @�33@�  A	��A  A,��AK33Ai��A�  A�  A���A���A���A�33A�33A���B33B
ffB  B  B"��B*  B2  B:  BB  BJffBRffBZffBa��Bi��BrffBy��B�  B�33B�33B�ffB�33B�  B�  B�  B�ffB�33B�33B�ffB���B���B�  B�  B�33B�33Bș�B�  Bљ�Bՙ�B�ffBݙ�B�ffB噚B���B왚B�ffB�33B�  B���C ffCffCffC� C��C
� CffC� CffC� C� C�3C�3C��C��C� C � C"ffC$��C&��C(� C*�3C,��C.ffC0��C2� C4L�C6�3C8��C:L�C<ffC>� C@��CB��CD� CFL�CHffCJ� CL��CN�3CP��CR��CTL�CVffCX� CZ��C\�3C^��C`�3CbffCd�3Cf��ChffCj�3Cl� CnL�Cp�3Cr��Ct� Cv��Cx�3Cz��C|��C~� C�33C�@ C�33C�33C�33C�33C�Y�C�Y�C�L�C�@ C�33C�Y�C�L�C�@ C�ffC�Y�C�L�C�L�C�@ C�@ C�33C�Y�C�L�C�@ C�33C�&fC�L�C�L�C�33C�L�C�@ C�&fC�L�C�L�C�@ C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�Y�C�Y�C�@ C�L�C�@ C�33C�Y�C�ffC�Y�C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�@ C�Y�C�Y�C�@ C�Y�C�@ C�&fC�@ C�ffC�L�C�@ C�ffC�Y�C�@ C�L�C�Y�C�L�C�&fC�@ C�L�C�Y�C�ffC�@ C�&fC�33C�33C�L�C�L�C�ffC�L�C�&fC�&fC�33C�33C�33C�L�C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�33C�&fC�&fC�&fC�&fC�&fC��C�L�C�ffC�ffC�ffC�Y�C�ffC�Y�C�Y�C�33C�Y�C�ffC��C�&fC�&fD   D � D,�D��D33D&fD	� D  D��D�D�fD�D��D  D� D l�D#3D%��D(Y�D+�D-�3D0�fD3@ D5ٚD8` D:� D=` D?�3DB@ DD�3DG  DI�3DL�DNy�DP��DSffDU�3DXL�DZ� D]9�D_��Db@ Dd�3DgY�Di�fDlffDn�3Dq��Dt3Dv�3Dy�D{��D}�fD��D�ffD���D���D�6fD�s3D���D���D�FfD��fD�ɚD�fD�\�D���D��fD� D�FfD�|�D���D�� D��D�0 D�P D�ffD�|�D�� D��fD���D�� D��3D��3D�3D�fD�0 D�FfD�VfD�i�D��3D�� D���D�� D��fD�  D� D�#3D�9�D�P D�` D�p D��3D���D��3D��3D�ٚD��3D�fD��D�9�D�L�D�l�DĐ Dţ3D�� D�� D�	�D�0 D�Y�D̆fDͰ D���D� D�9�D�i�DӐ D��3D��fD�)�D�` Dِ D�� D��fD�  D�L�D�|�Dਗ਼D��fD���D�&fD�VfD�y�D��D��D���D��3D�3D�,�D�FfD�` D�s3D�fD� D��D���D���D��3D�� D���D���D��fD��fD��3D��fD��3D��3E P E ��ENfE��ENfE�fEP E�3EY�E� E�3E��E	�fE
�3E�3Ec3E|�E�3E��E^fEvfE�3E0 EA�EVfE� E��E^fES3E�3E� E!3E"q�E#� E$�fE&  E'P E(��E)�3E+;3E,��E-�3E.�3E/�3E1NfE2� E4 E4�fE6P E7��E93E9�fE;\�E<��E?�3EB��EE��EIfEL EOS3ER~fEU�3EX�3E[��E^�Ea��EeNfEh)�Ek` En�3Eq�3Et�fEw��Ez�fE~,�E��3E�!�E���E�Q�E��3E�p E� �E�D E���E��E�E�E���E���E��E�{3E���E��E�p�E���E��E�^fE��3E���E�L E���E��3E�?3E�|�E��3E�0 G�O�?��G�O�?333G�O�G�O�G�O�G�O�?L��G�O�G�O�G�O�?333G�O�G�O�?L��G�O�G�O�?�  G�O�?�  G�O�?���G�O�?�ff?�33?�  ?���?ٙ�@   @��@33@&ff@9��@L��@fff@�  @���@�33@�33@�  @�  @���@�ff@�33@�  A   A��AffA33A��A$��A+33A4��A>ffAFffANffAVffA\��Ad��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414144441444144144141414111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           @ @ %@ �@ *@ O@ ""@ )�@ /�@ 7L@ =q@ FQ@ SI@ `B@ m:@ z�@ �7@ ��@ ��@ ��@ �w@ �|@ ܀@ ��@ ��@j@o@�@,`@:@G�@V@c�@qS@}�@�D@�H@��@��@�>@��@�;@�4@�,@�@{@#�@0x@>@Lu@Z�@hs@t@��@�@��@�M@�R@�@խ@�@��@��@J@6@$.@4�@A�@N�@[z@i!@v�@�p@��@�@�@�@�c@�[@�@�Y@^@@�@)�@6�@DD@Q=@`B@m�@z�@��@��@�(@�-@�&@�o@��@��@�e@�@@g@.l@:@FQ@T�@b�@qS@�@��@�H@��@�9@@��@�;@�@��@%@�@"�@/@>�@K@Wb@g�@t�@��@�h@�a@�Y@�@��@��@�H@�@@��@	�@6@&�@4�@A�@N�@[z@k.@x&@�@��@��@��@�k@�c@�
@�@�@ �@�@�@'�@7L@D�@Q=@`B@m:@y�@�7@��@��@��@��@��@��@�y@��@	�@	o@	�@	-@	:@	F�@	V�@	e	@	r@	}�@	��@	��@	��@	��@	@	є@	��@	�(@	�,@
1@
�@
""@
1'@
=q@
I�@
X�@
hs@
t�@
��@
�h@
�a@
��@
�@
�W@
�O@
��@
��@
�E@�@�@%�@1�@@,@M�@\�@j@y�@��@�h@�@�f@�@ȴ@׹@�@�Y@ �@�@O@(�@6�@DD@Q�@`B@m:@z�@��@��@�y@�!@��@�o@�h@��@��@v@@ @.l@;d@I@T�@dZ@r�@|?@��@�<@�A@��@��@є@��@g@��@��@2�@v�@�@��@B8@��@�7@B@a�@��@�@<�@�+@��@O@bN@�A@�@/�@r�@��@��@:@|�@��@�@F�@�7@�o@@Q�@��@�h@�@e	@��@�@3�@y�@��@v@I�@�P@є@
�@Q=@��@܀@!s@g�@�Y@��@5�@|?@��@v@Lu@�@�\@B@\)@�a@��@  �@ b�@ �(@ �H@!�@!Z@!��@!�7@"�@"FQ@"��@"�j@"�q@#0x@#k.@#�A@#�@$�@$Wb@$�u@$�7@%�@%G�@%�@%�&@%�,@&3�@&oF@&��@&�@'�@'Yn@'��@'��@(
�@(FQ@(�d@(�@(�~@)5@@)o�@)�f@)�@*&;@*b�@*�m@*��@+�@+^5@+�a@+��@,
@,_�@,�@,��@-�@-`A@-��@-�T@.%�@.ff@.�A@.�y@/(�@/i!@/��@/�(@0*S@0i!@0��@0�y@1'�@1e�@1�(@1��@2�@2Yn@2��@2є@3�@3H]@3�@3��@3�@4+�@4e	@4��@4�
@5�@5B�@5uk@5�@5��@6B@6O0@6��@6��@6��@7(G@7^5@7��@7�@8j@8;d@8t�@9�@9��@:@:�f@;#�@;��@<O�@<�@=B8@=�^@>o�@>�l@?]�@@@@��@@��@A��@B@B�9@C�@C��@D�@D�F@EK�@E��@F@,@F�7@G_�@G�@H|?@I�@I�<@J#�@J�@Ko@K�(@L6�@L��@M/@M��@NZ�@N�L@OT�@O�@P�d@Q��@SV@TZ�@U�@W�@Xr@Y�@[ @\k.@]��@_�@`hs@a��@c
�@di�@e��@g�@hX@i��@k�@loF@m��@o1@pg�@q��@s�@ti�@u��@u�,@vLv@v�@v��@w
�@wYn@w�P@w�/@xo@x`B@x��@x�@y-�@yy�@y�f@y��@zDD@zx&@z�J@{�@{H]@{��@{�HG�O�@  �G�O�@ ^G�O�G�O�G�O�G�O�@ G�O�G�O�G�O�@ ^G�O�G�O�@ G�O�G�O�@ jG�O�@ jG�O�@ �G�O�@ v@ %@ �@ �@ 1@ 
=@ �@ J@ V@ b@ o@ *@ �@ �@ �@ g@ ""@ %�@ (G@ *S@ -@ /�@ 33@ 5?@ 9X@ ;d@ ?}@ B�@ E�@ I�@ M�@ Q=@ T�@ X@ Z�@ ^5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��mA��yA��yA��mA��A���A���A���A���A�A�%A�A�A�%A�1A�
=A�JA�JA�JA�JA�JA�VA�JA�JA�JA�1A���A�=qA�bA�+A�(�A�+A��mAǡ�A��`A�A�=qAĮA���A��A¾wA�t�A�^5A��hA�hsA�JA��A�+A�`BA�1'A��^A�(�A�l�A��
A�{A�;dA��yA�/A�A���A�bNA���A�XA�(�A���A���A�C�A�1A��^A�A�A��A���A�^5A��A�bA��A��A���A�bNA��A���A�r�A���A�O�A��mA�{A���A�VA�n�A��A�I�A���A��9A��A�x�A�t�A�~�A��A�`BA��A���A��A�Q�A�1'A�A�  A�A�S�A��
A��-A��A��-A��A��A�\)A��A�E�AXAy?}Av~�At��As�7ArM�Aop�Al=qAj�`Ah�uAg�Afz�Ae�AdA�AcVAb�Aa��A`��A_\)A^(�A]&�A\�/A\$�A[�7AZ��AV�AT1'AS��AQO�ANbAK��AK�AJ�uAIp�AH{AF�AF9XADĜAA�AA�A@ffA@1'A?�A>{A<��A;�hA;VA:��A:  A8��A7A5�mA4��A4�\A3�A2ĜA2 �A0�uA0bA/�FA.�+A,�HA+ƨA+%A*��A*1'A(�A&��A&  A$�HA$1'A#K�A"��A!�A!+A�wAK�A�A�A��AK�AE�A/A�9AAhsA&�A^5Al�A��Ax�A��A{A��A�A��A�A&�Av�A��A�HA �AhsA
��A
M�A	hsA�yAVA�FA�HA{A&�A��A�#A �uA bNA A�@���@���@��@��H@�hs@��@��@�  @�5?@�X@��u@��;@�
=@�ff@��@��@�E�@��`@���@�7L@�9@�R@�R@띲@�ƨ@���@�Q�@�@�?}@�A�@��@�A�@�  @�x�@�E�@�l�@�%@��@��j@�I�@�`B@��w@��H@�x�@�/@��
@��@�b@�S�@��7@�"�@�x�@�z�@�C�@�ȴ@��/@�Q�@��@�V@��^@�j@��@��\@�5?@���@���@��y@���@�/@�(�@l�@~�@~$�@|1@{��@z�\@x��@wl�@wK�@vE�@up�@t�D@s��@r~�@qhs@p��@o\)@nff@m`B@k�F@j�@iX@i%@h�@g
=@eO�@cƨ@bJ@`��@`1'@]@Y&�@X�`@Xb@WK�@U�@SdZ@RM�@QG�@O��@N�y@N@L(�@J��@JM�@IX@G�w@F@E�@D�D@C33@BJ@Ax�@@��@>�+@<��@<�D@;"�@9��@8�`@8�u@8b@6ff@5@4I�@3S�@2n�@1�^@1%@0bN@/\)@.v�@-�-@,I�@+��@)��@(b@'K�@&��@%�@$��@$1@#��@#��@"M�@!��@ �u@\)@�y@$�@�h@�@��@@�@bN@  @�@�+@��@V@��@(�@��@X@hs@�9@Q�@\)@+@�R@$�@V@I�@33@
��@
M�@	�7@��@b@�@;d@ff@�T@�@��@�@@~�@-@�@ bN?��?�j?���?�X?��P?��
?�33?��?�v�?�?�^5?�X?�
=?�+?��/?�33?�n�?�G�?�  ?�{?ܬ?�ƨ?�~�?�1'?և+?�Z?�t�?���?У�?�\)?�5??͑h?�/?��m?˅?���?�^5?��#?ȓu?�1'?�1'?���?Ǯ?Ƈ+?š�?�Z?öF?�o?��?��`?� �?��?�5??�O�?��?�I�?�"�?��H?�~�?�~�?�=q?���?��?�^5?���?�?�C�?�ƨ?�(�?��D?�V?���?���?�{?�{?�V?�v�?���?���?��?��?���?��?�;d?�\)?���?��w?��;?�  ?�  ?�A�?�bN?�bN?�Ĝ?��`A��#A��;A��
A��A���A���A�A�A�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��/A��;A��TA��yA��`A��`A��mA��yA��A��yA��yA��yA��mA��mA��mA��yA��A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A�A�A�%A�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           A���A��A��mA��yA��yA��mA��A���A���A���A���A�A�%A�A�A�%A�1A�
=A�JA�JA�JA�JA�JA�VA�JA�JA�JA�1A���A�=qA�bA�+A�(�A�+A��mAǡ�A��`A�A�=qAĮA���A��A¾wA�t�A�^5A��hA�hsA�JA��A�+A�`BA�1'A��^A�(�A�l�A��
A�{A�;dA��yA�/A�A���A�bNA���A�XA�(�A���A���A�C�A�1A��^A�A�A��A���A�^5A��A�bA��A��A���A�bNA��A���A�r�A���A�O�A��mA�{A���A�VA�n�A��A�I�A���A��9A��A�x�A�t�A�~�A��A�`BA��A���A��A�Q�A�1'A�A�  A�A�S�A��
A��-A��A��-A��A��A�\)A��A�E�AXAy?}Av~�At��As�7ArM�Aop�Al=qAj�`Ah�uAg�Afz�Ae�AdA�AcVAb�Aa��A`��A_\)A^(�A]&�A\�/A\$�A[�7AZ��AV�AT1'AS��AQO�ANbAK��AK�AJ�uAIp�AH{AF�AF9XADĜAA�AA�A@ffA@1'A?�A>{A<��A;�hA;VA:��A:  A8��A7A5�mA4��A4�\A3�A2ĜA2 �A0�uA0bA/�FA.�+A,�HA+ƨA+%A*��A*1'A(�A&��A&  A$�HA$1'A#K�A"��A!�A!+A�wAK�A�A�A��AK�AE�A/A�9AAhsA&�A^5Al�A��Ax�A��A{A��A�A��A�A&�Av�A��A�HA �AhsA
��A
M�A	hsA�yAVA�FA�HA{A&�A��A�#A �uA bNA A�@���@���@��@��H@�hs@��@��@�  @�5?@�X@��u@��;@�
=@�ff@��@��@�E�@��`@���@�7L@�9@�R@�R@띲@�ƨ@���@�Q�@�@�?}@�A�@��@�A�@�  @�x�@�E�@�l�@�%@��@��j@�I�@�`B@��w@��H@�x�@�/@��
@��@�b@�S�@��7@�"�@�x�@�z�@�C�@�ȴ@��/@�Q�@��@�V@��^@�j@��@��\@�5?@���@���@��y@���@�/@�(�@l�@~�@~$�@|1@{��@z�\@x��@wl�@wK�@vE�@up�@t�D@s��@r~�@qhs@p��@o\)@nff@m`B@k�F@j�@iX@i%@h�@g
=@eO�@cƨ@bJ@`��@`1'@]@Y&�@X�`@Xb@WK�@U�@SdZ@RM�@QG�@O��@N�y@N@L(�@J��@JM�@IX@G�w@F@E�@D�D@C33@BJ@Ax�@@��@>�+@<��@<�D@;"�@9��@8�`@8�u@8b@6ff@5@4I�@3S�@2n�@1�^@1%@0bN@/\)@.v�@-�-@,I�@+��@)��@(b@'K�@&��@%�@$��@$1@#��@#��@"M�@!��@ �u@\)@�y@$�@�h@�@��@@�@bN@  @�@�+@��@V@��@(�@��@X@hs@�9@Q�@\)@+@�R@$�@V@I�@33@
��@
M�@	�7@��@b@�@;d@ff@�T@�@��@�@@~�@-@�@ bN?��?�j?���?�X?��P?��
?�33?��?�v�?�?�^5?�X?�
=?�+?��/?�33?�n�?�G�?�  ?�{?ܬ?�ƨ?�~�?�1'?և+?�Z?�t�?���?У�?�\)?�5??͑h?�/?��m?˅?���?�^5?��#?ȓu?�1'?�1'?���?Ǯ?Ƈ+?š�?�Z?öF?�o?��?��`?� �?��?�5??�O�?��?�I�?�"�?��H?�~�?�~�?�=q?���?��?�^5?���?�?�C�?�ƨ?�(�?��D?�V?���?���?�{?�{?�V?�v�?���?���?��?��?���?��?�;d?�\)?���?��w?��;?�  ?�  ?�A�?�bN?�bN?�Ĝ?��`A��#A��;A��
A��A���A���A�A�A�ƨA�ȴA�ƨA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��/A��;A��TA��yA��`A��`A��mA��yA��A��yA��yA��yA��mA��mA��mA��yA��A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A�A�A�%A�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�uB
�oB
�oB
�hB
�oB
�oB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
��B
�BB�B-B-B@�BB�BL�B]/BdZBgmBp�B��B�jBƨB��B�B�#B��B��BǮBɺB�)B�yB�B��B+BuB6FB7LBG�BJ�BK�BL�BP�BQ�BI�BE�BC�B?}B?}B<jB9XB;dB<jB=qB7LB2-B.B+B(�B#�B"�B�B�BuBbBJB
=BDB+BB��B�B�B�BB�B��B�jB��B�VB~�Bo�BZBH�B=qB)�B�BDB
��B
�B
�BB
�9B
�uB
y�B
VB
8RB
 �B
uB	��B	�5B	�3B	��B	�bB	�7B	�1B	y�B	jB	gmB	^5B	S�B	N�B	G�B	@�B	>wB	;dB	8RB	33B	/B	(�B	%�B	#�B	�B	�B	uB��B��B��B�BĜBBB�jB�?B�9B�RB�LB�!B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�VB�VB�7B�%B�B�Bz�Bt�Bo�Bo�Bn�BiyBcTBe`B`BB_;B^5B^5B_;B`BBaHBe`BffBe`Be`Be`BbNBbNBbNBbNB^5B_;B_;B]/B_;BaHB_;BbNB]/B_;BYBZBW
BT�BR�BP�BO�BL�BK�BI�BF�BG�BG�BG�BF�BE�BE�BC�B@�B;dB:^B8RB7LB6FB49B6FB6FB5?B6FB5?B49B2-B33B1'B1'B/B.B.B,B-B6FB:^BD�BH�BH�BW
B[#B^5B`BBbNB`BBW
B`BBW
BT�B]/BgmBo�B}�B�B�1B��B�qBɺB��B��B�B�)B�sB	B	VB	�B	%�B	,B	9XB	7LB	7LB	5?B	@�B	C�B	I�B	VB	\)B	jB	m�B	�B	�B	�+B	�VB	��B	��B	�!B	�qB	B	��B	��B	�B	�B	�BB	�HB	�sB	�yB	�B	�B	�B	��B	��B	��B	��B
B
%B

=B
PB
VB
\B
\B
bB
uB
�B
�B
�B
�B
�B
�B
"�B
!�B
"�B
#�B
&�B
'�B
(�B
(�B
+B
,B
,B
/B
0!B
0!B
1'B
2-B
5?B
6FB
7LB
8RB
9XB
9XB
:^B
<jB
>wB
>wB
?}B
?}B
@�B
@�B
A�B
C�B
C�B
E�B
E�B
G�B
G�B
H�B
G�B
J�B
J�B
K�B
L�B
L�B
O�B
P�B
Q�B
R�B
S�B
T�B
T�B
VB
VB
XB
YB
ZB
ZB
ZB
]/B
\)B
^5B
_;B
`BB
bNB
cTB
cTB
dZB
dZB
ffB
e`B
ffB
gmB
iyB
jB
iyB
jB
jB
l�B
l�B
l�B
m�B
m�B
o�B
p�B
q�B
r�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
v�B
w�B
x�B
z�B
z�B
z�B
z�B
}�B
}�B
~�B
�B
�B
�B
�%B
�B
�+B
�7B
�7B
�DB
�DB
�PB
�PB
�\B
�hB
�bB
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
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�?B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�^B
�XB
�XB
��B
�hB
�hB
�oB
�{B
�{B
�uB
�{B
�oB
�oB
�uB
�oB
�oB
�uB
�uB
�oB
�uB
�oB
�uB
�oB
�oB
�hB
�oB
�uB
�oB
�oB
�oB
�oB
�hB
�hB
�hB
�oB
�oB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�hB
�hB
�oB
�hB
�hB
�bB
�bG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           B
�MB
�GB
�HB
�AB
�HB
�IB
�BB
�BB
�CB
�CB
�CB
�DB
�DB
�EB
�FB
�FB
�GB
�GB
�HB
�IB
�IB
�JB
�JB
�KB
�LB
�LB
�MB
�TB
��B
��BBtB,�B,�B@lBBxBL�B]BdEBgXBp�B��B�WBƕB��B��B�B��B��BǟBɫB�B�kB�B��BBiB6;B7ABG�BJ�BK�BL�BP�BQ�BI�BE�BC�B?xB?xB<fB9TB;aB<gB=oB7JB2,B.B+B(�B#�B"�B�B�BxBfBNB
BBIB1BB��B�B�B�JB� B��B�tB��B�`BBo�BZ)BH�B=}B*	B�BRB
��B
��B
�QB
�HB
��B
y�B
VB
8bB
 �B
�B	�B	�GB	�EB	��B	�uB	�JB	�DB	y�B	j�B	g�B	^JB	TB	N�B	G�B	@�B	>�B	;|B	8jB	3LB	/4B	)B	%�B	#�B	�B	�B	�B�B��B��B�.BĺB®B®B��B�_B�ZB�sB�nB�CB�B� B�B�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�cB�QB�FB�9B{Bt�Bo�Bo�Bn�Bi�Bc�Be�B`sB_mB^gB^hB_nB`vBa}Be�Bf�Be�Be�Be�Bb�Bb�Bb�Bb�B^oB_uB_vB]jB_wBa�B_xBb�B]mB_yBYVBZ\BWJBU>BS3BQ&BP!BMBL
BI�BF�BG�BG�BG�BF�BE�BE�BC�B@�B;�B:�B8�B7�B6�B4�B6�B6�B5�B6�B5�B4�B2{B3�B1vB1wB/lB.eB.fB,ZB-aB6�B:�BD�BI	BI
BW`B[zB^�B`�Bb�B`�BWcB`�BWiBU`B]�Bg�BpB~aB�vB��B�kB��B�5B�QB�`BؔBܰB��B	�B	�B	B	&zB	,�B	9�B	7�B	7�B	5�B	A+B	DAB	JhB	V�B	\�B	k5B	nJB	��B	��B	��B	�B	�zB	��B	��B	�BB	�cB	˘B	έB	��B	��B	�%B	�.B	�\B	�eB	�zB	�B	�B	��B	��B	��B	��B

B
,B
GB
^B
gB
pB
sB
{B
�B
�B
�B
�B
�B
�B
�B
$B
"�B
$B
%B
(#B
)-B
*5B
*8B
,FB
-OB
-QB
0gB
1oB
1rB
2{B
3�B
6�B
7�B
8�B
9�B
:�B
:�B
;�B
=�B
?�B
?�B
@�B
@�B
A�B
A�B
CB
EB
EB
G'B
G*B
I8B
I;B
JDB
I@B
LVB
LYB
MaB
NjB
NmB
Q�B
R�B
S�B
T�B
U�B
V�B
V�B
W�B
W�B
Y�B
Z�B
[�B
[�B
[�B
^�B
]�B
`B
aB
bB
d'B
e/B
e2B
f;B
f>B
hMB
gIB
hRB
i\B
kjB
lsB
koB
lxB
l{B
n�B
n�B
n�B
o�B
o�B
q�B
r�B
s�B
t�B
u�B
u�B
v�B
v�B
w�B
x�B
y�B
x�B
y�B
z�B
}B
}B
}B
}B
�/B
�4B
�?B
�SB
�XB
�iB
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
�B
�B
�"B
�/B
�9B
�FB
�SB
�_B
�jB
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
�B
�B
�B
�!B
�/B
�6B
�IB
�eB
�tB
��B
��B
��B
��B
��B
��B
�
B
� B
�/B
�KB
�`B
�pB
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�:B
�IB
�QB
�OB
�XB
�UB
�XB
�[B
�dB
�aB
�dB
�gB
�qB
�mB
�vB
�tB
�vB
�zB
��B
�B
��B
��B
��B
��B
��B
�YB
�@B
�@B
�GB
�SB
�SB
�MB
�SB
�GB
�GB
�MB
�GB
�GB
�MB
�MB
�GB
�MB
�GB
�MB
�GB
�GB
�@B
�GB
�MB
�GB
�GB
�GB
�GB
�@B
�AB
�AB
�HB
�HB
�AB
�AB
�AB
�AB
�AB
�HB
�IB
�IB
�BB
�BB
�BB
�BB
�BB
�<B
�BB
�CB
�CB
�CB
�CB
�=B
�CB
�DB
�KB
�DB
�DB
�>B
�>G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202512021061413552620210614135526202106171312142021061713121420210617131214201807242202512021061413552620210614135526202106171312142021061713121420210617131214PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422025120180724220251  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025120180724220251QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422025120180724220251QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145720210617131457IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                