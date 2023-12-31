CDF   '   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2009-04-12T12:56:20Z creation;2015-03-10T02:10:42Z update;2015-06-07T15:11:50Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  4900898 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA  20090412125620  20150613142511  A9_60144_101                    2C  D   APEX                            2414                            061305                          846 @�$�E6�1   @�$���@C�;dZ��d
=p��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ai��A�  A���A陚B��B33B2  BE��BZ  BlffB�  B���B���B���B���B�  B���Bƙ�B���B�  B���B�33B�  C  C�C
�fC�fC�fC�fC  C$  C)�C.�C3�C8  C<�fCA�fCG�CP��CZ��Ce�CoffCyffC��fC��3C��fC��fC���C��3C��3C���C�s3C�ffC�� C�ffC��fC³3C�� C̙�C���C�� C۳3C���C噚CꙚC�3C� C���DٚD�fD� D��D�fD�fDٚD$�fD)� D.��D3ٚD8�3D=ٚDB� DG�fDL� DQ�3DV� D[��D`ٚDe�3DjٚDoٚDtٚDy��D�)�D�i�D��fD���D��D�l�D���D��3D�#3D�ffD�� D��fD��D�i�Dڰ D��fD�  D�` D�3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A  A^ffA�ffA�  A�  B  BffB/33BB��BW33Bi��B33B�ffB�33B�33B�33B���B�ffB�33B�ffBٙ�B�ffB���B���C L�CffC
33C33C33C33CL�C#L�C(ffC-ffC2ffC7L�C<33CA33CFffCP�CZ�CdffCn�3Cx�3C�L�C�Y�C�L�C�L�C�@ C�Y�C�Y�C�@ C��C��C�&fC��C�L�C�Y�C�ffC�@ C�s3C�ffC�Y�C�33C�@ C�@ C�Y�C�&fC�33D��D��D�3D� D��D��D��D$��D)�3D.��D3��D8�fD=��DB�3DG��DL�3DQ�fDV�3D[� D`��De�fDj��Do��Dt��Dy� D�3D�S3D�� D��fD�fD�VfD��3D���D��D�P D���D�� D�fD�S3Dڙ�D�� D�	�D�I�D�|�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AO7LAOC�AO+AN�`AM�-AMoALv�AK�AH��AE��AD�!AC�ACG�AC�ACC�AAl�A>��A:^5A6��A2n�A0�A0JA/�mA0E�A0�A2��A4��A5�-A4��A4bA3/A1�PA0�9A/dZA.ffA,�A*Q�A)oA'C�A'&�A#�A!��A��A  AdZA5?A?}A�^AQ�AȴA��A7LA	�A"�A-A��A ��@��@���@�  @�~�@�?}@�bN@��@���@��@�j@ɑh@Ǿw@�p�@�&�@�x�@���@��/@�I�@��m@�7L@�`B@��`@�/@�`B@��T@�
=@�I�@�&�@}@xbN@s�@oK�@jM�@e�-@co@^�@[ƨ@W�;@P��@I�@B�H@<��@6@01'@+�F@(b@#��@�y@dZ@1'@��@�@$�@
��@l�@�D@�#@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AO7LAOC�AO+AN�`AM�-AMoALv�AK�AH��AE��AD�!AC�ACG�AC�ACC�AAl�A>��A:^5A6��A2n�A0�A0JA/�mA0E�A0�A2��A4��A5�-A4��A4bA3/A1�PA0�9A/dZA.ffA,�A*Q�A)oA'C�A'&�A#�A!��A��A  AdZA5?A?}A�^AQ�AȴA��A7LA	�A"�A-A��A ��@��@���@�  @�~�@�?}@�bN@��@���@��@�j@ɑh@Ǿw@�p�@�&�@�x�@���@��/@�I�@��m@�7L@�`B@��`@�/@�`B@��T@�
=@�I�@�&�@}@xbN@s�@oK�@jM�@e�-@co@^�@[ƨ@W�;@P��@I�@B�H@<��@6@01'@+�F@(b@#��@�y@dZ@1'@��@�@$�@
��@l�@�D@�#@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�{B��B�hB�JB�3B��B�B�sB�BS�B�JB��B��B��B��B��B�bB�=B{�Bm�BdZBdZBjBZB_;BR�BdZB]/BQ�B[#BXBI�B?}B8RB2-B-B(�B!�B�B�B�BB��B��B�B�yB�fB�NB�;B�5B�/B�)B�#B�ZB�B��BJB{B#�B-B9XBH�BYBiyBx�B�JB��B�B�RBǮB�B�mB��B	%B	VB	�B	%�B	2-B	G�B	\)B	q�B	�%B	��B	�3B	ÖB	��B	�NB	�B	��B
	7B
oB
�B
,B
6FB
?}B
G�B
O�B
T�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B�oB�JB�3B��B�B�mB�BS�B�JB��B��B��B��B��B�hB�DB|�Bn�Be`BdZBk�B[#B`BBR�Be`B^5BQ�B[#BYBJ�B@�B9XB33B.B)�B!�B�B�B�BB��B��B�B�B�mB�TB�BB�5B�/B�)B�)B�`B�B��BJB{B#�B-B9XBH�BYBiyBx�B�JB��B�B�RBǮB�B�mB��B	%B	VB	�B	%�B	2-B	G�B	\)B	q�B	�%B	��B	�3B	ÖB	��B	�NB	�B	��B
	7B
oB
�B
,B
6FB
?}B
G�B
O�B
T�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.7(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200904280134572009042801345720090428013457200904280143022009042801430220090428014302201010040000002010100400000020101004000000  JA  ARFMdecpA9_b                                                                20090412125619  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090412125620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090412125620  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090412125620  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090412125621  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090412125621  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090412125621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090412125621  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090412125622  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090412130322                      G�O�G�O�G�O�                JA  ARFMdecpA9_b                                                                20090416095516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.5                                                                 20090416095739  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20090416095739  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20090416095739  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090416095740  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090416095740  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090416095741  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090416095741  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090416095741  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090416100120                      G�O�G�O�G�O�                JA  ARCArsal2.1a                                                                20090821003840  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20090821003840  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20090821003841  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20090821003841  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8b                                                                20090821003841  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8b                                                                20090821003841  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20090821003841  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20090821011154                      G�O�G�O�G�O�                JM  ARGQJMQC1.0                                                                 20090415232554  CV  DAT$            G�O�G�O�F�&�                JM  ARCAJMQC1.0                                                                 20090428013457  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20090428013457  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20090428014302  CV  PSAL            G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2010V1                                                       20101004000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20101014014525  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20101015004921                      G�O�G�O�G�O�                JM  RENCREJM1.1c                                                                20150209092125  ED  SCIENTIFIC_CALIBG�O�G�O�G�O�                JA  ARDU                                                                        20150310021042                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607151142                      G�O�G�O�G�O�                JA  ARDU                                                                        20150613142511                      G�O�G�O�G�O�                