CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-04-05T04:50:19Z creation;2009-03-18T07:29:40Z update;2015-06-09T20:04:18Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER       	            	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME      	            	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME       	            	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS        	               	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER      	         	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION         	         	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE       	            	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE      	            	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR      	            	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE         	         	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE         	            	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO       	            	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION      	            	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE         	            	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD      	         	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC       	         	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION         	         	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE      	         	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE         	         	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC       	         	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM        	            	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC       	         	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC       	         	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC       	         	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME      	            	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER         	         	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES      	         
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;l   PRES_ADJUSTED         	         	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP      	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED         	         	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL      	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC       	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED         	         	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC      	            	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR       	            	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER         	   
               	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA  20060405045019  20150614054512  A5_23632_057                    2C  D   APEX                            1557                            013004                          846 @�����1   @���P@59XbM��cY�Q�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A  Ah  A�  A���A�ffB	��B��B1��BF��BZ��Bm33B���B�ffB���B�33B���B�33B�ffBƙ�BЙ�B���B���B�  B���CffCffC33C�C� C��C��C$� C)ffC.L�C3�C8L�C=ffCBffCGL�CQ33C[33Ce� Co� CyL�C�� C���C�� C���C���C��3C�� C��fC��3C�� C��3C���C���C�CǦfC̦fCљ�Cր Cۀ C�� C�3C�3C���C���C���D� D�fD�fD��D�3D��D��D$�3D)��D.�3D3��D8�fD=�3DB�3DG��DL�3DQ�3DV�fD[�fD`� De�3Dj� Do��Dt� Dy�3D�#3D�i�D��fD���D�  D�\�D���D��3D�)�D�ffD��3D��3D�  D�i�Dڰ D��D�&fD�\�D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��Ad��A�ffA�33A���B��B��B0��BF  BZ  BlffB�33B�  B�33B���B�33B���B�  B�33B�33B�ffB�ffBB�33C33C33C  C�fCL�CffCffC$L�C)33C.�C2�fC8�C=33CB33CG�CQ  C[  CeL�CoL�Cy�C��fC�s3C��fC�s3C�� C���C�ffC���C���C��fC���C�s3C�� C�s3Cǌ�Č�Cр C�ffC�ffC�ffC噚CꙚC�3C��3C�� D�3D��D��D� D�fD��D� D$�fD)� D.�fD3� D8��D=�fDB�fDG� DL�fDQ�fDV��D[��D`�3De�fDj�3Do� Dt�3Dy�fD��D�c3D�� D��fD��D�VfD��fD���D�#3D�` D���D���D��D�c3Dک�D��3D�  D�VfD��D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�O�A�VA�Q�A�O�A�E�A�E�A�E�A�C�A�1'A�A��A���A��A�5?A��A�(�A�I�A�dZA�ffA���A�1'A�|�A��PA���A�E�A�jA���A���A�7LA�A��A�A�33A��mA��\A�dZA��A�VA���A���Aw�ApbAjv�Af�\A]�
AT�9AJ�AI�
ACl�A=�PA8n�A5�A.��A*~�A$�9A�DAv�A%A�A�hA��An�A��@�$�@�~�@�9X@��@���@���@�o@Ĭ@�hs@�7L@��@�\)@�@���@�$�@�t�@��@���@�/@�b@��#@�-@���@�+@���@��
@���@�I�@�E�@��/@�dZ@�b@~v�@vE�@qX@ix�@aX@XQ�@M�@FE�@A�#@:-@5@/+@'�P@"~�@�@��@�@��@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�O�A�VA�Q�A�O�A�E�A�E�A�E�A�C�A�1'A�A��A���A��A�5?A��A�(�A�I�A�dZA�ffA���A�1'A�|�A��PA���A�E�A�jA���A���A�7LA�A��A�A�33A��mA��\A�dZA��A�VA���A���Aw�ApbAjv�Af�\A]�
AT�9AJ�AI�
ACl�A=�PA8n�A5�A.��A*~�A$�9A�DAv�A%A�A�hA��An�A��@�$�@�~�@�9X@��@���@���@�o@Ĭ@�hs@�7L@��@�\)@�@���@�$�@�t�@��@���@�/@�b@��#@�-@���@�+@���@��
@���@�I�@�E�@��/@�dZ@�b@~v�@vE�@qX@ix�@aX@XQ�@M�@FE�@A�#@:-@5@/+@'�P@"~�@�@��@�@��@V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ĜB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ȴB
��B�BM�B��B�B1B �BuBVB�uB��B��B�RBǮB�?B�oBdZBffBp�B|�BG�B<jB�ZB��B�RBR�BDBB
�BB
�wB	�B	��B	iyB	��B	��B	�B	�B	  B	B�#B��B�LB�?B��B��B�B~�Br�Bn�BiyBdZB`BBP�BJ�B9XB9XB;dB8RB2-B9XBI�BVBl�B��B��B�B	�B�B	8RB	@�B	(�B	;dB	H�B	O�B	��B	��B	��B	ȴB	��B	�HB	�`B	�B	�B	��B	��B
B
DB
{B
�B
#�B
-B
7LB
?}B
F�B
I�B
P�B
T�B
ZB
aHB
e`B
hsB
n�B
o�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
ĜB
ĜB
ĜB
ŢB
ĜB
ŢB
ŢB
ŢB
ŢB
ȴB
�B�BM�B��B�B	7B!�B{BW
B�{B��B��B�^BȴB�FB��BffBgmBq�B~�BK�BA�B�mB��B�jBW
BJBB
�NB
ÖB	��B	�B	k�B	��B	��B	�%B	�B	  B	B�/B��B�RB�LB��B��B�B� Bs�Bo�BjBe`BaHBR�BL�B:^B9XB<jB9XB33B:^BJ�BVBl�B��B��B�B	�B�B	8RB	A�B	(�B	;dB	H�B	N�B	��B	��B	��B	ȴB	��B	�HB	�`B	�B	�B	��B	��B
B
DB
{B
�B
#�B
-B
7LB
?}B
F�B
I�B
P�B
T�B
ZB
aHB
e`B
hsB
n�B
o�B
s�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281404572007062814045720070628140457200706290235552007062902355520070629023555200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060405045019  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060405045020  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060405045020  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060405045812                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060409034823  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.0                                                                 20060409034823  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.4                                                                 20060409034824  QCP$                G�O�G�O�G�O�           3F6BCJA  ARUP                                                                        20060409035149                      G�O�G�O�G�O�                JA  ARUP                                                                        20060602051136                      G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060627031727  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060627031727  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060627035410                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140457  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140457  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629023555  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072805  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091737                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120641  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072419  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072940                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200412                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054512                      G�O�G�O�G�O�                