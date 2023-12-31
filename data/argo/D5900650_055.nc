CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-03-16T06:51:35Z creation;2009-04-07T03:46:23Z update;2015-06-09T20:03:59Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               7A   JA  20060316065135  20150614054513  A5_23632_055                    2C  D   APEX                            1557                            013004                          846 @� ��1   @����@5�t�j~��cn�+J1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @���A33Ac33A�ffA�33A�33B	33B��B2ffBF  BX��Bm��B�ffB���B���B�33B���B�  B���Bř�B�ffB���B���B�ffB���C33C� C� CL�C� C� C33C$� C)  C.ffC3L�C8  C=L�CB��CG��CQffC[33Ce33CoffCy� C���C��fC���C��fC���C�� C��3C�� C���C�� C���C��fC�� C�CǦfC̳3Cљ�C֙�Cی�C�� C噚CꙚC�3C�� C���D� DٚDٚD� D��D��DٚD$�3D)��D.�fD3�3D8�fD=�fDBٚDG��DL�fDQ� DV�fD[ٚD`�fDe� Dj�3DoٚDt�3Dy��D�)�D�i�D�� D��fD�&fD�l�D���D�� D��D�l�D�� D��fD�&fD�l�Dڰ D��D�&fD�c3D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���A33Ac33A�ffA�33A�33B	33B��B2ffBF  BX��Bm��B�ffB���B���B�33B���B�  B���Bř�B�ffB���B���B�ffB���C33C� C� CL�C� C� C33C$� C)  C.ffC3L�C8  C=L�CB��CG��CQffC[33Ce33CoffCy� C���C��fC���C��fC���C�� C��3C�� C���C�� C���C��fC�� C�CǦfC̳3Cљ�C֙�Cی�C�� C噚CꙚC�3C�� C���D� DٚDٚD� D��D��DٚD$�3D)��D.�fD3�3D8�fD=�fDBٚDG��DL�fDQ� DV�fD[ٚD`�fDe� Dj�3DoٚDt�3Dy��D�)�D�i�D�� D��fD�&fD�l�D���D�� D��D�l�D�� D��fD�&fD�l�Dڰ D��D�&fD�c3D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�Q�A�XA�ZA�\)A�\)A�\)A�^5A�`BA�ZA�E�A���A�E�A�&�A�{A�oA�|�A��wA�VA�G�A��9A��A�5?A��TA�-A�?}A���A���A���A�VA���A�XA��\A�7LA�33A� �A��!A�M�A��A�\)A�O�Au�Ao��AbbNA\1'A[�AR��AE\)A=O�A9��A49XA/+A*�\A&��A!�A�#A�-AA&�A��A
��A��A�h@��h@�%@�;d@�^5@���@�$�@�V@�=q@�=q@��w@��7@�z�@���@�1@�ff@��j@���@�z�@��h@�1'@��7@�5?@���@��@�@�X@�@���@�S�@��-@��u@�ȴ@�~�@;d@s�m@k��@c�F@\��@U�h@N@E��@>�R@9��@3�F@.@)��@$��@��@��@E�@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�O�A�Q�A�XA�ZA�\)A�\)A�\)A�^5A�`BA�ZA�E�A���A�E�A�&�A�{A�oA�|�A��wA�VA�G�A��9A��A�5?A��TA�-A�?}A���A���A���A�VA���A�XA��\A�7LA�33A� �A��!A�M�A��A�\)A�O�Au�Ao��AbbNA\1'A[�AR��AE\)A=O�A9��A49XA/+A*�\A&��A!�A�#A�-AA&�A��A
��A��A�h@��h@�%@�;d@�^5@���@�$�@�V@�=q@�=q@��w@��7@�z�@���@�1@�ff@��j@���@�z�@��h@�1'@��7@�5?@���@��@�@�X@�@���@�S�@��-@��u@�ȴ@�~�@;d@s�m@k��@c�F@\��@U�h@N@E��@>�R@9��@3�F@.@)��@$��@��@��@E�@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBm�Bm�Bm�Bm�Bm�Bn�Bn�Bo�Bo�Bq�Bu�B�B��B�
B��B��BB1BPBhBVB
=BB��B�B��B�9Bu�BS�B2-B��B�NB�DB[#B(�B\B
ÖB
��B
�%B
ffB
8RB	�jB	�B	E�B	D�B	��B	\)B�;B��B��B�XB��B��B�JB}�Bw�Br�BiyBgmBcTB]/BW
BI�BD�B;dB0!B+B-B5?B7LB>wBQ�Bl�B|�B�+B��B��B�B	B	1'B	C�B	K�B	k�B	K�B	bNB	�B	��B	�jB	��B	�B	�/B	�NB	�yB	�B	��B
B
JB
�B
 �B
(�B
/B
5?B
<jB
B�B
G�B
K�B
P�B
VB
ZB
_;B
dZB
iyB
n�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bo�Bo�Bq�Bv�B�B��B�
B  B  BB	7BVBhB\BDBB��B��B��B�XBx�BVB5?B��B�`B�VB^5B)�B{B
ŢB
��B
�1B
hsB
;dB	�wB	�'B	G�B	D�B	��B	_;B�HB��B��B�^B��B��B�PB~�Bx�Bt�BjBhsBdZB^5BXBJ�BF�B<jB1'B,B.B6FB8RB>wBR�Bm�B|�B�+B��B��B�B	B	1'B	D�B	K�B	l�B	K�B	bNB	�B	��B	�jB	��B	�B	�/B	�NB	�yB	�B	��B
B
JB
�B
 �B
(�B
/B
5?B
<jB
B�B
G�B
K�B
P�B
VB
ZB
_;B
dZB
iyB
n�B
r�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.0 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281405072007062814050720070628140507200706290310122007062903101220070629031012200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060316065135  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060316065136  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060316070345                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060320005129  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060320005130  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060320010230                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140507  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140507  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629031012  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072807  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091740                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120640  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072412  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072934                      G�O�G�O�G�O�                JA  ARDU                                                                        20090407034623                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200348                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054513                      G�O�G�O�G�O�                