CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-02-24T06:51:55Z creation;2009-03-18T07:28:21Z update;2015-06-09T20:03:34Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               5A   JA  20060224065155  20150614054511  A5_23632_053                    2C  D   APEX                            1557                            013004                          846 @� ���$1   @� �C!@66E�����cf�G�{1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Ac33A���A���A�  BffB��B133BE33BY33Bk��B���B�33B�  B���B���B�ffB�ffB���BЙ�B�  B���B�33B���C  C� CL�C� C� C33CL�C$� C)� C.�C3  C8  C=ffCBffCG� CQL�C[� Ce� CoL�Cy�C�� C��fC�� C�� C��3C��3C��3C���C�� C�� C���C��3C�� C¦fCǙ�C�ffCљ�C֦fCی�C�Y�C�3CꙚC��C� C���D��D��D� D��D�fD�fD�fD$��D)�3D.�3D3��D8ٚD=�fDB�3DGٚDLٚDQ�fDVٚD[� D`� De�3Dj� Do�fDt�3Dy��D�0 D�c3D��3D���D�&fD�l�D��fD��fD�0 D�` D��3D��D�,�D�VfDڰ D���D�  D�c3D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��Ac33A���A���A�  BffB��B133BE33BY33Bk��B���B�33B�  B���B���B�ffB�ffB���BЙ�B�  B���B�33B���C  C� CL�C� C� C33CL�C$� C)� C.�C3  C8  C=ffCBffCG� CQL�C[� Ce� CoL�Cy�C�� C��fC�� C�� C��3C��3C��3C���C�� C�� C���C��3C�� C¦fCǙ�C�ffCљ�C֦fCی�C�Y�C�3CꙚC��C� C���D��D��D� D��D�fD�fD�fD$��D)�3D.�3D3��D8ٚD=�fDB�3DGٚDLٚDQ�fDVٚD[� D`� De�3Dj� Do�fDt�3Dy��D�0 D�c3D��3D���D�&fD�l�D��fD��fD�0 D�` D��3D��D�,�D�VfDڰ D���D�  D�c3D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A�A��mA���A�A��^A��A�"�A�%A���A�  A���A��A��;A���A��wA��!A��^A���A�|�A�dZA�  A��9A�E�A�ȴA�(�A��-A�`BA��A�/A�bNA���A�p�A���A�C�A�S�A��hA���A��A��wA��#A���A���A}�Au��Ah�A\��AVbNAL�AJA�AE��A9�TA57LA3�A,��A(v�A%`BA!��A�+AƨAt�A��AVA�^A
^5A�9@��D@��y@�@��H@܃@ա�@�V@���@�5?@��@�G�@��w@�\)@�33@��7@�/@�bN@��@��h@��/@�=q@��@���@��@�7L@�E�@��;@�=q@���@�9X@�@w�;@jJ@c�F@]�-@V@N@GK�@?\)@6V@1hs@,j@'��@$j@ A�@�j@��@z�@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A�A��mA���A�A��^A��A�"�A�%A���A�  A���A��A��;A���A��wA��!A��^A���A�|�A�dZA�  A��9A�E�A�ȴA�(�A��-A�`BA��A�/A�bNA���A�p�A���A�C�A�S�A��hA���A��A��wA��#A���A���A}�Au��Ah�A\��AVbNAL�AJA�AE��A9�TA57LA3�A,��A(v�A%`BA!��A�+AƨAt�A��AVA�^A
^5A�9@��D@��y@�@��H@܃@ա�@�V@���@�5?@��@�G�@��w@�\)@�33@��7@�/@�bN@��@��h@��/@�=q@��@���@��@�7L@�E�@��;@�=q@���@�9X@�@w�;@jJ@c�F@]�-@V@N@GK�@?\)@6V@1hs@,j@'��@$j@ A�@�j@��@z�@M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�JB�PB�VB�VB�\B�hB�{B��B�{B�uB��B�!B�'B�3B�?B�?B��B��B�B0!B5?B=qB@�BC�B=qB=qB9XB��B��B�B��B��BM�B49B�B�B��B��BffB^5B7LB
��B
�hB
N�B
bB	��B	>wB	#�B�B�TB��B��B�bB�PB�7B�DB�B� B|�Bq�Bq�Bo�BhsB`BBVBI�BB�B>wB9XBC�BJ�B@�BW
BcTB��B�-B��B�B	1'B	H�B	K�B	l�B	jB	��B	�B	�'B	�jB	�wB	ƨB	�
B	�HB	�sB	�B	�B	��B
B
PB
�B
�B
$�B
,B
2-B
:^B
@�B
E�B
L�B
M�B
R�B
W
B
[#B
_;B
ffB
iyB
o�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�PB�PB�VB�VB�\B�hB�{B��B�{B�uB��B�!B�'B�3B�?B�?B��B��B�B0!B5?B=qB@�BC�B>wB=qB;dB��B��B�/B�
B��BO�B7LB�B��B�B��BgmB`BB9XB
��B
�uB
P�B
uB	��B	@�B	%�B�B�ZB��B��B�hB�\B�=B�JB�B�B}�Br�Bq�Bp�BiyBbNBXBK�BD�B>wB:^BD�BK�BA�BXBdZB��B�3B��B�B	1'B	H�B	K�B	l�B	jB	��B	�B	�'B	�jB	�wB	ƨB	�
B	�HB	�sB	�B	�B	��B
B
PB
�B
�B
$�B
,B
2-B
:^B
@�B
E�B
L�B
M�B
R�B
W
B
[#B
_;B
ffB
iyB
o�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.0 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281405182007062814051820070628140518200706290310252007062903102520070629031025200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060224065155  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060224065156  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060224070621                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060228005604  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060228005605  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060228011520                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140518  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140518  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629031025  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072513  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001090948                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120640  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072252  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072821                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200324                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054511                      G�O�G�O�G�O�                