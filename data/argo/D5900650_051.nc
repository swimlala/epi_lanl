CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-02-04T06:48:01Z creation;2009-03-18T07:28:43Z update;2015-06-09T20:03:11Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900650 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA  20060204064801  20150614054511  A5_23632_051                    2C  D   APEX                            1557                            013004                          846 @���3�I1   @�+��@6*~��"��cs����1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffAffAc33A���A�  A�  B
ffB  B2  BE��BY33Bk��B�  B�  B�33B���B���B�  B�ffBƙ�B�ffB�33B䙚B���B�33CffC�C  CL�C� C� C33C$�C)�C.� C3��C8��C=� CBL�CG� CQ  C[  Ce33Co33Cy33C���C���C��fC���C��fC�ffC�� C��3C��3C���C��3C���C�s3C�Cǳ3C̙�Cр C�� CۦfC�3C�fC�� C�3C�� C�� D��D��D� DٚD��D��DٚD$��D)� D.�3D3�fD8��D=��DB�3DGٚDLٚDQ��DV�fD[��D`ٚDe�fDj� Do�3DtٚDy�fD�&fD�l�D��3D���D�&fD�i�D���D��3D�)�D�ffD���D��3D�)�D�l�Dڬ�D�� D�)�D�VfD�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ffAffA[33A���A�  A�  BffB  B0  BC��BW33Bi��B�  B�  B�33B���B���B�  B�ffBř�B�ffB�33B㙚B���B�33C �fC��C
� C��C  C  C�3C#��C(��C.  C3�C8�C=  CA��CG  CP� CZ� Cd�3Cn�3Cx�3C�Y�C���C�ffC�L�C�ffC�&fC�@ C�s3C�s3C���C�s3C�L�C�33C�Y�C�s3C�Y�C�@ Cր C�ffC�s3C�ffC� C�s3C� C�� D��D��D� D��D��D��D��D$��D)� D.�3D3�fD8��D=��DB�3DG��DL��DQ��DV�fD[��D`��De�fDj� Do�3Dt��Dy�fD�fD�\�D��3D���D�fD�Y�D���D��3D��D�VfD���D��3D��D�\�Dڜ�D�� D��D�FfD�3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�C�A�"�A��A��A� �A�"�A�+A�/A�9XA�"�A�%A�ȴA��-A�O�A�-A��A�z�A�A�A�{A�A�A�v�A��A�=qA�%A�A�oA���A��A���A��A��mA�7LA���A�9XA�=qA�ZA���A�K�A��A�r�A�VA���A��;A�n�A���AwS�Aj  AS��AK�hAG�A@�A<M�A4�+A1�A+�A$z�A�A��A��AK�A��AK�A�w@���@�x�@��@�5?@�7L@У�@��@�E�@���@�l�@�1@�r�@��D@�ff@���@�o@�x�@�X@��w@�?}@�\)@�ȴ@�A�@�
=@�`B@��@���@�/@��@�\)@��+@z^5@t�@l��@fff@[��@P �@I�#@C�F@=�h@5�h@.ȴ@'�@#��@ b@/@�`@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�C�A�"�A��A��A� �A�"�A�+A�/A�9XA�"�A�%A�ȴA��-A�O�A�-A��A�z�A�A�A�{A�A�A�v�A��A�=qA�%A�A�oA���A��A���A��A��mA�7LA���A�9XA�=qA�ZA���A�K�A��A�r�A�VA���A��;A�n�A���AwS�Aj  AS��AK�hAG�A@�A<M�A4�+A1�A+�A$z�A�A��A��AK�A��AK�A�w@���@�x�@��@�5?@�7L@У�@��@�E�@���@�l�@�1@�r�@��D@�ff@���@�o@�x�@�X@��w@�?}@�\)@�ȴ@�A�@�
=@�`B@��@���@�/@��@�\)@��+@z^5@t�@l��@fff@[��@P �@I�#@C�F@=�h@5�h@.ȴ@'�@#��@ b@/@�`@�@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB��BĜBǮB��B��B��B�;B�NB��BJB�BO�BT�BffBffBffB��B��B�NB�ZB��BuB0!B	7BbB��B�ZB�ZB�TB�`B��B�dB��B��B{�By�B}�Bq�B�B�/BM�B,B
�B
bNB
;dB	��B	�DB	jB�B�B��B�fB��B�LB�B��B�uB�Bz�Bq�Bk�BcTBXB\)BF�B=qB?}B49B)�B)�B5?B@�BM�BbNBv�B��B�B	%B	33B	=qB	H�B	Q�B	dZB	|�B	�\B	��B	�RB	��B	��B	�5B	�BB	�B	�B	��B
%B
DB
PB
�B
$�B
.B
:^B
?}B
D�B
I�B
O�B
T�B
XB
^5B
bNB
dZB
hsB
m�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BB��BĜBǮB��B��B��B�;B�NB��BJB�BO�BT�BffBffBffB��B�B�TB�`B��BuB1'B	7BhB��B�`B�ZB�ZB�mB��B�wB��B��B}�By�B~�Bu�B�B�BBO�B.B
�B
dZB
>wB	��B	�\B	o�B�B�B��B�mB��B�RB�B��B��B�B{�Br�Bm�Be`BZB]/BG�B=qBA�B5?B+B+B6FBA�BN�BcTBv�B��B�B	%B	33B	=qB	H�B	Q�B	dZB	|�B	�\B	��B	�RB	��B	��B	�5B	�BB	�B	�B	��B
%B
DB
PB
�B
$�B
.B
:^B
?}B
D�B
I�B
O�B
T�B
XB
^5B
bNB
dZB
hsB
m�B
t�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = RecalS = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = celltm_sbe41(RecalS,TEMP,PRES_ADJUSTED,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P= dbar since the start of the profile for each samples.                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.5 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200706281405292007062814052920070628140529200706290310372007062903103720070629031037200707020000002007070200000020070702000000  JA  ARFMfmtp2.2                                                                 20060204064801  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060204064802  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060204070049                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060208005431  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060208005433  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060208010819                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140529  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20070628140529  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070629031037  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001072747  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001091645                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120639  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318072319  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072843                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609200305                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614054511                      G�O�G�O�G�O�                