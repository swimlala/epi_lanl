CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PARAM       N_PROF        N_CALIB       N_LEVELS   s   	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-02-28T14:48:47Z creation;2009-03-18T07:20:00Z update;2015-06-09T19:31:17Z conversion to V3.1;     
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
_FillValue                  `  L�   SCIENTIFIC_CALIB_EQUATION         	   
               	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT      	   
               	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT      	   
               	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE         	   
                	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION          	            	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _8   HISTORY_STEP         	            	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _<   HISTORY_SOFTWARE         	            	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _@   HISTORY_SOFTWARE_RELEASE         	            	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _D   HISTORY_REFERENCE            	            	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _H   HISTORY_DATE         	             	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _�   HISTORY_ACTION           	            	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _�   HISTORY_PARAMETER            	            	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES           	         	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES            	         	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE           	         	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST           	            	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.0 19500101000000  5900649 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050228144847  20150621172519  A5_23579_017                    2C  D   APEX                            1556                            013004                          846 @Ӭ�Y��X1   @Ӭ߬/@@5�l�C���b߅�Q�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�ffA��Ad��A�33A���A�ffB	��B��B/33BC��BXffBm33B��B���B�ffB�ffB�33B�  B���Bƙ�B�ffBڙ�B�ffB�  B�33C �fC�3C��C33C� CL�CL�C$L�C)  C.33C333C8L�C=��CB� CG� CQffC[� Ce��CoffCyffC��fC�� C�� C�s3C��fC�� C��fC���C���C�� C�� C�s3C��3C�� CǦfC�� Cѳ3C�� Cۀ C���C��C� C�fC� C���D�3D�fD�fD�3D� D��D�fD$�3D)� D.�fD3�fD8��D=� DB��DG�fDL�3DQ��DV�3D[� D`�3De� Dj��DoٚDt�fDy��D�)�D�l�D�� D���D�,�D�i�D��3D�� D�fD�i�D�� D��fD�#3D�p Dڠ D��fD�&fD�Y�D�3D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��Aa��A���A�  A���B��B  B.ffBB��BW��BlffB~��B�ffB�  B�  B���B���B�ffB�33B�  B�33B�  BB���C �3C� CffC  CL�C�C�C$�C(��C.  C3  C8�C=ffCBL�CGL�CQ33C[L�CeffCo33Cy33C���C�ffC�ffC�Y�C���C��fC���C�s3C�� C�ffC�ffC�Y�C���C¦fCǌ�C̦fCљ�C֦fC�ffC�s3C�s3C�ffC��C�ffC�� D�fDٚD��D�fD�3D��D��D$�fD)�3D.��D3��D8��D=�3DB��DG��DL�fDQ��DV�fD[�3D`�fDe�3Dj� Do��Dt��Dy� D�#3D�ffD���D��fD�&fD�c3D���D�ٚD� D�c3D���D�� D��D�i�Dڙ�D�� D�  D�S3D��D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A��A��#A��#A��;A��HA��TA��yA��yA��A��yA��#A��+A�ƨA�VA���A�|�A�?}A��
A���A��RA��/A�{A�ZA�9XA�5?A�M�A�\)A��;A�bNA�x�A��wA�`BA��A��#A�/A�ƨA�I�A�&�A���A�x�A��A~�9Aq�mAh�HA\ȴAV�AJ$�AC;dA?XA9�hA2E�A*ZA$�!A ��AhsA1'At�AA	�#AdZA bN@���@�b@�\)@�?}@�{@���@۝�@��`@ɑh@�9X@�J@��w@�~�@�S�@��P@��@���@�bN@��@���@�`B@��/@���@���@�"�@��7@�+@�?}@���@�o@�ƨ@�5?@���@��
@~ff@t�j@j��@`��@Z��@QX@L�@E@?;d@8�u@2^5@,z�@&ȴ@!hs@�@  @��@�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��
A��A��#A��#A��;A��HA��TA��yA��yA��A��yA��#A��+A�ƨA�VA���A�|�A�?}A��
A���A��RA��/A�{A�ZA�9XA�5?A�M�A�\)A��;A�bNA�x�A��wA�`BA��A��#A�/A�ƨA�I�A�&�A���A�x�A��A~�9Aq�mAh�HA\ȴAV�AJ$�AC;dA?XA9�hA2E�A*ZA$�!A ��AhsA1'At�AA	�#AdZA bN@���@�b@�\)@�?}@�{@���@۝�@��`@ɑh@�9X@�J@��w@�~�@�S�@��P@��@���@�bN@��@���@�`B@��/@���@���@�"�@��7@�+@�?}@���@�o@�ƨ@�5?@���@��
@~ff@t�j@j��@`��@Z��@QX@L�@E@?;d@8�u@2^5@,z�@&ȴ@!hs@�@  @��@�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbB\B\B\B\B\BbBbB\BVBhB�B5?B�B1'B33B1'B=qBQ�BVB�B�JB��B�RB�jBĜB@�B�uB��B�?B�PBr�B�1BhsB33B�XB�BaHB�B
�B
o�B
-B	�TB	�B	M�B	uB�B�-B��B��B�Bo�BgmBm�Bk�BjB^5BS�BM�BG�BA�B;dB8RB7LB2-B5?B8RBG�B[#By�B}�B�oB�wBɺB�B	B	�B	33B	XB	t�B	�+B	��B	�B	�B	�B	�RB	B	��B	�B	�#B	�`B	�B	�B	�B	��B
B
JB
�B
 �B
+B
.B
7LB
;dB
A�B
G�B
L�B
P�B
XB
]/B
bNB
gmB
k�B
p�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BbB\B\B\B\B\BbBbB\BVBhB�B5?B�B1'B33B1'B=qBQ�BVB�B�JB��B�RB�jBĜB@�B�uB��B�?B�PBr�B�1BhsB33B�XB�BaHB�B
�B
o�B
-B	�TB	�B	M�B	uB�B�-B��B��B�Bo�BgmBm�Bk�BjB^5BS�BM�BG�BA�B;dB8RB7LB2-B5?B8RBG�B[#By�B}�B�oB�wBɺB�B	B	�B	33B	XB	t�B	�+B	��B	�B	�B	�B	�RB	B	��B	�B	�#B	�`B	�B	�B	�B	��B
B
JB
�B
 �B
+B
.B
7LB
;dB
A�B
G�B
L�B
P�B
XB
]/B
bNB
gmB
k�B
p�B
u�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - SP, where SP is SURFACE PRESSURE (minus 5 dbar for Apf-6,7,8) from next cycle.                                                                                                                                                           none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL(PRES_ADJUSTED,TEMP,Conductivity)                                                                                                                                                                                                           none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            SP(NextCycle) = 0.2 dbar                                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using reported SURFACE PRESSURE. The quoted error is max [2.4, size of pressure adjustment] in dbar.                                                                                                                                      The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Salinity Recalculation using ADJUSTED Pressure                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200503120000002005031200000020050312000000200506060000002005060600000020050606000000JA  ARFMfmtp2.1                                                                 20050228144847  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050228144848  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050228145525                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050304124940  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050304124941  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050304125532                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050312000000  CV  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050312000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20050606000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060310063553  IP                  G�O�G�O�G�O�                JA  RFMTcnv22.0                                                                 20060414072356  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060418030330                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120555  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071840  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072000                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609193104                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621172519                      G�O�G�O�G�O�                