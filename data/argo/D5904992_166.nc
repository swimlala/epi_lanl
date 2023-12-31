CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-17T06:52:19Z creation;2020-05-20T21:54:21Z conversion to V3.1;2020-12-25T04:14:11Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Dh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  20200517065219  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_166                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @����̀1   @�����@<��-V�c��G�{1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@���A,��AvffA���A�  A�ffB  B$ffB6  BI��B^��BrffB���B�ffB�  B�  B�33B�ffB���B���B���B�  B�  B�ffB���C��C  C��C��C� C33C�fC%� C*� C/ffC4��C9�fC>��CB�fCH�fCR��C\ffCfL�CoffCz  C���C��C�@ C��fC�s3C���C�  C��3C�  C��fC�ٚC�33C��3C��3C��3C�ffC�L�C��C۳3C�@ C�� C���C��C��C�� D  D�fD�3D�D�D33D �D$� D*3D/33D4�D9@ D>  DCfDG��DL��DR�DWL�D\�Da33Df&fDk3DpfDt��Dz�D�L�D�|�D�ɚD�3D�P D��fD���D��D�<�D�� D��3D��D�` D�vfD���D�	�D�` D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?fff@���A,��AvffA���A�  A�ffB  B$ffB6  BI��B^��BrffB���B�ffB�  B�  B�33B�ffB���B���B���B�  B�  B�ffB���C��C  C��C��C� C33C�fC%� C*� C/ffC4��C9�fC>��CB�fCH�fCR��C\ffCfL�CoffCz  C���C��C�@ C��fC�s3C���C�  C��3C�  C��fC�ٚC�33C��3C��3C��3C�ffC�L�C��C۳3C�@ C�� C���C��C��C�� D  D�fD�3D�D�D33D �D$� D*3D/33D4�D9@ D>  DCfDG��DL��DR�DWL�D\�Da33Df&fDk3DpfDt��Dz�D�L�D�|�D�ɚD�3D�P D��fD���D��D�<�D�� D��3D��D�` D�vfD���D�	�D�` D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��/A���A�ffA�I�A�+A�bA�A���A��
A��wA��+A�?}A��DA�  A���A�ĜA�Q�A�;dA��!A��PA���A�l�A�7LA��!A��PA�r�A��\A��DA��uA���A�9XA�x�A�(�A�oA�x�A�p�A��HA���A�r�A�"�A���A|-AtAjJAd��A[|�AV-AP(�AJ�jAF��AB$�A>�A=��A;7LA6�A3t�A09XA.r�A*�yA%XA!�FA��AA�AC�A��A��A��A
Q�A��A
=A Ĝ@�!@�b@�J@�@���@��@�p�@�ff@�@���@���@�{@�@��@��@�~�@�j@��^@�K�@��h@��@|j@z~�@vV@so@i��@b=q@[t�@T(�@L��@HA�@A��@;S�@7
=@333@.@)&�@$Z@�@�\@\)@dZ@�P@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��/A���A�ffA�I�A�+A�bA�A���A��
A��wA��+A�?}A��DA�  A���A�ĜA�Q�A�;dA��!A��PA���A�l�A�7LA��!A��PA�r�A��\A��DA��uA���A�9XA�x�A�(�A�oA�x�A�p�A��HA���A�r�A�"�A���A|-AtAjJAd��A[|�AV-AP(�AJ�jAF��AB$�A>�A=��A;7LA6�A3t�A09XA.r�A*�yA%XA!�FA��AA�AC�A��A��A��A
Q�A��A
=A Ĝ@�!@�b@�J@�@���@��@�p�@�ff@�@���@���@�{@�@��@��@�~�@�j@��^@�K�@��h@��@|j@z~�@vV@so@i��@b=q@[t�@T(�@L��@HA�@A��@;S�@7
=@333@.@)&�@$Z@�@�\@\)@dZ@�P@S�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�!B9XB9XB9XB;dB;dB<jB<jB>wB?}B@�BA�BD�BH�BXB_;Be`BgmB�+By�Bv�Bt�Bn�B_;BZB5?B�B��B�B�TB��B�'B� BN�B�B
�B
�)B
��B
��B
�B
~�B
M�B

=B	�}B	��B	dZB	J�B	1'B	�B	1B��B�B�TB�BȴB�qB�'B��B�{B�Bv�Bn�Be`B^5BQ�BF�B=qB7LB1'B'�B!�B�BhBPB1BDBuB%�B+BA�BW
Bn�B�DB�BŢB��B�B��B	uB	'�B	6FB	A�B	P�B	[#B	l�B	z�B	��B	�wB	�B	�B
  B
hB
�B
'�B
/B
6FB
?}B
E�B
M�B
T�B
[#B
aHB
gmB
l�B
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B9	B9rB9rB;B;B<jB<�B>�B?�B@�BBABFYBK�BX�B`vBf�Bi�B��B{BxlBu�Bp!B`vB]B6�B �B  B�|B�BΥB�nB��BR:B�B
�?B
�B
�TB
��B
��B
��B
O�B
�B	�B	��B	e�B	LJB	2�B	�B		�B��B��B�B�KBɺB�]B��B��B�B�3Bw�BoOBfLB_�BSBG�B>BB8B2|B(�B"�BBTB"B�B�BaB&2B+�BB'BW�BoB��B�QB��B�&B�B�B	�B	(>B	6zB	A�B	QB	[WB	l�B	{B	��B	��B	�B	�B
 B
�B
�B
(
B
/B
6`B
?�B
E�B
M�B
U2B
[=B
abB
g�B
l�B
q�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005310015342020053100153420200531001534202005310200122020053102001220200531020012202006010013342020060100133420200601001334  JA  ARFMdecpV4_b                                                                20200517065219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200517065219  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200517065220  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200517065221  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200517065221  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200517065222  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200517065404                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200520215342  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200520215419  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200520215419  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200520215420  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200520215420  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200520215420  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200520215421                      G�O�G�O�G�O�                JA  ARUP                                                                        20200520215508                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200521000000  CF  PSAL_ADJUSTED_QC?fff?fffG�O�                JM  ARCAJMQC2.0                                                                 20200530151534  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200530151534  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200530170012  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200531151334  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                