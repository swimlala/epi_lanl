CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-06T06:52:01Z creation;2020-06-09T21:53:56Z conversion to V3.1;2020-12-25T04:13:52Z update;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  20200606065201  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_168                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @��Q�n 1   @���,�@;��+J�c�I�^1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA+33At��A���A���A�  B��B��B7��BL  B^ffBs33B���B�  B�ffB���B���B�ffB�ffB�  B�ffB�  B�  B�ffB���CL�C� C�C  C  C��C �C%�fC+  C/��C533C9��C?�CC�3CH  CRffC[�fCf33Co��Cz33C�ٚC�  C�s3C�L�C�ٚC�  C�  C��3C�@ C�L�C��3C�ٚC���C��C�&fC�33C�L�C�&fC�ffC�ٚC��C�L�C��3C�� C��DfD33D  D&fD�DfD fD%&fD*,�D/�D3�fD9�D>fDC�DGٚDM  DR  DW,�D\,�Da  De�3Dk,�Dp�Du3Dz�D�S3D���D��fD� D�\�D��3D��3D��3D�P D��fD�� D��3D�<�DԆfD��fD�3D�Y�D퉚D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�  @�ffA+33At��A���A���A�  B��B��B7��BL  B^ffBs33B���B�  B�ffB���B���B�ffB�ffB�  B�ffB�  B�  B�ffB���CL�C� C�C  C  C��C �C%�fC+  C/��C533C9��C?�CC�3CH  CRffC[�fCf33Co��Cz33C�ٚC�  C�s3C�L�C�ٚC�  C�  C��3C�@ C�L�C��3C�ٚC���C��C�&fC�33C�L�C�&fC�ffC�ٚC��C�L�C��3C�� C��DfD33D  D&fD�DfD fD%&fD*,�D/�D3�fD9�D>fDC�DGٚDM  DR  DW,�D\,�Da  De�3Dk,�Dp�Du3Dz�D�S3D���D��fD� D�\�D��3D��3D��3D�P D��fD�� D��3D�<�DԆfD��fD�3D�Y�D퉚D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A���A�33A�ȴA�ZAħ�A���A���A��A���A�oA�7LA�hsA���A���A��A��A��PA���A���A�jA���A��!A��DA�
=A�n�A�G�A�dZA�|�A��/A��;A�
=A���A���A��A�1'A�?}A���A��\A�/A{�;AuAn��Aj5?AcXA_/AZE�AUp�AO�AH�AE&�A?��A;�TA7
=A4�!A1%A-��A*9XA(v�A#XA M�A��A�AA�A�A{A��A	��A��A�@�Ĝ@��^@�`B@��T@��@��@���@�x�@�b@�@� �@��@�`B@��@��T@�t�@� �@�&�@���@�|�@�x�@K�@z�!@v�R@sC�@o\)@h�9@`b@W�w@R-@L�/@G�w@A��@;�F@3��@-�-@)7L@%O�@ Q�@�@K�@dZ@\)@��@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A���A�33A�ȴA�ZAħ�A���A���A��A���A�oA�7LA�hsA���A���A��A��A��PA���A���A�jA���A��!A��DA�
=A�n�A�G�A�dZA�|�A��/A��;A�
=A���A���A��A�1'A�?}A���A��\A�/A{�;AuAn��Aj5?AcXA_/AZE�AUp�AO�AH�AE&�A?��A;�TA7
=A4�!A1%A-��A*9XA(v�A#XA M�A��A�AA�A�A{A��A	��A��A�@�Ĝ@��^@�`B@��T@��@��@���@�x�@�b@�@� �@��@�`B@��@��T@�t�@� �@�&�@���@�|�@�x�@K�@z�!@v�R@sC�@o\)@h�9@`b@W�w@R-@L�/@G�w@A��@;�F@3��@-�-@)7L@%O�@ Q�@�@K�@dZ@\)@��@	X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B<jB8RB7LB?}B5?B"�B�B.BW
BVBM�BP�Bq�B{�B�B� Bs�Bu�Bp�Bt�Bm�BS�BN�BH�B9XB�BB��B�#B�^B�B��Bv�B?}B#�B
��B
�B
��B
�B
E�B
�B	�B	��B	�B	�{B	u�B	T�B	&�B	
=B	  B�ZB�B�}B�dB�!B��B��B�{B�B{�BjB\)BN�BL�BE�B@�B9XB/B(�B!�BoBJBDBPBoB�B%�B0!B@�BVBiyB�B��B�FBƨB�BB��B	�B	'�B	7LB	E�B	YB	k�B	y�B	�=B	��B	ǮB	�TB	��B
B
bB
�B
+B
8RB
@�B
G�B
M�B
S�B
ZB
`BB
e`B
k�B
o�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+�B<�B8�B7�B@OB;0B%�B�B/�BX�BW�BOvBS[Br�B}VB��B�UBu�Bv�Bp�BvBo�BUBPHBJ�B;�B BBB�FBܬB�JB�IB�xBzDBAoB&fB
��B
��B
��B
��B
GEB
_B	��B	��B	�!B	��B	v�B	V�B	(�B	^B	oB�zB�kB�4B�jB�'B��B�IB��B�B}<Bk�B]~BO�BM�BFYBA;B:�B0;B)�B"�B�BB�B�BB)B&fB0�BABV�Bi�B�uB�B�zB��B��B�B	�B	($B	7�B	E�B	YKB	k�B	zB	�XB	�B	��B	�TB	��B
3B
}B
�B
+B
8lB
@�B
G�B
M�B
TB
Z7B
`\B
ezB
k�B
o�B
s�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202006200015292020062000152920200620001529202006200200122020062002001220200620020012202006210012502020062100125020200621001250  JA  ARFMdecpV4_b                                                                20200606065201  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200606065201  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200606065202  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200606065203  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200606065203  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200606065204  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200606065344                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200609215311  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200609215354  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200609215354  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200609215355  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200609215355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200609215356  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200609215356                      G�O�G�O�G�O�                JA  ARUP                                                                        20200609215444                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200610000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20200619151529  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200619151529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200619170012  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200620151250  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                