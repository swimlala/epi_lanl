CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-06-06T18:53:09Z creation;2018-06-06T18:53:10Z conversion to V3.1;2019-09-10T08:55:06Z update;2022-07-26T02:46:56Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20180606185309  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               LA   JA  V4_131545_076                   2C  Ddg�ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�g{z�0�1   @�g�#E�@31���l��dg��-V1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�33A&ffAy��A�33A�ffA�B��B!��B733BG33B]33Br  B���B�  B�  B�ffB�  B���B�33B�33B���Bۙ�B�  B�33B�ffCffC�C33C��C��C��C�3C%� C)��C/L�C4� C9��C>��CB��CHL�CQffC[33CeffCo� CzL�C���C�L�C�s3C�ffC��C�s3C�&fC�&fC�L�C�ffC�L�C��fC��CÀ C�  C�Y�Cҙ�C׀ C�@ C�L�C�  C�&fC��fC��3C��DٚD�D&fD33D,�D3D   D$� D)��D/fD4  D9FfD>&fDC  DGٚDL�3DRfDW  D[�fD`��Df&fDk9�Dp&fDt�3Dz33D�FfD��fD��3D� D�VfD��3D�� D�3D�<�D��3D�ٚD�fD�6fDԀ D�� D��D�S3D�vfD��fD�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�ffA(  A{33A�  A�33A�ffB33B"  B7��BG��B]��BrffB�  B�33B�33B���B�33B�  B�ffB�ffB�  B���B�33B�ffB���C� C33CL�C�3C�fC�3C��C%��C)�fC/ffC4��C9�fC>�fCB�fCHffCQ� C[L�Ce� Co��CzffC���C�Y�C�� C�s3C�&fC�� C�33C�33C�Y�C�s3C�Y�C��3C��CÌ�C��C�ffCҦfC׌�C�L�C�Y�C��C�33C��3C�  C��D� D  D,�D9�D33D�D fD$�fD)�3D/�D4&fD9L�D>,�DCfDG� DL��DR�DWfD[��D`�3Df,�Dk@ Dp,�Dt��Dz9�D�I�D���D��fD�3D�Y�D��fD��3D�fD�@ D��fD���D�	�D�9�Dԃ3D��3D� D�VfD�y�D�ɚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�?}A� �A�E�A�;dAҾwAҋDA�E�A�AѬAѡ�AѓuA��mA���A�(�A�(�A�ȴA�7LA���A��mA�oA�|�A��FA���A�/A��9A���A��uA��A�7LA�A��mA�$�A���A�XA�A�A�1A�=qA���A��A��A�dZA�|�A��A{x�Ag�AU�AL��AHAD�A=�FA5��A0�+A'�7A$JA�A��A��AS�A��A �A�DAXAoA $�@�G�@��@��@�(�@�w@�bN@��#@���@���@�/@��h@�t�@�V@�+@���@��w@�v�@���@��7@���@��y@���@�o@�1'@��#@�^5@��F@���@�ƨ@�J@��D@}�@s��@kS�@c33@Y�@R~�@K�m@Fȴ@AG�@;o@3t�@,�/@(b@"��@/@Ĝ@��@%@�T@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�?}A� �A�E�A�;dAҾwAҋDA�E�A�AѬAѡ�AѓuA��mA���A�(�A�(�A�ȴA�7LA���A��mA�oA�|�A��FA���A�/A��9A���A��uA��A�7LA�A��mA�$�A���A�XA�A�A�1A�=qA���A��A��A�dZA�|�A��A{x�Ag�AU�AL��AHAD�A=�FA5��A0�+A'�7A$JA�A��A��AS�A��A �A�DAXAoA $�@�G�@��@��@�(�@�w@�bN@��#@���@���@�/@��h@�t�@�V@�+@���@��w@�v�@���@��7@���@��y@���@�o@�1'@��#@�^5@��F@���@�ƨ@�J@��D@}�@s��@kS�@c33@Y�@R~�@K�m@Fȴ@AG�@;o@3t�@,�/@(b@"��@/@Ĝ@��@%@�T@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�qB	ÖB	ÖB	��B	��B	�B	�B	�B	�
B	�#B	�#B	�)B	�TB
-B
ǮB+BH�B�bB��B�LB�B�B�HB��B�jB�LB�B�?B�B��B��B��B�B��BĜB�LB��BiyB=qB%B
�dB
^5B
5?B
DB	�B	ffB��B�B�mB�/B��B�jB�!B�B�B��B�7B�B�B�+B�B�DB�VB��B�B�-B�FB�RB�?B�B��B��B�qB�`B	5?B	N�B	hsB	{�B	�DB	��B	��B	�3B	ɺB	�)B	�B	�B	��B	��B
B
B
JB
bB
uB
�B
�B
�B
&�B
-B
1'B
9XB
?}B
E�B
H�B
K�B
O�B
S�B
\)B
bNB
ffB
l�B
p�B
t�B
x�B
|�B
� B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	ðB	�{B	��B	�NB	�EB	�EB	�SB	�YB	�=B	�=B	��B	��B
1AB
�B	�BJrB��B��B��B�=B��B��B�xB��B�rBؓB��B��B�BB��B �B�}B�NBňB�XB�BlBAoB	7B
��B
_�B
6�B
�B	��B	j�B�fB��B�>B�;B��B��B�|B�!B�5B�&B��B�uB�B�KB��B��B��B�yB��B�3B�fB�$B��B�B�B��B��B�B	5�B	O�B	h�B	|B	�xB	��B	��B	�hB	��B	�CB	�B	��B	��B	�B
GB
mB
~B
�B
�B
�B
�B
�B
'B
-)B
1AB
9rB
?}B
E�B
H�B
K�B
O�B
TB
\CB
bNB
f�B
l�B
p�B
t�B
x�B
|�B
�B
��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806170015472018061700154720180617001547202207232056292022072320562920220723205629202207261123402022072611234020220726112340  JA  ARFMdecpV4_b                                                                20180606185143  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180606185309  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180606185309  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180606185310  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180606185310  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180606185310  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180606185310                      G�O�G�O�G�O�                JA  ARUP                                                                        20180606185531                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180607000000  CF  PSAL_ADJUSTED_QC?���@�33G�O�                JM  ARCAJMQC2.0                                                                 20180616151547  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180616151547  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180617151521  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115629  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                