CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-01-02T21:53:11Z creation;2019-01-02T21:53:13Z conversion to V3.1;2019-09-10T08:52:24Z update;2022-07-26T02:46:04Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190102215311  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA  V4_131545_097                   2C  Dd
�ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @؛�5y� 1   @؜I2q @3(�9Xb�d
�Q�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A+33Ak33A���A�ffA���B
��B��B6��BHffBZffBnffB�ffB���B�ffB���B�33B���B���B�33B���B�33B�ffB�ffB���C��C33C��C�3C�fC��C 33C%��C*33C/�3C5  C8�fC>L�CB�3CG33CP�fC[ffCf�Co  CyffC���C�ٚC�ffC��3C��C�  C�  C��3C��fC��C��C�ffC�ffC��C��3C�L�C��C��C��3C�3C��3C�33C�Y�C�  C�ffD&fD��D3D�D,�D,�D 3D%9�D*33D/  D3��D9  D>fDC9�DGٚDM�DR,�DW  D[��Da3Df3Dk&fDp33Dt�fDz�D�P D���D�� D�fD�S3D�s3D���D� D�S3D�� D��fD�� D�9�DԌ�D��3D��3D�S3D�fD�ɚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A+33Ak33A���A�ffA���B
��B��B6��BHffBZffBnffB�ffB���B�ffB���B�33B���B���B�33B���B�33B�ffB�ffB���C��C33C��C�3C�fC��C 33C%��C*33C/�3C5  C8�fC>L�CB�3CG33CP�fC[ffCf�Co  CyffC���C�ٚC�ffC��3C��C�  C�  C��3C��fC��C��C�ffC�ffC��C��3C�L�C��C��C��3C�3C��3C�33C�Y�C�  C�ffD&fD��D3D�D,�D,�D 3D%9�D*33D/  D3��D9  D>fDC9�DGٚDM�DR,�DW  D[��Da3Df3Dk&fDp33Dt�fDz�D�P D���D�� D�fD�S3D�s3D���D� D�S3D�� D��fD�� D�9�DԌ�D��3D��3D�S3D�fD�ɚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A͑hA͑hA͓uA͓uA̓AͅA͓uA͡�Aͧ�A�p�A�33A��HAʬA�oA�|�A�p�Aě�A��;A��A�~�A��A�A�l�A�r�A��A��A�Q�A��A��+A�S�A��wA��A�jA�JA���A���A�A�ƨA�bA�A���A��\A�(�A}�^Au;dAh~�A_\)AW\)AQ�AN�uAH�+AA/A:�A5�A/A&=qA!AA%AA�A
VA9XAO�A�#AVAV@�|�@��@�dZ@�
=@�v�@�%@�~�@�@��@���@�\)@��\@��/@��@�{@���@�j@��@��@�Ĝ@�V@�l�@��`@��@�b@���@��w@��@���@�G�@�bN@w�;@n��@d��@]�T@T�D@J��@@��@7l�@49X@.�y@*�\@$��@!�^@V@r�@(�@��@C�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A͑hA͑hA͓uA͓uA̓AͅA͓uA͡�Aͧ�A�p�A�33A��HAʬA�oA�|�A�p�Aě�A��;A��A�~�A��A�A�l�A�r�A��A��A�Q�A��A��+A�S�A��wA��A�jA�JA���A���A�A�ƨA�bA�A���A��\A�(�A}�^Au;dAh~�A_\)AW\)AQ�AN�uAH�+AA/A:�A5�A/A&=qA!AA%AA�A
VA9XAO�A�#AVAV@�|�@��@�dZ@�
=@�v�@�%@�~�@�@��@���@�\)@��\@��/@��@�{@���@�j@��@��@�Ĝ@�V@�l�@��`@��@�b@���@��w@��@���@�G�@�bN@w�;@n��@d��@]�T@T�D@J��@@��@7l�@49X@.�y@*�\@$��@!�^@V@r�@(�@��@C�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�PB
�\B
�\B
�\B
�{B
��B
��B
�3B
��B
�HB
�B(�B��B�TB�B+BBJBB��B�B(�B"�B$�B(�B2-B#�B�BhB��B��B�ZB�B�ZB�B�BB��B�B>wBhB
�TB
��B
�=B
cTB
#�B	��B	��B	�B	ffB	T�B	49B	\B�B�B�'B�\B�{B�BiyB_;BXBW
Bv�B�'B�)B��B�)B�;B��BÖB�#B�/B�B	PB	#�B	F�B	W
B	v�B	�+B	��B	�'B	�wB	ɺB	��B	�B	�BB	�yB	�B	��B
  B
B
1B
VB
{B
�B
�B
$�B
+B
2-B
7LB
?}B
C�B
J�B
O�B
XB
^5B
dZB
gmB
l�B
o�B
t�B
y�B
|�B
�B
�B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�;B
�GB
�GB
�aB
�fB
��B
��B
�8B
��B
ՁB
��B5B�+B׍B\B"hB��BB�^B��BB�B�B)B�B'RB�B4B�B�B�B��B�"BٚB��B�
B��B|B5�B�B
�B
�/B
HB
Y�B
�B	�EB	��B	v�B	[�B	J�B	*KB	9B�LB�B��B��B��Bw�B^�BT�BMjBK�BkB��B��BɠB� BԕB�B��B�B�:B�B	B	�B	;dB	K^B	kB	{�B	� B	�zB	��B	��B	�EB	�dB	ԕB	��B	�B	�"B	�TB	�RB	��B
�B
�B
�B
B
1B
VB
&�B
+�B
3�B
7�B
?.B
D3B
LJB
RoB
XyB
[�B
`�B
c�B
h�B
nB
q'B
wLB
yXB
}V31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=-0.012(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201901130015302019011300153020190113001530202207232056482022072320564820220723205648202207261125042022072611250420220726112504  JA  ARFMdecpV4_b                                                                20190102215154  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190102215311  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190102215312  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190102215312  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190102215312  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190102215312  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190102215313                      G�O�G�O�G�O�                JA  ARUP                                                                        20190102215548                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190103000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190112151530  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190112151530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190113151432  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115648  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022504  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                