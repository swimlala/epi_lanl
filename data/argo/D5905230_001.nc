CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2018-05-28T06:51:46Z creation;2018-05-31T15:52:44Z conversion to V3.1;2019-09-06T07:36:20Z update;2022-11-10T04:30:34Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20180528065146  20221117044512  5905230 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_123617_001                   2C  Dd�;ARVOR                           OIN-13JAP-ARL-57                5607A05                         844 @�e��� 1   @�e��+ @+�\(��d�;dZ�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@ə�A,��Ak33A���A�ffA�B��B��B2��BFffBY33Br��B�ffB���B�ffB�33B�  B���B�  B�ffB�33Bݙ�B���B�ffB�33C��C��C�fC  CffC�C � C%��C)�fC/� C433C9��C>� CCL�CHL�CR��C\��Cf�3Cp�3Cz�3C��3C��C��C��3C�@ C���C���C�L�C�Y�C�s3C�@ C�&fC�@ C�  C�ٚC��3C��C�&fC�@ C�ffC�33C�ffC�Y�C�@ C�L�DfD� D��D  D� D3D� D%�D*FfD/@ D433D9&fD>9�DC�DH  DM33DR  DW3D\3Da@ DffDk3Dp�Du  Dy�3D�<�D�|�D��3D���D�` D�y�D���D��3D�<�D�� D��fD�  D�VfDԜ�D���D�	�D�Y�D�3D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@���A.ffAl��A�ffA�33A�ffB  B   B333BF��BY��Bs33B���B���B���B�ffB�33B�  B�33Bə�B�ffB���B�  B�B�ffC�fC�fC  C�C� C33C ��C%�3C*  C/��C4L�C9�3C>��CCffCHffCR�fC\�3Cf��Cp��Cz��C�  C��C��C�  C�L�C��fC�ٚC�Y�C�ffC�� C�L�C�33C�L�C��C��fC�  C�&fC�33C�L�C�s3C�@ C�s3C�ffC�L�C�Y�D�D�fD�3D&fD�fD�D�fD%  D*L�D/FfD49�D9,�D>@ DC3DH&fDM9�DRfDW�D\�DaFfDf�Dk�Dp  DufDyٚD�@ D�� D��fD���D�c3D�|�D�� D��fD�@ D��3D�ٚD�#3D�Y�DԠ D�� D��D�\�D�fD��D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A���AھwAھwAڍPAڏ\Aڏ\Aڏ\Aډ7Aڇ+Aڇ+A�|�A�p�A�bNA�{A�v�A���AӺ^AϺ^A�;dA��A�A�A�A��A�=qA�jA��jA���A�1'A�|�A��FA�dZA�|�A���A�+A��A��mA{�
Apr�Ap�AiAP��AF��AB�uA5ƨA,��A&ĜA I�A��Az�A�jA/AQ�A
9XA\)An�AXAG�@�{@�~�@���@�9X@���@�Z@���@���@�7@�M�@�dZ@�9X@އ+@�x�@ա�@ϕ�@� �@�/@�v�@�\)@��@��-@�  @�`B@�=q@��m@��P@�$�@�7L@���@�ƨ@��;@���@�E�@�/@��!@��@���@�I�@�r�@��@z^5@m@b�@Y��@Sƨ@L(�@C@6V@0r�@)hs@&V@!��@�/@�@(�@�w@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#A���AھwAھwAڍPAڏ\Aڏ\Aڏ\Aډ7Aڇ+Aڇ+A�|�A�p�A�bNA�{A�v�A���AӺ^AϺ^A�;dA��A�A�A�A��A�=qA�jA��jA���A�1'A�|�A��FA�dZA�|�A���A�+A��A��mA{�
Apr�Ap�AiAP��AF��AB�uA5ƨA,��A&ĜA I�A��Az�A�jA/AQ�A
9XA\)An�AXAG�@�{@�~�@���@�9X@���@�Z@���@���@�7@�M�@�dZ@�9X@އ+@�x�@ա�@ϕ�@� �@�/@�v�@�\)@��@��-@�  @�`B@�=q@��m@��P@�$�@�7L@���@�ƨ@��;@���@�E�@�/@��!@��@���@�I�@�r�@��@z^5@m@b�@Y��@Sƨ@L(�@C@6V@0r�@)hs@&V@!��@�/@�@(�@�w@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�B�B�B�B�B�B�#B�#B�#B�)B�)B�/B�HB�mB�B	z�B
,B
�'B
�BB%�BA�BT�BXBk�BhsBv�Be`B^5B6FBVB
��B
�1B
A�B
#�B
B	�jB	�%B	~�B	T�B	uB	�B	{B��B�B��B��B	�B	�B	�B	�B	!�B	2-B	Q�B	n�B	|�B	�{B	�B	�?B	�FB	�}B	B	ƨB	��B	��B	�B	�BB	�BB	�TB	�`B	�mB	�B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
%B
DB
DB
hB
uB
�B
�B
!�B
$�B
'�B
+B
-B
.B
7LB
9XB
:^B
B�B
H�B
N�B
T�B
W
B
[#B
_;B
gmB
k�B
p�B
s�B
v�B
z�B
}�B
�B
�%B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�@B�7B�7B�KB�1B�1B�1B�B�=B�=B�=B�CB�]BݘB��B�>B��B	}B
.IB
�B
��B)yBD3BXB^BrGBk6Bx�Bg�Ba|B9	BYB
��B
�(B
C�B
&2B

XB	��B	��B	��B	Z�B	B	�B	�B�VB�OB��B�(B	B	�B	B	xB	"�B	3�B	R B	n�B	}qB	��B	��B	�?B	��B	��B	ªB	�+B	�2B	�MB	�yB	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�*B	�"B
 4B
GB
YB
DB
�B
�B
�B
�B
�B
!�B
$�B
($B
+B
-CB
./B
7fB
9XB
:xB
B�B
H�B
OB
UB
W
B
[=B
_pB
g�B
k�B
p�B
s�B
v�B
z�B
~B
�B
�?B
�131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.1(dbar); PO2=0.0(dbar)                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806110015302018061100153020180611001530202210251418002022102514180020221025141800201806120010552018061200105520180612001055  JA  ARFMdecpV4_b                                                                20180528065145  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180528065146  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180528065147  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180528065148  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180528065148  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180528065148  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20180528065943                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20180531155147  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20180531155242  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20180531155242  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20180531155243  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20180531155243  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20180531155243  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180531155244                      G�O�G�O�G�O�                JA  ARUP                                                                        20180531155525                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20180601000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20180610151530  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180610151530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180611151055  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920071515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025051800  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117044512                      G�O�G�O�G�O�                