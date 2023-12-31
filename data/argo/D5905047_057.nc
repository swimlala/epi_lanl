CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-11-28T18:54:10Z creation;2017-11-28T18:54:12Z conversion to V3.1;2019-09-10T08:57:41Z update;2022-07-26T02:47:41Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20171128185410  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA  V4_131545_057                   2C  Dd�/ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�7�����1   @�7���ˀ@4�I�^5?�d�/��w1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                           @y��A  Ac33A���A���A���B33BffB0  BH  BZ  BjffB���B�33B���B���B�  B�ffB�  Bʙ�B���B�33B�33B�33B���C  C�fC�fC�fC  CL�C � C%�3C*�fC.�fC4  C9L�C>�fCC  CHL�CQffC[� Cf�Cp� Cz�3C��C�@ C��C�&fC��fC�@ C�� C��C��fC��C��3C��fC��fC¦fCǳ3C�ffC�  C�  C��C���C���C�� C��3C�ffC�ٚD��D�fDfD,�D��D��D��D%  D*fD/,�D3�3D99�D=� DC  DH3DM  DQٚDV��D\fDa,�De��Dj��Do��Du33Dy�3D�I�D�vfD�� D���D�@ D�l�D���D� D�9�D�ffD���D�	�D�VfDԌ�D�ɚD��fD�` D��D��D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111    @y��A  Ac33A���A���A���B33BffB0  BH  BZ  BjffB���B�33B���B���B�  B�ffB�  Bʙ�B���B�33B�33B�33B���C  C�fC�fC�fC  CL�C � C%�3C*�fC.�fC4  C9L�C>�fCC  CHL�CQffC[� Cf�Cp� Cz�3C��C�@ C��C�&fC��fC�@ C�� C��C��fC��C��3C��fC��fC¦fCǳ3C�ffC�  C�  C��C���C���C�� C��3C�ffC�ٚD��D�fDfD,�D��D��D��D%  D*fD/,�D3�3D99�D=� DC  DH3DM  DQٚDV��D\fDa,�De��Dj��Do��Du33Dy�3D�I�D�vfD�� D���D�@ D�l�D���D� D�9�D�ffD���D�	�D�VfDԌ�D�ɚD��fD�` D��D��D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЬAа!AЩ�AЩ�AЬAЩ�AЮAЮAЮAв-Aд9AжFAиRAк^AмjAоwAУ�A�E�A�  A���A���A���A�ƨA��A��A�hsA���A�ZA��jA��TA�C�A�?}A��A�(�A��A��;A�ZA��RA���A�jA�VA���A��/A��mA���A��#Au��Ac�A^ �AT�HAQdZAG��AB�jA=��A9C�A8=qA.bA+oA$ȴA�#A�+A�A�A��Ap�A|�@�%@�S�@���@�t�@�9@���@�z�@�?}@ŉ7@��@��#@���@�r�@��j@���@���@��;@�t�@�=q@��R@�z�@�
=@�x�@��@�`B@�33@���@��@�ff@��w@�~�@}��@x  @st�@j~�@`��@XQ�@Q%@J�@E�T@?�@8r�@2=q@+�@&�+@!�7@�/@�#@�/@hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЬAа!AЩ�AЩ�AЬAЩ�AЮAЮAЮAв-Aд9AжFAиRAк^AмjAоwAУ�A�E�A�  A���A���A���A�ƨA��A��A�hsA���A�ZA��jA��TA�C�A�?}A��A�(�A��A��;A�ZA��RA���A�jA�VA���A��/A��mA���A��#Au��Ac�A^ �AT�HAQdZAG��AB�jA=��A9C�A8=qA.bA+oA$ȴA�#A�+A�A�A��Ap�A|�@�%@�S�@���@�t�@�9@���@�z�@�?}@ŉ7@��@��#@���@�r�@��j@���@���@��;@�t�@�=q@��R@�z�@�
=@�x�@��@�`B@�33@���@��@�ff@��w@�~�@}��@x  @st�@j~�@`��@XQ�@Q%@J�@E�T@?�@8r�@2=q@+�@&�+@!�7@�/@�#@�/@hs31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BB��BBBBBBBBBBBBBBB  B��Bw�B� Bu�BjBaHB_;B`BBw�B�uB�+B|�Br�BjBcTB]/BW
BM�B:^B.B1B��B�NB�9B/B
��B
�qB
�7B
v�B
�B	�B	�DB	\)B	E�B	�B	B�yB��B��B��B��B�PB}�B{�Br�Bs�Bq�Bq�Bl�Bp�Br�Bu�Bw�B}�B�B�bB��B�'B�
B��B	\B	�B	:^B	G�B	H�B	y�B	�DB	��B	�B	�wB	ǮB	��B	�5B	�`B	�B	��B	��B
  B
1B
uB
�B
�B
#�B
,B
49B
:^B
A�B
E�B
I�B
N�B
T�B
XB
]/B
aHB
e`B
iyB
k�B
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B��BBBBBBBBBBBBBB B �B Bz*B�;Bv�Bn�Bb�B`�B`�ByrB�$B��B}Bt9BkkBdB^jBXEBQB;�B1'B	RB�qB�,B�>B1'B
��B
�cB
��B
y�B
#B	��B	��B	]/B	G�B	IB	AB��B�gB͟B��B��B�BB}B}VBt�Bu%Br�BrGBm�Bq�BshBv`Bx�B~wB��B�4B�B��B��B�B	�B	B	:�B	HB	H�B	z*B	�^B	��B	�OB	��B	��B	�B	�OB	�B	�B	��B	�B
 4B
fB
�B
�B
�B
$B
,=B
4TB
:xB
A�B
E�B
I�B
OB
UB
X+B
]IB
abB
ezB
i�B
k�B
o�B
s�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712090016222017120900162220171209001622202207232056122022072320561220220723205612202207261122242022072611222420220726112224  JA  ARFMdecpV4_b                                                                20171128185204  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20171128185410  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20171128185411  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20171128185411  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20171128185411  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20171128185411  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171128185412                      G�O�G�O�G�O�                JA  ARUP                                                                        20171128185628                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171129000000  CF  PSAL_ADJUSTED_QC    @y��G�O�                JM  ARSQJMQC2.0                                                                 20171129000000  CF  TEMP_ADJUSTED_QC       G�O�                JM  ARCAJMQC2.0                                                                 20171208151622  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171208151622  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050645  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115612  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                