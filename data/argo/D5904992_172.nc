CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-07-16T06:51:59Z creation;2020-07-19T21:53:36Z conversion to V3.1;2020-12-25T04:13:13Z update;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  20200716065159  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_172                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�(�WkT�1   @�)�8�@;	x����c�     1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@�ffA0  Ay��A���A���A�  B��B#33B8ffBHffB\��Bm��B�33B���B���B�33B�  B�33B�33B�ffBҙ�B���B���B�33B�33C�fC  C�fC��C�3C33C L�C%�3C*��C/  C4�C8��C>  CB�fCH��CQ��C\ffCe�CpffCzffC��3C�L�C��3C��3C�@ C�@ C�33C��fC��3C��C�@ C���C�33C�ffC�33C��3C��C�  C��3C�3C��C��3C�@ C��3C�33D  D�fD��D�fD�D  D &fD%�D*fD/�D3� D8��D=��DC�DG��DL��DQ�fDV��D[�fDa  Df�Dj� Dp�Dt��Dz�D�C3D�l�D�ٚD��D�FfD��3D���D�3D�FfD�|�D�� D��D�c3Dԃ3D��3D�fD�VfD퉚D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?fff@�ffA0  Ay��A���A���A�  B��B#33B8ffBHffB\��Bm��B�33B���B���B�33B�  B�33B�33B�ffBҙ�B���B���B�33B�33C�fC  C�fC��C�3C33C L�C%�3C*��C/  C4�C8��C>  CB�fCH��CQ��C\ffCe�CpffCzffC��3C�L�C��3C��3C�@ C�@ C�33C��fC��3C��C�@ C���C�33C�ffC�33C��3C��C�  C��3C�3C��C��3C�@ C��3C�33D  D�fD��D�fD�D  D &fD%�D*fD/�D3� D8��D=��DC�DG��DL��DQ�fDV��D[�fDa  Df�Dj� Dp�Dt��Dz�D�C3D�l�D�ٚD��D�FfD��3D���D�3D�FfD�|�D�� D��D�c3Dԃ3D��3D�fD�VfD퉚D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A��AθRAΙ�A�l�A�G�A��;A�7LA���A�M�A��uA�ȴA��`A��
A�"�A���A��hA�bA��A���A�"�A��#A�^5A��/A���A�9XA�^5A�S�A���A��9A���A���A���A��TAoA}VAy&�AxJAt�+Ap�jAg?}Ac�AYƨAT��AM�FAHȴAD�A@1'A=dZA;A6�!A61A2~�A1%A,�9A)ƨA#33A ZAhsA��A��A33AffA�/Ap�Ap�A�7@�Z@���@�@���@��@��@���@���@�{@�r�@�o@�r�@�dZ@�=q@�"�@��
@�33@�z�@�Ĝ@���@�j@�M�@�j@{��@x1'@t1@pr�@nff@k�
@f@_l�@Xb@Q7L@IG�@B�!@;�
@5@01'@)��@$��@��@M�@ff@n�@+@dZ@�y@�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A��AθRAΙ�A�l�A�G�A��;A�7LA���A�M�A��uA�ȴA��`A��
A�"�A���A��hA�bA��A���A�"�A��#A�^5A��/A���A�9XA�^5A�S�A���A��9A���A���A���A��TAoA}VAy&�AxJAt�+Ap�jAg?}Ac�AYƨAT��AM�FAHȴAD�A@1'A=dZA;A6�!A61A2~�A1%A,�9A)ƨA#33A ZAhsA��A��A33AffA�/Ap�Ap�A�7@�Z@���@�@���@��@��@���@���@�{@�r�@�o@�r�@�dZ@�=q@�"�@��
@�33@�z�@�Ĝ@���@�j@�M�@�j@{��@x1'@t1@pr�@nff@k�
@f@_l�@Xb@Q7L@IG�@B�!@;�
@5@01'@)��@$��@��@M�@ff@n�@+@dZ@�y@�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BŢB��B��B��B�B�BB�TB�
B�BaHBW
BbNB+B�B�;B�B�fB?}BN�B;dB(�B�B�#B��B[#B/B
�B
�VB
YB
`BB
��B
��B
z�B
_;B
C�B
7LB
�B
�B	��B	�;B	�B	��B	XB	>wB	�B	1B	B	B��B�B�#B�BĜB�jB��B��B�+B|�Bs�Be`BW
BL�BF�B?}B2-B,B&�B!�B�BoBbB\BPB�B�B(�B33B@�BS�BhsB�B�uB��BȴB�)B��B	1B	�B	,B	<jB	R�B	dZB	v�B	�B	�VB	��B	�!B	ɺB	�TB	��B
PB
�B
(�B
33B
;dB
E�B
M�B
T�B
]/B
bNB
ffB
k�B
p�B
v�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�%B� B�B�2B�QB��B�B�|B�lBe�B[qBfLB.B�iB�vB�eB�B?HBPB=�B*eB_BݘB�VB]dB1�B
�?B
� B
[=B
`vB
��B
��B
|�B
a�B
D�B
9$B
pB
$B	��B	�TB	�;B	�B	Y�B	@B	�B		RB	YB	�B�FB��B�qB�
B�B��B��B�pB�B}�Bt�Bf�BX+BM�BG+BA�B2�B,�B'�B"�B�BBNB}B<B�BB)�B3�BA BT{Bh�B�UB��B�_B��B�]B�B	fB	�B	,=B	<�B	S&B	d�B	v�B	�3B	�pB	��B	�;B	��B	�nB	��B
jB
�B
)B
3MB
;B
E�B
M�B
UB
]IB
bhB
f�B
k�B
p�B
v�B
z�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202007300015362020073000153620200730001536202007300200102020073002001020200730020010202007310012292020073100122920200731001229  JA  ARFMdecpV4_b                                                                20200716065158  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200716065159  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200716065159  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200716065200  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200716065200  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200716065200  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200716065339                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200719215258  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200719215335  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200719215335  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200719215336  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200719215336  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200719215336  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200719215336                      G�O�G�O�G�O�                JA  ARUP                                                                        20200719215422                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200720000000  CF  PSAL_ADJUSTED_QC?fff?fffG�O�                JM  ARCAJMQC2.0                                                                 20200729151536  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200729151536  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200729170010  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200730151229  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                