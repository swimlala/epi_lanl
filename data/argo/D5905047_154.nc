CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-07-23T06:52:07Z creation;2020-07-26T21:53:37Z conversion to V3.1;2022-07-26T02:43:46Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
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
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  EX   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K<   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    il   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200723065207  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_154                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�*���,�1   @�*Ǻg� @4%�����cX     1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       =���@�ffA  Ad��A�  A�  A�ffBffB"ffB533BHffB_��Bo33B���B���B�  B�33B���B���B���B�33B�  B�ffB���B�33B�ffC�C  C�fC��CL�C  C � C%33C*��C.��C4�C9� C>� CC� CH��CR33C\��Ce�fCp�Cz�3C��C�L�C�ffC�Y�C��3C�s3C�33C��3C�ٚC�@ C�  C�  C��3C�&fC��3C�L�C��C�ٚC�L�C��C��3C�ffC���C�@ C��D�3D  D33DfD  D  DٚD$�3D)�3D.��D3�3D9,�D>3DC33DH,�DM  DR  DV��D\�Da  De��Dj��Dp�Du9�Dz�D�S3D���D�ɚD�	�D�Y�D���D��3D�3D�P D��3D��fD�3D�C3D�i�Dڹ�D��D�I�D��D��fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111=���@�ffA  Ad��A�  A�  A�ffBffB"ffB533BHffB_��Bo33B���B���B�  B�33B���B���B���B�33B�  B�ffB���B�33B�ffC�C  C�fC��CL�C  C � C%33C*��C.��C4�C9� C>� CC� CH��CR33C\��Ce�fCp�Cz�3C��C�L�C�ffC�Y�C��3C�s3C�33C��3C�ٚC�@ C�  C�  C��3C�&fC��3C�L�C��C�ٚC�L�C��C��3C�ffC���C�@ C��D�3D  D33DfD  D  DٚD$�3D)�3D.��D3�3D9,�D>3DC33DH,�DM  DR  DV��D\�Da  De��Dj��Dp�Du9�Dz�D�S3D���D�ɚD�	�D�Y�D���D��3D�3D�P D��3D��fD�3D�C3D�i�Dڹ�D��D�I�D��D��fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aե�A���Aқ�A�|�A�dZA�l�A�^5A�Aћ�A��#A�%AͬA�AȅA�A�^5A�JA�n�A��uA���A�ffA���A�Q�A�5?A��A���A���A���A�ffA���A��A�&�A���A�1A��A��RA�z�A�$�A��A�n�A�+A��A�-A���A~�Ae�^AW��AI�AE�mA;33A5G�A1\)A,��A)p�A#�wA=qA"�Ap�A�;Ap�A�A�-AZA
��A\)A��@�ƨ@�V@��@��@�{@�~�@�w@���@ۥ�@�o@��@�O�@�-@�v�@��@�+@�G�@�^5@�9X@���@��R@�A�@���@��@�33@��@���@�@�33@���@���@�ȴ@��+@z~�@l�j@f�@Z�H@TZ@M@D�D@=/@5`B@1hs@,�D@'
=@!�7@(�@��@�;@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aե�A���Aқ�A�|�A�dZA�l�A�^5A�Aћ�A��#A�%AͬA�AȅA�A�^5A�JA�n�A��uA���A�ffA���A�Q�A�5?A��A���A���A���A�ffA���A��A�&�A���A�1A��A��RA�z�A�$�A��A�n�A�+A��A�-A���A~�Ae�^AW��AI�AE�mA;33A5G�A1\)A,��A)p�A#�wA=qA"�Ap�A�;Ap�A�A�-AZA
��A\)A��@�ƨ@�V@��@��@�{@�~�@�w@���@ۥ�@�o@��@�O�@�-@�v�@��@�+@�G�@�^5@�9X@���@��R@�A�@���@��@�33@��@���@�@�33@���@���@�ȴ@��+@z~�@l�j@f�@Z�H@TZ@M@D�D@=/@5`B@1hs@,�D@'
=@!�7@(�@��@�;@�j31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	s�B	�FB	�dB	�dB	��B
VB
<jB
[#B
XB
I�B
>wB
33B
N�B
_;B
��B
�3B
v�B
r�B
}�B
�FB
�;B
��B
��BB/BYBq�B�dB��BJB/B#�B�BO�B�BgmBR�BD�BB��B��BXB1B
��B
-B	~�B	'�B�B��B�'B��B��B�B�BB�TB��B	\B	>wB	aHB	^5B	O�B	dZB	ffB	cTB	jB	n�B	XB	x�B	p�B	�7B	��B	��B	�?B	�RB	�jB	�}B	��B	�!B	�FB	��B	��B	ŢB	�!B	��B	�!B	�^B	ɺB	��B	�B	�`B	�mB	��B	��B	��B
  B
PB
{B
�B
#�B
49B
;dB
E�B
J�B
O�B
VB
\)B
dZB
hsB
m�B
q�B
v�B
z�B
�B
�1B
�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	`B	�'B	�`B	�zB	��B	�B
&�B
EB
B�B
5%B
)_B
 �B
9�B
I�B
�B
�BB
bNB
]dB
i*B
�HB
�rB
��B
�XB
�5BkBD�B]IB�FB��B��BQB�B�B:�BlWBRB>wB1B�/B��B��BDMB
��B
�wB
�B	mB	�B��B�B�/B�2B��B�SB��B�/B��B�pB�B	)�B	K�B	I�B	:xB	O�B	Q�B	N�B	U�B	ZB	A�B	c�B	[	B	s�B	��B	��B	��B	� B	��B	��B	�6B	��B	��B	�ZB	�rB	�UB	��B	�"B	�kB	��B	�9B	�XB	�[B	ϫB	ѷB	�!B	�FB	�XB	�B	��B	��B
�B
"B
jB
%�B
/�B
4�B
:*B
@4B
FYB
NpB
R�B
W�B
[�B
`�B
d�B
mCB
raB
tT31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.021(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202008060016532020080600165320200806001653202207232057392022072320573920220723205739202207261128462022072611284620220726112846  JA  ARFMdecpV4_b                                                                20200723065206  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200723065207  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200723065207  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200723065208  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200723065208  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200723065208  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200723065336                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200726215248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200726215334  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200726215334  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200726215336  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200726215336  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200726215336  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200726215337                      G�O�G�O�G�O�                JA  ARUP                                                                        20200726215423                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200728000000  CF  PSAL_ADJUSTED_QC=���=���G�O�                JM  ARSQJMQC2.0                                                                 20200728000000  CF  TEMP_ADJUSTED_QC=���=���G�O�                JM  ARCAJMQC2.0                                                                 20200805151653  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200805151653  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022846  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                