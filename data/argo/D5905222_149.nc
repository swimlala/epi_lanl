CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   r   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-06-28T07:00:50Z creation;2022-07-01T21:56:33Z conversion to V3.1;2022-11-10T04:16:53Z update;     
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
resolution        =���     �  <@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @D   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Gl   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   MT   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   VT   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  hT   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         iH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         iL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        iP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iTArgo profile    3.1 1.2 19500101000000  20220628070050  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_149                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @����X^ 1   @��(�@19XbN�e�����1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�  A   Aq��A���A���A�ffBffB   B733BH��BZffBr  B�  B���B���B���B�33B���B�33B���B�33B�33B���B�  B���C��C��C� C� C��C��C!  C$�fC*�C/L�C4��C9�3C>��CB��CH  CR� C\  Ce�fCp�3CyffC�� C��3C�  C�ٚC�33C���C���C�L�C�L�C�&fC�s3C�&fC�ٚC�Y�C�@ C�  CҀ C�33C�Y�C�L�C���C��C�33C��C��fD  D9�D3D��DٚD,�D 3D$��D*33D/33D4�D9�D>&fDB� DH&fDM�DQ��DV�3D\�D`�3De�fDj�3Dp  Du3Dz3D�@ D�� D���D���D�0 D�|�D���D�3D�P D���D�� D��D�P Dԃ3Dڹ�D�	�D�I�D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @���AffAp  A���A�  A홚B  B��B6��BHffBZ  Bq��B���B���B�ffB���B�  B�ffB�  Bș�B�  B�  B䙚B���B���C�3C�3CffCffC�3C�3C �fC$��C*  C/33C4�3C9��C>� CB� CG�fCRffC[�fCe��Cp��CyL�C��3C��fC��3C���C�&fC�� C�� C�@ C�@ C��C�ffC��C���C�L�C�33C��3C�s3C�&fC�L�C�@ C�� C��C�&fC��C�ٚD�D33D�D�fD�3D&fD �D$�3D*,�D/,�D4fD93D>  DBٚDH  DM3DQ�3DV��D\3D`��De� Dj��Dp�Du�Dz�D�<�D���D�ɚD���D�,�D�y�D�ɚD� D�L�D���D���D�	�D�L�DԀ DڶfD�fD�FfD� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��Aٲ-A١�Aٙ�Aٙ�Aٕ�Aٕ�Aُ\Aى7Aه+Aه+A�~�A�M�A���A�A��A�O�A�-A�JAԲ-A�ZAѰ!A�  A�S�AǺ^A���A���A��A���A�jA��A���A�A���A��A�-A��A�
=A�%A~�DArbAg�AY��ANQ�A;�A3G�A+�A'l�A$�uA"(�A�#Ar�AbA�A��At�A M�A�A �y@�Ĝ@�5?@��`@�bN@�v�@�x�@�Q�@�9X@�@�%@�`B@�9X@�x�@�C�@�j@��F@��
@�~�@�O�@���@�\)@���@�A�@�/@�  @��^@���@�l�@�C�@���@��@��+@�z�@��-@��F@���@��R@z�H@mO�@g\)@]�@T1@I7L@A%@9hs@2^5@+C�@$z�@K�@Ĝ@�
@A�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��Aٲ-A١�Aٙ�Aٙ�Aٕ�Aٕ�Aُ\Aى7Aه+Aه+A�~�A�M�A���A�A��A�O�A�-A�JAԲ-A�ZAѰ!A�  A�S�AǺ^A���A���A��A���A�jA��A���A�A���A��A�-A��A�
=A�%A~�DArbAg�AY��ANQ�A;�A3G�A+�A'l�A$�uA"(�A�#Ar�AbA�A��At�A M�A�A �y@�Ĝ@�5?@��`@�bN@�v�@�x�@�Q�@�9X@�@�%@�`B@�9X@�x�@�C�@�j@��F@��
@�~�@�O�@���@�\)@���@�A�@�/@�  @��^@���@�l�@�C�@���@��@��+@�z�@��-@��F@���@��R@z�H@mO�@g\)@]�@T1@I7L@A%@9hs@2^5@+C�@$z�@K�@Ĝ@�
@A�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�5B
�ZB
�fB
�`B
�`B
�`B
�`B
�`B
�`B
�fB
�fB
�`B
�`B
�`B
�NB
�BB
�;B
�/B
��B
�/B
�B7LBW
BbNB`BBR�B��B�1B�uB��B��B��B��B�bBm�BS�BPB
�
B
��B
��B
iyB
&�B	�sB	�B	gmB	/B��B�ZB��B��BɺBĜB�B��B��B��B��B��B�B	
=B	�B	$�B	{B	JB		7B	"�B	(�B	+B	G�B	P�B	A�B	e`B	� B	�bB	��B	��B	B	ɺB	��B	��B	�/B	�TB	�B	��B	��B	��B
B
1B
\B
\B
uB
�B
�B
�B
$�B
&�B
1'B
8RB
?}B
F�B
J�B
P�B
W
B
_;B
cTB
jB
o�B
u�B
{�B
� B
�B
�=B
�PB
�u311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�?B
�JB
�pB
�jB
�6B
�6B
�PB
�6B
�6B
�<B
�<B
�6B
�jB
̈́B
�XB
�KB
ǮB
ƎB
��B
�B
��B 'B@BJ�BJXBAB��Bv�B~(B��B��B�lB�-B}�BWsBA�B
��B
ÖB
� B
�#B
TFB
TB	�@B	��B	S&B	CB�B��B��B�B��B��B��B�cB�8B��B��B��B�B��B	�B	"B��B�B�B	xB	�B	�B	0oB	9�B	*B	N"B	h�B	y	B	�B	�7B	�6B	�-B	��B	�rB	ňB	�0B	�+B	ߊB	�tB	�B	�B	�B	��B	��B	��B
 B
9B
KB
jB
vB
�B
 �B
'�B
/B
33B
9XB
?�B
G�B
K�B
R�B
XB
^B
d@B
hXB
m�B
r�B
u�B
{�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.3(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.023(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202207120015062022071200150620220712001506202210251313282022102513132820221025131328202210251812502022102518125020221025181250  JA  ARFMdecpV4_b                                                                20220628070049  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220628070050  IP                  G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220628155526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220701215632  IP                  G�O�G�O�G�O�                JA      jafc1.0                                                                 20220701215633                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220702065637  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220702065637  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220701215657                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220701151507  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220701151507  QCF$                G�O�G�O�G�O�200000          JM  ARGQJMQC2.0                                                                 20220701151506  CV  JULD_LOCATION   G�O�G�O�F��X                JM  ARCAJMQC2.0                                                                 20220711151506  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220711151506  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041328  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091250  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                