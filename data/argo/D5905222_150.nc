CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   k   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-07-08T07:02:39Z creation;2022-07-11T21:56:32Z conversion to V3.1;2022-11-10T04:16:51Z update;     
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
_FillValue                  l  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  A�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Bd   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  l  F(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  F�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    K�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Q�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    W�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ]�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ^L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ^P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ^T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ^X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ^\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ^�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ^�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ^�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ^�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ^�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ^�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ^�Argo profile    3.1 1.2 19500101000000  20220708070239  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_150                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��|��0 1   @�݋>2� @17
=p���e���o1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@�ffA)��Ak33A�33A�ffA�33B
��B!��B2��BJffB\  Bo33B���B���B�ffB�  B�33B�ffB�33BǙ�Bҙ�B�ffB晚B���B�  C� C  C��CffC33C��C ffC%�3C)�3C.�fC4  C9L�C=� CCL�CH  CQ��C[��Ce�fCp33Cy�3C�ffC�Y�C���C�&fC�� C�  C��fC�&fC���C�  C�L�C��3C���C�C��Č�C�&fCֳ3C�&fC�fC�ٚC�ffC�33C��C�� D  DfD,�D33D33DٚD 3D$��D)��D/�D4  D8��D=��DBٚDH  DM33DR&fDW33D\,�Da3Df9�Dk  Do�fDt�3Dz3D�\�D���D��3D�3D�P D��3D�� D�3D�FfD���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 >���@�ffA)��Ak33A�33A�ffA�33B
��B!��B2��BJffB\  Bo33B���B���B�ffB�  B�33B�ffB�33BǙ�Bҙ�B�ffB晚B���B�  C� C  C��CffC33C��C ffC%�3C)�3C.�fC4  C9L�C=� CCL�CH  CQ��C[��Ce�fCp33Cy�3C�ffC�Y�C���C�&fC�� C�  C��fC�&fC���C�  C�L�C��3C���C�C��Č�C�&fCֳ3C�&fC�fC�ٚC�ffC�33C��C�� D  DfD,�D33D33DٚD 3D$��D)��D/�D4  D8��D=��DBٚDH  DM33DR&fDW33D\,�Da3Df9�Dk  Do�fDt�3Dz3D�\�D���D��3D�3D�P D��3D�� D�3D�FfD���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JA��A���A׾wA׼jA׸RAװ!Aק�A�|�A�x�A�XA��Aԝ�AҮAя\A�;dA�33A��A��`A�G�A���A�VA˥�Aɕ�A�l�A�hsAŧ�A�VAąA��A�VA���A��`A�ffA�(�A�"�A�ZA�S�A��A�JA�33A�ƨA�G�A�A�z�A��A|��Ap�uAV�AM�
AG;dAB�`A< �A.��A(E�A#O�A�FA��A"�A�A �A�
AXAO�A��@���@��@��@�1'@�9@�$�@أ�@�1@ɺ^@��@�bN@�/@�^5@��\@�Z@�`B@���@�9X@���@���@�1'@��@���@��D@�Z@�$�@�  @��
@���@���@�&�@��@�X@~��@q�@eV@^{@Q�@D�@:=q@0Q�@-��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JA��A���A׾wA׼jA׸RAװ!Aק�A�|�A�x�A�XA��Aԝ�AҮAя\A�;dA�33A��A��`A�G�A���A�VA˥�Aɕ�A�l�A�hsAŧ�A�VAąA��A�VA���A��`A�ffA�(�A�"�A�ZA�S�A��A�JA�33A�ƨA�G�A�A�z�A��A|��Ap�uAV�AM�
AG;dAB�`A< �A.��A(E�A#O�A�FA��A"�A�A �A�
AXAO�A��@���@��@��@�1'@�9@�$�@أ�@�1@ɺ^@��@�bN@�/@�^5@��\@�Z@�`B@���@�9X@���@���@�1'@��@���@��D@�Z@�$�@�  @��
@���@���@�&�@��@�X@~��@q�@eV@^{@Q�@D�@:=q@0Q�@-��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BXB^5B`BB^5B^5B^5B^5B^5B��B��B��B��B��B�+B~�B}�B� B� B�=B�bB�uB��B��B�3B�jB�B�TB�B�B�B#�B�B2-B<jBA�B>wB>wB:^B5?B �B	7B�B�B�B
ȴB
k�B
(�B	��B	P�B	6FB	$�B	{B��B�;B��B��B��B��B��B�/B�5B�BB�ZB�
B��B�B�B�`B	  B	&�B	49B	F�B	T�B	bNB	m�B	�B	��B	�B	�-B	ŢB	��B	��B	��B	��B	�B	�BB	�B

=B
PB
�B
�B
�B
!�B
%�B
,B
,B
5?B
:^B
;dB
C�B
K�B
P�B
YB
bNB
jB
q�B
t�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B?�BE�BG�BE�BE�BE�BE�BF�B��B�SB��B��B��Bo�Bf�Be�Bg�Bh�Br|BxRB|6B��B�DB��B��B�B�xBԯB�#B �BdBB~B%`B)�B(�B+B$�B �B
#B�vB�Bo5B	�B
�B
UgB
�B	�B	;�B	 'B	pB��B�B�lB��B��B�BB�wB�BżB�EB�RB��B��B�B�B�oBοB�yB	�B	�B	/�B	=�B	KB	VB	m�B	~�B	�SB	��B	�B	�ZB	�JB	�^B	�XB	��B	��B	�B	�B	��B	��B	��B
B

	B
VB
FB
aB
�B
"�B
#�B
+�B
4B
9>B
AoB
J�B
R�B
Y�B
\�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES, where PRES is already adjusted with PRESSURE OFFSET; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                   TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & OW error , 0.01(PSS-78))                                                                                                                                 PO1=-0.3(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.023(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202211100416512022111004165120221110041651202210251812532022102518125320221025181253JA  ARFMdecpV4_b                                                                20220708070238  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220708070239  IP                  G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220708125614  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220711215631  IP                  G�O�G�O�G�O�                JA      jafc1.0                                                                 20220711215632                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220712065635  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220712065635  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220711215655                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220711151506  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220711151506  QCF$                G�O�G�O�G�O�700000          JM  ARGQJMQC2.0                                                                 20220711151506  CV  JULD_LOCATION   G�O�G�O�F��W                JM  ARCAJMTM1.0                                                                 20221025041329  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091253  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                