CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-24T06:53:06Z creation;2020-01-27T21:53:38Z conversion to V3.1;2022-07-26T02:44:29Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200124065306  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_136                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��{fff�1   @����T� @4ڟ�vȴ�cn���m1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@���A+33A{33A�33A�  A�33B
  B ��B2  BH��B^��Bo��B���B�  B���B�ffB�33B�33B�  B���B�33B�ffB晚B���B�ffCL�C�C  C  C�fC�3C �fC%  C*�3C/L�C4�C9  C=��CB��CG��CQ��C\  Ce�fCp�fC{�C��3C�L�C���C���C��C��C�&fC�33C�L�C�L�C���C�Y�C�&fC�&fC��C���C�@ C��C�s3C�fC��3C�&fC���C�L�C��D  D�D  D��D,�DfD��D$�3D)�3D/,�D4�D8�3D=��DB�3DG�3DM&fDR�DV�fD[��Da33Df,�Dj�fDp  Du�Dz�D�VfD�� D��3D��D�VfD���D�� D��fD�6fD��3D�� D��D�L�Dԃ3D�� D�	�D�9�D�fD��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?L��@���A,��A|��A�  A���A�  B
ffB!33B2ffBI33B_33Bp  B�  B�33B���B���B�ffB�ffB�33B�  B�ffBܙ�B���B�  B���CffC33C�C�C  C��C!  C%�C*��C/ffC433C9�C=�fCB�fCG�fCQ�fC\�Cf  Cq  C{33C�  C�Y�C�ٚC��fC��C�&fC�33C�@ C�Y�C�Y�C�ٚC�ffC�33C�33C�&fC�ٚC�L�C��C܀ C�3C�  C�33C�ٚC�Y�C�&fD&fD3DfD  D33D�D   D$ٚD)ٚD/33D43D8��D=�3DB��DGٚDM,�DR3DV��D[�3Da9�Df33Dj��Dp&fDu  Dz  D�Y�D��3D��fD��D�Y�D�� D��3D���D�9�D��fD��3D� D�P DԆfD��3D��D�<�D홚D��fD�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A˛�A�ZA�Q�A�E�A�G�A�G�A�K�A�E�A�E�A�C�A�C�A�E�A�A�A�C�A�?}A�C�A�?}A�9XA���Aɡ�AȰ!A��`A�dZA�A�A�C�A�~�A��A��jA�9XA��A���A�33A�
=A��A���A���A���A���A��-A��!A�dZAw�Am��A_"�AY�AT�ALn�ACA=`BA7S�A/
=A+x�A(�+A%"�A�9AG�A�A-A�
A��Az�@��@��@�@�!@߮@���@�{@Ұ!@�X@�X@�+@�(�@\@�%@��m@��P@���@��@���@�`B@��`@�ȴ@�$�@��@��@�  @�5?@�(�@�hs@���@���@�  @��y@��#@�n�@~�+@t��@kt�@b=q@Y�@Q��@K�
@DI�@@r�@8��@3�@-��@+@$z�@   @1'@�7@z�@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A˛�A�ZA�Q�A�E�A�G�A�G�A�K�A�E�A�E�A�C�A�C�A�E�A�A�A�C�A�?}A�C�A�?}A�9XA���Aɡ�AȰ!A��`A�dZA�A�A�C�A�~�A��A��jA�9XA��A���A�33A�
=A��A���A���A���A���A��-A��!A�dZAw�Am��A_"�AY�AT�ALn�ACA=`BA7S�A/
=A+x�A(�+A%"�A�9AG�A�A-A�
A��Az�@��@��@�@�!@߮@���@�{@Ұ!@�X@�X@�+@�(�@\@�%@��m@��P@���@��@���@�`B@��`@�ȴ@�$�@��@��@�  @�5?@�(�@�hs@���@���@�  @��y@��#@�n�@~�+@t��@kt�@b=q@Y�@Q��@K�
@DI�@@r�@8��@3�@-��@+@$z�@   @1'@�7@z�@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
YB
bNB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
p�B
ĜB�B��B��B�TB�yB�B�fB�TB�)BBJB�B��B�Bw�BdZBoB
�B
�NB
�oB
49B	�B	�JB	iyB	K�B	$�B	  B�B��B�FB��B��B�oB�B{�BjBR�BF�B?}BL�BQ�BT�B]/Bm�Bw�B�%B�{B�B�qB�B�`B��B	B	VB	 �B	>wB	m�B	~�B	��B	��B	��B	�FB	B	ȴB	��B	��B	�#B	�ZB	�B	�B	�B	�B	��B
B
+B
oB
�B
%�B
:^B
>wB
E�B
H�B
N�B
T�B
\)B
aHB
e`B
gmB
m�B
r�B
x�B
� B
�B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
FtB
OvB
NVB
NVB
OBB
N<B
OBB
OBB
OBB
N"B
N<B
N"B
N<B
N<B
N<B
N<B
N<B
NVB
N�B
P.B
^�B
��B�B�gB��B�TB�B�_B�B� B�~B�3B�B�B��B��BfBV�BoB
��B
ѷB
��B
#�B	�'B	{JB	XEB	:�B	�B�B�qBÖB��B�B��B��BsMBj0BY�BA�B5ZB.B;�B@�BD3BKxB[qBezBtB��B�KB�BżB�&B�NB�B�B	<B	,B	[	B	l�B	�KB	��B	��B	��B	��B	�FB	�PB	�iB	ȚB	ѷB	��B	�B	�;B	��B	�B	��B	�nB	��B

	B
@B
'�B
+�B
2�B
6B
<B
B[B
IlB
N�B
R�B
T�B
Z�B
`B
f2B
m]B
q[B
vz31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.018(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202002070015372020020700153720200207001537202207232057232022072320572320220723205723202207261127372022072611273720220726112737  JA  ARFMdecpV4_b                                                                20200124065305  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200124065306  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200124065306  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200124065307  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200124065307  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200124065308  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200124065434                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200127215304  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200127215336  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200127215337  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200127215337  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200127215337  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200127215338  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200127215338                      G�O�G�O�G�O�                JA  ARUP                                                                        20200127215423                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200128000000  CF  PSAL_ADJUSTED_QC?333?333G�O�                JM  ARCAJMQC2.0                                                                 20200206151537  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200206151537  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022737  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                