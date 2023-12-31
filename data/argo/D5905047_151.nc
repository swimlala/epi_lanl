CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-06-23T06:52:32Z creation;2020-06-26T21:53:02Z conversion to V3.1;2022-07-26T02:43:53Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20200623065232  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_151                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�#;��� 1   @�#G�:� @3E�Q��c[��S��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�ffA+33At��A���A�33A�ffBffB"��B333BHffB\ffBp  B���B���B���B���B���B�  B�  B�ffB�ffB�ffB���B�33B�33C  CffC�fC��C�fC�fC�fC%L�C*�3C0  C3�fC:  C?  CB�fCG�fCQ� C\33Ce� Cp�CzffC�ffC�&fC��C�@ C��fC���C��3C���C�33C�@ C��C�ٚC�ffC�33C�33C��C�&fC�&fC��C��C�&fC�Y�C�  C��3C�&fD�3D�D33D@ D�D  DٚD$� D)��D.� D43D8��D>fDC  DH  DM&fDR�DW3D\3Da�Df  Dk�Dp,�Dt��Dz9�D�@ D���D�� D��D�P D�� D��3D���D�C3D���D�� D�fD�9�D�|�D�� D�  D�<�D�3D�� D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?���@�ffA+33At��A���A�33A�ffBffB"��B333BHffB\ffBp  B���B���B���B���B���B�  B�  B�ffB�ffB�ffB���B�33B�33C  CffC�fC��C�fC�fC�fC%L�C*�3C0  C3�fC:  C?  CB�fCG�fCQ� C\33Ce� Cp�CzffC�ffC�&fC��C�@ C��fC���C��3C���C�33C�@ C��C�ٚC�ffC�33C�33C��C�&fC�&fC��C��C�&fC�Y�C�  C��3C�&fD�3D�D33D@ D�D  DٚD$� D)��D.� D43D8��D>fDC  DH  DM&fDR�DW3D\3Da�Df  Dk�Dp,�Dt��Dz9�D�@ D���D�� D��D�P D�� D��3D���D�C3D���D�� D�fD�9�D�|�D�� D�  D�<�D�3D�� D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A���Aˣ�A˓uA˓uAˏ\A˃A�C�A�/A��A���A�ZAȰ!AƧ�A���A�7LA�r�A��HA�t�A�{A���A��HA��7A���A�(�A���A�
=A�C�A��;A�dZA�C�A�(�A�+A�1'A��A�9XA�oA�$�A�{A��A���A�+A�-Au�FAg7LAZjAK��AB^5A>M�A6Q�A2�!A)p�A#�hA �9A��AJAE�A-A��A�\A;dA��A��A�A  A�h@�M�@���@���@�|�@�bN@և+@�Z@���@��T@ɲ-@ģ�@�  @��^@�dZ@�v�@��
@�O�@��@�O�@��j@�t�@�=q@�z�@�-@��D@��@���@�33@���@�33@�5?@���@|��@tz�@h�`@a��@Y��@R~�@JJ@@r�@:-@4I�@.��@)�^@%�-@|�@@�@n�@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��;A���Aˣ�A˓uA˓uAˏ\A˃A�C�A�/A��A���A�ZAȰ!AƧ�A���A�7LA�r�A��HA�t�A�{A���A��HA��7A���A�(�A���A�
=A�C�A��;A�dZA�C�A�(�A�+A�1'A��A�9XA�oA�$�A�{A��A���A�+A�-Au�FAg7LAZjAK��AB^5A>M�A6Q�A2�!A)p�A#�hA �9A��AJAE�A-A��A�\A;dA��A��A�A  A�h@�M�@���@���@�|�@�bN@և+@�Z@���@��T@ɲ-@ģ�@�  @��^@�dZ@�v�@��
@�O�@��@�O�@��j@�t�@�=q@�z�@�-@��D@��@���@�33@���@�33@�5?@���@|��@tz�@h�`@a��@Y��@R~�@JJ@@r�@:-@4I�@.��@)�^@%�-@|�@@�@n�@E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	XB	[#B	\)B	^5B	_;B	`BB	aHB	iyB	iyB	hsB	e`B	]/B	N�B	<jB	ZB	jB	�hB	��B	�fB
�B
>wB
��B
�B
�B=qBiyB�\B~�Bv�Bu�Bl�BffBgmBW
BH�B;dB33B+B1B
�B
�B
� B
�B	B	T�B	1B�
B��BĜBɺB��B�B�#B�wB�)B	oB		7B	uB	'�B	=qB	N�B	O�B	L�B	2-B	7LB	/B	)�B	�B	DB��B	DB	B	PB	=qB	XB	r�B	�1B	��B	��B	��B	��B	�-B	�jB	��B	��B	��B	��B	��B	�NB	�NB	�B	��B
B
%B

=B
JB
{B
�B
�B
/B
5?B
@�B
E�B
N�B
VB
`BB
dZB
jB
m�B
q�B
t�B
x�B
}�B
�B
�%B
�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	B�B	E�B	F�B	H�B	I�B	J�B	LB	TB	TB	S@B	PbB	I7B	;JB	)�B	ESB	W
B	}�B	�OB	�B
�B
*�B
��B
� B
�IB($BT�B|�Bl�BcnBcnBX�BR�BS[BCaB49B'B�B+B
�%B
�B
�+B
nB
�B	��B	B�B��B�gB��B�vB��B�HB�YBƎB�KB�B�(B��B��B	[B	(�B	:*B	;B	8�B	�B	"�B		B	B	gB��B�KB��B�!B�RB	(sB	CB	]�B	sMB	�{B	��B	�B	��B	�B	�mB	��B	�B	��B	��B	��B	�6B	�6B	�MB	�B	��B	�B	�B	�2B	�cB

�B
�B
B
 B
+6B
0;B
9�B
@�B
J�B
OB
U2B
X+B
\]B
_pB
c�B
h�B
m�B
p�B
u�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9995(+-0.0000), deepest deltaS=-0.021(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202007070015292020070700152920200707001529202207232057362022072320573620220723205736202207261128352022072611283520220726112835  JA  ARFMdecpV4_b                                                                20200623065231  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200623065232  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200623065232  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200623065233  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200623065233  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200623065233  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200623065331                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200626215228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200626215300  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200626215300  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200626215301  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200626215301  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200626215301  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200626215302                      G�O�G�O�G�O�                JA  ARUP                                                                        20200626215346                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200627000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20200706151529  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200706151529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115736  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022835  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                