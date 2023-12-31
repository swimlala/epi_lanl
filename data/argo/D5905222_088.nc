CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-10-26T06:52:30Z creation;2020-10-29T21:52:48Z conversion to V3.1;2022-11-10T04:19:35Z update;     
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
resolution        =���     �  <D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ED   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  IP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Mx   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Vx   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  hx   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    iH   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    iX   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         il   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ip   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        it   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ixArgo profile    3.1 1.2 19500101000000  20201026065230  20221117231507  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               XA   JA  V4_131533_088                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�B{_b:�1   @�B��^Ѐ@0)x����d=�Q�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@���A)��A{33A�33A�ffA�B	��B!��B3��BF  BX��BrffB�  B���B���B�  B�ffB�ffB�33B�  B�ffB�33B�ffBB���C�fCffC�C33C33CL�C � C$�3C*� C.��C433C8��C=L�CCffCH  CQ��C[�3CfffCp��CyL�C��C�&fC��C�� C�@ C�&fC�33C�ffC��C��3C�  C�33C�� C�@ C�@ C��C�&fC�ٚC�L�C� C��3C�&fC�L�C��C��fD,�D�3D3D�3D�D�D 9�D%,�D*,�D/,�D433D8�3D>3DB��DG�3DM3DR@ DW&fD\�Da,�De��Dk�Do��Dt�3Dz�D�L�D���D�� D���D�VfD��fD�� D�3D�33D�|�D�ɚD��3D�0 D�s3D�� D���D�I�D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?���@���A)��A{33A�33A�ffA�B	��B!��B3��BF  BX��BrffB�  B���B���B�  B�ffB�ffB�33B�  B�ffB�33B�ffBB���C�fCffC�C33C33CL�C � C$�3C*� C.��C433C8��C=L�CCffCH  CQ��C[�3CfffCp��CyL�C��C�&fC��C�� C�@ C�&fC�33C�ffC��C��3C�  C�33C�� C�@ C�@ C��C�&fC�ٚC�L�C� C��3C�&fC�L�C��C��fD,�D�3D3D�3D�D�D 9�D%,�D*,�D/,�D433D8�3D>3DB��DG�3DM3DR@ DW&fD\�Da,�De��Dk�Do��Dt�3Dz�D�L�D���D�� D���D�VfD��fD�� D�3D�33D�|�D�ɚD��3D�0 D�s3D�� D���D�I�D� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A��A�bA��A���A���A�ƨA�hsA۝�A�|�A�7LA�{AڸRA�33A�9XA��yAؗ�A��`AѶFA��A�bA�AƩ�A��`A��A��A�K�A��+A��uA�n�A��mA�l�A���A�K�A��#A�G�A�M�A��A��A�Q�A�/A�?}A�\)Aq�Aa�wAX-AL��AE�hA8�DA0VA'��A%p�A�A��A/A�A�FA��A
�uA�Ax�AƨAJ@��m@��/@�+@�t�@��m@�7L@�@�D@���@��H@�t�@���@�ȴ@�n�@���@Ə\@�^5@�"�@�-@���@�G�@�Ĝ@�bN@�~�@�Q�@�G�@��@�@���@���@���@��@��@�@���@��R@|(�@l�@e�T@_�@V{@M��@C��@=O�@2��@-?}@%p�@ ��@��@��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A��A�bA��A���A���A�ƨA�hsA۝�A�|�A�7LA�{AڸRA�33A�9XA��yAؗ�A��`AѶFA��A�bA�AƩ�A��`A��A��A�K�A��+A��uA�n�A��mA�l�A���A�K�A��#A�G�A�M�A��A��A�Q�A�/A�?}A�\)Aq�Aa�wAX-AL��AE�hA8�DA0VA'��A%p�A�A��A/A�A�FA��A
�uA�Ax�AƨAJ@��m@��/@�+@�t�@��m@�7L@�@�D@���@��H@�t�@���@�ȴ@�n�@���@Ə\@�^5@�"�@�-@���@�G�@�Ĝ@�bN@�~�@�Q�@�G�@��@�@���@���@���@��@��@�@���@��R@|(�@l�@e�T@_�@V{@M��@C��@=O�@2��@-?}@%p�@ ��@��@��@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
9XB
A�B
A�B
?}B
?}B
?}B
>wB
8RB
49B
7LB
2-B
2-B
+B
$�B
�B
�B
�B
VB
#�B
@�B
%�B
8RB
l�B
n�B
�BoB,B,BQ�B�VB�B��B�#B�B��B��B  B�B�HB��B�\B
�B
p�B	��B	n�B	\)B	$�B��B��BȴB��B	&�B	"�B	8RB	P�B	l�B	e`B	G�B	G�B	[#B	dZB	s�B	|�B	�bB	��B	��B	��B	��B	�B	�dB	��B	ɺB	��B	�B	�/B	�BB	�`B	�mB	�B	�yB	�B	�B	��B	��B
B
	7B
JB
PB
bB
uB
{B
�B
�B
�B
 �B
&�B
.B
33B
:^B
?}B
H�B
L�B
P�B
VB
\)B
bNB
ffB
o�B
r�B
w�B
{�B
~�B
�%B
�DB
�V3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
(
B
/�B
0B
-�B
-�B
-�B
-CB
'mB
"�B
%�B
 �B
 �B
B
,B
	lB
�B
B
 �B
�B
2B
�B
*eB
^5B
^�B
��B[B)BCBB'B~B�]B��B�B�B�B��B��B��B�oB�B�aB
ʦB
dtB	ŢB	`BB	M�B	�B�)B��B�B��B	�B	�B	(XB	?�B	\B	U�B	72B	7�B	JXB	S�B	cB	lWB	}B	��B	�B	�$B	�B	�OB	�eB	��B	��B	��B	�+B	�0B	�BB	�aB	�mB	�B	�yB	��B	�B	�B	�B	�B	�8B	�0B	�6B	�.B
AB
aB
�B

�B
�B
�B
�B
�B
"B
)_B
.cB
7�B
;B
?�B
D�B
KB
QB
UMB
^�B
a|B
f�B
j�B
m�B
t�B
zB
}3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.017(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202011090015562020110900155620201109001556202210251312312022102513123120221025131231202210251808512022102518085120221025180851  JA  ARFMdecpV4_b                                                                20201026065229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201026065230  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201026065230  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201026065231  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201026065231  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201026065231  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20201026065339                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20201029215200  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201029215246  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201029215246  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201029215248  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201029215248  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201029215248  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201029215248                      G�O�G�O�G�O�                JA  ARUP                                                                        20201029215351                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20201029151551  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20201029151551  QCF$                G�O�G�O�G�O�300000          JM  ARCAJMQC2.0                                                                 20201108151556  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201108151556  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041231  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090851  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231507                      G�O�G�O�G�O�                