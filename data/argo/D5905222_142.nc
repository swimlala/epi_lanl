CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-04-19T07:04:50Z creation;2022-04-22T19:01:27Z conversion to V3.1;2022-11-10T04:17:11Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20220419070450  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_142                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��{�� 1   @��}kT� @1 �n���d�-V1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?��@�ffA��Al��A���Ař�A�ffB��B   B8  BL  B`  Bo33B�  B�33B�ffB���B�33B�  B�33B�ffB�33B���B癚B�ffB�  C��C33C��C�3C� C�C33C$ffC)� C.� C3ffC9�3C>  CC� CHffCSL�C\  Ce�fCo��Cz��C�� C�33C��3C�L�C�&fC��C��C�� C��C��3C�ٚC�L�C�@ C�Y�C��fC�L�C��C��C��C�  C� C�ffC�Y�C�L�C�s3D��D��D�fD&fD3DٚD� D$�fD)�3D.��D3�3D8�fD>3DC�DG�3DM3DR  DW3D\fDa,�Df3Dk  Do��Du3Dz  D�L�D�vfD��3D���D�)�D�y�D��3D� D�I�D��3D��fD��3D�9�D�p D��3D�  D�C3D홚D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@�ffA��Al��A���Ař�A�ffB��B   B8  BL  B`  Bo33B�  B�33B�ffB���B�33B�  B�33B�ffB�33B���B癚B�ffB�  C��C33C��C�3C� C�C33C$ffC)� C.� C3ffC9�3C>  CC� CHffCSL�C\  Ce�fCo��Cz��C�� C�33C��3C�L�C�&fC��C��C�� C��C��3C�ٚC�L�C�@ C�Y�C��fC�L�C��C��C��C�  C� C�ffC�Y�C�L�C�s3D��D��D�fD&fD3DٚD� D$�fD)�3D.��D3�3D8�fD>3DC�DG�3DM3DR  DW3D\fDa,�Df3Dk  Do��Du3Dz  D�L�D�vfD��3D���D�)�D�y�D��3D� D�I�D��3D��fD��3D�9�D�p D��3D�  D�C3D홚D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA�=qA�I�A�I�A�E�A�C�A�;dA�;dA�5?A�7LA�;dA�7LA�7LA�-A���A�+A��yAϑhA�p�Aκ^A˩�Aɉ7Aȣ�A×�A�$�A���A�/A��A���A�M�A�t�A�7LA���A�&�A��TA�9XA�%A�A�A�~�A�
=A��A~��Ai�Ab�RAR�DAK|�ABbNA:~�A2�uA,1'A ��A!��A�FA\)A(�A�\A
�A7LAXAE�A�A�A�`@��H@��@���@��@�ȴ@��y@�dZ@�n�@�Ĝ@�C�@׶F@�G�@�9X@Ɵ�@�A�@�z�@��+@���@��;@��^@�o@�O�@�S�@��/@��F@�Ĝ@�$�@���@�;d@��@���@�I�@�hs@���@�%@w;d@l�@d�j@\Z@T��@JJ@>{@2�@-`B@%�@!&�@��@J@5?@ƨ@��@(�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA�=qA�I�A�I�A�E�A�C�A�;dA�;dA�5?A�7LA�;dA�7LA�7LA�-A���A�+A��yAϑhA�p�Aκ^A˩�Aɉ7Aȣ�A×�A�$�A���A�/A��A���A�M�A�t�A�7LA���A�&�A��TA�9XA�%A�A�A�~�A�
=A��A~��Ai�Ab�RAR�DAK|�ABbNA:~�A2�uA,1'A ��A!��A�FA\)A(�A�\A
�A7LAXAE�A�A�A�`@��H@��@���@��@�ȴ@��y@�dZ@�n�@�Ĝ@�C�@׶F@�G�@�9X@Ɵ�@�A�@�z�@��+@���@��;@��^@�o@�O�@�S�@��/@��F@�Ĝ@�$�@���@�;d@��@���@�I�@�hs@���@�%@w;d@l�@d�j@\Z@T��@JJ@>{@2�@-`B@%�@!&�@��@J@5?@ƨ@��@(�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B
B
  B
  B
B
B
B
B
B
B
B
%B
B
+B
�B�jB��BŢBÖB��B�B��B�BBDB'�B'�B+B,B�B!�B!�BPB��B�B�`B��B�Bn�B>wB �B
�5B
J�B	��B	��B	[#B	9XB	hB�B��B�)B�B	�B	+B	�B�B	0!B	33B	@�B	J�B	k�B	p�B	��B	��B	��B	��B	�9B	�dB	ƨB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�/B	�mB	�B	�B	��B	��B	��B
B
+B
PB
{B
�B
�B
#�B
&�B
+B
,B
/B
9XB
@�B
E�B
K�B
Q�B
XB
]/B
dZB
k�B
t�B
w�B
}�B
�B
�+B
�1B
�JB
�VB
�hB
��B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B
��B��B�KB�/B��B��B��B��B�dB�B B�BBeB1B�BjB�LB��B�]B�vB�jB�1B[#B(�BBB
�DB
9�B	��B	�B	E�B	$�B��B��B��BȀB��B	�B	�B	�BөB	�B	/B	*0B	49B	T�B	Y�B	�B	��B	�AB	��B	��B	��B	�UB	�ZB	��B	�XB	�XB	�DB	�B	��B	B	��B	�VB	�tB	��B	��B	��B	�B	�B	�>B	�QB	�B	��B	��B
�B
B
B
.B
,B
MB
_B
"�B
)�B
.�B
4�B
;B
A B
FtB
M�B
T�B
]�B
`�B
gB
l"B
p;B
qAB
uZB
wfB
zxB
~�B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.3(dbar); PO2=-0.3(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.022(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202205030015052022050300150520220503001505202210251313212022102513132120221025131321202210251812222022102518122220221025181222  JA  ARFMdecpV4_b                                                                20220419070450  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220419070450  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220419070450  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220419070451  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220419070451  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220419070451  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20220419070546                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220419155959  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220422190125  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220422190125  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220422190126  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220422190126  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220422190126  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220422190127                      G�O�G�O�G�O�                JA  ARUP                                                                        20220422190148                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220422151509  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220422151509  QCF$                G�O�G�O�G�O�300000          JM  ARGQJMQC2.0                                                                 20220422151508  CV  JULD_LOCATION   G�O�G�O�F�K�                JM  ARCAJMQC2.0                                                                 20220502151505  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220502151505  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041321  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091222  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                