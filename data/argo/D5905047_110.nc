CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-05-09T06:53:21Z creation;2019-05-12T21:53:56Z conversion to V3.1;2019-09-10T08:50:37Z update;2022-07-26T02:45:32Z update;     
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
resolution        =���     �  <`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  D�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  E`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  G,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Il   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  K8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    id   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    it   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ix   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190509065321  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               nA   JA  V4_131545_110                   2C  Dc��ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @ؼ{��1   @ؼ�-�� @2�z�G��c�Ƨ1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?333@�  A)��Ak33A�33Ař�A陚B  B!33B4��BJ  B`  Bp��B�33B�ffB���B�33B�ffB�ffB�ffB�33B�33B�ffB�33B�ffB�33C� C��C�3C33C� C��C ffC%ffC*�3C/33C4��C9��C>� CC��CH�CS�C[��Cf�fCo��Cz��C��fC�33C��3C���C��3C�Y�C��C�Y�C�Y�C�ٚC�L�C��C��C�&fCǙ�C��C��fC�� C�Y�C��C�fC��3C�@ C�s3C���D,�D  D�D��DfD33D �D%�D)�3D.�3D4�D8�3D>  DB��DH,�DM9�DR  DW&fD\3DafDf&fDj�fDp@ Dt��Dz�D�<�D���D��3D� D�Y�D���D��3D���D�33D��3D�� D��fD�VfDԀ D�ɚD�3D�FfD퉚D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111f?333@�  A)��Ak33A�33Ař�A陚B  B!33B4��BJ  B`  Bp��B�33B�ffB���B�33B�ffB�ffB�ffB�33B�33B�ffB�33B�ffB�33C� C��C�3C33C� C��C ffC%ffC*�3C/33C4��C9��C>� CC��CH�CS�C[��Cf�fCo��Cz��C��fC�33C��3C���C��3C�Y�C��C�Y�C�Y�C�ٚC�L�C��C��C�&fCǙ�C��C��fC�� C�Y�C��C�fC��3C�@ C�s3C���D,�D  D�D��DfD33D �D%�D)�3D.�3D4�D8�3D>  DB��DH,�DM9�DR  DW&fD\3DafDf&fDj�fDp@ Dt��Dz�D�<�D���D��3D� D�Y�D���D��3D���D�33D��3D�� D��fD�VfDԀ D�ɚD�3D�FfD퉚D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�-A�M�A�A���A�ȴA��A�ĜA�^5A�I�A�XA�/A�"�A˴9A�p�A�7LA���A���Aė�A�~�A²-A�ZA��PA��mA���A�ZA�ZA���A��;A��A��A�ZA��;A���A��yA��hA�dZA�^5A�A�G�A��jA�I�A��#A~z�Ao`BA`��AV�AM�AB5?A=�A7VA/|�A%��A`BA��A��AM�A
�A	G�A��@�\)@�t�@�x�@��P@��@�V@���@�"�@��#@ָR@ʟ�@��;@°!@¸R@�
=@�?}@�b@��F@��D@�hs@��T@�|�@���@�M�@�`B@�V@���@�A�@���@�@���@�ff@��@���@���@��y@�p�@���@xĜ@n{@a�#@X�@R��@L�j@D�D@=V@5`B@0Q�@*�@&5?@!�@��@�9@ƨ@��@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�A�-A�M�A�A���A�ȴA��A�ĜA�^5A�I�A�XA�/A�"�A˴9A�p�A�7LA���A���Aė�A�~�A²-A�ZA��PA��mA���A�ZA�ZA���A��;A��A��A�ZA��;A���A��yA��hA�dZA�^5A�A�G�A��jA�I�A��#A~z�Ao`BA`��AV�AM�AB5?A=�A7VA/|�A%��A`BA��A��AM�A
�A	G�A��@�\)@�t�@�x�@��P@��@�V@���@�"�@��#@ָR@ʟ�@��;@°!@¸R@�
=@�?}@�b@��F@��D@�hs@��T@�|�@���@�M�@�`B@�V@���@�A�@���@�@���@�ff@��@���@���@��y@�p�@���@xĜ@n{@a�#@X�@R��@L�j@D�D@=V@5`B@0Q�@*�@&5?@!�@��@�9@ƨ@��@
�\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�B�/B	%�B	(�B	0!B	8RB	C�B	W
B	YB	]/B	iyB	hsB	�B	�B
B
&�B
iyB
�LB
��B8RBn�B��BȴBBbBB,B0!B;dBB�B1'B  B��B��BɺB��B�=B� BhsB:^BVB
�B
�%B
(�B	��B	dZB	!�B	hB�5BƨB��B��B�7B�bB�hB�uB��B��B��B�B�B��B�B�dBɺB�RB�B�5B	  B�B�'BĜB�B	oB	�B	&�B	F�B	[#B	l�B	�B	�\B	��B	�!B	ǮB	��B	�B	�BB	�`B	�B	��B	��B
B
1B
bB
�B
�B
�B
#�B
)�B
0!B
9XB
?}B
E�B
I�B
N�B
T�B
[#B
`BB
e`B
iyB
m�B
s�B
w�B
|�B
�B
�%1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�BЗB	�B	�B	"B	*B	5�B	IB	J�B	N�B	[qB	[=B	v�B	��B	�B
B
\�B
��B
�WB*�Bb4B�
B��B��BGB�RB�B#TB0B5�B%zB�LBªB�YB�(B��B|�Bs�B]~B.cB�B
�PB
{B
5B	�fB	X�B	�B	�B�hB��B��B�B}"B��B��B�?B��B��B��BtBu�B��B�IB��B�B��B̳B�}B�hB��B��B��BݲB	�B	B	KB	8�B	MjB	^�B	s3B	�oB	��B	�NB	��B	��B	�1B	�TB	�sB	ߊB	��B	��B	�B	�DB
uB
zB
�B
�B
�B
B
"4B
+kB
1vB
7�B
;�B
@�B
F�B
MB
R:B
W?B
[qB
_�B
e�B
i�B
n�B
r�B
x3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111�@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.014(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201905230015282019052300152820190523001528202207232057002022072320570020220723205700202207261125562022072611255620220726112556  JA  ARFMdecpV4_b                                                                20190509065320  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190509065321  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190509065322  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190509065322  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190509065322  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190509065323  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190509065950                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190512215237  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190512215354  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190512215354  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190512215355  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190512215355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190512215355  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190512215356                      G�O�G�O�G�O�                JA  ARUP                                                                        20190512215720                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190513000000  CF  PSAL_ADJUSTED_QC?333?333G�O�                JM  ARCAJMQC2.0                                                                 20190522151528  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190522151528  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190523151457  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115700  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022556  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                