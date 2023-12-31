CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-21T06:52:25Z creation;2019-09-24T21:53:37Z conversion to V3.1;2022-11-10T04:21:17Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190921065225  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  V4_131533_048                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��:�d  1   @��GƊ� @.�7Kƨ�d�\)1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @���A1��As33A�33A�33A�BffB33B533BI��B]33BrffB�33B���B���B�  B�  B�33B���B���B�  B�  B䙚B�ffB�33C33C�3C�CL�CL�CffC � C%�3C)�fC.ffC4ffC9  C>�3CC��CH� CR��C\�Ce�fCpffCz�3C�Y�C�&fC�ٚC��C���C��3C�� C�ffC�� C���C�Y�C�� C�� C�  C�s3C�ffC��3C�� C�  C�ٚC��3C�33C�3C�&fC��3DٚD3D33DfD��D,�D 3D%3D*fD/  D4,�D9�D>fDC&fDG��DM�DR3DV��D[�fD`ٚDe��Dk&fDp33Du  Dz33D�VfD���D�� D��D�VfD��3D�ɚD�fD�C3D���D�� D��fD�C3DԆfD��3D��3D�S3D�p D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @���A1��As33A�33A�33A�BffB33B533BI��B]33BrffB�33B���B���B�  B�  B�33B���B���B�  B�  B䙚B�ffB�33C33C�3C�CL�CL�CffC � C%�3C)�fC.ffC4ffC9  C>�3CC��CH� CR��C\�Ce�fCpffCz�3C�Y�C�&fC�ٚC��C���C��3C�� C�ffC�� C���C�Y�C�� C�� C�  C�s3C�ffC��3C�� C�  C�ٚC��3C�33C�3C�&fC��3DٚD3D33DfD��D,�D 3D%3D*fD/  D4,�D9�D>fDC&fDG��DM�DR3DV��D[�fD`ٚDe��Dk&fDp33Du  Dz33D�VfD���D�� D��D�VfD��3D�ɚD�fD�C3D���D�� D��fD�C3DԆfD��3D��3D�S3D�p D��3D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�VA�  A�  A�ȴA�wA�!A�\A�uA�A�bNA�/A���A�A��TA���A�hsA���A��A��TA��jA�33A���A��A���A�v�A� �A�  A��A��Az��Ak�^Ab��A[AX��AS?}AN��AHZAB1A@��A<�A4��A/\)A+K�A&�RA�
A�A��A�9AG�A
��A^5A�AhsA�A��A ��@��@�hs@��@�o@���@���@��y@�K�@�7@�+@�\)@�@�1'@۾w@ա�@ЋD@��@��/@�Ĝ@�x�@�5?@���@���@��@��@�o@��\@���@���@��7@�o@���@�n�@��@���@��@��j@���@���@��
@\)@w�@k��@d�@\j@R��@I��@CS�@;�F@4�j@-�T@(Q�@"�!@(�@�@x�@v�@
n�@;d31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�G�A�VA�  A�  A�ȴA�wA�!A�\A�uA�A�bNA�/A���A�A��TA���A�hsA���A��A��TA��jA�33A���A��A���A�v�A� �A�  A��A��Az��Ak�^Ab��A[AX��AS?}AN��AHZAB1A@��A<�A4��A/\)A+K�A&�RA�
A�A��A�9AG�A
��A^5A�AhsA�A��A ��@��@�hs@��@�o@���@���@��y@�K�@�7@�+@�\)@�@�1'@۾w@ա�@ЋD@��@��/@�Ĝ@�x�@�5?@���@���@��@��@�o@��\@���@���@��7@�o@���@�n�@��@���@��@��j@���@���@��
@\)@w�@k��@d�@\j@R��@I��@CS�@;�F@4�j@-�T@(Q�@"�!@(�@�@x�@v�@
n�@;d31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�B�B�B�B�?B�
B	7LB	J�B	K�B	�B
�B	�dB	��B
#�B
VB
�sB
�B
�#B
�qB
��B
��B
�wB
|�B
e`B
�B
B�B	�HB	ȴB	�jB	hsB	T�B	0!B	%�B	�B	B�mB��B��BB�RBÖB�B��B�#B	�B	DB�B�B�B��B	�B	0!B	D�B	L�B	S�B	e`B	e`B	|�B	��B	��B	�B	�9B	�qB	�}B	ĜB	ƨB	ɺB	��B	��B	ɺB	��B	�
B	�)B	�BB	�`B	�mB	�B	�B	��B	��B	��B
B
B
B
1B
JB
VB
bB
oB
�B
�B
�B
�B
�B
)�B
5?B
:^B
?}B
C�B
H�B
M�B
T�B
YB
`BB
dZB
iyB
l�B
p�B
u�B
{�B
~�B
�B
�%B
�731111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�,B�"B�)B�QB�)B�5B�tB�?B	6�B	J�B	LdB	�YB
%FB	ǮB	��B
'�B
\)B
�B
��B
ݲB
�'B
��B
��B
ÖB
��B
h�B
��B
H�B	�&B	�^B	�{B	l�B	X�B	1�B	)B	sB	�B�KB��B�BĜB�B��B�?B��B�B	�B	B�3B�]B�aB��B	kB	0;B	E9B	MjB	T�B	f�B	e�B	|�B	�IB	�-B	�]B	��B	��B	��B	�B	��B	��B	��B	�PB	�#B	�MB	�YB	�xB	��B	�B	�B	��B	��B	��B	�B	�PB
;B
�B
SB
fB
~B
�B
}B
�B
�B
�B
�B
�B
�B
*B
5ZB
:�B
?�B
C�B
H�B
M�B
UB
YB
`\B
dtB
i�B
l�B
p�B
u�B
|B
B
�'B
�?B
�731111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910050016082019100500160820191005001608202210251311522022102513115220221025131152202210251806152022102518061520221025180615  JA  ARFMdecpV4_b                                                                20190921065224  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190921065225  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190921065226  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190921065227  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190921065227  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190921065227  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190921065519                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190924215259  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190924215335  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190924215335  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190924215336  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190924215336  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190924215336  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190924215337                      G�O�G�O�G�O�                JA  ARUP                                                                        20190924215422                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20190924151605  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20190924151605  QCF$                G�O�G�O�G�O�700000          JM  ARCAJMQC2.0                                                                 20191004151608  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191004151608  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041152  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090615  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                