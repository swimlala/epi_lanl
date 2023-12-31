CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-07-18T06:53:05Z creation;2019-07-21T21:53:37Z conversion to V3.1;2022-07-26T02:45:15Z update;     
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
_FillValue                    ixArgo profile    3.1 1.2 19500101000000  20190718065305  20220818051506  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA  V4_131545_117                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @���lGM 1   @��6;�@4e�����c�-1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@���A(  Al��A�33A�33A���B��B%33B533BI��B_��Bp  B�ffB�33B�ffB���B�33B�ffB�  B�  B�ffBݙ�B�  B���B�ffC��C�C��C�3C� C33C �C$�fC)�3C.L�C4� C9�3C>��CC�CI  CQ�3C[� CfffCn�fCyffC�� C�33C��3C��3C�� C�&fC��C��C��C�33C�ٚC�ffC�Y�C³3C��fC��C��C�  C�s3C�L�C�&fC�&fC�Y�C��3C��3D  D3DfD��D33D��D�fD%fD*  D/�D3�fD8� D=�fDC  DH  DL�3DQ� DV��D\  Da�DeٚDk3Dp�Dt�fDy�fD�P D��3D���D�fD�@ D���D��fD�	�D�@ D�� D���D�3D�C3DԐ Dڹ�D� D�,�D퉚D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?fff@���A(  Al��A�33A�33A���B��B%33B533BI��B_��Bp  B�ffB�33B�ffB���B�33B�ffB�  B�  B�ffBݙ�B�  B���B�ffC��C�C��C�3C� C33C �C$�fC)�3C.L�C4� C9�3C>��CC�CI  CQ�3C[� CfffCn�fCyffC�� C�33C��3C��3C�� C�&fC��C��C��C�33C�ٚC�ffC�Y�C³3C��fC��C��C�  C�s3C�L�C�&fC�&fC�Y�C��3C��3D  D3DfD��D33D��D�fD%fD*  D/�D3�fD8� D=�fDC  DH  DL�3DQ� DV��D\  Da�DeٚDk3Dp�Dt�fDy�fD�P D��3D���D�fD�@ D���D��fD�	�D�@ D�� D���D�3D�C3DԐ Dڹ�D� D�,�D퉚D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A�t�A�p�A�jA�bNA�XAӃA�33A���A�l�A��PA��A�^5A�  A�5?A�$�A�VA��#A�+A��A�C�A�XA�\)A��A�O�A��HA��A���A���A�C�A�7LA�A���A�33A��7A��Az�RAu��AnbAk33AbA�AY�FAQdZAIx�AF(�A?dZA8v�A.�uA+oA$��Av�A��AĜA�^A
5?Ap�A �y@��@��\@�P@��
@ᙚ@��@�X@�  @�ƨ@��@�ȴ@°!@���@�C�@�j@�&�@��@��@��@��@�C�@���@�K�@�J@�?}@���@��w@��@��H@��D@�@�j@���@�&�@�C�@��/@�o@��@��h@z~�@q��@i�@c"�@[t�@Q��@I�7@A�@;��@3S�@,�D@&�R@ �`@Z@  @(�@�@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A�t�A�p�A�jA�bNA�XAӃA�33A���A�l�A��PA��A�^5A�  A�5?A�$�A�VA��#A�+A��A�C�A�XA�\)A��A�O�A��HA��A���A���A�C�A�7LA�A���A�33A��7A��Az�RAu��AnbAk33AbA�AY�FAQdZAIx�AF(�A?dZA8v�A.�uA+oA$��Av�A��AĜA�^A
5?Ap�A �y@��@��\@�P@��
@ᙚ@��@�X@�  @�ƨ@��@�ȴ@°!@���@�C�@�j@�&�@��@��@��@��@�C�@���@�K�@�J@�?}@���@��w@��@��H@��D@�@�j@���@�&�@�C�@��/@�o@��@��h@z~�@q��@i�@c"�@[t�@Q��@I�7@A�@;��@3S�@,�D@&�R@ �`@Z@  @(�@�@9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
�B
�B
��B
��B
��B+Bz�B�uB�^B�B�yB��B�B/B2-BM�BF�BB�B0!B@�B9XB49B"�BJB�ZB�}B��B�JBjBZB=qBB
�fB
��B
�7B
<jB	��B	�B	�B	��B	dZB	33B	B�NB��B�LB��B�hB�7B� Bz�Bs�Bp�BjBs�B�B�B}�B}�By�Bq�Bx�B�\B��B��B��B�}BɺB�)B�ZB	B	#�B	1'B	F�B	]/B	s�B	�7B	��B	��B	ÖB	��B	�B	�HB	�B	�B	��B	��B
  B
B
1B
DB
\B
uB
�B
�B
�B
'�B
/B
5?B
:^B
@�B
F�B
L�B
Q�B
XB
^5B
bNB
hsB
n�B
r�B
w�B
z�B
� B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�	B
�&B
�B
�B
�8B
��BBpUB�zB�5B̳B�]B�5B<B �B$@B@OB7�B4�B!�B2�B+QB&LB�B��BּB��B�BB\xBK�B1�B
��B
�yB
�B
~(B
1[B	�'B	��B	�'B	��B	W$B	%�B�8B��B�aB��B�uB�-B{�Br�BmwBezBb�B\�Be�Bv�Bv�Bo�Bo�Bk�BcnBjB�oB�0B�.B�7B��B�0B�B�gB�%B	�B	"hB	7�B	N"B	d�B	z*B	�xB	��B	�TB	��B	��B	� B	�WB	�bB	�B	�B	�B	��B	��B	�B
 4B
MB
_B
^B
}B
�B
�B
%�B
+B
1[B
7fB
=�B
B�B
H�B
N�B
SB
Y1B
_VB
cnB
h�B
k�B
p�B
t�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<*��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.015(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201908010015292019080100152920190801001529202207232057062022072320570620220723205706202207261126242022072611262420220726112624  JA  ARFMdecpV4_b                                                                20190718065304  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190718065305  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190718065306  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190718065306  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190718065307  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190718065307  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190718065814                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190721215255  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190721215335  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190721215335  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190721215336  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190721215336  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190721215337  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190721215337                      G�O�G�O�G�O�                JA  ARUP                                                                        20190721215709                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190722000000  CF  PSAL_ADJUSTED_QC?fff@���G�O�                JM  ARCAJMQC2.0                                                                 20190731151529  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190731151529  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115706  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022624  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051506                      G�O�G�O�G�O�                