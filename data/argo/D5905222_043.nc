CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-02T06:54:20Z creation;2019-08-05T21:52:55Z conversion to V3.1;2022-11-10T04:21:31Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190802065420  20221117231506  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  V4_131533_043                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @�ѻm� 1   @����(� @-ܬ1&��d��E�1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@���A��At��A���A�ffA�ffB��B"ffB8  BM33B^  Bs��B�ffB���B�ffB���B�  B�33B���B�ffBә�B�ffB癚B�  B���C��C�3C�fC�C��CL�C � C%ffC*  C/  C4� C9  C=�3CB�fCF��CQ��C\��Cf�fCp�3Cz  C��3C�� C�  C��C�ffC��3C�@ C�s3C�� C��3C�Y�C�Y�C�� C�  C�Y�C�ٚC�33C��fC�L�C�  C�33C�&fC��C�Y�C�  D  D9�D  D33D� D�D fD%9�D*fD/�D43D8ٚD>  DC&fDG�fDM,�DQ��DW3D\9�D`�3Df,�DkfDo��Dt�fDy� D�C3D���D��3D��3D�S3D��fD��fD��D�S3D���D��fD��fD�<�DԀ D���D�fD�S3D��D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@���A��At��A���A�ffA�ffB��B"ffB8  BM33B^  Bs��B�ffB���B�ffB���B�  B�33B���B�ffBә�B�ffB癚B�  B���C��C�3C�fC�C��CL�C � C%ffC*  C/  C4� C9  C=�3CB�fCF��CQ��C\��Cf�fCp�3Cz  C��3C�� C�  C��C�ffC��3C�@ C�s3C�� C��3C�Y�C�Y�C�� C�  C�Y�C�ٚC�33C��fC�L�C�  C�33C�&fC��C�Y�C�  D  D9�D  D33D� D�D fD%9�D*fD/�D43D8ٚD>  DC&fDG�fDM,�DQ��DW3D\9�D`�3Df,�DkfDo��Dt�fDy� D�C3D���D��3D��3D�S3D��fD��fD��D�S3D���D��fD��fD�<�DԀ D���D�fD�S3D��D��fD�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ĜA޴9Aޟ�A�l�A�ffA�`BA�\)A�\)A�ZA��/A���A��A�p�A��A�l�AŶFA�ƨAé�A��A��wA�r�A�5?A�O�A���A�A�I�A��\A�XA��A�z�A��`A�ĜA��\A�VA��PA�\)A|��Aw�Al{Ai�TA]oAO�;AA"�A9G�A2��A*�\A#�-A�\AĜA�^A��Az�A�^A-A��A
1'A��A��A��A�A��@��u@�E�@�@��@�j@�X@�1@�=q@�n�@�x�@ו�@җ�@�$�@�{@Ɵ�@�v�@��@���@���@��/@���@�$�@�A�@���@��^@�
=@�1@�~�@��9@��@��P@�G�@���@���@���@�r�@�;@tI�@j��@ct�@YX@Nff@Cƨ@;�
@4I�@.�R@(Ĝ@#��@��@�@��@�^@��@
J31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ĜA޴9Aޟ�A�l�A�ffA�`BA�\)A�\)A�ZA��/A���A��A�p�A��A�l�AŶFA�ƨAé�A��A��wA�r�A�5?A�O�A���A�A�I�A��\A�XA��A�z�A��`A�ĜA��\A�VA��PA�\)A|��Aw�Al{Ai�TA]oAO�;AA"�A9G�A2��A*�\A#�-A�\AĜA�^A��Az�A�^A-A��A
1'A��A��A��A�A��@��u@�E�@�@��@�j@�X@�1@�=q@�n�@�x�@ו�@җ�@�$�@�{@Ɵ�@�v�@��@���@���@��/@���@�$�@�A�@���@��^@�
=@�1@�~�@��9@��@��P@�G�@���@���@���@�r�@�;@tI�@j��@ct�@YX@Nff@Cƨ@;�
@4I�@.�R@(Ĝ@#��@��@�@��@�^@��@
J31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	_;B	bNB	bNB	bNB	bNB	bNB	cTB	dZB	cTB	cTB	n�B	~�B	�DB	��B	��B
 �B
��B=qBP�B_;B�XB�B�B�B�;B�
B�B��B��B�Bk�BT�B�B
�XB
e`B
{B	��B	�B	�B	T�B	G�B	JB��B��B�)B��B�B�B	+B	'�B	@�B	k�B	�B	��B	�B	�B	q�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�B	ɺB	��B	��B	�)B	�`B	�yB	�B	�B	�B	�B	��B	��B	��B	��B
B
B
B

=B
DB
PB
\B
oB
�B
�B
�B
%�B
+B
1'B
6FB
;dB
B�B
I�B
P�B
XB
]/B
bNB
e`B
jB
n�B
r�B
v�B
{�B
~�B
�B
�%31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	_VB	bhB	bhB	b�B	bhB	bhB	cnB	dtB	cnB	c�B	o�B	�B	�MB	��B	�B
"�B
��B?HBRTBb�B��BٴB��B�qB��BخB�B� B�EB�Bm�BXB&�B
��B
k�B
B	�&B	�B	�rB	V�B	J�B	�B��B� B�B�?B��B�TB	�B	(�B	A B	k�B	�;B	�sB	��B	�mB	qvB	�B	�\B	�dB	�_B	��B	��B	��B	��B	��B	�5B	�_B	��B	��B	�5B	�#B	�HB	�@B	�xB	�B	��B	��B	��B	��B	��B	��B	�B	�B	�<B
AB
MB
MB

XB
xB
�B
�B
�B
�B
�B
�B
&B
+6B
1[B
6zB
;B
B�B
I�B
QB
X+B
]IB
bhB
ezB
j�B
n�B
r�B
v�B
|B
.B
�'B
�%31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908160016062019081600160620190816001606202210251311482022102513114820221025131148202210251805572022102518055720221025180557  JA  ARFMdecpV4_b                                                                20190802065419  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190802065420  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190802065420  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190802065421  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190802065421  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190802065421  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190802065902                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190805215159  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190805215253  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190805215254  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190805215254  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190805215254  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190805215255  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190805215255                      G�O�G�O�G�O�                JA  ARUP                                                                        20190805215539                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20190805151552  QCP$                G�O�G�O�G�O�7DEB7C          JM  ARGQrqcjv291                                                                20190805151552  QCF$                G�O�G�O�G�O�700000          JM  ARCAJMQC2.0                                                                 20190815151606  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190815151606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041148  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025090557  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117231506                      G�O�G�O�G�O�                