CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-07-13T06:56:58Z creation;2020-07-16T21:53:17Z conversion to V3.1;2022-07-26T02:43:48Z update;     
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
_FillValue                  p  ;�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  <0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  @   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  BD   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Dp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   U�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  g�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    hP   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    hT   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    hX   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         h�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        h�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    h�Argo profile    3.1 1.2 19500101000000  20200713065658  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_153                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�(<�X�1   @�(F`� @3�I�^�cZfffff1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@�  A&ffA|��A���A�  A���B��B$  B533BK��B^��Bo33B�  B���B�  B���B���B�ffB�ffBə�Bҙ�B���B癚BB�33C��C��C��C� C��C�C �C%�C*  C/��C4��C8�fC>ffCC��CG�fCQ��C\�3Ce�fCo��Cz�3C�@ C���C�� C�ٚC��C�ffC��C�&fC�� C�  C�Y�C��C��fC�@ C�33C�33C�  C׀ C�&fC�s3C���C�� C�&fC��3C�Y�D��D��D  DfD�3D�3D fD%�D*9�D.��D43D9�D>@ DB��DH�DLٚDRfDW3D\33Da&fDe�fDj�3Dp�Du33Dz  D�I�D��fD�ɚD��D�Y�D��3D�� D���D��D�� D�� D�fD�C3DԌ�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�ff@�  A&ffA|��A���A�  A���B��B$  B533BK��B^��Bo33B�  B���B�  B���B���B�ffB�ffBə�Bҙ�B���B癚BB�33C��C��C��C� C��C�C �C%�C*  C/��C4��C8�fC>ffCC��CG�fCQ��C\�3Ce�fCo��Cz�3C�@ C���C�� C�ٚC��C�ffC��C�&fC�� C�  C�Y�C��C��fC�@ C�33C�33C�  C׀ C�&fC�s3C���C�� C�&fC��3C�Y�D��D��D  DfD�3D�3D fD%�D*9�D.��D43D9�D>@ DB��DH�DLٚDRfDW3D\33Da&fDe�fDj�3Dp�Du33Dz  D�I�D��fD�ɚD��D�Y�D��3D�� D���D��D�� D�� D�fD�C3DԌ�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A϶FAϝ�Aϛ�A���A�VA��A�
=A���Aω7A�hsA��A�-A���A¼jA��jA��/A�A�C�A�;dA�+A���A���A�K�A��A�l�A���A� �A���A��;A��hA��A��A�O�A�5?A��A�|�A���A���A���A���A��A��RA�{AzbNAq�wAa�ASK�AG�AA�7A7`BA.�A)�FA%/AE�AQ�A��A  A��A33A�AĜ@�ȴ@�E�@�bN@�Q�@�bN@�+@���@�/@�v�@�A�@̼j@�@Ɨ�@�&�@�7L@��@���@�Q�@��@��@�J@�ƨ@�K�@�t�@���@��@��@��@��+@��^@�n�@�/@��D@�|�@�j@�7L@y�#@t�@q�#@j�!@e�h@T�j@J�@F@:��@0r�@+dZ@*-@"�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�{A϶FAϝ�Aϛ�A���A�VA��A�
=A���Aω7A�hsA��A�-A���A¼jA��jA��/A�A�C�A�;dA�+A���A���A�K�A��A�l�A���A� �A���A��;A��hA��A��A�O�A�5?A��A�|�A���A���A���A���A��A��RA�{AzbNAq�wAa�ASK�AG�AA�7A7`BA.�A)�FA%/AE�AQ�A��A  A��A33A�AĜ@�ȴ@�E�@�bN@�Q�@�bN@�+@���@�/@�v�@�A�@̼j@�@Ɨ�@�&�@�7L@��@���@�Q�@��@��@�J@�ƨ@�K�@�t�@���@��@��@��@��+@��^@�n�@�/@��D@�|�@�j@�7L@y�#@t�@q�#@j�!@e�h@T�j@J�@F@:��@0r�@+dZ@*-@"�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	F�B	cTB	hsB	m�B	}�B	�?B	�
B	�B	�B	��B	�B	�B
oB
A�B
XB
ffB
z�B
� B
��B
�B%B&�BF�BR�B^5B�B��B�3B��B��B�ZB�NB�yB�mB�TB��B�^B�'B��B�bBx�BoB
�mB
<jB	�TB	��B	7LB��B��B�jB�'BĜB�B�)BĜB�ZB�5B��B��B��BɺB�B�NB�TB�TB�B�B��B	B	  B��B	\B	(�B	8RB	D�B	]/B	bNB	w�B	� B	�DB	�hB	��B	�'B	�XB	�jB	��B	��B	��B	�HB	�B	��B
B
  B
B
B
%B
JB
�B
&�B
,B
/B
8RB
<jB
J�B
VB
W
B
`BB
gmB
m�B
n�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	2�B	M�B	R�B	W�B	g�B	�VB	� B	�AB	��B	�B	ÖB	ؓB	��B
,�B
B�B
Q�B
g�B
j�B
��B
��B
�AB�B0�B=<BIRBoOB�#B�B�WB��BбB�BB�{B�TBϫB��B�2B��B��B|�BffB
��B
�
B
)�B	�vB	�B	%�B� B��B�_B��B�oBðBȚB�!BϫB�	B��B�HB�zB�tB�B��B�B�\B�?B��B�B�B��B�*B�B	�B	#B	/�B	G�B	MB	bhB	j�B	u�B	|PB	�uB	��B	��B	�B	�XB	�jB	�cB	ˬB	�$B	�XB	�B	�eB	�wB	�B	��B	��B

#B
NB
mB
B
"�B
'B
5?B
@iB
A�B
J�B
Q�B
W�B
X�B
^�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.021(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202007280015252020072800152520200728001525202207232057382022072320573820220723205738202207261128422022072611284220220726112842  JA  ARFMdecpV4_b                                                                20200713065654  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200713065658  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200713065659  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200713065709  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200713065709  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200713065720  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200713065951                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200716215230  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200716215316  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200716215316  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200716215317  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200716215317  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200716215317  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200716215317                      G�O�G�O�G�O�                JA  ARUP                                                                        20200716215403                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200717000000  CF  PSAL_ADJUSTED_QC?�ff?�ffG�O�                JM  ARCAJMQC2.0                                                                 20200727151525  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200727151525  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115738  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022842  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                