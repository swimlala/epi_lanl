CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-04-18T14:37:36Z creation;2022-04-18T14:37:38Z conversion to V3.1;2022-11-10T04:17:19Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20220418143736  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_139                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @���JU��1   @���i�� @0Z�G�{�d�+J1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�ffA&ffA{33A���A�33A噚B��B��B.ffBD��BZ��Bj��B���B�ffB�33B�33B�  B���B�  Bř�B�  B���B���B�  B�33C�3CL�C��C  C�3C��C� C%��C*�fC/33C4L�C8� C=  CA�fCG�CR��C]�CfffCpL�CzffC��fC��C�  C�@ C�Y�C�33C��3C�ٚC��C��C�Y�C��fC�L�C��C��C�&fC�@ C��C�Y�C��C�Y�C�&fC���C��C�� D3DfDٚD��D��D33D &fD%�D*,�D/�D3�fD8��D=�3DC&fDH&fDM�DQ�3DW�D[�3D`�fDe� Dk  Dp�Dt��Dy� D�33D�� D�� D�	�D�I�D�vfD���D�fD�I�D�� D��fD���D�S3DԐ D���D��3D�L�D�3D�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @�ffA&ffA{33A���A�33A噚B��B��B.ffBD��BZ��Bj��B���B�ffB�33B�33B�  B���B�  Bř�B�  B���B���B�  B�33C�3CL�C��C  C�3C��C� C%��C*�fC/33C4L�C8� C=  CA�fCG�CR��C]�CfffCpL�CzffC��fC��C�  C�@ C�Y�C�33C��3C�ٚC��C��C�Y�C��fC�L�C��C��C�&fC�@ C��C�Y�C��C�Y�C�&fC���C��C�� D3DfDٚD��D��D33D &fD%�D*,�D/�D3�fD8��D=�3DC&fDH&fDM�DQ�3DW�D[�3D`�fDe� Dk  Dp�Dt��Dy� D�33D�� D�� D�	�D�I�D�vfD���D�fD�I�D�� D��fD���D�S3DԐ D���D��3D�L�D�3D�D�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϸRAϟ�Aϝ�AϓuA���A�
=A��mA�Ȁ\A̍PA�n�A�C�A�?}A�5?A�$�A�{A�oA��A�1A���A���A�ffA��A�ĜA��A�M�Aź^A�33A��wA��\A�C�A�ĜA�7LA�v�A��A�z�A��yA�&�A��A�bA��+A�VA��Aw��Akt�AX��AQ`BAIAC�7A9��A0�A*�HA'�A �+AS�A�HA��AA�PAO�A��A��AK�AI�A
5?A+AĜA ��A b@��/@���@�u@�C�@�X@۶F@�?}@�
=@�-@Å@�j@�9X@�?}@�/@��y@�
=@���@�+@�"�@�I�@���@�S�@�Ĝ@��/@���@���@�?}@��/@���@�X@t�D@kC�@co@X�`@K��@@Ĝ@6�+@1��@*~�@#"�@9X@�9@�@��@��@o@A�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AϸRAϟ�Aϝ�AϓuA���A�
=A��mA�Ȁ\A̍PA�n�A�C�A�?}A�5?A�$�A�{A�oA��A�1A���A���A�ffA��A�ĜA��A�M�Aź^A�33A��wA��\A�C�A�ĜA�7LA�v�A��A�z�A��yA�&�A��A�bA��+A�VA��Aw��Akt�AX��AQ`BAIAC�7A9��A0�A*�HA'�A �+AS�A�HA��AA�PAO�A��A��AK�AI�A
5?A+AĜA ��A b@��/@���@�u@�C�@�X@۶F@�?}@�
=@�-@Å@�j@�9X@�?}@�/@��y@�
=@���@�+@�"�@�I�@���@�S�@�Ĝ@��/@���@���@�?}@��/@���@�X@t�D@kC�@co@X�`@K��@@Ĝ@6�+@1��@*~�@#"�@9X@�9@�@��@��@o@A�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�FB	�jB	�qB	ÖBB��B��B��B��B��B�B�B�B�B��B��B�B�B�B�B�B�3B�^B�^BŢBƨB�mB�BB{B �B.B/B-B7LB33B,B��B��B2-B
�B
jB
33B
�B	ȴB	bNB	49B	 �B	  B��B	B	PB	{B	hB	 �B	M�B	�uB	�oB	~�B	m�B	}�B	�1B	�{B	��B	��B	��B	��B	�^B	ǮB	ȴB	��B	ɺB	ĜB	��B	ɺB	��B	��B	��B	�;B	��B	�B	�5B	�B	�B	��B
B
B
DB
\B
{B
�B
�B
"�B
#�B
'�B
-B
0!B
:^B
A�B
H�B
O�B
S�B
[#B
bNB
jB
q�B
u�B
{�B
�B
�1B
�=B
�PB
�hB
�{B
��B
��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	�zB	��B	�B
�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�CB�TB��B��B��B҉BیB��B�cB~BEB�BeB!�B!B�B�zB��B BB
�fB
W
B
 �B
	�B	��B	M�B	�B	�B��B��B�B��B��B�6B	
�B	6�B	}B	|�B	iB	WsB	g�B	q�B	~�B	��B	��B	�XB	��B	�B	��B	��B	�B	��B	�}B	�KB	��B	��B	��B	��B	�7B	��B	��B	��B	�B	�dB	�B	�B	��B	��B	��B	�B
'B
+B
dB
jB
�B
�B
�B
#�B
+B
2GB
9XB
=�B
D�B
K�B
TB
[#B
_VB
e`B
k�B
q�B
s�B
v�B
z�B
}�B
��B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.022(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202204150015152022041500151520220415001515202210251313182022102513131820221025131318202210251812092022102518120920221025181209  JA  ARFMdecpV4_b                                                                20220325035715  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220418143736  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20220418143736  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20220418143737  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20220418143737  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20220418143738  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220418143738                      G�O�G�O�G�O�                JA  ARUP                                                                        20220419211501                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220323151507  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220323151507  QCF$                G�O�G�O�G�O�600000          JM  ARGQJMQC2.0                                                                 20220323151506  CV  JULD_LOCATION   G�O�G�O�F�c                JM  ARGQJMQC2.0                                                                 20220323151506  CV  LATITUDE        G�O�G�O�A�~�                JM  ARGQJMQC2.0                                                                 20220323151506  CV  LONGITUDE       G�O�G�O��'J�                JM  ARCAJMQC2.0                                                                 20220414151515  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220414151515  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041318  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091209  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                