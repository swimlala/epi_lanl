CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-08-30T18:54:27Z creation;2017-08-30T18:54:29Z conversion to V3.1;2019-09-10T08:58:57Z update;2022-07-26T02:48:03Z update;     
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
resolution        =���     �  <d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  >4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  @x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  C0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  E    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Et   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  GD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  KX   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  M(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    iD   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  iH   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    i�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    i�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    i�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20170830185427  20220818051504  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               0A   JA  V4_131545_048                   2C  Dd��ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�!z��- 1   @�!�.��@4�n��P�d�����1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@�33A.ffAnffA���A���A�  B��B  B7��BHffB\ffBrffB�ffB�ffB���B�  B�33B���B���B���Bљ�B���B�  B�  B�ffC� C��C��C33C��CL�C�C%��C*L�C.�fC4� C9�C>�CC33CHL�CR� C[ffCe  Co33CyL�C�33C�@ C�  C��fC��C�ٚC��fC�ffC�� C��fC��C�&fC�@ C��3CȀ C�Y�C�@ C�ٚC���C��C��fC��3C�s3C�  C��3D�fD3D�D�D�D33D �D%&fD*�D/33D4  D8��D>3DB��DG�3DL� DQ�3DW�D[�fD`� De��Dk,�Dp33Dt�3Dz�D�P D��3D��fD�	�D�L�D���D��3D��fD�C3D���D�� D�fD�VfD�|�D��3D�3D�6fD�3D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?fff@�33A.ffAnffA���A���A�  B��B  B7��BHffB\ffBrffB�ffB�ffB���B�  B�33B���B���B���Bљ�B���B�  B�  B�ffC� C��C��C33C��CL�C�C%��C*L�C.�fC4� C9�C>�CC33CHL�CR� C[ffCe  Co33CyL�C�33C�@ C�  C��fC��C�ٚC��fC�ffC�� C��fC��C�&fC�@ C��3CȀ C�Y�C�@ C�ٚC���C��C��fC��3C�s3C�  C��3D�fD3D�D�D�D33D �D%&fD*�D/33D4  D8��D>3DB��DG�3DL� DQ�3DW�D[�fD`� De��Dk,�Dp33Dt�3Dz�D�P D��3D��fD�	�D�L�D���D��3D��fD�C3D���D�� D�fD�VfD�|�D��3D�3D�6fD�3D���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�uA�7A�n�A�l�A�bNA�`BA�^5A�&�A��A�ffAУ�A���AƲ-A� �Aĥ�Aú^APA��A�{A�G�A��DA��
A���A���A�bA�oA��-A��A��A��A�|�A�n�A�1A�dZA���A�E�A��uA�A�M�A�ffA��TA�ĜA���A~��Av��Ap~�Ae��A^��AS;dAP�ANĜAJz�ABE�A@�A=��A8bNA0�/A*�RA$�A�A�TAbA��A��A�h@��@��h@���@�@��;@�"�@�Ĝ@��@�O�@�C�@���@�S�@� �@�%@���@�V@�A�@���@�E�@�1'@�{@�Q�@�@�E�@�I�@���@�dZ@���@�o@�z�@��T@���@w�@q�7@l�@f��@`�`@X  @OK�@I��@C�@=�@6ff@.ȴ@)�@"�!@�@��@@ȴ@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�uA�7A�n�A�l�A�bNA�`BA�^5A�&�A��A�ffAУ�A���AƲ-A� �Aĥ�Aú^APA��A�{A�G�A��DA��
A���A���A�bA�oA��-A��A��A��A�|�A�n�A�1A�dZA���A�E�A��uA�A�M�A�ffA��TA�ĜA���A~��Av��Ap~�Ae��A^��AS;dAP�ANĜAJz�ABE�A@�A=��A8bNA0�/A*�RA$�A�A�TAbA��A��A�h@��@��h@���@�@��;@�"�@�Ĝ@��@�O�@�C�@���@�S�@� �@�%@���@�V@�A�@���@�E�@�1'@�{@�Q�@�@�E�@�I�@���@�dZ@���@�o@�z�@��T@���@w�@q�7@l�@f��@`�`@X  @OK�@I��@C�@=�@6ff@.ȴ@)�@"�!@�@��@@ȴ@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�hB��B�{B�{B��B��B��B��B��By�B�B��BɺBŢB��BȴB��B��B�}B�^B�3B�B�B��B�7B�+By�Bl�BJ�B@�B.BB�TBB�oB\)B)�B1BB
�fB
��B
��B
�hB
o�B
49B
B	��B	��B	_;B	S�B	O�B	49B	VB	B��B�B�wB�-B��B�=B{�Bw�Br�Bz�Bp�Bs�Bm�Br�Bt�By�B|�B�\B��B�wB�B��B	�B	/B	B�B	Q�B	m�B	z�B	�JB	��B	�B	�3B	�}B	ƨB	��B	�B	�BB	�sB	�B	��B
B

=B
{B
 �B
&�B
+B
1'B
49B
;dB
B�B
F�B
J�B
M�B
S�B
YB
^5B
aHB
hsB
jB
n�B
q�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B�{B�{B��B��B��B�LB��B}B�=B��B�=B��B�uB��BѷB�9B�;B�qB�+B�UB��B�B�B�7B{�BoBL�BA�B1'BtB�B�9B��B_VB,�B
	B�B
��B
�sB
�pB
��B
qvB
5�B
�B	ѝB	�B	`B	T{B	Q B	6`B	�B	�B�8B�#B�4B��B��B��B}"By�BtTB|Bq�BtnBn�Bs�ButBzB}�B��B��B�HBںB�JB	�B	/OB	B�B	RTB	m�B	{0B	�~B	��B	�6B	�hB	��B	��B	�"B	�KB	�vB	�B	��B	��B
3B

XB
�B
 �B
'B
+6B
1AB
4nB
;�B
B�B
F�B
J�B
M�B
T,B
Y1B
^OB
aHB
h�B
j�B
n�B
q�B
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709100016062017091000160620170910001606202207232056042022072320560420220723205604202207261121482022072611214820220726112148  JA  ARFMdecpV4_b                                                                20170830185206  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20170830185427  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20170830185428  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20170830185428  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20170830185428  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20170830185429  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170830185429                      G�O�G�O�G�O�                JA  ARUP                                                                        20170830185647                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20170831000000  CF  PSAL_ADJUSTED_QC?fff?fffG�O�                JM  ARCAJMQC2.0                                                                 20170909151606  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170909151606  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403050622  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920001517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115604  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051504                      G�O�G�O�G�O�                