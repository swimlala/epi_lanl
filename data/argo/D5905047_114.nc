CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-06-18T06:52:13Z creation;2019-06-21T21:53:18Z conversion to V3.1;2019-09-10T08:50:04Z update;2022-07-26T02:45:22Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190618065213  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  V4_131545_114                   2C  Dc�QARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @��{E�,�1   @�Ƈ���@3�O�;dZ�c�Q��1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @���A   A|��A�ffA���A���B��B!33B5��BJ  B^��Bq��B�ffB�33B�ffB�ffB�33B���B�ffBə�B���B���B�33B���B�ffC� C33C33C��CL�C�3C��C%�3C)��C/��C3�fC9�3C>�CBL�CHL�CRffC\��Cf�fCp�Cz33C�ffC���C�� C���C�Y�C�33C�33C�ffC�@ C�ffC��3C�@ C�ffC�Y�C��3C�  Cљ�C�L�C�L�C�&fC�  C�� C�@ C�  C�ffD��D&fD�3D,�D  D&fD   D$�3D)��D/,�D4,�D8�3D>33DB�3DHfDM3DR&fDW3D\&fDafDe� Dj� Dp  Du�DzfD�VfD�� D�ɚD�	�D�9�D�y�D���D�� D�9�D��3D�� D�3D�C3Dԉ�D��3D���D�@ D�fD��3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?   @���A   A|��A�ffA���A���B��B!33B5��BJ  B^��Bq��B�ffB�33B�ffB�ffB�33B���B�ffBə�B���B���B�33B���B�ffC� C33C33C��CL�C�3C��C%�3C)��C/��C3�fC9�3C>�CBL�CHL�CRffC\��Cf�fCp�Cz33C�ffC���C�� C���C�Y�C�33C�33C�ffC�@ C�ffC��3C�@ C�ffC�Y�C��3C�  Cљ�C�L�C�L�C�&fC�  C�� C�@ C�  C�ffD��D&fD�3D,�D  D&fD   D$�3D)��D/,�D4,�D8�3D>33DB�3DHfDM3DR&fDW3D\&fDafDe� Dj� Dp  Du�DzfD�VfD�� D�ɚD�	�D�9�D�y�D���D�� D�9�D��3D�� D�3D�C3Dԉ�D��3D���D�@ D�fD��3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A�Q�A��#AڶFAڬAڝ�AڑhAډ7A�t�A�-A�;dA�-A؇+A��`A��A�`BAҰ!A���A�(�AσA��
AȅA���A��`A�VA�&�A���A��FA��A�A�A�K�A���A�A�A�r�A��jA�~�A���A�z�A��\A�ƨA�I�A��A���Av��An�DAg�A\�HAS/AG�^AA/A8�A-��A%`BA!A�DAbNA�A{A�A��A�DA�Ap�@��@�@��#@��@�I�@��@އ+@�$�@��H@ɡ�@�t�@���@�A�@�-@�X@��h@�o@���@��+@�1@�5?@�ƨ@�G�@�"�@� �@�M�@��@��@�@��/@���@�Q�@���@y&�@p��@h1'@^�y@Vv�@N��@GK�@BM�@;S�@7�@1�@*�\@#��@�@hs@$�@��@ff@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�1A�Q�A��#AڶFAڬAڝ�AڑhAډ7A�t�A�-A�;dA�-A؇+A��`A��A�`BAҰ!A���A�(�AσA��
AȅA���A��`A�VA�&�A���A��FA��A�A�A�K�A���A�A�A�r�A��jA�~�A���A�z�A��\A�ƨA�I�A��A���Av��An�DAg�A\�HAS/AG�^AA/A8�A-��A%`BA!A�DAbNA�A{A�A��A�DA�Ap�@��@�@��#@��@�I�@��@އ+@�$�@��H@ɡ�@�t�@���@�A�@�-@�X@��h@�o@���@��+@�1@�5?@�ƨ@�G�@�"�@� �@�M�@��@��@�@��/@���@�Q�@���@y&�@p��@h1'@^�y@Vv�@N��@GK�@BM�@;S�@7�@1�@*�\@#��@�@hs@$�@��@ff@"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	%B	uB	hB	oB	oB	oB	oB	hB	hB	PB	�B	L�B	ffB	k�B	hsB	G�B	N�B	� B	ŢB
B
I�B
��B
�B9XB�bB��B�HB�yB��B��B�B�`B��B�B�yB��B�B�B�B�mB�BM�B
�XB
gmB
B	�B	ȴB	o�B	%�B	+B�yB�!B�7Bw�Bm�BhsB[#BP�BK�BL�BaHBL�BQ�B^5Bq�Bt�Bs�B|�B�1B�oB�B�LB�RB��B�B	uB	!�B	;dB	M�B	iyB	}�B	�PB	�{B	��B	�B	�}B	��B	��B	�ZB	�B	��B	��B
B

=B
\B
�B
 �B
(�B
/B
7LB
:^B
A�B
G�B
L�B
P�B
W
B
\)B
`BB
ffB
m�B
q�B
w�B
{�B
~�B
�B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B	B	�B	�B	�B	�B	�B	�B	�B�HB	B	>wB	XB	^OB	[�B	9rB	@�B	raB	�LB	�XB
<�B
�IB
��B,=B��B�eB�[B�CB��B�B�NB�B��B�tB�B��B�B��B�~B��B˒BDB
�B
\)B	�TB	�NB	��B	cTB	B��B��B�tB|�BjeB_�B[�BM�BC-B>BB?BS�B>�BC�BP.BdBf�Bf2BoBy�B�3B��B�*B�B�OB�-B	SB	�B	-B	?cB	Z�B	oiB	~�B	��B	�,B	�vB	��B	�"B	�tB	յB	��B	�$B	�IB	�nB	��B
 �B
�B
 B
7B
 \B
(�B
+�B
2�B
8�B
>B
B'B
HfB
MjB
Q�B
W�B
^�B
b�B
iB
m)B
p;B
uZB
xR31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.015(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201907020015332019070200153320190702001533202207232057042022072320570420220723205704202207261126122022072611261220220726112612  JA  ARFMdecpV4_b                                                                20190618065212  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190618065213  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190618065213  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190618065214  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190618065214  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190618065215  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190618065734                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190621215210  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190621215316  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190621215316  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190621215317  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190621215317  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190621215317  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190621215318                      G�O�G�O�G�O�                JA  ARUP                                                                        20190621215602                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190622000000  CF  PSAL_ADJUSTED_QC?   ?   G�O�                JM  ARCAJMQC2.0                                                                 20190701151533  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190701151533  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190702151454  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011516                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115704  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022612  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                