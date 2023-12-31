CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-26T06:52:44Z creation;2019-10-29T21:53:47Z conversion to V3.1;2022-07-26T02:44:51Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20191026065244  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  V4_131545_127                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @����|ƀ1   @����A @5�vȴ9X�c�x���1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?L��@�  A0  A�  A�  A͙�A�B��B#��B5��BH  B\��BrffB�33B���B�ffB���B�ffB�  B���B�ffBљ�B�33B�33B�ffB���C� C�3C33C� C� C�C �fC%�fC)�3C/  C4ffC9� C>��CC�3CH�3CRL�C\  Cf  CpL�Cy�fC��3C���C��3C�ffC��C��3C�ٚC�L�C�� C��3C�33C�&fC�33C��fC���C�s3C�L�C�33C�L�C�@ C��3C�@ C�L�C�L�C�ffD�D�3D� D�3D�3D�DٚD$��D)��D.�fD3�3D8�fD=�fDCfDH&fDM&fDR  DV��D\3Da@ Df9�Dk33Do��Du  Dz&fD�P D��fD�� D���D�L�D���D���D�fD�L�D��fD��3D�  D�<�D�vfD�� D��D�FfD��D�� D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?fff@�33A1��A���A���A�ffA�ffB  B$  B6  BHffB]33Br��B�ffB�  B���B�  B���B�33B�  Bə�B���B�ffB�ffBB���C��C��CL�C��C��C33C!  C&  C)��C/�C4� C9��C>�3CC��CH��CRffC\�Cf�CpffCz  C�  C�ٚC�  C�s3C��C�� C��fC�Y�C���C�  C�@ C�33C�@ C��3C�ٚC̀ C�Y�C�@ C�Y�C�L�C�  C�L�C�Y�C�Y�C�s3D  D��D�fDٚD��D3D� D$�3D)�3D.��D3��D8��D=��DC�DH,�DM,�DR&fDW  D\�DaFfDf@ Dk9�Dp  Du&fDz,�D�S3D���D��3D�  D�P D�� D�� D��D�P D���D��fD�3D�@ D�y�D��3D� D�I�D� D��3D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�A�A�A��A���AڶFAڮA�+A��A�dZA��TA��yA���A���A��A�p�A���A�1A��PA�+A��A��9A��yA�M�A���A���A�I�A���A�M�A�-A�?}A��A��A��yA�VA�K�A�XA�9XA��uA���A�|�Av(�Ap(�AZ^5AL�RAE��A>�yA8�!A3K�A2��A/�A+��A+7LA$��A��A�PA�!A��A1A��AbN@�K�@��@�(�@�X@�5?@�`B@�E�@���@�V@�V@�^5@��@�`B@��@�p�@���@�o@�n�@�ƨ@��@�  @�A�@���@���@���@�n�@�l�@���@��@�O�@���@�  @���@�=q@��@��@�r�@vff@j�@eO�@Z=q@Nff@Ihs@>ȴ@8Ĝ@5�h@3ƨ@/;d@&��@��@=q@ff@t�@1'@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�A�A�A��A���AڶFAڮA�+A��A�dZA��TA��yA���A���A��A�p�A���A�1A��PA�+A��A��9A��yA�M�A���A���A�I�A���A�M�A�-A�?}A��A��A��yA�VA�K�A�XA�9XA��uA���A�|�Av(�Ap(�AZ^5AL�RAE��A>�yA8�!A3K�A2��A/�A+��A+7LA$��A��A�PA�!A��A1A��AbN@�K�@��@�(�@�X@�5?@�`B@�E�@���@�V@�V@�^5@��@�`B@��@�p�@���@�o@�n�@�ƨ@��@�  @�A�@���@���@���@�n�@�l�@���@��@�O�@���@�  @���@�=q@��@��@�r�@vff@j�@eO�@Z=q@Nff@Ihs@>ȴ@8Ĝ@5�h@3ƨ@/;d@&��@��@=q@ff@t�@1'@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�BÖBĜBĜBÖBÖBB��B�B+BJB2-B;dB=qB<jBB�BG�B`BBq�Br�Bu�BiyBO�B@�B=qB<jB2-B�B�fB��B��B�Br�BiyBXB0!B�B�B
�B
��B
��B
.B	��B	ffB	6FB	bB�B�B��B�qB�-B��B��B�hBu�B`BBQ�BO�BL�BP�BP�BW
BT�BR�BK�BffBT�Bl�Bs�B�PB�jB�sB��B	+B	#�B	=qB	F�B	YB	v�B	�B	�PB	�B	�oB	��B	�!B	ŢB	�B	�NB	�B	�B	��B	��B
  B
B
B
DB
{B
�B
&�B
0!B
5?B
=qB
F�B
I�B
R�B
YB
\)B
^5B
aHB
hsB
r�B
v�B
z�B
}�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�)B�B�B�B��B�B��B�JB�B��B��B$�B+kB-�B,�B2aB6�BP.B`BBc BfLBZ�B@�B/iB,�B+�B"hB B�7B�sB�~Bt�Ba�BYBIB�B�B�B
�&B
ĶB
��B
�B	�B	X�B	'B	;B�:B�?B��B�]B�4B�$B�{B��Bf�BQ�BBB?B<�B@�B@�BFYBEmBC{B;dBV�BD3B\Bb�B}�B��B��B�B�zB	�B	,�B	5�B	G�B	e�B	sMB	|�B	rGB	��B	��B	�VB	��B	�B	�NB	ݘB	�B	��B	��B	� B	��B	�B	�DB
�B
�B
B
B
$ZB
,qB
5�B
8�B
A�B
HB
KB
MB
PHB
WsB
a�B
e�B
i�B
l�B
pB
u33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6M�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.017(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201911090015282019110900152820191109001528202207232057152022072320571520220723205715202207261127032022072611270320220726112703  JA  ARFMdecpV4_b                                                                20191026065243  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191026065244  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191026065245  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191026065245  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191026065245  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191026065246  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20191026065511                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20191029215310  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20191029215345  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20191029215346  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20191029215346  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20191029215346  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20191029215347  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191029215347                      G�O�G�O�G�O�                JA  ARUP                                                                        20191029215433                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191030000000  CF  PSAL_ADJUSTED_QC?L��@�  G�O�                JM  ARCAJMQC2.0                                                                 20191108151528  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191108151528  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115715  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022703  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                