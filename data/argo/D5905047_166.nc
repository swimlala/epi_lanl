CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-11-20T06:51:16Z creation;2020-11-23T21:51:46Z conversion to V3.1;2022-07-26T02:43:17Z update;     
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
_FillValue                    ixArgo profile    3.1 1.2 19500101000000  20201120065116  20220818061505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131545_166                   2C  D   ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @�H�FZC�1   @�H�=�/�@3�V�u�c&��n�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�  @�ffA)��Aq��A�33A�ffA홚B
��BffB2ffBG33B\  Bq33B���B���B���B�ffB�33B�ffB�  B�33B���B�  B癚B�B���C��C��C� C��C��CL�C�fC$�fC*33C/L�C4ffC9ffC>33CC33CHL�CR�C\� Ce��CoffCz��C��C��fC��3C�ٚC�ffC�� C�ffC�  C�33C��fC�L�C�ٚC��C�� Cǳ3C��fC�&fC�L�C�@ C��3C�ٚC��C�s3C��C�� D3D�fD�3D�D3D33D @ D$��D)� D.� D3�fD8��D>  DC33DG� DMfDQ��DW  D[�3Da33Df  Dk3Dp,�Du,�Dy��D�9�D��fD���D�fD�C3D���D���D��D�VfD�� D�ɚD�3D�Y�Dԙ�D�ɚD���D�@ D홚D�ɚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?���@���A+33As33A�  A�33A�ffB33B��B2��BG��B\ffBq��B���B�  B���B���B�ffB���B�33B�ffB�  B�33B���B���B�  C�3C�fC��C�3C�fCffC   C%  C*L�C/ffC4� C9� C>L�CCL�CHffCR33C\��Ce�3Co� Cz�3C�&fC��3C�  C��fC�s3C���C�s3C��C�@ C��3C�Y�C��fC��C���C�� C��3C�33C�Y�C�L�C�  C��fC�&fC�� C��C���D�D��D��D  D�D9�D FfD%  D)�fD.�fD3��D9  D>&fDC9�DG�fDM�DR  DWfD[��Da9�DffDk�Dp33Du33Dy�3D�<�D���D���D�	�D�FfD���D�� D� D�Y�D��3D���D�fD�\�DԜ�D���D�� D�C3D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AӸRAӬAӛ�Aӕ�Aӗ�AӑhAӑhAӑhAӓuAӑhAӕ�Aӕ�Aӕ�Aӕ�Aӗ�AӑhA��A��A�oA�ZA��-A���A��A�A�A�5?A�-A�bNA�$�A���A��DA�7LA���A�;dA��-A��hA���A�z�A��A��RA�;dA�hsA���A�7LAn~�AbZAQ�AK��A@��A;��A7l�A3��A*�`A'l�A"�Av�A��A&�A�TA
�uA��AffA(�Ap�AA�@�Ĝ@��^@��@׮@���@ΰ!@˅@ċD@�C�@���@���@�t�@�o@�=q@��R@�5?@�@��@���@��m@�&�@���@��@���@��@�r�@���@�
=@��T@��@�V@�ƨ@��@~ff@vv�@m��@d��@]��@Sƨ@J�@E/@=O�@7K�@0�9@(Ĝ@$9X@ 1'@��@bN@�m@�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AӸRAӬAӛ�Aӕ�Aӗ�AӑhAӑhAӑhAӓuAӑhAӕ�Aӕ�Aӕ�Aӕ�Aӗ�AӑhA��A��A�oA�ZA��-A���A��A�A�A�5?A�-A�bNA�$�A���A��DA�7LA���A�;dA��-A��hA���A�z�A��A��RA�;dA�hsA���A�7LAn~�AbZAQ�AK��A@��A;��A7l�A3��A*�`A'l�A"�Av�A��A&�A�TA
�uA��AffA(�Ap�AA�@�Ĝ@��^@��@׮@���@ΰ!@˅@ċD@�C�@���@���@�t�@�o@�=q@��R@�5?@�@��@���@��m@�&�@���@��@���@��@�r�@���@�
=@��T@��@�V@�ƨ@��@~ff@vv�@m��@d��@]��@Sƨ@J�@E/@=O�@7K�@0�9@(Ĝ@$9X@ 1'@��@bN@�m@�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B=qBA�BB�BA�BA�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BA�B7LB2-B7LBR�Bz�B��B�-B��B�yB��B��B�B+B)�B&�B�BJBDB�ZBB��B�JBZB#�B
�B
��B
XB	ɺB	e`B	DB�yBÖB�9B�B�B��B�B��B�
B�BB��B�B�%B�B�bB�RB�B��B�B��B�hB��BĜB��B�NB	-B	E�B	]/B	�+B	�bB	o�B	t�B	t�B	�B	��B	��B	�B	�
B	ĜB	ɺB	�
B	�;B	�sB
B
1B	��B
B
1B
JB
{B
�B
"�B
+B
7LB
B�B
J�B
P�B
T�B
[#B
cTB
iyB
o�B
s�B
w�B
z�B
~�B
�B
�+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B0�B4�B5�B4�B4�B5�B5�B5�B5�B5�B5�B5�B5�B5�B5�B5tB5tB(>B-�BH�Bp�B�~B��B��B�~B�B�]B
rB B5B�BB�B �B�B��B�qB��BP�B�B
�B
��B
P�B	�OB	\xB	 �B��B��B��B�\B��B�jB��B��B�B�B��B��Bx�Bz^BwLB�3B��B�B�B�hB�uB��B�OB��B��B�gB	 �B	9XB	P�B	z�B	��B	cnB	h�B	h�B	t�B	�JB	�mB	�|B	��B	�lB	�VB	��B	��B	��B	�nB	�B	�|B	��B	��B	��B
�B
"B
SB
�B
*�B
6B
>(B
DMB
HfB
N�B
V�B
\�B
c B
g8B
k6B
ncB
r|B
w�B
z�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<4�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.3(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0001), deepest deltaS=-0.012(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202012040015382020120400153820201204001538202207232057502022072320575020220723205750202207261129322022072611293220220726112932  JA  ARFMdecpV4_b                                                                20201120065115  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201120065116  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201120065116  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201120065116  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201120065116  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201120065116  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20201120065142                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20201123215126  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20201123215144  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20201123215144  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20201123215145  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20201123215145  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20201123215146  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201123215146                      G�O�G�O�G�O�                JA  ARUP                                                                        20201123215205                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20201124000000  CF  PSAL_ADJUSTED_QC?�  ?�  G�O�                JM  ARCAJMQC2.0                                                                 20201203151538  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201203151538  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115750  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022932  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818061505                      G�O�G�O�G�O�                