CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2022-05-29T07:00:41Z creation;2022-06-01T15:58:57Z conversion to V3.1;2022-11-10T04:17:01Z update;     
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
_FillValue                    ixArgo profile    3.1 1.2 19500101000000  20220529070041  20221117234501  5905222 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131533_146                   2C  D   ARVOR                           OIN-13JAP-ARL-61                5607A07                         844 @��;�0*�1   @��{���@0�I�^5�d�7KƧ�1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?fff@�33A   At��A���A���A�B
��B��B1��BG��B\��Bl  B���B�  B���B�33B���B�ffB�33B�  B�  Bޙ�B���B�33B�ffCL�CL�CffC�C  C  C   C$ffC*� C/L�C3�fC8� C=ffCBL�CGffCQ� CZ�fCf  Cp�C{  C�@ C�33C��3C��fC��C��fC��C�L�C�33C��fC�&fC��C�&fC�ٚCǳ3C�33C��3C�Y�C�ٚC�@ C�ٚC�&fC�L�C�L�C�33D9�D  D��D�3D�3D�D��D%fD*3D/&fD43D8�3D=ٚDC�DG��DM�DR�DW9�D[��Da,�De��Dk  Dp  Dt� Dy��D�I�D��3D���D�3D�6fD��fD���D�  D�L�D�|�D��3D�fD�L�Dԓ3DڶfD��D�I�D�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?fff@�33A   At��A���A���A�B
��B��B1��BG��B\��Bl  B���B�  B���B�33B���B�ffB�33B�  B�  Bޙ�B���B�33B�ffCL�CL�CffC�C  C  C   C$ffC*� C/L�C3�fC8� C=ffCBL�CGffCQ� CZ�fCf  Cp�C{  C�@ C�33C��3C��fC��C��fC��C�L�C�33C��fC�&fC��C�&fC�ٚCǳ3C�33C��3C�Y�C�ٚC�@ C�ٚC�&fC�L�C�L�C�33D9�D  D��D�3D�3D�D��D%fD*3D/&fD43D8�3D=ٚDC�DG��DM�DR�DW9�D[��Da,�De��Dk  Dp  Dt� Dy��D�I�D��3D���D�3D�6fD��fD���D�  D�L�D�|�D��3D�fD�L�Dԓ3DڶfD��D�I�D�vfD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aס�A��A֋DAօA�v�A�p�A�hsA�=qA�/AӅAӓuA�ZA�=qA�$�A���A���A�{A�`BA�1A�`BA���A�z�AŲ-A\A���A��!A�n�A��uA�t�A�|�A���A��A���A�%A�JA��A�VA�C�A�9XA���A�/A�ffA~=qAu/Ahz�AVĜAG7LA>1A4ĜA,$�A(�A$��A�jA��A�A+A\)A�A�A�A�A	x�AO�A$�A��A -@�p�@�@���@�@�33@�"�@ݩ�@ش9@���@�7L@�K�@�o@�5?@��@��j@�{@��!@�  @��9@���@�X@�1@��@��/@��#@�ƨ@�$�@�"�@�V@�;d@���@�bN@wl�@m�-@c��@W�@L�@E�T@>ff@7��@.ff@'��@"�\@Z@\)@ƨ@��@9X@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aס�A��A֋DAօA�v�A�p�A�hsA�=qA�/AӅAӓuA�ZA�=qA�$�A���A���A�{A�`BA�1A�`BA���A�z�AŲ-A\A���A��!A�n�A��uA�t�A�|�A���A��A���A�%A�JA��A�VA�C�A�9XA���A�/A�ffA~=qAu/Ahz�AVĜAG7LA>1A4ĜA,$�A(�A$��A�jA��A�A+A\)A�A�A�A�A	x�AO�A$�A��A -@�p�@�@���@�@�33@�"�@ݩ�@ش9@���@�7L@�K�@�o@�5?@��@��j@�{@��!@�  @��9@���@�X@�1@��@��/@��#@�ƨ@�$�@�"�@�V@�;d@���@�bN@wl�@m�-@c��@W�@L�@E�T@>ff@7��@.ff@'��@"�\@Z@\)@ƨ@��@9X@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
��B
ɺB
ȴB
ȴB
ɺB
ȴB
ĜB
ĜBD�Bq�B�JB�PB�JB�VB�PB�uB�'B��B��B_;B[#BP�BL�BC�BH�BB�BI�B5?B2-B'�BJB�B�HB�XB��B�BL�B/BVB
�yB
v�B
G�B
JB	ĜB	ffB	$�B	B�sB�ZB��B	B	{B	2-B	S�B	_;B	~�B	� B	�DB	�bB	��B	��B	��B	��B	�'B	�?B	�'B	�B	��B	��B	��B	�{B	��B	�B	�LB	B	��B	�B	�#B	�#B	�B	�B	��B	��B
B
	7B
bB
oB
�B
�B
�B
"�B
%�B
)�B
-B
1'B
8RB
>wB
C�B
J�B
O�B
XB
_;B
cTB
hsB
l�B
t�B
z�B
}�B
�B
�7B
�JB
�\B
�oB
��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
�B
��B
��B
��B
��B
�'B
��B
��B,WBY�BtnButBt�BvzBvB|�B�	B�dB�OBIBDgB<B7B.}B4B/�B5�B�B�B�B��BۦB�B�nB��Bo�B7B�B
�$B
��B
a�B
2B	��B	�'B	S@B	B�qBӏB�"B�B�]B��B	qB	="B	HfB	g�B	i�B	t�B	yXB	~�B	�oB	��B	�NB	�QB	��B	�QB	��B	�:B	�B	��B	}�B	��B	�B	�'B	��B	��B	� B	�3B	�gB	�[B	ٚB	�B	��B	��B	�B	�$B	�B
[B
gB
zB
�B
�B
�B
�B
�B
 �B
'8B
,WB
3hB
8�B
@�B
G�B
K�B
QB
UMB
]dB
c�B
f�B
m�B
q�B
t�B
xB
{B
~3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=-0.2(dbar); PO2=-0.2(dbar)                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9994(+-0.0000), deepest deltaS=-0.023(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202206120015052022061200150520220612001505202210251313252022102513132520221025131325202210251812382022102518123820221025181238  JA  ARFMdecpV4_b                                                                20220529070041  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220529070041  IP                  G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20220529130025  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20220601155855  IP                  G�O�G�O�G�O�                JA      jafc1.0                                                                 20220601155857                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220602005902  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220602005902  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220601155925                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20220601151507  QCP$                G�O�G�O�G�O�7DE37C          JM  ARGQrqcjv291                                                                20220601151507  QCF$                G�O�G�O�G�O�200000          JM  ARGQJMQC2.0                                                                 20220601151506  CV  JULD            G�O�G�O�FΛ�                JM  ARGQJMQC2.0                                                                 20220601151506  CV  JULD_LOCATION   G�O�G�O�FΛ�                JM  ARCAJMQC2.0                                                                 20220611151505  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20220611151505  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221025041325  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20221025091238  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221117234501                      G�O�G�O�G�O�                