CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-05-19T06:52:25Z creation;2019-05-22T21:53:53Z conversion to V3.1;2019-09-10T08:50:29Z update;2022-07-26T02:45:30Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190519065225  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA  V4_131545_111                   2C  Dc��ARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @ؾ�h�� 1   @ؿx�� @3      �c������1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?���@�  A.ffAt��A�33A���A���B��B33B1��BJffB]33Bp  B�33B�33B���B�  B�ffB���B�33Bə�B�ffB�ffB癚B���B�  CffC�3C��C  C�3C�C ffC&�C*�3C/��C3�3C9�C=��CBffCG� CQ��C[  Ce��CpffCz� C���C�Y�C��C�33C�� C�&fC��3C��3C���C��C�33C�  C�  C���C���C�� C�L�C��fC��fC�  C�Y�C�33C�@ C�� C��D��D�3D�fD�D�3D�fD fD%  D*  D/  D4fD8��D=� DCfDG� DM�DQ� DW  D\  D`��Df  Dk9�Dp@ Du&fDz  D�I�D��3D���D�	�D�S3D�� D�� D� D�9�D�� D�� D�� D�<�DԌ�D��3D�� D�P D��D��fD�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @���A,��As33A�ffA�  A���BffB��B133BJ  B\��Bo��B�  B�  B�ffB���B�33B�ffB�  B�ffB�33B�33B�ffB�B���CL�C��C� C�fC��C  C L�C&  C*��C/�3C3��C9  C=� CBL�CGffCQ�3CZ�fCe� CpL�CzffC�� C�L�C�  C�&fC��3C��C��fC��fC���C�  C�&fC��3C��3C�� C�� C̳3C�@ C�ٚC�ٚC��3C�L�C�&fC�33C��3C��D�fD��D� D3D��D� D   D$��D)��D.��D4  D8�fD=ٚDC  DGٚDMfDQٚDW�D[��D`�3Df�Dk33Dp9�Du  Dy��D�FfD�� D���D�fD�P D���D���D��D�6fD���D���D���D�9�Dԉ�D�� D���D�L�D퉚D��3D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�p�A�  AԶFAԕ�AґhAЛ�Aϲ-A���A�~�A�x�AϋDA�A�AξwA�1A�  A�A�A�$�A���AǏ\A�$�A�bA���A�5?A��A�+A���A�G�A��DA�A�M�A��RA��9A��+A�A�A�ĜA���A���A��RA���A��A���A�VA���Av�jAo�#A^�AN-ABĜA<E�A6�\A-��A'\)A!p�A�RA�FAA�A��A�hA
jA�H@��;@�l�@�1@��@�b@���@�;d@ݑh@�"�@և+@��/@�ȴ@�;d@��@�ȴ@�1@�dZ@�V@���@�|�@���@��@�@�v�@��m@��@�dZ@�Z@�"�@���@�J@���@�?}@�C�@���@�^5@{��@tj@k��@co@Y%@Q7L@G�w@>{@7�@2�@-�-@)x�@$�/@ Q�@"�@{@�@@	�^@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�p�A�  AԶFAԕ�AґhAЛ�Aϲ-A���A�~�A�x�AϋDA�A�AξwA�1A�  A�A�A�$�A���AǏ\A�$�A�bA���A�5?A��A�+A���A�G�A��DA�A�M�A��RA��9A��+A�A�A�ĜA���A���A��RA���A��A���A�VA���Av�jAo�#A^�AN-ABĜA<E�A6�\A-��A'\)A!p�A�RA�FAA�A��A�hA
jA�H@��;@�l�@�1@��@�b@���@�;d@ݑh@�"�@և+@��/@�ȴ@�;d@��@�ȴ@�1@�dZ@�V@���@�|�@���@��@�@�v�@��m@��@�dZ@�Z@�"�@���@�J@���@�?}@�C�@���@�^5@{��@tj@k��@co@Y%@Q7L@G�w@>{@7�@2�@-�-@)x�@$�/@ Q�@"�@{@�@@	�^@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	@�B	��B	��B	��B	y�B	� B	}�B	��B	�B	�B	��B	��B	��B	ȴB	�NB
B
�B
ÖB
��B�B5?B2-B�B>wBp�B��B�B�NB�B#�B&�B �B{B��B�NB��B�Bl�B#�BhB
�B
ǮB
��B
B	��B	jB	uB�#BÖB�B��B�bB�7B�1B�JB��B��B�!Bz�BF�B=qBD�B@�BXB_;Bl�B�B��B��B��B�-B�fB	
=B	B	�B	2-B	M�B	dZB	}�B	�DB	��B	��B	�?B	ƨB	��B	�B	�ZB	�B	��B	��B
B

=B
hB
�B
�B
!�B
(�B
.B
33B
8RB
@�B
E�B
N�B
S�B
ZB
^5B
bNB
ffB
jB
o�B
u�B
z�B
~�B
�B
�+B
�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	4�B	�RB	��B	��B	mCB	rGB	o�B	��B	�_B	�)B	żB	��B	��B	��B	ԕB	�2B
�B
��B
�CB4B'�B$�B<B0�BcTB�}B�1B��B^B�BBB	�B��B��B��B��Bd@B�B�B
�B
�xB
��B	��B	��B	`'B	fB��B��B�vB�(B��B|PBz�BB�B�MB��Bn�B9�B0!B7�B2�BKxBRB^�BtTB��B�B�B�&BؓB��B��B	B	$tB	@B	VmB	pB	}<B	�rB	��B	�8B	��B	��B	��B	�mB	�B	�B	�B	��B	�6B
aB
fB
�B
�B
�B
�B
%B
*KB
2GB
7�B
@�B
E�B
K�B
PB
T,B
XEB
\CB
abB
g�B
l�B
p�B
t�B
y	B
}�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.3(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.014(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201906020015352019060200153520190602001535202207232057012022072320570120220723205701202207261126002022072611260020220726112600  JA  ARFMdecpV4_b                                                                20190519065224  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190519065225  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190519065226  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190519065227  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190519065227  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190519065228  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190519065923                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190522215300  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190522215351  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190522215352  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190522215352  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190522215352  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190522215353  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190522215353                      G�O�G�O�G�O�                JA  ARUP                                                                        20190522215712                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190523000000  CF  PSAL_ADJUSTED_QC?���?���G�O�                JM  ARCAJMQC2.0                                                                 20190601151535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190601151535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190602151457  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115701  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022600  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                