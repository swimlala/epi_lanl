CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   t   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-04-19T06:52:06Z creation;2019-04-22T18:53:29Z conversion to V3.1;2019-09-10T08:50:54Z update;2022-07-26T02:45:37Z update;     
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
_FillValue                    i�Argo profile    3.1 1.2 19500101000000  20190419065206  20220818051505  5905047 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               lA   JA  V4_131545_108                   2C  Dc�VARVOR                           OIN-13JAP-ARL-73                5607A07                         844 @ط{8�1   @ط���� @2޸Q��c�V�u1   ARGOS   A   B   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       >���@���A!��A|��A���Ař�A���B
��B!33B333BE33B`  Br��B���B�  B�33B�33B���B�  B�ffBƙ�B���B�33B噚B���B�ffC�3CL�C�3C�3CffC�C�fC%L�C*��C.�3C4ffC8��C=� CB33CH� CR�C[� Cf33CpffCz33C��fC�&fC�L�C��C�&fC�ٚC�  C�  C�&fC�@ C��3C�ٚC�  CÌ�C��C�  C�� Cֳ3C۳3C���C�33C�� C�  C�@ C�� D�DfDfDfD&fDfD 9�D%&fD*&fD.�fD3� D9  D>@ DC�DH�DL��DQ�fDW  D\3Da�Df�Dk3Dp3Du�Dz&fD�C3D���D�� D��D�L�D�� D��fD��D�` D���D�ٚD�fD�P D�ffDڳ3D�fD�` D�vfD�ٚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111>���@���A!��A|��A���Ař�A���B
��B!33B333BE33B`  Br��B���B�  B�33B�33B���B�  B�ffBƙ�B���B�33B噚B���B�ffC�3CL�C�3C�3CffC�C�fC%L�C*��C.�3C4ffC8��C=� CB33CH� CR�C[� Cf33CpffCz33C��fC�&fC�L�C��C�&fC�ٚC�  C�  C�&fC�@ C��3C�ٚC�  CÌ�C��C�  C�� Cֳ3C۳3C���C�33C�� C�  C�@ C�� D�DfDfDfD&fDfD 9�D%&fD*&fD.�fD3� D9  D>@ DC�DH�DL��DQ�fDW  D\3Da�Df�Dk3Dp3Du�Dz&fD�C3D���D�� D��D�L�D�� D��fD��D�` D���D�ٚD�fD�P D�ffDڳ3D�fD�` D�vfD�ٚD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A��A��#A˗�A˓uAˋDA�v�A���A�x�AȰ!A���A��A���A�|�A�(�A��uA�VA��A�=qA��/A��7A�JA���A�7LA�z�A�A�ƨA�G�A�dZA��A�ZA��A���A�t�A�M�A�  A���A�`BA�t�A�%A��A�~�A�A�Ay��An�Ad=qA\�+AR�!AE�A<E�A2�A+��A(^5A#��A?}A��A�/AVAG�A�@���@�h@�Q�@�ff@݁@�p�@Ӿw@·+@��@ƸR@ʟ�@��^@�Q�@�`B@��-@�=q@��@�b@�@�j@�E�@��9@�x�@���@�1'@�
=@�7L@�@��
@�V@�9X@��@�t�@�V@�M�@�9X@z��@t�@k"�@a&�@W�@P��@EO�@@�9@;�F@4�@,z�@'�@"��@�+@X@��@�`@�/@�9@O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A��A��#A˗�A˓uAˋDA�v�A���A�x�AȰ!A���A��A���A�|�A�(�A��uA�VA��A�=qA��/A��7A�JA���A�7LA�z�A�A�ƨA�G�A�dZA��A�ZA��A���A�t�A�M�A�  A���A�`BA�t�A�%A��A�~�A�A�Ay��An�Ad=qA\�+AR�!AE�A<E�A2�A+��A(^5A#��A?}A��A�/AVAG�A�@���@�h@�Q�@�ff@݁@�p�@Ӿw@·+@��@ƸR@ʟ�@��^@�Q�@�`B@��-@�=q@��@�b@�@�j@�E�@��9@�x�@���@�1'@�
=@�7L@�@��
@�V@�9X@��@�t�@�V@�M�@�9X@z��@t�@k"�@a&�@W�@P��@EO�@@�9@;�F@4�@,z�@'�@"��@�+@X@��@�`@�/@�9@O�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
P�B
YB
XB
W
B
W
B
XB
W
B
ZB
��B
��B
��B
�B
�/B
�B
��B33BO�BVBm�Bo�B��B�;B�B��BB\B(�BG�BA�BO�B?}B6FB �BDB��B�B�`B�
B�^B�+BXB
��B
hsB
�B	��B	�oB	^5B	 �B�/B�'B��B�PB�\B�\B�=B|�Bp�Bs�B[#BG�BW
BdZBr�Bw�B|�B}�B�7B�B�=B��BȴB�B		7B	(�B	L�B	_;B	m�B	�B	��B	��B	�-B	�^B	ɺB	��B	�B	�;B	�sB	�B	��B
B

=B
\B
�B
�B
 �B
#�B
(�B
.B
33B
9XB
@�B
E�B
@�B
F�B
P�B
\)B
cTB
gmB
l�B
p�B
v�B
{�B
� B
�B
�+B
�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
C-B
KDB
J=B
IB
IB
J#B
I�B
M�B
�B
��B
��B
��B
ϫB
��B
�vB&2BBABH�B`�Bc�B��B�oB��B�sB�B�B/B:�B4�BD3B2�B*0BMB�BB�B�XBٴB��B��B{�BNB
��B
\�B
oB	��B	��B	R�B	mB�B��B�B��B��B��B}�BqABd�Bg�BO(B;�BJ�BW�Be�BkBpBq'B|jBvB}<B��B��B�ZB�B	�B	?cB	Q�B	`'B	u�B	��B	�SB	��B	��B	�PB	�mB	�xB	ѝB	��B	�B	�B	�zB	��B
�B
�B
B
&B
B
#B
 vB
%�B
+�B
2�B
8B
2�B
8�B
C-B
NpB
U�B
Y�B
^�B
b�B
iB
n/B
r-B
uZB
y�B
}q33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     PO1=0.2(dbar); PO2=0.2(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9996(+-0.0000), deepest deltaS=-0.013(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-200(dbar) is excluded in mapping;                                                                                                                                        Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201905030015272019050300152720190503001527202207232056582022072320565820220723205658202207261125492022072611254920220726112549  JA  ARFMdecpV4_b                                                                20190419065205  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190419065206  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190419065207  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190419065208  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190419065209  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190419065209  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20190419065938                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20190422185209  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20190422185326  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20190422185327  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20190422185327  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20190422185328  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20190422185328  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190422185329                      G�O�G�O�G�O�                JA  ARUP                                                                        20190422185626                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190423000000  CF  PSAL_ADJUSTED_QC>���@���G�O�                JM  ARSQJMQC2.0                                                                 20190423000000  CF  TEMP_ADJUSTED_QC>���>���G�O�                JM  ARCAJMQC2.0                                                                 20190502151527  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190502151527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190503151444  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190920011515                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220723115658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220726022549  OW  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818051505                      G�O�G�O�G�O�                