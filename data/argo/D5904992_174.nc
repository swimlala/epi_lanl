CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-08-05T06:52:00Z creation;2020-08-08T21:53:17Z conversion to V3.1;2020-12-25T04:12:53Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  Dh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  20200805065200  20210115031508  5904992 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  V4_131538_174                   2C  D   ARVOR                           OIN-13JAP-ARL-66                5607A07                         844 @�-��{ 1   @�.�^�@;�\(��c�/��w1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?�ff@�  A(  A�  A���A�ffA���B��B#33B8ffBL  B]��Bq��B�33B�33B�  B�  B�ffB�ffB�  B���B�  B���B���B�B�ffC��C  CffC33C��C�C!�C%� C+�C0�C4�3C9  C?  CC�fCH��CR33C\��CfL�Cp� CzffC�&fC��C��3C��fC�33C�� C�ٚC��C�@ C��C��C��fC��3C�33C�33C�L�C��C�  C�@ C�� C��C��C�&fC�ٚC���D�fDfD  D&fD�fD�D��D%  D)�3D.��D433D9�D>�DC3DHfDL�3DQ� DV��D\&fDa  De�3Dj�fDp�Du�Dz3D�<�D�y�D��3D�	�D�@ D��3D�� D�3D�I�D��3D�ٚD�  D�\�DԐ Dڹ�D�3D�S3D�fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ?�ff@�  A(  A�  A���A�ffA���B��B#33B8ffBL  B]��Bq��B�33B�33B�  B�  B�ffB�ffB�  B���B�  B���B���B�B�ffC��C  CffC33C��C�C!�C%� C+�C0�C4�3C9  C?  CC�fCH��CR33C\��CfL�Cp� CzffC�&fC��C��3C��fC�33C�� C�ٚC��C�@ C��C��C��fC��3C�33C�33C�L�C��C�  C�@ C�� C��C��C�&fC�ٚC���D�fDfD  D&fD�fD�D��D%  D)�3D.��D433D9�D>�DC3DHfDL�3DQ� DV��D\&fDa  De�3Dj�fDp�Du�Dz3D�<�D�y�D��3D�	�D�@ D��3D�� D�3D�I�D��3D�ٚD�  D�\�DԐ Dڹ�D�3D�S3D�fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AхA�l�A�VA��A�M�A�A�A�  Aʙ�A���A��;A�A�jA�"�A��/A���A��uA�;dA�A��-A�VA�7LA�1A��
A�p�A���A�M�A�G�A��A�33A�{A���A��A�bNA�A�A~�yA{O�Axz�AvbAq�;AlI�AjE�Af(�A^^5AV�RARz�AL�`AJbAD��A?G�A9�A7`BA4��A1oA.A*��A(A�A%��A#"�A!l�A��A{A��A5?A�-A
ĜA�A33@�o@�G�@�dZ@@��H@�  @с@���@��@��;@�{@�v�@��D@�1@��+@�C�@�b@�Q�@�`B@��@��@�v�@�r�@}`B@y�@vv�@r^5@n��@k��@e�@^��@V�R@O��@H  @A��@<��@7��@0��@+�
@&��@ A�@9X@�;@�/@��@�+@
��@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AхA�l�A�VA��A�M�A�A�A�  Aʙ�A���A��;A�A�jA�"�A��/A���A��uA�;dA�A��-A�VA�7LA�1A��
A�p�A���A�M�A�G�A��A�33A�{A���A��A�bNA�A�A~�yA{O�Axz�AvbAq�;AlI�AjE�Af(�A^^5AV�RARz�AL�`AJbAD��A?G�A9�A7`BA4��A1oA.A*��A(A�A%��A#"�A!l�A��A{A��A5?A�-A
ĜA�A33@�o@�G�@�dZ@@��H@�  @с@���@��@��;@�{@�v�@��D@�1@��+@�C�@�b@�Q�@�`B@��@��@�v�@�r�@}`B@y�@vv�@r^5@n��@k��@e�@^��@V�R@O��@H  @A��@<��@7��@0��@+�
@&��@ A�@9X@�;@�/@��@�+@
��@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BYBaHB^5BbNBq�B��B�RB��By�Bk�B_;BS�BC�BA�BD�BC�B;dB1'B �B{B��B�`B��B��B��B�DBM�B9XB33BhB
�B
�
B
�B
�=B
D�B
+B
�B
	7B	�B	ǮB	�^B	��B	x�B	R�B	=qB	'�B	�B��B�`B��B��BB�9B��B��B�hB�=B�B� Bo�BaHBZBP�BD�B=qB33B-B"�B�B�BoBPBDB
=BbB�B'�B6FBM�BaHBu�B�{B��B�wB�B�B	1B	�B	-B	=qB	M�B	`BB	jB	|�B	�JB	��B	�RB	��B	�mB	��B
\B
�B
&�B
0!B
:^B
B�B
J�B
S�B
ZB
`BB
dZB
hsB
m�B
s�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BYKBaHB^�Bb�Br�B��B��B��B~(Bn}Bb�BVBE�BD3BG�BE�B<�B3B"NBSB�B�B��B��B�QB��BOBB:�B5�B�B
�B
�B
��B
�VB
F�B
,�B
�B
B	�OB	��B	�B	��B	z�B	TB	>�B	(�B	B�wB�8B�oB˒BÖB�B��B�]B�:B�B��B�;Bp�Ba�B[=BR:BEmB>�B4B./B$@BB=B�B�B�B
�BhB=B(sB6�BNVBa�Bv`B��B�$B��B�kB��B	fB	�B	-CB	=�B	N"B	`\B	j�B	}"B	�~B	��B	��B	��B	�B	�B
vB
�B
'B
0;B
:xB
B�B
J�B
TB
ZQB
`\B
dtB
hsB
m�B
s�B
w�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?�[<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              PO1=0.1(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202008190015472020081900154720200819001547202008190200122020081902001220200819020012202008200012262020082000122620200820001226  JA  ARFMdecpV4_b                                                                20200805065159  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200805065200  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200805065201  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200805065202  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200805065202  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200805065202  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20200805065344                      G�O�G�O�G�O�                JA  ARFMdecpV4_b                                                                20200808215227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20200808215315  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20200808215315  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20200808215316  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20200808215316  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20200808215317  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200808215317                      G�O�G�O�G�O�                JA  ARUP                                                                        20200808215405                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200809000000  CF  PSAL_ADJUSTED_QC?�ff?�ffG�O�                JM  ARCAJMQC2.0                                                                 20200818151547  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200818151547  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20200818170012  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200819151226  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20210115031508                      G�O�G�O�G�O�                