CDF   +   
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2008-02-17T13:01:15Z creation;2011-02-01T07:53:28Z update;2015-06-09T10:17:26Z conversion to V3.1;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   
resolution        >�����h�   axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�����h�        8l   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure     axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  ;\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  Ap   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  A�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  E�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  G�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  G�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  p  I�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Ll   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Ul   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ^l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  gl   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    hL   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    hP   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         hd   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        hh   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  5900493 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  20080217130115  20150621120514  A5_21018_147                    2C  D   APEX                            1090                            061703                          846 @Ի�P��N1   @Ի��:�@2�V��b��1'1   ARGOS   A   B   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @y��AffA\��A�33A���A�  B	��B��B2  BE33BZ  Bm33B�ffB�ffB���B�  B���B���B�  B�ffBЙ�B�33B�ffB���B���C� CL�C�C�C33CL�C33C$L�C)��C.� C3� C833C=L�CBffCGL�CQ� C[��Ce� Co�CyL�C�s3C�s3C�Y�C�Y�C���C��fC���C��3C��3C��3C���C��3C���C�Cǀ C̙�C�� C֦fCۦfC�3C�3C�3CC���C��3D��D� D�3D��D�fD�D"S3D(��D.��D5�D;Y�DAs3DG��DN�DT` DZ��D`�3Dg  Dm9�Ds��Dy�3D�&fD�ffD��fD��3D�&fD�l�D���D�� D�,�D�i�D���D��fD�,�D�l�Dک�D��fD�)�D�S3D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��AffA\��A�33A���A�  B	��B��B2  BE33BZ  Bm33B�ffB�ffB���B�  B���B���B�  B�ffBЙ�B�33B�ffB���B���C� CL�C�C�C33CL�C33C$L�C)��C.� C3� C833C=L�CBffCGL�CQ� C[��Ce� Co�CyL�C�s3C�s3C�Y�C�Y�C���C��fC���C��3C��3C��3C���C��3C���C�Cǀ C̙�C�� C֦fCۦfC�3C�3C�3CC���C��3D��D� D�3D��D�fD�D"S3D(��D.��D5�D;Y�DAs3DG��DN�DT` DZ��D`�3Dg  Dm9�Ds��Dy�3D�&fD�ffD��fD��3D�&fD�l�D���D�� D�,�D�i�D���D��fD�,�D�l�Dک�D��fD�)�D�S3D�3D�� 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��7A�ZA�Q�A�M�A�I�A�E�A�C�A�C�A�E�A�K�A�S�A�\)A�\)A�ZA�O�A�I�A�XA���A�A�A���A���A��RA��uA��/A�VA�VA�C�A�ȴA�ĜA���A�ZA��A�7LA���A��A��7Au��As�
Aq��AX1AI`BA8�yA4r�A,v�A(��A!t�A�A~�A�A�DA�wAȴA-A
�`A	�-A	�7A	+A9XAoA�@�K�@�v�@�{@��@�J@��;@�n�@�9X@�F@�E�@�X@���@�ff@���@�
=@��!@���@�{@�%@��F@���@�9X@�9X@��@�j@�G�@�-@�O�@���@���@��@z�@r�H@j^5@`��@X  @P �@J�\@Co@:�@4�@-p�@"�H@"�H@v�@t�@��@hs@��@
^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��7A�ZA�Q�A�M�A�I�A�E�A�C�A�C�A�E�A�K�A�S�A�\)A�\)A�ZA�O�A�I�A�XA���A�A�A���A���A��RA��uA��/A�VA�VA�C�A�ȴA�ĜA���A�ZA��A�7LA���A��A��7Au��As�
Aq��AX1AI`BA8�yA4r�A,v�A(��A!t�A�A~�A�A�DA�wAȴA-A
�`A	�-A	�7A	+A9XAoA�@�K�@�v�@�{@��@�J@��;@�n�@�9X@�F@�E�@�X@���@�ff@���@�
=@��!@���@�{@�%@��F@���@�9X@�9X@��@�j@�G�@�-@�O�@���@���@��@z�@r�H@j^5@`��@X  @P �@J�\@Co@:�@4�@-p�G�O�@"�H@v�@t�@��@hs@��@
^5222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;oB	{�B	u�B	u�B	w�B	x�B	y�B	y�B	y�B	y�B	z�B	{�B	}�B	�B	�B	�B	�1B	�DB
R�B
�B1BBB&�B"�By�By�BiyBgmBiyBR�B;dB�B
r�B
^5B
.B	�B	�jB	w�B	t�B	L�B��B�7BjB�B��B�5B��BɺB��B�B�B��B	B	%B	�B	49B	m�B	�{B	��B	�{B	�uB	�JB	�JB	�7B	�bB	�hB	��B	��B	�'B	�?B	�dB	�wB	��B	ȴB	��B	�B	�B	�;B	�`B	�sB	�B	�B	��B
  B
  B
B
%B
	7B
VB
oB
�B
!�B
+B
33B
=qB
B�B
J�B
Q�B
V    B
_;B
dZB
jB
t�B
t�B
x�B
{�B
� B
�B
�7B
�P111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111 B	{�B	u�B	u�B	w�B	x�B	y�B	y�B	y�B	y�B	z�B	{�B	}�B	�B	�B	�B	�1B	�PB
T�B
�BVB	7B+B,B%�Bz�B{�BjBhsBk�BT�B<jB�B
s�B
aHB
1'B	�B	B	x�B	u�B	R�B��B�PBl�B�B��B�BB��B��BB�B�#B��B	B	%B	�B	49B	m�B	�{B	��B	��B	�{B	�PB	�PB	�=B	�hB	�hB	��B	��B	�'B	�?B	�dB	�wB	��B	ȴB	��B	�B	�B	�;B	�`B	�sB	�B	�B	��B
  B
  B
B
%B
	7B
VB
oB
�B
!�B
+B
33B
=qB
B�B
J�B
Q�B
VG�O�B
_;B
dZB
jG�O�B
t�B
x�B
{�B
� B
�B
�7B
�P222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222422242222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(thisCycle), where SP is SURFACE PRESSURE from this cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(ThisCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200803071500342008030715003420080307150034200803071507122008030715071220080307150712200808110000002008081100000020080811000000  JA  ARFMdecpA5_a                                                                20080217130057  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080217130115  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080217130118  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080217130120  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080217130131  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080217130131  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080217130131  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080217130131  QCF$                G�O�G�O�G�O�              40JA  ARGQaqcp2.8a                                                                20080217130131  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080217130131  QCF$                G�O�G�O�G�O�              40JA  ARGQrqcpt16b                                                                20080217130133  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080217141711                      G�O�G�O�G�O�                JA  ARFMdecpA5_a                                                                20080221154234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.4                                                                 20080221154239  IP                  G�O�G�O�G�O�                JA  ARCArsal2.1                                                                 20080221154240  IP  PSAL            G�O�G�O�G�O�                JA  ARGQrqcppo_a                                                                20080221154240  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20080221154244  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20080221154244  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8a                                                                20080221154244  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8a                                                                20080221154244  QCF$                G�O�G�O�G�O�              40JA  ARGQaqcp2.8a                                                                20080221154244  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8a                                                                20080221154244  QCF$                G�O�G�O�G�O�              40JA  ARGQrqcpt16b                                                                20080221154245  QCP$                G�O�G�O�G�O�           10000JA  ARUP                                                                        20080221190603                      G�O�G�O�G�O�                JM  ARSQJMQC1.0                                                                 20080221052254  CF  TEMP            D�,�D�,�G�O�                JM  ARSQJMQC1.0                                                                 20080221052254  CF  PSAL            D�,�D�,�G�O�                JM  ARCAJMQC1.0                                                                 20080307150034  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20080307150034  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20080307150712  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080811000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080930080205  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080930095842                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312115748  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318063030  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318063521                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110124100904  CV  JULD            G�O�G�O�F�ޖ                JM  AREQREJM1.0                                                                 20110124100904  CF  PRES_ADJUSTED_QC@y��D�� G�O�                JM  AREQREJM1.0                                                                 20110124100904  CF  TEMP_ADJUSTED_QC@y��D�� G�O�                JM  AREQREJM1.0                                                                 20110124100904  CF  PSAL_ADJUSTED_QC@y��D�� G�O�                JA  RFMTcnvd2.1                                                                 20110201075214  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110201075328                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609101721                      G�O�G�O�G�O�                JA  ARDU                                                                        20150621120514                      G�O�G�O�G�O�                