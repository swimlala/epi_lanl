CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-03-30T04:53:09Z creation;2013-09-24T05:26:25Z update;2015-06-09T19:07:08Z conversion to V3.1;     
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
_FillValue                  t  ;l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  ?�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  A�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        �  B,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  C�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Dl   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  F8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       �  Hx   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  JD   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  L�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   M   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   V   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   _   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         i   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         i   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        i   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20050330045309  20150614050518  A5_28347_020                    2C  D   APEX                            1316                            013004                          846 @Ӵ@�� 1   @ӴD��	@5Լj~���b�^5?|�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�33A��AfffA�ffA���A�33B��B��B/33BD��BX  Bm��B�ffB���B�ffB���B�ffB�  B�  Bƙ�B�  B�ffB�  B�33B���C33CL�C� C33C  C  C  C$�C)L�C.33C3L�C8� C=L�CB� CG� CQ� C[ffCe  Co  Cx�fC�� C�� C���C�� C�s3C���C���C�� C��3C���C���C���C��3C³3C�s3Č�Cр C֦fC���C�3C噚CꙚC��C� C��3D��D�3D��D� DٚD��D�fD$��D)��D.ٚD3�fD8��D=�fDB��DG�fDLٚDQ� DV�3D[�3D`��DeٚDj�fDo�3Dt��Dy��D�)�D�p D��fD�� D�)�D�ffD��fD��3D�#3D�l�D���D��fD�&fD�p DڦfD��fD��D�c3D�3D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33A��AfffA�ffA���A�33B��B��B/33BD��BX  Bm��B�ffB���B�ffB���B�ffB�  B�  Bƙ�B�  B�ffB�  B�33B���C33CL�C� C33C  C  C  C$�C)L�C.33C3L�C8� C=L�CB� CG� CQ� C[ffCe  Co  Cx�fC�� C�� C���C�� C�s3C���C���C�� C��3C���C���C���C��3C³3C�s3Č�Cр C֦fC���C�3C噚CꙚC��C� C��3D��D�3D��D� DٚD��D�fD$��D)��D.ٚD3�fD8��D=�fDB��DG�fDLٚDQ� DV�3D[�3D`��DeٚDj�fDo�3Dt��Dy��D�)�D�p D��fD�� D�)�D�ffD��fD��3D�#3D�l�D���D��fD�&fD�p DڦfD��fD��D�c3D�3D�L�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�/A� �A�A��-A��uA��7A��A��+A�~�A�~�A�~�A��A��A�~�A�~�A��A��A��A�v�A�7LA�ƨA���A�p�A��A�JA�A�hsA�(�A���A���A�v�A�?}A��wA��A���A��FA��;A��A�t�A�?}A�ĜA��^A�9XA�&�A��A���Ar�Aj1A[33AK��AD�uA=O�A8�DA2�HA-�#A'�FA#oA  �AJAO�A$�A&�A�A33A�wA (�@��@��R@��#@݁@�1@̋D@�%@�A�@��@�`B@�z�@�~�@��+@���@���@��@�j@�E�@�bN@��h@���@�@�Z@�J@�Q�@��-@�+@��7@��@~�y@v{@oK�@f{@]�@Wl�@N��@D�@?l�@9�7@1G�@,�@'
=@#o@5?@�@`B@K�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�/A� �A�A��-A��uA��7A��A��+A�~�A�~�A�~�A��A��A�~�A�~�A��A��A��A�v�A�7LA�ƨA���A�p�A��A�JA�A�hsA�(�A���A���A�v�A�?}A��wA��A���A��FA��;A��A�t�A�?}A�ĜA��^A�9XA�&�A��A���Ar�Aj1A[33AK��AD�uA=O�A8�DA2�HA-�#A'�FA#oA  �AJAO�A$�A&�A�A33A�wA (�@��@��R@��#@݁@�1@̋D@�%@�A�@��@�`B@�z�@�~�@��+@���@���@��@�j@�E�@�bN@��h@���@�@�Z@�J@�Q�@��-@�+@��7@��@~�y@v{@oK�@f{@]�@Wl�@N��@D�@?l�@9�7@1G�@,�@'
=@#o@5?@�@`B@K�@��2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��BɺB��BȴBȴBȴBȴBɺBɺBȴBɺBɺBɺBȴB��B��B��B��B�
B��BR�Bk�B�%B��B��B�FB�qBŢB��B�B�NB�B��BB�B%�B�B�HBƨB�XBv�B�B�5B�bB8RB
B
}�B	�jB	m�B	�B��B�-B��B��B��B��B�+B|�Br�BffBbNB`BBXBR�BQ�BVBW
BS�BO�BN�BK�BP�Be`B�B�BɺB�NB��B	�B	>wB	XB	n�B	�B	�DB	��B	��B	�'B	�XB	��B	�B	�ZB	�sB	�B	��B	��B
B
PB
�B
�B
$�B
-B
33B
;dB
D�B
H�B
M�B
VB
ZB
`BB
cTB
hsB
n�B
q�B
x�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��BɺB��BȴBȴBȴBȴBɺBɺBȴBɺBɺBɺBȴB��B��B��B��B�
B��BR�Bl�B�+B��B��B�FB�qBŢB��B�B�NB�B��BB�B&�B�B�ZBǮB�dBy�B�B�HB�oB;dB
ĜB
�B	�wB	q�B	�B��B�9B��B��B��B��B�1B}�Bs�BgmBcTBaHBZBS�BR�BW
BXBT�BP�BO�BL�BQ�BffB�B�BɺB�NB��B	�B	>wB	XB	n�B	�B	�DB	��B	��B	�'B	�XB	��B	�B	�ZB	�sB	�B	��B	��B
B
PB
�B
�B
$�B
-B
33B
;dB
D�B
H�B
M�B
VB
ZB
`BB
cTB
hsB
n�B
q�B
x�B
z�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200504100000002005041000000020050410000000201107080751492011070807514920110708075149201309120000002013091200000020130912000000  JA  ARFMfmtp2.2                                                                 20050330045309  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050330045310  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050330045828                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20050403005242  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20050403005243  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20050403005744                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050410000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20050410000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060906041913  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060906050442                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120543  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071852  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318072011                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20130912000000  CV  JULD            G�O�G�O�F��                JM  ARCAJMTM1.0                                                                 20110708075149  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  PRES_ADJUSTED_QC@�33D�L�G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  TEMP_ADJUSTED_QC@�33D�L�G�O�                JM  AREQREJM1.0                                                                 20130912000000  CF  PSAL_ADJUSTED_QC@�33D�L�G�O�                JA  RFMTcnvd2.1                                                                 20130924052435  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052625                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609190702                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614050518                      G�O�G�O�G�O�                