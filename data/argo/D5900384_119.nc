CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   I   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-09-11T18:49:58Z creation;2011-02-14T01:33:06Z update;2015-06-09T00:08:22Z conversion to V3.1;     
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
resolution        =���   standard_name         sea_water_pressure     axis      Z        $  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  :�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   standard_name         sea_water_pressure       $  ;   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  <4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  <�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  =�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  >�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_temperature        $  ?   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  @8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  @�   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  A�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   standard_name         sea_water_salinity       $  C   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  L  D<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     $  D�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  E�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   F<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   O<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   X<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  a<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    a�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    a�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    a�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    a�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  a�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    b   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900384 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               wA   JA  20060911184958  20150614014518  A4_19555_119                    2C  D   APEX                            782                             072602                          846 @�8���n1   @�8�d��@;�I�^5?�d���$�1   ARGOS   A   B   B   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @y��A  A�ffA���B��BE33Bn  B���B�33B�  B���Bڙ�B�  C�C  CffCL�C)ffC3ffC=L�CG  CZ�fCo33C���C�� C�ٚC��3C��fC���C��3Cǳ3Cр Cۙ�C�� C�fC�ffD� D��D�fD� D�fD��DٚD$��D)� D.ٚD3��D8��D=��DB�3DG� DN�DT33DZ��D`��DgfDml�Ds�3Dy� D�0 D�i�D��3D��fD�&fD�y�D���D�i�D�ٚD�S3D�� D�S3D���D�&f1111111111111111111111111111111111111111111111111111111111111111111111111   @y��A  A�ffA���B��BE33Bn  B���B�33B�  B���Bڙ�B�  C�C  CffCL�C)ffC3ffC=L�CG  CZ�fCo33C���C�� C�ٚC��3C��fC���C��3Cǳ3Cр Cۙ�C�� C�fC�ffD� D��D�fD� D�fD��DٚD$��D)� D.ٚD3��D8��D=��DB�3DG� DN�DT33DZ��D`��DgfDml�Ds�3Dy� D�0 D�i�D��3D��fD�&fD�y�D���D�i�D�ٚD�S3D�� D�S3D���D�&f2222222222222222222222222222222222222222222222222222222222222222222222222   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�jA�&�A�%Aϡ�A���A���A���A��hA�jA�x�A��RA�K�A��wA�G�A�9XA��DA}G�Aux�As�FAn��AeK�A_�#AW|�AQ�AM�wAB-A9C�A4��A,�A&�A9XA��A^5A	��A�@�5?@�hs@��@ϥ�@�;d@�&�@��-@�ȴ@��@��@�hs@�(�@��@��@�{@�  @��D@{33@y7L@v��@st�@m�@kt�@d��@^ff@W\)@R=q@K�m@D��@4(�@&ff@��@�u@	��@��?���?���1111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�jA�&�A�%Aϡ�A���A���A���A��hA�jA�x�A��RA�K�A��wA�G�A�9XA��DA}G�Aux�As�FAn��AeK�A_�#AW|�AQ�AM�wAB-A9C�A4��A,�A&�A9XA��A^5A	��A�@�5?@�hs@��@ϥ�@�;d@�&�@��-@�ȴ@��@��@�hs@�(�@��@��@�{@�  @��D@{33@y7L@v��@st�@m�@kt�@d��@^ff@W\)@R=q@K�m@D��@4(�@&ff@��@�u@	��@��?���G�O�2222222222222222222222222222222222222222222222222222222222222222222222224   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�B�VB�PB�=B�+BjB�}B��BXB9XB��B��B�\B<jBoB
�B
�-B
��B
x�B
W
B
Q�B
/B
�B	��B	��B	��B	��B	_;B	,B	�B��B��B�B�uBw�BdZBO�BA�B49B+B#�B#�B'�B/B<jBE�BN�B`BBo�B�B��B�B�B��B	�B	%�B	9XB	B�B	e`B	z�B	��B	B	�B	�mB	��B
B
!�B
7LB
K�B
YB
dZB
p�B
y�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111   B�VB�PB�=B�1Bs�BB��BZB;dB  B��B�oB>wB{B
�B
�3B
��B
z�B
XB
R�B
0!B
�B	��B	��B	��B	��B	`BB	-B	�B��B�B�!B�{Bx�Be`BP�BB�B5?B,B$�B$�B(�B0!B<jBE�BO�B`BBo�B�B��B�B�B��B	�B	%�B	9XB	B�B	e`B	z�B	��B	B	�B	�mB	��B
B
!�B
7LB
K�B
YB
dZB
p�B
y�G�O�2222222222222222222222222222222222222222222222222222222222222222222222224   <#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200609250547132006092505471320060925054713200701261135462007012611354620070126113546200707020000002007070200000020070702000000  JA  ARFMfmtp2.3                                                                 20060911184958  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060911184959  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060911184959  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060911184959  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060911185947                      G�O�G�O�G�O�                JA  ARFMfmtp2.3                                                                 20060915154021  IP                  G�O�G�O�G�O�                JA  ARCAsspa2.1                                                                 20060915154022  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcp2.5                                                                 20060915154022  QCP$                G�O�G�O�G�O�           1FB7CJA  ARGQaqcp2.5                                                                 20060915154022  QCP$                G�O�G�O�G�O�           1FB40JA  ARUP                                                                        20060915154829                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060925054713  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060925054713  CV  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070126113546  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20070702000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20071001051451  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20071001071010                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114716  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318055810  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318060156                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110209080653  CV  JULD            G�O�G�O�F��                JM  AREQREJM1.0                                                                 20110209080653  CF  PRES_ADJUSTED_QC@y��D�&fG�O�                JM  AREQREJM1.0                                                                 20110209080653  CF  TEMP_ADJUSTED_QC@y��D�&fG�O�                JM  AREQREJM1.0                                                                 20110209080653  CF  PSAL_ADJUSTED_QC@y��D�&fG�O�                JA  RFMTcnvd2.1                                                                 20110214013204  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110214013306                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609000812                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614014518                      G�O�G�O�G�O�                