CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   s   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2004-11-20T04:50:54Z creation;2013-09-24T05:25:47Z update;2015-06-09T19:04:38Z conversion to V3.1;     
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
_FillValue                    iArgo profile    3.1 1.2 19500101000000  5900648 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20041120045054  20150614050511  A5_28347_007                    2C  D   APEX                            1316                            013004                          846 @ӓ���`1   @ӓħ@�k@4t��E��bޏ\(��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A��Aa��A�33A�  A�ffB	��B  B0  BE33BW��Bm33B���B�ffB���B���B���B�33B���B�33B���B�ffB���BB�  C��CffC��C��CffC� C33C$� C)� C.� C3ffC8  C=L�CB�CG�CQL�C[L�CeffCo� Cy�C��fC�� C���C�� C��3C�� C��3C��3C�� C���C��3C�� C�� C C�s3C���CѦfCֳ3CۦfC���C噚C�fC��C��3C��3D� DٚD�3D� D�fD�fD�3D$� D)��D.� D3�fD8� D=ٚDBٚDG�fDL� DQٚDV� D[� D`� De�3DjٚDo�3Dt� Dy��D�)�D�c3D�� D���D�0 D�c3D�� D�� D�0 D�l�D��3D���D�0 D�ffDڣ3D�ٚD�)�D�\�D�fD�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A��Aa��A�33A�  A�ffB	��B  B0  BE33BW��Bm33B���B�ffB���B���B���B�33B���B�33B���B�ffB���BB�  C��CffC��C��CffC� C33C$� C)� C.� C3ffC8  C=L�CB�CG�CQL�C[L�CeffCo� Cy�C��fC�� C���C�� C��3C�� C��3C��3C�� C���C��3C�� C�� C C�s3C���CѦfCֳ3CۦfC���C噚C�fC��C��3C��3D� DٚD�3D� D�fD�fD�3D$� D)��D.� D3�fD8� D=ٚDBٚDG�fDL� DQٚDV� D[� D`� De�3DjٚDo�3Dt� Dy��D�)�D�c3D�� D���D�0 D�c3D�� D�� D�0 D�l�D��3D���D�0 D�ffDڣ3D�ٚD�)�D�\�D�fD�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA��mA���A���A�AμjAβ-Aΰ!AήAΩ�AΧ�AΧ�AΥ�AΙ�A΁A�E�A�A�A�oA�A��HA��FA�  A�ffA�C�A�{A�I�A��jA���A�+A�A��A�A� �A���A��HA��jA���A�ƨA�^5A�-A��A��+A�$�A{7LAm�Ab��A[�AQ��AG�AAC�A:bA4ffA-�mA'A!A�!AS�A^5A
=A�A�A�RA {@�@�hs@�@�ƨ@�Ĝ@��T@�z�@ÍP@��w@���@�@��u@�l�@�j@�/@��H@���@���@���@��`@�{@��@��7@��T@���@�$�@��u@��R@��@��R@��@�n�@|(�@r�@j��@a�7@Y%@P�9@HbN@A�7@;o@4I�@.ff@*J@%�-@!&�@j@��@��@x�@�/@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ZA��mA���A���A�AμjAβ-Aΰ!AήAΩ�AΧ�AΧ�AΥ�AΙ�A΁A�E�A�A�A�oA�A��HA��FA�  A�ffA�C�A�{A�I�A��jA���A�+A�A��A�A� �A���A��HA��jA���A�ƨA�^5A�-A��A��+A�$�A{7LAm�Ab��A[�AQ��AG�AAC�A:bA4ffA-�mA'A!A�!AS�A^5A
=A�A�A�RA {@�@�hs@�@�ƨ@�Ĝ@��T@�z�@ÍP@��w@���@�@��u@�l�@�j@�/@��H@���@���@���@��`@�{@��@��7@��T@���@�$�@��u@��R@��@��R@��@�n�@|(�@r�@j��@a�7@Y%@P�9@HbN@A�7@;o@4I�@.ff@*J@%�-@!&�@j@��@��@x�@�/@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��BBBBÖBÖBÖBŢBǮBȴB��B��B�B�B$�B^5B��B�BÖB��B�B��BJB1BB�NB��BɺB��B�Bu�B)�B��B�B�dB��B]/B
�B
��B
ZB	��B	�oB	G�B	uB�ZB�qB��B�bB�+Bo�BffBYB]/BaHBdZB`BB]/BdZBO�B@�B7LB33B/B2-B2-BI�BXBl�B�7B�B��B��B	"�B	A�B	^5B	r�B	�7B	��B	��B	�-B	�}B	ǮB	��B	��B	�/B	�;B	�`B	�B	�B	��B	��B
B
VB
�B
�B
(�B
1'B
8RB
?}B
F�B
L�B
R�B
XB
^5B
aHB
ffB
k�B
p�B
s�B
u�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��BBBBÖBÖBÖBŢBǮBȴB��B��B�B��B%�B_;B��B�BĜB�
B�B��BPB
=B+B�ZB��B��BB�Bx�B.B��B�B�qB��B`BB
��B
��B
]/B	��B	��B	I�B	�B�mB�}B��B�oB�7Bq�BhsBZB^5BbNBe`BaHB^5BffBP�BB�B8RB49B0!B49B33BI�BXBm�B�=B�B��B��B	"�B	A�B	^5B	r�B	�7B	��B	��B	�-B	�}B	ǮB	��B	��B	�/B	�;B	�`B	�B	�B	��B	��B
B
VB
�B
�B
(�B
1'B
8RB
?}B
F�B
L�B
R�B
XB
^5B
aHB
ffB
k�B
p�B
s�B
u�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200412010000002004120100000020041201000000200701270159292007012701592920070127015929200604190000002006041900000020060419000000  JA  ARFMfmtp2.1                                                                 20041120045054  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20041120045055  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20041120045547                      G�O�G�O�G�O�                JA  ARFMfmtp2.1                                                                 20041124024946  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20041124024947  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20041124025427                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20041201000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20041201000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20060419000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060906041906  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060906050437                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312120540  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318071809  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318071932                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20130912115832  CV  JULD            G�O�G�O�F��	                JM  ARCAJMTM1.0                                                                 20070127015929  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20130924052337  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20130924052547                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150609190432                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614050511                      G�O�G�O�G�O�                