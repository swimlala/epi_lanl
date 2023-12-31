CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   c   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2009-11-08T03:55:21Z creation;2015-04-24T02:28:17Z conversion to V3.1;2019-04-22T03:37:24Z update;     
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
_FillValue                  d  ;(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  =   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  =|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  ?   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ?l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  @�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  A\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  B�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  CL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  d  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  HT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  I�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   Jp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   Sp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   \p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ep   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  f    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    fP   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    fT   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         fd   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fh   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fl   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    fpArgo profile    3.1 1.2 19500101000000  20091108035521  20190423051516  2900548 Argo eq. HNFRI                                                  Akira Kusaka                                                    PRES            TEMP            PSAL               �A   JA  A3_030280_158                   2C  D   APEX                            1602                            070204                          846 @�Y5��1   @�Y6�n�@C����m�c�����1   ARGOS   A   A   A   Primary sampling: discrete [1 Hz CTD subsampled]                                                                                                                                                                                                                   @���A��A[33A���A�  A�33B  B33B2  BDffBY33Bm33B�  B���B�ffB�  B���B���B���B�  B���B�ffB���BB�  C ��CffC33C��CL�C�C�C$L�C)33C.33C3ffC8L�C=L�CBL�CF�fCLL�CQ�CU�fCZ��C_��Cd��Ci�fCo�CtL�Cx�3C}�fC�� C��C��fC�ٚC�s3C�  C���C�&fC�Y�C���C���C�� C�s3Cǀ Cѳ3CۦfC�3C�fC��fD�3D�fD�3D�3D� D� DٚD$ٚD)� D.�3D3�fD8�fD=� DB�fDG�3DL� DQ�3DV�fD[��D`�3De�fDj� DoٚDt� Dy�fD�#3D�ffD���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��AffAT��A���A���A�  BffB��B0ffBB��BW��Bk��B�33B�  B���B�33B�  B���B���B�33B�  Bٙ�B�  B���B�33C ffC  C
��C33C�fC�3C�3C#�fC(��C-��C3  C7�fC<�fCA�fCF� CK�fCP�3CU� CZffC_ffCdffCi� Cn�3Cs�fCxL�C}� C�L�C��fC�s3C��fC�@ C���C�Y�C��3C�&fC���C�Y�C���C�@ C�L�Cр C�s3C� C�s3C�s3D��D��D��D��D�fD�fD� D$� D)�fD.��D3��D8��D=�fDB��DG��DL�fDQ��DV��D[� D`��De��Dj�fDo� Dt�fDy��D�fD�Y�D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Al��Al��Al�uAl��Al��Al��Al��Al��Al��Al��Al�Al�!Al�Ae�wAC�-A?�TA;�A:v�A9�A9?}A9oA7%A5dZA4��A2�/A0�+A0r�A0��A/C�A1�A/��A/��A.��A,��A+G�A,Q�A+%A*(�A(VA'O�A%A$��A#��A"��A!��A!XA�mA��A �Ax�A~�AAȴA�A/A(�A�\A��AhsAA�HAp�A�HA��@��\@���@��H@��@��@őh@���@���@�z�@�V@��@��@��D@� �@�  @��@�X@�7L@���@��u@�$�@�w@{ƨ@v�y@rn�@nV@i��@f�+@cC�@_;d@[33@R��@J��@CS�@<�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Al��Al��Al�uAl��Al��Al��Al��Al��Al��Al��Al�Al�!Al�Ae�wAC�-A?�TA;�A:v�A9�A9?}A9oA7%A5dZA4��A2�/A0�+A0r�A0��A/C�A1�A/��A/��A.��A,��A+G�A,Q�A+%A*(�A(VA'O�A%A$��A#��A"��A!��A!XA�mA��A �Ax�A~�AAȴA�A/A(�A�\A��AhsAA�HAp�A�HA��@��\@���@��H@��@��@őh@���@���@�z�@�V@��@��@��D@� �@�  @��@�X@�7L@���@��u@�$�@�w@{ƨ@v�y@rn�@nV@i��@f�+@cC�@_;d@[33@R��@J��@CS�@<�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�B�!B�dB��B��B�B�yB��BBB��B��B�B�B��BbB'�BS�BS�Bl�Bx�Bu�Bu�B��B��B��B�hB�=B�B� B|�Bx�Bq�Bn�BgmBgmBdZBcTB_;BbNB`BB]/B\)BW
BN�BF�B=qB;dB;dB@�B5?B$�B�BB�B�;B�/B�mB�B�B��B	7B{B&�B8RBH�BZBe`Bu�B�B�uB��B�B�qBȴB�B�mB��B	B	VB	�B	$�B	1'B	J�B	cTB	y�B	�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BkQBkQBkQBk6BkQBk6BkQBk6BkQBkQBkQBk�Bo�B�FB�AB�fB��B�"BݲB�|B�rB�+B�B�IB�XB�B�IBmBqBH�BH�Ba�BnIBj�Bi�B��B��B��B��B}BxBu%Bq�Bm�BffBc�B\xB\CBYKBXEBTBWsBU2BR:BQBLdBDMB;�B2GB0oB0oB5�B*eBB
�B�fB��B�{B�:B�xB��B�B��B�(B	lB�B-CB=�BOBZQBj�By�B�B�gB��B�B�VB̳B�B�_B��B	�B	B	B	%�B	?}B	W�B	ncB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�3F<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.4(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9997(+-0.0000), deepest deltaS=0.011(+-0.001)(PSS-78); Mapping scale = 8/4,4/2; 0-800(dbar) is excluded in mapping; Use P>(dbar) Use P<(dbar) Use THETA<(deg.C) Use THETA>(deg.C)                                                                           Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      201611102118452016111021184520161110211845201611110209352016111102093520161111020935201904221201142019042212011420190422120114  JA  ARFMdecpA3_c                                                                20091108035520  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20091108035521  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091108035522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091108035523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091108035524  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091108035524  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091108035524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091108035524  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091108035524  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091108040245                      G�O�G�O�G�O�                JA  ARFMdecpA3_c                                                                20091111215416  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20091111215820  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.0                                                                 20091111215821  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20091111215822  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19b                                                                20091111215823  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19b                                                                20091111215823  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8c                                                                20091111215823  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8c                                                                20091111215823  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20091111215824  QCP$                G�O�G�O�G�O�               0JA  ARUP                                                                        20091111220322                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424022817                      G�O�G�O�G�O�                JA  ARUP                                                                        20150427114516                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121845  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161110121845  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20161110170935  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190422030114  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20190423051516                      G�O�G�O�G�O�                