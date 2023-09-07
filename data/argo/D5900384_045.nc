CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   I   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2004-09-01T16:46:50Z creation;2011-02-14T00:48:17Z update;2015-06-08T23:53:55Z conversion to V3.1;     
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
_FillValue                    b<Argo profile    3.1 1.2 19500101000000  5900384 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  20040901164650  20150614012513  A4_19555_045                    2C  D   APEX                            782                             072602                          846 @���ή1   @��|5�@;Ffffff�d8z�G�1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A33A�ffA�  B33BE33BnffB�ffB�33B���B���Bڙ�B�33CL�CL�C33CL�C)ffC3  C=L�CG�CZ�fCo�C��3C��fC��3C�s3C�� C��3C���Cǌ�Cь�Cۙ�C��C�fC�� D�3D�3D�fD� D�fD�3D��D$��D)�3D.�fD3�3D8� D=��DB�3DG��DM�3DTFfDZ�fD`ٚDg  Dm@ Ds�fDy�3D�33D�S3D��fD�� D��D�c3D�� D�s3D��3D�p D�� D�y�D��fD���1111111111111111111111111111111111111111111111111111111111111111111111111   @�  A33A�ffA�  B33BE33BnffB�ffB�33B���B���Bڙ�B�33CL�CL�C33CL�C)ffC3  C=L�CG�CZ�fCo�C��3C��fC��3C�s3C�� C��3C���Cǌ�Cь�Cۙ�C��C�fC�� D�3D�3D�fD� D�fD�3D��D$��D)�3D.�fD3�3D8� D=��DB�3DG��DM�3DTFfDZ�fD`ٚDg  Dm@ Ds�fDy�3D�33D�S3D��fD�� D��D�c3D�� D�s3D��3D�p D�� D�y�D��fD���2222222222222222222222222222222222222222222222222222222222222222222222222   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���Aя\A�jA�?}A���A���A�jA���A��7A��A�VA�5?A�VA|bNAxM�As33AnAi�mAb�9A\bNAR�AK�;AF��A?A;&�A4��A0 �A)�A$Q�A�HAl�A�hA��A�;@���@�-@�v�@� �@��@��
@�V@��9@�dZ@���@��+@���@��;@��
@���@�~�@��F@�I�@�hs@~5?@{�m@z�@w�@r��@k"�@`Ĝ@Y%@S��@M�h@H��@;o@.�R@#dZ@&�@=q@
�!@�@��1111111111111111111111111111111111111111111111111111111111111111111111111   A���A���Aя\A�jA�?}A���A���A�jA���A��7A��A�VA�5?A�VA|bNAxM�As33AnAi�mAb�9A\bNAR�AK�;AF��A?A;&�A4��A0 �A)�A$Q�A�HAl�A�hA��A�;@���@�-@�v�@� �@��@��
@�V@��9@�dZ@���@��+@���@��;@��
@���@�~�@��F@�I�@�hs@~5?@{�m@z�@w�@r��@k"�@`Ĝ@Y%@S��@M�h@H��@;o@.�R@#dZ@&�@=q@
�!@�@��2222222222222222222222222222222222222222222222222222222222222222222222222   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�)B�JB�B�'B�Be`Be`B]/BM�B
��B
�B
��B
�-B
�hB
[#B
2-B
hB	�5B	�^B	�DB	iyB	R�B	8RB	$�B		7B�B�B�}B��B�7Bs�BW
BF�B49B�B{BVB�B�B%�B7LBN�Be`B�B��B��B�wB��B�;B��B	�B	=qB	ZB	hsB	t�B	� B	�{B	�-B	��B	�NB	�B	��B

=B
�B
/B
I�B
ZB
bNB
m�B
v�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B�BB��B��B�?B�+BhsBgmB^5BP�B
��B
�
B
��B
�3B
�oB
\)B
33B
uB	�BB	�dB	�JB	jB	S�B	9XB	%�B	
=B��B�B��B��B�=Bt�BXBG�B5?B�B�B\B�B�B%�B8RBO�BffB�B��B��B�wB��B�;B��B	�B	=qB	ZB	hsB	t�B	� B	�{B	�-B	��B	�NB	�B	��B

=B
�B
/B
I�B
ZB
bNB
m�B
v�B
w�2222222222222222222222222222222222222222222222222222222222222222222222222   <#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200409130000002004091300000020040913000000200701261129162007012611291620070126112916200411050000002004110500000020041105000000  JA  ARFMfmtp2.1                                                                 20040901164650  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20040901164650  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20040901165135                      G�O�G�O�G�O�                JA  ARFMfmtp2.1                                                                 20040905144732  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20040905144733  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20040905145346                      G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20040913000000  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC                                                                    20040913000000  IP  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 1   SeHyD1                                                          20041105000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20060131021358  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20060201010159                      G�O�G�O�G�O�                JM  AREVjmrvp1.2                                                                20090312114646  IP  PRES            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20090318055703  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20090318060115                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20110208022635  CV  JULD            G�O�G�O�F��                JM  ARCAJMTM1.0                                                                 20070126112916  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  AREQREJM1.0                                                                 20110208022635  CF  PRES_ADJUSTED_QC@�  D���G�O�                JM  AREQREJM1.0                                                                 20110208022635  CF  TEMP_ADJUSTED_QC@�  D���G�O�                JM  AREQREJM1.0                                                                 20110208022635  CF  PSAL_ADJUSTED_QC@�  D���G�O�                JA  RFMTcnvd2.1                                                                 20110214004713  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20110214004817                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150608235350                      G�O�G�O�G�O�                JA  ARDU                                                                        20150614012513                      G�O�G�O�G�O�                