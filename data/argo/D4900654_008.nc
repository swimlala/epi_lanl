CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2005-11-27T12:51:50Z creation;2012-10-19T06:14:55Z update;2015-06-07T03:17:59Z conversion to V3.1;     
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
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  4900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20051127125150  20150615220512  A5_24187_008                    2C  D   APEX                            1142                            061703                          846 @��Ʈu� 1   @�����]@C�I�^5?�d�+1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @y��A33A`  A���A���A�33B
  B  B0��BE��BZffBl��B�ffB���B�  B�33B�  B�33B���Bƙ�BЙ�B�ffB�  BB�33CffCL�C�C  C  C  C  C$� C)ffC.ffC3ffC8  C=33CB  CF�fCQ��C[ffCe� Co33Cy� C���C�� C���C���C���C��fC�� C�� C��fC�� C��fC�ffC���C�CǙ�C�s3C�ffC�ffC�ffC�fC噚C�3C�� C�� C��3D� D��D�3DٚD��D�D"S3D(��D.�3D53D;S3DA� DG�fDN�DT` DZ�fD`�fDg3DmY�Ds�3Dy� D�&fD�Y�D��fD�� D�  D�s3D��3D���D��D�i�D��fD�ٚD�)�D�p Dک�D��D�  D�VfD�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��A33A`  A���A���A�33B
  B  B0��BE��BZffBl��B�ffB���B�  B�33B�  B�33B���Bƙ�BЙ�B�ffB�  BB�33CffCL�C�C  C  C  C  C$� C)ffC.ffC3ffC8  C=33CB  CF�fCQ��C[ffCe� Co33Cy� C���C�� C���C���C���C��fC�� C�� C��fC�� C��fC�ffC���C�CǙ�C�s3C�ffC�ffC�ffC�fC噚C�3C�� C�� C��3D� D��D�3DٚD��D�D"S3D(��D.�3D53D;S3DA� DG�fDN�DT` DZ�fD`�fDg3DmY�Ds�3Dy� D�&fD�Y�D��fD�� D�  D�s3D��3D���D��D�i�D��fD�ٚD�)�D�p Dک�D��D�  D�VfD�D��3222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AsdZAsG�AshsAsl�Asl�Ast�Asx�Asx�As?}As?}Ad�RAE
=A;+A5�FA0JA.v�A,(�A* �A&�A&(�A%��A%C�A%oA%S�A%�#A'�A&�HA&�uA%��A&A�A&bNA&�A&�jA&��A&��A'��A&��A%|�A%x�A$M�A#
=A ��A��A�A��A�+A��Ap�A��AA�A^5A
�`A	%A��A�A �/@��@���@��@�  @�7@�C�@�@��@ҏ\@�5?@ȴ9@��@�n�@�"�@���@�K�@��+@��@��T@�x�@���@���@�Q�@�O�@���@���@�(�@z�\@v@q��@l�@fff@aG�@\(�@Xb@N�+@F�y@@�`@:�\@5O�@/�;@*�H@'
=@"M�@��@~�@�w@?}@�^@+@�
@�9@�T@C�@ 1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AsdZAsG�AshsAsl�Asl�Ast�Asx�Asx�As?}As?}Ad�RAE
=A;+A5�FA0JA.v�A,(�A* �A&�A&(�A%��A%C�A%oA%S�A%�#A'�A&�HA&�uA%��A&A�A&bNA&�A&�jA&��A&��A'��A&��A%|�A%x�A$M�A#
=A ��A��A�A��A�+A��Ap�A��AA�A^5A
�`A	%A��A�A �/@��@���@��@�  @�7@�C�@�@��@ҏ\@�5?@ȴ9@��@�n�@�"�@���@�K�@��+@��@��T@�x�@���@���@�Q�@�O�@���@���@�(�@z�\@v@q��@l�@fff@aG�@\(�@Xb@N�+@F�y@@�`@:�\@5O�@/�;@*�H@'
=@"M�@��@~�@�w@?}@�^@+@�
@�9@�T@C�@ 1'222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBŢBƨBŢBƨBŢBŢBŢBŢBŢBĜB��B�B:^B33BK�B\)BbNBn�Bp�By�B�B�%B�DB��B�'B��B�)B�)B�BB�B��BBJB�B#�B33B)�B&�B/B(�B6FBG�B^5B\)BhsBaHBN�BL�B:^B49B-B(�B �B{BDB	7BB��B��B�B�B�ZB�HB�/B�B�B�B�/B�HB�`B��BB
=B{B!�B1'B=qBQ�B_;Bs�B�7B��B�'BĜB�B�ZB��B		7B	�B	(�B	5?B	P�B	gmB	y�B	�PB	��B	�'B	��B	��B	�5B	�B	��B
+B
hB
�B
'�B
33B
>wB
F�B
N�B
X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BŢBƨBŢBƨBŢBŢBŢBŢBŢB��B�TB$�B=qB6FBL�B]/BcTBp�Bp�By�B�B�%B�DB��B�!B��B�)B�)B�BB�B��BBJB�B#�B49B+B&�B0!B(�B7LBH�B_;B\)BiyBbNBN�BM�B;dB5?B.B)�B!�B�BJB
=BB��B��B�B�B�`B�NB�/B�B�
B�B�5B�HB�fB��BB
=B{B!�B1'B=qBQ�B_;Bs�B�7B��B�'BĜB�B�ZB��B		7B	�B	(�B	5?B	P�B	gmB	y�B	�PB	��B	�'B	��B	��B	�5B	�B	��B
+B
hB
�B
'�B
33B
>wB
F�B
N�B
X222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200512130541242005121305412420051213054124201107110713222011071107132220110711071322200808290000002008082900000020080829000000  JA  ARFMfmtp2.2                                                                 20051127125150  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051127125150  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051127130009                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20051201045422  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20051201045423  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20051201050039                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20051213054124  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20051213054124  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127163116  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080829000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080908023654  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080908032818                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20121003105751  CV  JULD            G�O�G�O�F��9                JM  AREQREJM1.0                                                                 20121003105751  CF  PRES_ADJUSTED_QC@y��D��3G�O�                JM  AREQREJM1.0                                                                 20121003105751  CF  TEMP_ADJUSTED_QC@y��D��3G�O�                JM  AREQREJM1.0                                                                 20121003105751  CF  PSAL_ADJUSTED_QC@y��D��3G�O�                JA  RFMTcnvd2.1                                                                 20121019061054  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20121019061455                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607031753                      G�O�G�O�G�O�                JA  ARDU                                                                        20150615220512                      G�O�G�O�G�O�                