CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   o   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JAMSTEC    source        
Argo float     history       b2006-02-15T18:52:38Z creation;2012-10-19T06:15:57Z update;2015-06-07T03:19:37Z conversion to V3.1;     
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
_FillValue                    hlArgo profile    3.1 1.2 19500101000000  4900654 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  20060215185238  20150615220518  A5_24187_016                    2C  D   APEX                            1142                            061703                          846 @�Ɨ�%�1   @�ǝd��@C���R�c񙙙��1   ARGOS   A   A   A   Primary sampling: discrete [spot sampling CTD data]                                                                                                                                                                                                                @�  A33Ad��A���A���A�ffB��B��B1��BC33BX  Bm��B�33B���B���B�ffB���B���B���Bƙ�B�  Bڙ�B���B�  B���C33C  C  C33C33C�C  C#�fC)  C.  C3�C833C=L�CA�3CF�fCQ�C[33Ce� Co33Cy�C��3C�� C���C���C���C�� C�� C���C��fC���C�s3C�Y�C��fC�C�s3C̳3CѦfCֳ3CۦfC���C�� CꙚC�fC�s3C���D��DٚD��D�3D��D  D"S3D(y�D.�fD5fD;Y�DA�fDG�fDN�DTFfDZ�3D`��Dg  DmS3Ds� DyٚD�&fD�VfD�� D��3D�)�D�i�D���D��fD�&fD�p D�� D��3D�)�D�i�Dک�D��3D�#3D�\�D� D�L�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  A33Ad��A���A���A�ffB��B��B1��BC33BX  Bm��B�33B���B���B�ffB���B���B���Bƙ�B�  Bڙ�B���B�  B���C33C  C  C33C33C�C  C#�fC)  C.  C3�C833C=L�CA�3CF�fCQ�C[33Ce� Co33Cy�C��3C�� C���C���C���C�� C�� C���C��fC���C�s3C�Y�C��fC�C�s3C̳3CѦfCֳ3CۦfC���C�� CꙚC�fC�s3C���D��DٚD��D�3D��D  D"S3D(y�D.�fD5fD;Y�DA�fDG�fDN�DTFfDZ�3D`��Dg  DmS3Ds� DyٚD�&fD�VfD�� D��3D�)�D�i�D���D��fD�&fD�p D�� D��3D�)�D�i�Dک�D��3D�#3D�\�D� D�L�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A(jA(-A'�mA'�A'�^A'XA'7LA'33A&�A'�A(bA(�+A(��A(��A(��A(�+A(E�A(1'A(-A((�A(�A(  A'�TA'�
A'��A'�;A(n�A(ȴA'&�A'VA&jA%+A$Q�A#�TA#XA#oA"�9A �AoAI�A��AXA$�AC�AQ�A�Ar�A\)A
��AAAoA�A b@���@��7@�Q�@�X@�j@�~�@�?}@�  @�b@�C�@Ϯ@�G�@ɡ�@�n�@��^@�=q@���@�@��\@��j@�dZ@��@��@�=q@���@��H@��u@���@}@x  @q��@j��@e�T@a��@^{@Y�7@U`B@M?}@EO�@?�P@9�^@3�
@.v�@*-@'
=@"��@5?@t�@\)@t�@b@��@	��@��@�F@�7?�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A(jA(-A'�mA'�A'�^A'XA'7LA'33A&�A'�A(bA(�+A(��A(��A(��A(�+A(E�A(1'A(-A((�A(�A(  A'�TA'�
A'��A'�;A(n�A(ȴA'&�A'VA&jA%+A$Q�A#�TA#XA#oA"�9A �AoAI�A��AXA$�AC�AQ�A�Ar�A\)A
��AAAoA�A b@���@��7@�Q�@�X@�j@�~�@�?}@�  @�b@�C�@Ϯ@�G�@ɡ�@�n�@��^@�=q@���@�@��\@��j@�dZ@��@��@�=q@���@��H@��u@���@}@x  @q��@j��@e�T@a��@^{@Y�7@U`B@M?}@EO�@?�P@9�^@3�
@.v�@*-@'
=@"��@5?@t�@\)@t�@b@��@	��@��@�F@�7?�V222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B��B��B��B��B��B�)B�mB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	7B1'B�bB�^B	7B1'BL�BXBVBl�Bt�Bo�Be`BdZBXBZB_;BVBI�BC�B7LB.B(�B�B�B�BoB\BPB\BJBB��B�B�B�mB�ZB�;B�5B�TB�ZB�`B�yB�B��BB\B�B%�B7LBH�BT�BiyB�B�oB��B�jB��B�ZB��B	
=B	�B	!�B	0!B	;dB	S�B	k�B	|�B	�bB	��B	�jB	��B	�B	�sB	��B
B
hB
�B
(�B
49B
<jB
D�B
M�B
T�B
[#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B��B��B��B��B��B�)B�mB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	7B2-B�bB�^B
=B1'BL�BXBVBl�Bu�Bp�BffBe`BXBZB`BBW
BI�BD�B8RB.B)�B�B�B�BuB\BPBbBPBB��B��B�B�sB�`B�BB�5B�ZB�ZB�fB�yB�B��BBbB�B%�B7LBH�BT�BiyB�B�oB��B�jB��B�ZB��B	
=B	�B	!�B	0!B	;dB	S�B	k�B	|�B	�bB	��B	�jB	��B	�B	�sB	��B
B
hB
�B
(�B
49B
<jB
D�B
M�B
T�B
[#222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ = PRES-SP(NextCycle), where SP is SURFACE PRESSURE from next cycle                                                                                                                                                                                     TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,PRES_ADJ,elapsed_time,alpha,tau),elapsed_time=P/mean_rise_rate,P=dbar since the start of the profile for each samples                                                                                                       None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            SP(NextCycle)=0.0(dbar)                                                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE; PRES_ADJ_ERR : Manufacture sensor accuracy                                                                                                                                                                 TEMP_ADJ_ERR : SBE sensor accuracy                                                                                                                                                                                                                              Salinity Recalculation using PRES_ADJ; PSAL_ADJ_ERR : max(sum of RecalS & CTM errors, 0.01(PSS-78))                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            TNPD: APEX float that truncated negative pressure drift                                                                                                                                                                                                         None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         200602282315082006022823150820060228231508201107082344562011070823445620110708234456200808290000002008082900000020080829000000  JA  ARFMfmtp2.2                                                                 20060215185238  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060215185238  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060215190351                      G�O�G�O�G�O�                JA  ARFMfmtp2.2                                                                 20060219025238  IP                  G�O�G�O�G�O�                JA  ARGQrqcp2.3                                                                 20060219025238  QCP$                G�O�G�O�G�O�           1FB7CJA  ARUP                                                                        20060219025920                      G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060228231508  IP  PRES            G�O�G�O�G�O�                JM  ARCAJMQC1.0                                                                 20060228231508  IP  PSAL            G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20070127163159  CV  PSAL            G�O�G�O�G�O�                JM  ARSQWJO 2.0 SeHyD1.0                                                        20080829000000  IP  PSAL            G�O�G�O�G�O�                JA  RFMTcnvd2.1                                                                 20080908023658  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20080908033203                      G�O�G�O�G�O�                JM  ARGQREJM1.0                                                                 20121003105812  CV  JULD            G�O�G�O�F�&8                JM  AREQREJM1.0                                                                 20121003105812  CF  PRES_ADJUSTED_QC@�  D�L�G�O�                JM  AREQREJM1.0                                                                 20121003105812  CF  TEMP_ADJUSTED_QC@�  D�L�G�O�                JM  AREQREJM1.0                                                                 20121003105812  CF  PSAL_ADJUSTED_QC@�  D�L�G�O�                JA  RFMTcnvd2.1                                                                 20121019061348  IP                  G�O�G�O�G�O�                JA  ARDU                                                                        20121019061557                      G�O�G�O�G�O�                JM      JMFC1.0                                                                 20150607031930                      G�O�G�O�G�O�                JA  ARDU                                                                        20150615220518                      G�O�G�O�G�O�                