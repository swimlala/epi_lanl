CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:26:40Z creation;2022-06-04T19:26:41Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192640  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               [A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ي);*1   @ي)��b�@,k��Q��d|�hs1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�ffB���B�33B�  B�  B�  B�  B�33B�ffB�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B���B���C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJL�CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B@33BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB�� B��3B��fB��fB��fB�L�B�� B��B��fB��fB��fB��fB��B�L�B��fB��B��B׳3B��fB��fB��fB��fB��fB��fB��fB��B�� B��3C�3CٙC�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C �C"�C#ٙC%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CJ@ CKٙCM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DPvgDP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�L�A�OA�PHA�P�A�Q�A�R�A�S�A�WsA�YA�YA�W?A�YA�Y�A�X�A�ZA�[�A�^jA�`BA�a�A�b�A�e`A�f2A�f�A�h�A�iyA�jA�jA�k�A�k�A�g�A�d�A�]�A�S�A�1�Aӫ6AӆYA�|A�4A�o A���A�}�A�AUA��RA�v`A���A��A��fA�C�A���A��;A�poA���A�g8A�2aA���A�49A���A�=A�_A��LA�H�A���A��uA��6A��A��$A�ݘA�
�A���A��A���A��9A���A��*A���A��TA��~A���A��A���A�;A}6�Ay��Au33ArAn��Ak[�Ag��Ac!�A`�A[}�AW��ASیARYKAN�AJ�XAG�AAD&A@�RA?�A>�A:�gA9��A8�A8��A91�A9��A9��A8#�A5�A3�MA2�A1c�A1�A/�|A.�5A.!�A-�KA,�XA+V�A*�A)_pA(}VA(8�A'�A(A)��A(f�A&�ZA%��A$��A$��A$�A#�=A"�PA!�.A!��A!��A!xA!HA ߤA��A�qA��AFA�KA��A`�AѷAx�AP�A�AϫAn/A�A��Aa|A%�A�`A��AĜA�FAU2AYA�mA��A֡AqA��AU2A��A��A��A��A<�A��A�hA^�AMjA|�A+A{JAJA�jA]�Ag�A�'A��AO�A�)A\�A�A}�A�AcA�.A҉A�|A�[A��A�4AN<A'�AA
�wA
�A	�{A�&A�<A��A�}AJ#A�A7A�A�YA��Ag8A��An�A?�AںA�VA�rA}�AY�A/�A�zAA�+A ͟A tTA aA +@���@�u�@��@�y>@��a@�
=@��r@�ϫ@��@�{@��q@�C�@��@�u%@�($@��-@�=@���@�Ta@�&�@��
@�o @�@��j@��@�@��@�k@�e,@�O�@�}V@�G�@��@�S�@��>@�m]@��@���@�H�@��B@�=q@�_@�hs@�GE@�6�@�E�@�$@�t@��@�A@��@㙚@���@�R�@�H@�x@��@�(�@ߍP@��@���@�?}@��@�:�@�J@���@۾w@��@��@�|@��@��@ؿ�@تe@ؚ�@؃�@�4n@ּj@�n/@��E@� �@�e,@�W?@��5@�@�@���@���@�S�@о�@��@�n/@��?@�7@��@�H@ͩ�@�~�@�B�@�!�@�;@�q�@�A�@��@˛=@�F@ʚ@���@�{J@��@��@ȑ @�Ov@�!�@��@�K�@��@ī6@�l"@���@�Y�@ªe@�T�@���@��+@�dZ@���@�oi@���@��@��e@�M@���@���@�dZ@���@�V@�$�@��@�J#@��M@�g8@��@��=@�:�@��m@���@�)�@��m@��S@�@���@��o@�C-@�.I@��o@�,=@�;d@��`@�s�@�6@�@��@�c@��@���@��@��@���@��t@��P@��@�	@��Z@��H@�]�@��@�Xy@�M@�˒@��n@�`B@��@��s@���@���@�%�@��@���@��4@�e,@�Z�@�H�@��@��@��1@�ƨ@��@�Ov@�x�@��P@�ߤ@�E�@�� @��@@�K�@�Q�@�	�@���@���@�IR@��@��/@�c�@��@�!@�@���@�o @���@���@���@�j@��Q@�~�@�4�@��r@��@���@���@�a@�#�@� i@��p@���@�N�@��@���@�+�@��M@���@�C-@��3@�]�@��@��@�z�@� �@���@��@�W?@��@���@���@��1@���@���@�e�@�V�@�O@�خ@��@�Y�@�@��f@���@�,=@�ϫ@��~@�\�@�$t@��5@��B@��9@��@�M@�v`@�T�@�B�@�:�@� \@��@��@��@��@�e�@�"h@�ݘ@��M@�t�@�f�@�7L@�(�@��@���@���@��@�r�@�g8@�C-@���@���@���@���@�Vm@�&�@��8@�֡@��9@�\�@��@��@���@��@@��7@�O�@�,�@���@�n�@�4n@���@���@��X@���@�o @�q@���@���@�Ta@��@��@���@���@��f@�zx@�/@��"@��@�ں@���@���@�^5@�/�@��@�_@���@��S@�RT@�,�@�@�ȴ@�g8@�'R@��@�@~��@~�@}��@}��@}c�@}�@|�z@|�@|`�@|�@{��@{�;@{K�@zl�@y�@y�N@yF@x��@x~@wZ�@v��@v�@u��@uc@u@@tɆ@t7�@tx@s˒@s�P@sU�@r�@r�@q�j@qp�@p��@p�@o� @o�:@oj�@o�@n��@n�\@nH�@m�@mL�@l��@l�Y@lq@lm�@l>B@k��@k/�@j��@jOv@jJ@i�9@iN<@i�@h�/@hy>@hG@g��@gdZ@g�@f��@f�@e��@ec�@e�@dѷ@dy>@c�@cv`@c�@b��@bv�@bYK@ae,@`�@`ѷ@`��@`Z@_�]@_� @_��@_e�@_C�@_@^�c@^GE@]�'@]�@\�@\(�@[� @[~�@[K�@[Y@Z��@Ze@Y�@Y�-@Y2a@X��@X��@X<�@W��@WMj@V�F@V@U�Z@U��@U�@T�I@T`�@T>B@T�@S�@S��@SF�@R�y@R�r@R�@Q��@Q��@Q}�@P��@Pu�@P'R@O��@Oqv@Ot�@OU�@OY@O�@N�c@Nl�@N3�@M�9@M��@M�S@L��@L�Y@Lh�@L[�@LQ�@LK^@L?�@K�0@KZ�@K�@J�X@J�b@J�A@J@I��@I�@H�?@HQ�@G�
@G@F�,@F��@F�R@F��@FYK@F�@E8�@D�5@D`�@D!@D�@C�@C��@Cg�@CA�@C+@CC@B�@B��@B��@B҉@B�@B�m@B��@B�6@BV@A�7@A&�@@��@@�E@@�O@@?�@?�@?v`@?o@>��@>c @>u@=��@=��@=Dg@=�@<�@;�@:�@:�@:6�@9�-@9c@9j@9!�@8��@7� @7U�@6��@6Ov@5��@5hs@5�@4��@4V�@3��@3��@3�@2�@2��@2��@2s�@2&�@1�@1[W@1;@0��@0~(@0,=@/��@/��@/@O@.�M@.��@.h
@.GE@.�@-�^@-��@-@@,�p@,!@+�*@+v`@+&@*�@*u@)�h@)N<@)@(Ĝ@(Q�@'�A@'x@';d@'
=@&�y@&��@&R�@&
�@%��@%J�@$�P@$�@$H@$7@#�@#��@#iD@"�@"��@"~�@"0U@"!�@!��@!�C@!j@!V@ ��@ Z@ 7�@��@��@l�@�@��@Ta@��@�d@��@X@-w@��@�/@�[@�@��@�j@��@[�@�@��@�
@�@��@g�@Y@ȴ@�}@�@��@d�@+k@u@��@e,@�@�@|�@m�@_@PH@(�@�0@�$@��@b�@]�@>�@!-@�@��@�6@�@:*@��@��@��@��@x�@Y�@N<@F@<6@+�@�@	l@�@�U@�@��@_@!@�@�*@~�@b�@X�@A�@&@
=@�@�"@��@�r@~�@~�@H�@	@	@ԕ@�M@B�@�@!�@!�@+@��@��@��@��@_@1@�}@��@&@�@��@�@q�@0U@@��@�n@�@rG@-w@�@�@ی@֡@ѷ@�)@��@�.@~(@j@A�@%�@@�
@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�L�A�OA�PHA�P�A�Q�A�R�A�S�A�WsA�YA�YA�W?A�YA�Y�A�X�A�ZA�[�A�^jA�`BA�a�A�b�A�e`A�f2A�f�A�h�A�iyA�jA�jA�k�A�k�A�g�A�d�A�]�A�S�A�1�Aӫ6AӆYA�|A�4A�o A���A�}�A�AUA��RA�v`A���A��A��fA�C�A���A��;A�poA���A�g8A�2aA���A�49A���A�=A�_A��LA�H�A���A��uA��6A��A��$A�ݘA�
�A���A��A���A��9A���A��*A���A��TA��~A���A��A���A�;A}6�Ay��Au33ArAn��Ak[�Ag��Ac!�A`�A[}�AW��ASیARYKAN�AJ�XAG�AAD&A@�RA?�A>�A:�gA9��A8�A8��A91�A9��A9��A8#�A5�A3�MA2�A1c�A1�A/�|A.�5A.!�A-�KA,�XA+V�A*�A)_pA(}VA(8�A'�A(A)��A(f�A&�ZA%��A$��A$��A$�A#�=A"�PA!�.A!��A!��A!xA!HA ߤA��A�qA��AFA�KA��A`�AѷAx�AP�A�AϫAn/A�A��Aa|A%�A�`A��AĜA�FAU2AYA�mA��A֡AqA��AU2A��A��A��A��A<�A��A�hA^�AMjA|�A+A{JAJA�jA]�Ag�A�'A��AO�A�)A\�A�A}�A�AcA�.A҉A�|A�[A��A�4AN<A'�AA
�wA
�A	�{A�&A�<A��A�}AJ#A�A7A�A�YA��Ag8A��An�A?�AںA�VA�rA}�AY�A/�A�zAA�+A ͟A tTA aA +@���@�u�@��@�y>@��a@�
=@��r@�ϫ@��@�{@��q@�C�@��@�u%@�($@��-@�=@���@�Ta@�&�@��
@�o @�@��j@��@�@��@�k@�e,@�O�@�}V@�G�@��@�S�@��>@�m]@��@���@�H�@��B@�=q@�_@�hs@�GE@�6�@�E�@�$@�t@��@�A@��@㙚@���@�R�@�H@�x@��@�(�@ߍP@��@���@�?}@��@�:�@�J@���@۾w@��@��@�|@��@��@ؿ�@تe@ؚ�@؃�@�4n@ּj@�n/@��E@� �@�e,@�W?@��5@�@�@���@���@�S�@о�@��@�n/@��?@�7@��@�H@ͩ�@�~�@�B�@�!�@�;@�q�@�A�@��@˛=@�F@ʚ@���@�{J@��@��@ȑ @�Ov@�!�@��@�K�@��@ī6@�l"@���@�Y�@ªe@�T�@���@��+@�dZ@���@�oi@���@��@��e@�M@���@���@�dZ@���@�V@�$�@��@�J#@��M@�g8@��@��=@�:�@��m@���@�)�@��m@��S@�@���@��o@�C-@�.I@��o@�,=@�;d@��`@�s�@�6@�@��@�c@��@���@��@��@���@��t@��P@��@�	@��Z@��H@�]�@��@�Xy@�M@�˒@��n@�`B@��@��s@���@���@�%�@��@���@��4@�e,@�Z�@�H�@��@��@��1@�ƨ@��@�Ov@�x�@��P@�ߤ@�E�@�� @��@@�K�@�Q�@�	�@���@���@�IR@��@��/@�c�@��@�!@�@���@�o @���@���@���@�j@��Q@�~�@�4�@��r@��@���@���@�a@�#�@� i@��p@���@�N�@��@���@�+�@��M@���@�C-@��3@�]�@��@��@�z�@� �@���@��@�W?@��@���@���@��1@���@���@�e�@�V�@�O@�خ@��@�Y�@�@��f@���@�,=@�ϫ@��~@�\�@�$t@��5@��B@��9@��@�M@�v`@�T�@�B�@�:�@� \@��@��@��@��@�e�@�"h@�ݘ@��M@�t�@�f�@�7L@�(�@��@���@���@��@�r�@�g8@�C-@���@���@���@���@�Vm@�&�@��8@�֡@��9@�\�@��@��@���@��@@��7@�O�@�,�@���@�n�@�4n@���@���@��X@���@�o @�q@���@���@�Ta@��@��@���@���@��f@�zx@�/@��"@��@�ں@���@���@�^5@�/�@��@�_@���@��S@�RT@�,�@�@�ȴ@�g8@�'R@��@�@~��@~�@}��@}��@}c�@}�@|�z@|�@|`�@|�@{��@{�;@{K�@zl�@y�@y�N@yF@x��@x~@wZ�@v��@v�@u��@uc@u@@tɆ@t7�@tx@s˒@s�P@sU�@r�@r�@q�j@qp�@p��@p�@o� @o�:@oj�@o�@n��@n�\@nH�@m�@mL�@l��@l�Y@lq@lm�@l>B@k��@k/�@j��@jOv@jJ@i�9@iN<@i�@h�/@hy>@hG@g��@gdZ@g�@f��@f�@e��@ec�@e�@dѷ@dy>@c�@cv`@c�@b��@bv�@bYK@ae,@`�@`ѷ@`��@`Z@_�]@_� @_��@_e�@_C�@_@^�c@^GE@]�'@]�@\�@\(�@[� @[~�@[K�@[Y@Z��@Ze@Y�@Y�-@Y2a@X��@X��@X<�@W��@WMj@V�F@V@U�Z@U��@U�@T�I@T`�@T>B@T�@S�@S��@SF�@R�y@R�r@R�@Q��@Q��@Q}�@P��@Pu�@P'R@O��@Oqv@Ot�@OU�@OY@O�@N�c@Nl�@N3�@M�9@M��@M�S@L��@L�Y@Lh�@L[�@LQ�@LK^@L?�@K�0@KZ�@K�@J�X@J�b@J�A@J@I��@I�@H�?@HQ�@G�
@G@F�,@F��@F�R@F��@FYK@F�@E8�@D�5@D`�@D!@D�@C�@C��@Cg�@CA�@C+@CC@B�@B��@B��@B҉@B�@B�m@B��@B�6@BV@A�7@A&�@@��@@�E@@�O@@?�@?�@?v`@?o@>��@>c @>u@=��@=��@=Dg@=�@<�@;�@:�@:�@:6�@9�-@9c@9j@9!�@8��@7� @7U�@6��@6Ov@5��@5hs@5�@4��@4V�@3��@3��@3�@2�@2��@2��@2s�@2&�@1�@1[W@1;@0��@0~(@0,=@/��@/��@/@O@.�M@.��@.h
@.GE@.�@-�^@-��@-@@,�p@,!@+�*@+v`@+&@*�@*u@)�h@)N<@)@(Ĝ@(Q�@'�A@'x@';d@'
=@&�y@&��@&R�@&
�@%��@%J�@$�P@$�@$H@$7@#�@#��@#iD@"�@"��@"~�@"0U@"!�@!��@!�C@!j@!V@ ��@ Z@ 7�@��@��@l�@�@��@Ta@��@�d@��@X@-w@��@�/@�[@�@��@�j@��@[�@�@��@�
@�@��@g�@Y@ȴ@�}@�@��@d�@+k@u@��@e,@�@�@|�@m�@_@PH@(�@�0@�$@��@b�@]�@>�@!-@�@��@�6@�@:*@��@��@��@��@x�@Y�@N<@F@<6@+�@�@	l@�@�U@�@��@_@!@�@�*@~�@b�@X�@A�@&@
=@�@�"@��@�r@~�@~�@H�@	@	@ԕ@�M@B�@�@!�@!�@+@��@��@��@��@_@1@�}@��@&@�@��@�@q�@0U@@��@�n@�@rG@-w@�@�@ی@֡@ѷ@�)@��@�.@~(@j@A�@%�@@�
@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
�B
�B
�B
�B
��B
�B
�B
�)B
�CB
�)B
�)B
�CB
�]B
�]B
�CB
�B
�)B
�)B
�CB
�]B
�CB
�wB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�lB
H�B
JrB
P�B
c:B
��B
��B
�B
��B�B5BA;BnB��B~�B~wB��B�B��B�[B�B��BňB�oB�aB��B�aB�=B�)B�9Bo�BX_BM�BF�BB
�pB
�HB
��B
��B
��B
��B
i�B
H�B
�B
0B	�"B	�RB	��B	�|B	��B	��B	�B	e�B	X�B	J=B	A�B	>�B	7B	)�B	�B	�B	B	B	/5B	2GB	-�B	-�B	6�B	C�B	Q�B	q�B	~wB	{�B	q[B	w�B	}B	{JB	~�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	ƎB	�EB	�HB	��B	�0B	�B	�hB	��B	�KB	�B	��B	��B	ٴB	��B	�&B	�,B	�mB	�6B	�[B	��B	�aB	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�.B	��B
tB
�B
B
B
B
�B
�B
	lB
YB
?B
�B
mB
�B
�B
tB
	RB
	B
	7B
~B
�B
*�B
.B
0�B
)*B
&B
!-B
�B
B
�B
-CB
+QB
)�B
'�B
&�B
$�B
'�B
#�B
,�B
/5B
.�B
/OB
/�B
/�B
/�B
/ B
-B
*0B
)*B
)�B
)�B
)*B
(�B
'B
&�B
$�B
$&B
"4B
�B
B
�B
B
OB
�B
B
�B
�B
�B
~B
	B
�B
�B
B
�B
YB

B
�B
B
B
�B
�B
�B
FB
B
�B
uB
�B
B
�B
�B
(B
B
�B
"B
�B
6B
0B
�B

�B
�B
B
�B
B
YB
�B
�B
oB
B
 �B
;B
UB	��B	�qB	�B	�0B	��B	�jB	��B	��B	�B	��B	�.B	�HB	��B	��B	�B	�jB	�6B	�dB	��B	�xB	��B	�^B	�*B	��B	�RB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	�fB	�B	��B	�B	��B	�B	��B	�XB	�dB	��B	��B	�>B	�XB	�DB	�*B	��B	�B	��B	�B	�JB	�cB	��B	�]B	��B
 �B
�B
�B
oB
�B
'B
�B
�B
B
3B
MB
MB
�B
�B
�B
B
3B
�B
�B
GB
�B
[B
'B
 �B
 OB
 B	�}B	��B	�.B	��B	��B	��B	��B	��B	�B	�B
 B	��B	�}B
 B
 4B
 OB
 �B
UB
oB
�B
�B
B
�B
�B
B
�B
{B
�B
�B
�B
�B
YB
fB
1B
�B
�B
	7B
	RB
	RB
	�B
	�B

�B

�B

�B

�B

rB
)B
�B
xB
�B
dB
�B
6B
6B
�B
B
�B
�B
BB
BB
\B
�B
.B
bB
bB
�B
}B
�B
�B
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
@B
,B
{B
FB
�B
�B
aB
B
B
�B
�B
?B
�B
�B
�B
�B
�B
�B
$B
YB
YB
B
EB
_B
yB
�B
�B
B
1B
1B
B
�B
B
�B
kB
�B
	B
�B
�B
B
dB
�B
�B
jB
�B
�B
!B
pB
�B
�B
�B
�B
�B
�B
 B
 vB
 �B
 �B
!-B
!HB
"4B
"�B
#B
# B
#nB
#�B
#�B
#�B
#�B
#�B
$&B
%B
%B
%B
%B
%,B
%FB
%`B
%`B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'mB
'mB
'RB
'RB
'�B
'�B
'�B
'�B
(
B
(�B
(�B
(�B
(�B
)*B
)_B
)�B
)�B
)�B
*�B
*�B
+6B
+�B
+kB
+�B
+�B
+�B
,B
,WB
,�B
,�B
-�B
-�B
-�B
-�B
.cB
.�B
.�B
/�B
/�B
0B
0oB
0�B
0oB
0�B
0�B
1'B
1'B
1AB
1'B
1[B
1[B
1AB
1[B
1'B
1�B
2-B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4B
4B
4TB
4TB
4�B
4�B
5%B
5?B
5ZB
5tB
5tB
5ZB
5tB
5�B
6B
5�B
6`B
6�B
6�B
7fB
7�B
7�B
8B
8RB
8�B
8�B
9XB
9rB
9�B
9�B
9�B
:B
:DB
:�B
;0B
;0B
;0B
;�B
<6B
<�B
<�B
="B
=qB
=�B
=�B
>B
>]B
>�B
>�B
>�B
>�B
?cB
?�B
?�B
@B
@B
@OB
@�B
@�B
@�B
AB
AoB
A�B
A�B
BB
BAB
CB
C�B
C�B
C�B
C�B
DB
D�B
D�B
EmB
E�B
E�B
E�B
F?B
F%B
F%B
FB
FtB
F�B
F�B
F�B
GB
G+B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
IRB
IRB
IlB
IRB
I�B
J=B
J#B
JrB
J�B
K�B
L�B
MPB
M�B
M�B
M�B
N"B
NB
NB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O\B
OBB
O�B
O�B
O�B
O�B
O�B
PHB
PHB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
S&B
S[B
SuB
SuB
SuB
S[B
SuB
T,B
T{B
T�B
T�B
T�B
T{B
T{B
T�B
T�B
U2B
U�B
U�B
VSB
VmB
V�B
VmB
V�B
V�B
V�B
WsB
W?B
W�B
XB
X+B
XEB
XyB
X�B
X�B
X�B
X�B
YB
X�B
YB
X�B
YB
YB
X�B
X�B
YB
ZB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
[#B
[�B
\B
\CB
\�B
\xB
\�B
\�B
\�B
]~B
^B
^B
^OB
^�B
^�B
^�B
^�B
_�B
_�B
`BB
`�B
`�B
a�B
a�B
bB
b4B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
fB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
i_B
i�B
j�B
kB
kQB
k�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
nB
nIB
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
oiB
o�B
o�B
o�B
p;B
p�B
p�B
p�B
qAB
q[B
q�B
rB
rGB
r�B
sB
sMB
shB
s�B
tB
tB
tTB
tnB
uB
uZB
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
wfB
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
zB
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}VB
}qB
}qB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
HB
HB
�B
�B
� B
�OB
��B
�B
�B
�B
��B
��B
�[B
��B
��B
��B
��B
��B
�-B
�GB
�aB
�aB
�aB
�aB
��B
��B
��B
��B
��B
�B
�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
�B
�B
�B
�B
��B
�B
�B
�)B
�CB
�)B
�)B
�CB
�]B
�]B
�CB
�B
�)B
�)B
�CB
�]B
�CB
�wB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�lB
H�B
JrB
P�B
c:B
��B
��B
�B
��B�B5BA;BnB��B~�B~wB��B�B��B�[B�B��BňB�oB�aB��B�aB�=B�)B�9Bo�BX_BM�BF�BB
�pB
�HB
��B
��B
��B
��B
i�B
H�B
�B
0B	�"B	�RB	��B	�|B	��B	��B	�B	e�B	X�B	J=B	A�B	>�B	7B	)�B	�B	�B	B	B	/5B	2GB	-�B	-�B	6�B	C�B	Q�B	q�B	~wB	{�B	q[B	w�B	}B	{JB	~�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	ƎB	�EB	�HB	��B	�0B	�B	�hB	��B	�KB	�B	��B	��B	ٴB	��B	�&B	�,B	�mB	�6B	�[B	��B	�aB	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�.B	��B
tB
�B
B
B
B
�B
�B
	lB
YB
?B
�B
mB
�B
�B
tB
	RB
	B
	7B
~B
�B
*�B
.B
0�B
)*B
&B
!-B
�B
B
�B
-CB
+QB
)�B
'�B
&�B
$�B
'�B
#�B
,�B
/5B
.�B
/OB
/�B
/�B
/�B
/ B
-B
*0B
)*B
)�B
)�B
)*B
(�B
'B
&�B
$�B
$&B
"4B
�B
B
�B
B
OB
�B
B
�B
�B
�B
~B
	B
�B
�B
B
�B
YB

B
�B
B
B
�B
�B
�B
FB
B
�B
uB
�B
B
�B
�B
(B
B
�B
"B
�B
6B
0B
�B

�B
�B
B
�B
B
YB
�B
�B
oB
B
 �B
;B
UB	��B	�qB	�B	�0B	��B	�jB	��B	��B	�B	��B	�.B	�HB	��B	��B	�B	�jB	�6B	�dB	��B	�xB	��B	�^B	�*B	��B	�RB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB	�fB	�B	��B	�B	��B	�B	��B	�XB	�dB	��B	��B	�>B	�XB	�DB	�*B	��B	�B	��B	�B	�JB	�cB	��B	�]B	��B
 �B
�B
�B
oB
�B
'B
�B
�B
B
3B
MB
MB
�B
�B
�B
B
3B
�B
�B
GB
�B
[B
'B
 �B
 OB
 B	�}B	��B	�.B	��B	��B	��B	��B	��B	�B	�B
 B	��B	�}B
 B
 4B
 OB
 �B
UB
oB
�B
�B
B
�B
�B
B
�B
{B
�B
�B
�B
�B
YB
fB
1B
�B
�B
	7B
	RB
	RB
	�B
	�B

�B

�B

�B

�B

rB
)B
�B
xB
�B
dB
�B
6B
6B
�B
B
�B
�B
BB
BB
\B
�B
.B
bB
bB
�B
}B
�B
�B
�B
�B
�B
 B
oB
�B
�B
�B
�B
�B
�B
@B
,B
{B
FB
�B
�B
aB
B
B
�B
�B
?B
�B
�B
�B
�B
�B
�B
$B
YB
YB
B
EB
_B
yB
�B
�B
B
1B
1B
B
�B
B
�B
kB
�B
	B
�B
�B
B
dB
�B
�B
jB
�B
�B
!B
pB
�B
�B
�B
�B
�B
�B
 B
 vB
 �B
 �B
!-B
!HB
"4B
"�B
#B
# B
#nB
#�B
#�B
#�B
#�B
#�B
$&B
%B
%B
%B
%B
%,B
%FB
%`B
%`B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
'mB
'mB
'RB
'RB
'�B
'�B
'�B
'�B
(
B
(�B
(�B
(�B
(�B
)*B
)_B
)�B
)�B
)�B
*�B
*�B
+6B
+�B
+kB
+�B
+�B
+�B
,B
,WB
,�B
,�B
-�B
-�B
-�B
-�B
.cB
.�B
.�B
/�B
/�B
0B
0oB
0�B
0oB
0�B
0�B
1'B
1'B
1AB
1'B
1[B
1[B
1AB
1[B
1'B
1�B
2-B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4B
4B
4TB
4TB
4�B
4�B
5%B
5?B
5ZB
5tB
5tB
5ZB
5tB
5�B
6B
5�B
6`B
6�B
6�B
7fB
7�B
7�B
8B
8RB
8�B
8�B
9XB
9rB
9�B
9�B
9�B
:B
:DB
:�B
;0B
;0B
;0B
;�B
<6B
<�B
<�B
="B
=qB
=�B
=�B
>B
>]B
>�B
>�B
>�B
>�B
?cB
?�B
?�B
@B
@B
@OB
@�B
@�B
@�B
AB
AoB
A�B
A�B
BB
BAB
CB
C�B
C�B
C�B
C�B
DB
D�B
D�B
EmB
E�B
E�B
E�B
F?B
F%B
F%B
FB
FtB
F�B
F�B
F�B
GB
G+B
G�B
G�B
G�B
G�B
H1B
H�B
H�B
IRB
IRB
IlB
IRB
I�B
J=B
J#B
JrB
J�B
K�B
L�B
MPB
M�B
M�B
M�B
N"B
NB
NB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O\B
OBB
O�B
O�B
O�B
O�B
O�B
PHB
PHB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
S&B
S[B
SuB
SuB
SuB
S[B
SuB
T,B
T{B
T�B
T�B
T�B
T{B
T{B
T�B
T�B
U2B
U�B
U�B
VSB
VmB
V�B
VmB
V�B
V�B
V�B
WsB
W?B
W�B
XB
X+B
XEB
XyB
X�B
X�B
X�B
X�B
YB
X�B
YB
X�B
YB
YB
X�B
X�B
YB
ZB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
[#B
[�B
\B
\CB
\�B
\xB
\�B
\�B
\�B
]~B
^B
^B
^OB
^�B
^�B
^�B
^�B
_�B
_�B
`BB
`�B
`�B
a�B
a�B
bB
b4B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
fB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
h$B
h�B
h�B
h�B
i_B
i�B
j�B
kB
kQB
k�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
l�B
m�B
m�B
m�B
nB
nIB
n�B
n�B
n�B
n�B
n�B
o B
o5B
oiB
oiB
o�B
o�B
o�B
p;B
p�B
p�B
p�B
qAB
q[B
q�B
rB
rGB
r�B
sB
sMB
shB
s�B
tB
tB
tTB
tnB
uB
uZB
u�B
u�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
wfB
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y>B
y>B
yXB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
z*B
zB
z*B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
|B
|jB
|�B
|�B
|�B
|�B
}B
}"B
}VB
}qB
}qB
}VB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
HB
HB
�B
�B
� B
�OB
��B
�B
�B
�B
��B
��B
�[B
��B
��B
��B
��B
��B
�-B
�GB
�aB
�aB
�aB
�aB
��B
��B
��B
��B
��B
�B
�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192640  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192641  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192641                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042648  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042648  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                