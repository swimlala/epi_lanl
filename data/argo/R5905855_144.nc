CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-01-23T15:44:13Z creation;2023-01-23T15:44:15Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230123154413  20230123155757  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�_��}(1   @�`��.�@/b��`A��d!�Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�ffB���BÙ�B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C � C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @fg@|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B33B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB�L�B��3B�L�B��3BÀ B��B��B��fB��fB��fB��fB��fB��fB��fB��fB�� B��fB��3B��fB��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�C s3C!ٙC#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CP�CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Cb�Cd�Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D'3D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D=3D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DN��DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���D��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��fD�>fD�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fDс�DѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��fD�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�a|A�bA�a�A�bA�b�A�c A�cTA�d&A�ffA�e`A�\�A�a�A�i�A�k�A�kQA�jA�k�A�k�A�l�A�m�A�n�A�p;A�n/A�n�A�o A�pA�p;A�n�A�ncA�n�A�oiA�p;A�o�A�h>A�e�A�jA�i�A�e�A�XEA�=�AξA�K)A��A�JA��tA�^5A�!�A���A�3�A�HKA��2A���A��	A�|�A�@OA���A�c A��8A�~A�qA�n/A��A��vA��A�_;A�.A��4A�tA�бA�?A�$@A�9�A���A��FAS�A|�ZAx��Au�DAr��Am�yAh�Af7LAc6�AaT�A]/�AX�ASAIi�ADVA?jA;E�A;�A;�A=&�A=�A<MA;��A:��A9�A8�'A8%A6e�A5YKA4�A3Q�A.�mA,~�A+�#A*E�A)1�A+��A+�A+�>A*`�A*��A*��A(��A&��A&�VA'G�A'PHA'��A'�A(��A(�6A(��A(�A(I�A(�A'�A&��A%��A%A#�A#�A"��A!�A!��A��A�AA�^Aq�AqA=Am�AE9AH�AQ�A�$A��A�aAPHAJA�#A��A|�A��A�A��A�9AxlAGEAƨA�ZA�A�AR�A$�AY�A�NA�A,=A��A�A�FAl"A<6A*�A3�Ae�Av`A��A��A��An/A�XARTAMA�-A^�A
��A	�xA�A�BA��A~�A��A_pAffAu�A��A��A��A��Ay�Aj�A!-A��AT�A
=A�xAJ�A�A�PA��A�IA'�A��A��A>BA	lA ��A �3A ��A ��A u%A *0@��<@�j@��@���@���@���@���@�\�@��@��@���@�*�@��7@��]@�E9@���@���@�U2@��@���@���@�{J@���@���@�1�@��]@�4@�ff@�a�@�͟@���@�?}@�Ɇ@�_@��@��>@���@��@�L@�b@�@�@@�($@�@߮�@�X@�.I@��,@އ�@�%�@�{@��@ݽ�@��	@ܽ<@ܫ6@܁o@�4@ۤ@@�|�@��@ڀ�@�@�ݘ@٢�@�0�@ذ!@�"h@�m]@�.I@���@��y@֟�@�1�@��@�c�@���@�n�@�J@ӍP@���@�V�@��T@��B@�˒@ϻ0@ϕ�@�a�@�Z�@͗�@�IR@̩�@̓u@�tT@��m@��@�J�@Ȁ�@�:�@�f�@ƹ�@ƆY@�@Ş�@Ŋ�@���@���@�e�@�%@½<@�s�@���@�֡@��}@�V�@��@�<6@�+�@��@��R@���@�M�@�7@�|@�=@��@�`�@���@��@���@��L@�7�@�_@��@���@��n@�4�@��@��y@��e@�5?@���@�dZ@��@��@���@�ی@���@���@�c @�@���@���@�S�@�V@��f@��/@�I�@�RT@�|�@�Z�@�K^@�5?@� �@���@�=@�v�@���@��@���@�oi@�C-@�1�@�	@���@���@�F�@��,@��A@�x@���@��O@���@���@��@�PH@�!@��@��"@�}�@�]�@�N<@�<6@��@���@��@��@��P@���@�N�@��@��@�iD@�+�@���@���@�z@�N�@�9X@���@�~�@�c�@�5�@�o@�҉@�u@���@���@�m]@�=�@�֡@���@�	@��n@�^�@�
=@��"@���@��`@��E@���@��@���@��@�j@�U2@�>B@�&�@��@��T@��K@���@�`B@�4@���@��'@���@�H@���@��"@��@�_@��@���@�(@��P@��8@��K@���@��}@�q@�-@�	@��Z@��;@�@���@��$@�F�@��@�ߤ@�m�@���@�|�@�K�@�<6@�1�@�&@�Y@��@���@���@�J�@��3@�X@�%F@�	l@��c@��@�Ĝ@���@��A@��6@�dZ@�{�@�+k@��@��@�F�@�:�@�%@���@�z@�1'@��@���@�K�@��h@�h
@�GE@�/�@��@���@�&@��c@�ѷ@��@�d�@�ƨ@���@�\�@���@��@�c�@�4n@��A@���@�[W@�*0@��@��@�ԕ@���@��V@�y�@�]�@�%F@��@�N�@��D@��W@���@��@���@�qv@��@��@�֡@���@�(�@خ@~�@~n�@~:*@}��@|�f@|��@|b@{P�@z� @y5�@x�)@x�I@x[�@x�@wdZ@v�2@v�@v:*@u��@u�)@u��@u@@t�?@t�9@t�e@t�.@t�Y@tq@t`�@t:�@s6z@r��@rL0@q��@q��@qe,@q�@pq@o�[@oO@n҉@n��@n�@m[W@m+�@m&�@m@l�e@k�;@j�8@i��@i&�@h�E@hZ@g�
@g��@giD@g)_@g�@f��@fQ@f@e�D@e��@e�^@ec@eY�@e�@dQ�@c� @c�@b��@b�1@b8�@a�@a�X@a�M@`�5@`?�@_�@_�a@_{J@_ i@^�@^� @^5?@]ԕ@]+�@]%@\��@\H@\~@[�@[��@[6z@[@Z��@Z{@Yu�@X��@XFt@W�m@W��@W��@W��@W��@W��@Wb�@W33@V�@V�h@Vq�@V6�@U��@U��@T�@Sv`@R��@R�@R1�@Q��@Q%@P�e@O�A@N��@N:*@M[W@LPH@KO@J��@JOv@I�>@I[W@I \@Hl"@G��@GK�@G�@F�@F�@F��@E�@E�@DM@D�@D	�@C�@C�6@C�[@C�4@B�2@BOv@B6�@B
�@A�>@A�#@A�"@Au�@Ae,@A:�@@�@@�e@@H@@'R@@�@?�@?�0@?��@?�@?{J@?RT@>��@=��@=c�@=q@=�@<�	@<�K@<�@<��@<�_@<c�@;ݘ@;iD@:�c@:0U@9�@9�"@9X@9*0@9�@8�@8w�@8oi@8Q�@8b@7��@7C�@6�c@6��@6)�@5��@5/@4�@4N�@4"h@3�
@3e�@3!-@2�H@2l�@2�@1��@1��@1c�@0�@0��@0@/��@/H�@.��@.�R@.H�@._@-��@-Vm@-+�@-�@,��@,<�@+��@+A�@+�@*��@*��@*B[@*�@)��@)��@)T�@(��@(-�@'��@'�[@'��@'9�@&c @%�#@%�@%Vm@%7L@$�U@$m�@$N�@$*�@$1@#�}@#��@#+@"҉@"kQ@"!�@!��@ ��@ Ɇ@ �_@ I�@ 7@�@��@��@�q@dZ@�@��@GE@��@ϫ@�@s�@+@�@��@H@��@�@@$t@�@�}@}V@{@�@��@@�@�@��@�I@�u@��@�o@?�@"h@�
@�k@��@v`@/�@�@�H@�@��@��@�A@i�@Z�@R�@1�@�>@@�H@��@��@S&@�@�|@�@M@�@��@�}@��@�0@��@��@K�@$t@�s@�F@5?@��@@�@hs@V@�@��@�.@Ft@*�@7@  @�@e�@҉@�A@.�@�@�)@��@�j@@��@��@f�@&�@@@�@�f@ی@��@��@�@m�@?�@*�@~@�@1@�@  @�@�6@��@E9@
�H@
s�@
0U@
4@
�@	��@	�@	�j@	�@	�=@	Vm@	;@��@q@c�@M@��@�k@y�@Z�@33@��@��@��@��@=q@
�@�t@Q�@A @?}@7L@:�@7L@&�@�@�@�?@�$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�a|A�bA�a�A�bA�b�A�c A�cTA�d&A�ffA�e`A�\�A�a�A�i�A�k�A�kQA�jA�k�A�k�A�l�A�m�A�n�A�p;A�n/A�n�A�o A�pA�p;A�n�A�ncA�n�A�oiA�p;A�o�A�h>A�e�A�jA�i�A�e�A�XEA�=�AξA�K)A��A�JA��tA�^5A�!�A���A�3�A�HKA��2A���A��	A�|�A�@OA���A�c A��8A�~A�qA�n/A��A��vA��A�_;A�.A��4A�tA�бA�?A�$@A�9�A���A��FAS�A|�ZAx��Au�DAr��Am�yAh�Af7LAc6�AaT�A]/�AX�ASAIi�ADVA?jA;E�A;�A;�A=&�A=�A<MA;��A:��A9�A8�'A8%A6e�A5YKA4�A3Q�A.�mA,~�A+�#A*E�A)1�A+��A+�A+�>A*`�A*��A*��A(��A&��A&�VA'G�A'PHA'��A'�A(��A(�6A(��A(�A(I�A(�A'�A&��A%��A%A#�A#�A"��A!�A!��A��A�AA�^Aq�AqA=Am�AE9AH�AQ�A�$A��A�aAPHAJA�#A��A|�A��A�A��A�9AxlAGEAƨA�ZA�A�AR�A$�AY�A�NA�A,=A��A�A�FAl"A<6A*�A3�Ae�Av`A��A��A��An/A�XARTAMA�-A^�A
��A	�xA�A�BA��A~�A��A_pAffAu�A��A��A��A��Ay�Aj�A!-A��AT�A
=A�xAJ�A�A�PA��A�IA'�A��A��A>BA	lA ��A �3A ��A ��A u%A *0@��<@�j@��@���@���@���@���@�\�@��@��@���@�*�@��7@��]@�E9@���@���@�U2@��@���@���@�{J@���@���@�1�@��]@�4@�ff@�a�@�͟@���@�?}@�Ɇ@�_@��@��>@���@��@�L@�b@�@�@@�($@�@߮�@�X@�.I@��,@އ�@�%�@�{@��@ݽ�@��	@ܽ<@ܫ6@܁o@�4@ۤ@@�|�@��@ڀ�@�@�ݘ@٢�@�0�@ذ!@�"h@�m]@�.I@���@��y@֟�@�1�@��@�c�@���@�n�@�J@ӍP@���@�V�@��T@��B@�˒@ϻ0@ϕ�@�a�@�Z�@͗�@�IR@̩�@̓u@�tT@��m@��@�J�@Ȁ�@�:�@�f�@ƹ�@ƆY@�@Ş�@Ŋ�@���@���@�e�@�%@½<@�s�@���@�֡@��}@�V�@��@�<6@�+�@��@��R@���@�M�@�7@�|@�=@��@�`�@���@��@���@��L@�7�@�_@��@���@��n@�4�@��@��y@��e@�5?@���@�dZ@��@��@���@�ی@���@���@�c @�@���@���@�S�@�V@��f@��/@�I�@�RT@�|�@�Z�@�K^@�5?@� �@���@�=@�v�@���@��@���@�oi@�C-@�1�@�	@���@���@�F�@��,@��A@�x@���@��O@���@���@��@�PH@�!@��@��"@�}�@�]�@�N<@�<6@��@���@��@��@��P@���@�N�@��@��@�iD@�+�@���@���@�z@�N�@�9X@���@�~�@�c�@�5�@�o@�҉@�u@���@���@�m]@�=�@�֡@���@�	@��n@�^�@�
=@��"@���@��`@��E@���@��@���@��@�j@�U2@�>B@�&�@��@��T@��K@���@�`B@�4@���@��'@���@�H@���@��"@��@�_@��@���@�(@��P@��8@��K@���@��}@�q@�-@�	@��Z@��;@�@���@��$@�F�@��@�ߤ@�m�@���@�|�@�K�@�<6@�1�@�&@�Y@��@���@���@�J�@��3@�X@�%F@�	l@��c@��@�Ĝ@���@��A@��6@�dZ@�{�@�+k@��@��@�F�@�:�@�%@���@�z@�1'@��@���@�K�@��h@�h
@�GE@�/�@��@���@�&@��c@�ѷ@��@�d�@�ƨ@���@�\�@���@��@�c�@�4n@��A@���@�[W@�*0@��@��@�ԕ@���@��V@�y�@�]�@�%F@��@�N�@��D@��W@���@��@���@�qv@��@��@�֡@���@�(�@خ@~�@~n�@~:*@}��@|�f@|��@|b@{P�@z� @y5�@x�)@x�I@x[�@x�@wdZ@v�2@v�@v:*@u��@u�)@u��@u@@t�?@t�9@t�e@t�.@t�Y@tq@t`�@t:�@s6z@r��@rL0@q��@q��@qe,@q�@pq@o�[@oO@n҉@n��@n�@m[W@m+�@m&�@m@l�e@k�;@j�8@i��@i&�@h�E@hZ@g�
@g��@giD@g)_@g�@f��@fQ@f@e�D@e��@e�^@ec@eY�@e�@dQ�@c� @c�@b��@b�1@b8�@a�@a�X@a�M@`�5@`?�@_�@_�a@_{J@_ i@^�@^� @^5?@]ԕ@]+�@]%@\��@\H@\~@[�@[��@[6z@[@Z��@Z{@Yu�@X��@XFt@W�m@W��@W��@W��@W��@W��@Wb�@W33@V�@V�h@Vq�@V6�@U��@U��@T�@Sv`@R��@R�@R1�@Q��@Q%@P�e@O�A@N��@N:*@M[W@LPH@KO@J��@JOv@I�>@I[W@I \@Hl"@G��@GK�@G�@F�@F�@F��@E�@E�@DM@D�@D	�@C�@C�6@C�[@C�4@B�2@BOv@B6�@B
�@A�>@A�#@A�"@Au�@Ae,@A:�@@�@@�e@@H@@'R@@�@?�@?�0@?��@?�@?{J@?RT@>��@=��@=c�@=q@=�@<�	@<�K@<�@<��@<�_@<c�@;ݘ@;iD@:�c@:0U@9�@9�"@9X@9*0@9�@8�@8w�@8oi@8Q�@8b@7��@7C�@6�c@6��@6)�@5��@5/@4�@4N�@4"h@3�
@3e�@3!-@2�H@2l�@2�@1��@1��@1c�@0�@0��@0@/��@/H�@.��@.�R@.H�@._@-��@-Vm@-+�@-�@,��@,<�@+��@+A�@+�@*��@*��@*B[@*�@)��@)��@)T�@(��@(-�@'��@'�[@'��@'9�@&c @%�#@%�@%Vm@%7L@$�U@$m�@$N�@$*�@$1@#�}@#��@#+@"҉@"kQ@"!�@!��@ ��@ Ɇ@ �_@ I�@ 7@�@��@��@�q@dZ@�@��@GE@��@ϫ@�@s�@+@�@��@H@��@�@@$t@�@�}@}V@{@�@��@@�@�@��@�I@�u@��@�o@?�@"h@�
@�k@��@v`@/�@�@�H@�@��@��@�A@i�@Z�@R�@1�@�>@@�H@��@��@S&@�@�|@�@M@�@��@�}@��@�0@��@��@K�@$t@�s@�F@5?@��@@�@hs@V@�@��@�.@Ft@*�@7@  @�@e�@҉@�A@.�@�@�)@��@�j@@��@��@f�@&�@@@�@�f@ی@��@��@�@m�@?�@*�@~@�@1@�@  @�@�6@��@E9@
�H@
s�@
0U@
4@
�@	��@	�@	�j@	�@	�=@	Vm@	;@��@q@c�@M@��@�k@y�@Z�@33@��@��@��@��@=q@
�@�t@Q�@A @?}@7L@:�@7L@&�@�@�@�?@�$1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	-�B	-�B	-�B	-�B	./B	.B	-�B	-�B	-�B	.IB	.IB	.�B	.cB	-�B	./B	-�B	-�B	-�B	-wB	-wB	-]B	-�B	.cB	./B	-�B	-wB	-�B	.B	.}B	.�B	.�B	.}B	.�B	/ B	.}B	.�B	.�B	.�B	.IB	/B	1AB	<6B	D�B	[�B	�	B	��B
?�B
H�B
i�B
�:B
�,B
��B
�CB
��B
�aB!B:�B
�FB
�sB
��B
��B
�jB
Z�B
1�B
!HB
QB
�B
	7B
�B
�B
+B
HfB
DMB
2B
 �B
�B
�B	�kB	ѷB	�B	�fB	v�B	o�B	e�B	[�B	G�B	-�B	�B��B�LB�9B��B��B�B	6B	;B	>�B	T�B	Z�B	T�B	UMB	v�B	�'B	��B	�"B	�)B	�.B	{B	u�B	qvB	raB	��B	��B	��B	��B	�4B	�@B	�B	�B	�B
�B
WB
4nB
;dB
MB
T�B
Z�B
_�B
a|B
ffB
c�B
f2B
\)B
]dB
T�B
[	B
a|B
^�B
b�B
XB
OvB
M6B
E�B
;�B
5?B
-]B
,�B
0UB
2�B
<�B
IB
MPB
J�B
IlB
H1B
GzB
F�B
DB
?.B
?}B
?�B
>�B
H�B
MPB
IB
O�B
N�B
X�B
c�B
_!B
\)B
[�B
[WB
ZkB
YKB
WYB
XEB
WYB
W?B
X�B
TB
L�B
IB
F�B
I�B
M�B
IRB
E�B
A�B
9	B
5tB
5?B
-B
&�B
$@B
#�B
&2B
"NB
�B
 �B
#TB
&LB
'B
(XB
)B
)�B
)*B
(�B
&�B
$@B
# B
!�B
!�B
!�B
!-B
�B
B
xB
#B
qB
�B
xB
�B
�B
xB
�B
qB
�B
B
�B
YB
B
gB
�B
�B
�B
B
�B
�B
	�B
�B
?B
mB
�B
	RB
�B
�B
4B
�B
bB
�B
�B
�B
B
B
�B
�B
�B
uB
�B
�B
-B
B
�B

�B

�B

�B

#B
�B
�B
�B
�B
 �B
 OB
  B	��B
  B
UB
�B
�B
aB
�B
�B
�B
%B
�B
SB
B
SB
MB
B
�B
�B
�B	��B	��B	�(B	�(B	�(B	��B	�<B	�6B	�jB	��B	��B	�dB	�JB	�B	�B	�0B	��B	�JB	��B	��B	�xB	��B	�B	��B	��B	�B	��B	�cB	��B	�B	�B	�PB	�(B	��B	��B	��B
B
oB
�B
AB
�B
�B
�B
[B
'B
�B
�B
�B
AB
uB
�B
�B
-B
�B
mB
tB
�B
�B
tB
�B
�B
�B
�B
�B
{B
GB
{B
�B
�B
MB
B
�B
�B
�B
�B
mB
B
�B
�B
B
B
�B
�B
fB
�B
	RB
	�B

�B
DB
)B

�B
0B
�B
jB
PB
6B
B
�B
B
B
�B
�B
�B
<B
<B
<B
"B
B
B
�B
�B
�B
�B
PB
PB
�B
�B
�B
(B
�B
�B
(B
\B
vB
�B
.B
HB
HB
HB
�B
�B
}B
�B
�B
}B
HB
�B
�B
\B
(B
B
B
�B
�B
PB
PB
6B
�B
~B
JB
B
0B
0B
�B
�B
�B
�B
 B
�B
&B
@B
[B
uB
�B
�B
aB
{B
�B
�B
MB
gB
�B
B
9B
�B
�B
�B
�B
B
EB
�B
B
�B
B
B
#B
	B
�B
IB
�B
�B
;B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!HB
!|B
!B
!HB
!�B
"NB
"�B
# B
# B
# B
# B
# B
#:B
#�B
$&B
%,B
&2B
&�B
&2B
&2B
%�B
%�B
%�B
%B
$�B
$tB
#�B
%�B
$�B
$�B
%,B
%�B
%`B
%�B
&B
&B
&fB
'B
'B
'�B
)DB
)�B
)�B
)�B
)�B
*0B
+�B
,qB
,�B
-CB
-�B
/�B
/�B
0B
0�B
0�B
1B
1AB
1[B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3B
3�B
3�B
3�B
3�B
3hB
3�B
3�B
4B
4B
3�B
4�B
4�B
5B
5tB
5tB
5ZB
6`B
6zB
6`B
6�B
7LB
72B
8�B
8�B
9	B
8�B
9rB
:*B
:�B
:�B
:xB
:^B
:DB
:�B
;�B
<PB
<�B
=<B
=�B
=�B
>B
>(B
>(B
>�B
?cB
?}B
?�B
?�B
?�B
@ B
@OB
@�B
@�B
AUB
AUB
B'B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
DMB
DB
C�B
DMB
D�B
D�B
D�B
EB
EB
EB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
EmB
ESB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FYB
FYB
G+B
GzB
H�B
HfB
H�B
I7B
I7B
I7B
I�B
I�B
I�B
I�B
IlB
I7B
IRB
IlB
IlB
IRB
I�B
J	B
JrB
K^B
K�B
L~B
L�B
M6B
M6B
M6B
MB
MB
M�B
M�B
M�B
MjB
L�B
L�B
L�B
MB
MPB
NVB
OB
O(B
O�B
P.B
P�B
Q�B
R B
RB
Q�B
Q�B
R B
R�B
RoB
R�B
S�B
S�B
S�B
S�B
TB
T�B
T�B
U�B
VB
VB
VB
U�B
U�B
U�B
V�B
W�B
W�B
W�B
XB
W�B
XEB
XEB
X_B
XEB
X�B
X�B
YB
YB
YB
Y1B
YeB
YeB
YKB
YeB
YeB
ZQB
Z�B
[#B
[=B
[=B
[WB
[=B
[=B
[WB
[=B
[WB
[�B
[�B
\CB
\�B
\�B
]/B
]/B
]~B
]dB
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
_!B
_pB
_�B
`BB
`�B
`�B
`�B
a-B
a|B
a�B
a�B
b4B
b�B
b�B
b�B
cB
c�B
c�B
c�B
d�B
d�B
d�B
eB
eFB
eFB
e�B
e�B
e�B
ezB
e�B
e�B
fB
e�B
f2B
fLB
f2B
fLB
f2B
fB
f�B
f�B
gB
g�B
h$B
hXB
h>B
hsB
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
kB
kkB
k�B
k�B
lWB
lqB
l�B
m)B
mB
m]B
m�B
nB
nIB
ncB
ncB
ncB
n�B
o5B
o5B
oiB
o�B
o�B
o�B
o�B
poB
poB
p�B
p�B
qAB
qAB
q�B
rB
rGB
rGB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tTB
t�B
t�B
uB
uB
uZB
u�B
u�B
u�B
u�B
vB
vB
v+B
vFB
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
xRB
xRB
x�B
y$B
y>B
y�B
yrB
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{JB
{B
{B
{�B
{�B
{�B
|B
|PB
|jB
}"B
}qB
}�B
}�B
~BB
~(B
~BB
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
� B
�B
�4B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
��B
�B
�'B
�[B
�[B
��B
�uB
��B
��B
��B
��B
�GB
��B
�MB
�3B
��B
�9B
�SB
�mB
��B
��B
�B
�B
��B
�YB
��B
��B
��B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�_B
�zB
�_1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	-�B	-�B	-�B	-�B	./B	.B	-�B	-�B	-�B	.IB	.IB	.�B	.cB	-�B	./B	-�B	-�B	-�B	-wB	-wB	-]B	-�B	.cB	./B	-�B	-wB	-�B	.B	.}B	.�B	.�B	.}B	.�B	/ B	.}B	.�B	.�B	.�B	.IB	/B	1AB	<6B	D�B	[�B	�	B	��B
?�B
H�B
i�B
�:B
�,B
��B
�CB
��B
�aB!B:�B
�FB
�sB
��B
��B
�jB
Z�B
1�B
!HB
QB
�B
	7B
�B
�B
+B
HfB
DMB
2B
 �B
�B
�B	�kB	ѷB	�B	�fB	v�B	o�B	e�B	[�B	G�B	-�B	�B��B�LB�9B��B��B�B	6B	;B	>�B	T�B	Z�B	T�B	UMB	v�B	�'B	��B	�"B	�)B	�.B	{B	u�B	qvB	raB	��B	��B	��B	��B	�4B	�@B	�B	�B	�B
�B
WB
4nB
;dB
MB
T�B
Z�B
_�B
a|B
ffB
c�B
f2B
\)B
]dB
T�B
[	B
a|B
^�B
b�B
XB
OvB
M6B
E�B
;�B
5?B
-]B
,�B
0UB
2�B
<�B
IB
MPB
J�B
IlB
H1B
GzB
F�B
DB
?.B
?}B
?�B
>�B
H�B
MPB
IB
O�B
N�B
X�B
c�B
_!B
\)B
[�B
[WB
ZkB
YKB
WYB
XEB
WYB
W?B
X�B
TB
L�B
IB
F�B
I�B
M�B
IRB
E�B
A�B
9	B
5tB
5?B
-B
&�B
$@B
#�B
&2B
"NB
�B
 �B
#TB
&LB
'B
(XB
)B
)�B
)*B
(�B
&�B
$@B
# B
!�B
!�B
!�B
!-B
�B
B
xB
#B
qB
�B
xB
�B
�B
xB
�B
qB
�B
B
�B
YB
B
gB
�B
�B
�B
B
�B
�B
	�B
�B
?B
mB
�B
	RB
�B
�B
4B
�B
bB
�B
�B
�B
B
B
�B
�B
�B
uB
�B
�B
-B
B
�B

�B

�B

�B

#B
�B
�B
�B
�B
 �B
 OB
  B	��B
  B
UB
�B
�B
aB
�B
�B
�B
%B
�B
SB
B
SB
MB
B
�B
�B
�B	��B	��B	�(B	�(B	�(B	��B	�<B	�6B	�jB	��B	��B	�dB	�JB	�B	�B	�0B	��B	�JB	��B	��B	�xB	��B	�B	��B	��B	�B	��B	�cB	��B	�B	�B	�PB	�(B	��B	��B	��B
B
oB
�B
AB
�B
�B
�B
[B
'B
�B
�B
�B
AB
uB
�B
�B
-B
�B
mB
tB
�B
�B
tB
�B
�B
�B
�B
�B
{B
GB
{B
�B
�B
MB
B
�B
�B
�B
�B
mB
B
�B
�B
B
B
�B
�B
fB
�B
	RB
	�B

�B
DB
)B

�B
0B
�B
jB
PB
6B
B
�B
B
B
�B
�B
�B
<B
<B
<B
"B
B
B
�B
�B
�B
�B
PB
PB
�B
�B
�B
(B
�B
�B
(B
\B
vB
�B
.B
HB
HB
HB
�B
�B
}B
�B
�B
}B
HB
�B
�B
\B
(B
B
B
�B
�B
PB
PB
6B
�B
~B
JB
B
0B
0B
�B
�B
�B
�B
 B
�B
&B
@B
[B
uB
�B
�B
aB
{B
�B
�B
MB
gB
�B
B
9B
�B
�B
�B
�B
B
EB
�B
B
�B
B
B
#B
	B
�B
IB
�B
�B
;B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!HB
!|B
!B
!HB
!�B
"NB
"�B
# B
# B
# B
# B
# B
#:B
#�B
$&B
%,B
&2B
&�B
&2B
&2B
%�B
%�B
%�B
%B
$�B
$tB
#�B
%�B
$�B
$�B
%,B
%�B
%`B
%�B
&B
&B
&fB
'B
'B
'�B
)DB
)�B
)�B
)�B
)�B
*0B
+�B
,qB
,�B
-CB
-�B
/�B
/�B
0B
0�B
0�B
1B
1AB
1[B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3B
3�B
3�B
3�B
3�B
3hB
3�B
3�B
4B
4B
3�B
4�B
4�B
5B
5tB
5tB
5ZB
6`B
6zB
6`B
6�B
7LB
72B
8�B
8�B
9	B
8�B
9rB
:*B
:�B
:�B
:xB
:^B
:DB
:�B
;�B
<PB
<�B
=<B
=�B
=�B
>B
>(B
>(B
>�B
?cB
?}B
?�B
?�B
?�B
@ B
@OB
@�B
@�B
AUB
AUB
B'B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
DMB
DB
C�B
DMB
D�B
D�B
D�B
EB
EB
EB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
EmB
ESB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FYB
FYB
G+B
GzB
H�B
HfB
H�B
I7B
I7B
I7B
I�B
I�B
I�B
I�B
IlB
I7B
IRB
IlB
IlB
IRB
I�B
J	B
JrB
K^B
K�B
L~B
L�B
M6B
M6B
M6B
MB
MB
M�B
M�B
M�B
MjB
L�B
L�B
L�B
MB
MPB
NVB
OB
O(B
O�B
P.B
P�B
Q�B
R B
RB
Q�B
Q�B
R B
R�B
RoB
R�B
S�B
S�B
S�B
S�B
TB
T�B
T�B
U�B
VB
VB
VB
U�B
U�B
U�B
V�B
W�B
W�B
W�B
XB
W�B
XEB
XEB
X_B
XEB
X�B
X�B
YB
YB
YB
Y1B
YeB
YeB
YKB
YeB
YeB
ZQB
Z�B
[#B
[=B
[=B
[WB
[=B
[=B
[WB
[=B
[WB
[�B
[�B
\CB
\�B
\�B
]/B
]/B
]~B
]dB
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
_!B
_pB
_�B
`BB
`�B
`�B
`�B
a-B
a|B
a�B
a�B
b4B
b�B
b�B
b�B
cB
c�B
c�B
c�B
d�B
d�B
d�B
eB
eFB
eFB
e�B
e�B
e�B
ezB
e�B
e�B
fB
e�B
f2B
fLB
f2B
fLB
f2B
fB
f�B
f�B
gB
g�B
h$B
hXB
h>B
hsB
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
kB
kkB
k�B
k�B
lWB
lqB
l�B
m)B
mB
m]B
m�B
nB
nIB
ncB
ncB
ncB
n�B
o5B
o5B
oiB
o�B
o�B
o�B
o�B
poB
poB
p�B
p�B
qAB
qAB
q�B
rB
rGB
rGB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tTB
t�B
t�B
uB
uB
uZB
u�B
u�B
u�B
u�B
vB
vB
v+B
vFB
v+B
v`B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
xRB
xRB
x�B
y$B
y>B
y�B
yrB
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{JB
{B
{B
{�B
{�B
{�B
|B
|PB
|jB
}"B
}qB
}�B
}�B
~BB
~(B
~BB
~�B
~�B
~�B
HB
}B
�B
�B
�B
�B
� B
�B
�4B
�OB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
��B
�B
�'B
�[B
�[B
��B
�uB
��B
��B
��B
��B
�GB
��B
�MB
�3B
��B
�9B
�SB
�mB
��B
��B
�B
�B
��B
�YB
��B
��B
��B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�_B
�zB
�_1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230123154341  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230123154413  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230123154414  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230123154415                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230123154415  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230123154415  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230123155757                      G�O�G�O�G�O�                