CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:44:20Z creation;2022-06-04T17:44:21Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174420  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���Ǯ{1   @���Յ��@0:��vȴ�c|9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B���B�  B���BÙ�B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�ffB�ffB�ffC   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C�fC  C   C"�C$33C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR33CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @|��@�ff@�ffA33A?33A_33A33A���A���A���A���Aϙ�Aߙ�AA���B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B��fB��fB��fB��fB��fB��fB��fB��fB��fB�L�B��fB��3B��fB��3B��fB��3BÀ Bǳ3B��fB��fBӳ3B׳3B��fB��fB��fB��fB��fB��fB�L�B�L�B�L�B��fC�3C�3C�3C�3C	�3C�3C�3C�3C�3CٙC�3C�3C�3CٙC�3C�3C"�C$&fC%�3C'�3C)�3C+ٙC-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CP�CR&fCS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�gC���C���C���C�gC�gC���C���C���C���C���C���C���C�gC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D |�D ��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D	|�D	��D
|�D
��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D|�D��D |�D ��D!|�D!��D"|�D"��D#|�D#��D$|�D$��D%|�D%��D&|�D&��D'|�D'��D(|�D(��D)|�D)��D*|�D*��D+|�D+��D,|�D,��D-|�D-��D.|�D.��D/|�D/��D0|�D0��D1|�D1��D2|�D2��D3|�D3��D4|�D4��D5|�D5��D6|�D6��D7|�D7��D8|�D8��D9|�D9��D:|�D:��D;|�D;��D<|�D<��D=|�D=��D>|�D>��D?|�D?��D@|�D@��DA|�DA��DB|�DB��DC|�DC��DD|�DD��DE|�DE��DF|�DF��DG|�DG��DH|�DH��DI|�DI��DJ|�DJ��DK|�DK��DL|�DL��DM|�DM��DN|�DO3DO|�DO��DP|�DP��DQ|�DQ��DR|�DR��DS|�DS��DT|�DT��DU|�DU��DV|�DV��DW|�DW��DX|�DX��DY|�DY��DZ|�DZ��D[|�D[��D\|�D\��D]|�D]��D^|�D^��D_|�D_��D`|�D`��Da|�Da��Db|�Db��Dc|�Dc��Dd|�Dd��De|�De��Df|�Df��Dg|�Dg��Dh|�Dh��Di|�Di��Dj|�Dj��Dk|�Dk��Dl|�Dl��Dm|�Dm��Dn|�Dn��Do|�Do��Dp|�Dp��Dq|�Dq��Dr|�Dr��Ds|�Ds��Dt|�Dt��Du|�Du��Dv|�Dv��Dw|�Dw��Dx|�Dx��Dy|�Dy��Dz|�Dz��D{|�D{��D||�D|��D}|�D}��D~|�D~��D|�D��D�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�A�D���D��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD¾fD��fD�>fD�~fDþfD��fD�>fD�~fDľfD��3D�;3D�~fDžfD��fD�>fD�~fDƾfD��fD�>fD�~fDǾfD��fD�>fD�~fDȾfD��fD�>fD�~fDɾfD��fD�>fD�~fDʾfD��fD�>fD�~fD˾fD��fD�>fD�~fD̾fD��fD�>fD�~fD;fD��fD�>fD�~fDξfD��fD�>fD�~fDϾfD��fD�>fD�~fDоfD��fD�>fD�~fDѾfD��fD�>fD�~fDҾfD��fD�>fD�~fDӾfD��fD�>fD�~fDԾfD��fD�>fD�~fDվfD��fD�>fD�~fD־fD��fD�>fD�~fD׾fD��fD�>fD�~fDؾfD��fD�>fD�~fDپfD��fD�>fD�~fDھfD��fD�>fD�~fD۾fD��fD�>fD�~fDܾfD��D�>fD�~fDݾfD��fD�>fD�~fD޾fD��fD�>fD�~fD߾fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�A�D�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD�fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�;3D�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~fD��fD��fD�>fD�~f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�a�A�^�A�^5A�b�A�h
A�h�A�iDA�i�A�hsA�kQA�m)A�m�A�n�A�o5A�n/A�n�A�poA�rGA�qAA�t�A�uZA�r�A�qAA�qAA�r|A�s�A�qAA�m)A�a�A�!�A��sA���A̵tA̪�A̧RA̞�Ȁ�Á�A�p;A�bNA�ȀAˢ�A�R�A�ٴA��lA�F?A�s�A��9A��)A��DA���A���A���A�eA��A�GA��A�|�A�d�A���A�sA��SA��A�0!A��A�A���A���A�VmA��A�NpA���A�Z�A�F?A��A�?}A�z�A�GEA~n/A{�.Az�/AyѷAy_Av�xAs=�AmX�Ai��Ad�2Aa��Aa=qA^�KAY�mAX��AW�AV��AScAPq�AN��AMsAH�ADE9AB�FA@�+A>��A:�'A9jA9A�A8�XA6��A5��A5y>A4~A2��A1�qA0a�A.�vA.�A,�A,�A+�oA+�TA+�'A*֡A*Z�A)��A(�A'��A(GA(�fA(�RA(g�A(�A'یA'�2A'(�A&��A%��A%�|A%�A%��A%��A%jA%6�A$ںA#�A"�A"��A"��A"Q�A!e�A!�A!C�A �EAX�A��AP�A��A��A&ASA�}A;�A�cA��A?}A��A�Ah
At�AffA9XAu%A<�A��A�
AK�A�MA�Af�A1A�Ax�A�A��A�0A�PA��A)�Aw�A	lA�A�At�A!-AS�A�KA˒A��A��A^5A6A�[Ao�A�A��Ap;A,=A|A��A?�A
��A
�*A
+kA	��A��A�+AxA!AxA{A��AN<AL�AB�A��A�}AW�A�|A�Ax�A�A"hAb�A�A_pA �5A W?@�33@��x@���@��@��@���@��E@�]d@�Z�@��+@���@���@�x@��m@�?�@��@��@�@���@�GE@�@��@�/�@� @��@��@��@��@��@�tT@� �@�-@��@�l�@��z@�iD@�RT@�A @�-w@�S@�@��.@�*0@�u�@�J@�@�8@�	l@�6@�.�@��z@��@妵@�8�@�҉@�u@�:�@�*@�?@�IR@��@���@���@��A@ߏ�@߄M@��@�YK@�=q@ݺ^@�H�@�i�@۔�@��@���@�8�@� �@��a@�4�@��,@׀4@��@�_@�˒@�#�@���@��m@�?�@��@�|�@�C�@ұ�@�K^@��@�ϫ@�J�@�_@�@��@�($@�s�@��Z@�_p@��@̆Y@�O@˕�@ˉ7@��@��E@�M�@�($@�"�@ȕ�@�1'@���@�;d@��c@ƾ@�v�@��.@���@�u�@���@�Q�@�
�@Ó�@��@���@�(�@��2@���@�|�@�#:@��@���@�g�@�e,@�O�@�6z@��2@���@��4@��@���@��	@��@��6@�R�@��m@�|�@��f@�V@���@�}�@��@�H�@�!@�$@�b@��@�w2@��@�8�@��@���@�q@�)�@���@���@�5?@���@��#@��H@��7@�H�@�&�@�S@��@�Z@��@���@�>�@�PH@��@��3@�x@�hs@�Mj@�Ĝ@�E�@���@���@�(�@��4@�dZ@�F@�%F@���@���@�@��^@�{J@�\�@�E9@�V@��?@��D@�G@��H@���@�A @� i@���@��@�a|@�:�@��z@��h@�A @�C@��O@�J@�خ@���@���@��h@��y@�l�@�]d@���@�N<@��@��y@��E@�q@��@���@��=@��T@��@���@���@�j�@�A @���@��\@�I�@�@��@���@�N<@�"�@��@��I@�tT@�5?@�'R@��@��@�s�@�O�@�$t@��H@���@�r�@�_@�5?@��g@���@�j@�8�@�"�@���@�?@���@�c�@��@��2@���@�z@��A@���@��@���@�ff@��^@���@��-@���@���@���@�_p@�:�@�.I@�Y@���@�q�@��@���@��@���@��@�b�@�*0@���@�͟@�^5@�
�@��;@��H@��M@�K�@�C@�V@��,@��h@��@���@�l"@�Q�@�8�@��>@�A @���@�֡@��?@�bN@�($@��@��Z@��&@���@���@��0@���@�8@�� @��D@���@���@��N@��X@���@���@�w2@�rG@�rG@�j�@�\�@�"�@��@��x@�^5@�B[@��@��
@�x�@�(@���@���@�s�@�e�@�Z@�($@���@���@�B�@�'�@�(@��@���@�_@�
�@���@�]�@�33@�
=@���@���@�_@��@33@~�@~c @~+k@}��@}�@|��@|��@|!@|G@{��@{H�@z�L@zB[@y�3@y�@x�p@x$@w��@w��@w�4@v�@v�r@u�@uL�@t�@t~(@tPH@s�@s4�@r�@rp;@rR�@r@q�C@qc@q=�@q(�@q(�@q�@p�O@p|�@p�@o@O@o�@n�!@n�@n��@nq�@n_�@n@m�@l�9@l~@k�@k��@k&@jȴ@i�9@i�T@h�@h%�@g��@g�V@g��@g��@g]�@g)_@g
=@f�b@e<6@d��@d9X@cخ@c��@c~�@c(@b��@b�@b�@a��@`��@`��@`N�@`	�@_x@^�2@^��@^!�@]�@]�'@]��@]�@\	�@[j�@[(@Z��@Z~�@Y�@X��@X�@W�}@WC�@V��@V��@VYK@U��@Uϫ@U�@US&@T�|@T�@T��@T�_@Tw�@S�6@SS�@S6z@S�@R��@Rl�@R;�@Q�>@Q	l@P�@P֡@P��@P��@Pw�@Pw�@Pj@PA�@P!@O�A@Oƨ@O��@Os@OZ�@OX�@O@O@N��@N}V@M��@M-w@L��@L��@L/�@LG@K˒@K�[@K�@J�@J�s@J�6@J\�@I�.@I��@I#�@H�@HQ�@H-�@H�@G��@G��@GC�@GS@F��@FE�@E�)@E�#@E�'@E�7@EN<@D�v@Dh�@C�g@C��@CP�@B��@B�@A��@A�@A%F@@[�@?��@?dZ@?�@>��@>YK@=��@=x�@<ѷ@<?�@;�@; i@:�@:GE@:�@:�@9�)@9�@9�@9�M@9IR@8��@8�@7b�@6�@6�@5��@5�@4�P@4ی@4N�@3��@3��@3��@3��@3v`@3$t@2L0@1�Z@1�9@1��@1zx@1Vm@1=�@12a@1&�@0��@0U2@0G@/�m@/�K@/K�@.�@._�@.@-�=@-\�@-F@,�|@,��@,�z@,|�@,U2@+�A@+�[@+\)@*��@*�b@*�A@*n�@*a|@*Z�@*Z�@*B[@)�@)^�@)?}@)+�@)*0@) \@)V@)	l@(�u@(�@'�r@'y�@'9�@&}V@&_@%�j@%��@%Y�@$��@$Ĝ@$�_@$u�@$*�@#X�@"�@"�x@"J�@"B[@"@�@"5?@")�@"_@!�#@!�t@!k�@!O�@!5�@!q@!�@!@@!�@!	l@ ��@ ��@ �@ ��@ m�@ ~@�+@��@)_@�@��@��@p;@V@3�@O@{@J@�@�~@(�@�@�@��@�@� @�q@�	@E9@�M@�@p;@Z�@L0@3�@��@�@��@rG@�@��@�@�I@oi@bN@U2@<�@6@'R@�@G@�@��@"�@��@��@�A@Ov@O@ϫ@�z@��@�'@��@+�@Ɇ@��@��@u�@g8@M@1'@M@�A@ݘ@�@��@��@�f@|�@>�@6z@4�@4�@4�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�a�A�^�A�^5A�b�A�h
A�h�A�iDA�i�A�hsA�kQA�m)A�m�A�n�A�o5A�n/A�n�A�poA�rGA�qAA�t�A�uZA�r�A�qAA�qAA�r|A�s�A�qAA�m)A�a�A�!�A��sA���A̵tA̪�A̧RA̞�Ȁ�Á�A�p;A�bNA�ȀAˢ�A�R�A�ٴA��lA�F?A�s�A��9A��)A��DA���A���A���A�eA��A�GA��A�|�A�d�A���A�sA��SA��A�0!A��A�A���A���A�VmA��A�NpA���A�Z�A�F?A��A�?}A�z�A�GEA~n/A{�.Az�/AyѷAy_Av�xAs=�AmX�Ai��Ad�2Aa��Aa=qA^�KAY�mAX��AW�AV��AScAPq�AN��AMsAH�ADE9AB�FA@�+A>��A:�'A9jA9A�A8�XA6��A5��A5y>A4~A2��A1�qA0a�A.�vA.�A,�A,�A+�oA+�TA+�'A*֡A*Z�A)��A(�A'��A(GA(�fA(�RA(g�A(�A'یA'�2A'(�A&��A%��A%�|A%�A%��A%��A%jA%6�A$ںA#�A"�A"��A"��A"Q�A!e�A!�A!C�A �EAX�A��AP�A��A��A&ASA�}A;�A�cA��A?}A��A�Ah
At�AffA9XAu%A<�A��A�
AK�A�MA�Af�A1A�Ax�A�A��A�0A�PA��A)�Aw�A	lA�A�At�A!-AS�A�KA˒A��A��A^5A6A�[Ao�A�A��Ap;A,=A|A��A?�A
��A
�*A
+kA	��A��A�+AxA!AxA{A��AN<AL�AB�A��A�}AW�A�|A�Ax�A�A"hAb�A�A_pA �5A W?@�33@��x@���@��@��@���@��E@�]d@�Z�@��+@���@���@�x@��m@�?�@��@��@�@���@�GE@�@��@�/�@� @��@��@��@��@��@�tT@� �@�-@��@�l�@��z@�iD@�RT@�A @�-w@�S@�@��.@�*0@�u�@�J@�@�8@�	l@�6@�.�@��z@��@妵@�8�@�҉@�u@�:�@�*@�?@�IR@��@���@���@��A@ߏ�@߄M@��@�YK@�=q@ݺ^@�H�@�i�@۔�@��@���@�8�@� �@��a@�4�@��,@׀4@��@�_@�˒@�#�@���@��m@�?�@��@�|�@�C�@ұ�@�K^@��@�ϫ@�J�@�_@�@��@�($@�s�@��Z@�_p@��@̆Y@�O@˕�@ˉ7@��@��E@�M�@�($@�"�@ȕ�@�1'@���@�;d@��c@ƾ@�v�@��.@���@�u�@���@�Q�@�
�@Ó�@��@���@�(�@��2@���@�|�@�#:@��@���@�g�@�e,@�O�@�6z@��2@���@��4@��@���@��	@��@��6@�R�@��m@�|�@��f@�V@���@�}�@��@�H�@�!@�$@�b@��@�w2@��@�8�@��@���@�q@�)�@���@���@�5?@���@��#@��H@��7@�H�@�&�@�S@��@�Z@��@���@�>�@�PH@��@��3@�x@�hs@�Mj@�Ĝ@�E�@���@���@�(�@��4@�dZ@�F@�%F@���@���@�@��^@�{J@�\�@�E9@�V@��?@��D@�G@��H@���@�A @� i@���@��@�a|@�:�@��z@��h@�A @�C@��O@�J@�خ@���@���@��h@��y@�l�@�]d@���@�N<@��@��y@��E@�q@��@���@��=@��T@��@���@���@�j�@�A @���@��\@�I�@�@��@���@�N<@�"�@��@��I@�tT@�5?@�'R@��@��@�s�@�O�@�$t@��H@���@�r�@�_@�5?@��g@���@�j@�8�@�"�@���@�?@���@�c�@��@��2@���@�z@��A@���@��@���@�ff@��^@���@��-@���@���@���@�_p@�:�@�.I@�Y@���@�q�@��@���@��@���@��@�b�@�*0@���@�͟@�^5@�
�@��;@��H@��M@�K�@�C@�V@��,@��h@��@���@�l"@�Q�@�8�@��>@�A @���@�֡@��?@�bN@�($@��@��Z@��&@���@���@��0@���@�8@�� @��D@���@���@��N@��X@���@���@�w2@�rG@�rG@�j�@�\�@�"�@��@��x@�^5@�B[@��@��
@�x�@�(@���@���@�s�@�e�@�Z@�($@���@���@�B�@�'�@�(@��@���@�_@�
�@���@�]�@�33@�
=@���@���@�_@��@33@~�@~c @~+k@}��@}�@|��@|��@|!@|G@{��@{H�@z�L@zB[@y�3@y�@x�p@x$@w��@w��@w�4@v�@v�r@u�@uL�@t�@t~(@tPH@s�@s4�@r�@rp;@rR�@r@q�C@qc@q=�@q(�@q(�@q�@p�O@p|�@p�@o@O@o�@n�!@n�@n��@nq�@n_�@n@m�@l�9@l~@k�@k��@k&@jȴ@i�9@i�T@h�@h%�@g��@g�V@g��@g��@g]�@g)_@g
=@f�b@e<6@d��@d9X@cخ@c��@c~�@c(@b��@b�@b�@a��@`��@`��@`N�@`	�@_x@^�2@^��@^!�@]�@]�'@]��@]�@\	�@[j�@[(@Z��@Z~�@Y�@X��@X�@W�}@WC�@V��@V��@VYK@U��@Uϫ@U�@US&@T�|@T�@T��@T�_@Tw�@S�6@SS�@S6z@S�@R��@Rl�@R;�@Q�>@Q	l@P�@P֡@P��@P��@Pw�@Pw�@Pj@PA�@P!@O�A@Oƨ@O��@Os@OZ�@OX�@O@O@N��@N}V@M��@M-w@L��@L��@L/�@LG@K˒@K�[@K�@J�@J�s@J�6@J\�@I�.@I��@I#�@H�@HQ�@H-�@H�@G��@G��@GC�@GS@F��@FE�@E�)@E�#@E�'@E�7@EN<@D�v@Dh�@C�g@C��@CP�@B��@B�@A��@A�@A%F@@[�@?��@?dZ@?�@>��@>YK@=��@=x�@<ѷ@<?�@;�@; i@:�@:GE@:�@:�@9�)@9�@9�@9�M@9IR@8��@8�@7b�@6�@6�@5��@5�@4�P@4ی@4N�@3��@3��@3��@3��@3v`@3$t@2L0@1�Z@1�9@1��@1zx@1Vm@1=�@12a@1&�@0��@0U2@0G@/�m@/�K@/K�@.�@._�@.@-�=@-\�@-F@,�|@,��@,�z@,|�@,U2@+�A@+�[@+\)@*��@*�b@*�A@*n�@*a|@*Z�@*Z�@*B[@)�@)^�@)?}@)+�@)*0@) \@)V@)	l@(�u@(�@'�r@'y�@'9�@&}V@&_@%�j@%��@%Y�@$��@$Ĝ@$�_@$u�@$*�@#X�@"�@"�x@"J�@"B[@"@�@"5?@")�@"_@!�#@!�t@!k�@!O�@!5�@!q@!�@!@@!�@!	l@ ��@ ��@ �@ ��@ m�@ ~@�+@��@)_@�@��@��@p;@V@3�@O@{@J@�@�~@(�@�@�@��@�@� @�q@�	@E9@�M@�@p;@Z�@L0@3�@��@�@��@rG@�@��@�@�I@oi@bN@U2@<�@6@'R@�@G@�@��@"�@��@��@�A@Ov@O@ϫ@�z@��@�'@��@+�@Ɇ@��@��@u�@g8@M@1'@M@�A@ݘ@�@��@��@�f@|�@>�@6z@4�@4�@4�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	B	)B	)B	)B	�B	B	B	)B	)B	�B	B	�B	B	)B	)B	)B	B	B	)B	)B	B	CB	xB	�B	xB	]B	�B	B	B	$&B	(
B	'mB	&�B	&�B	&�B	'�B	($B	(�B	-]B	/�B	O�B	V�B	g�B
e�B
�1B
�RBSB'B*B-wB1[B+B,qB6�B9rB,"B�B
��B
��B
��B
�jB	�BsB
��B
�IB
��B
��B
bhB
C�B
AB
88B
+�B
%�B
B
�B
DB
�B	�B	�B	��B	�B	�B	�1B	�lB	�2B	�]B	�B	t�B	dB	`BB	X�B	G+B	@ B	=qB	8�B	+�B	%FB	dB	�B	,B	�B	�B	�B	:B	7B	�B	�B	�B	$�B	'�B	(
B	0UB	<jB	D�B	Q B	\]B	\]B	h
B	o B	qB	qvB	s�B	��B	�1B	��B	�1B	��B	��B	��B	��B	ªB	�PB	�[B	�eB	�VB	ߊB	�$B	��B	�/B	�9B	��B	�dB
�B
~B
HB
�B
	�B
�B
�B

�B
xB
vB
uB
�B
�B
NB
[B
TB
�B
 B
4B
�B
hB
bB
^B
tB
B
fB
B
~B
�B
"B
vB
vB
BB
bB
B
7B
�B
,B
FB
�B
�B
#�B
)�B
%`B
pB
1B
�B
/OB
1B
1�B
0�B
0�B
.IB
,�B
,�B
'�B
;B
�B
@B
vB
BB
 B
B
�B
�B
;B
$&B
$�B
#�B
!�B
�B
�B
SB
&B
�B
�B
QB
yB
�B
MB
MB
�B
B
�B
�B
HB
\B
"B
	�B
�B
�B
�B
�B
�B
B	��B	�.B
 �B
 OB
 4B	��B	�wB	�"B	��B	�	B	�lB	��B	��B	��B	��B	�.B	�.B	�]B	�<B	��B	�<B	��B	��B	��B	��B	�jB	��B	��B	�"B	��B	��B	��B	��B	�B	��B	��B	��B	��B
 4B
[B
'B
uB
�B
;B
 �B
 B	�cB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�JB	�B	�8B	��B	�B	�2B	��B	��B	��B	��B	��B	�zB	�B	��B	��B	�ZB	��B	��B	��B	�`B	��B	��B	�+B	�+B	�XB	�>B	��B	�xB	��B	��B	�qB	�BB	�wB	��B	��B	��B	��B	��B	�}B	�]B	�PB	�PB	�B	��B	�<B
 �B
 iB	��B	��B	��B	�B	��B	�cB	�}B
 B
 OB
�B
oB
oB
B
�B
oB
B
'B
�B
uB
�B
�B
 B
  B	��B	�B	�B	�B	��B	��B	�6B	�jB	�B	��B	�B	�B	�qB	��B	��B	��B	��B	��B
 OB
 �B
 B
�B
�B
 iB	��B	�B	�(B	�PB	�B	��B	��B	��B	��B	��B	�0B	�dB	�xB	��B	��B	�8B	�B	��B	��B	�rB	��B	��B	��B	�dB	��B	��B	�cB	��B	��B
 �B
UB
UB
�B
�B
�B
�B
�B
{B
{B
-B
AB
�B
oB
�B
�B
�B
�B
�B
�B
aB
gB
�B
B
B
�B
9B
B
B
B
�B
MB
�B
�B
�B
B
mB
�B
zB
_B
�B
B
�B
�B
�B
fB
�B
�B
KB
fB

XB
B
DB

=B
	�B

�B

#B
	7B
	RB
	�B
B
B
�B
BB
}B
�B
B
NB
:B
TB
:B
B
�B
�B
:B
�B
�B
�B
�B
�B
�B
B
�B
mB
$B
YB
�B
�B
�B
yB
yB
B
B
B
�B
B
�B
B
�B
�B
�B
=B
CB
/B
�B
�B
jB
B
�B
�B
�B
�B
�B
VB
pB
�B
�B
�B
 BB
 �B
 �B
!-B
!-B
!HB
!|B
!�B
"B
"hB
"�B
!�B
!�B
!�B
!�B
!�B
"NB
"�B
# B
# B
#B
#B
"�B
#TB
#�B
$tB
$@B
$�B
$�B
$tB
$�B
%B
$�B
%FB
%FB
%`B
%,B
%FB
%`B
%FB
%�B
&fB
&�B
&�B
'mB
'�B
(XB
(�B
(�B
)yB
)�B
)�B
*B
*0B
)�B
)B
($B
'�B
'�B
(
B
(>B
(�B
(�B
(�B
)B
(�B
(�B
)DB
)_B
)�B
*eB
*eB
*B
*�B
*�B
+kB
+�B
,qB
-B
-CB
-wB
.B
./B
.�B
/B
/5B
/OB
/iB
/iB
/�B
/�B
0;B
0�B
0�B
1�B
4B
4B
4TB
4�B
5B
4�B
5?B
6�B
6�B
6�B
7LB
7�B
8B
7�B
7�B
7�B
8RB
8RB
8�B
8�B
8�B
8lB
8�B
9	B
9>B
9�B
:^B
:�B
:�B
;�B
=qB
>B
>�B
?cB
?�B
?�B
@ B
@4B
@B
@4B
@iB
@iB
A�B
A�B
A�B
B'B
B'B
BAB
A�B
B'B
B�B
C{B
C�B
C�B
C�B
C�B
DB
DMB
DMB
D�B
FtB
F�B
F�B
GEB
GzB
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
H�B
I7B
IRB
IlB
I�B
I�B
I�B
I�B
J#B
J�B
K^B
K�B
K�B
K�B
L0B
L�B
MB
MB
M�B
M�B
M�B
N"B
N<B
NVB
N<B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
PB
O�B
PB
PHB
P�B
P�B
P�B
P�B
Q B
QB
Q B
Q B
QB
QB
QNB
QNB
QNB
Q�B
QhB
QhB
Q�B
R:B
Q�B
R�B
S&B
S&B
S&B
S[B
S[B
S�B
S�B
TFB
T,B
T,B
TFB
TaB
T�B
T�B
T�B
U2B
UMB
UgB
UMB
UgB
U�B
U�B
U�B
U�B
VB
U�B
U�B
VB
U�B
U�B
V9B
V�B
V�B
W$B
W$B
W�B
W�B
W�B
XB
XEB
X�B
YKB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
[#B
[WB
[qB
\�B
\�B
\�B
\�B
\�B
\�B
]B
\�B
]/B
]/B
]/B
]�B
^jB
^�B
_�B
_�B
`'B
`'B
`'B
`�B
`�B
aB
aB
aB
aB
abB
b4B
b4B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
eFB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
fLB
ffB
f�B
gRB
g8B
gmB
gRB
gmB
gmB
gmB
gmB
h
B
h>B
hXB
h�B
hsB
hsB
h�B
hsB
iB
iyB
iyB
i�B
i�B
j�B
kB
kB
kkB
k�B
k�B
l"B
l=B
l=B
lqB
m)B
mwB
m�B
n/B
n/B
n/B
n/B
nIB
ncB
n}B
n�B
o B
oB
o5B
oOB
oOB
oOB
oOB
oiB
oiB
oiB
oiB
oiB
o�B
pB
p!B
p�B
p�B
qB
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s�B
s�B
s�B
tB
tB
tnB
t�B
uB
t�B
uB
uB
utB
uZB
utB
u�B
vB
vB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y�B
y�B
y�B
zB
zB
z*B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{0B
{JB
{JB
{0B
{dB
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	B	)B	)B	)B	�B	B	B	)B	)B	�B	B	�B	B	)B	)B	)B	B	B	)B	)B	B	CB	xB	�B	xB	]B	�B	B	B	$&B	(
B	'mB	&�B	&�B	&�B	'�B	($B	(�B	-]B	/�B	O�B	V�B	g�B
e�B
�1B
�RBSB'B*B-wB1[B+B,qB6�B9rB,"B�B
��B
��B
��B
�jB	�BsB
��B
�IB
��B
��B
bhB
C�B
AB
88B
+�B
%�B
B
�B
DB
�B	�B	�B	��B	�B	�B	�1B	�lB	�2B	�]B	�B	t�B	dB	`BB	X�B	G+B	@ B	=qB	8�B	+�B	%FB	dB	�B	,B	�B	�B	�B	:B	7B	�B	�B	�B	$�B	'�B	(
B	0UB	<jB	D�B	Q B	\]B	\]B	h
B	o B	qB	qvB	s�B	��B	�1B	��B	�1B	��B	��B	��B	��B	ªB	�PB	�[B	�eB	�VB	ߊB	�$B	��B	�/B	�9B	��B	�dB
�B
~B
HB
�B
	�B
�B
�B

�B
xB
vB
uB
�B
�B
NB
[B
TB
�B
 B
4B
�B
hB
bB
^B
tB
B
fB
B
~B
�B
"B
vB
vB
BB
bB
B
7B
�B
,B
FB
�B
�B
#�B
)�B
%`B
pB
1B
�B
/OB
1B
1�B
0�B
0�B
.IB
,�B
,�B
'�B
;B
�B
@B
vB
BB
 B
B
�B
�B
;B
$&B
$�B
#�B
!�B
�B
�B
SB
&B
�B
�B
QB
yB
�B
MB
MB
�B
B
�B
�B
HB
\B
"B
	�B
�B
�B
�B
�B
�B
B	��B	�.B
 �B
 OB
 4B	��B	�wB	�"B	��B	�	B	�lB	��B	��B	��B	��B	�.B	�.B	�]B	�<B	��B	�<B	��B	��B	��B	��B	�jB	��B	��B	�"B	��B	��B	��B	��B	�B	��B	��B	��B	��B
 4B
[B
'B
uB
�B
;B
 �B
 B	�cB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�JB	�B	�8B	��B	�B	�2B	��B	��B	��B	��B	��B	�zB	�B	��B	��B	�ZB	��B	��B	��B	�`B	��B	��B	�+B	�+B	�XB	�>B	��B	�xB	��B	��B	�qB	�BB	�wB	��B	��B	��B	��B	��B	�}B	�]B	�PB	�PB	�B	��B	�<B
 �B
 iB	��B	��B	��B	�B	��B	�cB	�}B
 B
 OB
�B
oB
oB
B
�B
oB
B
'B
�B
uB
�B
�B
 B
  B	��B	�B	�B	�B	��B	��B	�6B	�jB	�B	��B	�B	�B	�qB	��B	��B	��B	��B	��B
 OB
 �B
 B
�B
�B
 iB	��B	�B	�(B	�PB	�B	��B	��B	��B	��B	��B	�0B	�dB	�xB	��B	��B	�8B	�B	��B	��B	�rB	��B	��B	��B	�dB	��B	��B	�cB	��B	��B
 �B
UB
UB
�B
�B
�B
�B
�B
{B
{B
-B
AB
�B
oB
�B
�B
�B
�B
�B
�B
aB
gB
�B
B
B
�B
9B
B
B
B
�B
MB
�B
�B
�B
B
mB
�B
zB
_B
�B
B
�B
�B
�B
fB
�B
�B
KB
fB

XB
B
DB

=B
	�B

�B

#B
	7B
	RB
	�B
B
B
�B
BB
}B
�B
B
NB
:B
TB
:B
B
�B
�B
:B
�B
�B
�B
�B
�B
�B
B
�B
mB
$B
YB
�B
�B
�B
yB
yB
B
B
B
�B
B
�B
B
�B
�B
�B
=B
CB
/B
�B
�B
jB
B
�B
�B
�B
�B
�B
VB
pB
�B
�B
�B
 BB
 �B
 �B
!-B
!-B
!HB
!|B
!�B
"B
"hB
"�B
!�B
!�B
!�B
!�B
!�B
"NB
"�B
# B
# B
#B
#B
"�B
#TB
#�B
$tB
$@B
$�B
$�B
$tB
$�B
%B
$�B
%FB
%FB
%`B
%,B
%FB
%`B
%FB
%�B
&fB
&�B
&�B
'mB
'�B
(XB
(�B
(�B
)yB
)�B
)�B
*B
*0B
)�B
)B
($B
'�B
'�B
(
B
(>B
(�B
(�B
(�B
)B
(�B
(�B
)DB
)_B
)�B
*eB
*eB
*B
*�B
*�B
+kB
+�B
,qB
-B
-CB
-wB
.B
./B
.�B
/B
/5B
/OB
/iB
/iB
/�B
/�B
0;B
0�B
0�B
1�B
4B
4B
4TB
4�B
5B
4�B
5?B
6�B
6�B
6�B
7LB
7�B
8B
7�B
7�B
7�B
8RB
8RB
8�B
8�B
8�B
8lB
8�B
9	B
9>B
9�B
:^B
:�B
:�B
;�B
=qB
>B
>�B
?cB
?�B
?�B
@ B
@4B
@B
@4B
@iB
@iB
A�B
A�B
A�B
B'B
B'B
BAB
A�B
B'B
B�B
C{B
C�B
C�B
C�B
C�B
DB
DMB
DMB
D�B
FtB
F�B
F�B
GEB
GzB
G_B
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
H�B
I7B
IRB
IlB
I�B
I�B
I�B
I�B
J#B
J�B
K^B
K�B
K�B
K�B
L0B
L�B
MB
MB
M�B
M�B
M�B
N"B
N<B
NVB
N<B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
PB
O�B
PB
PHB
P�B
P�B
P�B
P�B
Q B
QB
Q B
Q B
QB
QB
QNB
QNB
QNB
Q�B
QhB
QhB
Q�B
R:B
Q�B
R�B
S&B
S&B
S&B
S[B
S[B
S�B
S�B
TFB
T,B
T,B
TFB
TaB
T�B
T�B
T�B
U2B
UMB
UgB
UMB
UgB
U�B
U�B
U�B
U�B
VB
U�B
U�B
VB
U�B
U�B
V9B
V�B
V�B
W$B
W$B
W�B
W�B
W�B
XB
XEB
X�B
YKB
YeB
Y�B
Y�B
ZB
ZQB
Z�B
[#B
[WB
[qB
\�B
\�B
\�B
\�B
\�B
\�B
]B
\�B
]/B
]/B
]/B
]�B
^jB
^�B
_�B
_�B
`'B
`'B
`'B
`�B
`�B
aB
aB
aB
aB
abB
b4B
b4B
bNB
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
eFB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
fLB
ffB
f�B
gRB
g8B
gmB
gRB
gmB
gmB
gmB
gmB
h
B
h>B
hXB
h�B
hsB
hsB
h�B
hsB
iB
iyB
iyB
i�B
i�B
j�B
kB
kB
kkB
k�B
k�B
l"B
l=B
l=B
lqB
m)B
mwB
m�B
n/B
n/B
n/B
n/B
nIB
ncB
n}B
n�B
o B
oB
o5B
oOB
oOB
oOB
oOB
oiB
oiB
oiB
oiB
oiB
o�B
pB
p!B
p�B
p�B
qB
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
r�B
s�B
s�B
s�B
tB
tB
tnB
t�B
uB
t�B
uB
uB
utB
uZB
utB
u�B
vB
vB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
y	B
x�B
x�B
y�B
y�B
y�B
zB
zB
z*B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{0B
{JB
{JB
{0B
{dB
{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104933  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174420  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174421  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174421                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024428  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024428  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                