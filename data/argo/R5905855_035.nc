CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:16:44Z creation;2022-06-04T19:16:44Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191644  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               #A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��e`�a1   @��e����@0|�hr��cθQ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@y��@�  A   A   A@  A`  A~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  BÙ�B�33B�ffBϙ�B�  B�  B�  B�  B�  B�  B�ffB���B�  B�33B���B���C  C  C  C  C
  C  C33C  C�C�fC  C  C�fC  C  C   C"  C$  C&  C(�C*33C,�C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�3D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @$z�@qG�@��
@��
A�A=�A]�A|Q�A�(�A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_�GBgz�Boz�Bwz�Bz�B��qB��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��>B��qB�WB��B�#�B�WBӽqB׽qB۽qB߽qB�qB�qB�#�B�>B�qB��B��>B��>C޸C޸C޸C޸C	޸C޸C�C޸C�RC�C޸C޸C�C޸C޸C޸C!޸C#޸C%޸C'�RC*�C+�RC-޸C/޸C1�C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]�C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck�RCm�RCo޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG�DH~DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D��
D�?
D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��QA��QA��MA��cA��WA��AA��2A��A��A��A��A��]A��.A�  A�A��A��A�uA��A�{A��A�A��A��A��A��A�1A��A�_A��A�
�A�DA��A�JA�A�A�_A��A�A��A��A��A�!-A�#�A�!A̓�À�A�D�A�K�A�L0A�-A��OA��A�e�A�$tA��fA���A���A�.A���A�[�A���A�($A�*�A��-A�.�A�̘A�b�A��&A�6A�#�A���A�h�A��;A��A�G�A��`A��A���A�W�A�K^A���A�L0A�YA�Az�"Awm]At�As�{Al��Ai�eAg�sAc�Aa�A]n/AY�ATe�AQ�"ANm]AKOvAH�nAF��AF�XAE��AC%�A?�]A=4A:�`A9A7|�A7_A4�DA3_A1A�A0 iA/e�A._A-0UA,S�A+:�A)��A(�hA'�eA&��A&VA%zxA$y�A#��A"��A"S&A!�.A S&A_pA�A��A�A-�Aa|A��AA��A;dA�rArGA�AkQA?A�A�oA*�Au�A�A�A�]A�A7AJ#A�FA
+A	�5A	�!A	�"A	��A	��A
($A	�A	P�AH�AF�A�A��A��A��A�#ATaA��A�A��A˒A��A]dAt�A�oAɆAMjA iA �A 7@���@���@�1�@��@��@��6@�~(@�|@��@�l"@�R�@��@�t�@�c @�خ@���@�Q@�x@�@�~@�j@��@�!�@�@�p�@�&@�x@���@�.I@�RT@��@�@�z@��@��@�ߤ@�"@�F@�*0@�ں@��E@��}@��d@�iD@�J@�GE@䖼@��@�]�@���@�D�@ޡb@�S�@�(�@�k�@�c @�-�@��}@��@�g8@�$@ٮ@�v`@�P�@�o@�i�@�q@���@ץ�@ֹ$@��@�|@�7L@��@�+@��@��@���@԰!@�n�@��o@ӷ@��|@�`�@ѨX@с@�A�@а�@�l�@�M@��@�1@��)@�ԕ@Ϛk@��B@�e�@͎�@���@̹�@�9X@��A@��#@ˈf@ʛ�@��Z@�ƨ@�t�@�+@ȅ�@��@�_@ǽ�@�A @��K@�ff@�H@�  @�@�+k@��T@�~�@�%@��@´9@���@��B@���@��@��"@��@�(@���@�H@�!�@��@��@�y>@��@�T�@�33@�@���@�;�@���@��h@�33@��?@�9X@��@�X@���@���@�PH@��@��@��C@��[@�6@�u@��@��a@�W?@���@��O@�|�@�H�@�@��a@���@��{@� \@��E@�^5@��}@��$@�%F@���@�Ĝ@���@�bN@��@�Q�@�1�@��2@���@�6�@��S@��@��/@��r@�:�@��"@��@��e@��u@�u%@�0U@�4@��Z@��3@�j@�F@� \@��"@��K@���@��u@�R�@��@�Vm@��@�ں@��@���@�!@��]@��o@��d@���@�.I@��$@�q@�N�@��@�ԕ@���@�Q�@�S&@�`B@�E9@�S@��p@�Z�@�%�@�u�@�
=@��@��9@��A@�?@�u@��@��@�}�@�6z@� i@�͟@��9@�M�@��)@���@�$t@��@�~(@�N�@�L0@�H@�A�@���@��f@��@��$@��@��A@�M@��@���@�}�@�g�@�iD@�>�@���@�[�@�#:@��@��q@�IR@�%@��e@��W@�F@��/@��j@��$@���@��@�YK@�J�@�	�@���@���@�U�@�1�@���@�Ĝ@�+k@���@�s�@��@��v@���@�l�@��@���@��H@��M@�/@���@�5?@��-@�v`@�S&@��K@�YK@��@�|�@�V@�}V@���@�'R@�~�@�O@�e�@�Y�@��@��@�5?@��.@��T@��@�j�@��@��1@�]d@��@��@���@��@��t@�e�@�)_@�@��]@���@��z@���@��u@�V@�1�@���@���@���@�hs@�7L@���@���@��j@���@�D�@�b@�  @��@��@
=@~{@}�t@}��@}%F@|]d@{��@{x@{S@z�}@z1�@y�@y��@y;@xی@x�D@xA�@x1@w�$@w+@v͟@v� @u�@uQ�@u-w@t�f@t%�@sƨ@s�@s�f@s�@sH�@r�@rO@q��@qQ�@p��@p�v@pz�@o��@o6z@n�<@np;@nC�@m�)@m�@m�^@m-w@l��@l��@ltT@l]d@kƨ@kb�@kC@j�\@j$�@j�@i�@i��@i��@iY�@i�@h�E@h�@h7�@h~@g�r@g˒@g�*@gy�@gF�@f��@fR�@f+k@f	@e��@e\�@e#�@d�4@d�@c��@cdZ@b��@b�@ax�@aT�@`��@`�@`�e@`�o@`e�@`>B@`�@_�a@_@O@^�"@^�X@^M�@^-@]�Z@]J�@]@\��@\z�@\V�@\9X@\,=@\@[�A@[�$@[�@Z�F@Z	@Y�@Y@@X��@Xm�@W�@W|�@WC@W@Wo@V�@V~�@V_@U�Z@U�3@U4@Tѷ@T�.@S��@S~�@S�@R��@R��@RL0@Q��@Q�M@QN<@P�|@P��@P��@P�.@P7@O�@O�{@O1�@N��@N��@M�j@M�'@Mj@M0�@L�?@Lx@K��@J�y@JB[@J@I��@I�S@IY�@IA @I�@I;@H��@H��@H�@HPH@G��@GY@F��@F�B@F�@Fp;@Fa|@FkQ@F3�@F$�@Fu@E��@E8�@D��@Dj@D_@C��@C�:@C��@Cy�@CRT@C>�@C)_@B�]@BQ@A�@A��@Ak�@A�@@�9@@]d@?��@?'�@>�@>�+@>h
@>@=�t@=Y�@<��@<ѷ@<��@<7@;�q@;��@;P�@;�@:�,@:��@:d�@:+k@:_@9��@9�@9��@97L@9q@8��@8��@8�D@8q@7��@7��@7C�@7 i@6�,@6i�@6
�@5��@5}�@5=�@4��@4�_@4Q�@4�@3��@3�{@3�@2��@2q�@2Ta@2L0@1�o@1w2@1F@1#�@0�|@0�@0��@0<�@07�@0@/�$@/X�@/;d@/C@.��@.�!@.u%@-��@-��@-�t@-��@-��@-Dg@,�P@,�@,�_@,K^@,�@+�W@+��@+��@+_p@+'�@*�H@*�@*H�@*�@)��@)��@)G�@)@@(�Y@(?�@(2�@(M@'�K@'�f@'j�@'.I@&͟@&��@&u%@&�@%��@%rG@%\�@$��@$tT@#��@#�;@#�0@#��@#E9@#(@"�,@"��@"Ta@" �@!��@!T�@ ی@ ��@ �@ _@ M@�
@��@��@y�@+@�"@�<@z@@��@�3@}�@8�@�f@�U@��@e�@I�@1'@@�g@�k@�f@Z�@�@��@�s@�@�!@xl@-@�@�C@��@X@:�@�@�j@�e@��@]d@%�@��@�m@�@9�@�"@�,@�}@��@p;@C�@_@Y�@�@ѷ@��@��@w�@m�@c�@N�@%�@�@��@=@33@�@�y@҉@�}@��@z@kQ@=q@�o@��@�=@��@f�@O�@!�@�5@�@��@�U@��@Z@S�@>B@/�@%�@�@� @�*@�f@v`@J#@)_@�@�y@�B@�@��@p;@l�@Z�@;�@4@ �@��@�@�@ԕ@��@�'@�7@|@S&@B�@5�@#�@�@Ĝ@��@�Y@e�@Xy11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��QA��QA��MA��cA��WA��AA��2A��A��A��A��A��]A��.A�  A�A��A��A�uA��A�{A��A�A��A��A��A��A�1A��A�_A��A�
�A�DA��A�JA�A�A�_A��A�A��A��A��A�!-A�#�A�!A̓�À�A�D�A�K�A�L0A�-A��OA��A�e�A�$tA��fA���A���A�.A���A�[�A���A�($A�*�A��-A�.�A�̘A�b�A��&A�6A�#�A���A�h�A��;A��A�G�A��`A��A���A�W�A�K^A���A�L0A�YA�Az�"Awm]At�As�{Al��Ai�eAg�sAc�Aa�A]n/AY�ATe�AQ�"ANm]AKOvAH�nAF��AF�XAE��AC%�A?�]A=4A:�`A9A7|�A7_A4�DA3_A1A�A0 iA/e�A._A-0UA,S�A+:�A)��A(�hA'�eA&��A&VA%zxA$y�A#��A"��A"S&A!�.A S&A_pA�A��A�A-�Aa|A��AA��A;dA�rArGA�AkQA?A�A�oA*�Au�A�A�A�]A�A7AJ#A�FA
+A	�5A	�!A	�"A	��A	��A
($A	�A	P�AH�AF�A�A��A��A��A�#ATaA��A�A��A˒A��A]dAt�A�oAɆAMjA iA �A 7@���@���@�1�@��@��@��6@�~(@�|@��@�l"@�R�@��@�t�@�c @�خ@���@�Q@�x@�@�~@�j@��@�!�@�@�p�@�&@�x@���@�.I@�RT@��@�@�z@��@��@�ߤ@�"@�F@�*0@�ں@��E@��}@��d@�iD@�J@�GE@䖼@��@�]�@���@�D�@ޡb@�S�@�(�@�k�@�c @�-�@��}@��@�g8@�$@ٮ@�v`@�P�@�o@�i�@�q@���@ץ�@ֹ$@��@�|@�7L@��@�+@��@��@���@԰!@�n�@��o@ӷ@��|@�`�@ѨX@с@�A�@а�@�l�@�M@��@�1@��)@�ԕ@Ϛk@��B@�e�@͎�@���@̹�@�9X@��A@��#@ˈf@ʛ�@��Z@�ƨ@�t�@�+@ȅ�@��@�_@ǽ�@�A @��K@�ff@�H@�  @�@�+k@��T@�~�@�%@��@´9@���@��B@���@��@��"@��@�(@���@�H@�!�@��@��@�y>@��@�T�@�33@�@���@�;�@���@��h@�33@��?@�9X@��@�X@���@���@�PH@��@��@��C@��[@�6@�u@��@��a@�W?@���@��O@�|�@�H�@�@��a@���@��{@� \@��E@�^5@��}@��$@�%F@���@�Ĝ@���@�bN@��@�Q�@�1�@��2@���@�6�@��S@��@��/@��r@�:�@��"@��@��e@��u@�u%@�0U@�4@��Z@��3@�j@�F@� \@��"@��K@���@��u@�R�@��@�Vm@��@�ں@��@���@�!@��]@��o@��d@���@�.I@��$@�q@�N�@��@�ԕ@���@�Q�@�S&@�`B@�E9@�S@��p@�Z�@�%�@�u�@�
=@��@��9@��A@�?@�u@��@��@�}�@�6z@� i@�͟@��9@�M�@��)@���@�$t@��@�~(@�N�@�L0@�H@�A�@���@��f@��@��$@��@��A@�M@��@���@�}�@�g�@�iD@�>�@���@�[�@�#:@��@��q@�IR@�%@��e@��W@�F@��/@��j@��$@���@��@�YK@�J�@�	�@���@���@�U�@�1�@���@�Ĝ@�+k@���@�s�@��@��v@���@�l�@��@���@��H@��M@�/@���@�5?@��-@�v`@�S&@��K@�YK@��@�|�@�V@�}V@���@�'R@�~�@�O@�e�@�Y�@��@��@�5?@��.@��T@��@�j�@��@��1@�]d@��@��@���@��@��t@�e�@�)_@�@��]@���@��z@���@��u@�V@�1�@���@���@���@�hs@�7L@���@���@��j@���@�D�@�b@�  @��@��@
=@~{@}�t@}��@}%F@|]d@{��@{x@{S@z�}@z1�@y�@y��@y;@xی@x�D@xA�@x1@w�$@w+@v͟@v� @u�@uQ�@u-w@t�f@t%�@sƨ@s�@s�f@s�@sH�@r�@rO@q��@qQ�@p��@p�v@pz�@o��@o6z@n�<@np;@nC�@m�)@m�@m�^@m-w@l��@l��@ltT@l]d@kƨ@kb�@kC@j�\@j$�@j�@i�@i��@i��@iY�@i�@h�E@h�@h7�@h~@g�r@g˒@g�*@gy�@gF�@f��@fR�@f+k@f	@e��@e\�@e#�@d�4@d�@c��@cdZ@b��@b�@ax�@aT�@`��@`�@`�e@`�o@`e�@`>B@`�@_�a@_@O@^�"@^�X@^M�@^-@]�Z@]J�@]@\��@\z�@\V�@\9X@\,=@\@[�A@[�$@[�@Z�F@Z	@Y�@Y@@X��@Xm�@W�@W|�@WC@W@Wo@V�@V~�@V_@U�Z@U�3@U4@Tѷ@T�.@S��@S~�@S�@R��@R��@RL0@Q��@Q�M@QN<@P�|@P��@P��@P�.@P7@O�@O�{@O1�@N��@N��@M�j@M�'@Mj@M0�@L�?@Lx@K��@J�y@JB[@J@I��@I�S@IY�@IA @I�@I;@H��@H��@H�@HPH@G��@GY@F��@F�B@F�@Fp;@Fa|@FkQ@F3�@F$�@Fu@E��@E8�@D��@Dj@D_@C��@C�:@C��@Cy�@CRT@C>�@C)_@B�]@BQ@A�@A��@Ak�@A�@@�9@@]d@?��@?'�@>�@>�+@>h
@>@=�t@=Y�@<��@<ѷ@<��@<7@;�q@;��@;P�@;�@:�,@:��@:d�@:+k@:_@9��@9�@9��@97L@9q@8��@8��@8�D@8q@7��@7��@7C�@7 i@6�,@6i�@6
�@5��@5}�@5=�@4��@4�_@4Q�@4�@3��@3�{@3�@2��@2q�@2Ta@2L0@1�o@1w2@1F@1#�@0�|@0�@0��@0<�@07�@0@/�$@/X�@/;d@/C@.��@.�!@.u%@-��@-��@-�t@-��@-��@-Dg@,�P@,�@,�_@,K^@,�@+�W@+��@+��@+_p@+'�@*�H@*�@*H�@*�@)��@)��@)G�@)@@(�Y@(?�@(2�@(M@'�K@'�f@'j�@'.I@&͟@&��@&u%@&�@%��@%rG@%\�@$��@$tT@#��@#�;@#�0@#��@#E9@#(@"�,@"��@"Ta@" �@!��@!T�@ ی@ ��@ �@ _@ M@�
@��@��@y�@+@�"@�<@z@@��@�3@}�@8�@�f@�U@��@e�@I�@1'@@�g@�k@�f@Z�@�@��@�s@�@�!@xl@-@�@�C@��@X@:�@�@�j@�e@��@]d@%�@��@�m@�@9�@�"@�,@�}@��@p;@C�@_@Y�@�@ѷ@��@��@w�@m�@c�@N�@%�@�@��@=@33@�@�y@҉@�}@��@z@kQ@=q@�o@��@�=@��@f�@O�@!�@�5@�@��@�U@��@Z@S�@>B@/�@%�@�@� @�*@�f@v`@J#@)_@�@�y@�B@�@��@p;@l�@Z�@;�@4@ �@��@�@�@ԕ@��@�'@�7@|@S&@B�@5�@#�@�@Ĝ@��@�Y@e�@Xy11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	6B	6B	jB	PB	jB	PB	jB	PB	PB	jB	�B	PB	PB	jB	�B	�B	�B	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	�B	B	�B	�B	�B	B	�B	�B	�B	"B	HfB	LB	`�B	mB
aB
B
�B
1�B
8�B
;JB
<�B
="B
F�B
��B
�\B
�B
�BB
��B
��B
��B
�B
��B
��B
��B
��B
�:B
�~B
��B
�9B
vB
_�B
J#B
/�B
)_B
!HB
oB	�FB	ۦB	�}B	�+B	�+B	��B	�dB	�FB	q�B	cB	V�B	@�B	0!B	�B	4B��B�B��B�BڠB��BөB�pB�dB�SB��B�;B��B�.B�JB�xB��B�^B��B��B�HB��B�OB��B�B��B��B�3BªBB�BB�B�B��B��BȚB�B��B��B��BǮB�)BևB��B�,B�eB��B�B�`B�wB	�B	B	 B	�B	gB	-]B	@4B	?.B	>�B	'RB	�B�B�6B�JB�.B�oBڠB��B�B�2B��B��B��B	�B	1B	�B	�B��B��B	�B	�B		�B	bB	2B	�B	'B	2�B	4nB	3�B	2�B	2�B	1AB	0�B	1'B	3B	3hB	6`B	7�B	8�B	:xB	<�B	A�B	EB	G�B	L0B	PbB	RB	S�B	XyB	Y�B	Z�B	ZkB	Z�B	\�B	]/B	^�B	^�B	^�B	_;B	`\B	b�B	`\B	_B	`\B	d&B	hXB	w�B	��B	�B	��B	��B	�9B	��B	��B	�hB	��B	��B	�&B	��B	�)B	�#B	��B	�sB	��B	�@B	��B	�	B	��B	�EB	�yB	��B	�#B	�	B	��B	�B	�)B	��B	��B	�-B	��B	��B	�-B	��B	�dB	�xB	�B	��B	��B	��B	�UB	��B	��B	��B	�`B	�8B	��B	��B	ªB	�B	��B	ƨB	��B	��B	�B	�fB	�fB	ȚB	ʦB	�B	�PB	�B	͹B	�VB	��B	� B	��B	��B	��B	�FB	�FB	�2B	՛B	�B	�sB	�B	�yB	��B	�7B	ںB	�]B	��B	��B	�bB	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�"B	�WB	�"B	�B	��B	��B	�=B	�"B	�=B	��B	�)B	�wB	�CB	��B	��B	��B	�oB	�B	��B	�B	�B	�3B	��B	�|B	�B	��B	�|B	�B	�B	�aB	�-B	�|B	�B	��B	�B	�3B	�B	�B	�hB	�B	�B	�B	�B	�GB	�-B	�GB	��B	��B	�B	�B	�B	�B	��B	��B	��B	�FB	��B	�LB	�fB	�lB	��B	�rB	�rB	��B	�*B	�*B	�*B	��B	�0B	�JB	��B	��B	��B	�jB	��B	�B	�qB	�wB	��B	�B	�HB	�.B
  B
  B	��B
  B
 OB
 iB
�B
'B
uB
[B
uB
�B
MB
�B
B
mB
mB
�B
�B
�B
tB
�B
B
+B
_B
�B
�B
�B
B
1B
�B
�B
	B
�B
	�B

=B

�B
)B
)B
�B
JB
0B
�B
�B
dB
�B
�B
�B
�B
jB
�B
�B
.B
�B
B
}B
�B
�B
oB
B
mB
mB
�B
�B
�B
�B
�B
$B
?B
sB
B
�B
�B
�B
/B
B
B
�B
�B
CB
�B
�B
�B
�B
B
QB
kB
�B
�B
�B
)B
�B
�B
IB
�B
�B
�B
IB
�B
5B
�B
dB
B
/B
�B
B
�B
�B
�B
 �B
 �B
 vB
 BB
 \B
!B
!�B
"�B
!�B
!B
!bB
!-B
 �B
!B
!|B
"4B
!�B
!�B
"hB
#TB
#�B
#�B
$&B
%�B
'8B
(>B
(�B
)yB
)yB
*B
*B
*�B
+6B
+6B
+�B
-)B
-�B
.�B
.�B
.�B
./B
-�B
.B
.�B
/�B
/�B
/�B
/iB
/�B
/�B
0�B
1vB
1vB
1�B
1�B
1�B
1[B
1vB
1�B
1�B
2B
1�B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5B
5?B
5B
5tB
5�B
6�B
6�B
6�B
7B
7fB
7fB
7fB
7�B
8B
8RB
8�B
8lB
9	B
9XB
9rB
9�B
:DB
:DB
:^B
:�B
:�B
:�B
;0B
;JB
;dB
;�B
;�B
;dB
;JB
;dB
;�B
;�B
<jB
<�B
=B
<�B
="B
=<B
=<B
=�B
=�B
>B
>BB
>�B
?cB
?�B
?�B
@4B
@iB
@�B
@�B
@�B
A B
A B
AUB
A�B
A�B
B'B
B�B
B�B
B�B
C-B
C-B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
D�B
E9B
ESB
E�B
F%B
FB
FYB
F�B
FYB
FtB
FtB
FYB
F�B
F�B
GEB
G�B
H�B
H�B
H�B
I7B
IB
I�B
I�B
J=B
JXB
JXB
J#B
JXB
J	B
JXB
JrB
JXB
J=B
J�B
KB
K)B
K�B
K�B
LB
L�B
MB
MPB
M�B
M�B
NVB
NVB
N�B
O(B
O(B
N�B
O(B
OBB
OBB
OvB
OvB
O�B
O�B
P.B
P�B
P�B
Q B
QNB
QNB
Q4B
QB
QB
Q�B
RB
RoB
R�B
R�B
R�B
R�B
R�B
RoB
R�B
R�B
R�B
SB
S@B
S[B
S�B
S�B
TB
S�B
S�B
TFB
TaB
TaB
T�B
UB
U�B
U�B
U�B
U�B
VB
V9B
V�B
W?B
W�B
W�B
XyB
XyB
X�B
YKB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
[�B
[�B
\B
\)B
[�B
[�B
\)B
\]B
\xB
\�B
]/B
]IB
]IB
]/B
]�B
]�B
]~B
]�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
aHB
a�B
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
c B
c:B
c:B
cTB
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
e�B
ffB
fLB
fLB
f�B
gB
gmB
g�B
g�B
g�B
g�B
h
B
hXB
hXB
h�B
h�B
i*B
iyB
i�B
jB
i�B
jKB
jB
j�B
j�B
j�B
j�B
kQB
kQB
k�B
k�B
lB
l"B
l=B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
n�B
o B
o B
o5B
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
qB
qB
q[B
qvB
qvB
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v`B
vzB
v�B
wB
w2B
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
xlB
xlB
xlB
x�B
x�B
x�B
y	B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
z�B
z�B
{B
{dB
{B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�B	6B	6B	jB	PB	jB	PB	jB	PB	PB	jB	�B	PB	PB	jB	�B	�B	�B	jB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	B	�B	B	�B	�B	�B	B	�B	�B	�B	"B	HfB	LB	`�B	mB
aB
B
�B
1�B
8�B
;JB
<�B
="B
F�B
��B
�\B
�B
�BB
��B
��B
��B
�B
��B
��B
��B
��B
�:B
�~B
��B
�9B
vB
_�B
J#B
/�B
)_B
!HB
oB	�FB	ۦB	�}B	�+B	�+B	��B	�dB	�FB	q�B	cB	V�B	@�B	0!B	�B	4B��B�B��B�BڠB��BөB�pB�dB�SB��B�;B��B�.B�JB�xB��B�^B��B��B�HB��B�OB��B�B��B��B�3BªBB�BB�B�B��B��BȚB�B��B��B��BǮB�)BևB��B�,B�eB��B�B�`B�wB	�B	B	 B	�B	gB	-]B	@4B	?.B	>�B	'RB	�B�B�6B�JB�.B�oBڠB��B�B�2B��B��B��B	�B	1B	�B	�B��B��B	�B	�B		�B	bB	2B	�B	'B	2�B	4nB	3�B	2�B	2�B	1AB	0�B	1'B	3B	3hB	6`B	7�B	8�B	:xB	<�B	A�B	EB	G�B	L0B	PbB	RB	S�B	XyB	Y�B	Z�B	ZkB	Z�B	\�B	]/B	^�B	^�B	^�B	_;B	`\B	b�B	`\B	_B	`\B	d&B	hXB	w�B	��B	�B	��B	��B	�9B	��B	��B	�hB	��B	��B	�&B	��B	�)B	�#B	��B	�sB	��B	�@B	��B	�	B	��B	�EB	�yB	��B	�#B	�	B	��B	�B	�)B	��B	��B	�-B	��B	��B	�-B	��B	�dB	�xB	�B	��B	��B	��B	�UB	��B	��B	��B	�`B	�8B	��B	��B	ªB	�B	��B	ƨB	��B	��B	�B	�fB	�fB	ȚB	ʦB	�B	�PB	�B	͹B	�VB	��B	� B	��B	��B	��B	�FB	�FB	�2B	՛B	�B	�sB	�B	�yB	��B	�7B	ںB	�]B	��B	��B	�bB	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�"B	�WB	�"B	�B	��B	��B	�=B	�"B	�=B	��B	�)B	�wB	�CB	��B	��B	��B	�oB	�B	��B	�B	�B	�3B	��B	�|B	�B	��B	�|B	�B	�B	�aB	�-B	�|B	�B	��B	�B	�3B	�B	�B	�hB	�B	�B	�B	�B	�GB	�-B	�GB	��B	��B	�B	�B	�B	�B	��B	��B	��B	�FB	��B	�LB	�fB	�lB	��B	�rB	�rB	��B	�*B	�*B	�*B	��B	�0B	�JB	��B	��B	��B	�jB	��B	�B	�qB	�wB	��B	�B	�HB	�.B
  B
  B	��B
  B
 OB
 iB
�B
'B
uB
[B
uB
�B
MB
�B
B
mB
mB
�B
�B
�B
tB
�B
B
+B
_B
�B
�B
�B
B
1B
�B
�B
	B
�B
	�B

=B

�B
)B
)B
�B
JB
0B
�B
�B
dB
�B
�B
�B
�B
jB
�B
�B
.B
�B
B
}B
�B
�B
oB
B
mB
mB
�B
�B
�B
�B
�B
$B
?B
sB
B
�B
�B
�B
/B
B
B
�B
�B
CB
�B
�B
�B
�B
B
QB
kB
�B
�B
�B
)B
�B
�B
IB
�B
�B
�B
IB
�B
5B
�B
dB
B
/B
�B
B
�B
�B
�B
 �B
 �B
 vB
 BB
 \B
!B
!�B
"�B
!�B
!B
!bB
!-B
 �B
!B
!|B
"4B
!�B
!�B
"hB
#TB
#�B
#�B
$&B
%�B
'8B
(>B
(�B
)yB
)yB
*B
*B
*�B
+6B
+6B
+�B
-)B
-�B
.�B
.�B
.�B
./B
-�B
.B
.�B
/�B
/�B
/�B
/iB
/�B
/�B
0�B
1vB
1vB
1�B
1�B
1�B
1[B
1vB
1�B
1�B
2B
1�B
2|B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4TB
4�B
4�B
5B
5?B
5B
5tB
5�B
6�B
6�B
6�B
7B
7fB
7fB
7fB
7�B
8B
8RB
8�B
8lB
9	B
9XB
9rB
9�B
:DB
:DB
:^B
:�B
:�B
:�B
;0B
;JB
;dB
;�B
;�B
;dB
;JB
;dB
;�B
;�B
<jB
<�B
=B
<�B
="B
=<B
=<B
=�B
=�B
>B
>BB
>�B
?cB
?�B
?�B
@4B
@iB
@�B
@�B
@�B
A B
A B
AUB
A�B
A�B
B'B
B�B
B�B
B�B
C-B
C-B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
D�B
E9B
ESB
E�B
F%B
FB
FYB
F�B
FYB
FtB
FtB
FYB
F�B
F�B
GEB
G�B
H�B
H�B
H�B
I7B
IB
I�B
I�B
J=B
JXB
JXB
J#B
JXB
J	B
JXB
JrB
JXB
J=B
J�B
KB
K)B
K�B
K�B
LB
L�B
MB
MPB
M�B
M�B
NVB
NVB
N�B
O(B
O(B
N�B
O(B
OBB
OBB
OvB
OvB
O�B
O�B
P.B
P�B
P�B
Q B
QNB
QNB
Q4B
QB
QB
Q�B
RB
RoB
R�B
R�B
R�B
R�B
R�B
RoB
R�B
R�B
R�B
SB
S@B
S[B
S�B
S�B
TB
S�B
S�B
TFB
TaB
TaB
T�B
UB
U�B
U�B
U�B
U�B
VB
V9B
V�B
W?B
W�B
W�B
XyB
XyB
X�B
YKB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZB
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
[�B
[�B
\B
\)B
[�B
[�B
\)B
\]B
\xB
\�B
]/B
]IB
]IB
]/B
]�B
]�B
]~B
]�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
aHB
a�B
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
c B
c:B
c:B
cTB
c�B
c�B
dtB
d�B
d�B
d�B
d�B
d�B
d�B
eFB
e�B
e�B
e�B
e�B
ffB
fLB
fLB
f�B
gB
gmB
g�B
g�B
g�B
g�B
h
B
hXB
hXB
h�B
h�B
i*B
iyB
i�B
jB
i�B
jKB
jB
j�B
j�B
j�B
j�B
kQB
kQB
k�B
k�B
lB
l"B
l=B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
nIB
n�B
n�B
n�B
o B
o B
o5B
o�B
o�B
pB
p!B
p;B
pUB
p�B
p�B
p�B
qB
qB
q[B
qvB
qvB
q�B
r-B
rGB
r|B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v`B
vzB
v�B
wB
w2B
w2B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
xlB
xlB
xlB
x�B
x�B
x�B
y	B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z^B
zxB
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
z�B
z�B
{B
{dB
{B
{�B
{�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191644  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191644  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191644                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041652  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041652  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                