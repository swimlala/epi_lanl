CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:57:34Z creation;2022-06-04T17:57:34Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175734  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�Yj_�Q�1   @�Yj�q�@.s33333�c�`A�71   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@�  @�  A��A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�ffB���B�  B�  B�  B�  B�  B�  B�ffB�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C9��C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@$z�@w�@��
@�
>A�A=�A]�A}�A���A���A�A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��B��qB�#�B��>B��qB��qB��qB��qB��qB��qB�#�B��qB��qB��>BÊ>BǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C�RC޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7�RC9xRC;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_�RCa޸Cc޸Ce޸Cg�Ci�Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��)C��)C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!�D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.�D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH�DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRqHDR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�
D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�#A�"hA�"�A�$@A�%A�%�A�'A�(XA��A�خAİUA��A�.A�>BA�I�A��tA��A�=�A�J�A�l�AŮIA���A��`A��]A��A���Aź�Aş!AŊ	A�:�A��A��`A�7AÖ�A�_;A�Q�A�A��A��NA�MA��oA�k�A�U�A���A��A�H�A�XA�bNA�;0A��A��XA�Q�A��|A�w�A��A�7�A��A��BA�\A��A���A�m�A��9A�6�A���A�h>A�.IA���A�FA�
�A�$A�M�A��oA��-A���A� 4A�[�A�>wA�_A�rA�ffA���A�t�A�=�A�"4A�A�7�A�A�-A���A���A���A�b�A�A{�Ay�Av{�As�Ao	AiK�Ad�vAb4A]��AWe,AS1�AQu�APL�AP\�AO�!AN�AIS&AG��AF�)AB�RA?��A=��A7��A0O�A-��A-_pA-��A-k�A,U2A,�/A-��A.��A-�HA,D�A,(�A-h�A0ZA1Y�A2J#A2��A2��A1�A/($A-GA,\�A)��A(4A'�4A&�A&VmA%��A%�{A%��A%�nA%�YA%c�A$�TA#�nA#OA#.�A"��A"�A"|A#�KA$s�A$�A$��A$\)A#CA �A�A}�A�bAO�A{A��A��A�A\�A�QAB[A�)A�A� Aq�A�A��AGEA��A%FAخA�AsA�"A�A5�A5�A�A��A��An/AM�A��AW?A%A�A<6AU2A-AaAjA}�AخAkQAOAD�A�A/A
��A
jA

=A	�6A	}VA	(AYA��AZA'RA��A^5A!A��A[�A��AtTA&�Ad�A֡A\�A�A �A��A�A��A;A ��@��@���@��@��@�خ@���@�ԕ@�Ta@��.@��@��@���@�dZ@��@���@���@�;d@��@�&�@�>B@�r@�n�@�{�@�@��@�2�@�S�@궮@���@�x@��M@�p;@���@�j@歬@��@㋬@�֡@�J@���@�rG@�r�@ߊ�@���@��@ݱ[@�Q�@�{�@�4n@���@�P�@�S�@��@��W@�m�@�J�@�S�@�_�@�c�@�D�@��@��#@�9�@�Xy@��@��@Ҝx@�j@�� @�5�@��m@Э�@П�@�3�@υ�@κ�@��A@��@͈f@͇�@͎"@��@ΆY@�_�@̲�@�M�@�
�@˖S@�q@� �@���@���@Ɉf@�p�@��@ȏ\@�0U@� �@�c�@��B@�<�@���@�X@�!-@��@ĔF@�y>@�H�@ø�@�G�@�ȴ@¡b@�Q�@���@��@�� @���@���@�{�@��&@��S@�}�@�2a@���@���@�Q@�~�@���@�!-@���@�  @��P@�+�@��I@�-@��o@��F@�l�@��M@�H�@��@���@�qv@�!-@��D@�C�@���@�A @���@�%�@���@�Q�@��@�:�@�:*@�7@��@��d@�W?@��@�Ta@���@��@�@�#�@�^5@�?�@��h@�@O@�+@���@���@�9X@��[@�A�@�!-@��U@�z@�4@��H@�c�@�<6@���@��X@���@�4n@��Q@��4@�1�@�Ɇ@�y>@�/�@��@���@�:�@��L@�oi@��@��@��K@��n@�x�@�=@���@�ff@�#:@��@��@�=@�V@��@��r@�%�@��]@��@�+@��m@���@�_@�6@��+@���@��@��@���@���@�YK@���@��S@�Dg@���@�7�@��]@��@�K�@�&�@�;@��U@�1�@�u@��@�@��@��<@��F@�w�@�S�@�7@��@�w2@�S&@�<6@��B@���@��o@�Q�@��.@���@�a@��@�;@��K@��p@�~(@�:�@�O@��w@�F@��@���@�H�@��+@���@��n@��{@�'�@���@�͟@��L@��@��@���@�y>@�i�@�Ta@�+k@��@��;@�hs@�2a@��@���@���@���@��u@�i�@��@��m@��@���@���@���@�X@���@���@�m�@��@���@�<6@���@��@�c @�K^@�8�@���@��X@��{@�hs@�=�@��@���@�<�@���@��w@��~@�Y�@�<6@�.I@�-w@��@��R@��r@�8�@��@���@��6@���@���@��"@�}�@�u�@�J�@���@��_@�?@��@��@>�@~� @~($@}��@|��@|Q�@|'R@{�]@{�@z��@z_@yhs@x�.@x'R@w��@w>�@v�@vz@v�@u�-@uN<@tѷ@t$@s"�@ri�@q�@q�~@q7L@p��@p��@pr�@p@o��@oe�@n��@n�b@nL0@n@n �@m�7@m4@m�@l�.@l �@k|�@k;d@k�@j�,@jl�@i�o@i�3@i��@iB�@h��@h��@h:�@g�
@g�$@g+@f��@f�@e��@e8�@d�K@dѷ@d��@d�Y@d'R@c��@c~�@cZ�@cK�@b�@bv�@b&�@aϫ@a�'@aDg@`�_@`�@_�:@_n/@_@O@_�@^�c@^�m@^_�@]ϫ@]e,@\j@\	�@[�@[��@Z�m@Z�@YO�@Y@Y�@X��@X��@XFt@W��@W�[@Wj�@W(@Vq�@U��@U��@UL�@T�@T��@TN�@T@S�@S��@S��@S��@S��@SF�@S@R�@R}V@R8�@Q�@Qc�@P��@P��@P�@Ph�@P7�@Oخ@O��@O��@N�h@N��@N}V@Ni�@NR�@Ne@Mk�@L�5@L��@Lg8@L�@K��@K!-@J�F@JTa@J�@I�X@H�@H`�@G��@F��@F��@F� @F��@Fv�@F6�@E�@EIR@EV@D�@D��@D��@C˒@C=@Bں@B��@BJ@A�@Ahs@A@@�$@@M@@1'@@7@?��@?@O@>ߤ@>�L@>W�@>{@=�@=�3@=[W@<�`@<��@<"h@;�Q@;C�@;.I@;S@:q�@9��@9��@9�M@9hs@9�@8��@8g8@8,=@7�@7dZ@7;d@6�c@6�b@6E�@6	@5�@5�@4�@4��@4`�@4$@3��@3�@3��@3RT@39�@2��@2��@2d�@2_@1��@1a�@1 \@0�$@0�@0q@0�@/�[@/S�@.��@.��@.i�@.$�@.J@-�j@-�@-^�@-&�@-�@-�@,��@,��@,r�@,2�@+��@+�4@+a@+1�@*�@*��@*��@*i�@*)�@*�@)��@)�"@)`B@)G�@)	l@(�D@(M@(G@'�@'�@'W?@'$t@'@&�@&_�@&:*@&@%�@%�-@%e,@%Q�@%#�@$֡@$�.@$j@$]d@$PH@$C-@$9X@$ �@$	�@#�@#\)@#6z@#�@"�@"�'@"�r@"?@"e@!�j@!�@!��@!�=@!��@!/@ ��@ �o@ -�@�A@��@��@>�@�@�H@�'@ff@#:@��@�@��@��@/@�?@�@~(@~(@m�@Xy@x@�w@�*@��@��@�4@n/@
=@��@^5@��@(�@��@�@�E@�@�@�;@�6@��@o�@iD@e�@+@ȴ@YK@+k@�j@�@��@��@hs@Y�@5�@�@�p@��@��@`�@ �@�@�[@Z�@�2@}V@J�@1�@ԕ@�@��@�n@c@a�@F@2a@�@�E@��@��@g8@N�@<�@-�@�@�A@�q@qv@a@.I@ȴ@��@ff@H�@1�@$�@�N@�@0�@�@;@��@��@��@�O@*�@�Q@� @� @�@=@&@o@
�H@
�X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�#A�"hA�"�A�$@A�%A�%�A�'A�(XA��A�خAİUA��A�.A�>BA�I�A��tA��A�=�A�J�A�l�AŮIA���A��`A��]A��A���Aź�Aş!AŊ	A�:�A��A��`A�7AÖ�A�_;A�Q�A�A��A��NA�MA��oA�k�A�U�A���A��A�H�A�XA�bNA�;0A��A��XA�Q�A��|A�w�A��A�7�A��A��BA�\A��A���A�m�A��9A�6�A���A�h>A�.IA���A�FA�
�A�$A�M�A��oA��-A���A� 4A�[�A�>wA�_A�rA�ffA���A�t�A�=�A�"4A�A�7�A�A�-A���A���A���A�b�A�A{�Ay�Av{�As�Ao	AiK�Ad�vAb4A]��AWe,AS1�AQu�APL�AP\�AO�!AN�AIS&AG��AF�)AB�RA?��A=��A7��A0O�A-��A-_pA-��A-k�A,U2A,�/A-��A.��A-�HA,D�A,(�A-h�A0ZA1Y�A2J#A2��A2��A1�A/($A-GA,\�A)��A(4A'�4A&�A&VmA%��A%�{A%��A%�nA%�YA%c�A$�TA#�nA#OA#.�A"��A"�A"|A#�KA$s�A$�A$��A$\)A#CA �A�A}�A�bAO�A{A��A��A�A\�A�QAB[A�)A�A� Aq�A�A��AGEA��A%FAخA�AsA�"A�A5�A5�A�A��A��An/AM�A��AW?A%A�A<6AU2A-AaAjA}�AخAkQAOAD�A�A/A
��A
jA

=A	�6A	}VA	(AYA��AZA'RA��A^5A!A��A[�A��AtTA&�Ad�A֡A\�A�A �A��A�A��A;A ��@��@���@��@��@�خ@���@�ԕ@�Ta@��.@��@��@���@�dZ@��@���@���@�;d@��@�&�@�>B@�r@�n�@�{�@�@��@�2�@�S�@궮@���@�x@��M@�p;@���@�j@歬@��@㋬@�֡@�J@���@�rG@�r�@ߊ�@���@��@ݱ[@�Q�@�{�@�4n@���@�P�@�S�@��@��W@�m�@�J�@�S�@�_�@�c�@�D�@��@��#@�9�@�Xy@��@��@Ҝx@�j@�� @�5�@��m@Э�@П�@�3�@υ�@κ�@��A@��@͈f@͇�@͎"@��@ΆY@�_�@̲�@�M�@�
�@˖S@�q@� �@���@���@Ɉf@�p�@��@ȏ\@�0U@� �@�c�@��B@�<�@���@�X@�!-@��@ĔF@�y>@�H�@ø�@�G�@�ȴ@¡b@�Q�@���@��@�� @���@���@�{�@��&@��S@�}�@�2a@���@���@�Q@�~�@���@�!-@���@�  @��P@�+�@��I@�-@��o@��F@�l�@��M@�H�@��@���@�qv@�!-@��D@�C�@���@�A @���@�%�@���@�Q�@��@�:�@�:*@�7@��@��d@�W?@��@�Ta@���@��@�@�#�@�^5@�?�@��h@�@O@�+@���@���@�9X@��[@�A�@�!-@��U@�z@�4@��H@�c�@�<6@���@��X@���@�4n@��Q@��4@�1�@�Ɇ@�y>@�/�@��@���@�:�@��L@�oi@��@��@��K@��n@�x�@�=@���@�ff@�#:@��@��@�=@�V@��@��r@�%�@��]@��@�+@��m@���@�_@�6@��+@���@��@��@���@���@�YK@���@��S@�Dg@���@�7�@��]@��@�K�@�&�@�;@��U@�1�@�u@��@�@��@��<@��F@�w�@�S�@�7@��@�w2@�S&@�<6@��B@���@��o@�Q�@��.@���@�a@��@�;@��K@��p@�~(@�:�@�O@��w@�F@��@���@�H�@��+@���@��n@��{@�'�@���@�͟@��L@��@��@���@�y>@�i�@�Ta@�+k@��@��;@�hs@�2a@��@���@���@���@��u@�i�@��@��m@��@���@���@���@�X@���@���@�m�@��@���@�<6@���@��@�c @�K^@�8�@���@��X@��{@�hs@�=�@��@���@�<�@���@��w@��~@�Y�@�<6@�.I@�-w@��@��R@��r@�8�@��@���@��6@���@���@��"@�}�@�u�@�J�@���@��_@�?@��@��@>�@~� @~($@}��@|��@|Q�@|'R@{�]@{�@z��@z_@yhs@x�.@x'R@w��@w>�@v�@vz@v�@u�-@uN<@tѷ@t$@s"�@ri�@q�@q�~@q7L@p��@p��@pr�@p@o��@oe�@n��@n�b@nL0@n@n �@m�7@m4@m�@l�.@l �@k|�@k;d@k�@j�,@jl�@i�o@i�3@i��@iB�@h��@h��@h:�@g�
@g�$@g+@f��@f�@e��@e8�@d�K@dѷ@d��@d�Y@d'R@c��@c~�@cZ�@cK�@b�@bv�@b&�@aϫ@a�'@aDg@`�_@`�@_�:@_n/@_@O@_�@^�c@^�m@^_�@]ϫ@]e,@\j@\	�@[�@[��@Z�m@Z�@YO�@Y@Y�@X��@X��@XFt@W��@W�[@Wj�@W(@Vq�@U��@U��@UL�@T�@T��@TN�@T@S�@S��@S��@S��@S��@SF�@S@R�@R}V@R8�@Q�@Qc�@P��@P��@P�@Ph�@P7�@Oخ@O��@O��@N�h@N��@N}V@Ni�@NR�@Ne@Mk�@L�5@L��@Lg8@L�@K��@K!-@J�F@JTa@J�@I�X@H�@H`�@G��@F��@F��@F� @F��@Fv�@F6�@E�@EIR@EV@D�@D��@D��@C˒@C=@Bں@B��@BJ@A�@Ahs@A@@�$@@M@@1'@@7@?��@?@O@>ߤ@>�L@>W�@>{@=�@=�3@=[W@<�`@<��@<"h@;�Q@;C�@;.I@;S@:q�@9��@9��@9�M@9hs@9�@8��@8g8@8,=@7�@7dZ@7;d@6�c@6�b@6E�@6	@5�@5�@4�@4��@4`�@4$@3��@3�@3��@3RT@39�@2��@2��@2d�@2_@1��@1a�@1 \@0�$@0�@0q@0�@/�[@/S�@.��@.��@.i�@.$�@.J@-�j@-�@-^�@-&�@-�@-�@,��@,��@,r�@,2�@+��@+�4@+a@+1�@*�@*��@*��@*i�@*)�@*�@)��@)�"@)`B@)G�@)	l@(�D@(M@(G@'�@'�@'W?@'$t@'@&�@&_�@&:*@&@%�@%�-@%e,@%Q�@%#�@$֡@$�.@$j@$]d@$PH@$C-@$9X@$ �@$	�@#�@#\)@#6z@#�@"�@"�'@"�r@"?@"e@!�j@!�@!��@!�=@!��@!/@ ��@ �o@ -�@�A@��@��@>�@�@�H@�'@ff@#:@��@�@��@��@/@�?@�@~(@~(@m�@Xy@x@�w@�*@��@��@�4@n/@
=@��@^5@��@(�@��@�@�E@�@�@�;@�6@��@o�@iD@e�@+@ȴ@YK@+k@�j@�@��@��@hs@Y�@5�@�@�p@��@��@`�@ �@�@�[@Z�@�2@}V@J�@1�@ԕ@�@��@�n@c@a�@F@2a@�@�E@��@��@g8@N�@<�@-�@�@�A@�q@qv@a@.I@ȴ@��@ff@H�@1�@$�@�N@�@0�@�@;@��@��@��@�O@*�@�Q@� @� @�@=@&@o@
�H@
�X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�fB�B��B��B��B�$B��B�B0�B��B	GzB	��B	ѝB
#:B
Z�B
jKB
oB
w�B
�#B
�mB
��B
�B
�bB
��B
�-B
��B
��B
�0B
�(B
��B
��B
āB
��B
˒B
�_B
�iB
�B
�B�BBkB%�B=�BDgB]IBnIB��B��B�[B�AB�%B�}B�7B��B��B��B��B� B�?B�	BοB�vB�6B��B�-B��B�BBB��B��B��B��B�-B�hB�TB�uBm)B_pBH�B3�B \B<B
�<B
��B
��B
�<B
�AB
�fB
\B
R�B
	�B	�eB	޸B	�B	�$B	�CB	w�B	ZQB	E�B	+B	 B�6B� B�B	'�B	2�B	2|B	 \B	#�B	5�B	U�B	a-B	[�B	# B��B�1B�LB	+B	(�B	1�B	B�B	^B	��B	�_B	��B	��B	�CB	��B
~B
1vB
@�B
B'B
<PB
-wB
KB
[B
  B	�;B	�kB	�B	��B	�B	�]B	�aB	�^B
�B
	lB
~B
	lB
�B
�B
�B
�B
{B
6�B
LB
Q�B
S�B
S&B
I�B
8lB
%`B
_B
}B
�B
HB
�B
�B
�B
�B
	�B
B
tB
�B
	�B
�B
JB
~B
�B
�B

#B
�B
�B
�B
!|B
�B
�B
 BB
�B
uB
�B
#�B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
<B
	�B
+B
YB
�B
9B
�B
B
�B
�B
'B
 OB	�qB	�DB	�B	��B	��B	�B	��B	��B	��B	�LB	��B	�nB	��B	�B	�B	�B	�oB	�`B	�RB	��B	��B	��B	�RB	�vB	��B	�pB	�CB	��B	ބB	�dB	خB	ٴB	��B	�B	��B	ݘB	ٴB	�YB	ּB	��B	��B	�CB	��B	ٚB	ٴB	�B	چB	�hB	�pB	�~B	�xB	�B	�xB	�]B	��B	یB	�#B	ٚB	�_B	רB	�9B	ևB	�sB	�B	��B	֡B	�MB	՛B	��B	��B	�
B	ּB	�gB	��B	��B	��B	ԯB	�NB	�B	�{B	�B	՛B	ٚB	�B	��B	��B	�WB	�	B	ٚB	��B	�?B	��B	�eB	�yB	��B	�sB	��B	ּB	յB	��B	��B	�SB	ևB	��B	�xB	�B	�hB	�hB	��B	��B	�\B	��B	��B	ߤB	ߤB	�!B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�NB	�|B	��B	�hB	�@B	�zB	�`B	�B	�B	�2B	�B	�B	�B	�B	�LB	��B	�8B	�2B	��B	�B	��B	�XB	�>B	�>B	�
B	�B	��B	��B	�XB	�*B	��B	�B	�eB	�6B	�B	�WB	��B	��B	�/B	� B	�B	��B	�B	�B	�OB	�B	�OB	�;B	�!B	�oB	�UB	�B	�B	��B	�'B	��B	�TB	��B	�B	��B	�B	�*B	�*B	��B	��B	�JB	�B	�0B	��B	��B	�xB	�B	�B	�B	��B	�6B	��B	��B	�qB	�wB	�B	�cB	�HB
 �B
 �B
 �B
 B
;B
�B
�B
�B
B
AB
�B
�B
GB
�B
{B
�B
�B
{B
�B
gB
�B
�B
tB
�B
�B
YB
EB
zB
_B
zB
�B
�B
�B
�B
�B
�B
	�B

XB

�B

�B

�B
^B
�B
B
�B
�B
VB
VB
�B
BB
\B
�B
�B
�B
bB
�B
�B
B
:B
:B
oB
oB
&B
�B
�B
,B
B
�B
B
B
2B
�B
B
�B
�B
�B
�B
�B
?B
?B
?B
YB
+B
�B
1B
B
eB
�B
�B
7B
	B
	B
�B
�B
B
)B
)B
CB
]B
]B
�B
�B
B
�B
�B
B
5B
OB
OB
�B
�B
B
B
�B
�B
!B
!B
!B
�B
�B
�B
 B
 �B
 �B
!bB
!�B
!|B
!bB
!HB
"NB
"�B
"�B
"�B
"�B
"NB
"�B
"�B
"�B
#B
# B
#TB
#nB
#nB
#TB
#TB
$B
$B
$�B
$�B
%`B
%�B
&B
&2B
&�B
'B
'B
'�B
(�B
)DB
)�B
*eB
*KB
+B
+�B
+�B
,"B
,�B
-]B
-�B
-�B
-�B
.�B
.�B
/iB
0UB
0�B
0�B
1'B
1�B
1�B
1�B
1�B
2-B
2aB
2�B
3�B
4TB
4�B
4�B
5?B
5tB
5�B
5�B
5�B
6+B
6`B
6�B
7fB
7�B
8�B
8�B
9$B
9	B
9$B
9�B
:B
:DB
:*B
:DB
:DB
:�B
;0B
;dB
;dB
;�B
;�B
;�B
<6B
<6B
<6B
<�B
=qB
>BB
>]B
>�B
?B
?B
?.B
?B
?.B
?}B
?�B
?�B
?�B
@OB
@�B
@�B
@�B
@�B
A;B
A�B
A�B
A�B
B'B
A�B
B'B
B�B
B�B
C-B
C{B
C�B
DgB
D�B
D�B
D�B
D�B
E9B
FB
E�B
E�B
E�B
E�B
F%B
FYB
F?B
FtB
FtB
F�B
G+B
GB
G�B
G�B
G�B
G�B
H1B
H1B
HfB
H�B
H�B
HfB
H�B
H�B
IB
I7B
I�B
I�B
I�B
J#B
J#B
JrB
JrB
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L0B
L�B
M6B
M6B
M6B
M�B
M�B
N<B
N�B
N�B
N�B
N�B
O\B
OvB
PB
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
SB
S&B
SuB
S�B
S�B
T,B
T,B
T,B
TaB
T�B
UMB
UMB
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W?B
WYB
X+B
XB
X_B
X�B
YeB
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[qB
[�B
[�B
[�B
[�B
\]B
\CB
\�B
]IB
]/B
]/B
]IB
]dB
]dB
]~B
]�B
]�B
^B
^B
^B
^B
^OB
^�B
^�B
^�B
_VB
_!B
_;B
_�B
`'B
`�B
`�B
`�B
a-B
a�B
a�B
b4B
bNB
b�B
b�B
b�B
b�B
cB
c B
c�B
c�B
c�B
d�B
dtB
d�B
d�B
d�B
eB
e,B
ezB
ezB
ezB
e�B
e�B
e�B
fB
e�B
fB
fB
ffB
fLB
f�B
f�B
ffB
f�B
f�B
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
hXB
hsB
hsB
h�B
hsB
h�B
h�B
h�B
i*B
iyB
iyB
i�B
i�B
i�B
jB
jeB
jB
j�B
j�B
j�B
j�B
j�B
kQB
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
l�B
mCB
m�B
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
n�B
oB
oiB
o�B
oiB
o�B
o�B
o�B
pB
pB
o�B
p�B
qB
qAB
p�B
q'B
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r-B
r|B
r�B
shB
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
vB
v�B
wB
w2B
w2B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zDB
zxB
z�B
z�B
{B
{0B
{0B
{0B
{dB
{�B
|B
|B
|B
|B
|B
|B
|6B
}B
}VB
}VB
}VB
}�B
~(B
~(B
~BB
~wB
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�fB�B��B��B��B�$B��B�B0�B��B	GzB	��B	ѝB
#:B
Z�B
jKB
oB
w�B
�#B
�mB
��B
�B
�bB
��B
�-B
��B
��B
�0B
�(B
��B
��B
āB
��B
˒B
�_B
�iB
�B
�B�BBkB%�B=�BDgB]IBnIB��B��B�[B�AB�%B�}B�7B��B��B��B��B� B�?B�	BοB�vB�6B��B�-B��B�BBB��B��B��B��B�-B�hB�TB�uBm)B_pBH�B3�B \B<B
�<B
��B
��B
�<B
�AB
�fB
\B
R�B
	�B	�eB	޸B	�B	�$B	�CB	w�B	ZQB	E�B	+B	 B�6B� B�B	'�B	2�B	2|B	 \B	#�B	5�B	U�B	a-B	[�B	# B��B�1B�LB	+B	(�B	1�B	B�B	^B	��B	�_B	��B	��B	�CB	��B
~B
1vB
@�B
B'B
<PB
-wB
KB
[B
  B	�;B	�kB	�B	��B	�B	�]B	�aB	�^B
�B
	lB
~B
	lB
�B
�B
�B
�B
{B
6�B
LB
Q�B
S�B
S&B
I�B
8lB
%`B
_B
}B
�B
HB
�B
�B
�B
�B
	�B
B
tB
�B
	�B
�B
JB
~B
�B
�B

#B
�B
�B
�B
!|B
�B
�B
 BB
�B
uB
�B
#�B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
<B
	�B
+B
YB
�B
9B
�B
B
�B
�B
'B
 OB	�qB	�DB	�B	��B	��B	�B	��B	��B	��B	�LB	��B	�nB	��B	�B	�B	�B	�oB	�`B	�RB	��B	��B	��B	�RB	�vB	��B	�pB	�CB	��B	ބB	�dB	خB	ٴB	��B	�B	��B	ݘB	ٴB	�YB	ּB	��B	��B	�CB	��B	ٚB	ٴB	�B	چB	�hB	�pB	�~B	�xB	�B	�xB	�]B	��B	یB	�#B	ٚB	�_B	רB	�9B	ևB	�sB	�B	��B	֡B	�MB	՛B	��B	��B	�
B	ּB	�gB	��B	��B	��B	ԯB	�NB	�B	�{B	�B	՛B	ٚB	�B	��B	��B	�WB	�	B	ٚB	��B	�?B	��B	�eB	�yB	��B	�sB	��B	ּB	յB	��B	��B	�SB	ևB	��B	�xB	�B	�hB	�hB	��B	��B	�\B	��B	��B	ߤB	ߤB	�!B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�NB	�|B	��B	�hB	�@B	�zB	�`B	�B	�B	�2B	�B	�B	�B	�B	�LB	��B	�8B	�2B	��B	�B	��B	�XB	�>B	�>B	�
B	�B	��B	��B	�XB	�*B	��B	�B	�eB	�6B	�B	�WB	��B	��B	�/B	� B	�B	��B	�B	�B	�OB	�B	�OB	�;B	�!B	�oB	�UB	�B	�B	��B	�'B	��B	�TB	��B	�B	��B	�B	�*B	�*B	��B	��B	�JB	�B	�0B	��B	��B	�xB	�B	�B	�B	��B	�6B	��B	��B	�qB	�wB	�B	�cB	�HB
 �B
 �B
 �B
 B
;B
�B
�B
�B
B
AB
�B
�B
GB
�B
{B
�B
�B
{B
�B
gB
�B
�B
tB
�B
�B
YB
EB
zB
_B
zB
�B
�B
�B
�B
�B
�B
	�B

XB

�B

�B

�B
^B
�B
B
�B
�B
VB
VB
�B
BB
\B
�B
�B
�B
bB
�B
�B
B
:B
:B
oB
oB
&B
�B
�B
,B
B
�B
B
B
2B
�B
B
�B
�B
�B
�B
�B
?B
?B
?B
YB
+B
�B
1B
B
eB
�B
�B
7B
	B
	B
�B
�B
B
)B
)B
CB
]B
]B
�B
�B
B
�B
�B
B
5B
OB
OB
�B
�B
B
B
�B
�B
!B
!B
!B
�B
�B
�B
 B
 �B
 �B
!bB
!�B
!|B
!bB
!HB
"NB
"�B
"�B
"�B
"�B
"NB
"�B
"�B
"�B
#B
# B
#TB
#nB
#nB
#TB
#TB
$B
$B
$�B
$�B
%`B
%�B
&B
&2B
&�B
'B
'B
'�B
(�B
)DB
)�B
*eB
*KB
+B
+�B
+�B
,"B
,�B
-]B
-�B
-�B
-�B
.�B
.�B
/iB
0UB
0�B
0�B
1'B
1�B
1�B
1�B
1�B
2-B
2aB
2�B
3�B
4TB
4�B
4�B
5?B
5tB
5�B
5�B
5�B
6+B
6`B
6�B
7fB
7�B
8�B
8�B
9$B
9	B
9$B
9�B
:B
:DB
:*B
:DB
:DB
:�B
;0B
;dB
;dB
;�B
;�B
;�B
<6B
<6B
<6B
<�B
=qB
>BB
>]B
>�B
?B
?B
?.B
?B
?.B
?}B
?�B
?�B
?�B
@OB
@�B
@�B
@�B
@�B
A;B
A�B
A�B
A�B
B'B
A�B
B'B
B�B
B�B
C-B
C{B
C�B
DgB
D�B
D�B
D�B
D�B
E9B
FB
E�B
E�B
E�B
E�B
F%B
FYB
F?B
FtB
FtB
F�B
G+B
GB
G�B
G�B
G�B
G�B
H1B
H1B
HfB
H�B
H�B
HfB
H�B
H�B
IB
I7B
I�B
I�B
I�B
J#B
J#B
JrB
JrB
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L0B
L�B
M6B
M6B
M6B
M�B
M�B
N<B
N�B
N�B
N�B
N�B
O\B
OvB
PB
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
SB
S&B
SuB
S�B
S�B
T,B
T,B
T,B
TaB
T�B
UMB
UMB
U�B
U�B
U�B
U�B
VSB
V�B
W
B
W?B
WYB
X+B
XB
X_B
X�B
YeB
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
[qB
[�B
[�B
[�B
[�B
\]B
\CB
\�B
]IB
]/B
]/B
]IB
]dB
]dB
]~B
]�B
]�B
^B
^B
^B
^B
^OB
^�B
^�B
^�B
_VB
_!B
_;B
_�B
`'B
`�B
`�B
`�B
a-B
a�B
a�B
b4B
bNB
b�B
b�B
b�B
b�B
cB
c B
c�B
c�B
c�B
d�B
dtB
d�B
d�B
d�B
eB
e,B
ezB
ezB
ezB
e�B
e�B
e�B
fB
e�B
fB
fB
ffB
fLB
f�B
f�B
ffB
f�B
f�B
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
hXB
hsB
hsB
h�B
hsB
h�B
h�B
h�B
i*B
iyB
iyB
i�B
i�B
i�B
jB
jeB
jB
j�B
j�B
j�B
j�B
j�B
kQB
k�B
k�B
k�B
l"B
lWB
lWB
l�B
l�B
l�B
l�B
mCB
m�B
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
n�B
oB
oiB
o�B
oiB
o�B
o�B
o�B
pB
pB
o�B
p�B
qB
qAB
p�B
q'B
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r-B
r|B
r�B
shB
s�B
s�B
s�B
tB
t9B
tTB
tnB
t�B
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
vB
v�B
wB
w2B
w2B
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
zB
zDB
zDB
zxB
z�B
z�B
{B
{0B
{0B
{0B
{dB
{�B
|B
|B
|B
|B
|B
|B
|6B
}B
}VB
}VB
}VB
}�B
~(B
~(B
~BB
~wB
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105004  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175734  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175734  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175734                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025742  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025742  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                