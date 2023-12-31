CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:56Z creation;2022-06-04T17:52:56Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175256  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               /A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��C�-"1   @���ۗS@0��+�cU`A�7L1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bԙ�B�  B�33B���B�  B�  B�  B�  B�  B���B���C   C  C  C� C�C	�fC  C  C  C  C  C  C  C  C  C��C   C!��C#�fC%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<33C=�fC@  CB  CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Co�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @
�H@w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BW�GB_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqB�WB׽qB��Bߊ>B�qB�qB�qB�qB�qB�WB��>B��qC޸C޸C^�C�RC	�C޸C޸C޸C޸C޸C޸C޸C޸C޸CxRC޸C!��C#�C%�C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C<�C=�C?޸CA޸CC޸CE�CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck�RCm�RCo�Cq�Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH�DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�?
D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�o
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aͧ�AͫkA͚�A͒�A͢4A͑�A͊	A͌�A͇_A͓@AͭCAͷLAͻ�Aͼ�Aͺ�A͌JAͅ�Aͅ�A͌�AͦLA͎�A͈�Ä́�A͇+A�~�A̓�A�}�A�v�A�~(A͂uA͉lA͂�A�k�A�]/A�B�A�&LA�kA�IA��A�OA��A��yA���A��
A���A̳3ḀzǍ�A�uZA��A���Aˢ4A���Aɷ�A� �A�'RA�0�A��ZA�S�A�u�A·�A�PHA�A�WsA�t�A�{�A��1A��A�B�A�ݘA��
A�'�A�4A�R�A��vA���A� iA��wA�+A���A���A�q�A�	�A���A��A���A���A��mA�͟A��bA�+A�ܒA�a�A��}A�e�A��A���A�zA|�rAt��Aq�zAm��Ah�Af��Af�.Ae��Ab�qA`��A]n/AZJ�AVC�AS��AP�RANd�AJR�AG��AD��AA�A?��A>��A=��A;��A9�:A7�xA72aA6��A5�~A5oA4�)A3�A0خA0�A1%�A/xlA-8�A,3�A*��A)�=A(�\A'�A(	�A(2�A'�1A%�XA$hsA"�Ab�A�6A�[A�A�A�jA�=A1�A�A(A_A9XA��A��AQA�.A�	A�A�$A>BA�A�A&A=qA��Aq�AĜA��A�_Av`A�A��Ab�A�)A�rA�UA�A/�Am]A1'A]�A�AZ�A�A�"A�A=qA�gA�}A��Al�A
�A
�A
�YA
@�A	�>A	��A	:�A͟A��A��A(�A	A��A$�AkQA��AC-A�}A6A��A1�A��A�A �QA zx@��@�[W@�/�@��8@�Ɇ@��<@��"@�-w@��E@�Xy@�ƨ@��7@���@��5@��@��H@�o@�X�@���@��@�@�p;@��+@�F@��@���@�$t@@�i�@��o@���@��c@�@�ԕ@�'@�P@�e�@��@�C-@�M@�(�@�	l@��@�6@�A@�Z@�E�@�/�@��T@��f@愶@�$@勬@��c@䒣@�~@��a@��@�i�@��@�&�@��@�w�@߭C@�N�@ݽ�@ݦ�@�}�@�f�@��@ܶ�@ܰ!@ܝI@��6@�F@��?@�6�@��@�q�@�c@���@�҉@�Z�@�\)@��@��f@ԥz@�y>@��@��@�6�@�m�@ԂA@�b@ҦL@�3�@��@ѓ�@ѡ�@Б @�e�@�G@�Y�@��2@ΦL@�M@��@�F�@̹�@̍�@�1�@�C�@��6@�A�@��@�6�@ȋD@ǡ�@��@�R�@���@ż@���@�-@�4�@�(@�1'@Ŏ�@���@ă@�^�@��k@�o�@���@�~(@�L0@�~(@��I@�@��@�*�@�7�@�U2@�#:@��@���@��~@�}�@�H�@���@�?}@�Y�@�˒@�Ɇ@���@�qv@�33@��@��L@�?@���@��@�+@�y�@�A�@��5@��F@�Z�@���@��@���@��;@��}@�Vm@���@�e@�ƨ@��@�A @��@��@�&@�}�@��P@���@�(�@��s@�A�@���@�@�a|@���@�rG@�%F@��@���@��@�v�@�m�@�-�@��
@�t�@�@���@�Z�@���@��@���@�]d@�c�@�Xy@�?@��@��
@�F�@�"�@��@���@�z@�.�@�u@���@��~@���@���@�x�@�Z�@�.I@��K@���@�+k@���@�ϫ@��@@��n@���@��@�˒@�O@��c@��@�kQ@�1@���@�w2@�Vm@��c@�u�@�9X@���@��X@�v`@��@���@�v�@�K^@���@���@��q@�N<@��P@�{�@�;�@�u@���@�?}@��@�Xy@��]@�U�@�V@���@�tT@�e@��{@�Y�@�7L@��@��@��}@�<�@���@���@�k�@�E9@��|@� �@���@�qv@�Vm@� \@���@�Ta@�@��C@��@��b@�9X@�,=@��@�J@���@��g@��F@���@�RT@�&@�ѷ@�{�@�i�@�oi@�E�@���@��d@��M@�@��`@���@�u�@�]d@�?@�~@�u@��N@���@�C@��@���@��Y@�B[@��@���@�e�@�/@��@���@�{�@�PH@�/�@�u@���@���@�e�@�33@��/@��p@�Ĝ@���@�?@���@�c@�B�@� i@��'@�d�@�:*@�	@��@�o @�8@�V@�%@��P@��@��@��@�h�@�W�@�@�@�1�@�-@�	@��@���@��t@���@�G�@�Y@��f@���@�J�@���@���@��t@�rG@�P�@�(�@��@�ߤ@���@�xl@�I�@�:*@��@���@�˒@�c@��@���@��O@��e@�[�@�7@'�@~�X@~�\@~d�@~GE@~-@}�X@};@|I�@{�4@{�@z�"@zJ�@y�H@y��@y4@x�5@xh�@xb@w�@wK�@v�H@v�L@vW�@v&�@u�d@t�[@t�D@tPH@tx@sqv@s&@r��@r)�@q��@q��@p��@pw�@p%�@o�@np;@m�.@m�'@mf�@m�@l�@lC-@k��@kخ@k�P@j�@j^5@i��@i�@i�@iB�@h��@hg8@h/�@h"h@hx@g�g@g�4@fa|@eT�@d��@d<�@d$@d@c�@@c��@cqv@c�@b�@b�s@b��@ba|@a�N@`�v@`"h@_�@@_�@^� @^8�@^B[@^#:@]�@]�@\�_@\j@\[�@\S�@\N�@\�@[�K@[�$@[e�@[J#@Z��@Ze@Y��@Y��@X��@X��@X?�@W�0@Wo�@V�m@V+k@U�@U7L@T��@T��@Th�@T4n@T7@T�@S�Q@S]�@R�1@RH�@Q��@QDg@P�@PA�@P(�@P�@O��@O{J@Os@Oqv@On/@Oa@O@O@O�@N�@N��@NTa@M�z@MY�@M%@L�K@L�`@L��@L��@LQ�@K��@K�$@KW?@K�@J͟@J~�@J($@I�@I4@I&�@I!�@H��@H>B@G��@G�K@GiD@G�@F��@F�X@F�!@F��@F��@FOv@E�h@EA @D�)@D|�@DQ�@C�&@C��@B�m@A�T@A��@A�h@Au�@A=�@A5�@@��@@Ɇ@@��@@bN@@A�@@�@?�m@?�V@?�	@?Mj@>�s@>��@>Q@=��@=��@=zx@=m]@=�@<�@<�@;��@;P�@; i@:��@:Ov@:6�@9�@9�'@9hs@9Vm@9A @8��@8��@8�o@8M@8�@7خ@7˒@7�K@7t�@6��@6҉@6��@6@�@5��@5o @5-w@4��@4�9@4oi@4�@3l�@2��@2�@2�X@2��@2p;@1�Z@1�)@1�@1�S@0u�@0"h@/�F@/_p@/�@.�@.��@.�1@.��@.c @-�D@-�t@-��@-x�@-O�@-B�@-�@,��@,*�@+~�@+W?@+>�@+9�@+(@*�@*��@*.�@*e@)�d@)�@(��@(��@(��@(w�@(`�@(6@(�@'�V@'J#@'�@&��@&�@&�x@&z@&H�@%��@%��@%�~@%Dg@$�@$�p@$��@$%�@$�@#�+@#��@#�g@#�:@#Y@"�@"��@"�+@"8�@!�T@!��@!!�@ ��@ `�@ V�@ V�@ x@qv@Mj@6z@�@�@��@d�@($@ϫ@s�@*0@��@�|@��@��@tT@M@�0@l�@=@!-@�6@;�@ϫ@��@�@�n@�'@��@�S@f�@!�@�@��@c�@-�@�W@��@��@J#@1�@�@��@҉@�6@^5@��@F@+@��@��@��@�@:�@�@�*@��@��@E9@C@
=@�M@�R@c @:*@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aͧ�AͫkA͚�A͒�A͢4A͑�A͊	A͌�A͇_A͓@AͭCAͷLAͻ�Aͼ�Aͺ�A͌JAͅ�Aͅ�A͌�AͦLA͎�A͈�Ä́�A͇+A�~�A̓�A�}�A�v�A�~(A͂uA͉lA͂�A�k�A�]/A�B�A�&LA�kA�IA��A�OA��A��yA���A��
A���A̳3ḀzǍ�A�uZA��A���Aˢ4A���Aɷ�A� �A�'RA�0�A��ZA�S�A�u�A·�A�PHA�A�WsA�t�A�{�A��1A��A�B�A�ݘA��
A�'�A�4A�R�A��vA���A� iA��wA�+A���A���A�q�A�	�A���A��A���A���A��mA�͟A��bA�+A�ܒA�a�A��}A�e�A��A���A�zA|�rAt��Aq�zAm��Ah�Af��Af�.Ae��Ab�qA`��A]n/AZJ�AVC�AS��AP�RANd�AJR�AG��AD��AA�A?��A>��A=��A;��A9�:A7�xA72aA6��A5�~A5oA4�)A3�A0خA0�A1%�A/xlA-8�A,3�A*��A)�=A(�\A'�A(	�A(2�A'�1A%�XA$hsA"�Ab�A�6A�[A�A�A�jA�=A1�A�A(A_A9XA��A��AQA�.A�	A�A�$A>BA�A�A&A=qA��Aq�AĜA��A�_Av`A�A��Ab�A�)A�rA�UA�A/�Am]A1'A]�A�AZ�A�A�"A�A=qA�gA�}A��Al�A
�A
�A
�YA
@�A	�>A	��A	:�A͟A��A��A(�A	A��A$�AkQA��AC-A�}A6A��A1�A��A�A �QA zx@��@�[W@�/�@��8@�Ɇ@��<@��"@�-w@��E@�Xy@�ƨ@��7@���@��5@��@��H@�o@�X�@���@��@�@�p;@��+@�F@��@���@�$t@@�i�@��o@���@��c@�@�ԕ@�'@�P@�e�@��@�C-@�M@�(�@�	l@��@�6@�A@�Z@�E�@�/�@��T@��f@愶@�$@勬@��c@䒣@�~@��a@��@�i�@��@�&�@��@�w�@߭C@�N�@ݽ�@ݦ�@�}�@�f�@��@ܶ�@ܰ!@ܝI@��6@�F@��?@�6�@��@�q�@�c@���@�҉@�Z�@�\)@��@��f@ԥz@�y>@��@��@�6�@�m�@ԂA@�b@ҦL@�3�@��@ѓ�@ѡ�@Б @�e�@�G@�Y�@��2@ΦL@�M@��@�F�@̹�@̍�@�1�@�C�@��6@�A�@��@�6�@ȋD@ǡ�@��@�R�@���@ż@���@�-@�4�@�(@�1'@Ŏ�@���@ă@�^�@��k@�o�@���@�~(@�L0@�~(@��I@�@��@�*�@�7�@�U2@�#:@��@���@��~@�}�@�H�@���@�?}@�Y�@�˒@�Ɇ@���@�qv@�33@��@��L@�?@���@��@�+@�y�@�A�@��5@��F@�Z�@���@��@���@��;@��}@�Vm@���@�e@�ƨ@��@�A @��@��@�&@�}�@��P@���@�(�@��s@�A�@���@�@�a|@���@�rG@�%F@��@���@��@�v�@�m�@�-�@��
@�t�@�@���@�Z�@���@��@���@�]d@�c�@�Xy@�?@��@��
@�F�@�"�@��@���@�z@�.�@�u@���@��~@���@���@�x�@�Z�@�.I@��K@���@�+k@���@�ϫ@��@@��n@���@��@�˒@�O@��c@��@�kQ@�1@���@�w2@�Vm@��c@�u�@�9X@���@��X@�v`@��@���@�v�@�K^@���@���@��q@�N<@��P@�{�@�;�@�u@���@�?}@��@�Xy@��]@�U�@�V@���@�tT@�e@��{@�Y�@�7L@��@��@��}@�<�@���@���@�k�@�E9@��|@� �@���@�qv@�Vm@� \@���@�Ta@�@��C@��@��b@�9X@�,=@��@�J@���@��g@��F@���@�RT@�&@�ѷ@�{�@�i�@�oi@�E�@���@��d@��M@�@��`@���@�u�@�]d@�?@�~@�u@��N@���@�C@��@���@��Y@�B[@��@���@�e�@�/@��@���@�{�@�PH@�/�@�u@���@���@�e�@�33@��/@��p@�Ĝ@���@�?@���@�c@�B�@� i@��'@�d�@�:*@�	@��@�o @�8@�V@�%@��P@��@��@��@�h�@�W�@�@�@�1�@�-@�	@��@���@��t@���@�G�@�Y@��f@���@�J�@���@���@��t@�rG@�P�@�(�@��@�ߤ@���@�xl@�I�@�:*@��@���@�˒@�c@��@���@��O@��e@�[�@�7@'�@~�X@~�\@~d�@~GE@~-@}�X@};@|I�@{�4@{�@z�"@zJ�@y�H@y��@y4@x�5@xh�@xb@w�@wK�@v�H@v�L@vW�@v&�@u�d@t�[@t�D@tPH@tx@sqv@s&@r��@r)�@q��@q��@p��@pw�@p%�@o�@np;@m�.@m�'@mf�@m�@l�@lC-@k��@kخ@k�P@j�@j^5@i��@i�@i�@iB�@h��@hg8@h/�@h"h@hx@g�g@g�4@fa|@eT�@d��@d<�@d$@d@c�@@c��@cqv@c�@b�@b�s@b��@ba|@a�N@`�v@`"h@_�@@_�@^� @^8�@^B[@^#:@]�@]�@\�_@\j@\[�@\S�@\N�@\�@[�K@[�$@[e�@[J#@Z��@Ze@Y��@Y��@X��@X��@X?�@W�0@Wo�@V�m@V+k@U�@U7L@T��@T��@Th�@T4n@T7@T�@S�Q@S]�@R�1@RH�@Q��@QDg@P�@PA�@P(�@P�@O��@O{J@Os@Oqv@On/@Oa@O@O@O�@N�@N��@NTa@M�z@MY�@M%@L�K@L�`@L��@L��@LQ�@K��@K�$@KW?@K�@J͟@J~�@J($@I�@I4@I&�@I!�@H��@H>B@G��@G�K@GiD@G�@F��@F�X@F�!@F��@F��@FOv@E�h@EA @D�)@D|�@DQ�@C�&@C��@B�m@A�T@A��@A�h@Au�@A=�@A5�@@��@@Ɇ@@��@@bN@@A�@@�@?�m@?�V@?�	@?Mj@>�s@>��@>Q@=��@=��@=zx@=m]@=�@<�@<�@;��@;P�@; i@:��@:Ov@:6�@9�@9�'@9hs@9Vm@9A @8��@8��@8�o@8M@8�@7خ@7˒@7�K@7t�@6��@6҉@6��@6@�@5��@5o @5-w@4��@4�9@4oi@4�@3l�@2��@2�@2�X@2��@2p;@1�Z@1�)@1�@1�S@0u�@0"h@/�F@/_p@/�@.�@.��@.�1@.��@.c @-�D@-�t@-��@-x�@-O�@-B�@-�@,��@,*�@+~�@+W?@+>�@+9�@+(@*�@*��@*.�@*e@)�d@)�@(��@(��@(��@(w�@(`�@(6@(�@'�V@'J#@'�@&��@&�@&�x@&z@&H�@%��@%��@%�~@%Dg@$�@$�p@$��@$%�@$�@#�+@#��@#�g@#�:@#Y@"�@"��@"�+@"8�@!�T@!��@!!�@ ��@ `�@ V�@ V�@ x@qv@Mj@6z@�@�@��@d�@($@ϫ@s�@*0@��@�|@��@��@tT@M@�0@l�@=@!-@�6@;�@ϫ@��@�@�n@�'@��@�S@f�@!�@�@��@c�@-�@�W@��@��@J#@1�@�@��@҉@�6@^5@��@F@+@��@��@��@�@:�@�@�*@��@��@E9@C@
=@�M@�R@c @:*@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�zB��B��B��B��B�B�aB��B�zB��B�B��B�7B��B��B��B�[B��B��B��B�UB��B��B��B�iBcB�B��B�;B��B}�B{�ByrBv�Bu?Bu�Bu�ButBs�Bo�Bm�BmwBk�Bi�BhsBf�Bf�Bh>Bh>Bh
Bp!BrBw�B�;B�B	jB	?cB	dtB	�FB	��B
W�B
�OB
��B
�(B
��B
�BBKB�B�B�B
�B�BB
�B �B
��B
�B
�$B
�B
�B
یB
߾B
��B
��B
��B
q�B
a�B
[�B
VmB
E�B
�B	�kB	�4B	�)B	�B	�3B	�&B	��B	}B	kB	`'B	e�B	h�B	fB	[�B	U�B	[�B	L�B	E�B	>�B	3B	!�B	B	�B	�B	�B	'B	[B	:B	�B	IB	!bB	.�B	B�B	F%B	E�B	8�B	�B	$B	H�B	IlB	9�B	/B	.�B	%�B	 'B	!�B	3�B	@�B	B�B	A;B	8�B	/�B	&�B	)�B	*KB	:�B	P�B	\B	`B	b�B	h�B	pB	tnB	|6B	|B	|B	{�B	|B	z�B	~wB	�B	�B	�}B	��B	��B	�B	��B	��B	��B	��B	��B	�zB	��B	�FB	�%B	�!B	��B	��B	�B	� B	�nB	��B	�!B	�IB	�]B	�aB	��B	��B	��B	��B	�UB	� B	�cB	�}B	�B	��B	��B	�)B	�OB	�3B	��B	��B	�LB	��B	�B	��B	�DB	��B	��B	��B	��B	�jB	��B	�JB	��B	�lB	�lB	�8B	��B	�XB	�DB	��B	��B	��B	�"B	��B	�B	��B	��B	��B	�cB	�oB	��B	ðB	�GB	�SB	�mB	ňB	ŢB	��B	�%B	�tB	ƨB	��B	��B	��B	��B	��B	ƎB	�tB	ƎB	ƎB	ƨB	ƎB	ƎB	�+B	�%B	�mB	ƨB	ǮB	�1B	ȚB	�RB	�7B	��B	ȀB	��B	ʌB	�dB	�"B	ΥB	�<B	ΥB	�B	��B	��B	�TB	��B	�FB	յB	�SB	�sB	ڠB	ۦB	�)B	�CB	��B	��B	یB	�xB	�OB	�\B	�B	�hB	��B	�BB	�B	�qB	�dB	ݲB	�IB	��B	�OB	�OB	�5B	ޞB	�B	��B	�tB	��B	��B	�B	�B	��B	�8B	�B	�>B	�B	�
B	�B	�*B	��B	�XB	��B	�@B	�LB	�B	�B	�yB	�"B	�B	�@B	�dB	�=B	�OB	�	B	�	B	�yB	��B	��B	�CB	��B	�0B	�eB	�$B	��B	�B	�nB	�ZB	ޞB	�B	�OB	�B	�VB	�B	�B	�&B	��B	�B	�WB	�B	�B	�CB	�wB	�B	�cB	��B	��B	�mB	�B	�0B	�*B	�B	��B	�fB	�2B	�`B	�B	��B	�nB	�@B	�B	�mB	�B	��B	�_B	�B	��B	�AB	�vB	�B	��B	��B	��B	�aB	�GB	�|B	�B	��B	�B	��B	�B	�^B	�^B	��B	��B	�DB	��B	�DB	��B	�XB	�^B	�*B	�B	�^B	�dB	��B	��B	�PB	�"B	��B	�BB	��B	�VB	��B	��B	��B	��B	��B	��B	��B	�cB	�cB	�.B	�HB	�HB	��B	��B	��B
 B
  B
 B
 4B
 iB
 �B
 B
;B
 �B
;B
 B
oB
�B
[B
�B
�B
�B
gB
gB
SB
B
%B
%B
YB
tB
EB
�B
�B
KB
�B
	B
	�B
	�B
	RB
�B
�B
	�B
^B
�B
0B
jB
�B
"B
�B
B
\B
vB
(B
�B
�B
vB
(B
BB
�B
�B
B
HB
}B
bB
�B
�B
�B
�B
�B
�B
uB
�B
�B
uB
�B
�B
B
@B
[B
�B
&B
uB
�B
�B
�B
�B
2B
mB
9B
�B
B
B
�B
B
�B
�B
�B
B
yB
�B
�B
�B
B
QB
7B
B
QB
�B
�B
�B
B
�B
xB
�B
B
IB
OB
�B
�B
�B
�B
�B
�B
 B
 vB
 vB
 �B
 �B
!-B
 �B
 �B
 �B
!|B
"�B
"�B
"�B
# B
#TB
#�B
$B
$@B
$�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
'�B
($B
(>B
(sB
(XB
(sB
(�B
(�B
(�B
(XB
(XB
)DB
)*B
)yB
)�B
)�B
)�B
)�B
*eB
+B
+6B
+�B
,B
+�B
+�B
+�B
,"B
,"B
,qB
,�B
,�B
-]B
-)B
-CB
-�B
-�B
.�B
/B
/�B
/�B
0B
0B
0B
/�B
0�B
1vB
1�B
1�B
1�B
1vB
2B
2|B
2GB
2�B
2�B
2�B
2�B
3MB
3�B
4B
4B
4�B
4�B
4�B
5?B
5?B
5�B
6+B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
9$B
9�B
9�B
:*B
9�B
9�B
9XB
9$B
8�B
8�B
8�B
:DB
9�B
9XB
9�B
:xB
;�B
;JB
:�B
:�B
;B
<�B
=VB
=�B
=<B
;�B
;�B
;0B
;JB
<PB
<6B
<�B
="B
=B
<�B
<�B
<�B
<�B
=B
=<B
=VB
<�B
=�B
>(B
>�B
>�B
?B
?cB
?�B
@B
@OB
@OB
@OB
@4B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AB
A;B
B'B
B[B
B�B
C-B
CB
C�B
D3B
DMB
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
F�B
F�B
GEB
G�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J=B
JXB
J�B
J�B
K�B
K�B
LB
L0B
LB
K�B
L0B
L~B
MB
L�B
MB
MPB
MPB
M�B
M�B
N<B
N�B
N�B
NpB
N�B
OvB
O�B
O�B
PB
PbB
PbB
PbB
P}B
P�B
P.B
P�B
QhB
Q�B
RB
R B
R:B
R�B
R�B
S@B
S�B
S�B
S�B
TB
TFB
TFB
T{B
T{B
T�B
T�B
T�B
U2B
U2B
U�B
UgB
U�B
U�B
U�B
V9B
VmB
V�B
V�B
V�B
W
B
W$B
W�B
X+B
XEB
X�B
X�B
Y1B
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZkB
ZkB
ZkB
Z�B
Z�B
[	B
[	B
Z�B
[=B
[�B
[�B
[�B
[�B
\)B
\CB
\)B
\xB
\]B
[�B
[qB
[=B
[=B
[WB
\CB
\�B
]~B
]�B
]�B
]�B
]�B
^B
^B
^OB
^B
^OB
^jB
^�B
^�B
^jB
^�B
^�B
_!B
_B
_!B
_;B
_;B
_VB
_pB
`'B
`�B
`�B
`�B
`�B
aB
`�B
a|B
b4B
bB
b�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
d@B
d�B
d�B
eB
e,B
eFB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
gB
f�B
gRB
h
B
h
B
h$B
h$B
h$B
h
B
hXB
h�B
h�B
h�B
h�B
iB
i*B
i�B
jeB
j�B
j�B
jB
kB
k�B
k�B
kkB
k�B
kkB
k�B
k�B
lB
l=B
l�B
l�B
mB
mB
m)B
m�B
m�B
n/B
ncB
n�B
n�B
n�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r|B
r|B
r�B
r�B
s�B
s�B
tB
tB
tB
t9B
tTB
t�B
uB
u?B
u?B
uZB
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B�zB��B��B��B��B�B�aB��B�zB��B�B��B�7B��B��B��B�[B��B��B��B�UB��B��B��B�iBcB�B��B�;B��B}�B{�ByrBv�Bu?Bu�Bu�ButBs�Bo�Bm�BmwBk�Bi�BhsBf�Bf�Bh>Bh>Bh
Bp!BrBw�B�;B�B	jB	?cB	dtB	�FB	��B
W�B
�OB
��B
�(B
��B
�BBKB�B�B�B
�B�BB
�B �B
��B
�B
�$B
�B
�B
یB
߾B
��B
��B
��B
q�B
a�B
[�B
VmB
E�B
�B	�kB	�4B	�)B	�B	�3B	�&B	��B	}B	kB	`'B	e�B	h�B	fB	[�B	U�B	[�B	L�B	E�B	>�B	3B	!�B	B	�B	�B	�B	'B	[B	:B	�B	IB	!bB	.�B	B�B	F%B	E�B	8�B	�B	$B	H�B	IlB	9�B	/B	.�B	%�B	 'B	!�B	3�B	@�B	B�B	A;B	8�B	/�B	&�B	)�B	*KB	:�B	P�B	\B	`B	b�B	h�B	pB	tnB	|6B	|B	|B	{�B	|B	z�B	~wB	�B	�B	�}B	��B	��B	�B	��B	��B	��B	��B	��B	�zB	��B	�FB	�%B	�!B	��B	��B	�B	� B	�nB	��B	�!B	�IB	�]B	�aB	��B	��B	��B	��B	�UB	� B	�cB	�}B	�B	��B	��B	�)B	�OB	�3B	��B	��B	�LB	��B	�B	��B	�DB	��B	��B	��B	��B	�jB	��B	�JB	��B	�lB	�lB	�8B	��B	�XB	�DB	��B	��B	��B	�"B	��B	�B	��B	��B	��B	�cB	�oB	��B	ðB	�GB	�SB	�mB	ňB	ŢB	��B	�%B	�tB	ƨB	��B	��B	��B	��B	��B	ƎB	�tB	ƎB	ƎB	ƨB	ƎB	ƎB	�+B	�%B	�mB	ƨB	ǮB	�1B	ȚB	�RB	�7B	��B	ȀB	��B	ʌB	�dB	�"B	ΥB	�<B	ΥB	�B	��B	��B	�TB	��B	�FB	յB	�SB	�sB	ڠB	ۦB	�)B	�CB	��B	��B	یB	�xB	�OB	�\B	�B	�hB	��B	�BB	�B	�qB	�dB	ݲB	�IB	��B	�OB	�OB	�5B	ޞB	�B	��B	�tB	��B	��B	�B	�B	��B	�8B	�B	�>B	�B	�
B	�B	�*B	��B	�XB	��B	�@B	�LB	�B	�B	�yB	�"B	�B	�@B	�dB	�=B	�OB	�	B	�	B	�yB	��B	��B	�CB	��B	�0B	�eB	�$B	��B	�B	�nB	�ZB	ޞB	�B	�OB	�B	�VB	�B	�B	�&B	��B	�B	�WB	�B	�B	�CB	�wB	�B	�cB	��B	��B	�mB	�B	�0B	�*B	�B	��B	�fB	�2B	�`B	�B	��B	�nB	�@B	�B	�mB	�B	��B	�_B	�B	��B	�AB	�vB	�B	��B	��B	��B	�aB	�GB	�|B	�B	��B	�B	��B	�B	�^B	�^B	��B	��B	�DB	��B	�DB	��B	�XB	�^B	�*B	�B	�^B	�dB	��B	��B	�PB	�"B	��B	�BB	��B	�VB	��B	��B	��B	��B	��B	��B	��B	�cB	�cB	�.B	�HB	�HB	��B	��B	��B
 B
  B
 B
 4B
 iB
 �B
 B
;B
 �B
;B
 B
oB
�B
[B
�B
�B
�B
gB
gB
SB
B
%B
%B
YB
tB
EB
�B
�B
KB
�B
	B
	�B
	�B
	RB
�B
�B
	�B
^B
�B
0B
jB
�B
"B
�B
B
\B
vB
(B
�B
�B
vB
(B
BB
�B
�B
B
HB
}B
bB
�B
�B
�B
�B
�B
�B
uB
�B
�B
uB
�B
�B
B
@B
[B
�B
&B
uB
�B
�B
�B
�B
2B
mB
9B
�B
B
B
�B
B
�B
�B
�B
B
yB
�B
�B
�B
B
QB
7B
B
QB
�B
�B
�B
B
�B
xB
�B
B
IB
OB
�B
�B
�B
�B
�B
�B
 B
 vB
 vB
 �B
 �B
!-B
 �B
 �B
 �B
!|B
"�B
"�B
"�B
# B
#TB
#�B
$B
$@B
$�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
'�B
($B
(>B
(sB
(XB
(sB
(�B
(�B
(�B
(XB
(XB
)DB
)*B
)yB
)�B
)�B
)�B
)�B
*eB
+B
+6B
+�B
,B
+�B
+�B
+�B
,"B
,"B
,qB
,�B
,�B
-]B
-)B
-CB
-�B
-�B
.�B
/B
/�B
/�B
0B
0B
0B
/�B
0�B
1vB
1�B
1�B
1�B
1vB
2B
2|B
2GB
2�B
2�B
2�B
2�B
3MB
3�B
4B
4B
4�B
4�B
4�B
5?B
5?B
5�B
6+B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
9$B
9�B
9�B
:*B
9�B
9�B
9XB
9$B
8�B
8�B
8�B
:DB
9�B
9XB
9�B
:xB
;�B
;JB
:�B
:�B
;B
<�B
=VB
=�B
=<B
;�B
;�B
;0B
;JB
<PB
<6B
<�B
="B
=B
<�B
<�B
<�B
<�B
=B
=<B
=VB
<�B
=�B
>(B
>�B
>�B
?B
?cB
?�B
@B
@OB
@OB
@OB
@4B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AB
A;B
B'B
B[B
B�B
C-B
CB
C�B
D3B
DMB
E9B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
F�B
F�B
GEB
G�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
J=B
JXB
J�B
J�B
K�B
K�B
LB
L0B
LB
K�B
L0B
L~B
MB
L�B
MB
MPB
MPB
M�B
M�B
N<B
N�B
N�B
NpB
N�B
OvB
O�B
O�B
PB
PbB
PbB
PbB
P}B
P�B
P.B
P�B
QhB
Q�B
RB
R B
R:B
R�B
R�B
S@B
S�B
S�B
S�B
TB
TFB
TFB
T{B
T{B
T�B
T�B
T�B
U2B
U2B
U�B
UgB
U�B
U�B
U�B
V9B
VmB
V�B
V�B
V�B
W
B
W$B
W�B
X+B
XEB
X�B
X�B
Y1B
YB
YeB
Y�B
Y�B
Y�B
Y�B
ZkB
ZkB
ZkB
Z�B
Z�B
[	B
[	B
Z�B
[=B
[�B
[�B
[�B
[�B
\)B
\CB
\)B
\xB
\]B
[�B
[qB
[=B
[=B
[WB
\CB
\�B
]~B
]�B
]�B
]�B
]�B
^B
^B
^OB
^B
^OB
^jB
^�B
^�B
^jB
^�B
^�B
_!B
_B
_!B
_;B
_;B
_VB
_pB
`'B
`�B
`�B
`�B
`�B
aB
`�B
a|B
b4B
bB
b�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
d@B
d�B
d�B
eB
e,B
eFB
e�B
e�B
e�B
f2B
ffB
f�B
f�B
gB
f�B
gRB
h
B
h
B
h$B
h$B
h$B
h
B
hXB
h�B
h�B
h�B
h�B
iB
i*B
i�B
jeB
j�B
j�B
jB
kB
k�B
k�B
kkB
k�B
kkB
k�B
k�B
lB
l=B
l�B
l�B
mB
mB
m)B
m�B
m�B
n/B
ncB
n�B
n�B
n�B
o�B
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r|B
r|B
r�B
r�B
s�B
s�B
tB
tB
tB
t9B
tTB
t�B
uB
u?B
u?B
uZB
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175256  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175256  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175256                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025304  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025304  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                