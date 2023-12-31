CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:01Z creation;2022-06-04T17:52:01Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175201  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�Ӡm:1   @�`$h�@0�Ƨ�cz�+J1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bz��B33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B���B�  B�ffB�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>33C?�fCA�fCC�fCF  CH  CJ  CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @G�@qG�@��
@��
A�A<Q�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�BzG�B~�B��>B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��B��qB��qB��>B��qBýqBȊ>B˽qB�#�BӽqB׽qB۽qB߽qB�qB�>B�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	�RC�RC޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;�RC>�C?�CA�CC�CE޸CG޸CI޸CK�CM�CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce�RCg�RCi޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&~D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN�DO~DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D��
D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A� �A�A�"�A�A�{A���A��
A��A���A�iDA��A��A��"A��A��A��ZA��A���A�خA��KAƻdAƸ�AƷ�AƾA��'Aƨ$AƄA�C-A��A���AŢ�A��A�	A�YA�&�A���A�*�A��A�L�A��#A��]A�Q�A�Q�A��NA�:A�w�A��zA�F�A���A�!A��uA�v�A���A�w�A�sA��UA��RA�f�A�ʌA��5A���A�!�A���A���A���A��iA���A��#A���A���A��vA�R A�%FA���A���A���A��?A��7A��A���A�y�A�"hA���A�I�A�C�A�:�A�oA��XA�{�A�iA�;�A��A�7LA��&A~GAy�Aw*0As\)AoM�Al��Ag��A]=�AU��AT.�AR��AQl�AK�cAI�HAH�4AGM�AB�A@)�A=�A;�A:�]A:�A8��A8��A7�?A6e,A5��A3�A3VA1c�A0 \A/�A-�rA-�;A-4A,�AA+f�A)�9A)�:A(^5A&y�A%�{A$kQA#�:A"҉A"/�A!-�A!0UA!K^A!hsA ��A��A��A��AA A��A�AxlA�AAU2A��A�A�A%�A{�A�XA�;A�A,=A� A�RA<�A�AzxA�RAL0A1'A�A�A�'A�MA��A�A[WA+�A�A_�AA�WAa|AG�A��AAiDA��A�A+AjA|�A
�|A
�"A
B�A
�A	��A��AB[A�0A�rAo�A4nA�xA�A��AMjAȴA��A��A]dA|�A$tA�HA��A�FA_�AMA w2A W�A XA e@�\�@���@�c@���@���@�͟@���@���@��)@�҉@��@���@�0U@���@�Vm@���@�Ov@���@��/@�\�@�6@�8�@��@�Ta@��
@�J�@�bN@���@�4�@��Q@�C@���@�@�_�@�@�2a@��P@��@�z@���@�@�#�@�"h@�S@�@��B@��@�h@���@�&�@�V@�+@�6�@��@���@�@ߒ:@�X@�0�@�+k@��@�z@ܝI@ܴ9@���@��"@�j�@���@�v`@ܵ@�q@�S�@���@��@�j@�/@��y@�:�@�c�@��@�A�@�@��m@׭C@�P�@�%@��]@�ѷ@֞@�8�@՞�@ԧ�@ӫ�@���@�~�@�@�J#@й$@� �@ϲ�@ς�@ϑh@ϔ�@ϊ�@�}�@�n/@�=�@���@���@΁o@��}@�@@̓u@�{@�ԕ@ˉ7@�}�@�O�@��"@��@�@�˒@ƣ�@Ƴh@��@��6@�]�@�!-@�!-@��@�j@��@��@�Z�@��@�@���@�P�@��b@���@�^5@��A@���@��
@��@��~@�,�@��\@�]d@�$@���@�/�@���@��@��@�S�@��@�@�"h@���@�T�@��@�@��	@���@�L0@�3�@���@��@�n/@���@���@���@�c @���@�F�@�8@�$t@��"@��}@��@�"�@��O@�V�@�خ@��f@�ȴ@��@��C@��@�ѷ@�V@��K@�4@��?@���@�]d@�ƨ@�|�@��@�~@���@�H�@�Y�@��@�ѷ@�~(@�($@��@��@�{�@�p;@���@�8�@�;�@�H�@���@�j�@�1�@��]@���@�{�@��@��w@��@�K�@���@�� @�7�@���@���@�m]@��@���@��@��j@��@���@��^@�ƨ@��M@���@��~@�=�@�o@��2@���@��Y@�YK@��A@��N@� �@�1@�G@��@�k�@�6�@���@�e�@�:�@�"�@��@�@�@��$@�Y�@��@�Ĝ@��L@�e�@��@�j@�	l@��s@��6@�S�@�	�@��X@�K�@�@���@��j@�_@�@��~@�A�@�(@��]@��1@�e�@�.�@��@���@�X�@��y@��\@�c @�8�@�	@�1@��o@��T@��=@���@�y>@��@�[W@��@��b@�Ov@�'R@�x@��g@��:@�2a@�ȴ@���@���@�y>@�@�@�u@��#@��@��0@���@�qv@�.I@�@@��@��@��I@�4n@��@��@�?}@��@���@��v@��@���@��u@�z@�W�@��@���@��@�X�@��@��@��@��@�c�@�C�@�J@��&@��t@���@��f@��f@�f�@�,�@�V@��h@�;�@�M@�  @�6@�P@Z�@U�@S�@Mj@=@C@~�!@}��@}��@}��@}��@}rG@}5�@|��@|9X@{�@{l�@z��@z�R@z��@zz@zH�@z
�@yY�@xS�@x�@w��@w��@w_p@v��@v�@vc @v)�@u��@uX@uO�@u�@t��@t��@th�@tZ@t �@t1@sX�@s�@r�m@rW�@q��@qc�@p��@pg8@p�@o�q@o�@nO@m+�@l��@l��@l�@l��@ly>@lg8@l%�@k��@kt�@kX�@k;d@k(@j��@j&�@i��@i;@h��@he�@hU2@g�r@g��@gC@f�@f3�@e��@e�M@eY�@dĜ@d�I@dc�@dM@d?�@c�q@c.I@bYK@a�T@a�"@aVm@`��@`Q�@_�@_��@_�$@_e�@_�@_�@_�@^ȴ@^H�@]�@]��@]O�@\�@\��@\K^@[��@Z�2@Zv�@Z@Y��@Y��@Y�n@Y��@Y\�@Y%F@XĜ@X<�@W�W@Wy�@W33@V��@V�@U�@U8�@T��@T6@T%�@TM@Tb@S�Q@S{J@S�@R��@RE�@Q��@Qhs@Q@P�u@O�F@O��@O��@O_p@O)_@O�@N��@N}V@N	@M�-@MQ�@MB�@M&�@L�@L��@L�O@L�@K�0@Ke�@K�@K�@Jߤ@Je@I��@IrG@I7L@H�@H��@H%�@G�[@Gt�@G�@F��@F�X@F��@FV@F0U@E�D@E��@EVm@D�@DG@C�;@C˒@C�f@CRT@C;d@C�@B��@B�@A�@Ac�@@�f@@�e@@S�@@@?��@?+@>ں@>�\@>5?@>�@> �@=�>@=�@=�j@=��@=ϫ@=��@=�C@=4@=�@<�@<�D@<c�@<<�@<"h@;�m@;�4@;�@:�@:�}@:��@:xl@:J�@9��@9s�@9?}@8�)@8tT@8e�@8(�@8G@7�q@6��@6��@6C�@5�@5��@5c�@5?}@5�@5@4�v@4�O@4��@4e�@4,=@3�
@3iD@2�@2��@2?@2&�@1��@1?}@1�@0��@0r�@0*�@/�q@//�@.�@.�B@.�1@.@�@-�@-�-@-w2@-a�@-B�@,�	@,��@,��@,�@,_@,1@+��@+s@+b�@+>�@*�H@*� @*�+@*��@*z@*E�@)�o@)�X@)x�@)-w@(��@(�@(�_@(I�@'��@'U�@'�@&�R@&�@&s�@&YK@&@&
�@%�.@%�^@%a�@%Q�@%?}@$�@$��@$��@$�z@$�@$1'@#��@#�Q@#��@#��@#=@"�@"�@"�B@"�6@"��@"}V@"i�@"C�@"�@!��@!�@!zx@ �P@ y>@ <�@��@�:@x@33@Y@�@�@ȴ@:*@�@k�@-w@��@Q�@�@� @�[@�@@�V@��@P�@��@�R@�r@�A@��@v�@\�@c @c @-@ԕ@�@�/@��@��@h�@A�@~@��@��@,�@�H@�6@��@~�@l�@R�@8�@)�@�T@zx@+�@�@��@�Y@-�@�@�m@��@��@x@@O@�@�@��@�x@:*@-@$�@�@�@��@w2@^�@S&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A� �A�A�"�A�A�{A���A��
A��A���A�iDA��A��A��"A��A��A��ZA��A���A�خA��KAƻdAƸ�AƷ�AƾA��'Aƨ$AƄA�C-A��A���AŢ�A��A�	A�YA�&�A���A�*�A��A�L�A��#A��]A�Q�A�Q�A��NA�:A�w�A��zA�F�A���A�!A��uA�v�A���A�w�A�sA��UA��RA�f�A�ʌA��5A���A�!�A���A���A���A��iA���A��#A���A���A��vA�R A�%FA���A���A���A��?A��7A��A���A�y�A�"hA���A�I�A�C�A�:�A�oA��XA�{�A�iA�;�A��A�7LA��&A~GAy�Aw*0As\)AoM�Al��Ag��A]=�AU��AT.�AR��AQl�AK�cAI�HAH�4AGM�AB�A@)�A=�A;�A:�]A:�A8��A8��A7�?A6e,A5��A3�A3VA1c�A0 \A/�A-�rA-�;A-4A,�AA+f�A)�9A)�:A(^5A&y�A%�{A$kQA#�:A"҉A"/�A!-�A!0UA!K^A!hsA ��A��A��A��AA A��A�AxlA�AAU2A��A�A�A%�A{�A�XA�;A�A,=A� A�RA<�A�AzxA�RAL0A1'A�A�A�'A�MA��A�A[WA+�A�A_�AA�WAa|AG�A��AAiDA��A�A+AjA|�A
�|A
�"A
B�A
�A	��A��AB[A�0A�rAo�A4nA�xA�A��AMjAȴA��A��A]dA|�A$tA�HA��A�FA_�AMA w2A W�A XA e@�\�@���@�c@���@���@�͟@���@���@��)@�҉@��@���@�0U@���@�Vm@���@�Ov@���@��/@�\�@�6@�8�@��@�Ta@��
@�J�@�bN@���@�4�@��Q@�C@���@�@�_�@�@�2a@��P@��@�z@���@�@�#�@�"h@�S@�@��B@��@�h@���@�&�@�V@�+@�6�@��@���@�@ߒ:@�X@�0�@�+k@��@�z@ܝI@ܴ9@���@��"@�j�@���@�v`@ܵ@�q@�S�@���@��@�j@�/@��y@�:�@�c�@��@�A�@�@��m@׭C@�P�@�%@��]@�ѷ@֞@�8�@՞�@ԧ�@ӫ�@���@�~�@�@�J#@й$@� �@ϲ�@ς�@ϑh@ϔ�@ϊ�@�}�@�n/@�=�@���@���@΁o@��}@�@@̓u@�{@�ԕ@ˉ7@�}�@�O�@��"@��@�@�˒@ƣ�@Ƴh@��@��6@�]�@�!-@�!-@��@�j@��@��@�Z�@��@�@���@�P�@��b@���@�^5@��A@���@��
@��@��~@�,�@��\@�]d@�$@���@�/�@���@��@��@�S�@��@�@�"h@���@�T�@��@�@��	@���@�L0@�3�@���@��@�n/@���@���@���@�c @���@�F�@�8@�$t@��"@��}@��@�"�@��O@�V�@�خ@��f@�ȴ@��@��C@��@�ѷ@�V@��K@�4@��?@���@�]d@�ƨ@�|�@��@�~@���@�H�@�Y�@��@�ѷ@�~(@�($@��@��@�{�@�p;@���@�8�@�;�@�H�@���@�j�@�1�@��]@���@�{�@��@��w@��@�K�@���@�� @�7�@���@���@�m]@��@���@��@��j@��@���@��^@�ƨ@��M@���@��~@�=�@�o@��2@���@��Y@�YK@��A@��N@� �@�1@�G@��@�k�@�6�@���@�e�@�:�@�"�@��@�@�@��$@�Y�@��@�Ĝ@��L@�e�@��@�j@�	l@��s@��6@�S�@�	�@��X@�K�@�@���@��j@�_@�@��~@�A�@�(@��]@��1@�e�@�.�@��@���@�X�@��y@��\@�c @�8�@�	@�1@��o@��T@��=@���@�y>@��@�[W@��@��b@�Ov@�'R@�x@��g@��:@�2a@�ȴ@���@���@�y>@�@�@�u@��#@��@��0@���@�qv@�.I@�@@��@��@��I@�4n@��@��@�?}@��@���@��v@��@���@��u@�z@�W�@��@���@��@�X�@��@��@��@��@�c�@�C�@�J@��&@��t@���@��f@��f@�f�@�,�@�V@��h@�;�@�M@�  @�6@�P@Z�@U�@S�@Mj@=@C@~�!@}��@}��@}��@}��@}rG@}5�@|��@|9X@{�@{l�@z��@z�R@z��@zz@zH�@z
�@yY�@xS�@x�@w��@w��@w_p@v��@v�@vc @v)�@u��@uX@uO�@u�@t��@t��@th�@tZ@t �@t1@sX�@s�@r�m@rW�@q��@qc�@p��@pg8@p�@o�q@o�@nO@m+�@l��@l��@l�@l��@ly>@lg8@l%�@k��@kt�@kX�@k;d@k(@j��@j&�@i��@i;@h��@he�@hU2@g�r@g��@gC@f�@f3�@e��@e�M@eY�@dĜ@d�I@dc�@dM@d?�@c�q@c.I@bYK@a�T@a�"@aVm@`��@`Q�@_�@_��@_�$@_e�@_�@_�@_�@^ȴ@^H�@]�@]��@]O�@\�@\��@\K^@[��@Z�2@Zv�@Z@Y��@Y��@Y�n@Y��@Y\�@Y%F@XĜ@X<�@W�W@Wy�@W33@V��@V�@U�@U8�@T��@T6@T%�@TM@Tb@S�Q@S{J@S�@R��@RE�@Q��@Qhs@Q@P�u@O�F@O��@O��@O_p@O)_@O�@N��@N}V@N	@M�-@MQ�@MB�@M&�@L�@L��@L�O@L�@K�0@Ke�@K�@K�@Jߤ@Je@I��@IrG@I7L@H�@H��@H%�@G�[@Gt�@G�@F��@F�X@F��@FV@F0U@E�D@E��@EVm@D�@DG@C�;@C˒@C�f@CRT@C;d@C�@B��@B�@A�@Ac�@@�f@@�e@@S�@@@?��@?+@>ں@>�\@>5?@>�@> �@=�>@=�@=�j@=��@=ϫ@=��@=�C@=4@=�@<�@<�D@<c�@<<�@<"h@;�m@;�4@;�@:�@:�}@:��@:xl@:J�@9��@9s�@9?}@8�)@8tT@8e�@8(�@8G@7�q@6��@6��@6C�@5�@5��@5c�@5?}@5�@5@4�v@4�O@4��@4e�@4,=@3�
@3iD@2�@2��@2?@2&�@1��@1?}@1�@0��@0r�@0*�@/�q@//�@.�@.�B@.�1@.@�@-�@-�-@-w2@-a�@-B�@,�	@,��@,��@,�@,_@,1@+��@+s@+b�@+>�@*�H@*� @*�+@*��@*z@*E�@)�o@)�X@)x�@)-w@(��@(�@(�_@(I�@'��@'U�@'�@&�R@&�@&s�@&YK@&@&
�@%�.@%�^@%a�@%Q�@%?}@$�@$��@$��@$�z@$�@$1'@#��@#�Q@#��@#��@#=@"�@"�@"�B@"�6@"��@"}V@"i�@"C�@"�@!��@!�@!zx@ �P@ y>@ <�@��@�:@x@33@Y@�@�@ȴ@:*@�@k�@-w@��@Q�@�@� @�[@�@@�V@��@P�@��@�R@�r@�A@��@v�@\�@c @c @-@ԕ@�@�/@��@��@h�@A�@~@��@��@,�@�H@�6@��@~�@l�@R�@8�@)�@�T@zx@+�@�@��@�Y@-�@�@�m@��@��@x@@O@�@�@��@�x@:*@-@$�@�@�@��@w2@^�@S&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�BxBw�Bw�Bw2BvFBu?BtTBs3Bq�Bh$B`B]IB]�B_;B_pBa�BeBg�Bh
BiDBl"Bn�Bq�Bz�B~B�iB�XB�B�CB�	B��B	;B	�B	��B	��B	��B	��B	�HB	�B
*�B
v�B
��B
��B
��B
��B~B�BF�B~B�YB��B��B�JBȀBѷB��B� B��B�B��B�BB�B�?B.Bs�Bg�BY�BQhBZQBZ7BS&BJXBX+B`�Be`Bf�BY�B9>B&2B'�B�B�B�B
�B
�@B
�hB
�`B
yXB
aB
P�B
@�B
8RB
0�B
jB	�fB	��B	�aB	�0B	�\B	zDB	VB	$�B�HB��B�'B�QB�OBؓB�FB�VB� B��B՛B�aB��BںB�B�B�}B	�B	{B	�B	'�B	8lB	@�B	O�B	X�B	gmB	~B	�B	�B	�~B	�RB	�B	��B	�B	��B	��B	�wB	�FB	��B	��B	��B	��B	�^B	�3B	��B	�WB	��B	��B	��B	��B	�BB	�!B	�*B	�B	��B	��B	ȚB	��B	�mB	��B	�4B	ѷB	�gB	�B	��B	��B	��B	҉B	�oB	ҽB	ҽB	҉B	�aB	�
B	֡B	��B	�B	��B	�B	��B	��B	��B	خB	�B	�#B	�=B	�B	ܒB	��B	�CB	��B	ݲB	�IB	�dB	�/B	ܬB	�)B	��B	�IB	�]B	�WB	�=B	��B	چB	��B	��B	�yB	�EB	�B	�YB	�YB	��B	��B	�[B	��B	ңB	҉B	�B	��B	ѝB	ּB	�
B	֡B	�,B	��B	͟B	�DB	ˬB	̳B	��B	�B	��B	�<B	��B	�B	��B	�aB	өB	� B	� B	ϫB	�vB	�B	͹B	�PB	�0B	˒B	��B	ʦB	��B	�B	��B	�=B	�#B	�	B	ɺB	�7B	ɠB	͹B	ȴB	�YB	�9B	ðB	�'B	��B	�B	�}B	�cB	�}B	��B	�1B	��B	��B	�.B	��B	��B	�(B	��B	ɆB	�	B	ɠB	ʦB	͹B	��B	�B	�sB	��B	��B	��B	��B	�B	��B	��B	�5B	��B	�B	�vB	��B	�|B	�|B	�|B	��B	��B	�B	��B	�B	��B	�B	�|B	�B	�aB	��B	��B	�-B	��B	�B	�!B	�iB	��B	�B	�OB	�B	�'B	�B	�MB	�hB	�hB	�hB	��B	�9B	�B	�fB	�B	��B	�FB	��B	�B	��B	��B	�+B	��B	�B	�]B	�kB	�B	��B	�B	�QB	��B	��B	��B	��B	�B	�B	�iB	��B	�B	�hB	�MB	�B	�-B	�B	�!B	��B	��B	��B	��B	�B	��B	��B	�TB	�B	�B	�B	�hB	�%B	��B	�zB	�+B	�MB	�B	�aB	�-B	��B	��B	��B	�GB	�MB	�B	�hB	�B	��B	��B	�aB	��B	�'B	��B	��B	�B	�oB	�B	�B	�'B	��B	�B	�|B	�9B	�zB	��B	�B	��B	��B	��B	�*B	�B	�rB	�lB	��B	�$B	��B	�jB	��B	�rB	�^B	�PB	�B	��B
 �B
 �B
[B
�B
�B
�B
{B
�B
3B
�B
�B

	B

rB
�B
�B
�B
~B
0B
�B
�B
�B
�B
<B
pB
pB
VB
pB
�B
�B
�B
<B
�B
�B
}B
bB
[B
�B
�B
�B
�B
uB
�B
�B
�B
:B
�B
MB
MB
�B
&B
4B
�B
BB
(B
B
vB
�B
�B
hB
�B
TB
oB
oB
�B
B
�B
�B
�B
B
MB
B
2B
�B
B
B
mB
�B
�B
$B
�B
�B
EB
�B
�B
B
KB
eB
�B
B
QB
�B
#B
WB
�B
�B
)B
B
jB
OB
�B
�B
 B
 �B
 �B
 �B
!B
!-B
!|B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#:B
#:B
# B
#�B
#�B
#�B
#�B
#�B
$B
$�B
%,B
%�B
&2B
&�B
'RB
'mB
'RB
'RB
'RB
'�B
'�B
(
B
(XB
(�B
(�B
)_B
)�B
*�B
*�B
*�B
*�B
+kB
+QB
+�B
,qB
,�B
-CB
.cB
.}B
.�B
/5B
/�B
/�B
/iB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0UB
0;B
0�B
0�B
1[B
1[B
1AB
1vB
0�B
0�B
0�B
1[B
1vB
1vB
1�B
1�B
1�B
2B
2�B
2�B
2|B
2�B
2�B
33B
33B
3MB
3�B
49B
4nB
4TB
4�B
4�B
5B
5B
5B
5ZB
5?B
6`B
6`B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8lB
9$B
9>B
8�B
8�B
:^B
:�B
:�B
:�B
:�B
;�B
<B
="B
=B
<�B
<�B
=B
=<B
=�B
=�B
=�B
>B
>B
>BB
>wB
>�B
>�B
?HB
?�B
?�B
?�B
@iB
@OB
@�B
@iB
@OB
@�B
@�B
A�B
A�B
BB
BAB
B[B
B�B
B�B
B�B
CGB
C�B
C�B
C{B
C{B
C�B
D3B
D3B
DMB
D�B
D�B
D�B
D�B
EmB
FB
FYB
F�B
F�B
F�B
F�B
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
HB
H�B
H�B
IRB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
J�B
K^B
K^B
K^B
K�B
L0B
LJB
LJB
LdB
L�B
L�B
L�B
M6B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NVB
N�B
N�B
N�B
N�B
OvB
O�B
O�B
O�B
P.B
PHB
P}B
P�B
P�B
Q4B
Q4B
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S&B
S[B
S[B
SuB
SuB
S�B
T{B
T�B
T�B
T�B
UB
UB
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W
B
W?B
WsB
W�B
W�B
W�B
W�B
XB
XEB
XyB
XyB
X�B
X�B
X�B
X�B
YeB
YeB
Y�B
ZB
ZB
ZQB
Z7B
ZkB
[#B
[WB
[�B
[�B
\B
\]B
\xB
\�B
\xB
\�B
\�B
\�B
\�B
]/B
]dB
]�B
^B
^jB
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`B
`\B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
a�B
bB
bNB
b4B
bNB
b�B
b�B
c:B
cTB
cTB
c�B
dB
dZB
dZB
dZB
d@B
dtB
d�B
eB
d�B
eFB
e`B
e�B
ezB
e�B
fLB
f�B
f�B
gB
gB
gB
gB
gmB
gmB
gmB
g�B
h
B
h$B
h
B
hsB
hsB
h�B
h�B
h�B
h�B
i*B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
k6B
k�B
k�B
l=B
lqB
l�B
mB
l�B
mB
m)B
mB
m�B
m)B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
poB
p�B
qB
q'B
q'B
qAB
q[B
q�B
rGB
raB
r�B
r�B
sB
s3B
s3B
shB
shB
sMB
s�B
s�B
t9B
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
vFB
vFB
vzB
wB
v�B
v�B
wB
wLB
wfB
w�B
w�B
x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�BxBw�Bw�Bw2BvFBu?BtTBs3Bq�Bh$B`B]IB]�B_;B_pBa�BeBg�Bh
BiDBl"Bn�Bq�Bz�B~B�iB�XB�B�CB�	B��B	;B	�B	��B	��B	��B	��B	�HB	�B
*�B
v�B
��B
��B
��B
��B~B�BF�B~B�YB��B��B�JBȀBѷB��B� B��B�B��B�BB�B�?B.Bs�Bg�BY�BQhBZQBZ7BS&BJXBX+B`�Be`Bf�BY�B9>B&2B'�B�B�B�B
�B
�@B
�hB
�`B
yXB
aB
P�B
@�B
8RB
0�B
jB	�fB	��B	�aB	�0B	�\B	zDB	VB	$�B�HB��B�'B�QB�OBؓB�FB�VB� B��B՛B�aB��BںB�B�B�}B	�B	{B	�B	'�B	8lB	@�B	O�B	X�B	gmB	~B	�B	�B	�~B	�RB	�B	��B	�B	��B	��B	�wB	�FB	��B	��B	��B	��B	�^B	�3B	��B	�WB	��B	��B	��B	��B	�BB	�!B	�*B	�B	��B	��B	ȚB	��B	�mB	��B	�4B	ѷB	�gB	�B	��B	��B	��B	҉B	�oB	ҽB	ҽB	҉B	�aB	�
B	֡B	��B	�B	��B	�B	��B	��B	��B	خB	�B	�#B	�=B	�B	ܒB	��B	�CB	��B	ݲB	�IB	�dB	�/B	ܬB	�)B	��B	�IB	�]B	�WB	�=B	��B	چB	��B	��B	�yB	�EB	�B	�YB	�YB	��B	��B	�[B	��B	ңB	҉B	�B	��B	ѝB	ּB	�
B	֡B	�,B	��B	͟B	�DB	ˬB	̳B	��B	�B	��B	�<B	��B	�B	��B	�aB	өB	� B	� B	ϫB	�vB	�B	͹B	�PB	�0B	˒B	��B	ʦB	��B	�B	��B	�=B	�#B	�	B	ɺB	�7B	ɠB	͹B	ȴB	�YB	�9B	ðB	�'B	��B	�B	�}B	�cB	�}B	��B	�1B	��B	��B	�.B	��B	��B	�(B	��B	ɆB	�	B	ɠB	ʦB	͹B	��B	�B	�sB	��B	��B	��B	��B	�B	��B	��B	�5B	��B	�B	�vB	��B	�|B	�|B	�|B	��B	��B	�B	��B	�B	��B	�B	�|B	�B	�aB	��B	��B	�-B	��B	�B	�!B	�iB	��B	�B	�OB	�B	�'B	�B	�MB	�hB	�hB	�hB	��B	�9B	�B	�fB	�B	��B	�FB	��B	�B	��B	��B	�+B	��B	�B	�]B	�kB	�B	��B	�B	�QB	��B	��B	��B	��B	�B	�B	�iB	��B	�B	�hB	�MB	�B	�-B	�B	�!B	��B	��B	��B	��B	�B	��B	��B	�TB	�B	�B	�B	�hB	�%B	��B	�zB	�+B	�MB	�B	�aB	�-B	��B	��B	��B	�GB	�MB	�B	�hB	�B	��B	��B	�aB	��B	�'B	��B	��B	�B	�oB	�B	�B	�'B	��B	�B	�|B	�9B	�zB	��B	�B	��B	��B	��B	�*B	�B	�rB	�lB	��B	�$B	��B	�jB	��B	�rB	�^B	�PB	�B	��B
 �B
 �B
[B
�B
�B
�B
{B
�B
3B
�B
�B

	B

rB
�B
�B
�B
~B
0B
�B
�B
�B
�B
<B
pB
pB
VB
pB
�B
�B
�B
<B
�B
�B
}B
bB
[B
�B
�B
�B
�B
uB
�B
�B
�B
:B
�B
MB
MB
�B
&B
4B
�B
BB
(B
B
vB
�B
�B
hB
�B
TB
oB
oB
�B
B
�B
�B
�B
B
MB
B
2B
�B
B
B
mB
�B
�B
$B
�B
�B
EB
�B
�B
B
KB
eB
�B
B
QB
�B
#B
WB
�B
�B
)B
B
jB
OB
�B
�B
 B
 �B
 �B
 �B
!B
!-B
!|B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#:B
#:B
# B
#�B
#�B
#�B
#�B
#�B
$B
$�B
%,B
%�B
&2B
&�B
'RB
'mB
'RB
'RB
'RB
'�B
'�B
(
B
(XB
(�B
(�B
)_B
)�B
*�B
*�B
*�B
*�B
+kB
+QB
+�B
,qB
,�B
-CB
.cB
.}B
.�B
/5B
/�B
/�B
/iB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0UB
0;B
0�B
0�B
1[B
1[B
1AB
1vB
0�B
0�B
0�B
1[B
1vB
1vB
1�B
1�B
1�B
2B
2�B
2�B
2|B
2�B
2�B
33B
33B
3MB
3�B
49B
4nB
4TB
4�B
4�B
5B
5B
5B
5ZB
5?B
6`B
6`B
6�B
6�B
6�B
7fB
7�B
7�B
7�B
8lB
9$B
9>B
8�B
8�B
:^B
:�B
:�B
:�B
:�B
;�B
<B
="B
=B
<�B
<�B
=B
=<B
=�B
=�B
=�B
>B
>B
>BB
>wB
>�B
>�B
?HB
?�B
?�B
?�B
@iB
@OB
@�B
@iB
@OB
@�B
@�B
A�B
A�B
BB
BAB
B[B
B�B
B�B
B�B
CGB
C�B
C�B
C{B
C{B
C�B
D3B
D3B
DMB
D�B
D�B
D�B
D�B
EmB
FB
FYB
F�B
F�B
F�B
F�B
F�B
F�B
GB
GEB
G�B
G�B
G�B
G�B
HB
H�B
H�B
IRB
I�B
I�B
I�B
I�B
I�B
I�B
J#B
JXB
J�B
J�B
K^B
K^B
K^B
K�B
L0B
LJB
LJB
LdB
L�B
L�B
L�B
M6B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NVB
N�B
N�B
N�B
N�B
OvB
O�B
O�B
O�B
P.B
PHB
P}B
P�B
P�B
Q4B
Q4B
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S&B
S[B
S[B
SuB
SuB
S�B
T{B
T�B
T�B
T�B
UB
UB
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W
B
W?B
WsB
W�B
W�B
W�B
W�B
XB
XEB
XyB
XyB
X�B
X�B
X�B
X�B
YeB
YeB
Y�B
ZB
ZB
ZQB
Z7B
ZkB
[#B
[WB
[�B
[�B
\B
\]B
\xB
\�B
\xB
\�B
\�B
\�B
\�B
]/B
]dB
]�B
^B
^jB
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`B
`\B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
a�B
bB
bNB
b4B
bNB
b�B
b�B
c:B
cTB
cTB
c�B
dB
dZB
dZB
dZB
d@B
dtB
d�B
eB
d�B
eFB
e`B
e�B
ezB
e�B
fLB
f�B
f�B
gB
gB
gB
gB
gmB
gmB
gmB
g�B
h
B
h$B
h
B
hsB
hsB
h�B
h�B
h�B
h�B
i*B
i*B
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
jB
j�B
j�B
j�B
k6B
k�B
k�B
l=B
lqB
l�B
mB
l�B
mB
m)B
mB
m�B
m)B
m�B
m�B
m�B
nB
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
poB
p�B
qB
q'B
q'B
qAB
q[B
q�B
rGB
raB
r�B
r�B
sB
s3B
s3B
shB
shB
sMB
s�B
s�B
t9B
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
vFB
vFB
vzB
wB
v�B
v�B
wB
wLB
wfB
w�B
w�B
x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104952  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175201  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175201  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175201                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025209  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025209  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                