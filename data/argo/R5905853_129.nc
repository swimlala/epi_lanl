CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-21T03:43:10Z creation;2022-08-21T03:43:10Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220821034310  20220821035905  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��}0�d1   @��~>F�@-�O�;d�c�ě��T1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBq33Bw��B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33BÙ�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<�C>�C?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @1G�@w�@��
@��
A�A=�A\Q�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_�GBg�GBp�Bw{Bz�B��qB��qB��qB�WB��>B��qB��qB��qB��qB��qB��qB��B��qB��qB��B��B�WBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C�RC�RC޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5�RC7޸C9޸C;�RC=�RC?�CA�CC�CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_�RCa�RCc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!�D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DN~DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�x�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D븤D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�x�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�J�A�J#A�H�A�A�A�"hA�FA��A�ٴA�ΥA���Aؽ�Aط�Aط�Aش9Aر�AذUAخIAجAإ�Aؖ�A؎�A؃�A�X�A�\�A�M6A�  AՐ�A�;�A�P}AҢ�AЂ�A��WA�7A�-CA�.A��A��
A��A� \A�x�A��zA���A�%zA��pA��HA�E�A�kA��A�PA��A�w2A���A�jKA���A�ÖA��A�qvA�X�A��-A���A�ȀA��=A�S&A�p;A�/A�>A�[#A��9A���A�`�A��~A���A���A��A���A��A��A�]�A��A�0�A��A��%A�.A���A��PA��A�j�A��A�XyA��EA��A���A�b�A�A|MAq�~Ai�Ae��Aa.IA\;AYDgAU7APE�AM�AM�AK��AG\)AD�}AC{JAA�)A@��A>\�A<qvA;o A9��A9�A8+A65?A5�A4�nA16A0VA0A/�hA.!A,m�A+MA*��A*XyA*�A)��A)�{A(֡A'�#A'VmA&�4A%�A%��A%6A$��A$��A$m�A#c A#p�A#��A#�A"6�A ��A��A��A�:A(A+�An/A�Av`A)�AH�A҉A7�A�&A�wA]dAںA\�A?�A��A�;A�A	lA�A��A^5A�A�yA��A�xA�Ay>AV�A��AA6�A��Al"A8A7A�AAGA��A�AOA�/Aw2A[WA�A
��A
@�A
�4A
A	w2A	�ASA�EA�gA�A��A(�A�A��A@AJ#AA�A�YAC�A�AJ�A a@�IR@��)@�K�@�P�@��e@�H�@�;d@��z@��P@��}@�\)@�;�@���@��@�|�@�#:@��@��1@��K@��@�`�@��@��@�9X@�^�@��,@�p;@���@�/@�d�@�^@�2a@��p@��6@�g8@��@��@�r�@�n@�^�@���@�_�@�+k@㸻@��@��a@�H�@��]@���@�9X@���@�L�@ެ@��@�#�@�\�@۵t@ۊ	@�B�@�F@��@ڦL@�q�@��@���@�~@�Y@��@��?@�Ft@�e,@ԕ�@�B[@�ƨ@�s@��@Ұ�@�\�@��;@�@�֡@Ь@�_@��m@�H�@Θ_@�C�@�	@��@���@�<6@��M@�}V@�_@˞�@��@ʫ6@�c @�#:@���@�$t@��	@�֡@��@��@ƝI@��]@Ŋ�@�;@�Z@�($@�˒@æ�@� \@���@�[�@�b@�rG@��@�L0@�u@���@�p�@�N<@�(�@��@���@�C-@��@�Vm@��@���@�C�@�@�	�@��o@���@�e�@���@��L@�_�@��o@��F@��@@��S@�X@���@���@�S�@�~@��;@��@�[W@��h@�x@��[@��=@�x@�J#@��@���@�;�@�b@��@��@�+�@��@�@��h@�X�@�A @��@���@�-�@���@��9@�RT@��@��1@�!@���@��-@�f�@�,�@���@�l"@�6�@��+@���@��[@�p;@���@��F@��~@�f�@� \@���@�Ft@��@�q@���@�'R@��@@�+@��h@�@�@�ϫ@���@�S&@���@���@�R�@��H@�'�@���@���@�~�@�J@���@�l�@���@��6@�GE@�*�@��r@���@�Y�@��@��<@�{�@�a|@�D�@�6@�2�@�@��T@��C@�Q�@�V@��5@��F@�YK@�I�@�G@�m]@�ߤ@�q@�S�@�.�@�{@��Q@�y�@�+�@��@���@��4@�oi@�GE@�4@���@���@�dZ@��@��@�h
@�*�@���@���@���@�o�@�K�@���@�u%@�*�@��F@�<6@�S@�ں@���@�a|@�D�@�&�@�b@��K@�a@��@���@��1@�u%@�5?@�e@��@���@�Dg@��,@���@�Q@���@���@���@�F�@���@��L@�c @�.�@��@�ԕ@�E9@�Ĝ@�kQ@���@���@�Vm@��@���@�YK@��@��^@�^�@���@��@��@���@�GE@�O@���@��0@�IR@��P@��p@��h@�s�@�:�@��@�33@��5@�֡@���@���@�q@�w@_p@�@~ȴ@~;�@}j@|��@|"h@{�@{��@z�@z�+@zs�@z=q@y�=@yp�@y+@xN�@w�+@w~�@w�@vn�@u��@uzx@uG�@t��@t|�@tM@s�&@r�"@r:*@q�@qL�@p��@pZ@p9X@o��@o'�@nR�@m�-@l�5@lc�@l�@kP�@jJ�@i<6@h�U@hc�@h�@g�W@g�g@g�F@g��@g�q@g��@gW?@g]�@g�@f-@e	l@d6@dG@dl"@d�@dq@cC�@b�!@bV@bZ�@bE�@b�B@b�@ae,@a+�@`�P@`�Y@`$@_�F@_{J@_;d@_X�@^�@^#:@^B[@^ �@]��@]2a@\ی@\bN@\$@[خ@[e�@[8@[ i@Z��@Z
�@Y�@Y�~@YT�@Y�@X��@Xm�@W��@W&@V��@V^5@V#:@U�.@U�7@U8�@T�$@TQ�@S�g@S��@SA�@R��@R��@Rd�@Q��@Q�'@Qe,@QB�@Q+@P�E@PtT@PA�@O��@Oo@N�}@Nn�@M�@M[W@L�[@L�@LA�@K�@J��@Jc @JH�@Je@I��@I�@I�@I?}@H�5@H�9@H�@HQ�@H9X@G��@G9�@F�c@F�'@F($@E�z@E}�@E#�@D�`@D��@De�@C��@C
=@B��@BR�@A�@Au�@AIR@A&�@@�@@�@@[�@@  @>�@=�Z@=��@=G�@=#�@<�f@<�)@<�Y@<�@;�V@;K�@:�@:z@:�@9��@9��@9��@9IR@9%F@8�)@8U2@8�@7��@7��@7RT@7o@6ȴ@6��@6�@5��@5Vm@5(�@4�@4�$@4�@47@3��@3��@3|�@3S�@3C@2��@2p;@2�@1�'@1T�@0�|@0��@0tT@0-�@/��@/l�@.�]@.�+@.L0@. �@-�t@-8�@,�@,�?@,e�@+خ@+�k@+�{@+\)@*�@*��@*GE@*�@)ϫ@)��@)e,@)2a@(�?@(l"@'��@'=@&҉@&� @&J�@&	@%�t@%[W@%A @%@$��@$�@$>B@$G@#�@#�w@#�k@#iD@#>�@"��@"��@"�b@"i�@!��@!�S@!x�@!f�@!?}@!�@ �@ ��@ Q�@ %�@�}@��@�{@S�@�@��@��@L0@�@�N@��@j@a�@+�@�P@֡@�4@��@~(@y>@q@?�@2�@%�@�@��@{J@H�@�@�<@��@xl@E�@-@�D@�@��@hs@<6@@@�@��@�@Xy@?�@M@�@�@�:@j�@Z�@8@�M@ȴ@�b@��@{�@��@}V@h
@@�@;�@�@�9@��@[W@7L@ \@V@��@�p@��@g8@$@G@�m@��@� @� @��@J#@��@��@�@ں@��@h
@@�@�@��@��@5�@��@�@��@��@V�@��@�@\)@Mj@@�@�@��@�'@��@Ta@.�@_@�N@��@�^@�t@��@�X@x�@��@��@@��@��@c@G�@�@�	@��@�@r�@C-@�@�$@t�@t�@dZ@33@(@
��@
�L@
��@
��@
�@
�@
v�@
V@
0U@
($@
@	�.@	��@	��@	��@	�h@	Y�@	�@�5@�|@�`@��@��@7�@��@��@˒@��@l�@K�@=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�J�A�J#A�H�A�A�A�"hA�FA��A�ٴA�ΥA���Aؽ�Aط�Aط�Aش9Aر�AذUAخIAجAإ�Aؖ�A؎�A؃�A�X�A�\�A�M6A�  AՐ�A�;�A�P}AҢ�AЂ�A��WA�7A�-CA�.A��A��
A��A� \A�x�A��zA���A�%zA��pA��HA�E�A�kA��A�PA��A�w2A���A�jKA���A�ÖA��A�qvA�X�A��-A���A�ȀA��=A�S&A�p;A�/A�>A�[#A��9A���A�`�A��~A���A���A��A���A��A��A�]�A��A�0�A��A��%A�.A���A��PA��A�j�A��A�XyA��EA��A���A�b�A�A|MAq�~Ai�Ae��Aa.IA\;AYDgAU7APE�AM�AM�AK��AG\)AD�}AC{JAA�)A@��A>\�A<qvA;o A9��A9�A8+A65?A5�A4�nA16A0VA0A/�hA.!A,m�A+MA*��A*XyA*�A)��A)�{A(֡A'�#A'VmA&�4A%�A%��A%6A$��A$��A$m�A#c A#p�A#��A#�A"6�A ��A��A��A�:A(A+�An/A�Av`A)�AH�A҉A7�A�&A�wA]dAںA\�A?�A��A�;A�A	lA�A��A^5A�A�yA��A�xA�Ay>AV�A��AA6�A��Al"A8A7A�AAGA��A�AOA�/Aw2A[WA�A
��A
@�A
�4A
A	w2A	�ASA�EA�gA�A��A(�A�A��A@AJ#AA�A�YAC�A�AJ�A a@�IR@��)@�K�@�P�@��e@�H�@�;d@��z@��P@��}@�\)@�;�@���@��@�|�@�#:@��@��1@��K@��@�`�@��@��@�9X@�^�@��,@�p;@���@�/@�d�@�^@�2a@��p@��6@�g8@��@��@�r�@�n@�^�@���@�_�@�+k@㸻@��@��a@�H�@��]@���@�9X@���@�L�@ެ@��@�#�@�\�@۵t@ۊ	@�B�@�F@��@ڦL@�q�@��@���@�~@�Y@��@��?@�Ft@�e,@ԕ�@�B[@�ƨ@�s@��@Ұ�@�\�@��;@�@�֡@Ь@�_@��m@�H�@Θ_@�C�@�	@��@���@�<6@��M@�}V@�_@˞�@��@ʫ6@�c @�#:@���@�$t@��	@�֡@��@��@ƝI@��]@Ŋ�@�;@�Z@�($@�˒@æ�@� \@���@�[�@�b@�rG@��@�L0@�u@���@�p�@�N<@�(�@��@���@�C-@��@�Vm@��@���@�C�@�@�	�@��o@���@�e�@���@��L@�_�@��o@��F@��@@��S@�X@���@���@�S�@�~@��;@��@�[W@��h@�x@��[@��=@�x@�J#@��@���@�;�@�b@��@��@�+�@��@�@��h@�X�@�A @��@���@�-�@���@��9@�RT@��@��1@�!@���@��-@�f�@�,�@���@�l"@�6�@��+@���@��[@�p;@���@��F@��~@�f�@� \@���@�Ft@��@�q@���@�'R@��@@�+@��h@�@�@�ϫ@���@�S&@���@���@�R�@��H@�'�@���@���@�~�@�J@���@�l�@���@��6@�GE@�*�@��r@���@�Y�@��@��<@�{�@�a|@�D�@�6@�2�@�@��T@��C@�Q�@�V@��5@��F@�YK@�I�@�G@�m]@�ߤ@�q@�S�@�.�@�{@��Q@�y�@�+�@��@���@��4@�oi@�GE@�4@���@���@�dZ@��@��@�h
@�*�@���@���@���@�o�@�K�@���@�u%@�*�@��F@�<6@�S@�ں@���@�a|@�D�@�&�@�b@��K@�a@��@���@��1@�u%@�5?@�e@��@���@�Dg@��,@���@�Q@���@���@���@�F�@���@��L@�c @�.�@��@�ԕ@�E9@�Ĝ@�kQ@���@���@�Vm@��@���@�YK@��@��^@�^�@���@��@��@���@�GE@�O@���@��0@�IR@��P@��p@��h@�s�@�:�@��@�33@��5@�֡@���@���@�q@�w@_p@�@~ȴ@~;�@}j@|��@|"h@{�@{��@z�@z�+@zs�@z=q@y�=@yp�@y+@xN�@w�+@w~�@w�@vn�@u��@uzx@uG�@t��@t|�@tM@s�&@r�"@r:*@q�@qL�@p��@pZ@p9X@o��@o'�@nR�@m�-@l�5@lc�@l�@kP�@jJ�@i<6@h�U@hc�@h�@g�W@g�g@g�F@g��@g�q@g��@gW?@g]�@g�@f-@e	l@d6@dG@dl"@d�@dq@cC�@b�!@bV@bZ�@bE�@b�B@b�@ae,@a+�@`�P@`�Y@`$@_�F@_{J@_;d@_X�@^�@^#:@^B[@^ �@]��@]2a@\ی@\bN@\$@[خ@[e�@[8@[ i@Z��@Z
�@Y�@Y�~@YT�@Y�@X��@Xm�@W��@W&@V��@V^5@V#:@U�.@U�7@U8�@T�$@TQ�@S�g@S��@SA�@R��@R��@Rd�@Q��@Q�'@Qe,@QB�@Q+@P�E@PtT@PA�@O��@Oo@N�}@Nn�@M�@M[W@L�[@L�@LA�@K�@J��@Jc @JH�@Je@I��@I�@I�@I?}@H�5@H�9@H�@HQ�@H9X@G��@G9�@F�c@F�'@F($@E�z@E}�@E#�@D�`@D��@De�@C��@C
=@B��@BR�@A�@Au�@AIR@A&�@@�@@�@@[�@@  @>�@=�Z@=��@=G�@=#�@<�f@<�)@<�Y@<�@;�V@;K�@:�@:z@:�@9��@9��@9��@9IR@9%F@8�)@8U2@8�@7��@7��@7RT@7o@6ȴ@6��@6�@5��@5Vm@5(�@4�@4�$@4�@47@3��@3��@3|�@3S�@3C@2��@2p;@2�@1�'@1T�@0�|@0��@0tT@0-�@/��@/l�@.�]@.�+@.L0@. �@-�t@-8�@,�@,�?@,e�@+خ@+�k@+�{@+\)@*�@*��@*GE@*�@)ϫ@)��@)e,@)2a@(�?@(l"@'��@'=@&҉@&� @&J�@&	@%�t@%[W@%A @%@$��@$�@$>B@$G@#�@#�w@#�k@#iD@#>�@"��@"��@"�b@"i�@!��@!�S@!x�@!f�@!?}@!�@ �@ ��@ Q�@ %�@�}@��@�{@S�@�@��@��@L0@�@�N@��@j@a�@+�@�P@֡@�4@��@~(@y>@q@?�@2�@%�@�@��@{J@H�@�@�<@��@xl@E�@-@�D@�@��@hs@<6@@@�@��@�@Xy@?�@M@�@�@�:@j�@Z�@8@�M@ȴ@�b@��@{�@��@}V@h
@@�@;�@�@�9@��@[W@7L@ \@V@��@�p@��@g8@$@G@�m@��@� @� @��@J#@��@��@�@ں@��@h
@@�@�@��@��@5�@��@�@��@��@V�@��@�@\)@Mj@@�@�@��@�'@��@Ta@.�@_@�N@��@�^@�t@��@�X@x�@��@��@@��@��@c@G�@�@�	@��@�@r�@C-@�@�$@t�@t�@dZ@33@(@
��@
�L@
��@
��@
�@
�@
v�@
V@
0U@
($@
@	�.@	��@	��@	��@	�h@	Y�@	�@�5@�|@�`@��@��@7�@��@��@˒@��@l�@K�@=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
��B
��B
�B
�B
�B
��B
��B
�kB
��B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
�B
�B
{0B
g�B
gRB
oB
�B
��B
��B
׍B&�B5tBRTB}�B�B�hB�B7fBB�BgmB{dB��B��B�'B}VBw�B�BlWBx�B��B��B��B�B��B�Bu�Bj�BiDBd�B_;BW�BR BFB9>B2�B)*B# B+B:B�B��B��B�:B�sB�PB�B��B��ByrBM�B�B
�TB
��B
��B
��B
�UB
k�B
^B
VmB
IRB
BAB
4�B
	B	�}B	��B	��B	m]B	Y�B	<B	-�B		B�B��B�IB�B�#B�oB��B҉BϫB҉BϫB�B߾B�B��B�B��B��B��B�B�B�qB	gB	KB	vB	SB	�B	1'B	3B	8RB	7LB	;B	D�B	G�B	JXB	MPB	OBB	R�B	`\B	c�B	d�B	ncB	��B	��B	�@B	�PB	�oB	��B	��B	�`B	��B	v`B	p�B	w2B	~�B	�B	�_B	��B	�lB	��B	��B	�TB	��B	��B	��B	��B	�B	�BB	�'B	�B	�B	�B	�ZB	�`B	�B	�fB	��B	��B	��B	�yB	��B	��B	�)B	��B	�B	��B	��B	�B	ǔB	�<B	�[B	�2B	ՁB	�2B	�SB	�,B	�#B	�CB	�B	�jB	��B	� B	�B	�JB	ˬB	˒B	�BB	�B	��B	ȚB	�B	�B	�DB	��B	�6B	��B	ΥB	�\B	� B	�QB	�:B	ЗB	��B	�yB	�,B	�{B	��B	�yB	�1B	ٚB	�B	�$B	�B	��B	�2B	�MB	�B	��B	�B	�B	�aB	��B	�
B	�
B	�$B	�sB	��B	�yB	�1B	�]B	��B	��B	�VB	ߤB	�pB	��B	�BB	�'B	�B	��B	�vB	��B	�VB	�B	��B	��B	�vB	��B	�B	��B	ߊB	��B	��B	��B	��B	�B	�4B	�hB	��B	�tB	�B	�B	�8B	�B	�B	�B	�XB	�B	��B	��B	�B	�KB	�eB	�B	�6B	�"B	�WB	�B	�)B	�B	�)B	�)B	�OB	��B	�;B	�oB	��B	��B	��B	�-B	�B	�B	�MB	�hB	�B	�B	�9B	�?B	�B	�%B	�B	�B	�B	�9B	��B	��B	��B	��B	�*B	�B	��B	�B	��B	��B	�6B	�6B	��B	��B	�B	�B	�B	��B	�B	��B	��B	�6B	�6B	�6B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	��B	�qB	�<B	��B	��B	��B	�B	�B	��B	��B	�]B	�BB	�B	�B	�B	�]B	��B	��B	�}B	��B	��B	��B
 �B
 �B
;B
uB
[B
uB
�B
-B
�B
9B
�B
�B
�B
�B
B
�B
9B
9B
�B
B
EB
�B
�B
	B
B
	�B
	�B

�B

�B
)B
DB
�B
�B
dB
dB
�B
�B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
.B
B
.B
HB
bB
 B
4B
4B
NB
�B
�B
�B
[B
�B
2B
�B
�B
B
�B
�B
�B
�B

B
sB
+B
_B
_B
B
�B
�B
�B
�B
YB
sB
sB
$B

B

B
sB
�B
�B
+B
yB
�B
KB
eB
�B
�B
B
QB
�B
�B
�B
B
]B
�B
�B
�B
�B
�B
�B
!B
�B
 �B
!B
!�B
!�B
!�B
"B
!�B
"hB
"�B
"�B
"�B
"4B
"4B
"�B
"�B
"�B
# B
#�B
#�B
#�B
#�B
#�B
$@B
$ZB
$ZB
$�B
%B
%�B
%�B
%�B
%�B
&2B
&�B
&�B
'mB
($B
(�B
(sB
(XB
(�B
)�B
)�B
*B
*KB
*B
*�B
+B
,"B
,�B
,�B
-CB
-�B
.�B
/5B
/�B
/�B
0B
0B
0�B
1'B
2|B
2|B
2�B
33B
3�B
49B
4�B
4�B
4�B
5?B
5�B
6+B
6FB
6+B
6+B
6�B
6�B
6�B
7LB
8B
7�B
88B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:*B
:DB
:�B
:�B
:�B
;JB
;dB
;0B
;B
:�B
:�B
:�B
;dB
:�B
<B
<B
<PB
<jB
="B
<�B
=<B
=�B
=�B
=qB
=<B
>B
?HB
@�B
@�B
A�B
BAB
BB
B'B
BAB
A;B
@OB
@ B
@OB
A�B
CB
CB
AUB
@�B
@�B
A;B
A�B
EB
EB
C�B
C{B
C{B
C�B
C�B
DMB
C�B
D�B
F�B
F�B
F�B
H�B
I�B
I�B
J#B
J=B
J�B
J�B
J�B
K)B
K)B
K)B
K^B
L0B
L0B
L0B
LJB
LdB
L~B
L�B
MPB
M�B
N"B
N<B
N<B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PB
P}B
P�B
P�B
QB
QNB
QhB
Q�B
Q�B
Q�B
R B
RB
R�B
R�B
SB
S&B
S�B
S�B
S�B
S�B
T,B
T�B
TaB
T{B
T{B
U2B
VB
V�B
V�B
V�B
V�B
W$B
W$B
WYB
WYB
W�B
W�B
XB
X+B
X_B
X�B
YB
YeB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\)B
\CB
\]B
\CB
[�B
[qB
[�B
[�B
[�B
[�B
\B
\�B
\�B
]B
]/B
]�B
^5B
^jB
^�B
^�B
_!B
_pB
_pB
`B
`\B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
b4B
bhB
b�B
cTB
c�B
c�B
c�B
d&B
dB
d@B
d�B
d�B
d�B
d�B
e`B
eFB
e,B
e`B
ezB
e�B
fLB
f�B
f�B
gRB
gmB
gRB
gRB
g�B
g�B
h
B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
j�B
kB
j�B
kB
kB
kQB
kkB
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
n}B
n�B
n�B
o5B
oOB
oiB
oOB
oiB
o�B
o�B
o�B
o�B
pB
p!B
pUB
poB
p�B
p�B
qB
q�B
q�B
q�B
q�B
raB
r|B
r�B
r�B
r�B
s3B
s3B
sMB
sMB
shB
sMB
s�B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
u?B
uZB
utB
utB
u�B
u�B
u�B
v+B
vFB
vzB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y>B
yrB
y�B
zDB
{dB
{0B
|B
|�B
}VB
}qB
}qB
}<B
}�B
~B
}�B
}�B
~B
}�B
~]B
~�B
~�B
~�B
~�B
B
B
B
~�B
~�B
~wB
~�B
�B
�B
�OB
��B
��B
� B
�UB
�oB
�UB
�UB
��B
��B
��B
�UB
��B
�OB
�B
�B
�iB
��B
� B
�UB
�B
��B
� B
� B
�;B
��B
��B
��B
�GB
�aB
��B
��B
�B
�B
�9B
�9B
��B
�B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�1B
��B
�B
��B
��B
��B
�7B
�lB
��B
��B
��B
��B
��B
�#B
�#B
�XB
�rB
�rB
��B
�DB
�^B
�DB
�^B
�xB
�^B
�0B
�JB
�0B
�0B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
��B
��B
�B
�B
�B
��B
��B
�kB
��B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�B
��B
�B
�B
{0B
g�B
gRB
oB
�B
��B
��B
׍B&�B5tBRTB}�B�B�hB�B7fBB�BgmB{dB��B��B�'B}VBw�B�BlWBx�B��B��B��B�B��B�Bu�Bj�BiDBd�B_;BW�BR BFB9>B2�B)*B# B+B:B�B��B��B�:B�sB�PB�B��B��ByrBM�B�B
�TB
��B
��B
��B
�UB
k�B
^B
VmB
IRB
BAB
4�B
	B	�}B	��B	��B	m]B	Y�B	<B	-�B		B�B��B�IB�B�#B�oB��B҉BϫB҉BϫB�B߾B�B��B�B��B��B��B�B�B�qB	gB	KB	vB	SB	�B	1'B	3B	8RB	7LB	;B	D�B	G�B	JXB	MPB	OBB	R�B	`\B	c�B	d�B	ncB	��B	��B	�@B	�PB	�oB	��B	��B	�`B	��B	v`B	p�B	w2B	~�B	�B	�_B	��B	�lB	��B	��B	�TB	��B	��B	��B	��B	�B	�BB	�'B	�B	�B	�B	�ZB	�`B	�B	�fB	��B	��B	��B	�yB	��B	��B	�)B	��B	�B	��B	��B	�B	ǔB	�<B	�[B	�2B	ՁB	�2B	�SB	�,B	�#B	�CB	�B	�jB	��B	� B	�B	�JB	ˬB	˒B	�BB	�B	��B	ȚB	�B	�B	�DB	��B	�6B	��B	ΥB	�\B	� B	�QB	�:B	ЗB	��B	�yB	�,B	�{B	��B	�yB	�1B	ٚB	�B	�$B	�B	��B	�2B	�MB	�B	��B	�B	�B	�aB	��B	�
B	�
B	�$B	�sB	��B	�yB	�1B	�]B	��B	��B	�VB	ߤB	�pB	��B	�BB	�'B	�B	��B	�vB	��B	�VB	�B	��B	��B	�vB	��B	�B	��B	ߊB	��B	��B	��B	��B	�B	�4B	�hB	��B	�tB	�B	�B	�8B	�B	�B	�B	�XB	�B	��B	��B	�B	�KB	�eB	�B	�6B	�"B	�WB	�B	�)B	�B	�)B	�)B	�OB	��B	�;B	�oB	��B	��B	��B	�-B	�B	�B	�MB	�hB	�B	�B	�9B	�?B	�B	�%B	�B	�B	�B	�9B	��B	��B	��B	��B	�*B	�B	��B	�B	��B	��B	�6B	�6B	��B	��B	�B	�B	�B	��B	�B	��B	��B	�6B	�6B	�6B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	��B	�qB	�<B	��B	��B	��B	�B	�B	��B	��B	�]B	�BB	�B	�B	�B	�]B	��B	��B	�}B	��B	��B	��B
 �B
 �B
;B
uB
[B
uB
�B
-B
�B
9B
�B
�B
�B
�B
B
�B
9B
9B
�B
B
EB
�B
�B
	B
B
	�B
	�B

�B

�B
)B
DB
�B
�B
dB
dB
�B
�B
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
�B
.B
B
.B
HB
bB
 B
4B
4B
NB
�B
�B
�B
[B
�B
2B
�B
�B
B
�B
�B
�B
�B

B
sB
+B
_B
_B
B
�B
�B
�B
�B
YB
sB
sB
$B

B

B
sB
�B
�B
+B
yB
�B
KB
eB
�B
�B
B
QB
�B
�B
�B
B
]B
�B
�B
�B
�B
�B
�B
!B
�B
 �B
!B
!�B
!�B
!�B
"B
!�B
"hB
"�B
"�B
"�B
"4B
"4B
"�B
"�B
"�B
# B
#�B
#�B
#�B
#�B
#�B
$@B
$ZB
$ZB
$�B
%B
%�B
%�B
%�B
%�B
&2B
&�B
&�B
'mB
($B
(�B
(sB
(XB
(�B
)�B
)�B
*B
*KB
*B
*�B
+B
,"B
,�B
,�B
-CB
-�B
.�B
/5B
/�B
/�B
0B
0B
0�B
1'B
2|B
2|B
2�B
33B
3�B
49B
4�B
4�B
4�B
5?B
5�B
6+B
6FB
6+B
6+B
6�B
6�B
6�B
7LB
8B
7�B
88B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:*B
:DB
:�B
:�B
:�B
;JB
;dB
;0B
;B
:�B
:�B
:�B
;dB
:�B
<B
<B
<PB
<jB
="B
<�B
=<B
=�B
=�B
=qB
=<B
>B
?HB
@�B
@�B
A�B
BAB
BB
B'B
BAB
A;B
@OB
@ B
@OB
A�B
CB
CB
AUB
@�B
@�B
A;B
A�B
EB
EB
C�B
C{B
C{B
C�B
C�B
DMB
C�B
D�B
F�B
F�B
F�B
H�B
I�B
I�B
J#B
J=B
J�B
J�B
J�B
K)B
K)B
K)B
K^B
L0B
L0B
L0B
LJB
LdB
L~B
L�B
MPB
M�B
N"B
N<B
N<B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PB
P}B
P�B
P�B
QB
QNB
QhB
Q�B
Q�B
Q�B
R B
RB
R�B
R�B
SB
S&B
S�B
S�B
S�B
S�B
T,B
T�B
TaB
T{B
T{B
U2B
VB
V�B
V�B
V�B
V�B
W$B
W$B
WYB
WYB
W�B
W�B
XB
X+B
X_B
X�B
YB
YeB
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
\)B
\CB
\]B
\CB
[�B
[qB
[�B
[�B
[�B
[�B
\B
\�B
\�B
]B
]/B
]�B
^5B
^jB
^�B
^�B
_!B
_pB
_pB
`B
`\B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
b4B
bhB
b�B
cTB
c�B
c�B
c�B
d&B
dB
d@B
d�B
d�B
d�B
d�B
e`B
eFB
e,B
e`B
ezB
e�B
fLB
f�B
f�B
gRB
gmB
gRB
gRB
g�B
g�B
h
B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
i�B
j0B
jeB
j�B
j�B
kB
j�B
kB
kB
kQB
kkB
k�B
k�B
lB
l"B
l�B
l�B
l�B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
n}B
n�B
n�B
o5B
oOB
oiB
oOB
oiB
o�B
o�B
o�B
o�B
pB
p!B
pUB
poB
p�B
p�B
qB
q�B
q�B
q�B
q�B
raB
r|B
r�B
r�B
r�B
s3B
s3B
sMB
sMB
shB
sMB
s�B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
u?B
uZB
utB
utB
u�B
u�B
u�B
v+B
vFB
vzB
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
w�B
xlB
x�B
x�B
y	B
y>B
yrB
y�B
zDB
{dB
{0B
|B
|�B
}VB
}qB
}qB
}<B
}�B
~B
}�B
}�B
~B
}�B
~]B
~�B
~�B
~�B
~�B
B
B
B
~�B
~�B
~wB
~�B
�B
�B
�OB
��B
��B
� B
�UB
�oB
�UB
�UB
��B
��B
��B
�UB
��B
�OB
�B
�B
�iB
��B
� B
�UB
�B
��B
� B
� B
�;B
��B
��B
��B
�GB
�aB
��B
��B
�B
�B
�9B
�9B
��B
�B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�1B
��B
�B
��B
��B
��B
�7B
�lB
��B
��B
��B
��B
��B
�#B
�#B
�XB
�rB
�rB
��B
�DB
�^B
�DB
�^B
�xB
�^B
�0B
�JB
�0B
�0B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220821034219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220821034310  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220821034310  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220821034310                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220821124315  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220821124315  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220821035905                      G�O�G�O�G�O�                