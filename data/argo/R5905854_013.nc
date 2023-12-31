CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:46:41Z creation;2022-06-04T17:46:41Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174641  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @����t�1   @��+l��@-yXbM��d�+1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B
  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BfffBp  Bx  B�33B���B���B���B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C33C  C  C�C��C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� DzfDz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@w�@��
@���A�A=�A]�A}�A���A���A���A���A���A���A���A���B	z�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Be�GBoz�Bwz�B�GB��>B��>B��>B��qB��qB��qB��qB��qB��B�WB��qB��qB��qB��qB��>B��qBýqBǽqB˽qBϽqBӽqB�#�B۽qBߊ>B�qB�qB�qB�qB�>B��qB��qB��qC޸C޸C޸C޸C	޸C�C޸C޸C�RC��C�C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9�RC;�RC=޸C?޸CA޸CC�CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu�RCw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DM~DM�DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy�Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�רA�רA�֡A��gA�רA�ٴA�یA���A��]A��A���A���A�� A���A�ŢAئ�A��Aׯ�A҅SA�kQA�2�AΗ�A�m�A�>wAͬA�'�A���A�.A˵?A���A�E�Aɯ�A���A��.A�x�A�;dA��lA��A�)�A�M�Aú�A��A��BA���A�Y�A�&LA�8�A��vA�#A�<A�]/A� �A�n/A��A���A�gA��A�SA��A��mA�7�A�e`A�'�A��BA��	A�_�A�_�A��A�CaA�u�A���A�VA�S[A�{�A�8�A�/�A��!A�C�A�pA���A��SA�y	A�  A��A��A�<�A��VA�<6A��A��A�@A���A�NA}�YAz�hAu��Ap�WAn�Ai	�Ae�&Ad�,AcdZA`�aA^�FA]�)AX��AU�AQ�AOPHANA�AMS�ALYAK)�AI��AGZ�AF:*AB��A@V�A<�9A:#:A8MA6��A5��A5A3�MA3�A2��A2  A0H�A.ԕA.��A-�"A+cA)�$A&��A%��A%A$o A#��A#��A#��A#�A"��A!��A %A�A��AhsA�A��A(�A;A�A?A�A��A��AU�A��A�AD�A1A��A@A�A �A�Au%A��A:�A+AI�A�2AN�AA��A33A�1A|�A?}A�AA��AF�A�A�A��A��Ae�A��ASA!�A�A
��A	��A	��A	0UA�nAVAb�A;A��AA�@A6zA�cA��AMA��A�AA+A�A)�A�A�PA"hA � A %F@��M@���@�o @�.I@��@���@�$�@�1�@�N�@�9�@��}@�*0@��@��3@�Y�@��]@�]d@��9@��@�6�@�[W@��@�U2@��)@�u%@�v`@��5@�x@���@�e@�4@�ff@�U�@��@���@�@�@�F�@�!@�tT@�x@�|@�.I@�%F@��@��@߁@��K@ރ@���@�t�@�@�a|@���@ۺ^@�;@څ�@�M@�l�@���@ش9@�B[@�_@��@��@��@׸�@�;d@֧@�u�@�خ@�p�@�8�@���@Խ<@��@��@ҷ�@�;�@���@ѱ[@��@���@ЂA@��@�*0@��]@Ό@�#:@ͿH@�N<@�͟@��W@�&�@�?�@��@ɲ-@�}�@�Z�@�E9@�,�@�@ȇ+@�ԕ@�?}@ƪe@�Ft@���@�X�@�.I@�"�@�Y@��@�YK@æ�@��H@�Ta@���@��4@�P�@��@���@�1�@���@��@�m�@�9X@��@��S@�'�@��.@��@�b�@��h@�3�@�~@���@��Q@���@�RT@��@��@�c @�	@���@���@��@��[@�N<@���@��4@�GE@���@�RT@�(@��@�~@��)@���@�4�@��R@�h
@���@��6@���@�?}@�֡@�ȴ@�Ĝ@���@���@���@�zx@�33@�!�@���@��/@���@�B[@��@��@@�[W@�>�@�q@��s@�n�@���@���@�IR@��R@�bN@���@�qv@�`B@��@��[@��o@��@�G@���@�e,@�ѷ@��r@�J@�x�@�'�@���@���@�PH@��@�Y@�\�@��W@�j�@�@@��e@�b@��F@�;d@��@���@��$@��z@��D@�~�@�\�@�C-@�!�@���@��P@�Vm@��"@�~�@�e�@�+k@��@���@���@�7L@��B@�W�@��r@���@���@�s@�L�@�8@��@���@�r�@�$�@���@��C@��V@���@�x�@���@���@���@�Vm@�C@��@��@��{@�C�@� \@��	@���@��9@�b@�t�@�U�@��@��P@���@��)@���@�r�@�D�@�)�@�"h@��)@���@���@�j@�!�@���@���@�a|@��@��3@�n/@�C�@�~(@�!�@��Z@��3@�l�@�'�@��@�;�@��@���@���@�Dg@��@�"�@�@��I@��Y@�u�@�[�@�-�@��@��~@�2a@�$t@���@�Z�@��D@�@��@�IR@��@���@��@��x@�a|@�E�@�9X@��@��@��f@�Q�@�/�@��@��@��4@�n�@�PH@�5?@���@��#@���@���@�A�@�/�@�)_@��@��@��@�Ɇ@���@�Q@�2�@�
�@��@��@s@C�@�@~ں@~�+@~5?@~@}�j@}`B@|y>@|,=@|!@{��@{.I@z�@z�@y\�@x�E@xQ�@w�a@wH�@w�@v�h@v@u�t@uS&@t�Y@t�@s33@r��@rGE@q��@q��@q%@p�$@pM@o��@on/@o33@n��@nYK@m��@mO�@m�@l��@lFt@k�A@k�q@j�M@i�Z@ia�@iq@h��@h�[@h��@hH@gb�@f�@f@e�h@e&�@d��@dD�@cy�@b�H@bi�@a��@aQ�@`�@`U2@_�
@_�k@_�@^�@^��@^��@^l�@^\�@^GE@^O@]�@\�?@\4n@\~@\�@\�@[�w@[y�@[e�@[A�@Z�"@Z{�@Y�C@Y0�@X��@X��@X~@W�@W!-@V�c@V�F@V)�@U��@U�H@UT�@Tی@T�p@T�@T��@Tm�@T-�@S�Q@S��@SH�@S�@R��@Rq�@R+k@R	@Q�@QrG@Q:�@Qq@Q�@Q�@P��@PN�@P-�@P%�@O�]@Oخ@O��@O��@OS�@N�y@N�1@N �@M�~@MDg@M�@Lg8@LK^@L9X@L7@Kƨ@K_p@J��@J�+@JE�@J1�@J4@I��@I�h@I*0@H��@G�w@GY@F��@Fp;@Eϫ@E�7@EVm@E;@DPH@D �@D~@C�6@Ct�@CP�@C6z@C�@B�b@BC�@B	@A��@A�@A�N@A��@AL�@@�@@�`@@j@@2�@?��@?C�@>�@>�!@>YK@>$�@=�=@=k�@=Vm@<��@<1@;�@@;9�@:�6@:L0@:	@9�@9�S@9`B@8��@8%�@7�[@6�"@6C�@6!�@5�@5a�@4�P@4��@4�@3�F@2�s@2�F@2�@2�@0�@0c�@0C-@/�m@/�
@/�6@/�a@/�[@/�4@/_p@/1�@.�h@.a|@.:*@-�Z@-j@-�@,�)@,_@+�&@+�@+�q@+�@+J#@+ i@*�B@*}V@*a|@)�@)�@)*0@(�	@(ѷ@(��@(,=@(	�@'�6@'�F@'@O@&�6@&xl@&c @&E�@&�@%�#@%�n@%}�@%Dg@%q@$��@$��@$r�@$4n@$M@#�+@#�@#�}@#��@"�!@"W�@" �@!�@!��@!|@!Q�@!q@ �|@ �[@ ��@ z�@ Z@ -�@ M@�]@��@��@]�@S@�@E�@O@�@o @2a@�@�p@PH@�@�+@�@��@~�@6z@�2@��@�@v�@Q@-@
�@�>@�S@k�@a�@7L@+@�P@��@oi@C-@7@�]@��@�@�}@�q@v`@dZ@iD@C�@�@�s@��@��@{�@C�@��@��@�@c@a�@+�@��@r�@'R@�}@��@v`@�@��@�X@�m@�1@�@�r@~�@Ta@6�@@��@�C@zx@O�@&�@�@��@��@Xy@6@�@�g@�w@��@��@�@qv@.I@�@�@�x@Ta@�@_@�D@�9@ϫ@@�=@��@|@|@rG@N<@:�@@��@��@��@oi@V�@�@�@��@˒@��@x@;d@1�@o@
�8@
�@
��@
�6@
�@
��@
Q@
C�@
J�@

�@	��@	�z@	��@	a�@	8�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�רA�רA�֡A��gA�רA�ٴA�یA���A��]A��A���A���A�� A���A�ŢAئ�A��Aׯ�A҅SA�kQA�2�AΗ�A�m�A�>wAͬA�'�A���A�.A˵?A���A�E�Aɯ�A���A��.A�x�A�;dA��lA��A�)�A�M�Aú�A��A��BA���A�Y�A�&LA�8�A��vA�#A�<A�]/A� �A�n/A��A���A�gA��A�SA��A��mA�7�A�e`A�'�A��BA��	A�_�A�_�A��A�CaA�u�A���A�VA�S[A�{�A�8�A�/�A��!A�C�A�pA���A��SA�y	A�  A��A��A�<�A��VA�<6A��A��A�@A���A�NA}�YAz�hAu��Ap�WAn�Ai	�Ae�&Ad�,AcdZA`�aA^�FA]�)AX��AU�AQ�AOPHANA�AMS�ALYAK)�AI��AGZ�AF:*AB��A@V�A<�9A:#:A8MA6��A5��A5A3�MA3�A2��A2  A0H�A.ԕA.��A-�"A+cA)�$A&��A%��A%A$o A#��A#��A#��A#�A"��A!��A %A�A��AhsA�A��A(�A;A�A?A�A��A��AU�A��A�AD�A1A��A@A�A �A�Au%A��A:�A+AI�A�2AN�AA��A33A�1A|�A?}A�AA��AF�A�A�A��A��Ae�A��ASA!�A�A
��A	��A	��A	0UA�nAVAb�A;A��AA�@A6zA�cA��AMA��A�AA+A�A)�A�A�PA"hA � A %F@��M@���@�o @�.I@��@���@�$�@�1�@�N�@�9�@��}@�*0@��@��3@�Y�@��]@�]d@��9@��@�6�@�[W@��@�U2@��)@�u%@�v`@��5@�x@���@�e@�4@�ff@�U�@��@���@�@�@�F�@�!@�tT@�x@�|@�.I@�%F@��@��@߁@��K@ރ@���@�t�@�@�a|@���@ۺ^@�;@څ�@�M@�l�@���@ش9@�B[@�_@��@��@��@׸�@�;d@֧@�u�@�خ@�p�@�8�@���@Խ<@��@��@ҷ�@�;�@���@ѱ[@��@���@ЂA@��@�*0@��]@Ό@�#:@ͿH@�N<@�͟@��W@�&�@�?�@��@ɲ-@�}�@�Z�@�E9@�,�@�@ȇ+@�ԕ@�?}@ƪe@�Ft@���@�X�@�.I@�"�@�Y@��@�YK@æ�@��H@�Ta@���@��4@�P�@��@���@�1�@���@��@�m�@�9X@��@��S@�'�@��.@��@�b�@��h@�3�@�~@���@��Q@���@�RT@��@��@�c @�	@���@���@��@��[@�N<@���@��4@�GE@���@�RT@�(@��@�~@��)@���@�4�@��R@�h
@���@��6@���@�?}@�֡@�ȴ@�Ĝ@���@���@���@�zx@�33@�!�@���@��/@���@�B[@��@��@@�[W@�>�@�q@��s@�n�@���@���@�IR@��R@�bN@���@�qv@�`B@��@��[@��o@��@�G@���@�e,@�ѷ@��r@�J@�x�@�'�@���@���@�PH@��@�Y@�\�@��W@�j�@�@@��e@�b@��F@�;d@��@���@��$@��z@��D@�~�@�\�@�C-@�!�@���@��P@�Vm@��"@�~�@�e�@�+k@��@���@���@�7L@��B@�W�@��r@���@���@�s@�L�@�8@��@���@�r�@�$�@���@��C@��V@���@�x�@���@���@���@�Vm@�C@��@��@��{@�C�@� \@��	@���@��9@�b@�t�@�U�@��@��P@���@��)@���@�r�@�D�@�)�@�"h@��)@���@���@�j@�!�@���@���@�a|@��@��3@�n/@�C�@�~(@�!�@��Z@��3@�l�@�'�@��@�;�@��@���@���@�Dg@��@�"�@�@��I@��Y@�u�@�[�@�-�@��@��~@�2a@�$t@���@�Z�@��D@�@��@�IR@��@���@��@��x@�a|@�E�@�9X@��@��@��f@�Q�@�/�@��@��@��4@�n�@�PH@�5?@���@��#@���@���@�A�@�/�@�)_@��@��@��@�Ɇ@���@�Q@�2�@�
�@��@��@s@C�@�@~ں@~�+@~5?@~@}�j@}`B@|y>@|,=@|!@{��@{.I@z�@z�@y\�@x�E@xQ�@w�a@wH�@w�@v�h@v@u�t@uS&@t�Y@t�@s33@r��@rGE@q��@q��@q%@p�$@pM@o��@on/@o33@n��@nYK@m��@mO�@m�@l��@lFt@k�A@k�q@j�M@i�Z@ia�@iq@h��@h�[@h��@hH@gb�@f�@f@e�h@e&�@d��@dD�@cy�@b�H@bi�@a��@aQ�@`�@`U2@_�
@_�k@_�@^�@^��@^��@^l�@^\�@^GE@^O@]�@\�?@\4n@\~@\�@\�@[�w@[y�@[e�@[A�@Z�"@Z{�@Y�C@Y0�@X��@X��@X~@W�@W!-@V�c@V�F@V)�@U��@U�H@UT�@Tی@T�p@T�@T��@Tm�@T-�@S�Q@S��@SH�@S�@R��@Rq�@R+k@R	@Q�@QrG@Q:�@Qq@Q�@Q�@P��@PN�@P-�@P%�@O�]@Oخ@O��@O��@OS�@N�y@N�1@N �@M�~@MDg@M�@Lg8@LK^@L9X@L7@Kƨ@K_p@J��@J�+@JE�@J1�@J4@I��@I�h@I*0@H��@G�w@GY@F��@Fp;@Eϫ@E�7@EVm@E;@DPH@D �@D~@C�6@Ct�@CP�@C6z@C�@B�b@BC�@B	@A��@A�@A�N@A��@AL�@@�@@�`@@j@@2�@?��@?C�@>�@>�!@>YK@>$�@=�=@=k�@=Vm@<��@<1@;�@@;9�@:�6@:L0@:	@9�@9�S@9`B@8��@8%�@7�[@6�"@6C�@6!�@5�@5a�@4�P@4��@4�@3�F@2�s@2�F@2�@2�@0�@0c�@0C-@/�m@/�
@/�6@/�a@/�[@/�4@/_p@/1�@.�h@.a|@.:*@-�Z@-j@-�@,�)@,_@+�&@+�@+�q@+�@+J#@+ i@*�B@*}V@*a|@)�@)�@)*0@(�	@(ѷ@(��@(,=@(	�@'�6@'�F@'@O@&�6@&xl@&c @&E�@&�@%�#@%�n@%}�@%Dg@%q@$��@$��@$r�@$4n@$M@#�+@#�@#�}@#��@"�!@"W�@" �@!�@!��@!|@!Q�@!q@ �|@ �[@ ��@ z�@ Z@ -�@ M@�]@��@��@]�@S@�@E�@O@�@o @2a@�@�p@PH@�@�+@�@��@~�@6z@�2@��@�@v�@Q@-@
�@�>@�S@k�@a�@7L@+@�P@��@oi@C-@7@�]@��@�@�}@�q@v`@dZ@iD@C�@�@�s@��@��@{�@C�@��@��@�@c@a�@+�@��@r�@'R@�}@��@v`@�@��@�X@�m@�1@�@�r@~�@Ta@6�@@��@�C@zx@O�@&�@�@��@��@Xy@6@�@�g@�w@��@��@�@qv@.I@�@�@�x@Ta@�@_@�D@�9@ϫ@@�=@��@|@|@rG@N<@:�@@��@��@��@oi@V�@�@�@��@˒@��@x@;d@1�@o@
�8@
�@
��@
�6@
�@
��@
Q@
C�@
J�@

�@	��@	�z@	��@	a�@	8�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B		B	�B	�B	�B	�B	�B	�B		RB	�B		B	�B		B		B	�B	�B	C�B	=�B	>B	BAB	C{B	EB	M6B	V�B	\xB	b4B	jB	w�B	��B	��B	�B	�B	�cB
.B
@B
�B
-�B
PB
d�B
~�B
��B
�FB
��B
��B
��B
��BdB+kB6�BG�BXB�OB��B��B�B��B�!B�B��BjB�B�BJB�.B�`B�ZB�B��BӏBѷB��B�qBx�BhXBP�B.IB
�B
�RB
�B
�B
��B
�)B
�"B
� B
��B
�B
r�B
_B
E�B
"�B
GB	��B	ݘB	��B	��B	��B	z�B	h$B	a|B	Y�B	S�B	MPB	J�B	=�B	'�B	WB	PB	�B	�B	�B�B��B�B�)B�TB�~BیB�B�,B��B�"B��B��B	  B��B��B�>B�HB	�B	AB�B�B�)B�0B�B��B	EB	�B	6FB	L~B	HfB	F%B	KxB	S&B	UMB	jeB	hsB	�aB	�kB	��B	�BB	��B	�NB	��B	� B	�?B	��B	��B	�	B	�B	��B	��B	��B	�_B	�IB	��B	�>B	�6B	�B	�JB	ɆB	ȀB	�B	�1B	��B	�B	��B	�aB	�3B	��B	�B	�B	��B	��B	�PB	�#B	�1B	�PB	�@B	ӏB	�aB	��B	�oB	�4B	�B	҉B	��B	��B	��B	� B	��B	ϫB	�pB	ΊB	ΊB	��B	�pB	ΥB	οB	�B	�VB	�vB	�vB	�VB	�jB	̈́B	�B	�BB	ΥB	�pB	�<B	͹B	��B	�dB	�dB	�dB	�B	ˬB	��B	�B	��B	�~B	��B	� B	��B	�NB	�hB	��B	��B	�B	ҽB	��B	�&B	��B	��B	֡B	��B	��B	�B	��B	��B	҉B	��B	��B	��B	�2B	ԯB	��B	��B	��B	�aB	ӏB	��B	�2B	՛B	�gB	��B	�2B	�{B	�{B	��B	ԯB	ԯB	ՁB	�B	�9B	��B	��B	ּB	�mB	�SB	�B	֡B	�SB	�9B	�B	��B	�9B	�B	ՁB	�YB	��B	�YB	רB	�EB	�B	�B	ٴB	ٴB	ڠB	�KB	�kB	��B	��B	چB	یB	ۦB	��B	�xB	��B	ܒB	��B	�xB	ܒB	ܬB	ܬB	��B	ݲB	�jB	ޞB	�;B	ߊB	��B	��B	�B	��B	��B	�B	�bB	��B	�B	�B	��B	�B	�B	�&B	��B	�,B	�FB	��B	�B	�B	��B	�B	�RB	��B	��B	�_B	�B	��B	�0B	�B	�B	��B	�B	��B	�kB	�qB	�B	��B	�B	�)B	�B	��B	�B	�B	�B	�B	��B	�;B	�UB	�'B	�'B	�B	�B	�B	�B	�B	�B	��B	�9B	�ZB	�ZB	�%B	��B	��B	��B	��B	�+B	�B	��B	��B	��B	��B	�B	�lB	��B	�XB	��B	�xB	�JB	�B	�B	�B	��B	��B	�"B	�jB	�PB	��B	�JB	�B	�PB	��B	��B	��B	��B	��B	�B	�VB	�<B	��B	��B	��B
 �B
�B
AB
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
fB
�B
�B
�B
B
�B
�B
	B
	�B
	�B
	�B
B

�B

�B

�B
)B
xB
�B
�B
�B
�B
�B
PB
�B
�B
bB
�B
�B
 B
�B
�B
NB
�B
TB
NB
hB
NB
NB
4B
B
�B
 B
�B
:B
�B
�B
FB
@B
uB
�B
�B
FB
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
QB
�B
/B
�B
5B
OB
jB
�B
B
�B
�B
VB
;B
�B
 B
 �B
 �B
!�B
#:B
#TB
#�B
#�B
#�B
$@B
$@B
$�B
%�B
%�B
&2B
'�B
'�B
(
B
(XB
(�B
(�B
(�B
(�B
)*B
)DB
)�B
*0B
*eB
*�B
+QB
+QB
+�B
+�B
+�B
,WB
,�B
,�B
,�B
-)B
-)B
-)B
./B
.}B
.cB
.cB
.}B
.}B
.�B
.}B
/ B
/�B
/�B
0!B
0;B
0oB
0UB
0�B
0�B
0�B
1B
1'B
1'B
1B
1�B
2-B
2B
1�B
2|B
2�B
2�B
33B
3�B
4B
4TB
4�B
4�B
4�B
5%B
5�B
5�B
6B
6�B
6�B
7�B
8B
8�B
8�B
8�B
9$B
9�B
9�B
9�B
:^B
:^B
:xB
;B
;�B
;�B
;�B
;�B
<PB
<PB
<6B
<�B
=B
=VB
=�B
=�B
=�B
=�B
=qB
=�B
>B
>�B
>�B
?B
?HB
?}B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C-B
C-B
CGB
CGB
CGB
CGB
CGB
C{B
DB
C�B
C�B
C�B
C�B
DB
D3B
D3B
DMB
E�B
FtB
F�B
GB
F�B
GEB
G�B
G�B
HKB
H�B
H�B
IRB
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
I�B
JrB
J�B
J�B
J�B
KB
K�B
K�B
K�B
LB
LJB
LdB
L�B
L�B
M6B
MPB
MPB
MPB
MPB
M�B
M�B
M�B
M�B
N"B
NVB
N�B
OvB
OBB
O\B
OvB
O�B
O\B
O�B
Q B
Q4B
Q4B
QNB
Q4B
QhB
QhB
Q�B
R�B
R�B
S&B
S&B
S�B
TB
T,B
TFB
T�B
TaB
TFB
T�B
T�B
UB
UB
UB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VB
VB
U�B
VSB
V9B
VSB
VmB
W�B
W�B
W�B
XB
XyB
X_B
X_B
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
Z7B
ZB
Z�B
Z�B
Z�B
[�B
\)B
[�B
\)B
\�B
\�B
]/B
]~B
]�B
^jB
^jB
^OB
^�B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`'B
`'B
`BB
`�B
`�B
`�B
aB
a|B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
eFB
eFB
e`B
eFB
e�B
ffB
ffB
f�B
f�B
f�B
f�B
gB
g8B
gmB
g�B
g�B
g�B
g�B
h$B
h$B
h>B
h>B
h>B
h�B
iyB
i�B
i�B
jB
jB
j0B
jKB
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
lB
lB
lWB
l�B
l�B
mB
l�B
mCB
mwB
m�B
m�B
n/B
n�B
n�B
n�B
n�B
n�B
n�B
oB
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
qvB
q�B
q�B
rB
rGB
r|B
raB
raB
r�B
r�B
s3B
sMB
s�B
s�B
tB
tB
tTB
tTB
t�B
t�B
t�B
t�B
u?B
u?B
uZB
u�B
vB
v`B
vFB
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
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
z*B
zDB
zDB
z^B
z^B
z^B
z�B
z�B
{JB
{dB
{B
{�B
{�B
{�B
|6B
|PB
|jB
|�B
|�B
|�B
}B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
cB
cB
�B
�B
�B
� B
�4B
�OB
�4B
�OB
�4B
�OB
�OB
�OB
�OB
��B
��B
� B
�oB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B		B	�B	�B	�B	�B	�B	�B		RB	�B		B	�B		B		B	�B	�B	C�B	=�B	>B	BAB	C{B	EB	M6B	V�B	\xB	b4B	jB	w�B	��B	��B	�B	�B	�cB
.B
@B
�B
-�B
PB
d�B
~�B
��B
�FB
��B
��B
��B
��BdB+kB6�BG�BXB�OB��B��B�B��B�!B�B��BjB�B�BJB�.B�`B�ZB�B��BӏBѷB��B�qBx�BhXBP�B.IB
�B
�RB
�B
�B
��B
�)B
�"B
� B
��B
�B
r�B
_B
E�B
"�B
GB	��B	ݘB	��B	��B	��B	z�B	h$B	a|B	Y�B	S�B	MPB	J�B	=�B	'�B	WB	PB	�B	�B	�B�B��B�B�)B�TB�~BیB�B�,B��B�"B��B��B	  B��B��B�>B�HB	�B	AB�B�B�)B�0B�B��B	EB	�B	6FB	L~B	HfB	F%B	KxB	S&B	UMB	jeB	hsB	�aB	�kB	��B	�BB	��B	�NB	��B	� B	�?B	��B	��B	�	B	�B	��B	��B	��B	�_B	�IB	��B	�>B	�6B	�B	�JB	ɆB	ȀB	�B	�1B	��B	�B	��B	�aB	�3B	��B	�B	�B	��B	��B	�PB	�#B	�1B	�PB	�@B	ӏB	�aB	��B	�oB	�4B	�B	҉B	��B	��B	��B	� B	��B	ϫB	�pB	ΊB	ΊB	��B	�pB	ΥB	οB	�B	�VB	�vB	�vB	�VB	�jB	̈́B	�B	�BB	ΥB	�pB	�<B	͹B	��B	�dB	�dB	�dB	�B	ˬB	��B	�B	��B	�~B	��B	� B	��B	�NB	�hB	��B	��B	�B	ҽB	��B	�&B	��B	��B	֡B	��B	��B	�B	��B	��B	҉B	��B	��B	��B	�2B	ԯB	��B	��B	��B	�aB	ӏB	��B	�2B	՛B	�gB	��B	�2B	�{B	�{B	��B	ԯB	ԯB	ՁB	�B	�9B	��B	��B	ּB	�mB	�SB	�B	֡B	�SB	�9B	�B	��B	�9B	�B	ՁB	�YB	��B	�YB	רB	�EB	�B	�B	ٴB	ٴB	ڠB	�KB	�kB	��B	��B	چB	یB	ۦB	��B	�xB	��B	ܒB	��B	�xB	ܒB	ܬB	ܬB	��B	ݲB	�jB	ޞB	�;B	ߊB	��B	��B	�B	��B	��B	�B	�bB	��B	�B	�B	��B	�B	�B	�&B	��B	�,B	�FB	��B	�B	�B	��B	�B	�RB	��B	��B	�_B	�B	��B	�0B	�B	�B	��B	�B	��B	�kB	�qB	�B	��B	�B	�)B	�B	��B	�B	�B	�B	�B	��B	�;B	�UB	�'B	�'B	�B	�B	�B	�B	�B	�B	��B	�9B	�ZB	�ZB	�%B	��B	��B	��B	��B	�+B	�B	��B	��B	��B	��B	�B	�lB	��B	�XB	��B	�xB	�JB	�B	�B	�B	��B	��B	�"B	�jB	�PB	��B	�JB	�B	�PB	��B	��B	��B	��B	��B	�B	�VB	�<B	��B	��B	��B
 �B
�B
AB
�B
�B
�B
�B
B
�B
B
B
�B
�B
�B
fB
�B
�B
�B
B
�B
�B
	B
	�B
	�B
	�B
B

�B

�B

�B
)B
xB
�B
�B
�B
�B
�B
PB
�B
�B
bB
�B
�B
 B
�B
�B
NB
�B
TB
NB
hB
NB
NB
4B
B
�B
 B
�B
:B
�B
�B
FB
@B
uB
�B
�B
FB
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
B
QB
�B
/B
�B
5B
OB
jB
�B
B
�B
�B
VB
;B
�B
 B
 �B
 �B
!�B
#:B
#TB
#�B
#�B
#�B
$@B
$@B
$�B
%�B
%�B
&2B
'�B
'�B
(
B
(XB
(�B
(�B
(�B
(�B
)*B
)DB
)�B
*0B
*eB
*�B
+QB
+QB
+�B
+�B
+�B
,WB
,�B
,�B
,�B
-)B
-)B
-)B
./B
.}B
.cB
.cB
.}B
.}B
.�B
.}B
/ B
/�B
/�B
0!B
0;B
0oB
0UB
0�B
0�B
0�B
1B
1'B
1'B
1B
1�B
2-B
2B
1�B
2|B
2�B
2�B
33B
3�B
4B
4TB
4�B
4�B
4�B
5%B
5�B
5�B
6B
6�B
6�B
7�B
8B
8�B
8�B
8�B
9$B
9�B
9�B
9�B
:^B
:^B
:xB
;B
;�B
;�B
;�B
;�B
<PB
<PB
<6B
<�B
=B
=VB
=�B
=�B
=�B
=�B
=qB
=�B
>B
>�B
>�B
?B
?HB
?}B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
C-B
C-B
CGB
CGB
CGB
CGB
CGB
C{B
DB
C�B
C�B
C�B
C�B
DB
D3B
D3B
DMB
E�B
FtB
F�B
GB
F�B
GEB
G�B
G�B
HKB
H�B
H�B
IRB
IB
IB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
I�B
JrB
J�B
J�B
J�B
KB
K�B
K�B
K�B
LB
LJB
LdB
L�B
L�B
M6B
MPB
MPB
MPB
MPB
M�B
M�B
M�B
M�B
N"B
NVB
N�B
OvB
OBB
O\B
OvB
O�B
O\B
O�B
Q B
Q4B
Q4B
QNB
Q4B
QhB
QhB
Q�B
R�B
R�B
S&B
S&B
S�B
TB
T,B
TFB
T�B
TaB
TFB
T�B
T�B
UB
UB
UB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VB
VB
U�B
VSB
V9B
VSB
VmB
W�B
W�B
W�B
XB
XyB
X_B
X_B
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
Z7B
ZB
Z�B
Z�B
Z�B
[�B
\)B
[�B
\)B
\�B
\�B
]/B
]~B
]�B
^jB
^jB
^OB
^�B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`'B
`'B
`BB
`�B
`�B
`�B
aB
a|B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
cB
cTB
cnB
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
eFB
eFB
e`B
eFB
e�B
ffB
ffB
f�B
f�B
f�B
f�B
gB
g8B
gmB
g�B
g�B
g�B
g�B
h$B
h$B
h>B
h>B
h>B
h�B
iyB
i�B
i�B
jB
jB
j0B
jKB
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
lB
lB
lWB
l�B
l�B
mB
l�B
mCB
mwB
m�B
m�B
n/B
n�B
n�B
n�B
n�B
n�B
n�B
oB
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
p�B
p�B
qB
q'B
q'B
qvB
q�B
q�B
rB
rGB
r|B
raB
raB
r�B
r�B
s3B
sMB
s�B
s�B
tB
tB
tTB
tTB
t�B
t�B
t�B
t�B
u?B
u?B
uZB
u�B
vB
v`B
vFB
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
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
z*B
zDB
zDB
z^B
z^B
z^B
z�B
z�B
{JB
{dB
{B
{�B
{�B
{�B
|6B
|PB
|jB
|�B
|�B
|�B
}B
}B
}<B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
cB
cB
�B
�B
�B
� B
�4B
�OB
�4B
�OB
�4B
�OB
�OB
�OB
�OB
��B
��B
� B
�oB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104940  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174641  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174641  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174641                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024648  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024648  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                