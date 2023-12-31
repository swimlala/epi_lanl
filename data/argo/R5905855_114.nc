CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:31:10Z creation;2022-06-04T19:31:11Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604193110  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�����˪1   @���OC�@-I�^5?}�c�-1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP��BW��B]��Bh  Bp  Bx��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(33C*�C,  C-�fC/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @G�@w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BPG�BW{B]{Bgz�Boz�BxG�B{B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��>B��>B�#�B�WB��>BÊ>BǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB��B��B�>B�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C�C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C(�C)�RC+޸C-�C/�C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ�RCS�RCU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϽqAϺ�A϶�Aύ�Aπ A�q�A�s�A�_pA�PA�K)A�HA�C�A�B[A�A�A�?HA�<�A�<�A�:�A�9XA�7�A�7�A�7�A�7A�6zA�4�A��A��yA��fAɃ{A��`A�M6A��Aǧ�A�m]A�CaA�zDA��A�S�A��A�Q�A���A�.A�e`A��A�G�A��A�W
A�!�A�/OA���A���A���A�Z�A�A��#A�GA���A�A��A�O�A�-�A�hsA��rA��A���A�e,A��$A�aA�bA�($A�,=A�6�A���A���A�<jA�4�A�C�A�֡A��lA�,qA��A�� A��A��cA�\)A|ȴAw�At��Ao��Ak��Aj�Ah,=Af�zAf�Aa9�A\AW4nAS�AR��AQ��AL6AD�A?�WA?(�A<��A:1'A8YKA7�A5�oA4��A3@�A1�kA/N<A.�A,��A*�A)�4A(�A'��A&MA%� A$M�A#˒A#q�A#�A"QA!�A!A!-wA!�A!SA A�A!P�A"��A$��A%W�A%��A%�IA%A A$��A$d�A#~(A"�|A"�	A"a|A"N<A"�A!|�A �tA B�A��A%FAeAi�AL0A2�A�:A�A��A�sA�AS�AA�vA�RAaA	�A�!AQA��A��AM�A+A҉A�A&�AB[A�?AS�A�sA@OA�ZAx�A��A�Al�A%�A�$AOA��A
�A
��A
G�A	�A	f�A	J#A��A%FA+kA�LA��Ad�A�$A�A��A��Ab�A��A��AA�+A��A�KA�A�AC-AjA�AH�A��AخA�CAw�AaA&�A ��A �@���@�6�@�#�@�RT@�1'@��@��x@��@��@���@�v`@�b�@��q@�A�@�2a@��@��@��@��K@��@�n�@�u@�C@��@�:*@�zx@�@�Y@�C�@� �@���@�@���@�͟@��@��@�H@独@橓@��@�a@�S&@��|@��@䀝@��>@�=@�x@��@��;@��@߰�@�v`@�!-@��@�m�@�a�@��#@�b�@ڵ@�:�@��@��@�)�@�0U@�~@��@ٙ�@�e�@�s@��[@�z�@�-@�M�@�B[@��@�g�@�%@ԅ�@�W?@�O�@ҕ@�2�@�_@�c�@�q@��)@�M�@�RT@�Ĝ@·�@�s�@�Z�@�~@͏�@�F�@��y@̑ @�C�@˽�@��@ɨX@�|@��y@�4n@ǭC@Ǳ[@ǵt@��D@��@��A@ǹ�@�t�@�F�@ƭ�@�e�@��|@�Ta@�G�@��2@�@��;@� \@�֡@��A@��.@�ƨ@��:@�O@�p;@�x@��&@���@��@�^�@��,@�	�@��b@�\)@��@��@���@���@���@�,=@��]@��@���@�~�@�e,@��@���@��+@��@���@�q@���@���@�Ov@���@��h@�}�@�m]@�\�@� \@���@���@��@�i�@�@���@�J�@�%�@�a�@�33@�+@�#�@��@�b@�{J@�4�@��@��@��@�<6@���@�-@��@���@�y�@�Mj@�'�@���@�7�@�e@�@�J@���@�Z�@�ff@�J@���@��Q@���@��
@�E9@���@��m@�Xy@��f@��@�PH@��j@���@���@�A @�+@��8@���@�y>@�@�~�@�Mj@��@��@��K@��@���@�g8@�=q@�-�@� �@��@���@��@�bN@��@�2a@��9@���@�h
@�2�@��@��@��@��M@�O@�z�@�H@�:*@�'R@� �@���@��'@�s�@�Vm@�"�@��p@�u�@�1@��@�t�@�E9@��<@��A@�,=@�1@�ݘ@��~@�{J@�a�@�@O@��@���@���@�`�@�<�@��@�  @��d@��C@���@��@���@�i�@�>B@�+k@��@��T@���@�a@��@���@���@�H@��@���@���@�K�@�f�@�;d@�-w@�$t@���@��A@�S�@�1�@��@���@�
=@��@�A�@��@���@�X@�W?@�RT@�&@���@���@�>B@��@���@�e�@��@���@�i�@���@�e,@�%F@�Y@�ی@�bN@�,=@���@���@��@�X@�q@���@��_@�{�@��@��@�!�@���@��R@���@���@��O@���@���@�a|@�=q@\)@~l�@~O@}[W@}%@|�O@|�@z��@z�1@z_�@y�@y*0@x�[@xS�@w��@w�q@wl�@v��@v�r@u��@u�7@u^�@uT�@u/@u!�@t��@t$@t�@s.I@r��@r4@q�#@q��@qA @q�@p��@p��@p��@p�z@p�o@pr�@pq@pq@p]d@p9X@o�F@n�@n�r@nW�@n-@m�T@m�"@m*0@l�@k��@kqv@k8@j�y@jkQ@jQ@jB[@i�o@i<6@h|�@gK�@f҉@fR�@e�=@e+�@d��@d	�@c;d@co@c@be@aDg@`�@`�Y@`|�@`*�@_s@^�@]��@]��@\��@\��@\|�@\C-@[�[@Z�!@Z@Y@Y�7@Y2a@X�@Xoi@X4n@W�]@W��@WX�@W>�@W�@V��@V^5@Uc�@U+@T��@T��@T�@S�r@Sخ@Sl�@R��@R��@R{@Q�@Qp�@P��@PtT@P>B@P$@PM@O�@Oo�@N��@N}V@Nu@MO�@M/@M@M�@L֡@LtT@K� @Kb�@K;d@K�@J��@J��@J@�@I��@Ie,@I�@H�z@H �@G��@G&@F�2@F�@FTa@F)�@E��@EY�@E�@D�@D[�@D�@C˒@C�$@Cg�@C
=@B�h@B��@B��@Bu%@B-@A��@A�@A��@Af�@@�E@@S�@@@?��@?dZ@?W?@?�@>�B@>�1@=�j@=m]@=0�@=�@<�@<w�@;�@;��@;e�@;'�@:�R@:V@:0U@:�@9��@9�@9�n@9p�@8��@8[�@8D�@87�@8~@7��@7P�@7�@6��@6�!@6~�@6�@5��@5Dg@4�@4�o@4/�@3�@3C@2��@2}V@2.�@2!�@1�H@1��@1��@17L@0��@0�@0`�@0D�@0:�@0�@/��@.�@.��@.{�@.z@.=q@.
�@-w2@,��@,:�@+�;@+�@+{J@+g�@+a@+a@+P�@+@*��@*��@*z@*R�@)�>@)��@)hs@)A @)/@)�@(��@(��@(��@(�@(�@(��@(��@(tT@(M@'��@'�@'t�@']�@'U�@'Mj@'K�@'A�@'33@&�c@&�}@&R�@&#:@%��@%�@%�T@%��@%��@%c@%O�@%-w@$��@$��@$j@$7@#˒@#��@#b�@"�@"��@"a|@"�@!ԕ@!�3@!�@!Q�@ �`@ ��@ g8@ Q�@ �@��@�6@�@~�@X�@S@�'@xl@c @L0@J@ԕ@�@�X@�"@x�@Y�@?}@�	@�$@�u@r�@V�@<�@$@�@��@�{@;d@Y@��@�L@��@@�3@p�@�P@��@�Y@y>@w�@Z@M@�q@��@g�@�@�<@p;@E�@-@��@ϫ@��@�"@o @Y�@O�@?}@�@��@j@V�@>B@M@�&@�0@��@b�@�@�R@s�@Z�@-@�@�@��@�@�@N<@-w@ی@K^@�@�}@��@U�@�@�M@͟@�6@�x@z@C�@�@w2@Dg@%F@�@�@�z@�D@_@D�@$@�W@�0@�[@�V@�:@��@�{@�f@�	@{J@t�@v`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AϽqAϺ�A϶�Aύ�Aπ A�q�A�s�A�_pA�PA�K)A�HA�C�A�B[A�A�A�?HA�<�A�<�A�:�A�9XA�7�A�7�A�7�A�7A�6zA�4�A��A��yA��fAɃ{A��`A�M6A��Aǧ�A�m]A�CaA�zDA��A�S�A��A�Q�A���A�.A�e`A��A�G�A��A�W
A�!�A�/OA���A���A���A�Z�A�A��#A�GA���A�A��A�O�A�-�A�hsA��rA��A���A�e,A��$A�aA�bA�($A�,=A�6�A���A���A�<jA�4�A�C�A�֡A��lA�,qA��A�� A��A��cA�\)A|ȴAw�At��Ao��Ak��Aj�Ah,=Af�zAf�Aa9�A\AW4nAS�AR��AQ��AL6AD�A?�WA?(�A<��A:1'A8YKA7�A5�oA4��A3@�A1�kA/N<A.�A,��A*�A)�4A(�A'��A&MA%� A$M�A#˒A#q�A#�A"QA!�A!A!-wA!�A!SA A�A!P�A"��A$��A%W�A%��A%�IA%A A$��A$d�A#~(A"�|A"�	A"a|A"N<A"�A!|�A �tA B�A��A%FAeAi�AL0A2�A�:A�A��A�sA�AS�AA�vA�RAaA	�A�!AQA��A��AM�A+A҉A�A&�AB[A�?AS�A�sA@OA�ZAx�A��A�Al�A%�A�$AOA��A
�A
��A
G�A	�A	f�A	J#A��A%FA+kA�LA��Ad�A�$A�A��A��Ab�A��A��AA�+A��A�KA�A�AC-AjA�AH�A��AخA�CAw�AaA&�A ��A �@���@�6�@�#�@�RT@�1'@��@��x@��@��@���@�v`@�b�@��q@�A�@�2a@��@��@��@��K@��@�n�@�u@�C@��@�:*@�zx@�@�Y@�C�@� �@���@�@���@�͟@��@��@�H@独@橓@��@�a@�S&@��|@��@䀝@��>@�=@�x@��@��;@��@߰�@�v`@�!-@��@�m�@�a�@��#@�b�@ڵ@�:�@��@��@�)�@�0U@�~@��@ٙ�@�e�@�s@��[@�z�@�-@�M�@�B[@��@�g�@�%@ԅ�@�W?@�O�@ҕ@�2�@�_@�c�@�q@��)@�M�@�RT@�Ĝ@·�@�s�@�Z�@�~@͏�@�F�@��y@̑ @�C�@˽�@��@ɨX@�|@��y@�4n@ǭC@Ǳ[@ǵt@��D@��@��A@ǹ�@�t�@�F�@ƭ�@�e�@��|@�Ta@�G�@��2@�@��;@� \@�֡@��A@��.@�ƨ@��:@�O@�p;@�x@��&@���@��@�^�@��,@�	�@��b@�\)@��@��@���@���@���@�,=@��]@��@���@�~�@�e,@��@���@��+@��@���@�q@���@���@�Ov@���@��h@�}�@�m]@�\�@� \@���@���@��@�i�@�@���@�J�@�%�@�a�@�33@�+@�#�@��@�b@�{J@�4�@��@��@��@�<6@���@�-@��@���@�y�@�Mj@�'�@���@�7�@�e@�@�J@���@�Z�@�ff@�J@���@��Q@���@��
@�E9@���@��m@�Xy@��f@��@�PH@��j@���@���@�A @�+@��8@���@�y>@�@�~�@�Mj@��@��@��K@��@���@�g8@�=q@�-�@� �@��@���@��@�bN@��@�2a@��9@���@�h
@�2�@��@��@��@��M@�O@�z�@�H@�:*@�'R@� �@���@��'@�s�@�Vm@�"�@��p@�u�@�1@��@�t�@�E9@��<@��A@�,=@�1@�ݘ@��~@�{J@�a�@�@O@��@���@���@�`�@�<�@��@�  @��d@��C@���@��@���@�i�@�>B@�+k@��@��T@���@�a@��@���@���@�H@��@���@���@�K�@�f�@�;d@�-w@�$t@���@��A@�S�@�1�@��@���@�
=@��@�A�@��@���@�X@�W?@�RT@�&@���@���@�>B@��@���@�e�@��@���@�i�@���@�e,@�%F@�Y@�ی@�bN@�,=@���@���@��@�X@�q@���@��_@�{�@��@��@�!�@���@��R@���@���@��O@���@���@�a|@�=q@\)@~l�@~O@}[W@}%@|�O@|�@z��@z�1@z_�@y�@y*0@x�[@xS�@w��@w�q@wl�@v��@v�r@u��@u�7@u^�@uT�@u/@u!�@t��@t$@t�@s.I@r��@r4@q�#@q��@qA @q�@p��@p��@p��@p�z@p�o@pr�@pq@pq@p]d@p9X@o�F@n�@n�r@nW�@n-@m�T@m�"@m*0@l�@k��@kqv@k8@j�y@jkQ@jQ@jB[@i�o@i<6@h|�@gK�@f҉@fR�@e�=@e+�@d��@d	�@c;d@co@c@be@aDg@`�@`�Y@`|�@`*�@_s@^�@]��@]��@\��@\��@\|�@\C-@[�[@Z�!@Z@Y@Y�7@Y2a@X�@Xoi@X4n@W�]@W��@WX�@W>�@W�@V��@V^5@Uc�@U+@T��@T��@T�@S�r@Sخ@Sl�@R��@R��@R{@Q�@Qp�@P��@PtT@P>B@P$@PM@O�@Oo�@N��@N}V@Nu@MO�@M/@M@M�@L֡@LtT@K� @Kb�@K;d@K�@J��@J��@J@�@I��@Ie,@I�@H�z@H �@G��@G&@F�2@F�@FTa@F)�@E��@EY�@E�@D�@D[�@D�@C˒@C�$@Cg�@C
=@B�h@B��@B��@Bu%@B-@A��@A�@A��@Af�@@�E@@S�@@@?��@?dZ@?W?@?�@>�B@>�1@=�j@=m]@=0�@=�@<�@<w�@;�@;��@;e�@;'�@:�R@:V@:0U@:�@9��@9�@9�n@9p�@8��@8[�@8D�@87�@8~@7��@7P�@7�@6��@6�!@6~�@6�@5��@5Dg@4�@4�o@4/�@3�@3C@2��@2}V@2.�@2!�@1�H@1��@1��@17L@0��@0�@0`�@0D�@0:�@0�@/��@.�@.��@.{�@.z@.=q@.
�@-w2@,��@,:�@+�;@+�@+{J@+g�@+a@+a@+P�@+@*��@*��@*z@*R�@)�>@)��@)hs@)A @)/@)�@(��@(��@(��@(�@(�@(��@(��@(tT@(M@'��@'�@'t�@']�@'U�@'Mj@'K�@'A�@'33@&�c@&�}@&R�@&#:@%��@%�@%�T@%��@%��@%c@%O�@%-w@$��@$��@$j@$7@#˒@#��@#b�@"�@"��@"a|@"�@!ԕ@!�3@!�@!Q�@ �`@ ��@ g8@ Q�@ �@��@�6@�@~�@X�@S@�'@xl@c @L0@J@ԕ@�@�X@�"@x�@Y�@?}@�	@�$@�u@r�@V�@<�@$@�@��@�{@;d@Y@��@�L@��@@�3@p�@�P@��@�Y@y>@w�@Z@M@�q@��@g�@�@�<@p;@E�@-@��@ϫ@��@�"@o @Y�@O�@?}@�@��@j@V�@>B@M@�&@�0@��@b�@�@�R@s�@Z�@-@�@�@��@�@�@N<@-w@ی@K^@�@�}@��@U�@�@�M@͟@�6@�x@z@C�@�@w2@Dg@%F@�@�@�z@�D@_@D�@$@�W@�0@�[@�V@�:@��@�{@�f@�	@{J@t�@v`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�mB	żB	ňB	�9B	��B	�B	�B	�9B	�mB	ŢB	�%B	��B	�+B	�B	�_B	��B	�B	��B	�B	�7B	�lB	�B	��B	�PB	��B	��B	��B
%�B�B33BW?B_�BkB{�B�0B�B��B�6B�yB�pB�8B�B��B��B�0B�B�B�IB�`B�VB�B�B�B1BVB!�BpB7B�B"BB��B�qB�AB��B��B�qB�?B�KB��Bv`BcnBA�B�B
�XB
�yB
�qB
�uB
�YB
��B
�KB
��B
w�B
h>B
C�B
	B	�>B	�SB	��B	�RB	�OB	��B	�"B	��B	yrB	iyB	YB	L0B	J=B	J�B	KDB	#TB		B	%B	uB�lB��B��B�B�TB�B�B�6B�qB��B��B	B	�B��B��B	SB	aB	$�B	,WB	3�B	49B	4�B	>B	D3B	E�B	]�B	o�B	��B	�?B	�XB
�B
TB
�B
KB
B
�B
�B
�B
EB
	B
)B
qB
�B
�B
�B
�B	�$B	�OB	�KB	چB	�CB	�kB	ںB	��B	��B
9B
�B
�B
�B
�B
EB
�B
�B
WB
�B
B
"�B
#�B
!�B
"NB
%�B
-)B
-)B
,WB
*B
'�B
&LB
$�B
!�B
�B
VB
;B
 �B
 B
�B
IB
=B
�B
B
aB
B
�B
~B
�B
�B
EB
�B
6B
�B
$B
�B
�B
B
�B
�B
pB
�B
.B
�B
pB
mB
/B
�B
)B
�B
�B
pB
jB
�B
IB
CB
#B
�B
�B
B
�B

�B
�B
+B
�B
AB
 �B
UB
{B
�B
4B
�B
�B
�B

=B

=B
dB
~B
}B
HB
6B
�B
6B
�B
�B
<B
VB
�B
�B
jB
jB
�B

rB
	�B
	�B
�B
mB
�B
�B
�B
�B
B
KB
�B
�B
�B
�B
B
�B
�B
?B
B
tB
B
B
�B
3B
GB
�B
�B
	B
	�B
	�B
	�B
�B
�B
SB
�B
9B
�B
�B
	B
	B
	RB
	7B
�B
+B
EB
tB
�B
�B
�B
�B
�B
�B
�B
 OB	�HB	�B	��B	��B	�.B
�B
�B
�B
oB
 4B	��B	�qB	�VB	�VB	�]B	�B	��B
�B
�B

�B
^B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
pB
�B
�B
�B
B
(B
�B
�B
�B
\B
(B
�B
�B
�B
�B
<B
�B
�B
)B

�B

�B

�B

XB

�B

�B
^B
^B
�B
�B
�B

�B

�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
jB
BB
}B
�B
�B
bB
}B
�B
hB
�B
hB
NB
NB
�B
 B
hB
hB
�B
�B
�B
�B
aB
�B
B
B
�B
B
�B
�B
B
9B
B
B
�B
�B

B
$B
�B
yB
�B
1B
KB
1B
1B
�B
�B
�B
�B
�B
QB
�B
�B
	B
WB
qB
�B
�B
)B
)B
B
)B
CB
)B
�B
�B
~B
B
�B
�B
�B
!B
;B
pB
�B
�B
VB
 �B
 �B
 �B
 �B
 vB
!B
 �B
 �B
 �B
 �B
!HB
!bB
 �B
!-B
!-B
!|B
!�B
!B
!�B
"�B
"B
!bB
!�B
!�B
"hB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$B
$B
#�B
#�B
#nB
#�B
#�B
#�B
$B
$tB
$ZB
%B
%`B
%,B
%,B
%�B
&LB
&�B
&�B
&LB
($B
)B
)�B
*B
*�B
+B
+B
+kB
,=B
,�B
,�B
,�B
-CB
-]B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-CB
-wB
-CB
-�B
.}B
/OB
/�B
0�B
1�B
1�B
2�B
3�B
3�B
3�B
2�B
3hB
33B
2�B
2�B
3hB
4�B
6B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
72B
72B
6�B
8B
8�B
8�B
9rB
9rB
9XB
9�B
9�B
9XB
9>B
9�B
9�B
:B
:�B
:�B
:�B
;0B
;JB
;0B
;�B
<B
<�B
=<B
=<B
=�B
=�B
=�B
>B
>wB
>wB
>�B
>�B
>�B
>�B
?B
?B
?.B
?cB
?cB
?}B
?}B
?}B
?cB
?cB
?HB
?�B
@B
@�B
@�B
@�B
AB
A;B
A�B
A�B
B�B
B�B
B[B
BuB
B[B
B[B
B'B
BuB
B[B
A�B
BuB
BAB
B�B
BuB
C-B
C{B
D�B
E9B
EmB
E9B
FYB
G+B
GzB
HB
G�B
HB
HB
IB
I�B
I�B
J=B
J=B
I�B
J	B
JrB
JrB
J#B
J#B
JrB
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K^B
KxB
K^B
K�B
L�B
L�B
L�B
L�B
MPB
M6B
M6B
M�B
M�B
NB
NpB
N�B
N�B
OB
O�B
O�B
O�B
OvB
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
SB
S@B
S[B
S�B
S�B
S�B
T{B
T�B
T�B
UgB
U�B
V9B
VSB
V�B
V�B
V�B
W?B
WsB
W�B
W�B
XB
XEB
X_B
X_B
X_B
X�B
X�B
X�B
X�B
X�B
YB
YB
Y1B
YB
Y1B
YeB
Y�B
Y�B
ZQB
Z7B
ZB
ZQB
Z�B
ZkB
[#B
[WB
[�B
[�B
\)B
\�B
\�B
]B
]/B
]/B
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_pB
_�B
`B
`B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
bB
bB
b�B
c B
cTB
c�B
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
e,B
e,B
eB
e,B
ezB
e�B
e�B
fB
fB
fLB
fLB
f�B
g8B
g�B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iB
i*B
i*B
iDB
i�B
jB
jKB
jeB
jeB
j�B
j�B
kB
kB
kB
kB
kB
kB
k6B
kB
l"B
lB
l"B
l=B
lWB
l=B
l=B
l=B
l=B
l�B
l�B
mCB
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nB
ncB
n}B
n�B
n�B
oOB
oiB
o5B
o�B
oOB
oiB
o�B
pB
p;B
poB
p�B
q'B
q[B
q�B
q�B
q�B
r-B
rGB
r�B
r|B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t9B
tTB
t�B
t�B
t�B
uB
t�B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
v�B
w2B
w2B
w�B
w�B
xB
x�B
x�B
y	B
y$B
y$B
y$B
yrB
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
{B
{dB
{dB
{B
{�B
|B
|PB
|�B
}B
}qB
}qB
}qB
}�B
}�B
}VB
}qB
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
�iB
�iB
��B
��B
��B
� B
�;B
�;B
�;B
�oB
��B
�B
�'B
�AB
�[B
��B
��B
�B
�B
�aB
�{B
��B
��B
��B
�B
�3B
�3B
�MB
�3B
�3B
�MB
�gB
�gB
�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�mB	żB	ňB	�9B	��B	�B	�B	�9B	�mB	ŢB	�%B	��B	�+B	�B	�_B	��B	�B	��B	�B	�7B	�lB	�B	��B	�PB	��B	��B	��B
%�B�B33BW?B_�BkB{�B�0B�B��B�6B�yB�pB�8B�B��B��B�0B�B�B�IB�`B�VB�B�B�B1BVB!�BpB7B�B"BB��B�qB�AB��B��B�qB�?B�KB��Bv`BcnBA�B�B
�XB
�yB
�qB
�uB
�YB
��B
�KB
��B
w�B
h>B
C�B
	B	�>B	�SB	��B	�RB	�OB	��B	�"B	��B	yrB	iyB	YB	L0B	J=B	J�B	KDB	#TB		B	%B	uB�lB��B��B�B�TB�B�B�6B�qB��B��B	B	�B��B��B	SB	aB	$�B	,WB	3�B	49B	4�B	>B	D3B	E�B	]�B	o�B	��B	�?B	�XB
�B
TB
�B
KB
B
�B
�B
�B
EB
	B
)B
qB
�B
�B
�B
�B	�$B	�OB	�KB	چB	�CB	�kB	ںB	��B	��B
9B
�B
�B
�B
�B
EB
�B
�B
WB
�B
B
"�B
#�B
!�B
"NB
%�B
-)B
-)B
,WB
*B
'�B
&LB
$�B
!�B
�B
VB
;B
 �B
 B
�B
IB
=B
�B
B
aB
B
�B
~B
�B
�B
EB
�B
6B
�B
$B
�B
�B
B
�B
�B
pB
�B
.B
�B
pB
mB
/B
�B
)B
�B
�B
pB
jB
�B
IB
CB
#B
�B
�B
B
�B

�B
�B
+B
�B
AB
 �B
UB
{B
�B
4B
�B
�B
�B

=B

=B
dB
~B
}B
HB
6B
�B
6B
�B
�B
<B
VB
�B
�B
jB
jB
�B

rB
	�B
	�B
�B
mB
�B
�B
�B
�B
B
KB
�B
�B
�B
�B
B
�B
�B
?B
B
tB
B
B
�B
3B
GB
�B
�B
	B
	�B
	�B
	�B
�B
�B
SB
�B
9B
�B
�B
	B
	B
	RB
	7B
�B
+B
EB
tB
�B
�B
�B
�B
�B
�B
�B
 OB	�HB	�B	��B	��B	�.B
�B
�B
�B
oB
 4B	��B	�qB	�VB	�VB	�]B	�B	��B
�B
�B

�B
^B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
pB
�B
�B
�B
B
(B
�B
�B
�B
\B
(B
�B
�B
�B
�B
<B
�B
�B
)B

�B

�B

�B

XB

�B

�B
^B
^B
�B
�B
�B

�B

�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
jB
BB
}B
�B
�B
bB
}B
�B
hB
�B
hB
NB
NB
�B
 B
hB
hB
�B
�B
�B
�B
aB
�B
B
B
�B
B
�B
�B
B
9B
B
B
�B
�B

B
$B
�B
yB
�B
1B
KB
1B
1B
�B
�B
�B
�B
�B
QB
�B
�B
	B
WB
qB
�B
�B
)B
)B
B
)B
CB
)B
�B
�B
~B
B
�B
�B
�B
!B
;B
pB
�B
�B
VB
 �B
 �B
 �B
 �B
 vB
!B
 �B
 �B
 �B
 �B
!HB
!bB
 �B
!-B
!-B
!|B
!�B
!B
!�B
"�B
"B
!bB
!�B
!�B
"hB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$B
$B
#�B
#�B
#nB
#�B
#�B
#�B
$B
$tB
$ZB
%B
%`B
%,B
%,B
%�B
&LB
&�B
&�B
&LB
($B
)B
)�B
*B
*�B
+B
+B
+kB
,=B
,�B
,�B
,�B
-CB
-]B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-CB
-wB
-CB
-�B
.}B
/OB
/�B
0�B
1�B
1�B
2�B
3�B
3�B
3�B
2�B
3hB
33B
2�B
2�B
3hB
4�B
6B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
72B
72B
6�B
8B
8�B
8�B
9rB
9rB
9XB
9�B
9�B
9XB
9>B
9�B
9�B
:B
:�B
:�B
:�B
;0B
;JB
;0B
;�B
<B
<�B
=<B
=<B
=�B
=�B
=�B
>B
>wB
>wB
>�B
>�B
>�B
>�B
?B
?B
?.B
?cB
?cB
?}B
?}B
?}B
?cB
?cB
?HB
?�B
@B
@�B
@�B
@�B
AB
A;B
A�B
A�B
B�B
B�B
B[B
BuB
B[B
B[B
B'B
BuB
B[B
A�B
BuB
BAB
B�B
BuB
C-B
C{B
D�B
E9B
EmB
E9B
FYB
G+B
GzB
HB
G�B
HB
HB
IB
I�B
I�B
J=B
J=B
I�B
J	B
JrB
JrB
J#B
J#B
JrB
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K^B
KxB
K^B
K�B
L�B
L�B
L�B
L�B
MPB
M6B
M6B
M�B
M�B
NB
NpB
N�B
N�B
OB
O�B
O�B
O�B
OvB
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
SB
S@B
S[B
S�B
S�B
S�B
T{B
T�B
T�B
UgB
U�B
V9B
VSB
V�B
V�B
V�B
W?B
WsB
W�B
W�B
XB
XEB
X_B
X_B
X_B
X�B
X�B
X�B
X�B
X�B
YB
YB
Y1B
YB
Y1B
YeB
Y�B
Y�B
ZQB
Z7B
ZB
ZQB
Z�B
ZkB
[#B
[WB
[�B
[�B
\)B
\�B
\�B
]B
]/B
]/B
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
_pB
_�B
`B
`B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
bB
bB
b�B
c B
cTB
c�B
c�B
c�B
d@B
dZB
dZB
d�B
d�B
d�B
e,B
e,B
eB
e,B
ezB
e�B
e�B
fB
fB
fLB
fLB
f�B
g8B
g�B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iB
i*B
i*B
iDB
i�B
jB
jKB
jeB
jeB
j�B
j�B
kB
kB
kB
kB
kB
kB
k6B
kB
l"B
lB
l"B
l=B
lWB
l=B
l=B
l=B
l=B
l�B
l�B
mCB
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nB
ncB
n}B
n�B
n�B
oOB
oiB
o5B
o�B
oOB
oiB
o�B
pB
p;B
poB
p�B
q'B
q[B
q�B
q�B
q�B
r-B
rGB
r�B
r|B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t9B
t9B
tTB
t�B
t�B
t�B
uB
t�B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
vB
vzB
v�B
v�B
v�B
w2B
w2B
w�B
w�B
xB
x�B
x�B
y	B
y$B
y$B
y$B
yrB
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
{B
{dB
{dB
{B
{�B
|B
|PB
|�B
}B
}qB
}qB
}qB
}�B
}�B
}VB
}qB
}�B
}�B
~B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
�B
�iB
�iB
��B
��B
��B
� B
�;B
�;B
�;B
�oB
��B
�B
�'B
�AB
�[B
��B
��B
�B
�B
�aB
�{B
��B
��B
��B
�B
�3B
�3B
�MB
�3B
�3B
�MB
�gB
�gB
�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193110  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193111  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193111                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043119  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043119  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                