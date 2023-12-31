CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:28:06Z creation;2022-06-04T19:28:06Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604192806  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٛ�A
��1   @ٛ£Eg�@,�������d*I�^51   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�33A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�33B�ffB�  B�  B�  B�  B���B�  B�33B���B�33B�  B�  B�  B�  B�  B�  B�33B㙚B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C33C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C633C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߃3D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @G�@w�@��
@��
AQ�A=�A]�A}�A���A���A���A���A�(�A�(�A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��B��B��qB��B�#�B��qB��qB��qB��qB��>B��qB��B�WB��BǽqB˽qBϽqBӽqB׽qB۽qB��B�WB�>B�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C�C�C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3�RC6�C7޸C9�C;�C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc�Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF�DG~DG�DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�
D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�b=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� �A�MA��.A��rA�ʌA�`vA�IRA�2-A�1A�hA��A��A��DA��%A�� A��A��A��A��vA���A��HA�ߤA���A�ܒA��;A��|A���A��A���A��&A��8A��A��A���A���A�\�Aֻ�A֔�A�k�A�S[AӕMAѢ4A�H�A�LdA̧�AɃGA�l�A��AŭwA�o A�A��\A�~�A��A�G�A�HA��dA�u�A��A�|�A���A��4A� �A��VA�Z�A�ƨA�xA���A�MjA��/A���A�ɺA�=qA��!A��/A� \A�G�A��A�B�A�5�A�5?A��A��A��A��XA��A�\]A�A���A�(XA�d�A� 'A�A�_A�1A�D�A~p�A{�Ax��Av"hApaAn��An=�Ak�MAdCA`'RAWq�AQ��AOd�ANFAM;AI2�AD@AA1�A?�mA>@OA<��A<C-A;SA9ɆA9V�A7m�A6($A5��A3˒A2c�A2D�A1��A0��A/IRA.��A.l�A-��A-bNA,��A+��A+9XA*W�A)�=A(VA'��A'Q�A'#�A&��A%u%A$($A!��A!(A �)A |�A A!AeAe�AA�A�A�A��As�A��A=qA%�A�A��A��A!�A	lA�pAQ�Ah�A�'A�A�Al"A�A�-A�VA��Aa�A�oA�7A�A�3A�A��A\)A%FA�Ag�A�bA��AiDA7LA͟AuA
��A
�DA
�A#�A  A
v`A	ѷA	>�A�AZA�AsA�A/A��A��A��AOAA�HA�CA�PAU2A:*A�]A��A�AݘA�}A�CA��A<�A�A��A($A�AA ��A ��A R�@���@��[@���@�tT@�2�@��K@�L�@�I�@�a@��1@��@�J#@���@�*0@�g8@���@��@�~�@��@�33@��@�@�0U@��m@�"@��]@�@��r@�K�@���@�ی@���@�Q�@�@�@�@�7@��@�|@�@�Z@�!@崢@�u@��`@ᦵ@�p�@���@��@��@���@߿H@�iD@��@ޞ�@��@ݠ'@�c@�q@�w�@���@�$t@ڑ�@�+k@ٲ�@��@ر�@�Q@�n/@�s�@՞�@�:�@��@��@Ԭ@�-�@ӷ@��P@ҙ1@�H@��@ъ	@�
=@ЯO@�z@��@Ϯ�@π4@�33@��@���@�C-@͟V@�P�@̉�@��m@�C�@��P@�,=@ɔ�@�$t@Ȋr@�~@�ϫ@ǘ�@�A�@��@��s@�A�@�J@���@�_p@�ѷ@��@ù�@Ù�@�33@�%@�tT@��#@�$t@���@��'@���@�Z�@���@�~�@�7L@��\@�@��W@��@�E9@���@��1@�	@�?}@�*�@��	@�9�@���@�|�@� �@��)@���@���@���@���@��@@���@�o @�hs@�\)@�K�@�IR@�/@��}@��@���@���@�m�@�+k@���@�@O@��@�a|@��D@�iD@�1�@��@��@�y>@�K^@���@��M@��r@���@�w�@�O@��@���@�_p@�;d@��@���@��1@�ff@�9X@��.@�c@��@��L@�=q@���@��n@���@�qv@�K�@��@��@��e@�ff@�V�@�R�@�R�@�Ov@�C-@�,=@���@�X�@���@��@�q�@�-@��@��A@��3@�4@�V@�ݘ@���@��@�Y�@�5�@��@���@�g8@�@�ݘ@��*@�`B@�F@��@��@��@��u@�bN@�	@���@��:@�a�@��@�M�@��)@�]�@��@���@��@�Ft@��@��3@���@�a@�O�@�!-@��f@��,@���@�_�@�u@��@�%F@���@�	@��@��6@���@�e�@��@�҉@��A@�V�@�-@�@��@���@��h@�?}@��<@�v�@�\�@�'R@���@��H@�Vm@�;d@�@��2@��6@�l�@�Ta@�C�@�_@�a@��@��,@���@�a|@�1�@��W@���@��{@�`B@�33@���@��h@�R�@�)�@��A@��a@�L�@���@��@�$�@��g@�y�@�8�@��@�z�@�:�@��T@��X@�o�@�!-@���@��x@�}V@�v�@��@��&@�v`@�E9@�@���@���@�|�@�YK@�!@���@���@�ԕ@���@�\)@��M@��A@��@��d@���@�Y�@�ی@��A@�?@��+@���@��~@�N<@��@��u@�;�@��@�@~�b@}�o@|�`@|�.@|PH@|H@|,=@z�8@y�C@y#�@x��@x!@wn/@w�@u�X@u��@u8�@t��@tI�@s�&@s��@sW?@s.I@s�@sC@s$t@s&@r�s@q�j@q#�@pm�@p	�@o�:@o{J@oe�@oK�@n�8@n�s@nZ�@m�T@m�C@m�@l�@l��@l�@l	�@k�@k@O@j��@jh
@j($@i�9@i��@i��@iu�@iX@h��@h��@hoi@h?�@h(�@g��@gP�@f�@fl�@e��@e�S@eL�@d��@d'R@c�@c�w@c\)@cP�@bߤ@bxl@ba|@a�t@a|@`�K@`1'@_W?@^�,@^� @^@]�@][W@\�@\�@[��@[!-@Z�2@Z�!@Zl�@Z@Yc@X��@Xu�@XD�@W�$@W�@V�@Va|@U�@U�@Ue,@U0�@Uq@T�K@TS�@Sخ@Sqv@S
=@R�b@Rff@RO@Q�)@Q��@Q-w@Q�@P�@P��@P�@P�@P�u@P|�@P>B@PG@P  @O�6@O@O@N��@NR�@NB[@N-@Ne@N_@M��@M��@MQ�@L�@L�@L�I@L��@L�@K�@Kl�@K8@J�L@JB[@J�@I��@I��@Ic@H�v@G��@G/�@F��@F{�@F�@E�@E�@E�C@Ec@E`B@EO�@Eq@D�@D��@D<�@D�@C�@C�&@C��@C��@Co�@C33@B��@BW�@A��@A��@A��@Am]@Ac�@AS&@AF@A4@A(�@@��@@��@@C-@@�@?��@?X�@?+@>��@>�r@=��@=j@=�@<Ɇ@<r�@;�g@;U�@:�@:��@:�+@:V@:&�@9�D@9|@9�@8��@8`�@82�@8 �@8 �@8M@7�m@7~�@7RT@7�@6�2@6}V@6M�@6J@5��@5F@5@4�v@4��@4I�@4?�@4"h@3�A@3��@38@3/�@333@3>�@3A�@3@O@2�@2-@1�d@1�@1\�@1+�@0�@07�@/�V@/n/@/iD@/O@.�@.��@.z@.c @.B[@-��@-�j@-x�@-+�@-%@,�e@,c�@,Xy@,�@+�@@+�f@+Mj@+�@*�'@*�}@*�1@*u%@)��@)^�@(�@(`�@(:�@(�@'��@'~�@'o�@'s@'�@&�'@&�L@&1�@%�D@%��@%�"@$�/@$x@#6z@#�@#Y@"��@"=q@!�@!f�@ ��@ m�@ >B@ "h@�@ߤ@h
@�@��@�n@��@x�@zx@k�@�p@:�@�@C@�\@\�@3�@&�@
�@�X@��@f�@�@�Y@N�@Ft@1'@�@�@�+@�}@��@�k@�@�y@��@�F@u%@L0@�-@F@ѷ@Xy@�@��@�@@|�@K�@�@H�@�@�~@=�@��@r�@I�@D�@<�@2�@2�@-�@*�@@�K@RT@�M@��@��@v�@C�@&�@�@��@��@�S@k�@\�@5�@��@�@�@�@��@�D@bN@S�@Ft@'R@M@x@�@�@�P@]�@�@
�@
��@
d�@
5?@
�@
u@	�#@	��@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� �A�MA��.A��rA�ʌA�`vA�IRA�2-A�1A�hA��A��A��DA��%A�� A��A��A��A��vA���A��HA�ߤA���A�ܒA��;A��|A���A��A���A��&A��8A��A��A���A���A�\�Aֻ�A֔�A�k�A�S[AӕMAѢ4A�H�A�LdA̧�AɃGA�l�A��AŭwA�o A�A��\A�~�A��A�G�A�HA��dA�u�A��A�|�A���A��4A� �A��VA�Z�A�ƨA�xA���A�MjA��/A���A�ɺA�=qA��!A��/A� \A�G�A��A�B�A�5�A�5?A��A��A��A��XA��A�\]A�A���A�(XA�d�A� 'A�A�_A�1A�D�A~p�A{�Ax��Av"hApaAn��An=�Ak�MAdCA`'RAWq�AQ��AOd�ANFAM;AI2�AD@AA1�A?�mA>@OA<��A<C-A;SA9ɆA9V�A7m�A6($A5��A3˒A2c�A2D�A1��A0��A/IRA.��A.l�A-��A-bNA,��A+��A+9XA*W�A)�=A(VA'��A'Q�A'#�A&��A%u%A$($A!��A!(A �)A |�A A!AeAe�AA�A�A�A��As�A��A=qA%�A�A��A��A!�A	lA�pAQ�Ah�A�'A�A�Al"A�A�-A�VA��Aa�A�oA�7A�A�3A�A��A\)A%FA�Ag�A�bA��AiDA7LA͟AuA
��A
�DA
�A#�A  A
v`A	ѷA	>�A�AZA�AsA�A/A��A��A��AOAA�HA�CA�PAU2A:*A�]A��A�AݘA�}A�CA��A<�A�A��A($A�AA ��A ��A R�@���@��[@���@�tT@�2�@��K@�L�@�I�@�a@��1@��@�J#@���@�*0@�g8@���@��@�~�@��@�33@��@�@�0U@��m@�"@��]@�@��r@�K�@���@�ی@���@�Q�@�@�@�@�7@��@�|@�@�Z@�!@崢@�u@��`@ᦵ@�p�@���@��@��@���@߿H@�iD@��@ޞ�@��@ݠ'@�c@�q@�w�@���@�$t@ڑ�@�+k@ٲ�@��@ر�@�Q@�n/@�s�@՞�@�:�@��@��@Ԭ@�-�@ӷ@��P@ҙ1@�H@��@ъ	@�
=@ЯO@�z@��@Ϯ�@π4@�33@��@���@�C-@͟V@�P�@̉�@��m@�C�@��P@�,=@ɔ�@�$t@Ȋr@�~@�ϫ@ǘ�@�A�@��@��s@�A�@�J@���@�_p@�ѷ@��@ù�@Ù�@�33@�%@�tT@��#@�$t@���@��'@���@�Z�@���@�~�@�7L@��\@�@��W@��@�E9@���@��1@�	@�?}@�*�@��	@�9�@���@�|�@� �@��)@���@���@���@���@��@@���@�o @�hs@�\)@�K�@�IR@�/@��}@��@���@���@�m�@�+k@���@�@O@��@�a|@��D@�iD@�1�@��@��@�y>@�K^@���@��M@��r@���@�w�@�O@��@���@�_p@�;d@��@���@��1@�ff@�9X@��.@�c@��@��L@�=q@���@��n@���@�qv@�K�@��@��@��e@�ff@�V�@�R�@�R�@�Ov@�C-@�,=@���@�X�@���@��@�q�@�-@��@��A@��3@�4@�V@�ݘ@���@��@�Y�@�5�@��@���@�g8@�@�ݘ@��*@�`B@�F@��@��@��@��u@�bN@�	@���@��:@�a�@��@�M�@��)@�]�@��@���@��@�Ft@��@��3@���@�a@�O�@�!-@��f@��,@���@�_�@�u@��@�%F@���@�	@��@��6@���@�e�@��@�҉@��A@�V�@�-@�@��@���@��h@�?}@��<@�v�@�\�@�'R@���@��H@�Vm@�;d@�@��2@��6@�l�@�Ta@�C�@�_@�a@��@��,@���@�a|@�1�@��W@���@��{@�`B@�33@���@��h@�R�@�)�@��A@��a@�L�@���@��@�$�@��g@�y�@�8�@��@�z�@�:�@��T@��X@�o�@�!-@���@��x@�}V@�v�@��@��&@�v`@�E9@�@���@���@�|�@�YK@�!@���@���@�ԕ@���@�\)@��M@��A@��@��d@���@�Y�@�ی@��A@�?@��+@���@��~@�N<@��@��u@�;�@��@�@~�b@}�o@|�`@|�.@|PH@|H@|,=@z�8@y�C@y#�@x��@x!@wn/@w�@u�X@u��@u8�@t��@tI�@s�&@s��@sW?@s.I@s�@sC@s$t@s&@r�s@q�j@q#�@pm�@p	�@o�:@o{J@oe�@oK�@n�8@n�s@nZ�@m�T@m�C@m�@l�@l��@l�@l	�@k�@k@O@j��@jh
@j($@i�9@i��@i��@iu�@iX@h��@h��@hoi@h?�@h(�@g��@gP�@f�@fl�@e��@e�S@eL�@d��@d'R@c�@c�w@c\)@cP�@bߤ@bxl@ba|@a�t@a|@`�K@`1'@_W?@^�,@^� @^@]�@][W@\�@\�@[��@[!-@Z�2@Z�!@Zl�@Z@Yc@X��@Xu�@XD�@W�$@W�@V�@Va|@U�@U�@Ue,@U0�@Uq@T�K@TS�@Sخ@Sqv@S
=@R�b@Rff@RO@Q�)@Q��@Q-w@Q�@P�@P��@P�@P�@P�u@P|�@P>B@PG@P  @O�6@O@O@N��@NR�@NB[@N-@Ne@N_@M��@M��@MQ�@L�@L�@L�I@L��@L�@K�@Kl�@K8@J�L@JB[@J�@I��@I��@Ic@H�v@G��@G/�@F��@F{�@F�@E�@E�@E�C@Ec@E`B@EO�@Eq@D�@D��@D<�@D�@C�@C�&@C��@C��@Co�@C33@B��@BW�@A��@A��@A��@Am]@Ac�@AS&@AF@A4@A(�@@��@@��@@C-@@�@?��@?X�@?+@>��@>�r@=��@=j@=�@<Ɇ@<r�@;�g@;U�@:�@:��@:�+@:V@:&�@9�D@9|@9�@8��@8`�@82�@8 �@8 �@8M@7�m@7~�@7RT@7�@6�2@6}V@6M�@6J@5��@5F@5@4�v@4��@4I�@4?�@4"h@3�A@3��@38@3/�@333@3>�@3A�@3@O@2�@2-@1�d@1�@1\�@1+�@0�@07�@/�V@/n/@/iD@/O@.�@.��@.z@.c @.B[@-��@-�j@-x�@-+�@-%@,�e@,c�@,Xy@,�@+�@@+�f@+Mj@+�@*�'@*�}@*�1@*u%@)��@)^�@(�@(`�@(:�@(�@'��@'~�@'o�@'s@'�@&�'@&�L@&1�@%�D@%��@%�"@$�/@$x@#6z@#�@#Y@"��@"=q@!�@!f�@ ��@ m�@ >B@ "h@�@ߤ@h
@�@��@�n@��@x�@zx@k�@�p@:�@�@C@�\@\�@3�@&�@
�@�X@��@f�@�@�Y@N�@Ft@1'@�@�@�+@�}@��@�k@�@�y@��@�F@u%@L0@�-@F@ѷ@Xy@�@��@�@@|�@K�@�@H�@�@�~@=�@��@r�@I�@D�@<�@2�@2�@-�@*�@@�K@RT@�M@��@��@v�@C�@&�@�@��@��@�S@k�@\�@5�@��@�@�@�@��@�D@bN@S�@Ft@'R@M@x@�@�@�P@]�@�@
�@
��@
d�@
5?@
�@
u@	�#@	��@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
)DB
)DB
(�B
(sB
(�B
*�B
+B
,�B
-�B
.B
.�B
/�B
0UB
0�B
1[B
1�B
2B
2|B
3hB
3�B
3�B
3�B
4B
4TB
5�B
7fB
9�B
;B
<�B
?HB
B'B
E�B
G�B
G�B
L�B
�+B
�-B
��B
�B
��B
��B
�B
��B
��B
�xB
̘B
�uB
�mB
�B
�B
��B
�LB�BF�Bl�B�ZB�B��B��B�dB��B�B�gB�B�B�fB�TB��B�_B�fB��BּB�JB�RBƨB��B�2B��B��B� By	BaHBN�B?�B./B�BjB�B
��B
�eB
�LB
�?B
x�B
mCB
T{B
;�B
VB
	B	�*B	�LB	ǮB	�>B	��B	��B	}�B	f�B	?�B	)_B	 �B	�B	mB	�B	�B	B	B	�B	�B	YB	;B	/�B	5�B	LB	[=B	^�B	q�B	�rB	��B	��B	��B	ɺB	��B	�B	�'B	�B	��B	�LB	�.B
B
�B
tB
�B
�B
#�B
+QB
,�B
.IB
%�B
�B
�B
�B
�B
 \B
,�B
-]B
;dB
7�B
3�B
0;B
0;B
,�B
-B
1B
0;B
0!B
/�B
,�B
-�B
-)B
*�B
'8B
'B
)yB
)�B
*�B
,�B
,�B
,�B
-]B
-wB
,=B
*B
'�B
$@B
!|B
 �B
!�B
 �B
!-B
&�B
&LB
#�B
!B
pB
IB
xB
�B
;B
'B
0UB
/�B
.B
-�B
,�B
)�B
(sB
)_B
&2B
&LB
#nB
#�B
$�B
)*B
+kB
+�B
+�B
-�B
-�B
.�B
/ B
-�B
,qB
*�B
)*B
($B
(>B
'�B
'�B
*0B
+�B
'�B
)_B
,�B
-�B
-�B
.}B
+kB
(XB
'�B
(XB
)*B
)�B
)B
(�B
(�B
(XB
&�B
&�B
$�B
"�B
!bB
jB
�B
]B
�B
CB
qB
�B
1B
�B
+B
sB
mB
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
B
\B
�B
�B
�B
JB

�B

	B

�B

�B

�B

�B

�B

�B

�B

�B
�B

�B

�B

�B
	�B
	B
�B
�B
�B
EB
B
%B
�B
�B
B
_B
�B
YB
YB
B
�B
9B
B
�B
�B
gB
�B
SB
�B
�B
�B
9B
SB
B
B
�B
�B
SB
SB
�B
�B
�B
�B
�B
B
9B
B
B
9B
9B
mB
�B
�B
B
�B
9B
MB
aB
GB
3B
MB
mB
9B
�B
�B
aB
�B
�B
uB
�B
�B
[B
uB
�B
�B
�B
�B
uB
�B
B
uB
B
�B
�B
�B
GB
-B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
aB
aB
�B
�B
�B
�B
�B
B
YB
�B
�B
B
�B
�B
�B
1B
KB
B
�B
�B
	B
�B
�B
	B
	B
	�B
	�B
	�B

	B

#B

	B

	B
	�B
	�B

rB
B
�B
PB
�B
<B
<B
"B
VB
<B
VB
<B
VB
<B
VB
"B
<B
"B
�B
"B
�B
}B
 B
�B
�B
�B
�B
hB
hB
TB
oB
TB
�B
&B
B
&B
�B
�B
{B
�B
�B
MB
MB
�B
�B
�B
�B
B
�B
9B
9B
9B
mB
�B
SB
�B
�B
�B
YB
sB
YB
�B
�B
�B
�B
�B
�B
�B
B
+B
yB
�B
B
B
7B
B
QB
7B
�B
�B
=B
�B
�B
�B
CB
]B
�B
�B
dB
�B
B
B
OB
�B
�B
;B
;B
VB
�B
�B
�B
�B
�B
�B
 BB
 'B
 �B
!B
!HB
!HB
!�B
!�B
"hB
"�B
#:B
#�B
#�B
#�B
$B
$tB
$�B
%B
%,B
%`B
%�B
&B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(XB
(sB
(�B
(�B
)yB
)�B
+�B
,=B
,�B
,�B
,�B
-wB
.B
.}B
.IB
.�B
/�B
0;B
0�B
0�B
0�B
0�B
1B
1�B
2|B
2�B
3hB
4TB
3�B
3�B
3�B
5�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
8RB
8�B
9$B
9>B
9XB
9>B
8�B
9�B
9>B
9	B
:*B
:xB
:�B
:�B
:�B
;�B
<B
;�B
<B
<�B
<�B
<�B
=B
=VB
=qB
=qB
=�B
=�B
<jB
;�B
;�B
;�B
<jB
<jB
<�B
<�B
=VB
=�B
=�B
>B
>B
=�B
>]B
?cB
A;B
A B
AUB
@�B
@�B
@�B
@�B
@�B
A B
AUB
AUB
A;B
AB
@�B
A B
AB
A B
A�B
A�B
B'B
B'B
B�B
BuB
B�B
CaB
C�B
C�B
C�B
D3B
D�B
ESB
EB
F�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
K�B
K�B
LdB
L�B
L~B
L�B
MB
MB
MPB
M�B
M�B
MjB
M�B
NB
NVB
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
P�B
P�B
P�B
Q�B
RoB
S[B
S�B
S�B
TB
T,B
T,B
TaB
T�B
TaB
T�B
T�B
T�B
UgB
U�B
U�B
UgB
U�B
U�B
U�B
U�B
U�B
VB
VB
V9B
VB
VSB
VSB
VmB
VSB
VmB
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W?B
WYB
W�B
XyB
YB
YB
YeB
YKB
Y�B
Y�B
ZB
Z7B
Z�B
[�B
\B
\)B
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]B
\�B
]~B
]dB
]dB
]~B
]�B
]�B
]�B
^5B
^5B
^�B
_pB
_�B
_pB
_�B
_�B
_�B
`BB
`vB
`�B
`�B
a-B
aHB
bB
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
dtB
d�B
d�B
eB
e`B
eFB
e�B
e�B
e�B
fLB
fLB
ffB
ffB
ffB
fLB
f�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
i�B
i�B
i�B
i�B
j0B
jKB
jKB
jeB
jB
j�B
jB
j�B
j�B
j�B
k�B
lWB
l=B
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
m�B
n�B
oB
o�B
p!B
p;B
poB
p�B
rB
rB
rB
raB
rGB
r�B
s�B
s�B
s�B
s�B
tB
u�B
u�B
utB
u?B
vzB
v�B
v�B
v�B
v�B
wB
v�B
v�B
xB
y$B
y�B
y	B
x�B
xlB
xB
xB
xB
xB
xRB
y	B
y�B
z�B
z�B
{JB
{dB
{�B
|B
|B
|6B
|�B
|PB
{�B
{B
{B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~B
~B
~(B
~BB
~wB
~�B
~wB
}�B
}�B
}�B
~B
~B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~(B
~BB
~BB
~BB
~]B
~]B
~]B
~wB
~�B
~�B
B
B
HB
.B
.B
.B
.B
HB
}B
}B
}B
}B
�B
�B
�4B
��B
��B
�iB
�OB
��B
��B
��B
�UB
�;B
�UB
��B
��B
��B
�'B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
)DB
)DB
(�B
(sB
(�B
*�B
+B
,�B
-�B
.B
.�B
/�B
0UB
0�B
1[B
1�B
2B
2|B
3hB
3�B
3�B
3�B
4B
4TB
5�B
7fB
9�B
;B
<�B
?HB
B'B
E�B
G�B
G�B
L�B
�+B
�-B
��B
�B
��B
��B
�B
��B
��B
�xB
̘B
�uB
�mB
�B
�B
��B
�LB�BF�Bl�B�ZB�B��B��B�dB��B�B�gB�B�B�fB�TB��B�_B�fB��BּB�JB�RBƨB��B�2B��B��B� By	BaHBN�B?�B./B�BjB�B
��B
�eB
�LB
�?B
x�B
mCB
T{B
;�B
VB
	B	�*B	�LB	ǮB	�>B	��B	��B	}�B	f�B	?�B	)_B	 �B	�B	mB	�B	�B	B	B	�B	�B	YB	;B	/�B	5�B	LB	[=B	^�B	q�B	�rB	��B	��B	��B	ɺB	��B	�B	�'B	�B	��B	�LB	�.B
B
�B
tB
�B
�B
#�B
+QB
,�B
.IB
%�B
�B
�B
�B
�B
 \B
,�B
-]B
;dB
7�B
3�B
0;B
0;B
,�B
-B
1B
0;B
0!B
/�B
,�B
-�B
-)B
*�B
'8B
'B
)yB
)�B
*�B
,�B
,�B
,�B
-]B
-wB
,=B
*B
'�B
$@B
!|B
 �B
!�B
 �B
!-B
&�B
&LB
#�B
!B
pB
IB
xB
�B
;B
'B
0UB
/�B
.B
-�B
,�B
)�B
(sB
)_B
&2B
&LB
#nB
#�B
$�B
)*B
+kB
+�B
+�B
-�B
-�B
.�B
/ B
-�B
,qB
*�B
)*B
($B
(>B
'�B
'�B
*0B
+�B
'�B
)_B
,�B
-�B
-�B
.}B
+kB
(XB
'�B
(XB
)*B
)�B
)B
(�B
(�B
(XB
&�B
&�B
$�B
"�B
!bB
jB
�B
]B
�B
CB
qB
�B
1B
�B
+B
sB
mB
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
B
\B
�B
�B
�B
JB

�B

	B

�B

�B

�B

�B

�B

�B

�B

�B
�B

�B

�B

�B
	�B
	B
�B
�B
�B
EB
B
%B
�B
�B
B
_B
�B
YB
YB
B
�B
9B
B
�B
�B
gB
�B
SB
�B
�B
�B
9B
SB
B
B
�B
�B
SB
SB
�B
�B
�B
�B
�B
B
9B
B
B
9B
9B
mB
�B
�B
B
�B
9B
MB
aB
GB
3B
MB
mB
9B
�B
�B
aB
�B
�B
uB
�B
�B
[B
uB
�B
�B
�B
�B
uB
�B
B
uB
B
�B
�B
�B
GB
-B
aB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
aB
aB
�B
�B
�B
�B
�B
B
YB
�B
�B
B
�B
�B
�B
1B
KB
B
�B
�B
	B
�B
�B
	B
	B
	�B
	�B
	�B

	B

#B

	B

	B
	�B
	�B

rB
B
�B
PB
�B
<B
<B
"B
VB
<B
VB
<B
VB
<B
VB
"B
<B
"B
�B
"B
�B
}B
 B
�B
�B
�B
�B
hB
hB
TB
oB
TB
�B
&B
B
&B
�B
�B
{B
�B
�B
MB
MB
�B
�B
�B
�B
B
�B
9B
9B
9B
mB
�B
SB
�B
�B
�B
YB
sB
YB
�B
�B
�B
�B
�B
�B
�B
B
+B
yB
�B
B
B
7B
B
QB
7B
�B
�B
=B
�B
�B
�B
CB
]B
�B
�B
dB
�B
B
B
OB
�B
�B
;B
;B
VB
�B
�B
�B
�B
�B
�B
 BB
 'B
 �B
!B
!HB
!HB
!�B
!�B
"hB
"�B
#:B
#�B
#�B
#�B
$B
$tB
$�B
%B
%,B
%`B
%�B
&B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(XB
(sB
(�B
(�B
)yB
)�B
+�B
,=B
,�B
,�B
,�B
-wB
.B
.}B
.IB
.�B
/�B
0;B
0�B
0�B
0�B
0�B
1B
1�B
2|B
2�B
3hB
4TB
3�B
3�B
3�B
5�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
8RB
8�B
9$B
9>B
9XB
9>B
8�B
9�B
9>B
9	B
:*B
:xB
:�B
:�B
:�B
;�B
<B
;�B
<B
<�B
<�B
<�B
=B
=VB
=qB
=qB
=�B
=�B
<jB
;�B
;�B
;�B
<jB
<jB
<�B
<�B
=VB
=�B
=�B
>B
>B
=�B
>]B
?cB
A;B
A B
AUB
@�B
@�B
@�B
@�B
@�B
A B
AUB
AUB
A;B
AB
@�B
A B
AB
A B
A�B
A�B
B'B
B'B
B�B
BuB
B�B
CaB
C�B
C�B
C�B
D3B
D�B
ESB
EB
F�B
G�B
HKB
H�B
H�B
H�B
H�B
H�B
H�B
IB
IB
I�B
I�B
J	B
JrB
J�B
J�B
J�B
J�B
KDB
K�B
K�B
K�B
LdB
L�B
L~B
L�B
MB
MB
MPB
M�B
M�B
MjB
M�B
NB
NVB
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
P�B
P�B
P�B
Q�B
RoB
S[B
S�B
S�B
TB
T,B
T,B
TaB
T�B
TaB
T�B
T�B
T�B
UgB
U�B
U�B
UgB
U�B
U�B
U�B
U�B
U�B
VB
VB
V9B
VB
VSB
VSB
VmB
VSB
VmB
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W?B
WYB
W�B
XyB
YB
YB
YeB
YKB
Y�B
Y�B
ZB
Z7B
Z�B
[�B
\B
\)B
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]B
\�B
]~B
]dB
]dB
]~B
]�B
]�B
]�B
^5B
^5B
^�B
_pB
_�B
_pB
_�B
_�B
_�B
`BB
`vB
`�B
`�B
a-B
aHB
bB
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
dtB
d�B
d�B
eB
e`B
eFB
e�B
e�B
e�B
fLB
fLB
ffB
ffB
ffB
fLB
f�B
g�B
g�B
g�B
g�B
g�B
g�B
hsB
i�B
i�B
i�B
i�B
j0B
jKB
jKB
jeB
jB
j�B
jB
j�B
j�B
j�B
k�B
lWB
l=B
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
m�B
n�B
oB
o�B
p!B
p;B
poB
p�B
rB
rB
rB
raB
rGB
r�B
s�B
s�B
s�B
s�B
tB
u�B
u�B
utB
u?B
vzB
v�B
v�B
v�B
v�B
wB
v�B
v�B
xB
y$B
y�B
y	B
x�B
xlB
xB
xB
xB
xB
xRB
y	B
y�B
z�B
z�B
{JB
{dB
{�B
|B
|B
|6B
|�B
|PB
{�B
{B
{B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|jB
|jB
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~B
~B
~(B
~BB
~wB
~�B
~wB
}�B
}�B
}�B
~B
~B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~(B
~BB
~BB
~BB
~]B
~]B
~]B
~wB
~�B
~�B
B
B
HB
.B
.B
.B
.B
HB
}B
}B
}B
}B
�B
�B
�4B
��B
��B
�iB
�OB
��B
��B
��B
�UB
�;B
�UB
��B
��B
��B
�'B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105248  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192806  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192806  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192806                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042814  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042814  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                