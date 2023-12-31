CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:44:44Z creation;2022-06-04T17:44:44Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174444  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ةy$h��1   @ةy{�u�@-��/���d:��`A�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B&  B/��B8  B@  BH  BP  BY��B^  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct33Cu�3Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @G�@w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�BG�Bz�B%z�B/{B7z�B?z�BGz�BOz�BY{B]z�Bg{Boz�Bwz�Bz�B��qB��qB��qB��qB��B��qB��>B��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBƽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC�RC޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?�RCA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW�CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Ct�Cu��Cw�Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DP~DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_�HD`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dÿ
D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�
D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�.A�~]A�~(A�}�A�|�A�{A�w2A�qvA�k�A�j�A�jA�e�A�]�A�XEA�U�A�LdA�G�A�2�A���AϿ�AϬ�A�I�AξBAͦA�jKA��WA˶�AˊrA��A�ʌA�GEA� A�xA���A�GA�uZA�L0AČJA�cTA���A���A���A�:�A��\A��PA�w�A��>A�K�A��A� �A�NpA��A��)A��FA���A��CA�d�A�ÖA��A��A���A�רA��[A�M�A�4A��cA�n/A��A���A��A���A��VA���A��A��'A�y	A�YA���A�.�A��&A���A���A�%A���A�L�A���A���A��|A�!bA�ԕAhsA{��AxϫAw�Au�_At}�Aq��AnC�Am�TAj^5Ah��Ag��AbzxA_�KA[q�AW��ASm]ANMAL�AK	AI�YAG�0AF�dAE��ABFA>��A<�pA< �A;IRA9�AA7��A3��A1#:A*y>A(�FA'�TA'?�A%�&A$��A#�CA##�A"OA!U2A 
=A��A[WA$�Aj�AȴA�A��A�+AeAI�A�AuAjAh�A��A˒A��A�A_A�A�XA�A�FA�zA��AzA�4A iAA�A=�A&A�AA�Aw�A~�A��A��A��A��A"hA�Ay>AW?AX�AA�A{JA^5A�A��Ab�A��A\�A�A��A�.ADgA�2A
��A	�RAiDA��A8A�@A�.A7�A!�A�]A�KAu�AVA��AC�A�fA~A �]A qv@��N@�j@��f@��|@�A�@���@�~@��D@�1@�m]@�q@���@��@�j@�$@��@�Ft@��@���@���@��Q@�c�@�5�@�q�@��@�&�@��U@��@��@��@�-@��]@妵@��8@�J�@��m@�`B@��@�6@�X@�f�@��@���@�@O@ި�@��@�ں@�M@��@� \@�ȴ@�z@ټ@ي�@م�@ك{@�`B@���@�)�@��@�=�@�l"@��#@�w2@�+@�ȴ@�tT@��g@�5�@��@Ҟ@�?@ѧ�@�ȴ@�	�@Ϲ�@��a@��T@�PH@�1�@���@Ͼw@Ϫ�@�zx@�Ɇ@�g8@�+k@�W?@��)@�<�@�+�@ʝI@��@�#�@��?@ȡb@�-�@�0�@ƞ@�"h@ŶF@�;@�[�@Û=@ �@�m�@�K^@�R�@���@�>�@��2@�^5@��@�33@��]@�<�@�x�@�(�@��8@��!@��$@��z@�M@��@� \@��)@�|�@���@��4@�?}@�&@�"�@��@���@�Q�@�@��@�iD@��@��@�M@�|@���@�N�@�6�@��]@��D@��@���@��}@�(�@��@�@�x@���@��@��9@�3�@�b�@�ȴ@�I�@���@���@�N<@��@��L@�<�@��o@��4@�b�@� i@��h@���@�|�@�4n@���@�L�@��@�I�@��
@���@�)_@���@��>@�G�@�C@��P@���@�%�@��@��@���@�a@�V@���@�w�@�YK@�0U@��7@��@��B@���@�e�@�C�@�$@�b@���@���@��@��@���@��@�e�@�;�@�	@��q@��4@�9�@��@��)@��h@���@�b@���@�|�@�&�@���@�q�@��=@��@�9�@�Y�@�ߤ@��@���@���@�y>@���@���@��8@�q@�Q�@�=q@�O�@�<6@�u�@�|@�:�@�
=@���@��v@���@�p;@��@���@���@��@�	l@���@���@��@���@���@�v�@�PH@�>B@�*�@�!@��@���@�x�@��8@���@�PH@�#:@��@��@�֡@���@�xl@�l�@��@�]�@���@�h�@�!@��@��~@���@�|�@�y�@�n/@�RT@�33@�;@���@�h
@�4n@�{@���@���@���@�}�@�IR@��'@�M@�u@�ƨ@���@��@�@�֡@�҉@��?@��@��!@���@���@�q@�x@��
@��	@�a@�<6@��@�_�@��@��7@�dZ@�5�@��K@��)@��h@��u@�YK@��@O@�@~�@~Z�@~@}�N@}w2@}f�@|�@{خ@{K�@z��@zp;@y��@x�v@x(�@w_p@w�@vߤ@v��@v�@vq�@vC�@u�h@u�@t�v@t�@th�@t>B@t1'@t@s�}@s>�@q�)@q��@q<6@q	l@p�K@p~(@pA�@pC-@p�@o��@o��@o>�@n��@nZ�@n5?@n�@m�N@ms�@mG�@l�P@lĜ@l�4@l[�@k�}@kK�@j�@iA @h��@hV�@h�@g��@f��@fV@e�#@eB�@e�@d��@d"h@cg�@b:*@a�#@aO�@a�@`��@`l"@`<�@`/�@_��@_b�@_@^��@^h
@^;�@]��@]��@]^�@]<6@\��@\��@\��@\��@[��@[�f@[y�@[\)@[�@Z��@Z�@Z=q@Z�@Ys�@Y?}@X�K@X�@W��@W��@W_p@V��@U��@U�n@T�9@T7@S��@SiD@S�@R��@Rq�@RL0@Q�.@Q`B@P��@P?�@O��@O+@N��@N�@NE�@N+k@N	@M��@Mhs@Me,@MIR@M&�@M@@LU2@K�@Jff@J)�@J�@J�@J�@J
�@I��@I�@IX@I&�@H�@H��@Hr�@H�@Gخ@G��@G
=@F��@Fz@F!�@E��@E�@E�M@D�O@D'R@C��@C�a@C�f@CO@C@B�@BH�@A�@A�M@A@@@��@@-�@?خ@?)_@>u@=��@=�@<��@<|�@<-�@<�@;��@;�@;خ@;�F@;�*@;$t@:�+@:+k@:!�@:	@9�@9e,@8�@8M@8�@7��@7�&@7�*@7dZ@7$t@7�@7S@6��@6_@5!�@4��@4�U@4�I@4K^@4(�@4x@3�A@3�w@3'�@2�'@2��@2n�@21�@1��@1�X@1u�@1B�@1�@0��@0��@0�_@0tT@0l"@0�@/�[@/��@.�@-��@-:�@,�P@,��@,��@,��@,��@,g8@,N�@,(�@,G@+��@+�@+@O@*�r@*d�@*)�@)�@)�@)�)@)�>@)�@)�3@)��@)�n@)��@)IR@)(�@)�@(�E@(��@(u�@(M@(1'@(7@(  @'ݘ@'�	@'S�@'@O@'�@'�@&�2@&��@&$�@%�"@%`B@%S&@%L�@%G�@%/@%@%�@%@@$�5@$��@$(�@#n/@#�@"��@"6�@!��@!�@ �@ �@ r�@ c�@ (�@�K@t�@Mj@)_@�@�s@�@s�@Ta@�@�M@A @�	@�9@�Y@q@K^@�@�;@��@Z�@��@��@i�@c @^5@#:@�~@/@�P@��@Ɇ@�j@��@�.@�@u�@c�@Xy@>B@7@��@�:@W?@"�@ں@��@� @n�@GE@1�@!�@{@��@�"@zx@k�@T�@�@�@��@�U@�I@�o@m�@4n@  @�@x@��@��@R�@�@��@j@O�@7L@#�@�@	l@�v@�@��@q@[�@7�@�&@˒@��@��@��@�k@��@�4@O@�@�s@��@u%@H�@	@�o@�@@�t@\�@�@�|@��@�D@l"@'R@*�@'R@"h@�@��@��@��@x@t�@t�@RT@,�@S@
��@
}V@
GE@	�9@	�'@	��@	u�@	Y�@	!�@	@��@Ĝ@��@�@��@�o@oi@$@��@��@]�@33@�"@�@͟@��@�x@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�.A�~]A�~(A�}�A�|�A�{A�w2A�qvA�k�A�j�A�jA�e�A�]�A�XEA�U�A�LdA�G�A�2�A���AϿ�AϬ�A�I�AξBAͦA�jKA��WA˶�AˊrA��A�ʌA�GEA� A�xA���A�GA�uZA�L0AČJA�cTA���A���A���A�:�A��\A��PA�w�A��>A�K�A��A� �A�NpA��A��)A��FA���A��CA�d�A�ÖA��A��A���A�רA��[A�M�A�4A��cA�n/A��A���A��A���A��VA���A��A��'A�y	A�YA���A�.�A��&A���A���A�%A���A�L�A���A���A��|A�!bA�ԕAhsA{��AxϫAw�Au�_At}�Aq��AnC�Am�TAj^5Ah��Ag��AbzxA_�KA[q�AW��ASm]ANMAL�AK	AI�YAG�0AF�dAE��ABFA>��A<�pA< �A;IRA9�AA7��A3��A1#:A*y>A(�FA'�TA'?�A%�&A$��A#�CA##�A"OA!U2A 
=A��A[WA$�Aj�AȴA�A��A�+AeAI�A�AuAjAh�A��A˒A��A�A_A�A�XA�A�FA�zA��AzA�4A iAA�A=�A&A�AA�Aw�A~�A��A��A��A��A"hA�Ay>AW?AX�AA�A{JA^5A�A��Ab�A��A\�A�A��A�.ADgA�2A
��A	�RAiDA��A8A�@A�.A7�A!�A�]A�KAu�AVA��AC�A�fA~A �]A qv@��N@�j@��f@��|@�A�@���@�~@��D@�1@�m]@�q@���@��@�j@�$@��@�Ft@��@���@���@��Q@�c�@�5�@�q�@��@�&�@��U@��@��@��@�-@��]@妵@��8@�J�@��m@�`B@��@�6@�X@�f�@��@���@�@O@ި�@��@�ں@�M@��@� \@�ȴ@�z@ټ@ي�@م�@ك{@�`B@���@�)�@��@�=�@�l"@��#@�w2@�+@�ȴ@�tT@��g@�5�@��@Ҟ@�?@ѧ�@�ȴ@�	�@Ϲ�@��a@��T@�PH@�1�@���@Ͼw@Ϫ�@�zx@�Ɇ@�g8@�+k@�W?@��)@�<�@�+�@ʝI@��@�#�@��?@ȡb@�-�@�0�@ƞ@�"h@ŶF@�;@�[�@Û=@ �@�m�@�K^@�R�@���@�>�@��2@�^5@��@�33@��]@�<�@�x�@�(�@��8@��!@��$@��z@�M@��@� \@��)@�|�@���@��4@�?}@�&@�"�@��@���@�Q�@�@��@�iD@��@��@�M@�|@���@�N�@�6�@��]@��D@��@���@��}@�(�@��@�@�x@���@��@��9@�3�@�b�@�ȴ@�I�@���@���@�N<@��@��L@�<�@��o@��4@�b�@� i@��h@���@�|�@�4n@���@�L�@��@�I�@��
@���@�)_@���@��>@�G�@�C@��P@���@�%�@��@��@���@�a@�V@���@�w�@�YK@�0U@��7@��@��B@���@�e�@�C�@�$@�b@���@���@��@��@���@��@�e�@�;�@�	@��q@��4@�9�@��@��)@��h@���@�b@���@�|�@�&�@���@�q�@��=@��@�9�@�Y�@�ߤ@��@���@���@�y>@���@���@��8@�q@�Q�@�=q@�O�@�<6@�u�@�|@�:�@�
=@���@��v@���@�p;@��@���@���@��@�	l@���@���@��@���@���@�v�@�PH@�>B@�*�@�!@��@���@�x�@��8@���@�PH@�#:@��@��@�֡@���@�xl@�l�@��@�]�@���@�h�@�!@��@��~@���@�|�@�y�@�n/@�RT@�33@�;@���@�h
@�4n@�{@���@���@���@�}�@�IR@��'@�M@�u@�ƨ@���@��@�@�֡@�҉@��?@��@��!@���@���@�q@�x@��
@��	@�a@�<6@��@�_�@��@��7@�dZ@�5�@��K@��)@��h@��u@�YK@��@O@�@~�@~Z�@~@}�N@}w2@}f�@|�@{خ@{K�@z��@zp;@y��@x�v@x(�@w_p@w�@vߤ@v��@v�@vq�@vC�@u�h@u�@t�v@t�@th�@t>B@t1'@t@s�}@s>�@q�)@q��@q<6@q	l@p�K@p~(@pA�@pC-@p�@o��@o��@o>�@n��@nZ�@n5?@n�@m�N@ms�@mG�@l�P@lĜ@l�4@l[�@k�}@kK�@j�@iA @h��@hV�@h�@g��@f��@fV@e�#@eB�@e�@d��@d"h@cg�@b:*@a�#@aO�@a�@`��@`l"@`<�@`/�@_��@_b�@_@^��@^h
@^;�@]��@]��@]^�@]<6@\��@\��@\��@\��@[��@[�f@[y�@[\)@[�@Z��@Z�@Z=q@Z�@Ys�@Y?}@X�K@X�@W��@W��@W_p@V��@U��@U�n@T�9@T7@S��@SiD@S�@R��@Rq�@RL0@Q�.@Q`B@P��@P?�@O��@O+@N��@N�@NE�@N+k@N	@M��@Mhs@Me,@MIR@M&�@M@@LU2@K�@Jff@J)�@J�@J�@J�@J
�@I��@I�@IX@I&�@H�@H��@Hr�@H�@Gخ@G��@G
=@F��@Fz@F!�@E��@E�@E�M@D�O@D'R@C��@C�a@C�f@CO@C@B�@BH�@A�@A�M@A@@@��@@-�@?خ@?)_@>u@=��@=�@<��@<|�@<-�@<�@;��@;�@;خ@;�F@;�*@;$t@:�+@:+k@:!�@:	@9�@9e,@8�@8M@8�@7��@7�&@7�*@7dZ@7$t@7�@7S@6��@6_@5!�@4��@4�U@4�I@4K^@4(�@4x@3�A@3�w@3'�@2�'@2��@2n�@21�@1��@1�X@1u�@1B�@1�@0��@0��@0�_@0tT@0l"@0�@/�[@/��@.�@-��@-:�@,�P@,��@,��@,��@,��@,g8@,N�@,(�@,G@+��@+�@+@O@*�r@*d�@*)�@)�@)�@)�)@)�>@)�@)�3@)��@)�n@)��@)IR@)(�@)�@(�E@(��@(u�@(M@(1'@(7@(  @'ݘ@'�	@'S�@'@O@'�@'�@&�2@&��@&$�@%�"@%`B@%S&@%L�@%G�@%/@%@%�@%@@$�5@$��@$(�@#n/@#�@"��@"6�@!��@!�@ �@ �@ r�@ c�@ (�@�K@t�@Mj@)_@�@�s@�@s�@Ta@�@�M@A @�	@�9@�Y@q@K^@�@�;@��@Z�@��@��@i�@c @^5@#:@�~@/@�P@��@Ɇ@�j@��@�.@�@u�@c�@Xy@>B@7@��@�:@W?@"�@ں@��@� @n�@GE@1�@!�@{@��@�"@zx@k�@T�@�@�@��@�U@�I@�o@m�@4n@  @�@x@��@��@R�@�@��@j@O�@7L@#�@�@	l@�v@�@��@q@[�@7�@�&@˒@��@��@��@�k@��@�4@O@�@�s@��@u%@H�@	@�o@�@@�t@\�@�@�|@��@�D@l"@'R@*�@'R@"h@�@��@��@��@x@t�@t�@RT@,�@S@
��@
}V@
GE@	�9@	�'@	��@	u�@	Y�@	!�@	@��@Ĝ@��@�@��@�o@oi@$@��@��@]�@33@�"@�@͟@��@�x@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�<B�<B�VB�VB�pB�pB��B��B��B��B��B��B��B��B��B�.B�TB��B�B�B@Bz�B	�B	V�B	��B	��B	ܒB	�`B
�B
pB
M�B
cB
��B
�B
�!B
�8BKDB`�B]�B��B��B��B�aB�B�B�]B�B�BB�B1[B6`B*0BdB�B"4B+�B'RBMBNBhBtB�3B�ZB�;B�pB�aB�UB��B��B�\Bm)BC�B6B	�B
��B
�B
�DB
�GB
�VB
ΥB
�xB
�B
�	B
ĶB
�2B
�hB
��B
�GB
e�B
O(B
B�B
1[B
)�B
"�B
#B
	lB	�B	��B	��B	οB	�B	�KB	�MB	xlB	XB	>�B	�B	[B	�B	%B	  B��B�B�zB��B̘BɆB�?BªB�-BňB�[B�.B�<B��B��B��B�zB� B�:B��B��B޸B�hB�B�B�/B�RB��B	
�B	aB	�B	�B	]B	QB	B	)yB	.�B	%`B	/�B	;B	B�B	DB	Y1B	f�B	h$B	e�B	dB	j�B	z�B	��B	��B	��B	�,B	�>B	�*B	�kB	��B	�=B	�sB	�>B	�]B	��B	�B	�"B	� B	��B	�PB	ɠB	�EB	��B	��B	��B	��B	��B	�_B	׍B	�YB	�$B	ּB	��B	רB	�aB	ΥB	ɺB	ƨB	��B	�SB	�UB	�}B	��B	�B	�oB	�mB	�9B	ĶB	�B	�\B	�B	οB	�<B	͟B	̳B	�B	�~B	�JB	�"B	ΊB	�B	�0B	οB	�HB	�NB	�:B	�$B	�YB	�B	�gB	�B	�[B	�B	��B	�.B	��B	�(B	�B	�vB	��B	ϫB	ϫB	�(B	ΥB	�VB	�"B	�\B	ϫB	��B	бB	�oB	�B	ӏB	�[B	��B	ԕB	�aB	�MB	�B	��B	�$B	�mB	�sB	�?B	�$B	�B	�B	��B	��B	��B	�B	�EB	��B	خB	��B	�B	�=B	�/B	��B	ބB	�B	�IB	�B	�dB	��B	ܬB	��B	��B	�~B	��B	��B	�8B	�B	�0B	�0B	��B	�0B	�B	��B	�0B	��B	�B	�B	�
B	�RB	�B	�mB	�B	�B	�WB	��B	�)B	��B	��B	�CB	��B	�/B	�"B	��B	�oB	��B	��B	�|B	�hB	��B	�|B	�B	�-B	�B	��B	�B	�B	�oB	�GB	�B	�B	��B	�ZB	�B	�tB	��B	��B	�`B	��B	��B	��B	�rB	�XB	�>B	��B	��B	��B	�$B	��B	�xB	�xB	�*B	��B	��B	��B	��B	�B	��B	��B	��B	�]B	��B	�]B	�BB	��B	�(B	��B	�BB	�.B	��B
  B
  B
 B
;B
�B
�B
�B
�B
aB
�B
�B
�B
�B
GB
{B
�B
{B
�B
3B
gB
mB
�B
�B
�B
�B
�B
	�B
	�B

	B

�B
DB
JB
�B
�B
~B
0B
�B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
HB
.B
bB
B
}B
.B
B
HB
�B
�B
�B
�B
�B
<B
6B
<B
�B
�B
(B
�B
�B
.B
�B
�B
 B
bB
�B
�B
�B
B
&B
�B
B
2B
B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
:B
�B
,B
,B
,B
,B
�B
�B
�B
�B
FB
MB
B
�B
�B
�B
�B
MB
�B
MB
�B
9B
�B
+B
B
7B
=B
�B
�B
�B
�B
)B
)B
�B
)B
�B
~B
B
�B
�B
 B
 �B
 �B
 �B
!�B
"�B
#nB
#�B
#�B
$&B
$�B
%FB
%FB
%FB
%zB
%zB
%zB
%�B
%�B
&�B
'B
'�B
'�B
(
B
(�B
)yB
*0B
*�B
+B
+6B
+�B
+�B
+�B
+�B
,B
,=B
,WB
,WB
,�B
,�B
-B
-B
-CB
-B
./B
.}B
/ B
/5B
/5B
0;B
0�B
1vB
2-B
2B
2-B
2-B
2GB
2GB
2GB
2�B
3MB
3MB
3hB
3�B
3�B
3�B
3�B
3�B
4nB
5�B
5�B
6+B
6FB
6+B
6�B
72B
7B
7LB
7LB
72B
7�B
8�B
9	B
9�B
9�B
:DB
:�B
:�B
;0B
;dB
;B
;�B
<B
<6B
<�B
=�B
=�B
>B
>B
>BB
?.B
?cB
?}B
@ B
?�B
@4B
@iB
@�B
A�B
BB
B[B
BuB
B�B
B�B
B�B
B�B
B�B
CGB
CaB
C�B
C{B
C{B
C�B
D3B
DgB
DMB
DMB
D3B
DB
DB
D�B
D�B
D�B
D�B
D�B
EB
EB
EmB
E9B
E�B
E�B
E�B
E�B
F?B
F%B
F?B
FtB
F�B
F�B
G�B
HB
HKB
H�B
H�B
I7B
IlB
IlB
I�B
J=B
J�B
J�B
K^B
K�B
K�B
K�B
LB
L0B
LJB
L�B
L�B
L�B
L�B
MB
L�B
M�B
N�B
OB
OB
O(B
O(B
OBB
OvB
O�B
PB
P.B
PbB
P�B
P�B
P�B
P�B
P�B
P�B
QB
P�B
QB
Q4B
QNB
Q B
QNB
Q�B
R:B
R�B
R�B
R�B
R�B
SB
R�B
S[B
S�B
S�B
S�B
TB
TaB
TaB
T�B
U2B
UgB
U�B
U�B
VB
VSB
VmB
V�B
V�B
V�B
V�B
V�B
W?B
W�B
X+B
X+B
X+B
X+B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
ZkB
ZkB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
^B
^5B
^B
^OB
^jB
^jB
_!B
_�B
_�B
_�B
`�B
abB
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
cTB
dB
dB
d&B
dtB
dtB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h
B
h
B
g�B
h
B
hXB
hsB
iDB
i*B
i�B
i�B
jB
jeB
kB
kB
kB
kB
kQB
k�B
k�B
k�B
k�B
lB
l=B
lqB
l�B
lqB
mB
mCB
mwB
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
qB
qB
q'B
qAB
q[B
q[B
q[B
qvB
qvB
qvB
q�B
q�B
rGB
rGB
r�B
r�B
r�B
sB
s3B
sMB
shB
sMB
shB
s�B
tB
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
utB
uZB
u�B
vzB
v�B
v�B
w2B
w�B
w�B
w�B
xB
xB
xB
xB
xRB
xlB
x�B
x�B
x�B
x�B
y>B
yXB
yXB
yXB
yrB
yrB
yrB
y�B
y�B
y�B
z*B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{B
{�B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}qB
}qB
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
.B
HB
HB
}B
�B
�B
�B
�B
�B
�4B
�4B
�4B
�4B
��B
�B
�;B
�oB
��B
��B
��B
�B
�'B
�AB
�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�<B�<B�VB�VB�pB�pB��B��B��B��B��B��B��B��B��B�.B�TB��B�B�B@Bz�B	�B	V�B	��B	��B	ܒB	�`B
�B
pB
M�B
cB
��B
�B
�!B
�8BKDB`�B]�B��B��B��B�aB�B�B�]B�B�BB�B1[B6`B*0BdB�B"4B+�B'RBMBNBhBtB�3B�ZB�;B�pB�aB�UB��B��B�\Bm)BC�B6B	�B
��B
�B
�DB
�GB
�VB
ΥB
�xB
�B
�	B
ĶB
�2B
�hB
��B
�GB
e�B
O(B
B�B
1[B
)�B
"�B
#B
	lB	�B	��B	��B	οB	�B	�KB	�MB	xlB	XB	>�B	�B	[B	�B	%B	  B��B�B�zB��B̘BɆB�?BªB�-BňB�[B�.B�<B��B��B��B�zB� B�:B��B��B޸B�hB�B�B�/B�RB��B	
�B	aB	�B	�B	]B	QB	B	)yB	.�B	%`B	/�B	;B	B�B	DB	Y1B	f�B	h$B	e�B	dB	j�B	z�B	��B	��B	��B	�,B	�>B	�*B	�kB	��B	�=B	�sB	�>B	�]B	��B	�B	�"B	� B	��B	�PB	ɠB	�EB	��B	��B	��B	��B	��B	�_B	׍B	�YB	�$B	ּB	��B	רB	�aB	ΥB	ɺB	ƨB	��B	�SB	�UB	�}B	��B	�B	�oB	�mB	�9B	ĶB	�B	�\B	�B	οB	�<B	͟B	̳B	�B	�~B	�JB	�"B	ΊB	�B	�0B	οB	�HB	�NB	�:B	�$B	�YB	�B	�gB	�B	�[B	�B	��B	�.B	��B	�(B	�B	�vB	��B	ϫB	ϫB	�(B	ΥB	�VB	�"B	�\B	ϫB	��B	бB	�oB	�B	ӏB	�[B	��B	ԕB	�aB	�MB	�B	��B	�$B	�mB	�sB	�?B	�$B	�B	�B	��B	��B	��B	�B	�EB	��B	خB	��B	�B	�=B	�/B	��B	ބB	�B	�IB	�B	�dB	��B	ܬB	��B	��B	�~B	��B	��B	�8B	�B	�0B	�0B	��B	�0B	�B	��B	�0B	��B	�B	�B	�
B	�RB	�B	�mB	�B	�B	�WB	��B	�)B	��B	��B	�CB	��B	�/B	�"B	��B	�oB	��B	��B	�|B	�hB	��B	�|B	�B	�-B	�B	��B	�B	�B	�oB	�GB	�B	�B	��B	�ZB	�B	�tB	��B	��B	�`B	��B	��B	��B	�rB	�XB	�>B	��B	��B	��B	�$B	��B	�xB	�xB	�*B	��B	��B	��B	��B	�B	��B	��B	��B	�]B	��B	�]B	�BB	��B	�(B	��B	�BB	�.B	��B
  B
  B
 B
;B
�B
�B
�B
�B
aB
�B
�B
�B
�B
GB
{B
�B
{B
�B
3B
gB
mB
�B
�B
�B
�B
�B
	�B
	�B

	B

�B
DB
JB
�B
�B
~B
0B
�B
�B
VB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
B
HB
.B
bB
B
}B
.B
B
HB
�B
�B
�B
�B
�B
<B
6B
<B
�B
�B
(B
�B
�B
.B
�B
�B
 B
bB
�B
�B
�B
B
&B
�B
B
2B
B
�B
�B
�B
B
�B
�B
B
�B
�B
�B
:B
�B
,B
,B
,B
,B
�B
�B
�B
�B
FB
MB
B
�B
�B
�B
�B
MB
�B
MB
�B
9B
�B
+B
B
7B
=B
�B
�B
�B
�B
)B
)B
�B
)B
�B
~B
B
�B
�B
 B
 �B
 �B
 �B
!�B
"�B
#nB
#�B
#�B
$&B
$�B
%FB
%FB
%FB
%zB
%zB
%zB
%�B
%�B
&�B
'B
'�B
'�B
(
B
(�B
)yB
*0B
*�B
+B
+6B
+�B
+�B
+�B
+�B
,B
,=B
,WB
,WB
,�B
,�B
-B
-B
-CB
-B
./B
.}B
/ B
/5B
/5B
0;B
0�B
1vB
2-B
2B
2-B
2-B
2GB
2GB
2GB
2�B
3MB
3MB
3hB
3�B
3�B
3�B
3�B
3�B
4nB
5�B
5�B
6+B
6FB
6+B
6�B
72B
7B
7LB
7LB
72B
7�B
8�B
9	B
9�B
9�B
:DB
:�B
:�B
;0B
;dB
;B
;�B
<B
<6B
<�B
=�B
=�B
>B
>B
>BB
?.B
?cB
?}B
@ B
?�B
@4B
@iB
@�B
A�B
BB
B[B
BuB
B�B
B�B
B�B
B�B
B�B
CGB
CaB
C�B
C{B
C{B
C�B
D3B
DgB
DMB
DMB
D3B
DB
DB
D�B
D�B
D�B
D�B
D�B
EB
EB
EmB
E9B
E�B
E�B
E�B
E�B
F?B
F%B
F?B
FtB
F�B
F�B
G�B
HB
HKB
H�B
H�B
I7B
IlB
IlB
I�B
J=B
J�B
J�B
K^B
K�B
K�B
K�B
LB
L0B
LJB
L�B
L�B
L�B
L�B
MB
L�B
M�B
N�B
OB
OB
O(B
O(B
OBB
OvB
O�B
PB
P.B
PbB
P�B
P�B
P�B
P�B
P�B
P�B
QB
P�B
QB
Q4B
QNB
Q B
QNB
Q�B
R:B
R�B
R�B
R�B
R�B
SB
R�B
S[B
S�B
S�B
S�B
TB
TaB
TaB
T�B
U2B
UgB
U�B
U�B
VB
VSB
VmB
V�B
V�B
V�B
V�B
V�B
W?B
W�B
X+B
X+B
X+B
X+B
X�B
YKB
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
Z�B
ZkB
ZkB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
\]B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
^B
^5B
^B
^OB
^jB
^jB
_!B
_�B
_�B
_�B
`�B
abB
a�B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
b�B
cTB
dB
dB
d&B
dtB
dtB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
eB
e,B
ezB
e�B
e�B
e�B
e�B
e�B
fLB
fLB
ffB
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h
B
h
B
g�B
h
B
hXB
hsB
iDB
i*B
i�B
i�B
jB
jeB
kB
kB
kB
kB
kQB
k�B
k�B
k�B
k�B
lB
l=B
lqB
l�B
lqB
mB
mCB
mwB
m�B
m�B
m�B
m�B
n/B
ncB
n}B
n�B
n�B
o5B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
qB
qB
q'B
qAB
q[B
q[B
q[B
qvB
qvB
qvB
q�B
q�B
rGB
rGB
r�B
r�B
r�B
sB
s3B
sMB
shB
sMB
shB
s�B
tB
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
utB
uZB
u�B
vzB
v�B
v�B
w2B
w�B
w�B
w�B
xB
xB
xB
xB
xRB
xlB
x�B
x�B
x�B
x�B
y>B
yXB
yXB
yXB
yrB
yrB
yrB
y�B
y�B
y�B
z*B
z�B
z�B
z�B
z�B
z�B
{B
{0B
{B
{�B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}VB
}qB
}qB
}VB
}�B
}�B
}�B
~(B
~BB
~wB
~�B
.B
HB
HB
}B
�B
�B
�B
�B
�B
�4B
�4B
�4B
�4B
��B
�B
�;B
�oB
��B
��B
��B
�B
�'B
�AB
�[11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104935  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174444  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174444  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174444                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024451  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024451  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                