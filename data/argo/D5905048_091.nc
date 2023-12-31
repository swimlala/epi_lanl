CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-02-21T00:35:34Z creation;2017-02-21T00:35:37Z conversion to V3.1;2019-12-19T08:14:30Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170221003534  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               [A   JA  I2_0577_091                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�����1   @��O���@3Ж�����d�ح��V1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~{@��
@��
A�A=�A]�A}�A���A���A���A���A�A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BG{BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/�RC1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�HDqHD��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUqHDU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De�Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�B=D�_
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AēuAĕ�Aĕ�Aė�Aę�Aę�AēuAēuA��Aº^A���A���A��^A��-A��A���A�v�A�n�A�dZA�VA�K�A�E�A�9XA�"�A���A��#A��A�1A���A���A��^A���A�z�A�O�A�K�A�5?A�(�A�+A�"�A��A��A��A�{A�bA�
=A�1A�A���A�ĜA�ZA�"�A��mA��\A�A���A���A�33A�%A��yA��FA�C�A�9XA��
A�ĜA�\)A�XA�G�A�33A��-A��mA��mA��-A���A���A��A��A��A��yA��;A��\A��A���A���A�t�A�/A�{A��9A�jA���A�|�A�dZA�VA�A�A�&�A��A��\A���A���A��A��jA�r�A�1'A�XA���A��-A��A���A�ZA�{A�A~��A}O�A{�Azr�AxȴAv~�Ar�Ao�An�+Am/Al��Ak+AgS�Ab  A`�!AZĜAX��AX�AW�^AWXAV��AVE�AU��AU7LAR  AO�7AOoAN�+AMAL1AI��AG?}AF��AF  AD��ACdZA?�PA=S�A<��A<9XA;�PA:��A9�FA8�A7;dA5��A5�PA4�uA4A3��A3XA3+A2v�A1��A1C�A0�`A0ZA/S�A. �A,r�A*��A*1A)K�A(�jA(1A&A�A%
=A$M�A#�FA#XA#A"��A"1'A �A  A�-AC�AA�DA��AC�AVAt�A�A�+A�PA�\A�A`BA+AZAVA"�A�RAA�AXAn�AoAZAt�A
��A
r�A
JA	\)A	
=A-A��A�-A��A=qA��AdZAt�AO�Ao@��F@���@���@��\@���@�"�@�E�@���@�7L@��@���@�Z@�S�@�n�@�`B@��@�F@�C�@�M�@��#@�Ĝ@�1@�33@�@�  @��@�M�@�$�@�dZ@�j@ڸR@�p�@���@��;@֏\@�O�@��/@���@Ӆ@�"�@�~�@�-@ёh@�?}@д9@� �@��
@�|�@��@Η�@���@�(�@˥�@�o@�{@���@�C�@��@��@���@þw@�t�@�@��@�Z@�I�@�Z@���@��@��@��7@�O�@�?}@���@��u@��D@�z�@�I�@�1@��@�"�@��T@��@�V@�{@��@�@��7@�r�@��w@�"�@�bN@��m@��;@��w@��P@�C�@�+@�E�@��@�J@�$�@��7@���@�A�@���@��y@���@��R@�ȴ@���@�J@���@��;@�S�@�~�@�{@���@���@��^@��h@�&�@� �@�C�@��+@�$�@�@���@��^@�p�@�&�@���@�z�@� �@��@��
@��F@��@�|�@�\)@�
=@��@��+@�=q@��#@���@���@�hs@�7L@�V@��j@��@�z�@�Z@�A�@� �@�  @���@��;@���@��w@���@�l�@�\)@�C�@�;d@��\@��#@���@��-@��^@���@�G�@��@�%@��@��j@�Z@�1@��m@��F@���@�|�@�l�@�\)@�33@�@���@��@�@���@��^@�@���@�&�@�/@�7L@��@��u@�z�@���@�j@��j@��@�&�@��@�V@�%@�Ĝ@���@���@��u@��D@�z�@�j@�Z@�(�@��;@��@�S�@��@���@�^5@�E�@��@�p�@�O�@�&�@���@���@���@��D@�Q�@�1@���@��P@�dZ@���@��\@�v�@�V@�V@�M�@�=q@�{@���@���@�X@�?}@�&�@���@��j@��D@��@�z�@�j@�A�@�1@��
@�ƨ@���@�dZ@�\)@�\)@�"�@���@��@���@�~�@�M�@�$�@�@���@�x�@�G�@�V@���@���@�r�@�A�@�9X@�1'@�(�@� �@�ƨ@�dZ@��@���@���@�ff@�5?@�J@��@��#@��-@�7L@���@�A�@��
@���@�t�@�l�@�"�@���@��y@��y@��y@���@��R@���@��\@�n�@�ff@�^5@�E�@�=q@�=q@�M�@�J@��@�V@�r�@� �@��@��@��@�@
=@~ȴ@}�h@|�j@|�@{�F@{dZ@{"�@z��@z^5@y�@yG�@x�@x  @w��@w�P@w|�@w
=@v�R@vE�@u@uO�@t�j@tj@tj@tj@tZ@tI�@s��@s��@s�@sS�@r�@r=q@q�#@q�7@qX@q%@o�@o��@o
=@nȴ@n�+@nV@m�@m�h@m/@l�@l�D@lZ@k��@k"�@j��@j�!@j��@j��@j~�@j�@i��@i�7@iX@h��@hb@h  @g�@g�@g�@g�@g��@gl�@f�R@f5?@e`B@d�@d�D@dI�@d9X@d�@c��@c33@b^5@`bN@`A�@_��@_;d@^�@^��@^V@\�@\�@[��@[S�@["�@[@Z�!@Zn�@Z^5@Z^5@Z=q@Y��@YG�@X��@XQ�@W�P@Wl�@W�@Vv�@V$�@V@U�T@U�-@UO�@T�@TI�@T�@S�m@S�F@S��@S@R��@R�!@R^5@Q��@Q��@Qx�@Q7L@P�`@O�@O�@O
=@N�@N�R@N�R@N�+@N$�@M�-@M`B@L��@L�@L�D@L(�@L1@K��@K"�@J��@Jn�@J=q@JJ@I�#@I�^@I�7@H��@H�u@HbN@H1'@H �@Hb@H  @G�;@G��@G\)@GK�@G;d@F��@F��@F��@Fff@F{@E�-@E�@D��@D�@D�D@D�D@Dz�@Dj@C��@C33@B^5@BM�@BM�@A��@@��@@ �@@b@?�@?��@?��@?+@>��@>�y@>�@>�R@>E�@=�-@<�j@;��@:=q@:J@9�@9�#@9�^@9�7@9x�@9G�@9�@8�`@8Ĝ@8Ĝ@8��@8bN@8 �@7�@7�w@7�w@7�w@7|�@7;d@7
=@6��@6�y@6��@6��@6�+@6V@65?@5��@5�@4�/@4j@4�@41@3ƨ@3�F@3��@3��@3�@3�@3t�@3t�@3dZ@3C�@3C�@3C�@3C�@333@3"�@3o@2�@2��@2�@1�#@1x�@1G�@0��@0��@0�@0r�@0 �@0 �@0b@/�@/��@/�P@/
=@.�+@-��@-`B@,�@,j@,�@+ƨ@+��@+dZ@+33@+o@*�@*��@*^5@*-@)��@)7L@(�`@(Ĝ@(��@( �@'�@'��@'l�@'+@'�@&��@&ȴ@&�+@&5?@%�@%�h@%p�@%`B@%?}@%V@$�/@$Z@#ƨ@#��@#C�@#@"�@"�H@"�!@!�@!G�@ �`@ ��@ �9@ �@ Q�@   @�@|�@��@V@�T@��@�@�@`B@O�@/@�@�j@9X@ƨ@t�@S�@33@��@��@�!@��@~�@^5@-@J@�#@��@G�@�`@�u@Q�@�;@l�@�@�y@V@�-@`B@�@��@��@�j@��@(�@�F@dZ@C�@C�@33@"�@o@��@M�@-@J@�@��@��@��@��@hs@hs@X@G�@G�@7L@&�@%@�9@�@r�@bN@ �@�@�w@��@|�@\)@�@�y@��@E�@@�T@��@�@p�@/@�/@��@�D@I�@1@��@��@��@�@�@t�@dZ@dZ@C�@"�@
�@
��@
��@
��@
��@
��@
��@
��@
�!@
��@
��@
�!@
�!@
�!@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AēuAĕ�Aĕ�Aė�Aę�Aę�AēuAēuA��Aº^A���A���A��^A��-A��A���A�v�A�n�A�dZA�VA�K�A�E�A�9XA�"�A���A��#A��A�1A���A���A��^A���A�z�A�O�A�K�A�5?A�(�A�+A�"�A��A��A��A�{A�bA�
=A�1A�A���A�ĜA�ZA�"�A��mA��\A�A���A���A�33A�%A��yA��FA�C�A�9XA��
A�ĜA�\)A�XA�G�A�33A��-A��mA��mA��-A���A���A��A��A��A��yA��;A��\A��A���A���A�t�A�/A�{A��9A�jA���A�|�A�dZA�VA�A�A�&�A��A��\A���A���A��A��jA�r�A�1'G�O�G�O�G�O�A��A���A�ZA�{A�G�O�G�O�A{�Azr�AxȴAv~�Ar�Ao�An�+Am/Al��Ak+AgS�Ab  A`�!AZĜAX��AX�AW�^AWXAV��AVE�AU��AU7LAR  AO�7AOoAN�+AMAL1AI��AG?}AF��AF  AD��ACdZA?�PA=S�A<��A<9XA;�PA:��A9�FA8�A7;dA5��A5�PA4�uA4A3��A3XA3+A2v�A1��A1C�A0�`A0ZA/S�A. �A,r�A*��A*1A)K�A(�jA(1A&A�A%
=A$M�A#�FA#XA#A"��A"1'A �A  A�-AC�AA�DA��AC�AVAt�A�A�+A�PA�\A�A`BA+AZAVA"�A�RAA�AXAn�AoAZAt�A
��A
r�A
JA	\)A	
=A-A��A�-A��A=qA��AdZAt�AO�Ao@��F@���@���@��\@���@�"�@�E�@���@�7L@��@���@�Z@�S�@�n�@�`B@��@�F@�C�@�M�@��#@�Ĝ@�1@�33@�@�  @��@�M�@�$�@�dZ@�j@ڸR@�p�@���@��;@֏\@�O�@��/@���@Ӆ@�"�@�~�@�-@ёh@�?}@д9@� �@��
@�|�@��@Η�@���@�(�@˥�@�o@�{@���@�C�@��@��@���@þw@�t�@�@��@�Z@�I�@�Z@���@��@��@��7@�O�@�?}@���@��u@��D@�z�@�I�@�1@��@�"�@��T@��@�V@�{@��@�@��7@�r�@��w@�"�@�bN@��m@��;@��w@��P@�C�@�+@�E�@��@�J@�$�@��7@���@�A�@���@��y@���@��R@�ȴ@���@�J@���@��;@�S�@�~�@�{@���@���@��^@��h@�&�@� �@�C�@��+@�$�@�@���@��^@�p�@�&�@���@�z�@� �@��@��
@��F@��@�|�@�\)@�
=@��@��+@�=q@��#@���@���@�hs@�7L@�V@��j@��@�z�@�Z@�A�@� �@�  @���@��;@���@��w@���@�l�@�\)@�C�@�;d@��\@��#@���@��-@��^@���@�G�@��@�%@��@��j@�Z@�1@��m@��F@���@�|�@�l�@�\)@�33@�@���@��@�@���@��^@�@���@�&�@�/@�7L@��@��u@�z�@���@�j@��j@��@�&�@��@�V@�%@�Ĝ@���@���@��u@��D@�z�@�j@�Z@�(�@��;@��@�S�@��@���@�^5@�E�@��@�p�@�O�@�&�@���@���@���@��D@�Q�@�1@���@��P@�dZ@���@��\@�v�@�V@�V@�M�@�=q@�{@���@���@�X@�?}@�&�@���@��j@��D@��@�z�@�j@�A�@�1@��
@�ƨ@���@�dZ@�\)@�\)@�"�@���@��@���@�~�@�M�@�$�@�@���@�x�@�G�@�V@���@���@�r�@�A�@�9X@�1'@�(�@� �@�ƨ@�dZ@��@���@���@�ff@�5?@�J@��@��#@��-@�7L@���@�A�@��
@���@�t�@�l�@�"�@���@��y@��y@��y@���@��R@���@��\@�n�@�ff@�^5@�E�@�=q@�=q@�M�@�J@��@�V@�r�@� �@��@��@��@�@
=@~ȴ@}�h@|�j@|�@{�F@{dZ@{"�@z��@z^5@y�@yG�@x�@x  @w��@w�P@w|�@w
=@v�R@vE�@u@uO�@t�j@tj@tj@tj@tZ@tI�@s��@s��@s�@sS�@r�@r=q@q�#@q�7@qX@q%@o�@o��@o
=@nȴ@n�+@nV@m�@m�h@m/@l�@l�D@lZ@k��@k"�@j��@j�!@j��@j��@j~�@j�@i��@i�7@iX@h��@hb@h  @g�@g�@g�@g�@g��@gl�@f�R@f5?@e`B@d�@d�D@dI�@d9X@d�@c��@c33@b^5@`bN@`A�@_��@_;d@^�@^��@^V@\�@\�@[��@[S�@["�@[@Z�!@Zn�@Z^5@Z^5@Z=q@Y��@YG�@X��@XQ�@W�P@Wl�@W�@Vv�@V$�@V@U�T@U�-@UO�@T�@TI�@T�@S�m@S�F@S��@S@R��@R�!@R^5@Q��@Q��@Qx�@Q7L@P�`@O�@O�@O
=@N�@N�R@N�R@N�+@N$�@M�-@M`B@L��@L�@L�D@L(�@L1@K��@K"�@J��@Jn�@J=q@JJ@I�#@I�^@I�7@H��@H�u@HbN@H1'@H �@Hb@H  @G�;@G��@G\)@GK�@G;d@F��@F��@F��@Fff@F{@E�-@E�@D��@D�@D�D@D�D@Dz�@Dj@C��@C33@B^5@BM�@BM�@A��@@��@@ �@@b@?�@?��@?��@?+@>��@>�y@>�@>�R@>E�@=�-@<�j@;��@:=q@:J@9�@9�#@9�^@9�7@9x�@9G�@9�@8�`@8Ĝ@8Ĝ@8��@8bN@8 �@7�@7�w@7�w@7�w@7|�@7;d@7
=@6��@6�y@6��@6��@6�+@6V@65?@5��@5�@4�/@4j@4�@41@3ƨ@3�F@3��@3��@3�@3�@3t�@3t�@3dZ@3C�@3C�@3C�@3C�@333@3"�@3o@2�@2��@2�@1�#@1x�@1G�@0��@0��@0�@0r�@0 �@0 �@0b@/�@/��@/�P@/
=@.�+@-��@-`B@,�@,j@,�@+ƨ@+��@+dZ@+33@+o@*�@*��@*^5@*-@)��@)7L@(�`@(Ĝ@(��@( �@'�@'��@'l�@'+@'�@&��@&ȴ@&�+@&5?@%�@%�h@%p�@%`B@%?}@%V@$�/@$Z@#ƨ@#��@#C�@#@"�@"�H@"�!@!�@!G�@ �`@ ��@ �9@ �@ Q�@   @�@|�@��@V@�T@��@�@�@`B@O�@/@�@�j@9X@ƨ@t�@S�@33@��@��@�!@��@~�@^5@-@J@�#@��@G�@�`@�u@Q�@�;@l�@�@�y@V@�-@`B@�@��@��@�j@��@(�@�F@dZ@C�@C�@33@"�@o@��@M�@-@J@�@��@��@��@��@hs@hs@X@G�@G�@7L@&�@%@�9@�@r�@bN@ �@�@�w@��@|�@\)@�@�y@��@E�@@�T@��@�@p�@/@�/@��@�D@I�@1@��@��@��@�@�@t�@dZ@dZ@C�@"�@
�@
��@
��@
��@
��@
��@
��@
��@
�!@
��@
��@
�!@
�!@
�!@
�!11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B(�B(�B(�B(�B(�B(�B(�B'�B5?BXB`BBaHBbNBbNBbNBcTBe`Be`BffBffBgmBiyBiyBl�Bp�Br�B|�B�B�B�B�B�%B�7B�DB�PB�\B�VB�bB�oB�oB�oB�uB��B��B��B��B��B��B�B�3B�?B�XB��B��B��BɺB��B�qB�wB��B��B��B�B�`B��B��B��B��B��B��B��B�RB��B�=B�JBk�Bu�BcTBJ�B.BhB �B�
B�'B��B��B��B��B�oB�uB�{B��B��B��B��B��B�hB{�BF�BBB
�B
��B
e`B	��B	��B	�/B	�B
�B
0!B
!�B	�B
+B
R�B
P�B
8RB
�B
1B
B	��B	��B	�B	�B	��B	��B	�B	m�B	iyB	gmB	dZB	aHB	_;B	[#B	XB	VB	H�B	D�B	B�B	=qB	8RB	.B	!�B	�B	�B	{B	PB	B��B��B�B�B�B�sB�TB�;B�#B�B�B��B��B��B��B��B��BȴBǮBŢB��B�wB�^B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B�\B�\B�PB�PB�JB�=B�1B�+B�B�B�B�B�B�7B�{B��B��B��B��B�B�B�9B�dBBŢBȴB��BȴBǮBǮBƨBǮB��BɺBǮB�?B�-B�?B�}BB��B��BȴBƨBÖB��B�wB�dB�^B�^B�^B�^B�^B�XB�^B�dB�jB�jB�jB�qB�qB�}B�}B��BBƨBƨBŢBĜBȴBŢBȴB��B��B��B��B�B�B�B�B�)B�;B�;B�BB�BB�NB�ZB�fB�sB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	%B	B	%B	+B	
=B	DB	DB	JB	\B	{B	�B	�B	!�B	$�B	&�B	+B	/B	49B	49B	49B	33B	2-B	2-B	33B	49B	49B	7LB	8RB	9XB	;dB	:^B	:^B	;dB	<jB	<jB	?}B	B�B	C�B	L�B	O�B	Q�B	Q�B	P�B	O�B	VB	XB	YB	ZB	_;B	hsB	k�B	k�B	k�B	k�B	l�B	q�B	u�B	v�B	v�B	x�B	x�B	{�B	}�B	�B	�B	�B	�+B	�DB	�DB	�JB	�PB	�VB	�bB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�9B	�?B	�FB	�FB	�FB	�9B	�LB	�dB	�jB	�qB	�}B	��B	��B	��B	B	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�#B	�5B	�NB	�`B	�`B	�fB	�fB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
PB
PB
PB
VB
\B
\B
\B
\B
bB
hB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144411111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B)B(�B(�B(�B)B)DB)_B)yB7�BYB`vBa�BbhBbhBb�Bc�BezBezBf�Bf�Bg�Bi�Bi�BmBqBs�B}�B�aB�GB�aB�gB�tB��B�^B��B��B�pB�}B�oB�oB�oB��B��B��B��B��B�B��B��B��B��B�^B��B�uB�AB��B�B�(B��B��B҉BՁBںB�_B��B�0B�~B�vBԕB�B��B�6B��B�PB��BncBz^Bh$BO�B2aB�B'8B�QB��B��B�`B��B�@B��B��B��B��B�B�yB��B��B�9B��BM�B�B_B
�G�O�G�O�G�O�B	�{B	�B	��B
�B
1�G�O�G�O�B
,�B
T�B
T,B
=B
KB

	B
�B	�JB	��B	�oB	یB	�]B	��B	�aB	n}B	jB	h
B	e,B	bB	`'B	\�B	[�B	XyB	I�B	E�B	C�B	@ B	;dB	0�B	"�B	B	�B	$B	�B	�B��B��B��B��B�/B�B��B�B��B�QB��B�uB҉BуB� B��B�^BɆB��B�+B�[B��B��B�B�GB�;B�wB�"B��B��B��B�pB�jB��B�B�WB��B�B�B��B�"B�jB�DB��B�fB��B��B��B��B��B�B��B��B�B��B��B�B�oB��B�"B��B�zB�xBˬBɆBȴBȚB�BɠB�dB�xBʦB�B��B��B� B��B҉BбBɠBǮB��BB�.B��B��B��B��B��B�B�B�0B�PB��B��B�<B��B�]B�B�iB��B��BǔB�B�YB��BʌB��BɠB�pB��B��B��BؓB��BچBچB��BߤB߾B��B��B�B��B��B��B�)B��B�hB�TB�TB��B��B��B��B�B�]B	;B	�B	�B	B	�B	YB	_B	
�B	�B	�B	�B	�B	�B	�B		B	!�B	$�B	'8B	+QB	/�B	4�B	5tB	5�B	4B	2aB	2aB	3hB	4�B	5%B	8B	9>B	:�B	;�B	:xB	:�B	;�B	<�B	<�B	@ B	B�B	C�B	MB	P}B	RoB	RoB	QhB	P}B	VSB	X+B	YKB	Z�B	_�B	iDB	lWB	lB	lB	k�B	l�B	q�B	vB	wB	wfB	y�B	y�B	|�B	~]B	�;B	�GB	�SB	�zB	��B	��B	��B	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�2B	�$B	�>B	�*B	�B	�6B	�=B	�)B	�)B	�IB	�5B	�OB	�[B	�nB	�ZB	��B	��B	��B	��B	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�6B	�"B	� B	��B	�?B	�KB	�KB	�+B	�1B	�eB	�B	�7B	�B	�CB	��B	�5B	�hB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�%B	��B	�B	�B	��B	�B	�B	�B	�$B	�B	�*B	�0B	�JB	�B	�B	�B	��B	�"B	�"B	�<B	�(B	�.B	�HB
 4B
 4B
 OB
;B
;B
'B
'B
AB
[B
[B
AB
-B
aB
MB
3B
MB
MB
9B
SB
YB
YB
tB
EB
_B
zB
zB
fB
�B
	�B
	�B
	RB

XB

XB

XB

rB

rB

�B
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
	B
B
B
�B
�B
�B
�B
B
�B
�B
B
B
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
!B
!B
 �B
!�B
"B
"B
#B
#B
$B
$�B
$�B
%B
&B
'B
(>B
($B
($B
(>B
)*B
)B
*0B
*B
)�B
*0B
*KB
*0B
*B
*0B
+QB
+QB
,"B
,B
,"B
,B
,"B
,"B
,WB
-]B
-CB
.}B
.IB
.cB
/OB
/OB
/OB
/OB
/iB
/�B
0�B
1AB
1[B
1vB
1vB
2aB
2|B
2�B
3�B
4nB
4TB
5tB
5tB
5tB
5tB
5?B
5ZB
5ZB
6zB
6�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
9�B
9rB
9�B
9�B
9�B
:xB
:�B
:�B
:�B
:�B
:�B
;B
;B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=qB
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
LB
MPB
NVB
O\B
O�B
QB
Q B
QB
QB
QB
QB
RB
RB
RB
RB
RB
RB
RB
R B
S&B
SB
SB
SB
S&B
S&B
SB
SB
S&B
S&B
TB
TB
TB
TFB
TFB
U2B
UMB
U2B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
V9B
VSB
W?B
W?B
W?B
W$B
W?B
W$B
XEB
X+B
XEB
XB
X+B
XEB
XEB
X+B
X_B
YeB
YKB
ZQB
ZQB
ZQB
ZQB
[=B
[WB
[WB
[WB
[WB
\]B
\CB
\CB
\]B
\]B
\�B
]IB
^jB
^jB
^�B
^�B
_VB
_VB
_VB
_VB
_pB
_pB
`vB
`\B
`vB
`vB
a|B
a|B
a|B
a|B
a|B
a�B
a�B
b�B
b�B
bhB
bNB
b�B
b�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
xB
xB
w�B
y	B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144433333443111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201702250037212017022500372120170225003721201806221309362018062213093620180622130936201804050710272018040507102720180405071027  JA  ARFMdecpA19c                                                                20170221093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170221003534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170221003535  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170221003536  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170221003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170221003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170221003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20170221003537  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20170221003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170221003537  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20170221003537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170221003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20170221010223                      G�O�G�O�G�O�                JA  ARUP                                                                        20170221040218                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170221153216  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20170221153216  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20170221153216  CV  LATITUDE        G�O�G�O�A�~�                JM  ARSQJMQC2.0                                                                 20170222000000  CF  PSAL_ADJUSTED_QCCV  Cd  G�O�                JM  ARCAJMQC2.0                                                                 20170224153721  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170224153721  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221027  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040936  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                