CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-07-18T00:36:28Z creation;2017-07-18T00:36:33Z conversion to V3.1;2019-12-19T08:02:16Z update;     
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `L   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̜   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �(   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �,   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170718003628  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_140                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�uͿ��1   @�v����@3��x����d�H��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~{@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�B�HBz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-�C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[�C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw�Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:�HD;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DB~DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D��
D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�8�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�
D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�1'A�-A�/A�(�A�&�A�+A�+A�+A�-A�/A�-A�+A�+A�+A�+A�&�A�(�A�&�A��A���A�jA�?}A��A�(�Aٕ�A�VA؍PAף�A�ZAԕ�A�9XAоwA�M�A�VA���A��#A���AʃA�dZA��A� �AȲ-A�9XA��A�ĜA���Aç�A�1'A�^5A�\)A�oA��PA�1A���A�S�A��
A���A�A���A���A��FA�;dA�
=A��A��jA��A���A�M�A���A���A�%A��FA���A��A���A���A�Q�A���A�I�A���A�C�A��A���A��!A��9A�|�A�E�A��;A�+A�G�A�+A��jA�(�A��A�n�A�A�A���A�;dA��/A��\A�K�A���A���A���A��A��A�A��jA��9A�=qA�  A��HA�O�A��A~9XA}�-A}G�A|VA{�A{G�Ay�Ax�Aw+Av  As�mArI�Ao�FAl��AkXAk��Ak33Ai��Ag��Aex�Aa�;A_O�A^��A\$�AY�TAV�AR��ARbNAQ�wAPz�AO+ANffAL�AJ��AI`BAHJAF�yAFM�AE��AD�AB�A@�`A@9XA>9XA;�;A;&�A:�A8~�A81A6�+A45?A1�A0�A0�A/A/G�A.jA,�A+�A+"�A*�jA*(�A(�A'�wA&A%t�A%%A$ZA"I�A v�At�AA�A\)AXAM�AhsA��A��A9XAĜA �A��A�`A��Ax�A^5A�AI�A1A�PA&�Av�AA
=A
�RA	�A	�A	dZA��AVAS�A�A�A$�A�-A�7A+A
=A��A ĜA �@�"�@�$�@���@��-@���@���@�V@�1'@�o@���@�@�$�@�@�M�@�@�5?@���@�X@�u@�
=@ᙚ@�n�@�9X@ى7@�E�@�/@��m@�M�@�%@��m@�;d@�o@�V@͡�@�bN@��@ʸR@�{@ɲ-@ȣ�@�1'@��y@�V@�%@���@þw@öF@�l�@���@���@�7L@��@�33@��R@�~�@�^5@��@���@�|�@���@�{@��T@�G�@���@���@���@��@�t�@�"�@��@�n�@���@�`B@��@�I�@�+@��+@��#@�/@��9@���@���@��D@�A�@��w@���@���@���@���@���@���@���@���@�r�@�Z@� �@���@�o@��@��!@���@�v�@�^5@�5?@���@���@�G�@��@��9@��@�Z@� �@��
@��w@��F@��F@���@�\)@�o@��H@��!@�5?@��#@��h@�x�@�`B@�G�@��@��j@�z�@�I�@���@�ƨ@�t�@�K�@�o@��y@���@�~�@�{@���@�X@��@��/@�j@�I�@�9X@�1'@� �@��@�b@��
@�t�@�"�@���@���@��\@�^5@�5?@�$�@��7@�O�@�G�@��/@��@��;@���@�\)@�33@��@�ȴ@���@�ff@�-@�@��#@�x�@�X@�&�@���@��j@��@�j@�A�@�1@��@��@�C�@���@��R@��!@��!@��@���@��7@��@�%@���@��@�I�@��@��w@���@��@�dZ@�C�@�@�ȴ@���@�ff@�{@��#@��-@���@��@�p�@�/@���@��j@��9@��D@��@��;@��w@�dZ@�o@���@��@���@��\@��+@�n�@�V@��@���@�p�@�?}@�%@��j@��D@��@�j@�bN@�Z@�Q�@���@�dZ@�@��@��!@�~�@�v�@�$�@��@��-@��@�hs@�`B@�X@�O�@�?}@���@��D@�I�@�A�@�9X@�  @��;@���@��w@�t�@�;d@�@��@��y@��H@��@��@��!@��\@�n�@�M�@��@��@�X@���@��/@�Ĝ@��@�Z@�(�@��m@�t�@�@��@���@�^5@�{@��#@���@���@���@��@�`B@�`B@�&�@��j@���@�bN@�1'@�P@�@~ȴ@~ff@~$�@}��@}?}@|�@|��@|�@{ƨ@{�@{33@z�H@z�!@z�\@zn�@z-@y��@y�7@y%@xQ�@x  @w�@wl�@w\)@w+@v�y@v�R@v{@u�@uV@t��@t�j@tj@s�@s@r�H@r�!@r��@r�\@r~�@r~�@r-@q�@qx�@qX@qG�@q&�@p��@p�9@pA�@p  @o|�@oK�@o
=@nv�@n$�@m�-@m?}@l��@lz�@lz�@lj@lZ@l1@k�m@k�F@kdZ@ko@jn�@i�#@i�@h�`@h�9@h��@h�u@hQ�@hb@g�@g��@g+@g�@g
=@f��@f��@f�y@f��@fff@f$�@e@e�@ep�@e�@d��@d�D@dz�@dj@d(�@cdZ@b��@bn�@b^5@b�@a�^@a7L@a�@`��@`r�@`Q�@`  @_\)@^�R@^v�@^$�@]��@]�-@]�-@]�@]?}@\�@\j@\I�@\(�@[��@[dZ@Z�@Z��@Zn�@Y�7@X�`@X1'@Xb@W�;@W+@V�+@V�+@Vv�@V@Up�@UV@T��@Tj@TZ@T(�@T1@S�F@S"�@R^5@Q�#@Q��@Q�7@Q7L@P�`@PĜ@P1'@O|�@N��@N�+@NE�@N{@M�-@M/@L�@Lj@Kƨ@KdZ@K"�@J��@J��@J-@I��@I��@IX@I7L@I&�@I%@H�`@HĜ@HbN@H  @G�@F��@F��@F�+@FE�@F$�@E�@E@E��@EO�@E�@D�/@D�j@Dj@D(�@C�m@C�F@CdZ@B�@B�H@B��@B�!@B�!@Bn�@BJ@A��@@��@@�u@@r�@@A�@?�;@?l�@?;d@?�@>�y@>��@>V@>5?@=��@=?}@<�/@<��@<(�@<1@;�
@;33@:��@:M�@:�@9�@9��@9��@9x�@9&�@8�`@8�@8A�@8b@7�w@7l�@6ȴ@6V@6E�@6E�@65?@6{@6{@6@5�@5�-@5`B@4�@4��@49X@4�@3��@3�
@3��@3t�@3"�@3o@3@2��@2�\@2�@1hs@1%@0Ĝ@0bN@0  @/�@/�P@/|�@/
=@.��@.v�@.V@-�@-�-@-p�@-O�@,�@,��@,j@,(�@+�
@+S�@*�H@*�\@*n�@*-@)�@)��@)hs@)�@(�u@'�@'�w@'�@'l�@'\)@&��@&��@&ff@&{@%�T@%@%�h@%`B@%O�@%/@%V@$�@$�/@$�@$��@$�D@$�D@$j@$Z@$9X@$�@$1@#�
@#ƨ@#��@#33@#o@"�@"��@"n�@"-@!��@!��@!x�@!x�@!x�@!x�@!x�@!hs@!7L@!%@ �`@ ��@ �9@ ��@ �u@ �u@ r�@ Q�@ 1'@ b@�;@�w@�P@K�@�@�@ȴ@�R@��@��@ff@5?@{@�T@��@�h@p�@V@�/@�j@�@j@I�@�@ƨ@t�@"�@@��@�\@M�@-@��@�^@x�@X@G�@�@�`@Ĝ@�u@Q�@1'@ �@b@��@�P@l�@K�@;d@
=@�@�R@��@V@5?@{@�T@��@�-@��@`B@O�@�@��@��@z�@9X@9X@(�@�@�@1@�m@��@dZ@33@33@"�@o@��@��@�!@^5@�@�@��@��@G�@��@Ĝ@�9@��@1'@�w@|�@l�@l�@\)@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�1'A�-A�/A�(�A�&�A�+A�+A�+A�-A�/A�-A�+A�+A�+A�+A�&�A�(�A�&�A��A���A�jA�?}A��A�(�Aٕ�A�VA؍PAף�A�ZAԕ�A�9XAоwA�M�A�VA���A��#A���AʃA�dZA��A� �AȲ-A�9XA��A�ĜA���Aç�A�1'A�^5A�\)A�oA��PA�1A���A�S�A��
A���A�A���A���A��FA�;dA�
=A��A��jA��A���A�M�A���A���A�%A��FA���A��A���A���A�Q�A���A�I�A���A�C�A��A���A��!A��9A�|�A�E�A��;A�+A�G�A�+A��jA�(�A��A�n�A�A�A���A�;dA��/A��\A�K�A���A���A���A��A��A�A��jA��9A�=qA�  A��HA�O�A��A~9XA}�-A}G�A|VA{�A{G�Ay�Ax�Aw+Av  As�mArI�Ao�FAl��AkXAk��Ak33Ai��Ag��Aex�Aa�;A_O�A^��A\$�AY�TAV�AR��ARbNAQ�wAPz�AO+ANffAL�AJ��AI`BAHJAF�yAFM�AE��AD�AB�A@�`A@9XA>9XA;�;A;&�A:�A8~�A81A6�+A45?A1�A0�A0�A/A/G�A.jA,�A+�A+"�A*�jA*(�A(�A'�wA&A%t�A%%A$ZA"I�A v�At�AA�A\)AXAM�AhsA��A��A9XAĜA �A��A�`A��Ax�A^5A�AI�A1A�PA&�Av�AA
=A
�RA	�A	�A	dZA��AVAS�A�A�A$�A�-A�7A+A
=A��A ĜA �@�"�@�$�@���@��-@���@���@�V@�1'@�o@���@�@�$�@�@�M�@�@�5?@���@�X@�u@�
=@ᙚ@�n�@�9X@ى7@�E�@�/@��m@�M�@�%@��m@�;d@�o@�V@͡�@�bN@��@ʸR@�{@ɲ-@ȣ�@�1'@��y@�V@�%@���@þw@öF@�l�@���@���@�7L@��@�33@��R@�~�@�^5@��@���@�|�@���@�{@��T@�G�@���@���@���@��@�t�@�"�@��@�n�@���@�`B@��@�I�@�+@��+@��#@�/@��9@���@���@��D@�A�@��w@���@���@���@���@���@���@���@���@�r�@�Z@� �@���@�o@��@��!@���@�v�@�^5@�5?@���@���@�G�@��@��9@��@�Z@� �@��
@��w@��F@��F@���@�\)@�o@��H@��!@�5?@��#@��h@�x�@�`B@�G�@��@��j@�z�@�I�@���@�ƨ@�t�@�K�@�o@��y@���@�~�@�{@���@�X@��@��/@�j@�I�@�9X@�1'@� �@��@�b@��
@�t�@�"�@���@���@��\@�^5@�5?@�$�@��7@�O�@�G�@��/@��@��;@���@�\)@�33@��@�ȴ@���@�ff@�-@�@��#@�x�@�X@�&�@���@��j@��@�j@�A�@�1@��@��@�C�@���@��R@��!@��!@��@���@��7@��@�%@���@��@�I�@��@��w@���@��@�dZ@�C�@�@�ȴ@���@�ff@�{@��#@��-@���@��@�p�@�/@���@��j@��9@��D@��@��;@��w@�dZ@�o@���@��@���@��\@��+@�n�@�V@��@���@�p�@�?}@�%@��j@��D@��@�j@�bN@�Z@�Q�@���@�dZ@�@��@��!@�~�@�v�@�$�@��@��-@��@�hs@�`B@�X@�O�@�?}@���@��D@�I�@�A�@�9X@�  @��;@���@��w@�t�@�;d@�@��@��y@��H@��@��@��!@��\@�n�@�M�@��@��@�X@���@��/@�Ĝ@��@�Z@�(�@��m@�t�@�@��@���@�^5@�{@��#@���@���@���@��@�`B@�`B@�&�@��j@���@�bN@�1'@�P@�@~ȴ@~ff@~$�@}��@}?}@|�@|��@|�@{ƨ@{�@{33@z�H@z�!@z�\@zn�@z-@y��@y�7@y%@xQ�@x  @w�@wl�@w\)@w+@v�y@v�R@v{@u�@uV@t��@t�j@tj@s�@s@r�H@r�!@r��@r�\@r~�@r~�@r-@q�@qx�@qX@qG�@q&�@p��@p�9@pA�@p  @o|�@oK�@o
=@nv�@n$�@m�-@m?}@l��@lz�@lz�@lj@lZ@l1@k�m@k�F@kdZ@ko@jn�@i�#@i�@h�`@h�9@h��@h�u@hQ�@hb@g�@g��@g+@g�@g
=@f��@f��@f�y@f��@fff@f$�@e@e�@ep�@e�@d��@d�D@dz�@dj@d(�@cdZ@b��@bn�@b^5@b�@a�^@a7L@a�@`��@`r�@`Q�@`  @_\)@^�R@^v�@^$�@]��@]�-@]�-@]�@]?}@\�@\j@\I�@\(�@[��@[dZ@Z�@Z��@Zn�@Y�7@X�`@X1'@Xb@W�;@W+@V�+@V�+@Vv�@V@Up�@UV@T��@Tj@TZ@T(�@T1@S�F@S"�@R^5@Q�#@Q��@Q�7@Q7L@P�`@PĜ@P1'@O|�@N��@N�+@NE�@N{@M�-@M/@L�@Lj@Kƨ@KdZ@K"�@J��@J��@J-@I��@I��@IX@I7L@I&�@I%@H�`@HĜ@HbN@H  @G�@F��@F��@F�+@FE�@F$�@E�@E@E��@EO�@E�@D�/@D�j@Dj@D(�@C�m@C�F@CdZ@B�@B�H@B��@B�!@B�!@Bn�@BJ@A��@@��@@�u@@r�@@A�@?�;@?l�@?;d@?�@>�y@>��@>V@>5?@=��@=?}@<�/@<��@<(�@<1@;�
@;33@:��@:M�@:�@9�@9��@9��@9x�@9&�@8�`@8�@8A�@8b@7�w@7l�@6ȴ@6V@6E�@6E�@65?@6{@6{@6@5�@5�-@5`B@4�@4��@49X@4�@3��@3�
@3��@3t�@3"�@3o@3@2��@2�\@2�@1hs@1%@0Ĝ@0bN@0  @/�@/�P@/|�@/
=@.��@.v�@.V@-�@-�-@-p�@-O�@,�@,��@,j@,(�@+�
@+S�@*�H@*�\@*n�@*-@)�@)��@)hs@)�@(�u@'�@'�w@'�@'l�@'\)@&��@&��@&ff@&{@%�T@%@%�h@%`B@%O�@%/@%V@$�@$�/@$�@$��@$�D@$�D@$j@$Z@$9X@$�@$1@#�
@#ƨ@#��@#33@#o@"�@"��@"n�@"-@!��@!��@!x�@!x�@!x�@!x�@!x�@!hs@!7L@!%@ �`@ ��@ �9@ ��@ �u@ �u@ r�@ Q�@ 1'@ b@�;@�w@�P@K�@�@�@ȴ@�R@��@��@ff@5?@{@�T@��@�h@p�@V@�/@�j@�@j@I�@�@ƨ@t�@"�@@��@�\@M�@-@��@�^@x�@X@G�@�@�`@Ĝ@�u@Q�@1'@ �@b@��@�P@l�@K�@;d@
=@�@�R@��@V@5?@{@�T@��@�-@��@`B@O�@�@��@��@z�@9X@9X@(�@�@�@1@�m@��@dZ@33@33@"�@o@��@��@�!@^5@�@�@��@��@G�@��@Ĝ@�9@��@1'@�w@|�@l�@l�@\)@K�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�-B
ŢBB1'BbNB�{B��B��B�B��B��B��B��B�B�B{B��B�'B�jBŢB�BBoB!�B9XB<jB"�B&�BE�BC�B:^B8RB9XB<jBD�BM�BS�B^5Bp�Bz�Br�Bp�Br�Bs�Bt�Bt�Bu�Bt�Bt�Bu�Bz�B}�B|�B}�By�BjBffB[#BR�BM�BE�B=qB7LB-B�B��B�B�fB�B��B�qB�-B��B��B�\Bx�Br�Bk�B_;BF�B;dB49B7LB8RB-B	7B
�yB
�5B
�B
ǮB
�wB
�FB
��B
n�B
YB
R�B
D�B
8RB
49B
B�B
J�B
I�B
H�B
F�B
?}B
7LB
-B
�B
bB	��B	�fB	�#B	�ZB	�yB	�B	ŢB	�-B	��B	|�B	x�B	jB	]/B	Q�B	7LB	33B	0!B	+B	&�B	"�B	�B	uB	1B	  B��B��B��B�B�yB�HB�/B�B��BɺBƨB��B�}B�jB�?B�!B�B�B�B�B�B�B�B�B�B�!B�'B�B��B��B��B��B��B�hB�VB�uB��B��B�{B�\B�PB�DB�7B�7B�B�%B�1B�1B�+B�B�B�B~�B~�B� B~�B~�B}�B|�Bz�B|�B{�Bz�By�Bx�Bu�Bu�Bw�Bt�Bs�Br�Bq�Bo�Bs�Br�Bs�Br�Bs�Bt�Bs�Bs�Bx�By�Bx�Bw�Bx�Bx�Bz�Bz�B}�B� B�B�B�%B�+B�B�By�Bq�Bo�Bm�Bm�Bn�Bm�Bp�Bs�Bu�Bt�Bw�Bx�B|�B� B� B�B�B�%B�+B�DB�JB�\B�uB�{B�{B��B��B��B��B��B��B��B��B��B�B�'B�XB�}B��B��BŢB��B��B��B��B�B�
B�
B�)B�HB�NB�ZB�mB�B�B�B��B��B��B��B��B��B	B	+B	DB	VB	oB	�B	�B	!�B	#�B	%�B	&�B	+B	1'B	6FB	9XB	;dB	;dB	=qB	>wB	?}B	A�B	C�B	F�B	I�B	K�B	L�B	N�B	P�B	T�B	W
B	W
B	W
B	XB	\)B	^5B	`BB	bNB	ffB	iyB	l�B	l�B	m�B	n�B	o�B	s�B	u�B	w�B	z�B	|�B	� B	�B	�B	�B	�+B	�7B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�RB	�wB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�;B	�BB	�HB	�HB	�NB	�ZB	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B

=B

=B
DB
JB
PB
PB
PB
PB
VB
\B
bB
bB
bB
bB
hB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
:^B
:^B
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
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
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
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
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
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
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
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
q�B
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
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
�LB
��B
ǔB�B3MBbhB�gB� B�eB�[B�B�B�B�]BqB�B�B��B��B�B�YB�B�B# B;0B@iB%zB(�BF%BD�B<PB:*B:*B=�BE�BN�BT�B_VBq�B}"Bv�Bu�Bt�Bt�Bu?Bu%Bu�BuBu%Bv�B{�B~�BB�UB}�Bl�Bi�B_!BU�BOBF�B>�B:B2�B$�B �B�AB�DB��BΥB�cB��B��B�B�:By�Bt�BoBd�BJ	B>�B7�B:DB:�B1'B�B
�B
�pB
��B
�^B
�B
��B
�;B
qAB
[#B
U�B
FtB
9	B
5B
C�B
K�B
J�B
J�B
H�B
@�B
9$B
/�B
!HB
�B
 �B	�B	�=B	�zB	�B	��B	��B	�FB	�yB	~�B	|B	m�B	aB	U�B	8lB	4nB	1�B	,�B	(sB	%zB	�B	MB	
	B	�B��B�0B��B�B�6B�B��BڠB�.B�^BȚB��B��B�cB��B��B�B��B�B��B�UB�/B��B��B�;B��B��B�B��B��B�fB�nB��B��B�(B�aB��B��B�B��B��B��B��B��B�?B�B�lB��B�B��B��B�'B�B�B��B�B�B~�B}�B{�B}qB|jB{�B{BzDBv�BxRBx�ButBt9BshBraBqABu?Bs�Bt�Bs�Bu�Bv+BuBt�By�Bz�By�By>By�BzB|PB|�B�B��B�oB��B�B��B��B�9B{�Bs�Bq�Bn�Bn�Bo�Bn}BqvBtTBv+Bu�Bx�By�B}�B��B��B��B��B��B�1B��B�6B�B��B��B��B�B�sB�xB��B�HB�ZB�8B�>B��B��B�B�B��B��B�'B�%B�DB�4B�B�2B�SB�YBרB��B�B�B�B�>B�"B�/B�'B�%B�B�	B�$B�$B�JB	GB	_B	�B	(B	@B	
B	�B	!�B	$B	&2B	'RB	+�B	1�B	6zB	9�B	;�B	;�B	=�B	>�B	?�B	A�B	C�B	F�B	J	B	K�B	MB	OB	Q4B	UB	W?B	W?B	W$B	XyB	\]B	^jB	`�B	b�B	f�B	i�B	l�B	l�B	m�B	n�B	pB	s�B	u�B	x8B	{B	}<B	�4B	�;B	�GB	�mB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�LB	�XB	�"B	�CB	�IB	�5B	�OB	��B	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�B	��B	�"B	�B	�B	�:B	�@B	�,B	�MB	�SB	�_B	�KB	�KB	ڠB	ބB	ߊB	�B	�|B	�|B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�%B	�B	��B	�B	�	B	�B	�B	��B	�B	�JB	�<B	�BB	�HB	�.B
 4B
 B
'B
AB
B
-B
GB
�B
mB
�B
EB
fB
KB
	RB
	�B

rB

rB
xB
dB
jB
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
B
B
�B
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
!�B
!�B
!�B
"B
#B
#B
"�B
"�B
#B
#B
$B
$&B
$&B
$&B
%B
$�B
%B
%FB
&B
&B
%�B
%�B
%�B
%�B
%�B
&B
'B
'8B
($B
(
B
($B
($B
($B
(>B
($B
(>B
)*B
)*B
)DB
*B
*KB
*0B
*KB
+B
+B
+B
+B
+6B
+B
+B
,=B
,WB
,WB
,=B
-]B
.IB
.IB
./B
.IB
.IB
.IB
/5B
/5B
/OB
/5B
0!B
0;B
0!B
0UB
0;B
0UB
0UB
1AB
1[B
1[B
1AB
2GB
2aB
2aB
2GB
2|B
2�B
3�B
4nB
4nB
4nB
4�B
5�B
5ZB
5ZB
5ZB
5tB
5�B
6zB
6�B
6zB
6zB
7�B
7�B
7fB
6zB
7fB
7fB
7�B
7�B
7fB
7�B
7�B
8�B
8lB
7�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
;dB
:xB
:�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J	B
J	B
J	B
J�B
J�B
J�B
K�B
LB
K�B
K�B
MB
MB
MB
L�B
MB
NB
NB
NB
OB
N�B
OB
O(B
P.B
P.B
QB
QB
QB
QB
QB
Q B
R B
RB
R B
RB
SB
S@B
S&B
TB
TB
TB
TB
TB
TB
TB
T,B
U2B
U2B
U2B
UB
V9B
V9B
VB
V9B
V9B
W$B
W?B
W
B
W?B
W$B
W$B
WYB
X_B
YKB
YKB
YeB
ZQB
ZQB
ZQB
ZQB
ZQB
Z7B
ZQB
[WB
[qB
\]B
\]B
\CB
[qB
\]B
]IB
]IB
]~B
]~B
^jB
^OB
^jB
_pB
_pB
_VB
_VB
_�B
`vB
`�B
a|B
a|B
a|B
abB
a�B
b�B
bhB
bhB
bhB
c�B
c�B
c�B
cnB
cnB
c�B
c�B
cnB
d�B
dtB
dZB
dtB
d�B
dtB
dtB
d�B
d�B
d�B
d�B
dtB
dtB
ezB
e�B
e�B
ezB
f�B
f�B
f�B
f�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
h�B
h�B
h�B
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
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
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
q�B
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
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707220037012017072200370120170722003701201806221316242018062213162420180622131624201804050718252018040507182520180405071825  JA  ARFMdecpA19c                                                                20170718093532  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170718003628  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170718003630  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170718003632  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170718003632  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170718003632  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170718003633  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170718003633  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170718003633  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170718003633                      G�O�G�O�G�O�                JA  ARUP                                                                        20170718012324                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170718153448  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20170721153701  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170721153701  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221825  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041624  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                