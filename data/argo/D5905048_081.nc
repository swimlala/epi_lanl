CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-01-22T00:35:20Z creation;2017-01-22T00:35:22Z conversion to V3.1;2019-12-19T08:16:47Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170122003520  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               QA   JA  I2_0577_081                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5��� 1   @��6:� @3����>B�d�q�i�C1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A@  A`  A~ffA���A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`y�D`��Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�C3Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�3D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�=q@�
=@��
A�A=�A]�A|Q�A�A���A���A���A���A���A���A�Bz�Bz�Bz�Bz�B'z�B/z�B7{B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��B��B��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq�RCs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��)C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	qHD	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*qHD*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`qHD`�HDaqHDa��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dp~Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�?
D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D��
D�2=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�^5A�`BA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�hsA�hsA�jA�jA�jA�jA�jA�hsA�hsA�l�A�l�A�n�A�p�A�p�A�n�A�n�A�n�A�hsA�bNA�XA�S�A�O�A�E�A�5?A�VA���A�z�A�r�A�Q�A��A�ȴAɛ�A�dZA�A�(�Aǲ-A��A��A�|�A�`BA�`BA�^5A�A�A��A�Aİ!AāA�33A�ȴA���A�VA��A���A���A��-A�M�A�{A���A��A� �A�t�A�&�A���A���A���A��DA�1'A���A��A��FA�z�A���A�ȴA���A��A���A���A�XA�ƨA�v�A�ƨA�33A��;A��#A�C�A�\)A���A�O�A�S�A�/A���A�7LA�VA�C�A�1A�-A��HA��\A�oA� �A�ȴA��wA�?}A� �A�A�A��yA��A���A�dZA���A~�DA|��AzbNAv�+At�AsS�Ap�/Ao�7Alv�Ag��Ae��Acx�Ab�RAa33A^��A[�#AYO�AXE�AV�!AQ
=AO7LAN�/AN^5AL�!AJE�AH�\AF�DAE;dAD�AC�7A@JA?�A>�A=\)A<bNA<5?A<-A<�A;�#A;&�A:ZA9�FA8�`A7��A6��A6��A6�DA5�;A4�A3�^A2��A1\)A.�DA-hsA+ƨA)�mA)%A(1A'��A&��A&�\A&z�A&r�A&bA%��A$�!A$�9A$�9A$�!A$��A$�uA$Q�A$  A#x�A"�/A!C�A��A��A^5A-A{AA�wAVA��A�mA�AĜA�+AI�A �A{A��A�A��A�+Ar�A=qA�yA{A�AK�A��A�TAjA"�A
��A
VA��A�^AO�AȴAv�A�AƨAS�A�A ��@��@�-@�G�@�Q�@�33@��#@��u@�S�@�ff@�b@�@�u@�ȴ@�G�@�D@�1@��H@�ff@���@�l�@⟾@��`@��@��@�&�@ڧ�@���@�@���@�@�@��#@�dZ@��@��;@׍P@ׅ@�;d@���@�n�@�E�@��#@�K�@�S�@�@��@�v�@ύP@�bN@ͺ^@ʗ�@��@ǝ�@�E�@�`B@Ĭ@��@î@�dZ@�=q@��T@���@�G�@���@��@�E�@��h@�O�@�bN@��F@�@�V@��@�%@��w@�;d@�;d@�33@�33@�+@��@���@���@��^@��@�V@�V@��@�Ĝ@�j@��F@�+@�+@���@���@�|�@��P@�C�@�n�@���@��@��u@�1@�1@���@�l�@��@���@� �@�1'@�  @���@�l�@���@���@�^5@�=q@��T@���@��h@�%@���@�+@��y@���@�;d@��P@��F@�ƨ@��F@�|�@�\)@�33@���@�V@�{@��-@�G�@��`@���@��D@�Z@�j@��@�|�@��@��+@��@�X@�G�@���@��m@�ƨ@��F@���@��w@���@���@���@��@�;d@��y@���@���@��\@�v�@�M�@�M�@�@�x�@�p�@�X@�&�@��`@��@�Q�@�Q�@�1'@�A�@�bN@��@��;@��@�33@���@�5?@���@��^@�x�@��@��j@�1'@�Q�@�1'@�\)@�-@�7L@��@��u@���@���@�\)@���@�M�@���@���@���@�@���@��^@���@��@��h@�x�@�?}@�%@�(�@��m@��P@�|�@�dZ@�K�@�;d@�"�@�"�@���@��R@��+@�v�@�M�@�$�@�{@�{@�{@��@��h@�O�@�hs@�hs@�hs@�hs@�O�@��@���@�Q�@�  @���@��;@��w@��P@�C�@��@��H@���@��R@���@�~�@��@��^@�x�@��@��`@��@�j@�I�@���@���@�|�@�\)@�33@�o@��y@�ȴ@���@���@��\@�~�@�ff@��@�@��@���@��^@���@���@��@���@��u@�bN@�9X@�1@�t�@�\)@�;d@��y@��\@�ff@�5?@��@�@��^@��7@�x�@�G�@���@�z�@�A�@��@;d@~�@~�+@~$�@}p�@|9X@{33@{o@z��@z-@y%@x1'@vv�@v5?@v$�@v$�@v@u��@up�@t�/@t�D@tI�@t9X@t�@s�m@s�@s"�@r�@q�7@qx�@qhs@o+@nff@nff@nff@nff@nV@n$�@m�@l�j@lz�@lZ@l9X@lI�@lI�@k�
@j�@jM�@j^5@i��@i��@iX@i�@hĜ@hr�@hQ�@hA�@g�@f�@fv�@f$�@e�@e@e��@ep�@e`B@eV@d�j@d�@c�
@c�
@c�F@c"�@b�@b�H@bn�@a��@a��@ax�@aG�@a&�@a%@`�u@`1'@` �@` �@` �@`  @_�;@_�P@_K�@^��@^5?@]��@]�@\��@\j@[�
@[�@[�@[33@Z��@Z�!@Z��@Z�\@Zn�@ZM�@Z-@Y��@Y�@Y�#@Y�#@Y�#@Y�#@Y7L@X��@W��@W|�@Wl�@W
=@Vȴ@V@U��@U@U@U�-@U`B@T��@T��@T�D@Tz�@TI�@S��@Sƨ@SdZ@S33@S@R��@R��@QX@P1'@O�w@O�@O��@O�P@O�P@O|�@O|�@Ol�@O\)@O;d@N��@N��@M��@M`B@MV@L��@Lj@L9X@L1@K��@Ko@J��@IX@H��@HbN@G�;@G+@E�@Ep�@E/@D�j@D��@D�D@Dz�@DZ@DI�@D(�@C��@C��@B��@B-@A��@A�@A�^@AG�@A%@@�@@1'@?��@?
=@>ȴ@>ff@>5?@=�@=p�@=?}@=�@=V@<��@<��@<��@<z�@<I�@;ƨ@;C�@:^5@9�7@9x�@9G�@9%@8Ĝ@8A�@7�w@7��@7�P@7l�@7
=@6�+@5�T@5��@5`B@5V@4�@4��@4�j@4�@4z�@4Z@4(�@41@3�m@3��@3��@3��@3�@3�@3�@3�@3�@3�@3t�@3t�@3dZ@3"�@3@2��@2�\@2M�@2=q@2�@1��@1�@1�^@1hs@1�@0�9@01'@0 �@/��@.�@.v�@.$�@-��@-`B@-�@-V@-V@,�/@,�j@,Z@+��@+ƨ@+��@+dZ@+33@+o@*=q@)�#@)�^@)��@)hs@)7L@)�@)%@(��@(Ĝ@(�9@(bN@(b@'��@'
=@&��@&$�@%�T@%�h@%p�@%p�@%p�@%`B@%O�@%/@%V@$�@$Z@#�F@#dZ@#C�@#C�@#"�@#@"��@"�\@"^5@"=q@"�@"J@!��@!��@!�@!��@!G�@!�@!%@ �`@ �@ A�@ b@\)@�R@�+@V@@��@@@�@O�@/@�@��@�j@z�@9X@�@1@��@�
@t�@@�H@��@n�@�@��@�#@�^@�^@��@��@��@hs@X@X@X@X@X@X@X@X@X@G�@��@��@�u@�@�@r�@r�@r�@Q�@  @�w@�w@�@��@l�@�@
=@�y@�@ȴ@��@v�@ff@ff@ff@ff@V@V@V@E�@E�@E�@5?@5?@$�@@@�@�T@@@�-@�h@`B@`B@`B@O�@��@9X@(�@ƨ@��@��@��@�@t�@dZ@33@�@�H@��@n�@~�@^5@=q@J@��@��@hs@X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�^5A�^5A�`BA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�ffA�hsA�hsA�jA�jA�jA�jA�jA�hsA�hsA�l�A�l�A�n�A�p�A�p�A�n�A�n�A�n�A�hsA�bNA�XA�S�A�O�A�E�A�5?A�VA���A�z�A�r�A�Q�A��A�ȴAɛ�A�dZA�A�(�Aǲ-A��A��A�|�A�`BA�`BA�^5A�A�A��A�Aİ!AāA�33A�ȴA���A�VA��A���A���A��-A�M�A�{A���A��A� �A�t�A�&�A���A���A���A��DA�1'A���A��A��FA�z�A���A�ȴA���A��A���A���A�XA�ƨA�v�A�ƨA�33A��;A��#A�C�A�\)A���A�O�A�S�A�/A���A�7LA�VA�C�A�1A�-A��HA��\A�oA� �A�ȴA��wA�?}A� �A�A�A��yA��A���A�dZA���A~�DA|��AzbNAv�+At�AsS�Ap�/Ao�7Alv�Ag��Ae��Acx�Ab�RAa33A^��A[�#AYO�AXE�AV�!AQ
=AO7LAN�/AN^5AL�!AJE�AH�\AF�DAE;dAD�AC�7A@JA?�A>�A=\)A<bNA<5?A<-A<�A;�#A;&�A:ZA9�FA8�`A7��A6��A6��A6�DA5�;A4�A3�^A2��A1\)A.�DA-hsA+ƨA)�mA)%A(1A'��A&��A&�\A&z�A&r�A&bA%��A$�!A$�9A$�9A$�!A$��A$�uA$Q�A$  A#x�A"�/A!C�A��A��A^5A-A{AA�wAVA��A�mA�AĜA�+AI�A �A{A��A�A��A�+Ar�A=qA�yA{A�AK�A��A�TAjA"�A
��A
VA��A�^AO�AȴAv�A�AƨAS�A�A ��@��@�-@�G�@�Q�@�33@��#@��u@�S�@�ff@�b@�@�u@�ȴ@�G�@�D@�1@��H@�ff@���@�l�@⟾@��`@��@��@�&�@ڧ�@���@�@���@�@�@��#@�dZ@��@��;@׍P@ׅ@�;d@���@�n�@�E�@��#@�K�@�S�@�@��@�v�@ύP@�bN@ͺ^@ʗ�@��@ǝ�@�E�@�`B@Ĭ@��@î@�dZ@�=q@��T@���@�G�@���@��@�E�@��h@�O�@�bN@��F@�@�V@��@�%@��w@�;d@�;d@�33@�33@�+@��@���@���@��^@��@�V@�V@��@�Ĝ@�j@��F@�+@�+@���@���@�|�@��P@�C�@�n�@���@��@��u@�1@�1@���@�l�@��@���@� �@�1'@�  @���@�l�@���@���@�^5@�=q@��T@���@��h@�%@���@�+@��y@���@�;d@��P@��F@�ƨ@��F@�|�@�\)@�33@���@�V@�{@��-@�G�@��`@���@��D@�Z@�j@��@�|�@��@��+@��@�X@�G�@���@��m@�ƨ@��F@���@��w@���@���@���@��@�;d@��y@���@���@��\@�v�@�M�@�M�@�@�x�@�p�@�X@�&�@��`@��@�Q�@�Q�@�1'@�A�@�bN@��@��;@��@�33@���@�5?@���@��^@�x�@��@��j@�1'@�Q�@�1'@�\)@�-@�7L@��@��u@���@���@�\)@���@�M�@���@���@���@�@���@��^@���@��@��h@�x�@�?}@�%@�(�@��m@��P@�|�@�dZ@�K�@�;d@�"�@�"�@���@��R@��+@�v�@�M�@�$�@�{@�{@�{@��@��h@�O�@�hs@�hs@�hs@�hs@�O�@��@���@�Q�@�  @���@��;@��w@��P@�C�@��@��H@���@��R@���@�~�@��@��^@�x�@��@��`@��@�j@�I�@���@���@�|�@�\)@�33@�o@��y@�ȴ@���@���@��\@�~�@�ff@��@�@��@���@��^@���@���@��@���@��u@�bN@�9X@�1@�t�@�\)@�;d@��y@��\@�ff@�5?@��@�@��^@��7@�x�@�G�@���@�z�@�A�@��@;d@~�@~�+@~$�@}p�@|9X@{33@{o@z��@z-@y%@x1'@vv�@v5?@v$�@v$�@v@u��@up�@t�/@t�D@tI�@t9X@t�@s�m@s�@s"�@r�@q�7@qx�@qhs@o+@nff@nff@nff@nff@nV@n$�@m�@l�j@lz�@lZ@l9X@lI�@lI�@k�
@j�@jM�@j^5@i��@i��@iX@i�@hĜ@hr�@hQ�@hA�@g�@f�@fv�@f$�@e�@e@e��@ep�@e`B@eV@d�j@d�@c�
@c�
@c�F@c"�@b�@b�H@bn�@a��@a��@ax�@aG�@a&�@a%@`�u@`1'@` �@` �@` �@`  @_�;@_�P@_K�@^��@^5?@]��@]�@\��@\j@[�
@[�@[�@[33@Z��@Z�!@Z��@Z�\@Zn�@ZM�@Z-@Y��@Y�@Y�#@Y�#@Y�#@Y�#@Y7L@X��@W��@W|�@Wl�@W
=@Vȴ@V@U��@U@U@U�-@U`B@T��@T��@T�D@Tz�@TI�@S��@Sƨ@SdZ@S33@S@R��@R��@QX@P1'@O�w@O�@O��@O�P@O�P@O|�@O|�@Ol�@O\)@O;d@N��@N��@M��@M`B@MV@L��@Lj@L9X@L1@K��@Ko@J��@IX@H��@HbN@G�;@G+@E�@Ep�@E/@D�j@D��@D�D@Dz�@DZ@DI�@D(�@C��@C��@B��@B-@A��@A�@A�^@AG�@A%@@�@@1'@?��@?
=@>ȴ@>ff@>5?@=�@=p�@=?}@=�@=V@<��@<��@<��@<z�@<I�@;ƨ@;C�@:^5@9�7@9x�@9G�@9%@8Ĝ@8A�@7�w@7��@7�P@7l�@7
=@6�+@5�T@5��@5`B@5V@4�@4��@4�j@4�@4z�@4Z@4(�@41@3�m@3��@3��@3��@3�@3�@3�@3�@3�@3�@3t�@3t�@3dZ@3"�@3@2��@2�\@2M�@2=q@2�@1��@1�@1�^@1hs@1�@0�9@01'@0 �@/��@.�@.v�@.$�@-��@-`B@-�@-V@-V@,�/@,�j@,Z@+��@+ƨ@+��@+dZ@+33@+o@*=q@)�#@)�^@)��@)hs@)7L@)�@)%@(��@(Ĝ@(�9@(bN@(b@'��@'
=@&��@&$�@%�T@%�h@%p�@%p�@%p�@%`B@%O�@%/@%V@$�@$Z@#�F@#dZ@#C�@#C�@#"�@#@"��@"�\@"^5@"=q@"�@"J@!��@!��@!�@!��@!G�@!�@!%@ �`@ �@ A�@ b@\)@�R@�+@V@@��@@@�@O�@/@�@��@�j@z�@9X@�@1@��@�
@t�@@�H@��@n�@�@��@�#@�^@�^@��@��@��@hs@X@X@X@X@X@X@X@X@X@G�@��@��@�u@�@�@r�@r�@r�@Q�@  @�w@�w@�@��@l�@�@
=@�y@�@ȴ@��@v�@ff@ff@ff@ff@V@V@V@E�@E�@E�@5?@5?@$�@@@�@�T@@@�-@�h@`B@`B@`B@O�@��@9X@(�@ƨ@��@��@��@�@t�@dZ@33@�@�H@��@n�@~�@^5@=q@J@��@��@hs@X11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B|�B}�B{�B|�B{�B{�B{�B}�B~�B� B�B�%B�=B�JB�VB�oB��B��B�dB�B�BBuB)�B1'B,B+B�B�B�B'�B.B1'B33BA�BK�BJ�BM�BVBS�BXBaHBq�B�PB��B��B�B��BƨB��B�wB�^B�3B��B��B��B��B��B�{B�hB�JB�%B}�Bv�Bv�Bq�BhsBcTBZBQ�BL�BE�B=qB�B�B%B�B�TB�BŢB�!B��B�DB{�Bo�Bm�BcTBS�BF�B7LB/B"�BbBPB  B
�B
�B
�B
�yB
��B
�}B
�XB
��B
��B
�%B
x�B
gmB
L�B
<jB
/B
�B

=B	��B	�B	��B	�3B	�B	��B	�\B	y�B	v�B	p�B	iyB	E�B	33B	33B	2-B	0!B	!�B	�B	�B	\B	DB	%B	B��B��B�B�B�yB�yB�sB�fB�TB�NB�/B�/B�B�B�B�B�B��B��B��B��BƨB�wB�jB�FB�wB�}B��BÖBĜB��B��B�
B�#B�5B�BB�HB�NB�`B�B�B�B�B�yB�TB�TB�B�B�B�B�B�B�B�B��B��B��B	B	B	B	%B	PB	�B	�B	�B	�B	�B	�B	�B	{B	DB��B�B�B�ZB�HB�/B�#B��B��BȴBǮBƨBǮBĜBÖB��B�}B�qB�jB�jB�dB�jB�jB�jB�qBƨB��B��B��B��B��B��B��B�B�B�#B�/B�5B�/B�#B�B��B��B��B�B�#B�/B�;B��B	B	+B	VB	bB	uB	�B	�B	�B	�B	�B	
=B	1B	JB	hB	�B	&�B	!�B	oB	DB	+B	%B	B	B	B	DB	VB	DB	
=B	DB	DB		7B	1B	
=B		7B	
=B	DB	JB	oB	{B	�B	�B	$�B	'�B	)�B	+B	+B	,B	,B	,B	/B	33B	49B	5?B	6FB	9XB	:^B	<jB	?}B	@�B	C�B	F�B	G�B	R�B	[#B	ZB	[#B	W
B	YB	\)B	]/B	cTB	dZB	dZB	aHB	^5B	]/B	cTB	hsB	k�B	o�B	r�B	s�B	t�B	u�B	v�B	w�B	z�B	{�B	z�B	z�B	z�B	|�B	�B	�B	�1B	�JB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�3B	�3B	�3B	�9B	�9B	�9B	�RB	�RB	�LB	�RB	�dB	��B	B	B	ÖB	ĜB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�;B	�BB	�HB	�TB	�TB	�TB	�NB	�NB	�HB	�HB	�;B	�5B	�5B	�BB	�5B	�)B	�B	�
B	�B	�B	�#B	�/B	�;B	�;B	�;B	�;B	�BB	�HB	�HB	�ZB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B
	7B

=B
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
"�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
%�B
&�B
&�B
'�B
(�B
(�B
(�B
'�B
(�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
49B
33B
33B
33B
33B
49B
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
5?B
5?B
7LB
8RB
8RB
8RB
8RB
8RB
9XB
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
<jB
<jB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
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
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
XB
XB
YB
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
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
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
e`B
e`B
e`B
ffB
ffB
ffB
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
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
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
m�B
m�B
m�B
n�B
n�B
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
q�B
q�B
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
r�B
r�B
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
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B}B}�B{�B|�B|B|B{�B}�B~�B�4B�GB�YB�=B�dB�pB��B�B�zB�PB�]B�B�B,B*eB1�B-)B,�B B�B�B(�B.IB1AB3MBA�BK�BKDBN�BV�BUBY�BeBxRB�MB�}BуB�OB�FB�_B� B�}B�B��B�}B��B��B�dB��B�gB��B�pB�7B��Bx�By>Bt9BjeBe�B[�BS�BN�BIlBBAB�B�B	�B�B��BٚBɺB�9B�kB��B}�Bp�Bo�BfBVSBIRB9	B2-B$�B�B\BB
�9B
�aB
�5B
��B
�B
�UB
�B
�XB
��B
��B
|jB
kQB
O(B
>�B
1�B
B
pB	��B	��B	�\B	��B	�UB	�&B	��B	|�B	x�B	s�B	n�B	G�B	4B	4�B	4�B	2�B	$&B	!B	?B	�B	�B		�B	�B�DB��B��B��B��B��B�B�mB�tB�:BބBޞB��BؓBּB�?B�sBԯBңB�B��B�fB��B��B��B��B�iB˒B�B��B�BԕB��B�)B�jB�\B�|B�B�B�B�;B�B��B�B�FB�tB�/B�!B�!B�B��B�B�B��B��B�XB�cB	oB	uB	�B	�B	B	�B	�B	�B	qB	pB	�B	�B	�B	VB�fB�B�B�FB��B��B�/B͟B�~B�7B�fB�_BȀB��B��B��B�iB�BB�VB�VB��B�qB�qB��B�HB�1B��B�B��BϑBϫB�hBԕB�qBںB�B�jB�B�5BܒB��B�[BЗB��B�+B�=B�/B޸B��B	;B	_B	pB	�B	�B	�B	B	B	�B	�B	)B	KB	B	B	�B	(�B	#�B	�B	JB	B	�B	�B	�B	�B	�B	�B	�B	
�B	�B	dB		�B	�B	
�B		�B	B	�B	B	B	2B	B	 �B	%FB	($B	*B	+6B	+6B	,=B	,WB	,�B	/�B	3�B	4TB	5tB	6�B	9�B	:�B	=B	?�B	@�B	C�B	F�B	G_B	S&B	[�B	Z�B	\B	W$B	Y�B	\�B	]dB	c�B	d�B	ezB	b4B	^�B	]dB	c�B	h�B	k�B	pB	sB	tB	uB	v+B	wB	x8B	{B	|�B	{0B	{B	z�B	|�B	��B	�B	�KB	�dB	��B	��B	��B	�B	��B	��B	�B	�B	�B	�2B	�
B	�QB	�CB	��B	��B	��B	��B	��B	��B	�nB	��B	��B	��B	��B	�lB	�dB	��B	ªB	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�VB	� B	�B	�B	� B	� B	�aB	�?B	�?B	�EB	�1B	�QB	ߊB	�vB	�B	�B	��B	�B	�B	�B	�B	�B	ߊB	ބB	�OB	��B	�B	��B	��B	׍B	�+B	ڠB	�qB	ݘB	߾B	ߊB	ߊB	�pB	�BB	�-B	�bB	�ZB	�zB	�B	��B	��B	��B	��B	�;B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	��B	��B	��B	��B	��B	�*B	�DB	�B	�B	�(B	�.B	�B
 4B
 OB
UB
;B
[B
GB
GB
GB
aB
�B
SB
mB
tB
_B
	�B
	lB
	�B

�B
~B
dB
dB
dB
jB
jB
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
/B
�B
�B
�B
B
;B
 BB
 BB
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
!B
!B
 B
�B
!B
"�B
# B
!�B
!�B
!�B
!�B
!�B
!B
!B
!�B
!�B
"�B
"�B
#B
#B
$@B
$B
#�B
$&B
%�B
'B
'B
($B
)*B
)*B
)*B
(sB
)*B
(>B
(
B
($B
(
B
(
B
(
B
(
B
($B
($B
)DB
*0B
*B
*B
+6B
,=B
,=B
,=B
,=B
-CB
-CB
.IB
.IB
.IB
.cB
/OB
0;B
0!B
0;B
0UB
0UB
1[B
2aB
2|B
2|B
3hB
3�B
3hB
4nB
4�B
3MB
3hB
3hB
3MB
4TB
4TB
4TB
4TB
4nB
5ZB
5tB
5ZB
5ZB
5?B
5ZB
5tB
5�B
7�B
8�B
8�B
8lB
8�B
8�B
9�B
:�B
:xB
:^B
:�B
:xB
;B
;�B
;�B
;�B
;B
<�B
<�B
<�B
<�B
<�B
<�B
;�B
;�B
<�B
=�B
=�B
=�B
=qB
=�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
AB
A�B
A�B
A�B
B�B
CB
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
H�B
I�B
I�B
I�B
I�B
J�B
KB
K�B
LB
LB
MB
MB
NB
NB
N"B
N�B
N�B
N�B
OB
OB
OB
OB
OB
P.B
PB
P.B
QB
RB
RB
RB
RB
R:B
S@B
SB
S&B
S&B
S&B
T,B
T,B
UB
U2B
U2B
UB
U2B
UB
UB
U2B
U2B
UB
UB
V9B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
V9B
VB
VB
VB
W?B
W?B
W$B
W$B
W?B
W?B
W$B
W$B
XEB
XEB
X_B
X+B
X_B
XyB
Y1B
YeB
YeB
YKB
Z7B
ZB
Z7B
Z7B
Z7B
ZkB
ZQB
Z7B
Z7B
[=B
[=B
[=B
[�B
\]B
\]B
\CB
\]B
\]B
]IB
]IB
]IB
]IB
]dB
]IB
]dB
]dB
^jB
^�B
_�B
_VB
_VB
`\B
`\B
`\B
`BB
`\B
`vB
`vB
`\B
`vB
a�B
a|B
bNB
bNB
bhB
b�B
b�B
b�B
c�B
c�B
c�B
cTB
cTB
cTB
cnB
c�B
cnB
d�B
d�B
dtB
d�B
ezB
ezB
e�B
f�B
f�B
f�B
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
i�B
i�B
iyB
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
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
m�B
m�B
m�B
n�B
n�B
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
q�B
q�B
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
r�B
r�B
tB
s�B
s�B
t�B
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
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201701260037152017012600371520170126003715201806221308112018062213081120180622130811201804050708492018040507084920180405070849  JA  ARFMdecpA19c                                                                20170122093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170122003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170122003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170122003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170122003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170122003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170122003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170122003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170122003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170122003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20170122010310                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170122153306  CV  JULD            G�O�G�O�F�Y�                JM  ARGQJMQC2.0                                                                 20170122153306  CV  JULD_LOCATION   G�O�G�O�F�Y�                JM  ARGQJMQC2.0                                                                 20170122153306  CV  LATITUDE        G�O�G�O�A��y                JM  ARGQJMQC2.0                                                                 20170122153306  CV  LONGITUDE       G�O�G�O��&{#                JM  ARCAJMQC2.0                                                                 20170125153715  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170125153715  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220849  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040811  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                