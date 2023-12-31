CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T03:41:13Z creation;2016-06-24T03:41:14Z conversion to V3.1;2019-12-19T08:35:42Z update;     
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
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   PRES_ADJUSTED_QC         
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
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20160624034113  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_002                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @װz���1   @װ{l��@3�a��f�d�e+��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D��3D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�@ D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@��
@���A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/{B7z�B?z�BG�HBOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!�C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO�RCQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D��
D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�8�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӿ
D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D��
D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�?
D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�?
D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D�=D�;�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��yA��mA��yA��`A��;A���A��A�AռjAպ^Aպ^Aմ9Aե�AՓuA��`A�JAҾwA�x�A�C�AѾwA���A϶FA�{A�jA��#A� �A��A�5?Aʧ�A�Q�A�/A��Aɥ�A�^5Aȝ�A���A�{A�`BAş�Aĉ7A�(�A�oA���A�`BA�dZA��A��jA��\A���A�l�A�A�A�|�A���A���A���A���A�A�A��A�dZA��A�dZA��A��RA�%A���A�%A��
A�~�A�ƨA�
=A��;A�ffA���A�C�A��`A�  A�$�A��jA�?}A��mA�t�A���A�S�A�9XA��A�VA��A���A�oA��RA��A�oA�M�A��A���A}33A|�Az�DAx��Ax^5Ax-Aw\)At��Ao��Al�AiG�Ah9XAd�RA_�^A\(�AZ�uAV�DAS��AS�PASl�AR�RAP��AO;dAK��AJ��AIƨAI��AH��AG�AEADn�AD(�AC�AC��AC�7AC?}AC�ACABĜAA�#A?%A<z�A;&�A:9XA8�9A6ffA4��A2�A0~�A/��A.�`A-�PA,��A,n�A+A)|�A(9XA'�A&jA%��A%%A$�A"�!A!��A��AS�AjA�AbNA&�A�DA1A"�A��A�hA�A�\AbA�yA=qA{A�PA~�A�A�PA/A�HAn�A�A��A`BAS�A��A��A�PA�7A�AXA
bNA	%A�TA33A��A5?AAx�A�HA1'At�A�`A�w@�@���@�  @��@�\)@�\)@�@���@�!@�^5@�p�@��`@@���@�C�@���@�u@�33@�%@���@�S�@��@�V@噚@���@�t�@�V@���@ߍP@�
=@�5?@�-@�{@�%@�;d@�o@���@�$�@�X@؃@�+@�@ԓu@�9X@�Q�@Ԭ@�b@Ӆ@�33@��@��@�"�@���@��`@�dZ@��@��@Ο�@�5?@���@Ͳ-@�7L@�1@�ȴ@�n�@ʏ\@�M�@�@�E�@Ɂ@�G�@�9X@�X@�@�S�@��7@�?}@��!@���@��@��@öF@�S�@��@�$�@���@�r�@�  @���@���@�C�@�"�@��H@��@��@��9@���@�M�@��7@��@��P@�+@��!@��@��y@��!@��!@�33@��@�ff@�E�@�5?@���@�(�@�v�@�&�@��@�$�@�v�@���@��u@��j@��@��@��@���@���@���@��@�dZ@�33@�o@���@�r�@���@��`@���@��D@�(�@�(�@�1'@�(�@�1'@��@�M�@�?}@�$�@���@���@��+@��7@��@�t�@�J@�dZ@�o@��@���@��@���@��7@��@��@�(�@���@��@��@�E�@�$�@��@���@�`B@�X@�G�@�G�@�G�@�`B@���@�@��7@���@���@��h@��@��@�/@�Ĝ@��@�|�@��y@�v�@��@�&�@�X@��@�1'@��@�b@�C�@�$�@��@�x�@��^@��T@��@�?}@���@�bN@���@��m@��@�
=@��H@�J@��7@�?}@���@��@���@�r�@��@���@�|�@�l�@�"�@���@��@�dZ@�|�@�t�@�33@�"�@�@�ȴ@���@��\@�V@��^@�G�@���@���@��@��D@�j@�I�@�1@��P@�dZ@��@�n�@�=q@���@��@��^@�x�@�x�@�X@�/@���@���@��@�;d@��H@�$�@��^@���@��7@�`B@�X@�O�@�%@�Q�@�\)@��@��H@��R@��+@�^5@�$�@���@�7L@�/@��@�Ĝ@��D@�bN@�bN@� �@�1@�1@�1@���@�|�@�t�@�l�@�S�@�\)@�\)@�S�@�K�@�o@��@��@���@��R@��+@�n�@�5?@���@�7L@�V@��/@��@�j@�bN@�I�@��@��w@��P@�;d@�ȴ@���@�n�@�=q@�-@�-@�$�@�$�@�-@�$�@�$�@�$�@�@�x�@��@�9X@��@\)@�@
=@~��@~ȴ@~�+@~E�@}p�@|�@|�@|Z@|�@{��@{ƨ@{dZ@{33@zM�@zJ@y��@yx�@x  @w\)@v�y@vff@v5?@u`B@t�@s�F@st�@so@r~�@p��@pbN@pA�@ol�@n��@nff@nV@nE�@nE�@nE�@n$�@n@n@mp�@l�@l��@kdZ@ko@j�@j��@jJ@i�7@iX@i%@h��@hr�@g��@g|�@gK�@g;d@f�y@f��@f�+@fE�@e/@d�@d��@d��@d�@c�@cdZ@cS�@c"�@c@b�H@b��@b-@a��@a%@`��@`��@`��@`�u@`A�@_��@_l�@_l�@^��@^��@^ff@]�h@\�/@\�D@\(�@[�F@Z��@Y�@Yx�@X�9@XQ�@X1'@X  @W�@W��@W��@W\)@V��@Vff@U�T@U�T@V@U�@U�T@U��@U@U�h@T�@Tj@TI�@T9X@T�@S�m@S�@Q�#@Q7L@P��@PĜ@P�9@PQ�@P �@O�w@O�@N�y@N��@M��@L�/@Lj@LZ@L�@L1@Kƨ@KC�@J��@I��@Ihs@H�u@Hr�@HA�@Hb@G�;@G�@G;d@F�y@Fff@E@Ep�@E�@D�/@D�@C��@CC�@B��@BM�@Ahs@AG�@A�@@bN@@ �@?�@?l�@?
=@>��@>5?@>{@>@=��@=�@=/@<�@<Z@<�@;�m@;ƨ@;��@:��@:=q@8��@8�@8bN@8A�@8 �@8b@7K�@6��@5�h@5O�@4�@4�@4z�@4Z@4�@3��@3�@3dZ@2�H@2�\@2^5@2=q@1��@1�7@01'@/��@/�@/|�@/\)@/K�@.�y@.��@.v�@.$�@.@-��@-p�@-V@,�/@,�D@,I�@,9X@+�m@+�F@+�F@+�
@+��@+t�@+S�@+"�@*��@*��@*��@*�\@*=q@)��@)��@)�7@)hs@)X@)G�@)%@(Ĝ@(�u@(r�@(A�@( �@(b@'�;@&��@&�+@&�+@&��@&�R@&�R@&�R@&�+@&5?@%@%`B@%V@$�j@$z�@$j@$�@#�
@#ƨ@#ƨ@#�@#S�@"�@"��@"��@"�\@"M�@"=q@"-@!��@!G�@!&�@!�@ ��@ Ĝ@ �u@ b@|�@�y@{@�T@�T@��@��@O�@�j@z�@Z@Z@Z@1@t�@C�@"�@33@"�@o@o@@�H@��@��@��@��@=q@��@�@�#@�^@�7@X@&�@%@Ĝ@�9@��@��@��@�u@�u@r�@bN@b@�@��@�P@l�@+@�@
=@��@�y@�@�R@ff@V@5?@5?@@p�@?}@/@�@��@��@��@��@��@�D@�@�@�@�
@t�@C�@"�@�@�H@��@�!@^5@M�@=q@-@J@��@�^@�7@�7@x�@7L@%@��@��@�`@Ĝ@�u@bN@bN@Q�@ �@b@b@b@  @�w@��@l�@\)@;d@
=@�y@�@��@@��@��@�T@�T@��@��@`B@?}@/@/@V@��@�@�/@�@Z@(�@1@�m@�
@�F@��@�@dZ@
�H@
��@
�!@
�\@
~�@
n�@
^5@
M�@
=q@
�@	�#@	�^@	��@	�7@	hs@	%@��@Ĝ@Ĝ@�9@��@�u@�u@r�@Q�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��yA��mA��yA��`A��;A���A��A�AռjAպ^Aպ^Aմ9Aե�AՓuA��`A�JAҾwA�x�A�C�AѾwA���A϶FA�{A�jA��#A� �A��A�5?Aʧ�A�Q�A�/A��Aɥ�A�^5Aȝ�A���A�{A�`BAş�Aĉ7A�(�A�oA���A�`BA�dZA��A��jA��\A���A�l�A�A�A�|�A���A���A���A���A�A�A��A�dZA��A�dZA��A��RA�%A���A�%A��
A�~�A�ƨA�
=A��;A�ffA���A�C�A��`A�  A�$�A��jA�?}A��mA�t�A���A�S�A�9XA��A�VA��A���A�oA��RA��A�oA�M�A��A���A}33A|�Az�DAx��Ax^5Ax-Aw\)At��Ao��Al�AiG�Ah9XAd�RA_�^A\(�AZ�uAV�DAS��AS�PASl�AR�RAP��AO;dAK��AJ��AIƨAI��AH��AG�AEADn�AD(�AC�AC��AC�7AC?}AC�ACABĜAA�#A?%A<z�A;&�A:9XA8�9A6ffA4��A2�A0~�A/��A.�`A-�PA,��A,n�A+A)|�A(9XA'�A&jA%��A%%A$�A"�!A!��A��AS�AjA�AbNA&�A�DA1A"�A��A�hA�A�\AbA�yA=qA{A�PA~�A�A�PA/A�HAn�A�A��A`BAS�A��A��A�PA�7A�AXA
bNA	%A�TA33A��A5?AAx�A�HA1'At�A�`A�w@�@���@�  @��@�\)@�\)@�@���@�!@�^5@�p�@��`@@���@�C�@���@�u@�33@�%@���@�S�@��@�V@噚@���@�t�@�V@���@ߍP@�
=@�5?@�-@�{@�%@�;d@�o@���@�$�@�X@؃@�+@�@ԓu@�9X@�Q�@Ԭ@�b@Ӆ@�33@��@��@�"�@���@��`@�dZ@��@��@Ο�@�5?@���@Ͳ-@�7L@�1@�ȴ@�n�@ʏ\@�M�@�@�E�@Ɂ@�G�@�9X@�X@�@�S�@��7@�?}@��!@���@��@��@öF@�S�@��@�$�@���@�r�@�  @���@���@�C�@�"�@��H@��@��@��9@���@�M�@��7@��@��P@�+@��!@��@��y@��!@��!@�33@��@�ff@�E�@�5?@���@�(�@�v�@�&�@��@�$�@�v�@���@��u@��j@��@��@��@���@���@���@��@�dZ@�33@�o@���@�r�@���@��`@���@��D@�(�@�(�@�1'@�(�@�1'@��@�M�@�?}@�$�@���@���@��+@��7@��@�t�@�J@�dZ@�o@��@���@��@���@��7@��@��@�(�@���@��@��@�E�@�$�@��@���@�`B@�X@�G�@�G�@�G�@�`B@���@�@��7@���@���@��h@��@��@�/@�Ĝ@��@�|�@��y@�v�@��@�&�@�X@��@�1'@��@�b@�C�@�$�@��@�x�@��^@��T@��@�?}@���@�bN@���@��m@��@�
=@��H@�J@��7@�?}@���@��@���@�r�@��@���@�|�@�l�@�"�@���@��@�dZ@�|�@�t�@�33@�"�@�@�ȴ@���@��\@�V@��^@�G�@���@���@��@��D@�j@�I�@�1@��P@�dZ@��@�n�@�=q@���@��@��^@�x�@�x�@�X@�/@���@���@��@�;d@��H@�$�@��^@���@��7@�`B@�X@�O�@�%@�Q�@�\)@��@��H@��R@��+@�^5@�$�@���@�7L@�/@��@�Ĝ@��D@�bN@�bN@� �@�1@�1@�1@���@�|�@�t�@�l�@�S�@�\)@�\)@�S�@�K�@�o@��@��@���@��R@��+@�n�@�5?@���@�7L@�V@��/@��@�j@�bN@�I�@��@��w@��P@�;d@�ȴ@���@�n�@�=q@�-@�-@�$�@�$�@�-@�$�@�$�@�$�@�@�x�@��@�9X@��@\)@�@
=@~��@~ȴ@~�+@~E�@}p�@|�@|�@|Z@|�@{��@{ƨ@{dZ@{33@zM�@zJ@y��@yx�@x  @w\)@v�y@vff@v5?@u`B@t�@s�F@st�@so@r~�@p��@pbN@pA�@ol�@n��@nff@nV@nE�@nE�@nE�@n$�@n@n@mp�@l�@l��@kdZ@ko@j�@j��@jJ@i�7@iX@i%@h��@hr�@g��@g|�@gK�@g;d@f�y@f��@f�+@fE�@e/@d�@d��@d��@d�@c�@cdZ@cS�@c"�@c@b�H@b��@b-@a��@a%@`��@`��@`��@`�u@`A�@_��@_l�@_l�@^��@^��@^ff@]�h@\�/@\�D@\(�@[�F@Z��@Y�@Yx�@X�9@XQ�@X1'@X  @W�@W��@W��@W\)@V��@Vff@U�T@U�T@V@U�@U�T@U��@U@U�h@T�@Tj@TI�@T9X@T�@S�m@S�@Q�#@Q7L@P��@PĜ@P�9@PQ�@P �@O�w@O�@N�y@N��@M��@L�/@Lj@LZ@L�@L1@Kƨ@KC�@J��@I��@Ihs@H�u@Hr�@HA�@Hb@G�;@G�@G;d@F�y@Fff@E@Ep�@E�@D�/@D�@C��@CC�@B��@BM�@Ahs@AG�@A�@@bN@@ �@?�@?l�@?
=@>��@>5?@>{@>@=��@=�@=/@<�@<Z@<�@;�m@;ƨ@;��@:��@:=q@8��@8�@8bN@8A�@8 �@8b@7K�@6��@5�h@5O�@4�@4�@4z�@4Z@4�@3��@3�@3dZ@2�H@2�\@2^5@2=q@1��@1�7@01'@/��@/�@/|�@/\)@/K�@.�y@.��@.v�@.$�@.@-��@-p�@-V@,�/@,�D@,I�@,9X@+�m@+�F@+�F@+�
@+��@+t�@+S�@+"�@*��@*��@*��@*�\@*=q@)��@)��@)�7@)hs@)X@)G�@)%@(Ĝ@(�u@(r�@(A�@( �@(b@'�;@&��@&�+@&�+@&��@&�R@&�R@&�R@&�+@&5?@%@%`B@%V@$�j@$z�@$j@$�@#�
@#ƨ@#ƨ@#�@#S�@"�@"��@"��@"�\@"M�@"=q@"-@!��@!G�@!&�@!�@ ��@ Ĝ@ �u@ b@|�@�y@{@�T@�T@��@��@O�@�j@z�@Z@Z@Z@1@t�@C�@"�@33@"�@o@o@@�H@��@��@��@��@=q@��@�@�#@�^@�7@X@&�@%@Ĝ@�9@��@��@��@�u@�u@r�@bN@b@�@��@�P@l�@+@�@
=@��@�y@�@�R@ff@V@5?@5?@@p�@?}@/@�@��@��@��@��@��@�D@�@�@�@�
@t�@C�@"�@�@�H@��@�!@^5@M�@=q@-@J@��@�^@�7@�7@x�@7L@%@��@��@�`@Ĝ@�u@bN@bN@Q�@ �@b@b@b@  @�w@��@l�@\)@;d@
=@�y@�@��@@��@��@�T@�T@��@��@`B@?}@/@/@V@��@�@�/@�@Z@(�@1@�m@�
@�F@��@�@dZ@
�H@
��@
�!@
�\@
~�@
n�@
^5@
M�@
=q@
�@	�#@	�^@	��@	�7@	hs@	%@��@Ĝ@Ĝ@�9@��@�u@�u@r�@Q�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B7LB6FB6FB7LB7LB6FB6FB6FB6FB5?B5?B5?B5?B33B1'B5?B8RB8RB6FB9XB@�BM�BZBdZBn�Bv�B�B��B��B��B�B�'B�LB�}BƨB�)B�sB��BBVB�B49BJ�BK�BR�B[#B^5BdZBjBp�Bm�BjBiyB\)BN�BE�B6FB+BPB�mB�;BB�B��B��B�{B�+Bw�B[#B?}B5?B&�B �B6FBK�B7LB)�B�B
��B
��B
�BB
��B
��B
��B
�HB
�yB
�)B
ǮB
�B
��B
�hB
�hB
�7B
�1B
{�B
dZB
?}B
1'B
)�B
%�B
!�B
�B
�B
%B	�5B	��B	�jB	�B	��B	�%B	q�B	ffB	YB	J�B	F�B	E�B	@�B	1'B	(�B	�B	bB	JB	
=B	+B	B��B��B��B��B�B�B�B�B�B�B�B�NB�#B��B��B��BɺBĜBĜB��B�wB�}B�wB�qB�dB�jB�jB�RB�^B�jB�dB�XB�FB�B��B��B��B��B��B��B��B��B�B�jBBÖBŢBĜBƨB��B��B��B��B��B��B��B��B��B�
B�B�B�B�
B�B�B�
B�
B�
B��B��B��BȴBƨB��B�qB�dB�dB�XB�FB�-B�B��B��B�Bz�Bz�B{�Bz�By�Bx�Bx�Bw�Bx�Bw�Bt�Bp�Bq�Br�Bs�Bs�Bw�B�B�hB�oB�uB�oB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�dB�jB��BȴB��B��B�B�BB�fB�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B	B		7B		7B	
=B		7B	B��B��B��B	  B	JB	(�B	.B	7LB	H�B	L�B	O�B	VB	]/B	_;B	bNB	aHB	aHB	bNB	dZB	iyB	k�B	hsB	hsB	gmB	ffB	iyB	iyB	jB	jB	jB	l�B	n�B	n�B	o�B	u�B	z�B	z�B	y�B	|�B	� B	|�B	{�B	x�B	� B	�B	�7B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�?B	�9B	�FB	�qB	B	ĜB	ĜB	��B	��B	�jB	�LB	ÖB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�;B	�BB	�HB	�HB	�BB	�;B	�;B	�5B	�BB	�BB	�BB	�BB	�`B	�ZB	�NB	�NB	�TB	�fB	�mB	�yB	�yB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
1B
	7B

=B

=B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
1B

=B

=B
DB
JB
JB
PB
\B
\B
VB
VB
\B
VB
VB
\B
\B
bB
hB
hB
hB
hB
uB
uB
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
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
!�B
!�B
!�B
!�B
"�B
#�B
$�B
#�B
#�B
"�B
!�B
!�B
!�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
<jB
=qB
<jB
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
@�B
@�B
@�B
@�B
@�B
?}B
@�B
@�B
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
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
XB
XB
YB
[#B
[#B
[#B
[#B
[#B
[#B
ZB
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
^5B
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
_;B
_;B
_;B
_;B
_;B
`BB
_;B
_;B
_;B
_;B
_;B
_;B
`BB
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
bNB
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
hsB
hsB
iyB
hsB
hsB
iyB
jB
jB
k�B
k�B
k�B
l�B
l�B
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
p�B
q�B
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
r�B
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
t�B
u�B
u�B
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
v�B
v�B
v�B
v�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B7�B6FB6`B7LB7fB6`B6FB6`B6FB5?B5ZB5�B5�B49B4B6�B8�B9	B72B:�BB�BO�B[�Be�Bp!Bx�B��B��B��B��B�iB��B�B��BȀB��B�B�lB�B�B!�B6+BK�BMPBU�B^�Bc:Bh�Bm�Bq�BncBl=Bk6B]�BPbBH�B:�B2�B�B�kB�nB	RB��B��B��B�?B��B|�B^�BA�B8B)�B#B7�BN�B:DB.}B 'B
��B
��B
�TB
��B
յB
�{B
�4B
�B
߾B
ˬB
��B
��B
�TB
��B
��B
�DB
�B
h>B
AB
33B
+�B
&�B
"�B
!�B
~B
xB	�4B	бB	��B	��B	�hB	�=B	t�B	j�B	[�B	K�B	G_B	G+B	C-B	3�B	,qB	B	�B	B	�B	�B	-B�]B�LB�FB�+B�B�9B�B�'B�B��B� B�,B��BյB�FBбB�B�zB�+BªB�B�;B��B�BB�B�.B�B��B��B��B��B��B�8B��B��B��B�,B�:B�zB�tB��B��B�oB�qB��BāB�?BňB�1BˬB�JB��B�4BбBӏBөBԕB՛BרBخB�kB�yB�B�kB�EB�?B�YB��B֡BөB�<B��BǔB�UB�BB�B�jB�^B��B��B�oB��B��B�MB{JB{0B|B{JBzBy>ByXBx�By�ByrBv`Bq�Br�Bs�Bt�Bt�Bw2B�B��B�B�B�@B��B�uB��B�KB�+B�B��B�B��B��B�B�4B��B��B��B�B�9B�B��B��B��B�B�<B�@B�7B�\B�B�}B��B��B�JB��B�*B�PB�VB�.B��B��B��B�B�B�	B��B	�B		�B		�B	xB	^B	B��B��B��B�B	^B	(�B	-�B	6�B	H�B	MPB	P�B	V�B	]�B	_�B	b�B	a|B	a�B	b�B	d�B	jB	l"B	iB	i_B	h>B	gB	jB	jB	j�B	j�B	jB	l�B	n�B	n�B	o�B	v+B	{B	{B	z*B	}�B	� B	}�B	|�B	x�B	�B	��B	��B	��B	��B	�BB	�B	��B	��B	�B	�B	��B	�B	�B	�TB	��B	�bB	��B	��B	�B	�B	�>B	�B	�B	�=B	�]B	��B	��B	��B	��B	�VB	ªB	�B	�mB	�B	ªB	�"B	��B	��B	��B	��B	�1B	��B	��B	��B	�+B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ˬB	��B	�,B	�B	�B	�?B	�yB	�QB	�]B	ߤB	��B	�B	��B	��B	߾B	ߤB	�jB	�B	��B	��B	�BB	��B	�,B	��B	�hB	�:B	�fB	�B	��B	��B	��B	��B	�B	��B	�B	��B	�UB	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�"B	�<B	�B	�"B	�VB	�VB	�BB	�HB
 4B
 4B
 4B
 4B
 OB
;B
oB
AB
�B
uB
GB
GB
GB
MB
MB
9B
YB
tB
�B
�B
	�B

�B

�B
	�B
	�B
	lB
	lB
	lB
	lB
KB
�B
�B
�B

�B

rB
^B
�B
~B
�B
�B
�B
pB
pB
�B
�B
pB
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
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
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
/B
jB
 B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!�B
!�B
#B
"�B
!�B
!�B
"B
"B
# B
$B
$�B
$&B
$ZB
# B
"B
"B
!�B
"4B
$ZB
$B
$B
$&B
$&B
$ZB
&2B
&B
&2B
'B
'B
'B
'B
&�B
'B
'B
($B
'B
'B
(>B
(>B
(sB
)*B
)B
)B
)DB
*KB
+B
+B
+B
+6B
+QB
,=B
,=B
-)B
-CB
-)B
-)B
-CB
-]B
./B
./B
/OB
/iB
/OB
0UB
0UB
1[B
1[B
1[B
2aB
2|B
2|B
3�B
3MB
3MB
3MB
3hB
3hB
4�B
4TB
4TB
5�B
5tB
5�B
5�B
6zB
6�B
6zB
6�B
6�B
7�B
7�B
7�B
7�B
6zB
7�B
7�B
7�B
7fB
7�B
8�B
8�B
9�B
8lB
9XB
:xB
;B
;B
;�B
;B
;�B
<�B
=�B
=�B
=�B
<�B
=�B
=B
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
@ B
@�B
@�B
@�B
@�B
@�B
?�B
@�B
@�B
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
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L0B
L0B
MPB
NB
NB
NB
M�B
N�B
N<B
OBB
PHB
QB
QB
QB
RB
RB
R B
R B
RB
R B
S@B
T,B
T,B
T,B
U2B
U2B
UgB
V9B
W?B
W?B
W?B
XEB
XEB
Y1B
[=B
[=B
[WB
[WB
[qB
[=B
ZQB
[WB
\]B
\]B
\]B
\CB
\)B
\)B
]IB
^jB
^jB
^jB
^jB
^OB
^OB
^OB
^jB
^jB
_pB
_VB
_VB
_VB
_VB
_pB
_VB
_VB
_VB
_VB
_VB
`\B
_pB
_�B
_VB
_;B
_VB
_VB
`\B
`\B
`vB
abB
a�B
a�B
a|B
a|B
b�B
bhB
b�B
bhB
bNB
cTB
cnB
c�B
c�B
cnB
cnB
cnB
d�B
d�B
d�B
e�B
f�B
f�B
f�B
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
g�B
g�B
h�B
h�B
iyB
h�B
h�B
i�B
j�B
j�B
k�B
k�B
k�B
l�B
l�B
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
p�B
q�B
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
r�B
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
t�B
u�B
u�B
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
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
xB
xB
w�B
xB
w�B
xB
xB
y	B
y	B
y	B
y$B
y	B
x�B
x�B
x�B
y	B
y	B
x�B
y	B
y�B
y�B
y�B
y�B
y�B
zB
zB
zB
y�B
{B
{B
z�B
z�B
z�B
z�B
{B
{B
|B
|B
|B
|B
|B
{�B
|B
}B
}B
}"B
}B
}B
}B
}"B
~(B
~B
}�B
~B
~B
~B
}�B
~B
~(B
B
.B
B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606061010262016060610102620160606101026201806221257142018062212571420180622125714201804050655232018040506552320180405065523  JA  ARFMdecpA19c                                                                20160624123512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624034113  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624034113  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624034113  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624034114  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624034114  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624034114  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624034114  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160624034114  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160624034114                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624042134                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160601153457  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160606011026  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160606011026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215523  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                