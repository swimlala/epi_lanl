CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-19T03:35:10Z creation;2017-09-19T03:35:14Z conversion to V3.1;2019-12-19T08:01:21Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170919033510  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_160                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�'6 �܀1   @�'6�Y  @:x����d�8}�H1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH��BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ DӼ�D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?�HBHG�BO{BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB��B�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]�RC_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&�D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De�Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�;�D�
D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�DӸ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�?
D�
D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D㸤D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D��
D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�A�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ĜA�ȴA�ȴA�ƨA�ĜA���A�A�A޼jA޼jA޼jA޼jA޼jAޥ�AۓuA�A�A�M�A�M�A�=qA�XA� �A�p�A7A��hA�~�A�1A�A�A���A��RA���A���A���A��mA�C�A��#A�C�A�dZA��+A��A���A�A�?}A���A�n�A�5?A���A��FA�&�A��#A�$�A���A�1'A�A�A�^5A�A�~�A�XA�bA�+A��A��jA�^5A�
=A���A�5?A��A���A��/A��-A�VA��
A���A�"�A��!A�\)A�JA�z�A�9XA�C�A��yA�l�A�r�A�+A�%A��FA�^5A���A�G�A��FA�VA���A���A��A�A�v�A��A%A~1'A}?}A|{A{"�Az�/Az��Ay��AyS�Ay
=Avn�Atn�As�^AsC�Ar�Ap�/Ao�wAo�Ao��Ao�An�HAnr�Amt�AihsAg;dAf��Af9XAeƨAc��AcAbĜAbZAa33A_�A_�PA_A^��A^��A]�hA]�A\��A[�7AZ�\AY��AY?}AX{AW�AV�/AU��AT�DAR�AR-AQ�wAQ�hAPbNAOp�AN��AMdZAL�ALbNAL�AK?}AJbNAI��AH��AE�mAD�!AC�ACoAB��AB��ABA�AA�TA@��A>��A=dZA=%A;�mA:�+A9��A8��A8��A8M�A7&�A6r�A6  A5?}A4�!A4bA3p�A2ĜA1/A/A/
=A.�\A-C�A,$�A+|�A*��A*n�A)��A)�A)�wA)%A'��A'?}A'A&�HA%l�A$bNA#�TA#7LA!�^A �`A�#AO�Ar�A?}AO�AAȴA��A�DA-A�A��A�RA�\AZA�wAXA�yA�yAoAQ�AhsAA{A
�+A$�Al�A�HAbNA��A?}A~�A9XAS�A��A�jAffA{A|�A
=A Q�@��@�hs@�r�@�"�@�7L@��m@�@���@�7L@���@���@�~�@�O�@��@�C�@��H@�ȴ@�R@��@@�M�@�A�@땁@�+@��@�(�@���@���@�@��m@��y@��@�7L@�bN@ߍP@��@ݑh@�Q�@��#@ؓu@�ff@ԓu@�"�@��#@У�@�Z@��H@��@�C�@���@���@�V@�t�@ģ�@���@��@���@��`@���@�O�@�Ĝ@��9@��D@��@��m@��@�5?@��h@���@���@�@�O�@�I�@���@���@�33@��R@�hs@�9X@�|�@�33@�o@���@��y@�ȴ@�~�@�{@�`B@�I�@���@��@���@���@��7@�hs@��@��j@��@�\)@���@���@�%@���@���@�bN@�9X@��F@��+@��@��/@�ƨ@���@�|�@�S�@�^5@�7L@�bN@���@��y@���@��+@�v�@�@�V@�A�@��m@��w@��P@���@�5?@��-@�?}@��/@�z�@��m@�l�@��+@�@��h@���@�j@�A�@�1@��@��m@��P@�"�@�@���@���@��@��@���@�`B@�G�@���@��j@�z�@�bN@�Z@�I�@��@���@��
@���@��@�33@��@���@��@��7@�x�@���@�(�@�ƨ@�dZ@��y@���@�V@��@���@��@��#@��-@�X@�7L@�V@��/@�j@�9X@���@�C�@��@���@��\@�M�@��-@�x�@�p�@�p�@�X@�&�@���@��9@���@��@�I�@�b@�;@�@�P@~��@~�@~��@~V@}�h@}V@|�j@z^5@x�@xA�@x  @w��@w|�@w�@v�y@v��@vV@v$�@u�T@u��@up�@u`B@u�@t�/@t��@s�F@q��@q%@p�9@p�u@pr�@pQ�@p1'@p  @o�;@o��@o�w@ol�@n�@n5?@m�T@m��@mO�@l��@lj@kƨ@kdZ@k@j�\@jM�@j-@i��@i7L@h�`@hQ�@g�@g+@f��@f@e`B@e?}@d�j@c�m@c�@cC�@b��@b�\@bn�@b=q@bJ@bJ@bJ@a��@a%@`��@`�@`A�@` �@_�;@_\)@_
=@^�+@^V@^E�@^$�@]/@\I�@\1@[�
@Z��@Z��@ZJ@Y7L@X�`@X�u@XQ�@X �@X  @W�@W�;@W�@W�P@Wl�@W\)@W+@V�R@V$�@Up�@T�/@T��@T�@T�D@TZ@T(�@S��@S�F@S33@R�\@R=q@R�@Q�@Q�7@Q%@P�9@PA�@O��@O;d@N��@Nȴ@N��@N5?@N@M@Mp�@M?}@L��@Lj@LI�@L9X@L(�@K�
@K��@KdZ@K"�@K"�@Ko@J�@J=q@I�^@I&�@H�u@H �@G��@G;d@G
=@F��@Fȴ@F��@F�+@F�+@F�+@Fv�@Fff@FE�@E@Ep�@E/@D�/@D(�@C33@B�H@B�@B�H@B��@B^5@BJ@A�^@Ax�@AX@AG�@AG�@A�@@A�@?�@?�@?+@>ȴ@>��@>�+@>5?@=�@=p�@=�@<�j@<��@<��@<��@<j@<j@<I�@;�m@;��@;�@;dZ@:��@:~�@:=q@9��@9��@9��@9hs@9G�@97L@9%@8��@8Q�@7�;@7|�@7
=@6��@6�+@6V@6$�@5�@5�h@5/@4�/@4Z@3�
@3dZ@333@3@2�H@2�\@2=q@1�#@1hs@1�@0��@0Ĝ@01'@/�w@/\)@/
=@.��@.�R@.��@.v�@.V@.@-@-`B@-?}@-�@,�/@,�D@,I�@+ƨ@+t�@+S�@+"�@*^5@)�@)X@)7L@)�@(��@(��@(��@(�@(bN@(A�@'�@'�@'|�@'\)@'
=@&��@&v�@&5?@%@%�@$z�@$I�@$9X@$(�@$1@#�m@#�
@#��@#t�@"��@"^5@"�@"J@!�#@!��@!�7@!x�@!X@!G�@!&�@!�@ ��@ �u@ Q�@   @�;@�w@�P@;d@�y@ȴ@�R@��@�+@E�@�@��@�-@?}@/@V@�/@�@(�@�@1@�F@33@�!@n�@^5@J@��@G�@��@��@1'@�w@�@��@��@��@|�@;d@��@ȴ@ȴ@ȴ@�+@V@@�h@�@��@�@��@��@I�@�
@ƨ@�F@��@�@t�@S�@"�@o@�@�\@�@��@��@hs@7L@&�@%@�`@�`@��@Ĝ@�u@�@r�@r�@r�@r�@A�@1'@1'@ �@b@�@�w@�P@;d@�@�@�@
=@�y@�@ȴ@��@V@��@�h@p�@O�@?}@�@��@�/@�/@��@�j@j@��@�
@ƨ@��@dZ@S�@C�@C�@"�@
�\@
=q@
�@
J@	�#@	�#@	��@	��@	��@	x�@	X@	&�@	�@	%@�`@��@r�@A�@ �@b@b@�;@�w@�w@��@�@�P@|�@;d@ȴ@��@v�@ff@V@E�@5?@$�@$�@{@{@@�@�@`B@`B@O�@?}@?}@/@�@�j@z�@Z@9X@�@��@�m@�
@ƨ@ƨ@�F@��@t�@S�@33@"�@@@�H@��@^5@=q@�@�@�#@��@��@��@x�@ ��@ ��@ ��@ r�@ 1'@ b@   ?��;?��w?���?�|�?�|�?�\)?�\)?�;d?���?��R?�5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�A�ĜA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ƨA�ĜA�ȴA�ȴA�ƨA�ĜA���A�A�A޼jA޼jA޼jA޼jA޼jAޥ�AۓuA�A�A�M�A�M�A�=qA�XA� �A�p�A7A��hA�~�A�1A�A�A���A��RA���A���A���A��mA�C�A��#A�C�A�dZA��+A��A���A�A�?}A���A�n�A�5?A���A��FA�&�A��#A�$�A���A�1'A�A�A�^5A�A�~�A�XA�bA�+A��A��jA�^5A�
=A���A�5?A��A���A��/A��-A�VA��
A���A�"�A��!A�\)A�JA�z�A�9XA�C�A��yA�l�A�r�A�+A�%A��FA�^5A���A�G�A��FA�VA���A���A��A�A�v�A��A%A~1'A}?}A|{A{"�Az�/Az��Ay��AyS�Ay
=Avn�Atn�As�^AsC�Ar�Ap�/Ao�wAo�Ao��Ao�An�HAnr�Amt�AihsAg;dAf��Af9XAeƨAc��AcAbĜAbZAa33A_�A_�PA_A^��A^��A]�hA]�A\��A[�7AZ�\AY��AY?}AX{AW�AV�/AU��AT�DAR�AR-AQ�wAQ�hAPbNAOp�AN��AMdZAL�ALbNAL�AK?}AJbNAI��AH��AE�mAD�!AC�ACoAB��AB��ABA�AA�TA@��A>��A=dZA=%A;�mA:�+A9��A8��A8��A8M�A7&�A6r�A6  A5?}A4�!A4bA3p�A2ĜA1/A/A/
=A.�\A-C�A,$�A+|�A*��A*n�A)��A)�A)�wA)%A'��A'?}A'A&�HA%l�A$bNA#�TA#7LA!�^A �`A�#AO�Ar�A?}AO�AAȴA��A�DA-A�A��A�RA�\AZA�wAXA�yA�yAoAQ�AhsAA{A
�+A$�Al�A�HAbNA��A?}A~�A9XAS�A��A�jAffA{A|�A
=A Q�@��@�hs@�r�@�"�@�7L@��m@�@���@�7L@���@���@�~�@�O�@��@�C�@��H@�ȴ@�R@��@@�M�@�A�@땁@�+@��@�(�@���@���@�@��m@��y@��@�7L@�bN@ߍP@��@ݑh@�Q�@��#@ؓu@�ff@ԓu@�"�@��#@У�@�Z@��H@��@�C�@���@���@�V@�t�@ģ�@���@��@���@��`@���@�O�@�Ĝ@��9@��D@��@��m@��@�5?@��h@���@���@�@�O�@�I�@���@���@�33@��R@�hs@�9X@�|�@�33@�o@���@��y@�ȴ@�~�@�{@�`B@�I�@���@��@���@���@��7@�hs@��@��j@��@�\)@���@���@�%@���@���@�bN@�9X@��F@��+@��@��/@�ƨ@���@�|�@�S�@�^5@�7L@�bN@���@��y@���@��+@�v�@�@�V@�A�@��m@��w@��P@���@�5?@��-@�?}@��/@�z�@��m@�l�@��+@�@��h@���@�j@�A�@�1@��@��m@��P@�"�@�@���@���@��@��@���@�`B@�G�@���@��j@�z�@�bN@�Z@�I�@��@���@��
@���@��@�33@��@���@��@��7@�x�@���@�(�@�ƨ@�dZ@��y@���@�V@��@���@��@��#@��-@�X@�7L@�V@��/@�j@�9X@���@�C�@��@���@��\@�M�@��-@�x�@�p�@�p�@�X@�&�@���@��9@���@��@�I�@�b@�;@�@�P@~��@~�@~��@~V@}�h@}V@|�j@z^5@x�@xA�@x  @w��@w|�@w�@v�y@v��@vV@v$�@u�T@u��@up�@u`B@u�@t�/@t��@s�F@q��@q%@p�9@p�u@pr�@pQ�@p1'@p  @o�;@o��@o�w@ol�@n�@n5?@m�T@m��@mO�@l��@lj@kƨ@kdZ@k@j�\@jM�@j-@i��@i7L@h�`@hQ�@g�@g+@f��@f@e`B@e?}@d�j@c�m@c�@cC�@b��@b�\@bn�@b=q@bJ@bJ@bJ@a��@a%@`��@`�@`A�@` �@_�;@_\)@_
=@^�+@^V@^E�@^$�@]/@\I�@\1@[�
@Z��@Z��@ZJ@Y7L@X�`@X�u@XQ�@X �@X  @W�@W�;@W�@W�P@Wl�@W\)@W+@V�R@V$�@Up�@T�/@T��@T�@T�D@TZ@T(�@S��@S�F@S33@R�\@R=q@R�@Q�@Q�7@Q%@P�9@PA�@O��@O;d@N��@Nȴ@N��@N5?@N@M@Mp�@M?}@L��@Lj@LI�@L9X@L(�@K�
@K��@KdZ@K"�@K"�@Ko@J�@J=q@I�^@I&�@H�u@H �@G��@G;d@G
=@F��@Fȴ@F��@F�+@F�+@F�+@Fv�@Fff@FE�@E@Ep�@E/@D�/@D(�@C33@B�H@B�@B�H@B��@B^5@BJ@A�^@Ax�@AX@AG�@AG�@A�@@A�@?�@?�@?+@>ȴ@>��@>�+@>5?@=�@=p�@=�@<�j@<��@<��@<��@<j@<j@<I�@;�m@;��@;�@;dZ@:��@:~�@:=q@9��@9��@9��@9hs@9G�@97L@9%@8��@8Q�@7�;@7|�@7
=@6��@6�+@6V@6$�@5�@5�h@5/@4�/@4Z@3�
@3dZ@333@3@2�H@2�\@2=q@1�#@1hs@1�@0��@0Ĝ@01'@/�w@/\)@/
=@.��@.�R@.��@.v�@.V@.@-@-`B@-?}@-�@,�/@,�D@,I�@+ƨ@+t�@+S�@+"�@*^5@)�@)X@)7L@)�@(��@(��@(��@(�@(bN@(A�@'�@'�@'|�@'\)@'
=@&��@&v�@&5?@%@%�@$z�@$I�@$9X@$(�@$1@#�m@#�
@#��@#t�@"��@"^5@"�@"J@!�#@!��@!�7@!x�@!X@!G�@!&�@!�@ ��@ �u@ Q�@   @�;@�w@�P@;d@�y@ȴ@�R@��@�+@E�@�@��@�-@?}@/@V@�/@�@(�@�@1@�F@33@�!@n�@^5@J@��@G�@��@��@1'@�w@�@��@��@��@|�@;d@��@ȴ@ȴ@ȴ@�+@V@@�h@�@��@�@��@��@I�@�
@ƨ@�F@��@�@t�@S�@"�@o@�@�\@�@��@��@hs@7L@&�@%@�`@�`@��@Ĝ@�u@�@r�@r�@r�@r�@A�@1'@1'@ �@b@�@�w@�P@;d@�@�@�@
=@�y@�@ȴ@��@V@��@�h@p�@O�@?}@�@��@�/@�/@��@�j@j@��@�
@ƨ@��@dZ@S�@C�@C�@"�@
�\@
=q@
�@
J@	�#@	�#@	��@	��@	��@	x�@	X@	&�@	�@	%@�`@��@r�@A�@ �@b@b@�;@�w@�w@��@�@�P@|�@;d@ȴ@��@v�@ff@V@E�@5?@$�@$�@{@{@@�@�@`B@`B@O�@?}@?}@/@�@�j@z�@Z@9X@�@��@�m@�
@ƨ@ƨ@�F@��@t�@S�@33@"�@@@�H@��@^5@=q@�@�@�#@��@��@��@x�@ ��@ ��@ ��@ r�@ 1'@ b@   ?��;?��w?���?�|�?�|�?�\)?�\)?�;d?���?��R?�5?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�B�TB��B�/B��BȴB�qBȴBǮB��B��B��B��B�=B�1B�Bz�Bs�BiyBr�Br�Bl�Bk�B^5BP�BG�BI�BA�B,B�B#�B�B�B
=B��B��B�B�B��BɺBÖB�wB�FB��B�PB�PB�7B�%B�Bx�Bp�BbNBM�B?}B9XB,B�B �B�B�B�BhBB
��B
��B
�B
�`B
�#B
�B
�)B
�#B
�B
��B
ɺB
��B
�dB
�3B
�B
��B
��B
��B
�oB
�PB
�+B
� B
y�B
v�B
t�B
o�B
k�B
gmB
[#B
M�B
K�B
G�B
B�B
:^B
33B
49B
33B
0!B
+B
$�B
�B
B	��B	��B	��B	�B	�ZB	�/B	�)B	�
B	��B	ŢB	ƨB	ĜB	ĜB	��B	�^B	�LB	�-B	�B	��B	��B	��B	��B	��B	��B	�{B	�PB	�%B	}�B	z�B	x�B	r�B	k�B	iyB	_;B	^5B	cTB	aHB	]/B	YB	T�B	M�B	C�B	?}B	<jB	7LB	7LB	5?B	33B	/B	'�B	�B	�B	oB	JB	B	B	B	B��B��B�B��B�B�B�B�mB�HB�B��B��B��BȴBƨBB��B��B�wB�wB�jB�RB�9B�-B�-B�!B�B��B��B��B��B��B��B�{B�hB�VB�1B�B� Bz�Bw�Bv�Bs�Br�Br�Bq�Bp�Bm�Bk�BhsB`BBZB\)B\)B\)BXBP�BN�BL�BK�BI�BG�BE�BB�BB�B?}B?}B@�B=qB<jB:^B:^B9XB8RB9XB8RB7LB49B49B6FB5?B5?B5?B49B33B49B49B5?B7LB8RB8RB8RB7LB5?B0!B33B1'B2-B1'B1'B0!B49B49B2-B2-B33B2-B2-B2-B0!B1'B-B2-B0!B49B7LB9XB:^B=qB:^B7LB>wB?}B?}B?}B=qB<jB@�BD�BD�BD�BG�BG�BK�BM�BM�BM�BO�BP�BO�BQ�BVBW
BXBXBYB\)B\)B\)B\)B[#B]/BaHBdZBe`Be`Be`Be`BdZBdZBcTBdZBgmBl�Bq�Br�Bs�Bs�Br�Bs�Bs�Bt�Bw�Bz�B|�B�B�B�B�B�B�B�+B�1B�JB�oB�uB�oB�hB�{B��B��B��B��B��B��B��B��B��B�B�'B�'B�B�FB�LB�XB�jB�wB�}B��BÖBɺB��B��B��B�B�B�B�B�B�#B�5B�;B�5B�BB�ZB�ZB�mB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	B	%B	+B	DB	JB	VB	bB	bB	hB	hB	hB	uB	{B	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	&�B	)�B	+B	+B	+B	+B	,B	/B	0!B	1'B	1'B	49B	6FB	7LB	8RB	8RB	:^B	:^B	:^B	:^B	=qB	=qB	;dB	D�B	N�B	O�B	P�B	Q�B	P�B	R�B	R�B	S�B	T�B	VB	W
B	XB	YB	XB	YB	YB	YB	ZB	bNB	ffB	hsB	iyB	jB	jB	k�B	k�B	l�B	l�B	l�B	m�B	o�B	r�B	s�B	s�B	t�B	u�B	w�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�JB	�PB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�FB	�LB	�RB	�XB	�^B	�^B	�^B	�^B	�dB	�jB	�dB	�jB	�qB	�}B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�BB	�BB	�BB	�HB	�NB	�TB	�`B	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
DB
JB
JB
PB
VB
\B
\B
\B
\B
bB
bB
bB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
#�B
"�B
"�B
$�B
%�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
.B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
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
>wB
>wB
?}B
@�B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
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
H�B
J�B
J�B
J�B
J�B
J�B
K�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
M�B
M�B
N�B
O�B
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
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
T�B
VB
VB
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
W
B
XB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
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
`BB
aHB
aHB
bNB
aHB
bNB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
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
gmB
gmB
gmB
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
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
m�B
l�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�1B�B�B�1B�B�1B�B�B�B�B�1B�B�B�B�B�B�1B�7B�7B�kB��B��B�5B��B�B��B�vB�MB��B�YB̈́B̈́B�`B��B�B��B��B��B�B}Bv`Bl�Bs�Bt9Bn�Bm�Ba�BU�BI�BKDBC�B0UB!HB$tB�BsB�B�B B�B��B��B��B�MB�.B��B��B��B�<B�#B�B�By�Bq�Bd�BP�BBAB;JB.�B"NB!�B�B�B�B&B�B
��B
��B
�B
�RB
��B
ںB
��B
�CB
׍B
�4B
�)B
�B
��B
��B
�CB
�$B
�B
��B
��B
�pB
��B
�oB
z�B
w2B
u?B
p�B
l=B
h�B
^B
PB
L�B
H�B
C�B
<6B
4TB
4TB
3hB
0�B
+�B
%�B
�B
	lB	�0B	��B	��B	�B	�B	�B	ܬB	��B	�hB	�B	�EB	�9B	�B	�[B	��B	�B	�B	�}B	�$B	��B	�$B	�NB	��B	�WB	�B	��B	��B	~�B	{B	y�B	tB	l�B	j�B	`�B	_;B	c�B	a�B	^OB	ZQB	VB	O�B	F�B	AB	=�B	88B	7�B	5�B	3�B	0B	)�B	)B	
B	@B	�B	�B	SB	�B	�B��B�XB��B�tB�B��B�B�B�B�CBҽB��B��BʌB��BÖB�oB� B�.B��B��B��B��B�3B��B��B��B�XB��B� B�~B��B��B��B��B�HB��B�B��B|�By	Bw�Bt�Bs�Bs3BrBq'Bn}BlWBi�BcB\xB]IB]dB]/BY�BS[BQ�BM�BL�BJ�BH�BF�BC�BCGB@�B@4B@�B>B=B;JB;0B:xB9�B:xB9$B8lB5�B5?B6�B6B5�B5�B4�B49B5?B5%B5�B7�B8�B8�B8�B7�B5�B1vB3�B2B2�B2aB2GB1AB4�B4�B2�B2�B3�B2�B2�B2�B1[B2aB.�B3hB1�B5�B8lB:^B;JB>B;�B9$B?B@B@4B@iB>�B>]BA�BE�BF%BFtBI7BH�BL0BN"BN<BNVBPBQ�BP�BRoBVBW$BXEBX�BY�B\xB\�B\�B\�B\)B^Ba�Bd�Be�Be�Be�Be�Bd�Bd�Bd&Be`BhsBmCBq�Br�Bs�BtBsBt9BtnButBx�B{�B}qB�aB�uB�{B��B��B��B��B�B�B��B��B��B�TB�gB�WB�pB�ZB�$B�DB�DB��B��B��B�iB�[B��B��B��B��B��B��B��B� B�'B�MB�#B�6B�HB�uB�+B�KB�QB�kBچBیB�jB�VBޞB��B�B�B�B��B��B�B��B��B�B��B��B�B��B��B��B�'B�B�+B�FB�JB�BB��B��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	=B	!-B	$&B	%,B	&LB	'mB	*0B	+B	+6B	+6B	+6B	,WB	/5B	0;B	1[B	1vB	4�B	6`B	7fB	8�B	8�B	:�B	:�B	:�B	:�B	=�B	=�B	<jB	E9B	OB	PB	Q4B	R B	QB	S&B	S@B	T,B	U2B	V9B	W$B	X+B	YKB	X_B	YeB	YKB	Y�B	Z�B	b�B	f�B	h�B	i�B	j�B	j�B	k�B	k�B	l�B	l�B	l�B	m�B	o�B	r�B	s�B	s�B	t�B	u�B	x8B	{B	|6B	}<B	.B	�B	�4B	�;B	�AB	�uB	�{B	�SB	�_B	��B	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�&B	�ZB	�LB	�0B	�0B	�eB	�/B	�cB	�oB	��B	�zB	��B	�lB	�rB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	ĶB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�.B	�4B	�:B	�B	�9B	�B	�9B	�EB	�EB	�KB	�QB	�kB	�IB	�dB	�jB	�jB	�OB	�pB	�vB	�vB	�bB	�vB	�\B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�2B	��B	��B	�0B	�B	�(B	�B	�B	�BB	�HB
 B
;B
'B
B
3B
GB
3B
MB
MB
MB
9B
SB
mB
SB
?B
_B
EB
1B
fB
	RB
	RB
	lB

XB

rB
�B
~B
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!B
!�B
$B
"�B
# B
%B
&2B
($B
($B
)*B
)*B
)*B
)*B
)B
)B
)*B
*0B
+6B
+6B
+6B
+QB
,=B
,"B
,WB
-]B
.IB
0UB
1AB
1AB
1AB
1[B
2aB
2aB
2aB
2|B
3�B
4TB
5ZB
5ZB
5tB
6zB
6`B
6`B
6`B
6zB
7fB
6zB
7�B
7�B
8lB
9rB
9�B
9�B
9�B
:�B
;B
;dB
;�B
;�B
;B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
@�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
D�B
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
IB
J�B
J�B
J�B
J�B
J�B
K�B
M�B
M�B
NB
M�B
M�B
NB
NB
OB
M�B
N"B
N"B
N�B
PB
O�B
O�B
P�B
QB
RB
RB
RB
RB
R B
RB
Q�B
RB
RB
RB
R B
SB
SB
R�B
SB
S&B
S&B
S&B
S&B
TB
S�B
TB
TB
TB
TB
TB
T,B
S@B
T,B
U2B
VB
VB
W$B
W?B
W$B
X+B
X+B
X+B
XEB
W?B
X_B
Z7B
ZQB
Z7B
[=B
[=B
[=B
[#B
[WB
[WB
\CB
]IB
]IB
]IB
^5B
^5B
^OB
^jB
^jB
_pB
_VB
_VB
_VB
_VB
_VB
_pB
`vB
a|B
abB
bhB
a|B
bNB
cnB
cnB
dtB
e�B
e�B
ezB
e�B
f�B
f�B
g�B
g�B
gmB
g�B
hsB
h�B
h�B
hsB
gmB
g�B
g�B
h�B
iyB
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
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
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
m�B
l�B
n�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K+<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*f�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201709250056372017092500563720170925005637201806221230582018062212305820180622123058201804050426112018040504261120180405042611  JA  ARFMdecpA19c                                                                20170919123507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170919033510  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170919033512  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170919033513  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170919033513  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170919033513  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170919033514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170919033514  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170919033514  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170919033514                      G�O�G�O�G�O�                JA  ARUP                                                                        20170919035620                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170924155437  CV  JULD            G�O�G�O�F�9�                JM  ARGQJMQC2.0                                                                 20170924155437  CV  JULD_LOCATION   G�O�G�O�F�9�                JM  ARGQJMQC2.0                                                                 20170924155437  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20170924155437  CV  LONGITUDE       G�O�G�O��&5�                JM  ARCAJMQC2.0                                                                 20170924155637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170924155637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192611  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033058  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                