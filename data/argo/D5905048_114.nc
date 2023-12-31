CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-05-01T00:35:39Z creation;2017-05-01T00:35:42Z conversion to V3.1;2019-12-19T08:08:49Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170501003539  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               rA   JA  I2_0577_114                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��&� 1   @���9 @2�T`�d��d��͞��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@7�@w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3�C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dm~Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�8�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�p�A�jA�l�A�l�A�n�A�n�A�n�A�r�A�p�A�t�A�r�A�r�A�v�A�v�A�~�A�~�A�~�A�~�A�|�A�|�A�z�A�z�A�|�A�r�A�S�AϋDA���A�E�A�?}A�1'A�  A��
A͋DA�I�A���AˋDA�Q�A���AʾwA�t�A�^5A�;dA��A�S�A��A���A�ƨAȥ�A�t�A�A�"�A��`A�Q�A�7LA�p�AÙ�A��A��A´9A�G�A��A�v�A���A���A���A��mA�(�A���A��A�(�A��A���A�n�A�p�A��/A�;dA�`BA��uA�?}A�hsA�  A�ĜA�|�A��A�S�A�ZA��A�+A��A��
A���A�5?A�?}A�v�A���A��A�~�A�l�A�E�A�`BA�G�A��A�p�A�ffA��yA�^5A��A�v�A���A�-A��
A�bNA��uA�K�A��PA���A��A��RA��
A{��Awx�As��Aq&�Apn�Am�Aj�AghsAd�Ac��Ac/Ab�DA_��A^��A\�/AY��AW�AV�AU�-ATr�AS`BARȴAR�DAQ�AQ33AP��AOt�AMAL~�AL{AKx�AJ��AJ$�AI�AHȴAGXAEO�ABȴA@ĜA>ffA<�9A;�FA:��A8-A4��A3��A2A/�
A.�/A.bA,��A+��A*{A(�A'G�A&�A$ffA#p�A!��AK�A��Ar�A�A�
AĜAĜAƨAr�A�A��A�TA��A|�A�AE�A��A�hA�7Ax�A`BA%A
Q�A	33A
=A�`A��AA��AdZAĜA�A ��A (�@��@��^@���@��h@��@��@��@���@��h@�`B@��`@��D@�1@�"�@�=q@��@���@�E�@�@��@�b@�V@��@��@��`@�\)@�X@�p�@�^@�-@���@�O�@�ƨ@�-@�7L@�?}@�z�@߅@���@��
@��;@�b@��;@�~�@�Q�@�Ĝ@��`@ܬ@�  @ڧ�@�@؃@�
=@�K�@�I�@��/@��`@ԃ@ӍP@�E�@�`B@���@Ѓ@��@�33@Ο�@Ͳ-@̼j@�+@��@ɡ�@��@��@�bN@ȓu@�b@Ƈ+@��@�{@ũ�@�&�@�Q�@��@��T@��`@�(�@��@�33@���@�=q@�x�@��@�Ĝ@��D@���@���@�E�@��@���@�z�@�b@�o@���@�=q@��@���@�x�@���@�z�@�I�@�o@�@�x�@��@�`B@�/@��9@��m@��
@�  @���@�C�@���@���@�ff@�@��-@���@��@�V@��`@���@�  @���@�"�@�ȴ@�v�@��@���@��@��`@��D@�9X@�ƨ@�S�@���@� �@�Z@��@���@�^5@�X@��j@�%@��^@�X@�9X@�C�@�M�@�$�@���@�O�@���@�C�@��F@�+@���@��@���@��h@� �@���@��@��@�ȴ@�V@���@���@��-@��@�p�@�O�@�/@��`@���@���@��@�9X@� �@���@���@��@�|�@�\)@�C�@��@�ȴ@���@���@�5?@�@���@��+@�~�@��@�&�@�Ĝ@�V@���@���@�1'@��m@�ƨ@�ƨ@��@���@��@�J@�/@��@��@�&�@�7L@�`B@�G�@�&�@�%@��@�Q�@���@���@��
@���@�ƨ@���@�|�@�33@��H@��!@�~�@�V@�5?@��^@�V@��@�z�@��@��@�t�@�33@��H@��R@�M�@�M�@�ff@�E�@��@���@�X@��@�%@�%@��@��u@� �@�t�@�+@��@�@���@�{@��@���@��@�p�@�p�@�V@��@���@���@�Ĝ@���@�z�@�Z@�1'@�1@��
@�l�@��H@���@�-@���@��-@�O�@��@��@��@�%@���@���@�z�@�(�@��
@��P@�+@��\@�ff@�V@���@��^@�O�@�V@��@�z�@�A�@�P@�@~�R@~V@~E�@~{@~@}@}p�@|z�@{��@{"�@{@z�H@z��@z�\@z�@y%@xr�@w�;@wl�@w�@v��@v5?@u��@t�@t(�@s��@s33@s@r��@r�\@rM�@r-@rJ@q�^@qX@p�9@p�@o��@o|�@o�@nȴ@n�@n��@m�h@mO�@m�@l�/@l�D@kS�@k"�@j~�@jJ@i�^@iX@i&�@i%@hĜ@hbN@hb@g��@g�P@gl�@gK�@g
=@f�y@fȴ@f��@fE�@e�T@e�@d9X@c��@ct�@c33@b�!@bM�@b�@ahs@`��@`��@`r�@`r�@`r�@`Q�@`1'@`b@_�@_+@^�R@^ff@^@]p�@]?}@\��@\z�@\I�@\(�@\1@[�m@[��@[C�@Z��@ZM�@Z=q@Y��@Y��@Yhs@Y�@X��@W�;@Wl�@W�@V�y@V��@Vff@U��@U/@T9X@S��@SdZ@SS�@R��@RM�@RJ@QX@P�`@P�@PQ�@P1'@O�@O��@Ol�@O�@N��@Nff@N@M@M��@Mp�@M/@MV@L�/@Lz�@L1@K��@K��@Kt�@J�@J��@J��@J~�@J-@I�#@I�^@I��@IX@H�9@H �@G�@G+@F��@Fv�@Fff@F{@E��@E?}@D�/@Dj@C��@C��@C�m@C�
@C�m@C�
@C��@CdZ@C33@C33@Co@B�\@A�@A&�@@��@@Ĝ@@�u@@ �@?��@?�P@?|�@?K�@?�@?�@?�@>��@>��@>E�@>$�@>$�@=�-@=p�@=/@=�@<��@<�/@<z�@;�
@;dZ@:��@:M�@:�@9�@9��@9G�@9&�@8��@8�@8bN@8b@7�w@7�@7\)@7�@7
=@6�@6�R@6�R@6��@6ff@6E�@6$�@5�@5��@5��@4��@4�/@4��@4�j@4z�@4I�@4(�@3�
@3��@3t�@3"�@3@2��@2^5@2�@1��@1�^@1��@1�7@1G�@1&�@0��@0Q�@0  @/�;@/��@/�@/\)@/�@.�y@.v�@.5?@-��@-�h@-�@-p�@-O�@,��@,��@,j@,(�@+��@+��@+"�@+o@*�H@*�!@*~�@*^5@*=q@*-@)��@)X@)G�@)G�@)7L@)%@(�u@(b@'�@&�R@&V@%�T@%�@%?}@$�@$�@$��@$�D@$(�@#��@#��@#C�@"�@"��@"��@"�!@"~�@"�@!�#@!hs@!%@ ��@ Q�@  �@ b@�@�@l�@K�@
=@ȴ@��@V@@�T@�-@?}@/@/@V@��@�@j@I�@�@�m@��@S�@33@@�H@��@��@^5@M�@J@��@�@�#@��@�7@hs@hs@&�@�`@Ĝ@�9@�9@��@�@Q�@b@��@�P@K�@�@��@ȴ@��@�+@V@5?@$�@@�T@�-@�h@�@p�@?}@V@��@�@�@�/@��@�@�D@j@Z@I�@I�@I�@1@�m@ƨ@�@C�@�@��@�\@=q@��@x�@�7@�7@X@�@�`@�`@��@�@1'@  @  @  @�@�;@�@|�@;d@�@��@�@v�@5?@$�@�@�-@p�@p�@`B@V@�j@I�@(�@�@�m@�F@�@S�@C�@C�@"�@
�@
�@
��@
�\@
=q@
�@
�@	��@	�#@	�#@	��@	�7@	x�@	X@	G�@	&�@��@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�hsA�hsA�hsA�hsA�hsA�hsA�jA�p�A�jA�l�A�l�A�n�A�n�A�n�A�r�A�p�A�t�A�r�A�r�A�v�A�v�A�~�A�~�A�~�A�~�A�|�A�|�A�z�A�z�A�|�A�r�A�S�AϋDA���A�E�A�?}A�1'A�  A��
A͋DA�I�A���AˋDA�Q�A���AʾwA�t�A�^5A�;dA��A�S�A��A���A�ƨAȥ�A�t�A�A�"�A��`A�Q�A�7LA�p�AÙ�A��A��A´9A�G�A��A�v�A���A���A���A��mA�(�A���A��A�(�A��A���A�n�A�p�A��/A�;dA�`BA��uA�?}A�hsA�  A�ĜA�|�A��A�S�A�ZA��A�+A��A��
A���A�5?A�?}A�v�A���A��A�~�A�l�A�E�A�`BA�G�A��A�p�A�ffA��yA�^5A��A�v�A���A�-A��
A�bNA��uA�K�A��PA���A��A��RA��
A{��Awx�As��Aq&�Apn�Am�Aj�AghsAd�Ac��Ac/Ab�DA_��A^��A\�/AY��AW�AV�AU�-ATr�AS`BARȴAR�DAQ�AQ33AP��AOt�AMAL~�AL{AKx�AJ��AJ$�AI�AHȴAGXAEO�ABȴA@ĜA>ffA<�9A;�FA:��A8-A4��A3��A2A/�
A.�/A.bA,��A+��A*{A(�A'G�A&�A$ffA#p�A!��AK�A��Ar�A�A�
AĜAĜAƨAr�A�A��A�TA��A|�A�AE�A��A�hA�7Ax�A`BA%A
Q�A	33A
=A�`A��AA��AdZAĜA�A ��A (�@��@��^@���@��h@��@��@��@���@��h@�`B@��`@��D@�1@�"�@�=q@��@���@�E�@�@��@�b@�V@��@��@��`@�\)@�X@�p�@�^@�-@���@�O�@�ƨ@�-@�7L@�?}@�z�@߅@���@��
@��;@�b@��;@�~�@�Q�@�Ĝ@��`@ܬ@�  @ڧ�@�@؃@�
=@�K�@�I�@��/@��`@ԃ@ӍP@�E�@�`B@���@Ѓ@��@�33@Ο�@Ͳ-@̼j@�+@��@ɡ�@��@��@�bN@ȓu@�b@Ƈ+@��@�{@ũ�@�&�@�Q�@��@��T@��`@�(�@��@�33@���@�=q@�x�@��@�Ĝ@��D@���@���@�E�@��@���@�z�@�b@�o@���@�=q@��@���@�x�@���@�z�@�I�@�o@�@�x�@��@�`B@�/@��9@��m@��
@�  @���@�C�@���@���@�ff@�@��-@���@��@�V@��`@���@�  @���@�"�@�ȴ@�v�@��@���@��@��`@��D@�9X@�ƨ@�S�@���@� �@�Z@��@���@�^5@�X@��j@�%@��^@�X@�9X@�C�@�M�@�$�@���@�O�@���@�C�@��F@�+@���@��@���@��h@� �@���@��@��@�ȴ@�V@���@���@��-@��@�p�@�O�@�/@��`@���@���@��@�9X@� �@���@���@��@�|�@�\)@�C�@��@�ȴ@���@���@�5?@�@���@��+@�~�@��@�&�@�Ĝ@�V@���@���@�1'@��m@�ƨ@�ƨ@��@���@��@�J@�/@��@��@�&�@�7L@�`B@�G�@�&�@�%@��@�Q�@���@���@��
@���@�ƨ@���@�|�@�33@��H@��!@�~�@�V@�5?@��^@�V@��@�z�@��@��@�t�@�33@��H@��R@�M�@�M�@�ff@�E�@��@���@�X@��@�%@�%@��@��u@� �@�t�@�+@��@�@���@�{@��@���@��@�p�@�p�@�V@��@���@���@�Ĝ@���@�z�@�Z@�1'@�1@��
@�l�@��H@���@�-@���@��-@�O�@��@��@��@�%@���@���@�z�@�(�@��
@��P@�+@��\@�ff@�V@���@��^@�O�@�V@��@�z�@�A�@�P@�@~�R@~V@~E�@~{@~@}@}p�@|z�@{��@{"�@{@z�H@z��@z�\@z�@y%@xr�@w�;@wl�@w�@v��@v5?@u��@t�@t(�@s��@s33@s@r��@r�\@rM�@r-@rJ@q�^@qX@p�9@p�@o��@o|�@o�@nȴ@n�@n��@m�h@mO�@m�@l�/@l�D@kS�@k"�@j~�@jJ@i�^@iX@i&�@i%@hĜ@hbN@hb@g��@g�P@gl�@gK�@g
=@f�y@fȴ@f��@fE�@e�T@e�@d9X@c��@ct�@c33@b�!@bM�@b�@ahs@`��@`��@`r�@`r�@`r�@`Q�@`1'@`b@_�@_+@^�R@^ff@^@]p�@]?}@\��@\z�@\I�@\(�@\1@[�m@[��@[C�@Z��@ZM�@Z=q@Y��@Y��@Yhs@Y�@X��@W�;@Wl�@W�@V�y@V��@Vff@U��@U/@T9X@S��@SdZ@SS�@R��@RM�@RJ@QX@P�`@P�@PQ�@P1'@O�@O��@Ol�@O�@N��@Nff@N@M@M��@Mp�@M/@MV@L�/@Lz�@L1@K��@K��@Kt�@J�@J��@J��@J~�@J-@I�#@I�^@I��@IX@H�9@H �@G�@G+@F��@Fv�@Fff@F{@E��@E?}@D�/@Dj@C��@C��@C�m@C�
@C�m@C�
@C��@CdZ@C33@C33@Co@B�\@A�@A&�@@��@@Ĝ@@�u@@ �@?��@?�P@?|�@?K�@?�@?�@?�@>��@>��@>E�@>$�@>$�@=�-@=p�@=/@=�@<��@<�/@<z�@;�
@;dZ@:��@:M�@:�@9�@9��@9G�@9&�@8��@8�@8bN@8b@7�w@7�@7\)@7�@7
=@6�@6�R@6�R@6��@6ff@6E�@6$�@5�@5��@5��@4��@4�/@4��@4�j@4z�@4I�@4(�@3�
@3��@3t�@3"�@3@2��@2^5@2�@1��@1�^@1��@1�7@1G�@1&�@0��@0Q�@0  @/�;@/��@/�@/\)@/�@.�y@.v�@.5?@-��@-�h@-�@-p�@-O�@,��@,��@,j@,(�@+��@+��@+"�@+o@*�H@*�!@*~�@*^5@*=q@*-@)��@)X@)G�@)G�@)7L@)%@(�u@(b@'�@&�R@&V@%�T@%�@%?}@$�@$�@$��@$�D@$(�@#��@#��@#C�@"�@"��@"��@"�!@"~�@"�@!�#@!hs@!%@ ��@ Q�@  �@ b@�@�@l�@K�@
=@ȴ@��@V@@�T@�-@?}@/@/@V@��@�@j@I�@�@�m@��@S�@33@@�H@��@��@^5@M�@J@��@�@�#@��@�7@hs@hs@&�@�`@Ĝ@�9@�9@��@�@Q�@b@��@�P@K�@�@��@ȴ@��@�+@V@5?@$�@@�T@�-@�h@�@p�@?}@V@��@�@�@�/@��@�@�D@j@Z@I�@I�@I�@1@�m@ƨ@�@C�@�@��@�\@=q@��@x�@�7@�7@X@�@�`@�`@��@�@1'@  @  @  @�@�;@�@|�@;d@�@��@�@v�@5?@$�@�@�-@p�@p�@`B@V@�j@I�@(�@�@�m@�F@�@S�@C�@C�@"�@
�@
�@
��@
�\@
=q@
�@
�@	��@	�#@	�#@	��@	�7@	x�@	X@	G�@	&�@��@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
6FB
n�B
�=B
�B
�LB
�XB
�^B
�^B
�}B
ȴB
��B
��B
�BB
��BhB)�B,B/B8RBD�BB�BQ�Bz�B�uB��B�B�RB��B��B�mB�B�HB�NB�fB�B�B�B��BBuBoB�B$�B'�B)�B1'B0!B/B6FB9XB:^B:^B<jB5?B2-B(�B�B�B{B�B.B8RB8RB;dB2-B%�BB�mB�B�BB�B��B�BB�9B��B�DB�B|�BjBM�BC�B<jB)�B�BVB
��B
�TB
��B
�?B
��B
�7B
�7B
�JB
�+B
T�B
%�B
+B	��B	�B	�BB	��B	�}B	�-B	�B	��B	��B	�\B	�%B	z�B	hsB	]/B	Q�B	N�B	I�B	E�B	C�B	B�B	A�B	>wB	;dB	7LB	.B	,B	,B	'�B	#�B	�B	�B	�B	VB	B�B�TB�B��BȴBÖB�wB�-B�B��B��B��B��B��B�uB�VB�PB�DB�7B�B}�Bw�Bl�BiyBk�BdZB^5BaHBZBYBVBT�BQ�BQ�BO�BM�BL�BI�BH�BG�BG�BG�BF�BE�BC�BD�BA�BE�BJ�BO�BO�BP�BS�BW
BYB\)B_;BbNBbNBbNBaHBdZBffBm�Bl�Bm�Bo�Br�B�B�B� B~�B{�Bz�By�Bx�Bx�By�B|�B}�B� B�B�B�+B�bB��B��B��B��B��B��B�B�B�B�B�!B�'B�3B�9B�?B�}BɺB��B��B�B�/B�)B�#B��B��B�/B�NB�yB�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	B	+B	PB	VB	PB	PB	VB	bB	hB	oB	oB	uB	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	(�B	)�B	(�B	'�B	)�B	,B	-B	-B	/B	2-B	2-B	33B	49B	5?B	6FB	7LB	:^B	?}B	?}B	A�B	E�B	G�B	G�B	K�B	M�B	N�B	Q�B	T�B	XB	XB	\)B	`BB	e`B	iyB	jB	k�B	l�B	o�B	q�B	p�B	t�B	y�B	� B	�B	�B	�1B	�=B	�=B	�7B	�1B	�1B	�+B	�DB	�hB	�{B	�{B	�hB	�hB	�bB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�-B	�FB	�RB	�FB	�FB	�LB	�FB	�FB	�FB	�FB	�LB	�^B	�jB	�wB	�wB	�wB	�wB	�wB	��B	ĜB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�;B	�HB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
JB
JB
DB
DB
JB
JB
VB
VB
\B
\B
bB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
)�B
+B
,B
,B
-B
,B
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
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
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
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
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
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
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
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
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
W
B
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
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
bNB
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
e`B
e`B
e`B
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
jB
jB
jB
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
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
y�B
z�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
%�B
8RB
o�B
�)B
��B
�fB
��B
��B
�B
��B
ɠB
�^B
�pB
��B
�ZB�B*KB,�B0B9XBEBB�BR�B{0B�B��B��B�>B�'B�B�B��B�NB��B�B�kB�cB�aB�BtB�B�B=B&LB)�B-�B3�B2�B2B8RB:�B<�B=�B>(B7B6B,WB# B�B�B�B0!B9�B:�B>�B6B,�B�B��B��B��B�AB�DB��BżB�B��B��B�EB�oBmwBOvBEB?�B+�B�B�B
�B
��B
�B
��B
��B
��B
��B
��B
�jB
ZB
*0B

	B	��B	�B	�B	�(B	�B	��B	�=B	�tB	��B	�B	�B	~BB	k6B	^�B	SB	PbB	J�B	FYB	D3B	C�B	B�B	?�B	=VB	9�B	/ B	,�B	,�B	)B	$�B	�B	�B	�B	4B	EB�nB�2B�)B�hBʦB�_B�AB�B�cB��B�HB�B�WB�yB��B�.B��B�6B�^B��B�iBz�Bn�BkQBm]BfLB`Bc�B[�BZ�BWYBV�BS@BS�BRBP�BM�BJ�BH�BG�BG�BHBG�BGBE�BG�BDBF�BK)BPbBP�BR:BVBW�BY�B]/B_�Bb�Bb�Bb�Ba�Bd�Bg�BncBl�BnBp!BshB��B��B�;B�4B|jB{Bz�By�Bz*B{dB~(BHB� B�B� B�+B��B��B��B�B��B�bB�B��B��B�B�5B�;B�AB��B�?B�zB�}BɺB�BBѝB��B�B��B��BбB�{B�B�B��B�aB�B�zB�8B�DB�B��B��B��B��B�B��B	�B	�B	�B	B	_B	�B	\B	�B	�B	�B	 B	:B	uB	[B	FB	$B	B	B	B		B	/B	 B	!B	&LB	)�B	*�B	)�B	(�B	*B	,�B	-�B	-�B	/�B	2�B	2|B	3hB	4nB	5�B	6�B	7�B	;JB	@OB	?�B	A�B	E�B	G�B	HB	LJB	NB	N�B	R:B	UgB	XyB	XEB	\�B	`�B	e�B	i�B	j�B	k�B	l�B	pB	rB	qB	u?B	z*B	�4B	�[B	��B	��B	��B	��B	��B	��B	��B	�+B	�)B	��B	�2B	�2B	��B	�B	��B	�\B	�aB	�)B	��B	�qB	�QB	��B	��B	��B	��B	�hB	��B	�}B	��B	��B	�|B	��B	�$B	��B	��B	��B	��B	��B	��B	�zB	�fB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�4B	�.B	� B	�,B	�?B	�YB	��B	��B	�VB	��B	��B	�B	�sB	��B	��B	��B	��B	��B	�B	��B	��B	�aB	�MB	�3B	�B	��B	��B	�B	�B	��B	��B	�B	�>B	�JB	�0B	�B	��B	�B
 B
;B
'B
AB
GB
aB
aB
MB
gB
�B
{B
aB
gB
�B
mB
3B
mB
mB
mB
mB
?B
	RB
�B
�B
�B
~B
jB
�B
�B
�B
�B
�B
�B
~B
^B
xB
�B
�B
pB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
 BB
!B
"B
"B
#B
#B
# B
#B
$&B
$@B
%,B
%B
$�B
$�B
$�B
&B
&B
'B
'8B
(>B
($B
)*B
*0B
+6B
,=B
,=B
-)B
,WB
-]B
.IB
.IB
.IB
.IB
/�B
0UB
0oB
0UB
0UB
1[B
1[B
1[B
1[B
1vB
2aB
2GB
2GB
2aB
3hB
3hB
3hB
3hB
3MB
3hB
3�B
3�B
4�B
5tB
5tB
5tB
5�B
5�B
5tB
5�B
6�B
6zB
6zB
6`B
6FB
6zB
6zB
6`B
6zB
7�B
7�B
7�B
7�B
7�B
8lB
8lB
8�B
9�B
9�B
9rB
9rB
9�B
9�B
9�B
:�B
:�B
:xB
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
IB
IB
J	B
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
J�B
KB
K)B
K�B
K�B
K�B
MB
MB
MB
MB
M�B
M�B
M�B
M�B
M�B
NB
NB
NB
OB
OB
O(B
OB
PB
PB
O�B
PB
P.B
PB
P.B
QB
R B
R B
R B
R B
RB
R B
R B
R:B
S&B
S&B
SB
T,B
TB
T,B
TB
T,B
T,B
UB
U2B
U2B
U2B
U2B
UB
UB
U2B
V9B
V9B
VB
VB
V9B
W$B
W$B
W?B
W$B
W$B
W$B
W?B
W?B
XEB
XEB
XEB
XEB
XEB
Y1B
YKB
YKB
YKB
ZQB
Z7B
ZQB
ZQB
Z7B
[=B
[WB
[WB
[qB
\]B
\CB
\]B
\)B
\]B
\CB
\]B
\CB
]IB
]IB
]dB
]~B
^jB
^jB
^jB
^jB
_VB
_VB
_pB
_pB
^�B
_pB
_VB
_;B
_pB
_pB
_�B
_�B
`vB
`�B
a|B
a|B
bhB
bhB
bhB
cTB
bhB
c�B
cnB
c�B
c�B
d�B
d�B
d�B
dtB
dtB
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
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
j�B
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
s�B
s�B
s�B
tB
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
xB
xB
x�B
x�B
x�B
y	B
x�B
zB
zB
z�B
zB
{B
{B
{B
z�B
z�B
z�B
{B
z�B
{B
|B
|B
|B
|B
|B
|B
|�B
}B
}B
}B
}B
}B
}B
}"B
}"B
}�B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201705050034402017050500344020170505003440201806221312482018062213124820180622131248201804050714112018040507141120180405071411  JA  ARFMdecpA19c                                                                20170501093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170501003539  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170501003540  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170501003541  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170501003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170501003541  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170501003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170501003541  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170501003542  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170501003542                      G�O�G�O�G�O�                JA  ARUP                                                                        20170501010738                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170501153551  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20170504153440  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170504153440  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221411  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041248  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                