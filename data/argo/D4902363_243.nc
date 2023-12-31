CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-26T00:35:24Z creation;2018-05-26T00:35:30Z conversion to V3.1;2019-12-19T07:41:30Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180526003524  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_243                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�eu�t�1   @�ev����@:�쿱[�dG�~($1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D���D�@ D߀ D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@��
@��
A�A<Q�A\Q�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C�RC	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D~D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�
Dջ�D���D�;�D�{�Dֻ�D���D�8�D�x�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߿
D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��!A��A���A�ZA��A�p�A���A�%A�ĜA�z�A�z�A�VA�K�A��A��/A�hsA�A��#A�S�A��RA�ZA�=qA�(�A�1A��;A��FA��7A�hsA�XA�G�A�A�A�5?A�1'A�-A�%A���A���A�oA��#A�1A�|�A�"�A�1A���A��9A���A���A�ĜA�XA���A�ZA���A��hA�
=A�?}A��A���A�I�A�{A��!A�r�A�dZA�|�A���A��A��PA�"�A�t�A��+A�  A���A���A��HA��HA�M�A�K�A�ĜA���A���A�G�A��A~�A}��A{�#AzA�Aw�
Av^5Au�^Atz�As�^Ar$�An�\Al�Al^5AjbNAi\)Ah�\Ag��Afv�Ad�uAc�
Ac��Ac��Ab5?A`v�A_|�A^��A^M�A]��A\E�A[33AY�#AXjAWG�AW�AV��AVn�AT��ASAR9XAQ&�AP�HAP��AP�AN~�AM\)ALA�AK�AJ�DAI�wAH�RAG�#AG�AD�9ADbAC��AB�HAA��A@I�A?�hA?dZA?/A>VA=7LA<��A;��A:�A:ZA9x�A7�wA7%A6�uA5�-A4�uA4-A3�A3��A3�wA3�A333A3%A2��A2��A2�DA2~�A2ZA2{A1�
A1�A0�yA0JA.��A.�DA.ZA-��A-C�A-A,�+A,I�A+�A*�A)hsA(�DA'��A&�9A&ffA%��A%��A%C�A$~�A$A#VA �`A+A��A�A�/AS�AƨA��A�
A��A��A&�A�A+A �A�7A"�A�AbAA33A��A�A��A�A\)A
�/A
1A^5A=qA9XA"�A��AbNAx�A%A Q�@��w@���@��`@��m@���@�33@��T@�~�@���@���@���@�J@���@�A�@���@�@��@�@�@�^5@�h@��@�b@�P@�K�@⟾@�{@�^@��@�-@�O�@��@��
@��@�/@�Z@��
@�t�@ڏ\@ج@��;@�~�@��T@�1'@��;@�\)@җ�@�Ĝ@�~�@���@�X@�%@̓u@�l�@��H@��@ȣ�@��
@�@ƸR@�v�@Ų-@��@���@ēu@�I�@�ƨ@+@��#@�p�@��u@��y@�7L@�b@���@�K�@���@�5?@�A�@�K�@�M�@���@���@��h@���@�A�@��@�"�@��+@�{@��-@�7L@��/@��@�(�@��
@�dZ@��H@���@���@�t�@��@��\@�E�@���@��@���@��j@��@���@�=q@��j@��;@��y@���@���@�p�@�&�@��j@�9X@��;@�dZ@��@���@�n�@�`B@��;@�\)@�E�@��@� �@���@�ƨ@�o@�V@��-@�O�@��u@��;@�33@���@��y@��y@���@�^5@��@�@���@��h@�p�@�O�@�?}@��@�bN@�33@���@���@��-@��@�O�@��@���@��/@��j@���@��D@�b@���@�\)@�"�@��@�5?@�@�hs@�/@��@���@��D@�1'@��;@��F@��P@��@���@���@���@���@��+@�ff@�5?@�{@���@���@���@��@�`B@���@�Ĝ@��@��D@�j@�A�@�(�@�@\)@\)@\)@+@~�y@~��@~{@}��@}p�@|��@|�D@|�D@|j@{��@{�F@{C�@z~�@y��@y�@xĜ@x�9@x�@xA�@w��@w�@w��@v��@v�y@v��@v@u@u�@u?}@u�@t��@t�@s�m@sƨ@s��@s33@so@s@r��@r~�@r�@q&�@p�`@p�`@p�9@p  @o�;@o��@o�w@ol�@n�@n��@n�+@n�+@nv�@nV@n5?@n$�@n{@n@m�T@m��@m`B@l�/@l�/@l�@l��@l�D@lz�@l�D@lz�@l9X@kS�@j��@i�7@i%@hĜ@hbN@h1'@hb@g�;@g�@g|�@gK�@f�y@f��@fff@fV@f@e�@d�@dI�@d1@c�F@c��@c��@c�F@c��@c"�@b��@b�\@a�@aG�@a�7@a��@ahs@a%@`�u@`  @_K�@^��@^�y@^ȴ@^E�@]@]�h@]�h@]�@]O�@\�j@\z�@\�@[��@[@Z�@Y��@YG�@Y&�@Y%@X�`@XĜ@X1'@W��@W��@W|�@Wl�@W�@V�R@VV@V{@U�-@U�@T�@T�D@T9X@Sƨ@S��@SS�@So@R��@Rn�@R=q@RJ@Q��@Qhs@Q7L@Q%@PĜ@Pr�@P1'@Pb@O�@O�w@O|�@N�y@Nv�@M�@M�@M/@L�/@L�j@L��@L�D@Lj@LI�@K��@K��@KdZ@Ko@J�H@J�!@J^5@I�#@I�^@I�7@I�@HĜ@H��@H�u@Hr�@HA�@H  @G\)@F��@F$�@E�@E@E��@E��@E�h@E�h@E�h@E�h@Ep�@E�@Dz�@D1@D1@C�m@C�
@C��@C@B�!@BM�@B=q@A�@Ahs@A�@A%@@�`@@Ĝ@@��@@�u@@Q�@?�@?�w@?�@?�@?�P@?l�@?K�@?;d@?�@>��@>�@>�R@>V@=��@=�@<�@<z�@;��@;t�@;o@:��@:��@:=q@:J@9��@9X@9�@8��@8��@8�u@8bN@8 �@7�;@7�w@7�@7��@7l�@7�@6�y@6��@6v�@6{@5�-@5`B@5?}@5/@5�@4�@4j@49X@4(�@4�@41@3�
@3t�@3o@2~�@1��@1�^@1�7@1&�@0�`@0bN@/�@/��@/\)@/K�@/;d@/+@/�@.��@.ȴ@.��@.v�@.v�@.v�@.E�@.5?@.5?@.5?@.{@-�@-�T@-��@-`B@,�/@,I�@+��@+�
@+S�@+"�@+"�@+"�@+"�@+o@*�H@*~�@*^5@*=q@*J@)�@)��@)X@(Ĝ@(�u@(r�@(Q�@(1'@(b@'��@'l�@'+@&ȴ@%�@%��@%O�@$z�@$1@#��@#�m@#��@#"�@"�H@"��@"�\@"J@!�#@!�^@!7L@ �u@ A�@ 1'@ b@�;@�w@�@|�@+@�@$�@@`B@�@�/@j@�@��@�@dZ@33@�@�!@n�@=q@J@�@�7@G�@�@��@��@Ĝ@Ĝ@��@�@bN@A�@b@��@�w@��@l�@;d@+@
=@��@��@�@��@��@��@��@v�@5?@{@@@�@��@�-@�@/@�@j@9X@1@�m@�F@��@��@�@o@�!@��@��@~�@^5@�@��@�#@�#@�7@G�@G�@%@��@r�@r�@bN@ �@b@�@��@�w@�P@K�@+@
=@�y@�@ȴ@�R@�+@V@{@�@@��@p�@O�@V@�@��@�j@�@�@�@��@Z@�@ƨ@�@C�@@
��@
��@
~�@
n�@
=q@
-@
J@	�#@	�7@	hs@	G�@	&�@�`@��@�@r�@r�@A�@  @�w@��@|�@;d@��@�@ȴ@�R@��@��@��@��@�+@v�@ff@E�@{@@�T@@�@p�@?}@�@�@�@�/@�j@�j@�D@z�@j@j@I�@(�@1@��@��@�m@�
@ƨ@��@t�@t�@t�@S�@o@��@��@n�@M�@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��!A��A���A�ZA��A�p�A���A�%A�ĜA�z�A�z�A�VA�K�A��A��/A�hsA�A��#A�S�A��RA�ZA�=qA�(�A�1A��;A��FA��7A�hsA�XA�G�A�A�A�5?A�1'A�-A�%A���A���A�oA��#A�1A�|�A�"�A�1A���A��9A���A���A�ĜA�XA���A�ZA���A��hA�
=A�?}A��A���A�I�A�{A��!A�r�A�dZA�|�A���A��A��PA�"�A�t�A��+A�  A���A���A��HA��HA�M�A�K�A�ĜA���A���A�G�A��A~�A}��A{�#AzA�Aw�
Av^5Au�^Atz�As�^Ar$�An�\Al�Al^5AjbNAi\)Ah�\Ag��Afv�Ad�uAc�
Ac��Ac��Ab5?A`v�A_|�A^��A^M�A]��A\E�A[33AY�#AXjAWG�AW�AV��AVn�AT��ASAR9XAQ&�AP�HAP��AP�AN~�AM\)ALA�AK�AJ�DAI�wAH�RAG�#AG�AD�9ADbAC��AB�HAA��A@I�A?�hA?dZA?/A>VA=7LA<��A;��A:�A:ZA9x�A7�wA7%A6�uA5�-A4�uA4-A3�A3��A3�wA3�A333A3%A2��A2��A2�DA2~�A2ZA2{A1�
A1�A0�yA0JA.��A.�DA.ZA-��A-C�A-A,�+A,I�A+�A*�A)hsA(�DA'��A&�9A&ffA%��A%��A%C�A$~�A$A#VA �`A+A��A�A�/AS�AƨA��A�
A��A��A&�A�A+A �A�7A"�A�AbAA33A��A�A��A�A\)A
�/A
1A^5A=qA9XA"�A��AbNAx�A%A Q�@��w@���@��`@��m@���@�33@��T@�~�@���@���@���@�J@���@�A�@���@�@��@�@�@�^5@�h@��@�b@�P@�K�@⟾@�{@�^@��@�-@�O�@��@��
@��@�/@�Z@��
@�t�@ڏ\@ج@��;@�~�@��T@�1'@��;@�\)@җ�@�Ĝ@�~�@���@�X@�%@̓u@�l�@��H@��@ȣ�@��
@�@ƸR@�v�@Ų-@��@���@ēu@�I�@�ƨ@+@��#@�p�@��u@��y@�7L@�b@���@�K�@���@�5?@�A�@�K�@�M�@���@���@��h@���@�A�@��@�"�@��+@�{@��-@�7L@��/@��@�(�@��
@�dZ@��H@���@���@�t�@��@��\@�E�@���@��@���@��j@��@���@�=q@��j@��;@��y@���@���@�p�@�&�@��j@�9X@��;@�dZ@��@���@�n�@�`B@��;@�\)@�E�@��@� �@���@�ƨ@�o@�V@��-@�O�@��u@��;@�33@���@��y@��y@���@�^5@��@�@���@��h@�p�@�O�@�?}@��@�bN@�33@���@���@��-@��@�O�@��@���@��/@��j@���@��D@�b@���@�\)@�"�@��@�5?@�@�hs@�/@��@���@��D@�1'@��;@��F@��P@��@���@���@���@���@��+@�ff@�5?@�{@���@���@���@��@�`B@���@�Ĝ@��@��D@�j@�A�@�(�@�@\)@\)@\)@+@~�y@~��@~{@}��@}p�@|��@|�D@|�D@|j@{��@{�F@{C�@z~�@y��@y�@xĜ@x�9@x�@xA�@w��@w�@w��@v��@v�y@v��@v@u@u�@u?}@u�@t��@t�@s�m@sƨ@s��@s33@so@s@r��@r~�@r�@q&�@p�`@p�`@p�9@p  @o�;@o��@o�w@ol�@n�@n��@n�+@n�+@nv�@nV@n5?@n$�@n{@n@m�T@m��@m`B@l�/@l�/@l�@l��@l�D@lz�@l�D@lz�@l9X@kS�@j��@i�7@i%@hĜ@hbN@h1'@hb@g�;@g�@g|�@gK�@f�y@f��@fff@fV@f@e�@d�@dI�@d1@c�F@c��@c��@c�F@c��@c"�@b��@b�\@a�@aG�@a�7@a��@ahs@a%@`�u@`  @_K�@^��@^�y@^ȴ@^E�@]@]�h@]�h@]�@]O�@\�j@\z�@\�@[��@[@Z�@Y��@YG�@Y&�@Y%@X�`@XĜ@X1'@W��@W��@W|�@Wl�@W�@V�R@VV@V{@U�-@U�@T�@T�D@T9X@Sƨ@S��@SS�@So@R��@Rn�@R=q@RJ@Q��@Qhs@Q7L@Q%@PĜ@Pr�@P1'@Pb@O�@O�w@O|�@N�y@Nv�@M�@M�@M/@L�/@L�j@L��@L�D@Lj@LI�@K��@K��@KdZ@Ko@J�H@J�!@J^5@I�#@I�^@I�7@I�@HĜ@H��@H�u@Hr�@HA�@H  @G\)@F��@F$�@E�@E@E��@E��@E�h@E�h@E�h@E�h@Ep�@E�@Dz�@D1@D1@C�m@C�
@C��@C@B�!@BM�@B=q@A�@Ahs@A�@A%@@�`@@Ĝ@@��@@�u@@Q�@?�@?�w@?�@?�@?�P@?l�@?K�@?;d@?�@>��@>�@>�R@>V@=��@=�@<�@<z�@;��@;t�@;o@:��@:��@:=q@:J@9��@9X@9�@8��@8��@8�u@8bN@8 �@7�;@7�w@7�@7��@7l�@7�@6�y@6��@6v�@6{@5�-@5`B@5?}@5/@5�@4�@4j@49X@4(�@4�@41@3�
@3t�@3o@2~�@1��@1�^@1�7@1&�@0�`@0bN@/�@/��@/\)@/K�@/;d@/+@/�@.��@.ȴ@.��@.v�@.v�@.v�@.E�@.5?@.5?@.5?@.{@-�@-�T@-��@-`B@,�/@,I�@+��@+�
@+S�@+"�@+"�@+"�@+"�@+o@*�H@*~�@*^5@*=q@*J@)�@)��@)X@(Ĝ@(�u@(r�@(Q�@(1'@(b@'��@'l�@'+@&ȴ@%�@%��@%O�@$z�@$1@#��@#�m@#��@#"�@"�H@"��@"�\@"J@!�#@!�^@!7L@ �u@ A�@ 1'@ b@�;@�w@�@|�@+@�@$�@@`B@�@�/@j@�@��@�@dZ@33@�@�!@n�@=q@J@�@�7@G�@�@��@��@Ĝ@Ĝ@��@�@bN@A�@b@��@�w@��@l�@;d@+@
=@��@��@�@��@��@��@��@v�@5?@{@@@�@��@�-@�@/@�@j@9X@1@�m@�F@��@��@�@o@�!@��@��@~�@^5@�@��@�#@�#@�7@G�@G�@%@��@r�@r�@bN@ �@b@�@��@�w@�P@K�@+@
=@�y@�@ȴ@�R@�+@V@{@�@@��@p�@O�@V@�@��@�j@�@�@�@��@Z@�@ƨ@�@C�@@
��@
��@
~�@
n�@
=q@
-@
J@	�#@	�7@	hs@	G�@	&�@�`@��@�@r�@r�@A�@  @�w@��@|�@;d@��@�@ȴ@�R@��@��@��@��@�+@v�@ff@E�@{@@�T@@�@p�@?}@�@�@�@�/@�j@�j@�D@z�@j@j@I�@(�@1@��@��@�m@�
@ƨ@��@t�@t�@t�@S�@o@��@��@n�@M�@=q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BiyBe`B[#BA�BG�By�B�\B��B��B�1B�B�BƨB��B�dB�XB�jB�'B��B�B�!B�!B�B�B��B��B��B��B��B��B��B��B��B�+BjB;dB��B��B��B�B$�B%�B"�B�B��B�TB�jB�B��B��BŢB��B�3B��B�bBy�Bq�Bz�Bk�BF�B9XB/B)�B�B�BVB
��B
�B
�`B
�#B
�jB
��B
�bB
�B
z�B
�JB
�hB
�bB
�%B
v�B
r�B
aHB
J�B
C�B
0!B
,B
33B
%�B
�B
	7B	�;B	�BB	�B	��B	�B	�B	��B	��B	�B	�RB	��B	�LB	��B	��B	��B	��B	��B	�\B	�B	s�B	o�B	bNB	ffB	o�B	jB	[#B	H�B	@�B	>wB	:^B	I�B	B�B	8RB	�B	!�B	�B	 �B	�B	�B	\B	DB	%B�sB��B��B��B�HB�`B�sB��B�B�yB�5B�fB�)B�#B�B��B��BÖBŢB�jB�3B��B��BB��B�wB�dB�qB�dB�dB�jB�dB�RB�3B�-B�B��B��B��B��B��B��B��B��B��B��B�\B�B�B�B~�B�B�7B�%B�B�Bv�Br�Be`BM�BVBk�BbNBR�BG�BC�BK�BJ�BF�BC�BL�B@�BA�B=qBE�BI�BI�BA�BG�BC�BH�BA�B6FB2-B-B33B)�BoB\B1B�B/B'�B�B'�B#�B%�B'�B!�B)�B.B&�B�B%BB�B�B&�B%�B&�B!�B$�B)�B(�B$�B#�B'�B+B&�B,B/B,B,B/B2-B0!B)�B#�B �B �B�B!�B%�B#�B�B\B�B�B�B�B#�B�B�BbBJB#�B%�B'�B#�B�B!�B �B�B$�B'�B.B.B)�B,B2-B/B.B,B%�B,B/B(�B#�B#�B0!B>wBA�B@�B7LB2-B<jB?}B;dB:^BG�BM�BQ�BR�BL�BO�BR�BS�BS�BT�BVBW
BVBT�BR�BO�BQ�BQ�BcTBgmBiyBffBe`BiyBl�BiyBdZB^5B_;BjBn�Bq�B�B�B�B� B�B�B�B�1B�7B�1B�B�B�oB�bB��B��B��B��B��B��B��B�B�B�3B�RBBƨBƨBŢBBŢBǮB��B��B��B��B��B��BƨBƨB�
B�B�NB�`B�mB�yB�B�B�B�B�B�B�B��B��B��B��B��B	B	1B	1B	DB	
=B	JB	VB	oB	oB	oB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	#�B	%�B	&�B	'�B	'�B	/B	2-B	33B	49B	5?B	9XB	<jB	?}B	C�B	D�B	D�B	E�B	F�B	G�B	J�B	M�B	N�B	W
B	W
B	VB	T�B	W
B	W
B	W
B	[#B	_;B	dZB	ffB	e`B	e`B	e`B	iyB	jB	iyB	l�B	k�B	jB	o�B	q�B	r�B	t�B	t�B	u�B	t�B	{�B	{�B	|�B	~�B	� B	� B	� B	�B	�B	�1B	�7B	�7B	�1B	�VB	�bB	�hB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�9B	�?B	�RB	�dB	�jB	�}B	��B	��B	��B	��B	��B	�}B	��B	B	ĜB	ÖB	B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�5B	�BB	�BB	�;B	�5B	�BB	�BB	�;B	�BB	�;B	�TB	�`B	�mB	�mB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
	7B
	7B
1B

=B
JB
JB
JB
DB

=B

=B

=B
VB
bB
hB
oB
uB
uB
uB
uB
oB
hB
bB
bB
oB
�B
�B
{B
{B
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
!�B
"�B
"�B
"�B
#�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
(�B
'�B
'�B
'�B
'�B
(�B
'�B
(�B
)�B
+B
,B
+B
)�B
+B
-B
.B
.B
.B
-B
,B
,B
+B
,B
/B
/B
.B
/B
/B
/B
1'B
2-B
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
6FB
7LB
6FB
6FB
5?B
5?B
49B
49B
33B
33B
5?B
6FB
6FB
9XB
;dB
;dB
:^B
:^B
9XB
9XB
;dB
;dB
:^B
;dB
:^B
:^B
:^B
<jB
=qB
=qB
=qB
=qB
<jB
;dB
<jB
;dB
:^B
=qB
=qB
<jB
>wB
B�B
B�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
D�B
B�B
C�B
E�B
G�B
F�B
F�B
F�B
G�B
F�B
F�B
E�B
C�B
E�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
K�B
L�B
K�B
L�B
L�B
M�B
N�B
N�B
M�B
N�B
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
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
S�B
T�B
VB
T�B
T�B
T�B
S�B
T�B
VB
VB
VB
VB
T�B
T�B
T�B
T�B
VB
W
B
XB
XB
XB
YB
YB
XB
W
B
W
B
ZB
ZB
YB
ZB
YB
ZB
ZB
ZB
YB
ZB
[#B
ZB
YB
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
^5B
]/B
^5B
^5B
_;B
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
`BB
aHB
aHB
bNB
bNB
aHB
aHB
_;B
`BB
`BB
`BB
aHB
bNB
bNB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
gmB
iyB
iyB
jB
jB
jB
jB
jB
iyB
jB
jB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
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
o�B
n�B
m�B
n�B
o�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bi�Be�B\�BE�BM6B}�B��B��B��B��B��B�oB��B�AB��B�DB��B�|B�*B��B�UB�UB�]B�kB�_B�sB�RB�
B�B��B��B��B��B��BlWB?�B��B��B �B�B%�B&LB#TB�B�.B�B�iB��B�TB��B��B�AB��B��B��B}BsMB{Bl�BI�B;�B1AB+�BOB�B�B
��B
�B
��B
�xB
�.B
��B
�@B
�mB
}<B
�6B
��B
��B
�B
xB
s�B
cB
M6B
E�B
2�B
-�B
49B
'mB
B
�B	��B	�NB	�vB	׍B	�kB	�?B	�"B	�B	�[B	�	B	��B	�B	��B	�sB	��B	�\B	��B	�}B	��B	uZB	q[B	d@B	g�B	pB	j�B	\)B	J�B	B'B	@4B	;�B	J	B	CGB	9rB	!�B	#:B	!-B	!�B	�B	�B	�B	�B	�B�kB��B��B�B�:B��B�_B�B�GB�B߾B�RB�~B�B�B�VB�{BĶB�YB��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�_B��B��B�NB�xB�QB��B��B�{B�gB�OB�AB��B��B��B��BxBs�BgBP�BX+Bl=Bc�BT�BI�BE�BMPBLBHKBEBM�BBBB�B>�BF�BJrBJrBB�BHKBDgBIRBBuB7�B3�B.�B49B+kB�B B
�B�B/iB)B �B(�B$�B&�B(�B# B*�B.}B'�BB�B�B�B�B'�B&�B'�B#B%�B*�B)�B%�B$�B(�B+�B'�B,�B/�B,�B,qB/iB2GB0oB*B$tB!|B!�B�B"hB&LB$tB�B�B7B�BWB�B$B BBB�B�B$&B&LB(sB$tB �B"hB!�B�B%zB(�B.}B.�B*�B,�B2|B/�B.�B,�B'B,�B/�B)�B%,B%B1B>�BA�B@�B88B3�B=<B@iB<�B;�BH�BN�BRTBS[BM�BP}BSuBT{BTaBUMBVSBWsBVmBUgBS�BP�BR�BR�Bc�Bg�Bi�Bf�Be�Bi�Bl�Bi�BeB_�B`\Bk6BoOBraB�'B�[B�[B��B��B�mB��B��B��B��B�B�?B�B�hB�SB�~B�0B�XB��B��B��B�}B��B��B��BªB��B��B��B�B��B��B�B��B�B�"B�B�B�zB�zB׍BٴB�B�B�B��B�B��B��B��B��B��B�B�B�B�2B�fB�BB	{B	fB	�B	xB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	$&B	%�B	'B	($B	(XB	/5B	2aB	3MB	4nB	5�B	9rB	<�B	?�B	C�B	D�B	D�B	E�B	F�B	G�B	J�B	NB	O(B	W
B	W$B	VB	UMB	WYB	WYB	W�B	[�B	_pB	dtB	f�B	ezB	e�B	e�B	i�B	j�B	i�B	l�B	k�B	j�B	o�B	q�B	r�B	t�B	t�B	vB	u%B	|B	|B	}<B	B	�4B	�B	�OB	�UB	�UB	�fB	�lB	�RB	��B	�pB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�&B	��B	�
B	�*B	�B	�=B	�B	�5B	�UB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�FB	�MB	�SB	�SB	�YB	�KB	�QB	�QB	�KB	�kB	�OB	�BB	�vB	�pB	ބB	�vB	�vB	ߊB	�B	ߤB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	��B	�B	�B	�B	�B	�B	�0B	�*B	�0B	�"B	�BB
 B
 B
GB
MB
MB
3B
GB
GB
-B
SB
SB
YB
YB
tB
tB
	lB
	lB
fB

rB
JB
~B
~B
^B

�B

�B

�B
�B
}B
�B
�B
uB
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
B
B
 B
!�B
#B
!�B
#B
#B
# B
#�B
%�B
&B
&B
%�B
'B
(
B
($B
)B
)B
($B
($B
(
B
(
B
)B
(>B
)*B
*0B
+6B
,"B
+B
*0B
+6B
-CB
./B
.IB
./B
-CB
,=B
,=B
+QB
,WB
/OB
/OB
.cB
/OB
/OB
/iB
1AB
2aB
3MB
33B
3MB
3MB
3hB
3hB
4nB
5ZB
5?B
5?B
5tB
6`B
7fB
6`B
6`B
5tB
5tB
4nB
4TB
3hB
3�B
5tB
6zB
6zB
9�B
;B
;dB
:^B
:xB
9�B
9�B
;B
;B
:�B
;�B
:�B
:�B
:�B
<�B
=�B
=�B
=�B
=�B
<�B
;�B
<�B
;�B
:�B
=�B
=�B
<�B
>�B
B�B
B�B
A�B
A�B
B�B
C�B
C�B
B�B
C�B
D�B
B�B
C�B
E�B
G�B
F�B
F�B
F�B
G�B
F�B
F�B
E�B
C�B
E�B
F�B
G�B
G�B
G�B
H�B
I�B
J	B
K�B
MB
K�B
MB
L�B
NB
N�B
N�B
N"B
OB
QB
QB
RB
Q�B
RB
RB
R B
R B
RB
RB
RB
SB
SB
S&B
TB
S�B
T,B
UB
T�B
TB
UB
VB
UB
UB
UB
T,B
UB
VB
VB
VB
VB
U2B
U2B
U2B
UMB
V9B
W$B
XEB
XEB
X+B
Y1B
Y1B
XEB
WYB
WYB
ZB
Z7B
YKB
ZQB
Y1B
ZQB
Z7B
ZB
Y1B
ZQB
[#B
Z7B
YeB
[WB
\CB
\CB
\]B
\]B
]dB
]IB
]dB
]IB
\CB
^OB
]dB
^5B
^OB
_;B
^jB
^OB
^jB
^OB
^jB
^jB
_pB
_VB
_VB
_VB
`vB
aHB
aHB
bhB
bNB
aHB
aHB
_VB
`vB
`\B
`vB
abB
b�B
b�B
b�B
cnB
dtB
cnB
dZB
c�B
cnB
cnB
dtB
d�B
d�B
dtB
e�B
f�B
f�B
ffB
f�B
ezB
f�B
g�B
g�B
g�B
g�B
iyB
i�B
j�B
j�B
jB
jB
j�B
i�B
jB
j�B
i�B
i�B
j�B
j�B
j�B
j�B
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
m�B
m�B
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
o�B
n�B
m�B
n�B
o�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805300036452018053000364520180530003645201806221242272018062212422720180622124227201806042121012018060421210120180604212101  JA  ARFMdecpA19c                                                                20180526093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180526003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180526003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180526003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180526003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180526003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180526003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180526003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180526003530  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180526003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20180526005623                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180526153507  CV  JULD            G�O�G�O�F�+�                JM  ARCAJMQC2.0                                                                 20180529153645  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180529153645  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122101  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034227  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                