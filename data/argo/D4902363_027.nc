CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-16T00:35:25Z creation;2016-08-16T00:35:27Z conversion to V3.1;2019-12-19T08:32:56Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160816003525  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_027                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��u8��1   @��u��J @;�֡a���dh�*0U1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D�fD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�  D�@ DӀ D�� D�  D�@ Dԃ3D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw�Cy޸C{޸C}޸C޸C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D~D�Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DN~DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\�D]~D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm�Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��D~D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dҿ
D���D�;�D�{�Dӻ�D���D�;�D�
DԻ�D���D�?
D�{�Dջ�D���D�;�D�{�Dֻ�D���D�8�D�x�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޿
D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�uq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̟�A̝�A̓uA̗�A̕�ȂhẢ7A̋DA̍PẢ7Ȧ+Ả7Ả7A̋DA̍PẢ7A�x�A�r�A�O�A��A���A�p�AƉ7AĴ9A���A��
A�n�A��hA�7LA��#A��-A�O�A��A�E�A�JA�jA��A�1'A�ffA�E�A�?}A��!A�n�A��;A���A�|�A���A�Q�A�+A���A�VA��7A�;dA���A�\)A���A�bNA�XA���A�C�A�S�A���A�v�A��A�ȴA�x�A��jA�1A�~�A�l�A��wA��A���A��uA�-A��A��HA�A�A�bNA���A��\A�A�A��\A���A�\)A���A�v�A���A���A��A�=qA���A�&�A��A���A�/A�A�AdZA~  A}"�A{�#A{%Ay%AwAwhsAv��AuG�AtM�As�
Asx�As7LAs�As�ArĜAr�uArE�Ap��Ap�Ap5?Ao��Aol�An�`AmƨAln�Al-Ak��AjI�Ah~�Ag�Ag��Af�uAe�-Ae�PAeS�Ad(�AchsAa�PA`v�A`  A_�A\�yA\-A[�TA[p�AY�mAX�+AX1AW�AWdZAV��AU�wAT�jAT��AT��AT5?AR�APJAO7LAO%ANȴANI�AM�-AM&�AKhsAI�AG�AG�AFA�AE�AE+AD�HAC�hAB1'AAXA>�9A<��A<r�A<A;�A:��A:JA9+A8��A8bNA85?A8��A8�uA8-A7XA7|�A8^5A7�A4��A49XA3�wA1�-A/�-A.�A.JA-"�A,$�A+�A+x�A+7LA+oA*��A)��A(��A(��A(�A'�wA&��A&ffA%\)A#�
A#33A"�/A"M�A!��A!33A �A �RA�TA��A��A`BA1'A�AA�AhsAXA
=A�\A�
A/A�AO�A�RA�mA
=A��AM�A��Al�A�/A��A��A �A��A
=A
r�A	/Al�A�A�A��AjAI�A��At�A33A&�A�A�RAVA{A�A �y@��R@���@��P@���@���@�
=@�{@�9X@�C�@�@�(�@�9@��@�C�@�J@�@�S�@�u@㝲@���@��@���@�dZ@��@�hs@���@�I�@�M�@�%@��@ա�@��@ҧ�@Ͼw@�M�@�?}@�/@̋D@�r�@�"�@�$�@��#@ź^@ź^@ũ�@�p�@���@��@Ý�@�C�@�o@+@�bN@��H@�$�@�@��`@�o@��@�x�@��j@��@��@��@��R@�n�@�E�@��@�@�O�@�I�@��@���@�dZ@��H@�7L@�"�@���@�-@�O�@�j@��@�\)@��R@��+@�E�@���@���@�C�@��@�I�@�|�@�o@��@��R@�{@���@��^@���@���@��7@��@�&�@�j@�33@��+@�{@�@��@��P@�C�@���@���@�Ĝ@�r�@�A�@���@�"�@���@�^5@�-@�-@�@��@��#@��@�t�@��R@�=q@�V@���@���@��+@�ff@�$�@�@���@�7L@�Q�@�t�@�o@��H@��T@�`B@�V@��j@��u@�r�@�I�@��
@���@�~�@���@���@���@���@�ȴ@��!@���@�$�@��/@��@�bN@�9X@���@���@�|�@���@���@��@���@��+@��@��H@��R@���@��h@���@��9@���@�l�@�
=@�{@��T@�x�@�7L@�V@�Ĝ@��/@��@�%@�V@�Q�@�P@
=@~ȴ@~��@~v�@~v�@~$�@}�@|z�@|(�@{ƨ@z��@z�\@z~�@z�\@zn�@z^5@z~�@{"�@{S�@{C�@z^5@y��@z�\@{��@|Z@}V@}�@}�-@|��@{S�@z��@z�!@z�@yG�@x�@xb@w�P@w\)@w;d@w+@w�@v��@v��@u�@u@u�@u`B@uO�@uO�@up�@up�@t�@s��@sS�@sS�@rM�@rJ@q��@q�@q�#@qX@p��@p��@p�@pr�@o�;@o
=@nff@m��@m�h@m�@m`B@m�@l�@lz�@lZ@l9X@k�F@k�@kC�@j�H@j��@j�@kS�@kt�@k33@i��@i7L@h�u@hbN@hQ�@h1'@hb@h  @g�;@g�@g�@g�@g��@gl�@g+@g�@f��@fE�@e�@e��@e`B@d�@d��@dz�@dz�@dZ@dZ@dj@d(�@c��@ct�@c@b��@b��@b�\@b~�@b~�@a��@`�`@`�@`r�@`�@`r�@`1'@^�@^E�@^{@]�T@]��@]`B@]?}@\��@\�j@\��@\Z@[ƨ@[C�@Z�@Z~�@Y�7@Y%@X��@XbN@Xb@W��@W;d@V�@V��@Vv�@Vff@Vff@Vff@V$�@U@U?}@Tz�@T1@S�@SdZ@SS�@S33@R�@R��@Rn�@RM�@Q��@Q�7@Qhs@Q�@P��@P1'@P �@P1'@P �@O��@O�P@O\)@O�@N�y@N�R@NE�@M@M�h@M�@Mp�@M?}@MV@L��@Lj@L1@K�@J��@J�!@J�\@J~�@J=q@J-@J-@J-@I�@I�7@IG�@H��@HĜ@H�9@Hr�@G�;@G��@G\)@G+@G�@F�y@F�+@F$�@E��@E��@Ep�@EV@D�/@D��@D�@D�D@Dj@D(�@CC�@Co@B�H@B=q@BJ@A�^@@�`@@�u@@�@@1'@@b@?�;@?�@?\)@?;d@?�@>�+@>5?@>{@=@=��@=�-@=p�@=?}@<��@<�@<�/@<�j@<�@<1@:�@:M�@9�^@9hs@9&�@9%@9%@8�9@8�@81'@7�@7;d@6�y@6��@6ff@6{@5@4�@4�j@4��@4��@4j@3�F@3�@333@2�@2��@2=q@1�^@0Ĝ@0�u@0r�@0r�@0bN@0A�@0A�@0A�@0 �@/l�@.�R@.ȴ@.ȴ@.ȴ@.�R@.��@.V@-�@-��@-V@,��@+�
@+��@+�@+t�@+dZ@+C�@+33@+"�@+@*�\@*�@)hs@)�@(�`@(�@(�@(Q�@'�@'l�@'�@&�y@&ȴ@&�R@&��@%O�@$I�@#��@#�m@#�
@#�F@#��@"��@"^5@"M�@"=q@"-@"J@!�@!�^@!�7@!7L@ ��@ �@ 1'@ b@�;@\)@�@�y@ȴ@��@$�@�@��@@�-@�-@�-@��@�h@��@9X@�@1@��@�
@��@t�@t�@t�@o@�\@^5@-@�@�@hs@&�@��@bN@b@�@��@�P@l�@K�@+@+@
=@��@�y@�@�@�R@V@@@�h@�@�@p�@/@�@�@/@�@V@��@�D@j@Z@I�@9X@1@ƨ@dZ@o@�@��@�\@~�@^5@M�@-@��@�7@x�@X@&�@Ĝ@�9@�9@��@bN@Q�@A�@ �@b@|�@\)@\)@\)@\)@K�@�@
=@�@�R@�R@��@ff@@@@@�T@�@`B@O�@/@?}@?}@/@/@/@�@��@��@�@�D@j@I�@(�@��@��@33@o@@
�@
��@
�\@
n�@
n�@
=q@
�@	��@	��@	��@	�@	�#@	��@	��@	�^@	hs@	&�@	&�@��@�@�@1'@�w@�@|�@;d@;d@�@�@�@ȴ@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A̟�A̝�A̓uA̗�A̕�ȂhẢ7A̋DA̍PẢ7Ȧ+Ả7Ả7A̋DA̍PẢ7A�x�A�r�A�O�A��A���A�p�AƉ7AĴ9A���A��
A�n�A��hA�7LA��#A��-A�O�A��A�E�A�JA�jA��A�1'A�ffA�E�A�?}A��!A�n�A��;A���A�|�A���A�Q�A�+A���A�VA��7A�;dA���A�\)A���A�bNA�XA���A�C�A�S�A���A�v�A��A�ȴA�x�A��jA�1A�~�A�l�A��wA��A���A��uA�-A��A��HA�A�A�bNA���A��\A�A�A��\A���A�\)A���A�v�A���A���A��A�=qA���A�&�A��A���A�/A�A�AdZA~  A}"�A{�#A{%Ay%AwAwhsAv��AuG�AtM�As�
Asx�As7LAs�As�ArĜAr�uArE�Ap��Ap�Ap5?Ao��Aol�An�`AmƨAln�Al-Ak��AjI�Ah~�Ag�Ag��Af�uAe�-Ae�PAeS�Ad(�AchsAa�PA`v�A`  A_�A\�yA\-A[�TA[p�AY�mAX�+AX1AW�AWdZAV��AU�wAT�jAT��AT��AT5?AR�APJAO7LAO%ANȴANI�AM�-AM&�AKhsAI�AG�AG�AFA�AE�AE+AD�HAC�hAB1'AAXA>�9A<��A<r�A<A;�A:��A:JA9+A8��A8bNA85?A8��A8�uA8-A7XA7|�A8^5A7�A4��A49XA3�wA1�-A/�-A.�A.JA-"�A,$�A+�A+x�A+7LA+oA*��A)��A(��A(��A(�A'�wA&��A&ffA%\)A#�
A#33A"�/A"M�A!��A!33A �A �RA�TA��A��A`BA1'A�AA�AhsAXA
=A�\A�
A/A�AO�A�RA�mA
=A��AM�A��Al�A�/A��A��A �A��A
=A
r�A	/Al�A�A�A��AjAI�A��At�A33A&�A�A�RAVA{A�A �y@��R@���@��P@���@���@�
=@�{@�9X@�C�@�@�(�@�9@��@�C�@�J@�@�S�@�u@㝲@���@��@���@�dZ@��@�hs@���@�I�@�M�@�%@��@ա�@��@ҧ�@Ͼw@�M�@�?}@�/@̋D@�r�@�"�@�$�@��#@ź^@ź^@ũ�@�p�@���@��@Ý�@�C�@�o@+@�bN@��H@�$�@�@��`@�o@��@�x�@��j@��@��@��@��R@�n�@�E�@��@�@�O�@�I�@��@���@�dZ@��H@�7L@�"�@���@�-@�O�@�j@��@�\)@��R@��+@�E�@���@���@�C�@��@�I�@�|�@�o@��@��R@�{@���@��^@���@���@��7@��@�&�@�j@�33@��+@�{@�@��@��P@�C�@���@���@�Ĝ@�r�@�A�@���@�"�@���@�^5@�-@�-@�@��@��#@��@�t�@��R@�=q@�V@���@���@��+@�ff@�$�@�@���@�7L@�Q�@�t�@�o@��H@��T@�`B@�V@��j@��u@�r�@�I�@��
@���@�~�@���@���@���@���@�ȴ@��!@���@�$�@��/@��@�bN@�9X@���@���@�|�@���@���@��@���@��+@��@��H@��R@���@��h@���@��9@���@�l�@�
=@�{@��T@�x�@�7L@�V@�Ĝ@��/@��@�%@�V@�Q�@�P@
=@~ȴ@~��@~v�@~v�@~$�@}�@|z�@|(�@{ƨ@z��@z�\@z~�@z�\@zn�@z^5@z~�@{"�@{S�@{C�@z^5@y��@z�\@{��@|Z@}V@}�@}�-@|��@{S�@z��@z�!@z�@yG�@x�@xb@w�P@w\)@w;d@w+@w�@v��@v��@u�@u@u�@u`B@uO�@uO�@up�@up�@t�@s��@sS�@sS�@rM�@rJ@q��@q�@q�#@qX@p��@p��@p�@pr�@o�;@o
=@nff@m��@m�h@m�@m`B@m�@l�@lz�@lZ@l9X@k�F@k�@kC�@j�H@j��@j�@kS�@kt�@k33@i��@i7L@h�u@hbN@hQ�@h1'@hb@h  @g�;@g�@g�@g�@g��@gl�@g+@g�@f��@fE�@e�@e��@e`B@d�@d��@dz�@dz�@dZ@dZ@dj@d(�@c��@ct�@c@b��@b��@b�\@b~�@b~�@a��@`�`@`�@`r�@`�@`r�@`1'@^�@^E�@^{@]�T@]��@]`B@]?}@\��@\�j@\��@\Z@[ƨ@[C�@Z�@Z~�@Y�7@Y%@X��@XbN@Xb@W��@W;d@V�@V��@Vv�@Vff@Vff@Vff@V$�@U@U?}@Tz�@T1@S�@SdZ@SS�@S33@R�@R��@Rn�@RM�@Q��@Q�7@Qhs@Q�@P��@P1'@P �@P1'@P �@O��@O�P@O\)@O�@N�y@N�R@NE�@M@M�h@M�@Mp�@M?}@MV@L��@Lj@L1@K�@J��@J�!@J�\@J~�@J=q@J-@J-@J-@I�@I�7@IG�@H��@HĜ@H�9@Hr�@G�;@G��@G\)@G+@G�@F�y@F�+@F$�@E��@E��@Ep�@EV@D�/@D��@D�@D�D@Dj@D(�@CC�@Co@B�H@B=q@BJ@A�^@@�`@@�u@@�@@1'@@b@?�;@?�@?\)@?;d@?�@>�+@>5?@>{@=@=��@=�-@=p�@=?}@<��@<�@<�/@<�j@<�@<1@:�@:M�@9�^@9hs@9&�@9%@9%@8�9@8�@81'@7�@7;d@6�y@6��@6ff@6{@5@4�@4�j@4��@4��@4j@3�F@3�@333@2�@2��@2=q@1�^@0Ĝ@0�u@0r�@0r�@0bN@0A�@0A�@0A�@0 �@/l�@.�R@.ȴ@.ȴ@.ȴ@.�R@.��@.V@-�@-��@-V@,��@+�
@+��@+�@+t�@+dZ@+C�@+33@+"�@+@*�\@*�@)hs@)�@(�`@(�@(�@(Q�@'�@'l�@'�@&�y@&ȴ@&�R@&��@%O�@$I�@#��@#�m@#�
@#�F@#��@"��@"^5@"M�@"=q@"-@"J@!�@!�^@!�7@!7L@ ��@ �@ 1'@ b@�;@\)@�@�y@ȴ@��@$�@�@��@@�-@�-@�-@��@�h@��@9X@�@1@��@�
@��@t�@t�@t�@o@�\@^5@-@�@�@hs@&�@��@bN@b@�@��@�P@l�@K�@+@+@
=@��@�y@�@�@�R@V@@@�h@�@�@p�@/@�@�@/@�@V@��@�D@j@Z@I�@9X@1@ƨ@dZ@o@�@��@�\@~�@^5@M�@-@��@�7@x�@X@&�@Ĝ@�9@�9@��@bN@Q�@A�@ �@b@|�@\)@\)@\)@\)@K�@�@
=@�@�R@�R@��@ff@@@@@�T@�@`B@O�@/@?}@?}@/@/@/@�@��@��@�@�D@j@I�@(�@��@��@33@o@@
�@
��@
�\@
n�@
n�@
=q@
�@	��@	��@	��@	�@	�#@	��@	��@	�^@	hs@	&�@	&�@��@�@�@1'@�w@�@|�@;d@;d@�@�@�@ȴ@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B&�B&�B&�B&�B&�B&�B&�B%�B&�B&�B&�B%�B&�B%�B%�B%�B&�BH�B�B�B�B�B�1B�B~�Bu�Bt�BiyBcTB]/BR�BH�B>wB)�BB  B��B�mB�BB�jB��B��B��B�VB�B|�Bs�Bo�Be`BZBP�BC�B@�B;dB;dB49B0!B%�B�B�BPBBB��B��B�B�yB�HB�B�B��B�}B�qB�XB�-B�B��B�uBt�BhsBbNB]/BS�BC�B=qB1'B�BuBB
��B
�sB
�5B
��B
��B
ɺB
��B
�B
��B
��B
��B
�JB
�B
v�B
iyB
e`B
`BB
VB
L�B
K�B
T�B
e`B
iyB
k�B
iyB
ffB
dZB
YB
S�B
Q�B
K�B
I�B
B�B
;dB
-B
)�B
$�B
�B
\B
+B
B	��B	�B	�B	�B	�yB	�TB	�
B	��B	��B	ƨB	�RB	�!B	�B	��B	��B	��B	��B	�oB	�hB	�VB	�7B	�B	�B	�B	~�B	|�B	m�B	dZB	bNB	_;B	ZB	S�B	N�B	E�B	33B	(�B	"�B	�B	�B	oB	bB	\B	DB	+B	B��B�B�B�B�yB�mB�ZB�BB�BB�TB�B�B�B�mB�yB��B��B�/B�B�BB��B��B��BĜB�}B�^B�LB�FB�9B�9B�-B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�7B�B�B� B~�B}�B}�B|�By�Bw�Bs�Bp�Bl�BjBgmBe`B`BB]/BXBVBR�BQ�BQ�BP�BN�BN�BL�BJ�BI�BD�BD�BC�BB�BA�B@�BA�B?}B?}B?}B?}B>wB=qB<jB;dB9XB7LB5?B49B33B2-B0!B0!B.B,B,B&�B%�B%�B$�B"�B"�B"�B!�B!�B!�B!�B!�B"�B"�B"�B"�B$�B#�B$�B%�B$�B&�B&�B(�B(�B'�B'�B.B-B/B.B.B.B.B.B/B/B0!B0!B0!B0!B49B5?B5?B5?B7LB9XB:^B:^B<jB=qB>wB?}B?}B@�B@�B@�BA�BB�BD�BE�BE�BE�BF�BJ�BP�BP�BR�BT�BW
BXBZB\)B\)B]/B^5BbNBffBjBn�Br�Bt�Bw�By�B}�B� B�B�B�B�B�B�B�%B�+B�+B�+B�+B�DB�{B��B��B��B��B��B��B�B��B�B�!B�FB�LB�^B�wBBĜB��B��B��B��B�B�)B�/B�;B�HB�NB�TB�fB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	B	B	+B	DB	\B	VB	VB	\B	hB	hB	hB	bB	hB	hB	uB	�B	/B	33B	49B	49B	49B	33B	33B	2-B	1'B	/B	/B	/B	0!B	33B	6FB	:^B	=qB	>wB	?}B	?}B	B�B	B�B	C�B	F�B	H�B	I�B	K�B	L�B	M�B	O�B	R�B	R�B	T�B	[#B	_;B	`BB	bNB	dZB	dZB	e`B	hsB	iyB	jB	m�B	m�B	t�B	z�B	}�B	� B	�B	�B	�%B	�+B	�1B	�1B	�=B	�DB	�JB	�JB	�PB	�PB	�PB	�PB	�PB	�PB	�VB	�\B	�\B	�\B	�\B	�bB	�bB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�!B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�3B	�?B	�FB	�LB	�RB	�XB	�dB	�qB	�wB	�}B	�wB	�}B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�ZB	�ZB	�`B	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
PB
PB
VB
VB
VB
VB
\B
bB
bB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
.B
/B
/B
0!B
/B
/B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
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
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
C�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
N�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
YB
YB
YB
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
\)B
]/B
]/B
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
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
`BB
`BB
aHB
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
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
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
jB
jB
jB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B&�B'B'B&�B&�B'B&�B%�B&�B&�B&�B%�B'B%�B&2B&�B*KBNB�;B��B��B��B��B�tB�oBy>Bx�Bm]BhsBa�BV�BL�BEB1vB�B�B��B�B�B��B��B�TB�KB��B�+B~�Bu�BtBhsB]dBRoBD�BA�B<�B<�B5tB2aB'�BB�B�BB�B��B��B��B�B�B��BרB�bB�4B�BB�^B�hB�vB��B��Bv+BiDBcnB_BU�BD�B?}B3�B!�B�B%B
��B
��B
�;B
өB
ϫB
�DB
�uB
��B
��B
�B
�B
��B
�9B
x8B
jKB
fLB
bB
W
B
MjB
LdB
UgB
ezB
i�B
k�B
i�B
gB
e�B
Y�B
T�B
R�B
LdB
J�B
C�B
<�B
-�B
+B
&�B
�B
HB
�B
MB	��B	�9B	�|B	�B	��B	�FB	�yB	��B	�dB	��B	�XB	��B	�B	��B	�`B	�QB	�B	�B	�oB	��B	�XB	�GB	��B	�B	�;B	�B	n}B	d�B	b�B	_�B	[	B	UMB	Q4B	H1B	4�B	)�B	#�B	qB	?B	[B	 B	4B	B	
=B	B��B�B��B�wB�eB�B�B�B�\B�:B��B�GB�IB�RB�yB��B��BބB�B�B�?B��B�B��B��B��B��B��B��B�B�MB�'B��B��B�$B�8B��B�TB�vB�~B�1B�yB��B�vB�NB��B�B�OB�B�[B��B��B�oB��BHB~]B~�B}�B{Bx�Bu%Bq�Bm�Bk�Bh�BgRBbNB_;BY�BV�BS[BRoBR�BQ�BO�BP.BN�BMBK^BE�BE�BDMBCBBABAUBBB?�B?�B?�B@4B?.B>]B=�B=VB:�B8RB6+B5ZB4�B33B1vB0�B/ B.B-�B'�B&�B&�B&B$&B$�B#�B"hB"�B"�B#B"�B#TB#TB#�B$@B%�B$�B&fB&�B&�B(�B(
B)�B)_B)B*eB/ B-�B/iB.IB./B.cB.}B.�B/�B/�B0�B0�B1B1�B5?B5�B5�B6FB8�B:DB:�B;B=B=�B>�B?�B?�B@�B@�B@�BB'BCaBEBE�BF%BFYBHBK�BQNBQ�BS�BU�BW�BX�BZ�B\xB\�B]�B_!Bc�Bg�BkkBoOBsBu%Bx8Bz^B~BB�4B�;B�;B�'B�[B��B��B�B��B��B��B��B�JB��B�9B�qB��B�*B�DB�B��B�eB�QB�UB�zB��B��B��BÖB�SB�B��B��B��B�7B�]B�dBߤB�B�B��B�8B�=B�B��B�aB�3B�B�%B��B�$B�$B�rB��B��B�B�B��B	 B	AB	gB	zB	�B	.B	�B	�B	�B	�B	�B	�B	 B	:B	�B	�B	B	/ B	3hB	4nB	4�B	5B	3�B	3�B	2�B	1�B	/�B	/�B	/OB	0oB	3�B	6�B	:�B	=�B	>]B	?�B	?�B	C-B	CB	C�B	F�B	H�B	I�B	K�B	MB	N<B	PbB	S&B	S@B	UgB	[WB	_VB	`BB	b�B	dZB	dZB	e`B	h�B	i�B	j�B	m�B	mwB	tnB	z�B	}�B	� B	�'B	��B	��B	�_B	�fB	��B	��B	��B	��B	�~B	��B	��B	��B	��B	�jB	��B	��B	�vB	�vB	��B	��B	�}B	�bB	�}B	��B	��B	��B	��B	�-B	�B	��B	�B	�B	�B	�>B	�6B	�)B	�CB	�cB	�oB	��B	�oB	�UB	�;B	�UB	�UB	�[B	�aB	�MB	�MB	�hB	�ZB	�zB	�fB	�RB	�rB	�dB	��B	��B	� B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	�B	�B	�.B	�B	� B	�&B	�,B	�EB	�KB	�7B	�QB	�B	�QB	�]B	�dB	�jB	ߊB	�vB	�|B	�bB	�|B	�|B	�B	�B	�tB	�B	�zB	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�	B	�0B	�B	�"B	�"B	�B	��B	�B	�"B	�(B	�.B
 iB
;B
AB
GB
GB
-B
-B
MB
SB
SB
mB
EB
_B
zB
fB
fB
	RB
	7B
	RB
	lB
	RB

rB

rB

XB
xB
�B
�B
�B
�B
pB
pB
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 �B
 �B
!B
#B
# B
# B
$�B
%B
%B
%B
%B
%B
%B
&B
&B
'8B
'B
'B
'B
'�B
)*B
)B
*0B
*0B
+B
+6B
+6B
+B
+kB
,�B
-]B
.cB
/5B
/OB
0;B
/OB
/5B
0UB
0UB
0oB
1[B
2GB
2aB
2GB
2|B
3hB
3�B
3MB
3MB
49B
4nB
5�B
6`B
6`B
6zB
7fB
7�B
7�B
8�B
9�B
9�B
9rB
9rB
9rB
9rB
8�B
8lB
9�B
9�B
:xB
:xB
:xB
:xB
:�B
;�B
;�B
;�B
;�B
;�B
<�B
=�B
=qB
=�B
=qB
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
DB
FB
E�B
E�B
E�B
E�B
E�B
E�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
LB
L�B
MB
MB
N"B
N�B
OB
OB
OB
O(B
PB
PB
O�B
O�B
O�B
O�B
PB
PB
PHB
QB
Q B
P�B
Q B
QB
QB
QB
Q B
QB
QB
QB
QB
QB
R B
RB
R:B
R B
R B
R B
S&B
SB
SB
TB
TB
TB
T,B
S�B
TB
TB
T�B
UB
T�B
UB
U2B
U2B
UB
V9B
VB
VB
V9B
V9B
VB
VB
VB
VB
VB
V9B
W$B
W$B
W
B
W$B
W?B
XEB
XEB
X_B
YKB
YKB
Y1B
Z7B
ZB
ZQB
ZQB
ZQB
ZkB
[=B
[=B
[=B
[=B
\CB
\)B
\CB
\]B
\CB
\)B
]IB
]IB
\]B
]~B
^5B
^5B
^5B
^5B
^jB
^jB
^jB
^OB
_;B
_;B
_VB
_pB
_pB
`\B
`\B
`BB
_VB
`vB
`vB
`\B
`vB
abB
`BB
`BB
abB
aHB
aHB
a|B
abB
abB
a|B
a|B
bhB
bhB
b�B
b�B
bhB
cnB
cTB
c�B
c�B
c�B
dZB
dtB
d�B
d�B
ezB
e`B
e`B
e`B
ezB
e`B
ezB
e�B
e�B
f�B
f�B
f�B
f�B
gmB
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
jB
j�B
j�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608200035272016082000352720160820003527201806221212332018062212123320180622121233201804050405032018040504050320180405040503  JA  ARFMdecpA19c                                                                20160816093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160816003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160816003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160816003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160816003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160816003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160816003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160816003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160816003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160816003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20160816011900                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160816153605  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160819153527  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160819153527  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190503  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031233  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                