CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-28T11:01:26Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ol   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۔   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ޔ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20220228110126  20220228110126  5906156 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               iA   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @ٽ�2~c1   @ٽ`� @7o��v��cL��E�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         iA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
�HDw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<qHD<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��DvqHDv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�8�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D��
D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�33A�1'A�"�A���A��jA��DA�n�A�G�A�33A� �A��A��A�{A�1A���A��mA�ȴA��FA��A�K�A��wA�E�A���A���A���A��wA�A��A�?}A�=qA��A��A��hA��A�r�A�l�A�\)A�?}A�5?A� �A�
=A���A���A��^A��A���A�l�A�I�A�;dA�/A��A�
=A���A��A��;A��RA�33A� �A�oA���A��hA��HA�VA�{A���A��
A�I�A�M�A��;A��FA��yA�ffA���A�
=A�p�A��DA�(�A��^A�;dA�;dA�-A�9XA��A�bNA�A�A�
=A���A�5?A���A�7LA�S�A���A�|�A�7LA��-A���A�l�A��A�?}A�ZA���A���A�Q�A���A�n�A�A��A���A��`A�&�A��mA��A�n�A�(�A�VA�r�A�ȴA�7LA�7A}�#Ay�Awt�Atz�Ar�\Ap�`Am�^Al�\AkƨAjz�AjZAiS�Ae�7Ab{A`jA_t�A^�`A^Q�A]G�A\-A[VAZE�AX�!AV^5AQdZAO|�AM�FALz�AL1AK�PAJAIC�AH�AG%ADffAC"�AA�^A@��A@ȴA?�A< �A:��A:1A9XA6�A5|�A5;dA5&�A4�uA3t�A2A�A1��A0��A.��A-��A-;dA,{A*n�A)�A(�A(�\A'O�A%l�A#VA"Q�A!/A�AVA��A33A��AoA�A�A�yAn�A  A�/A��A�A\)A/A��A�`A��A�A�DAE�A�7A��AE�A33A
�yA
�!A
��A
ffA	��A	;dA�A��A��A��AC�A�DA��A�A �A   @��@��@��T@��;@��+@�1'@�@���@�@�9X@���@�bN@���@��@�;d@�E�@��H@�@�C�@�|�@���@ش9@�l�@֧�@�x�@ӥ�@�@�/@�j@�S�@Ώ\@��T@�G�@̼j@�A�@˕�@�~�@ȣ�@�K�@�n�@�M�@�$�@��#@ũ�@�`B@��/@Ĵ9@��;@�dZ@�+@��y@�v�@���@�`B@�7L@��@��;@�33@��@�I�@�l�@�V@�O�@�z�@�;d@�$�@��^@�V@���@�v�@��@���@�?}@�z�@�"�@��@�V@��@�`B@���@�bN@��m@�S�@��@���@��h@��`@���@�v�@�{@��7@��@�1'@�;d@�^5@�@��-@��@�Ĝ@�I�@�l�@��\@�{@���@��@�G�@��u@�r�@�9X@��@��m@��;@���@��F@��P@�;d@�33@�@�ȴ@�v�@�ff@�V@�-@�J@���@��@��w@�S�@�K�@���@�v�@��+@���@��h@�`B@�p�@�hs@�O�@�X@�x�@�~�@�|�@�K�@�K�@�o@��@�@��h@�p�@�/@�O�@���@��u@�  @�t�@�dZ@�dZ@��y@�=q@�7L@��9@��9@��/@��@�7L@�Ĝ@��9@�Q�@�(�@��@��@�dZ@�;d@��H@���@��R@���@�V@�E�@���@��@�hs@�&�@�/@�7L@�%@��@�A�@�1'@�I�@�Q�@�(�@�1'@�9X@�  @��
@��w@��@�|�@��@���@���@���@��h@�p�@�%@���@�j@�9X@� �@�b@��m@�ƨ@��@��@��F@�|�@�"�@��H@��R@�V@�J@���@��-@���@�x�@�p�@�X@�X@�G�@��@��@�Ĝ@�A�@��@�1@���@���@���@���@�l�@�\)@�o@�
=@�@���@��@���@�M�@�5?@�$�@�{@�J@�@���@���@�p�@�?}@���@��@��@l�@\)@+@~�y@~v�@}�T@}�-@}p�@}O�@|�@|�@|j@|9X@{�F@{S�@z��@z^5@zM�@z�@y�@x��@x�9@x�@xr�@x �@w��@vȴ@v��@v��@vv�@v{@v@u�T@u@uV@t��@t�j@t�@t�@t��@tz�@t�@sS�@r�H@r^5@rM�@r�@qhs@q&�@q%@pr�@o��@o
=@n��@nv�@nV@n{@m�@m�-@m�@m/@m/@mV@l�D@lZ@l�@k�
@kƨ@k�@kdZ@k"�@j��@j-@i��@iX@h��@h �@g�;@g��@f�y@f5?@f5?@e�T@ep�@e/@dI�@cƨ@c�@cdZ@co@b�H@b��@bn�@bJ@a�@a��@a��@a��@a�7@a�7@ahs@`��@`bN@` �@_�w@_l�@^�@^V@^E�@^$�@]�@]�@]�-@]p�@]O�@\�@\j@\9X@[ƨ@[33@Z�H@Z�!@Zn�@Z�@Y��@Y�^@Yx�@Y&�@X�@XQ�@W�;@WK�@V��@V�+@Vv�@W+@W
=@V�R@Vv�@U�-@UO�@Tj@SC�@So@R�H@R��@Q�@QX@P��@P�9@PQ�@O�;@Ol�@O�@N�y@N��@N@M@M�h@MO�@M?}@M�@L�@L�/@LZ@L�@K��@K�F@Kt�@K33@Ko@J�!@J�\@Jn�@J-@I�#@I��@IG�@H�`@HĜ@HQ�@G�;@G;d@G�@G
=@F�y@F�@F�R@F��@F��@F$�@E�@D��@D��@D��@D�D@Dz�@DZ@D1@Cƨ@CdZ@C"�@C@B�@B�H@B�\@Bn�@Bn�@B^5@BM�@B=q@B=q@B�@A��@A�#@A��@AG�@@�`@@�9@@bN@@1'@@b@?��@?�@?�P@>ȴ@>V@>{@=�T@=�-@=��@=��@=��@=��@=�h@=O�@<�D@<Z@<9X@;�m@;�
@;�
@;�m@;��@;dZ@;S�@;S�@;C�@;33@;"�@;"�@:�\@:=q@:J@9�^@9��@9x�@9G�@9%@8��@8�@81'@7�@7��@7�P@7|�@7;d@6��@6v�@6v�@6E�@5�@5@5�h@5/@4�@41@3��@3o@2��@2^5@2M�@1�@1��@1�7@1hs@1&�@0�9@0��@0�@0 �@/�@/��@/��@.��@.�+@.ff@.5?@.{@-��@-�@-�@-p�@-V@,z�@,j@,Z@,9X@+�
@+C�@*��@*~�@*-@)��@)�#@)��@)G�@)&�@(��@(Ĝ@(r�@( �@'�;@'�@'��@'��@'�@'K�@'
=@&�R@&ff@%�@%��@%�-@%��@%?}@$��@$I�@$�@#��@#��@#��@#33@"�!@"M�@!��@!�7@!&�@!%@ �`@ �@ Q�@�@�P@+@�@v�@5?@@@�@�@�T@��@@�-@�h@/@��@�D@I�@��@�
@��@@�H@�H@�H@�H@��@��@�\@^5@^5@^5@M�@�@��@��@��@��@x�@7L@�@�`@��@��@Ĝ@��@r�@1'@�;@��@�P@�P@\)@+@�y@�+@V@$�@�@��@�h@p�@`B@`B@`B@O�@�@��@9X@��@�m@�F@��@��@��@dZ@dZ@C�@C�@33@�@��@~�@n�@^5@=q@�@�@�@J@��@�#@hs@7L@�@��@�9@��@�@r�@bN@A�@ �@  @�@�;@��@K�@�@��@�y@�@ȴ@�R@v�@V@$�@�@�-@O�@/@�@�@��@�D@9X@�
@dZ@C�@"�@
�!@
^5@	��@	��@	��@	x�@	X@	7L@	7L@	�@	�@��@�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�33A�33A�1'A�"�A���A��jA��DA�n�A�G�A�33A� �A��A��A�{A�1A���A��mA�ȴA��FA��A�K�A��wA�E�A���A���A���A��wA�A��A�?}A�=qA��A��A��hA��A�r�A�l�A�\)A�?}A�5?A� �A�
=A���A���A��^A��A���A�l�A�I�A�;dA�/A��A�
=A���A��A��;A��RA�33A� �A�oA���A��hA��HA�VA�{A���A��
A�I�A�M�A��;A��FA��yA�ffA���A�
=A�p�A��DA�(�A��^A�;dA�;dA�-A�9XA��A�bNA�A�A�
=A���A�5?A���A�7LA�S�A���A�|�A�7LA��-A���A�l�A��A�?}A�ZA���A���A�Q�A���A�n�A�A��A���A��`A�&�A��mA��A�n�A�(�A�VA�r�A�ȴA�7LA�7A}�#Ay�Awt�Atz�Ar�\Ap�`Am�^Al�\AkƨAjz�AjZAiS�Ae�7Ab{A`jA_t�A^�`A^Q�A]G�A\-A[VAZE�AX�!AV^5AQdZAO|�AM�FALz�AL1AK�PAJAIC�AH�AG%ADffAC"�AA�^A@��A@ȴA?�A< �A:��A:1A9XA6�A5|�A5;dA5&�A4�uA3t�A2A�A1��A0��A.��A-��A-;dA,{A*n�A)�A(�A(�\A'O�A%l�A#VA"Q�A!/A�AVA��A33A��AoA�A�A�yAn�A  A�/A��A�A\)A/A��A�`A��A�A�DAE�A�7A��AE�A33A
�yA
�!A
��A
ffA	��A	;dA�A��A��A��AC�A�DA��A�A �A   @��@��@��T@��;@��+@�1'@�@���@�@�9X@���@�bN@���@��@�;d@�E�@��H@�@�C�@�|�@���@ش9@�l�@֧�@�x�@ӥ�@�@�/@�j@�S�@Ώ\@��T@�G�@̼j@�A�@˕�@�~�@ȣ�@�K�@�n�@�M�@�$�@��#@ũ�@�`B@��/@Ĵ9@��;@�dZ@�+@��y@�v�@���@�`B@�7L@��@��;@�33@��@�I�@�l�@�V@�O�@�z�@�;d@�$�@��^@�V@���@�v�@��@���@�?}@�z�@�"�@��@�V@��@�`B@���@�bN@��m@�S�@��@���@��h@��`@���@�v�@�{@��7@��@�1'@�;d@�^5@�@��-@��@�Ĝ@�I�@�l�@��\@�{@���@��@�G�@��u@�r�@�9X@��@��m@��;@���@��F@��P@�;d@�33@�@�ȴ@�v�@�ff@�V@�-@�J@���@��@��w@�S�@�K�@���@�v�@��+@���@��h@�`B@�p�@�hs@�O�@�X@�x�@�~�@�|�@�K�@�K�@�o@��@�@��h@�p�@�/@�O�@���@��u@�  @�t�@�dZ@�dZ@��y@�=q@�7L@��9@��9@��/@��@�7L@�Ĝ@��9@�Q�@�(�@��@��@�dZ@�;d@��H@���@��R@���@�V@�E�@���@��@�hs@�&�@�/@�7L@�%@��@�A�@�1'@�I�@�Q�@�(�@�1'@�9X@�  @��
@��w@��@�|�@��@���@���@���@��h@�p�@�%@���@�j@�9X@� �@�b@��m@�ƨ@��@��@��F@�|�@�"�@��H@��R@�V@�J@���@��-@���@�x�@�p�@�X@�X@�G�@��@��@�Ĝ@�A�@��@�1@���@���@���@���@�l�@�\)@�o@�
=@�@���@��@���@�M�@�5?@�$�@�{@�J@�@���@���@�p�@�?}@���@��@��@l�@\)@+@~�y@~v�@}�T@}�-@}p�@}O�@|�@|�@|j@|9X@{�F@{S�@z��@z^5@zM�@z�@y�@x��@x�9@x�@xr�@x �@w��@vȴ@v��@v��@vv�@v{@v@u�T@u@uV@t��@t�j@t�@t�@t��@tz�@t�@sS�@r�H@r^5@rM�@r�@qhs@q&�@q%@pr�@o��@o
=@n��@nv�@nV@n{@m�@m�-@m�@m/@m/@mV@l�D@lZ@l�@k�
@kƨ@k�@kdZ@k"�@j��@j-@i��@iX@h��@h �@g�;@g��@f�y@f5?@f5?@e�T@ep�@e/@dI�@cƨ@c�@cdZ@co@b�H@b��@bn�@bJ@a�@a��@a��@a��@a�7@a�7@ahs@`��@`bN@` �@_�w@_l�@^�@^V@^E�@^$�@]�@]�@]�-@]p�@]O�@\�@\j@\9X@[ƨ@[33@Z�H@Z�!@Zn�@Z�@Y��@Y�^@Yx�@Y&�@X�@XQ�@W�;@WK�@V��@V�+@Vv�@W+@W
=@V�R@Vv�@U�-@UO�@Tj@SC�@So@R�H@R��@Q�@QX@P��@P�9@PQ�@O�;@Ol�@O�@N�y@N��@N@M@M�h@MO�@M?}@M�@L�@L�/@LZ@L�@K��@K�F@Kt�@K33@Ko@J�!@J�\@Jn�@J-@I�#@I��@IG�@H�`@HĜ@HQ�@G�;@G;d@G�@G
=@F�y@F�@F�R@F��@F��@F$�@E�@D��@D��@D��@D�D@Dz�@DZ@D1@Cƨ@CdZ@C"�@C@B�@B�H@B�\@Bn�@Bn�@B^5@BM�@B=q@B=q@B�@A��@A�#@A��@AG�@@�`@@�9@@bN@@1'@@b@?��@?�@?�P@>ȴ@>V@>{@=�T@=�-@=��@=��@=��@=��@=�h@=O�@<�D@<Z@<9X@;�m@;�
@;�
@;�m@;��@;dZ@;S�@;S�@;C�@;33@;"�@;"�@:�\@:=q@:J@9�^@9��@9x�@9G�@9%@8��@8�@81'@7�@7��@7�P@7|�@7;d@6��@6v�@6v�@6E�@5�@5@5�h@5/@4�@41@3��@3o@2��@2^5@2M�@1�@1��@1�7@1hs@1&�@0�9@0��@0�@0 �@/�@/��@/��@.��@.�+@.ff@.5?@.{@-��@-�@-�@-p�@-V@,z�@,j@,Z@,9X@+�
@+C�@*��@*~�@*-@)��@)�#@)��@)G�@)&�@(��@(Ĝ@(r�@( �@'�;@'�@'��@'��@'�@'K�@'
=@&�R@&ff@%�@%��@%�-@%��@%?}@$��@$I�@$�@#��@#��@#��@#33@"�!@"M�@!��@!�7@!&�@!%@ �`@ �@ Q�@�@�P@+@�@v�@5?@@@�@�@�T@��@@�-@�h@/@��@�D@I�@��@�
@��@@�H@�H@�H@�H@��@��@�\@^5@^5@^5@M�@�@��@��@��@��@x�@7L@�@�`@��@��@Ĝ@��@r�@1'@�;@��@�P@�P@\)@+@�y@�+@V@$�@�@��@�h@p�@`B@`B@`B@O�@�@��@9X@��@�m@�F@��@��@��@dZ@dZ@C�@C�@33@�@��@~�@n�@^5@=q@�@�@�@J@��@�#@hs@7L@�@��@�9@��@�@r�@bN@A�@ �@  @�@�;@��@K�@�@��@�y@�@ȴ@�R@v�@V@$�@�@�-@O�@/@�@�@��@�D@9X@�
@dZ@C�@"�@
�!@
^5@	��@	��@	��@	x�@	X@	7L@	7L@	�@	�@��@�@1'@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�By�By�B{�B�B�DB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�'B�wBɺB��B��B��B��B�5B�mB�B��BB
=BDBJBPBVBbBhBoBoB{B�B�B�B�B�B �B �B �B �B �B �B �B �B �B�B�B�B�B�B�B�BVBDB��B�B�B�mB�BB�BƨB�XB��B�{B�\B�%B~�By�Br�B`BBR�BG�BB�B?}B=qB;dB0!B!�B�BB�B�yB�TB�5B��BB��B�BVBD�B,B#�B�B{BB
��B
��B
�B
�`B
��B
ÖB
�wB
�B
��B
}�B
l�B
bNB
K�B
6FB
(�B
	7B	��B	�mB	�
B	��B	�XB	�B	��B	��B	��B	��B	� B	m�B	_;B	YB	T�B	P�B	K�B	C�B	=qB	7LB	.B	"�B	B��B�B�sB�`B�NB�/B�B��B��B��B�XB�3B�B�B��B��B��B�{B�oB�PB�1B�1B�+B�%B�B� B}�By�Bw�Br�Bp�Bn�BjBhsBffBe`BbNB_;BYBW
BVBR�BP�BN�BL�BL�BK�BJ�BH�BG�BG�BF�BD�BC�B@�B>wB=qB>wB;dB:^B8RB7LB6FB6FB5?B49B33B2-B2-B1'B1'B0!B0!B.B.B,B,B)�B(�B&�B'�B#�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B#�B#�B$�B&�B,B-B0!B49B6FB8RB=qBA�BC�BF�BK�BQ�BW
BXBXBXBXBYBYBZB[#B^5B`BB`BBaHBcTBffBgmBgmBgmBjBk�Bo�Bs�Bu�Bw�By�Bz�B~�B� B� B�B�B�+B�1B�1B�7B�JB�bB�bB�oB�oB�uB��B��B��B��B��B��B��B�B�?B�^B�dB�wB��BŢBɺB��B��B��B��B��B��B�#B�;B�NB�ZB�`B�fB�B�B�B�B�B��B	  B	B	B		7B	JB	bB	{B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	 �B	$�B	%�B	&�B	)�B	,B	.B	0!B	5?B	8RB	<jB	A�B	I�B	XB	ZB	\)B	_;B	dZB	ffB	k�B	n�B	o�B	t�B	u�B	u�B	v�B	y�B	z�B	y�B	z�B	y�B	x�B	y�B	{�B	~�B	�B	�B	�B	�+B	�1B	�1B	�1B	�+B	�+B	�+B	�+B	�+B	�+B	�1B	�1B	�7B	�=B	�DB	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�LB	�dB	�jB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�ZB	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
JB
PB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
2-B
33B
49B
49B
49B
33B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
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
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
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
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
H�B
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
L�B
L�B
M�B
M�B
O�B
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
T�B
VB
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
\)B
\)B
]/B
^5B
^5B
^5B
^5B
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
bNB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
r�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
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
z�B
z�B
{�B
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
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 By�By�B{�B�B�DB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�'B�wBɺB��B��B��B��B�5B�mB�B��BB
=BDBJBPBVBbBhBoBoB{B�B�B�B�B�B �B �B �B �B �B �B �B �B �B�B�B�B�B�B�B�BVBDB��B�B�B�mB�BB�BƨB�XB��B�{B�\B�%B~�By�Br�B`BBR�BG�BB�B?}B=qB;dB0!B!�B�BB�B�yB�TB�5B��BB��B�BVBD�B,B#�B�B{BB
��B
��B
�B
�`B
��B
ÖB
�wB
�B
��B
}�B
l�B
bNB
K�B
6FB
(�B
	7B	��B	�mB	�
B	��B	�XB	�B	��B	��B	��B	��B	� B	m�B	_;B	YB	T�B	P�B	K�B	C�B	=qB	7LB	.B	"�B	B��B�B�sB�`B�NB�/B�B��B��B��B�XB�3B�B�B��B��B��B�{B�oB�PB�1B�1B�+B�%B�B� B}�By�Bw�Br�Bp�Bn�BjBhsBffBe`BbNB_;BYBW
BVBR�BP�BN�BL�BL�BK�BJ�BH�BG�BG�BF�BD�BC�B@�B>wB=qB>wB;dB:^B8RB7LB6FB6FB5?B49B33B2-B2-B1'B1'B0!B0!B.B.B,B,B)�B(�B&�B'�B#�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B#�B#�B$�B&�B,B-B0!B49B6FB8RB=qBA�BC�BF�BK�BQ�BW
BXBXBXBXBYBYBZB[#B^5B`BB`BBaHBcTBffBgmBgmBgmBjBk�Bo�Bs�Bu�Bw�By�Bz�B~�B� B� B�B�B�+B�1B�1B�7B�JB�bB�bB�oB�oB�uB��B��B��B��B��B��B��B�B�?B�^B�dB�wB��BŢBɺB��B��B��B��B��B��B�#B�;B�NB�ZB�`B�fB�B�B�B�B�B��B	  B	B	B		7B	JB	bB	{B	�B	�B	�B	 �B	�B	�B	�B	�B	�B	 �B	$�B	%�B	&�B	)�B	,B	.B	0!B	5?B	8RB	<jB	A�B	I�B	XB	ZB	\)B	_;B	dZB	ffB	k�B	n�B	o�B	t�B	u�B	u�B	v�B	y�B	z�B	y�B	z�B	y�B	x�B	y�B	{�B	~�B	�B	�B	�B	�+B	�1B	�1B	�1B	�+B	�+B	�+B	�+B	�+B	�+B	�1B	�1B	�7B	�=B	�DB	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�9B	�LB	�dB	�jB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�/B	�5B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�ZB	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B
JB
PB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
2-B
33B
49B
49B
49B
33B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
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
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
E�B
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
G�B
G�B
G�B
H�B
H�B
G�B
G�B
G�B
G�B
H�B
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
L�B
L�B
M�B
M�B
O�B
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
T�B
VB
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
\)B
\)B
]/B
^5B
^5B
^5B
^5B
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
bNB
bNB
bNB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
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
r�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
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
z�B
z�B
{�B
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
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220228110126                              AO  ARCAADJP                                                                    20220228110126    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220228110126  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220228110126  QCF$                G�O�G�O�G�O�0               