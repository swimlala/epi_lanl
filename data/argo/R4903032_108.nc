CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-06-08T09:00:40Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20210608090040  20210608090040  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               lA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @�zԿ�8�1   @�z�l� @:�5?|��c��hr�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         lA   A   F   @���@�33A��A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D���D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�
=@�
>A�A=�A]�A}�A�(�A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[�C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��
D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�8�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�x�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A��
A��/A��;AŃA��A�1A�C�A�33A�p�A�dZA��RA�v�A�&�A�G�A�/A��A��A�"�A��A���A���A��A���A��A���A���A�`BA���A�"�A���A�O�A�S�A�JA��A�5?A�hsA���A���A���A�x�A�hsA�K�A�/A��A��A��A�M�A���A���A��`A��!A�oA�$�A��^A�p�A��\A��A���A�A�A�(�A�ȴA��wA�ffA��!A��^A�bNA�XA�?}A��A��yA���A��wA�/A�  A�bA�7LA���A���A���A���A�/A�z�A��RA~A{��AzAx�9AvVAs�mApv�Am��Al�/Ak`BAi�
AioAh�9AhE�Af�9Aep�Ae?}Ad~�AcAcS�Ab�RAbQ�Aa��Aa"�A`��A_C�A]��A]oA\ȴA\�9A\��A\E�A[�hAZ��AY�AW��AT�HATr�ASdZAQ�AQK�AP�jAP�AO�hAOoAM\)AL�\AK��AI��AH�AH~�AG��AF��AEƨAEADbNAC�hAAA@��A@^5A?K�A>�jA>n�A<�/A;K�A9�A9%A8�jA8bA7�PA7A6~�A61A4�A3�7A2 �A1"�A/��A.I�A-�A+�FA*��A*�A)��A)��A)\)A(��A(v�A(  A'G�A'&�A&ȴA&��A&ffA%�
A$�HA$5?A#\)A"1'A!VA M�A��AS�A�Av�A?}A1'A�7A?}A�/A�\Az�AJA\)A$�A��A�AA�A��A�A�;A�uA?}A�A�DAhsA�!A^5AbA��A
�A	�A�A��A$�Ap�AoA  A�jAbA�A�uA��A�FA��AhsA�AA ��A 5?@��@��@�1@��@��h@�bN@��@�7L@�;d@�=q@�^@��;@��@�o@�$�@���@�D@��
@��@�^@�j@��@�@�ƨ@�~�@ݩ�@ۅ@��#@؋D@֏\@�7L@ӕ�@Ұ!@�V@���@·+@�I�@�33@�-@���@ɺ^@ə�@�Q�@�ƨ@�O�@��9@�1@�o@��@��u@�o@��y@��+@��@���@��@�G�@���@��@���@�r�@���@���@��@��/@��@��D@�9X@���@�t�@�"�@�v�@�{@�&�@�I�@�dZ@��!@�hs@���@���@���@��@�I�@�  @��m@�\)@��@�~�@�J@���@�?}@�j@�S�@�$�@�p�@�&�@�%@���@���@�K�@�V@��@��^@�O�@���@��D@�9X@�  @���@�"�@��@��-@�hs@�p�@�p�@��h@���@��-@���@��h@��-@��@�z�@�5?@�?}@��@��@���@���@��@�ƨ@���@�C�@�
=@���@�ȴ@���@�-@�{@�@��@���@��@��@��j@��@��D@�j@�b@�t�@��@�$�@��^@�hs@�V@���@��/@��/@��/@��/@���@��u@�1@�l�@��\@�=q@�{@�7L@�x�@�&�@��j@�z�@� �@��F@�l�@�o@��!@�n�@�M�@�J@��^@��h@�hs@�/@��@��/@���@�Z@�b@�  @���@���@�dZ@�@��H@���@�v�@�ff@�5?@��@��T@�@���@��@�p�@�X@�G�@�7L@���@���@���@��u@�z�@��@���@���@�Q�@;d@�@�@~�y@~$�@}�T@}�@|��@|(�@|I�@|�@|1@{ƨ@{t�@y�7@w��@w�P@w\)@w
=@vv�@v@u�@u?}@u/@t��@tz�@st�@sC�@sC�@s"�@rM�@qx�@qX@q&�@p�`@pbN@p �@pb@o�;@o;d@nȴ@n��@n�+@m�h@m`B@m?}@l�j@lZ@k�
@k�F@k��@k�@kt�@k�F@k�
@l1@k��@kS�@j^5@i%@g��@f�@f�@f��@e�-@d��@d�/@d��@b��@bn�@b��@b�@c��@b=q@a��@aX@_�P@_
=@_
=@^ȴ@^ȴ@^ȴ@^�@^{@^@]�T@]�-@]��@]O�@\�/@\1@[S�@["�@[@[@Z�H@[o@[@[@[@Z�@Z��@Z�!@Z��@Z~�@Z^5@Z-@Y�@Y��@Y7L@Y�@X��@X�@Xr�@XQ�@Wl�@V5?@U�@U�@T��@TI�@S��@Sƨ@S��@S33@Rn�@R=q@R�@Q�#@Qhs@Q7L@P��@PĜ@P1'@O�@O�w@Ol�@O+@N��@Nȴ@N��@Nv�@NV@NE�@N5?@N@M@M�-@M�h@MV@L�@L�/@L��@L��@L��@L�j@L�j@L�@L�@L��@L��@Lz�@Lj@LZ@L1@K�@J-@I��@I��@I��@I�7@I7L@I%@H��@H��@H�`@H��@H�9@H��@H�u@Hr�@HbN@Hb@Gl�@Fȴ@F�R@F��@F�+@FV@F$�@F@E��@E�@E?}@D�@D�@D�D@Dj@D9X@C�
@C��@CS�@Co@B��@B^5@A�@A��@Ahs@A7L@@��@@Q�@?��@?;d@?+@?�@?
=@?
=@>�y@>�@>�R@>�+@>V@>V@>5?@>{@=�T@=�-@=�@=`B@=O�@=?}@<�@<I�@<9X@;ƨ@;S�@;"�@;o@:�@:�!@:^5@:-@9�^@9X@8�`@8�9@8r�@8 �@8b@7�P@6�R@5�@5p�@5`B@5/@4z�@3��@3S�@3S�@3C�@3C�@3"�@3"�@3"�@3"�@3o@3@3@2�@2�H@2�H@2��@2�!@2�\@2~�@2^5@2-@2-@2J@1�@1�#@1��@1��@1hs@1G�@0Q�@0b@/�w@/l�@/;d@.�+@.E�@.$�@-�-@-�-@-�h@-`B@-?}@-/@-�@-�@-V@,��@,��@,j@,9X@,�@+�m@+t�@+33@*�@*��@*�!@*n�@)��@)&�@)%@(�`@(��@(Ĝ@(Ĝ@(��@(��@(Ĝ@(��@(�u@(Q�@( �@'�@'�@';d@&��@&�+@&E�@&{@%�@%�@%�@%�T@%@%��@%/@$�j@$z�@$(�@#��@#t�@#C�@#"�@"�@"��@"��@"��@"~�@"M�@"J@!��@!�^@!hs@!�@ ��@ �@ A�@ Q�@ 1'@  �@��@|�@;d@
=@��@5?@��@��@�h@p�@`B@O�@/@/@V@�/@�D@ƨ@t�@dZ@S�@@�@��@��@��@�\@M�@=q@J@��@�7@&�@��@�9@�u@bN@A�@ �@b@b@b@b@b@  @�;@��@��@��@�w@�P@\)@�@�@v�@5?@E�@{@�-@`B@/@��@�@��@z�@j@9X@�@��@�m@�
@�F@��@�@S�@33@"�@o@@@�@�H@�!@�\@~�@J@hs@�`@�@�@�@r�@r�@r�@bN@Q�@  @�w@\)@ȴ@5?@`B@�@�j@��@Z@(�@�
@C�@33@o@@
�@
�@
�H@
�H@
��@
��@
��@
��@
n�@
M�@
-@
J@	�#@	�#@	�@	�#@	�#@	�#@	�#@	��@	X@	&�@��@��@Q�@  @��@�w@|�@K�@;d@��@�@��@V@{@�T@�T@�h@O�@/@�@��@�j@�j@(�@��@�
@��@t�@S�@@�!@�\@n�@n�@^5@=q@�@�^@�7@G�@�@ ��@ �`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���A��
A��/A��;AŃA��A�1A�C�A�33A�p�A�dZA��RA�v�A�&�A�G�A�/A��A��A�"�A��A���A���A��A���A��A���A���A�`BA���A�"�A���A�O�A�S�A�JA��A�5?A�hsA���A���A���A�x�A�hsA�K�A�/A��A��A��A�M�A���A���A��`A��!A�oA�$�A��^A�p�A��\A��A���A�A�A�(�A�ȴA��wA�ffA��!A��^A�bNA�XA�?}A��A��yA���A��wA�/A�  A�bA�7LA���A���A���A���A�/A�z�A��RA~A{��AzAx�9AvVAs�mApv�Am��Al�/Ak`BAi�
AioAh�9AhE�Af�9Aep�Ae?}Ad~�AcAcS�Ab�RAbQ�Aa��Aa"�A`��A_C�A]��A]oA\ȴA\�9A\��A\E�A[�hAZ��AY�AW��AT�HATr�ASdZAQ�AQK�AP�jAP�AO�hAOoAM\)AL�\AK��AI��AH�AH~�AG��AF��AEƨAEADbNAC�hAAA@��A@^5A?K�A>�jA>n�A<�/A;K�A9�A9%A8�jA8bA7�PA7A6~�A61A4�A3�7A2 �A1"�A/��A.I�A-�A+�FA*��A*�A)��A)��A)\)A(��A(v�A(  A'G�A'&�A&ȴA&��A&ffA%�
A$�HA$5?A#\)A"1'A!VA M�A��AS�A�Av�A?}A1'A�7A?}A�/A�\Az�AJA\)A$�A��A�AA�A��A�A�;A�uA?}A�A�DAhsA�!A^5AbA��A
�A	�A�A��A$�Ap�AoA  A�jAbA�A�uA��A�FA��AhsA�AA ��A 5?@��@��@�1@��@��h@�bN@��@�7L@�;d@�=q@�^@��;@��@�o@�$�@���@�D@��
@��@�^@�j@��@�@�ƨ@�~�@ݩ�@ۅ@��#@؋D@֏\@�7L@ӕ�@Ұ!@�V@���@·+@�I�@�33@�-@���@ɺ^@ə�@�Q�@�ƨ@�O�@��9@�1@�o@��@��u@�o@��y@��+@��@���@��@�G�@���@��@���@�r�@���@���@��@��/@��@��D@�9X@���@�t�@�"�@�v�@�{@�&�@�I�@�dZ@��!@�hs@���@���@���@��@�I�@�  @��m@�\)@��@�~�@�J@���@�?}@�j@�S�@�$�@�p�@�&�@�%@���@���@�K�@�V@��@��^@�O�@���@��D@�9X@�  @���@�"�@��@��-@�hs@�p�@�p�@��h@���@��-@���@��h@��-@��@�z�@�5?@�?}@��@��@���@���@��@�ƨ@���@�C�@�
=@���@�ȴ@���@�-@�{@�@��@���@��@��@��j@��@��D@�j@�b@�t�@��@�$�@��^@�hs@�V@���@��/@��/@��/@��/@���@��u@�1@�l�@��\@�=q@�{@�7L@�x�@�&�@��j@�z�@� �@��F@�l�@�o@��!@�n�@�M�@�J@��^@��h@�hs@�/@��@��/@���@�Z@�b@�  @���@���@�dZ@�@��H@���@�v�@�ff@�5?@��@��T@�@���@��@�p�@�X@�G�@�7L@���@���@���@��u@�z�@��@���@���@�Q�@;d@�@�@~�y@~$�@}�T@}�@|��@|(�@|I�@|�@|1@{ƨ@{t�@y�7@w��@w�P@w\)@w
=@vv�@v@u�@u?}@u/@t��@tz�@st�@sC�@sC�@s"�@rM�@qx�@qX@q&�@p�`@pbN@p �@pb@o�;@o;d@nȴ@n��@n�+@m�h@m`B@m?}@l�j@lZ@k�
@k�F@k��@k�@kt�@k�F@k�
@l1@k��@kS�@j^5@i%@g��@f�@f�@f��@e�-@d��@d�/@d��@b��@bn�@b��@b�@c��@b=q@a��@aX@_�P@_
=@_
=@^ȴ@^ȴ@^ȴ@^�@^{@^@]�T@]�-@]��@]O�@\�/@\1@[S�@["�@[@[@Z�H@[o@[@[@[@Z�@Z��@Z�!@Z��@Z~�@Z^5@Z-@Y�@Y��@Y7L@Y�@X��@X�@Xr�@XQ�@Wl�@V5?@U�@U�@T��@TI�@S��@Sƨ@S��@S33@Rn�@R=q@R�@Q�#@Qhs@Q7L@P��@PĜ@P1'@O�@O�w@Ol�@O+@N��@Nȴ@N��@Nv�@NV@NE�@N5?@N@M@M�-@M�h@MV@L�@L�/@L��@L��@L��@L�j@L�j@L�@L�@L��@L��@Lz�@Lj@LZ@L1@K�@J-@I��@I��@I��@I�7@I7L@I%@H��@H��@H�`@H��@H�9@H��@H�u@Hr�@HbN@Hb@Gl�@Fȴ@F�R@F��@F�+@FV@F$�@F@E��@E�@E?}@D�@D�@D�D@Dj@D9X@C�
@C��@CS�@Co@B��@B^5@A�@A��@Ahs@A7L@@��@@Q�@?��@?;d@?+@?�@?
=@?
=@>�y@>�@>�R@>�+@>V@>V@>5?@>{@=�T@=�-@=�@=`B@=O�@=?}@<�@<I�@<9X@;ƨ@;S�@;"�@;o@:�@:�!@:^5@:-@9�^@9X@8�`@8�9@8r�@8 �@8b@7�P@6�R@5�@5p�@5`B@5/@4z�@3��@3S�@3S�@3C�@3C�@3"�@3"�@3"�@3"�@3o@3@3@2�@2�H@2�H@2��@2�!@2�\@2~�@2^5@2-@2-@2J@1�@1�#@1��@1��@1hs@1G�@0Q�@0b@/�w@/l�@/;d@.�+@.E�@.$�@-�-@-�-@-�h@-`B@-?}@-/@-�@-�@-V@,��@,��@,j@,9X@,�@+�m@+t�@+33@*�@*��@*�!@*n�@)��@)&�@)%@(�`@(��@(Ĝ@(Ĝ@(��@(��@(Ĝ@(��@(�u@(Q�@( �@'�@'�@';d@&��@&�+@&E�@&{@%�@%�@%�@%�T@%@%��@%/@$�j@$z�@$(�@#��@#t�@#C�@#"�@"�@"��@"��@"��@"~�@"M�@"J@!��@!�^@!hs@!�@ ��@ �@ A�@ Q�@ 1'@  �@��@|�@;d@
=@��@5?@��@��@�h@p�@`B@O�@/@/@V@�/@�D@ƨ@t�@dZ@S�@@�@��@��@��@�\@M�@=q@J@��@�7@&�@��@�9@�u@bN@A�@ �@b@b@b@b@b@  @�;@��@��@��@�w@�P@\)@�@�@v�@5?@E�@{@�-@`B@/@��@�@��@z�@j@9X@�@��@�m@�
@�F@��@�@S�@33@"�@o@@@�@�H@�!@�\@~�@J@hs@�`@�@�@�@r�@r�@r�@bN@Q�@  @�w@\)@ȴ@5?@`B@�@�j@��@Z@(�@�
@C�@33@o@@
�@
�@
�H@
�H@
��@
��@
��@
��@
n�@
M�@
-@
J@	�#@	�#@	�@	�#@	�#@	�#@	�#@	��@	X@	&�@��@��@Q�@  @��@�w@|�@K�@;d@��@�@��@V@{@�T@�T@�h@O�@/@�@��@�j@�j@(�@��@�
@��@t�@S�@@�!@�\@n�@n�@^5@=q@�@�^@�7@G�@�@ ��@ �`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BuB{B{BuBuBoBbBhBq�BiyBP�B<jB/B$�B �B#�B"�B(�B5?B5?B33B6FB:^B5?B1'B5?B;dBL�BQ�BQ�BO�BN�BK�BH�BD�B>wB7LB?}B5?B'�B&�B6FB=qB=qB@�BB�BC�BH�BF�BB�B<jB49B'�B�BoB+B�B�`B�;B��B�B�7B|�Bp�BcTBI�B7LB33B&�B�BuBuBhBPB	7BB�B��B�^B��B��B{�BhsBK�B8RB��B�B�`B��B�}B�9B��B��B�JBz�BffBbNB\)BS�BP�BN�BK�BF�B>wB>wB9XB6FB33B1'B.B,B'�B#�B!�B�B�B�B�B�B�BoBhBuBPB
��B
��B
��B
�B
�B
�B
�sB
�`B
�TB
�B
�#B
�B
��B
��B
��B
ǮB
ÖB
�qB
�RB
�LB
�3B
�B
��B
��B
��B
��B
��B
��B
�{B
�VB
�=B
�1B
�%B
�B
� B
|�B
{�B
u�B
q�B
iyB
ffB
_;B
\)B
XB
S�B
O�B
M�B
L�B
J�B
J�B
G�B
F�B
E�B
D�B
B�B
B�B
A�B
@�B
?}B
=qB
:^B
6FB
5?B
0!B
-B
)�B
(�B
(�B
'�B
&�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
VB
PB
DB
+B
%B
B
B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�`B	�TB	�TB	�TB	�NB	�HB	�HB	�HB	�;B	�5B	�5B	�)B	�#B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
%B
%B
B
1B
1B
1B
DB
\B
oB
{B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
#�B
$�B
-B
33B
5?B
7LB
9XB
:^B
;dB
;dB
<jB
<jB
=qB
B�B
E�B
F�B
G�B
H�B
K�B
O�B
Q�B
R�B
T�B
XB
[#B
\)B
[#B
]/B
]/B
]/B
\)B
\)B
^5B
cTB
dZB
hsB
jB
k�B
m�B
n�B
s�B
v�B
v�B
w�B
x�B
{�B
~�B
�B
�B
�B
�B
�B
�+B
�7B
�\B
�hB
�uB
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
�B
�B
�3B
�FB
�^B
�^B
�^B
�dB
�qB
�wB
��B
ĜB
ƨB
ǮB
ɺB
��B
��B
��B
��B
��B
��B
�B
�B
�#B
�#B
�)B
�/B
�HB
�ZB
�`B
�sB
�yB
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BB1B\B{B�B�B�B�B�B!�B%�B(�B+B+B.B0!B0!B1'B2-B8RB<jB=qB>wB?}BB�BD�BF�BF�BF�BF�BH�BL�BM�BP�BR�BT�BVBVBW
BW
BXBYBYBYB[#B\)B\)B\)B_;BaHBcTBdZBffBjBk�Bk�Bl�Bn�Bo�Bq�Br�Bt�Bt�Bu�Bv�Bu�Bu�Bv�Bx�B{�B~�B�B�B�B�B�1B�7B�oB��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�-B�9B�9B�9B�9B�LB�^B�dB�jB�jB�qB�wB�wB�}B��BBBBBÖBÖBÖBĜBŢBŢBŢBƨBƨBǮBǮBǮBǮBȴBȴBȴBȴBɺBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�B�B�B�B�B�#B�#B�)B�5B�5B�5B�5B�5B�;B�;B�BB�BB�BB�HB�HB�NB�NB�NB�TB�TB�ZB�ZB�`B�`B�`B�fB�fB�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBB%B%B%B%B%B%B%B+B+B1B1B1B	7B	7B
=B
=B
=B
=BJBJBPBPBPBPBPBPBPBPBPBVBVBVB\B\BbBbBhBoBoBoBoBoBoBoBoBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B"�B"�B"�B"�B#�B"�B#�B#�B#�B#�B$�B$�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B(�B(�B(�B)�B)�B)�B)�B+B+B,B,B,B,B-B-B-B-B-B.B.B.B.B.B.B/B/B/B/B/B/B/B/B0!B/B0!B1'B2-B2-B2-B2-B2-B33B2-B33B2-B33B33B49B5?B5?B6FB7LB7LB7LB7LB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB;dB<jB;dB<jB<jB<jB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BD�BD�BD�BD�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BH�BH�BH�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 BuB{B{BuBuBoBbBhBq�BiyBP�B<jB/B$�B �B#�B"�B(�B5?B5?B33B6FB:^B5?B1'B5?B;dBL�BQ�BQ�BO�BN�BK�BH�BD�B>wB7LB?}B5?B'�B&�B6FB=qB=qB@�BB�BC�BH�BF�BB�B<jB49B'�B�BoB+B�B�`B�;B��B�B�7B|�Bp�BcTBI�B7LB33B&�B�BuBuBhBPB	7BB�B��B�^B��B��B{�BhsBK�B8RB��B�B�`B��B�}B�9B��B��B�JBz�BffBbNB\)BS�BP�BN�BK�BF�B>wB>wB9XB6FB33B1'B.B,B'�B#�B!�B�B�B�B�B�B�BoBhBuBPB
��B
��B
��B
�B
�B
�B
�sB
�`B
�TB
�B
�#B
�B
��B
��B
��B
ǮB
ÖB
�qB
�RB
�LB
�3B
�B
��B
��B
��B
��B
��B
��B
�{B
�VB
�=B
�1B
�%B
�B
� B
|�B
{�B
u�B
q�B
iyB
ffB
_;B
\)B
XB
S�B
O�B
M�B
L�B
J�B
J�B
G�B
F�B
E�B
D�B
B�B
B�B
A�B
@�B
?}B
=qB
:^B
6FB
5?B
0!B
-B
)�B
(�B
(�B
'�B
&�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
VB
PB
DB
+B
%B
B
B
  B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�`B	�TB	�TB	�TB	�NB	�HB	�HB	�HB	�;B	�5B	�5B	�)B	�#B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	ȴB	ɺB	ɺB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�HB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
%B
%B
B
1B
1B
1B
DB
\B
oB
{B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
#�B
$�B
-B
33B
5?B
7LB
9XB
:^B
;dB
;dB
<jB
<jB
=qB
B�B
E�B
F�B
G�B
H�B
K�B
O�B
Q�B
R�B
T�B
XB
[#B
\)B
[#B
]/B
]/B
]/B
\)B
\)B
^5B
cTB
dZB
hsB
jB
k�B
m�B
n�B
s�B
v�B
v�B
w�B
x�B
{�B
~�B
�B
�B
�B
�B
�B
�+B
�7B
�\B
�hB
�uB
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
�B
�B
�3B
�FB
�^B
�^B
�^B
�dB
�qB
�wB
��B
ĜB
ƨB
ǮB
ɺB
��B
��B
��B
��B
��B
��B
�B
�B
�#B
�#B
�)B
�/B
�HB
�ZB
�`B
�sB
�yB
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BB1B\B{B�B�B�B�B�B!�B%�B(�B+B+B.B0!B0!B1'B2-B8RB<jB=qB>wB?}BB�BD�BF�BF�BF�BF�BH�BL�BM�BP�BR�BT�BVBVBW
BW
BXBYBYBYB[#B\)B\)B\)B_;BaHBcTBdZBffBjBk�Bk�Bl�Bn�Bo�Bq�Br�Bt�Bt�Bu�Bv�Bu�Bu�Bv�Bx�B{�B~�B�B�B�B�B�1B�7B�oB��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�-B�9B�9B�9B�9B�LB�^B�dB�jB�jB�qB�wB�wB�}B��BBBBBÖBÖBÖBĜBŢBŢBŢBƨBƨBǮBǮBǮBǮBȴBȴBȴBȴBɺBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�B�B�B�B�B�#B�#B�)B�5B�5B�5B�5B�5B�;B�;B�BB�BB�BB�HB�HB�NB�NB�NB�TB�TB�ZB�ZB�`B�`B�`B�fB�fB�fB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  BBBBBBBBBBBB%B%B%B%B%B%B%B+B+B1B1B1B	7B	7B
=B
=B
=B
=BJBJBPBPBPBPBPBPBPBPBPBVBVBVB\B\BbBbBhBoBoBoBoBoBoBoBoBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B"�B"�B"�B"�B"�B#�B"�B#�B#�B#�B#�B$�B$�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B(�B(�B(�B)�B)�B)�B)�B+B+B,B,B,B,B-B-B-B-B-B.B.B.B.B.B.B/B/B/B/B/B/B/B/B0!B/B0!B1'B2-B2-B2-B2-B2-B33B2-B33B2-B33B33B49B5?B5?B6FB7LB7LB7LB7LB8RB8RB9XB9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB;dB;dB<jB;dB<jB<jB<jB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BD�BD�BD�BD�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BH�BH�BH�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210608090040                              AO  ARCAADJP                                                                    20210608090040    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210608090040  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210608090040  QCF$                G�O�G�O�G�O�8000            