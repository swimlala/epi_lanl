CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-12-05T10:01:06Z creation      
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  `    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȑ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �p   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211205100106  20211205100106  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               ~A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @٧�psV�1   @٧�`�f@<E�S����c�XbM�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         ~A   A   F   @�ff@�  A   A!��A@  A`  A�  A�  A���A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C3D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@��
@��
A�A=�A]�A}�A���A�A���A���A�A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?z�BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��B��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DKw�DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dz~Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�x�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D��
D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̻�D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�;�D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�x�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�
D���D���D�?
D�
D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A�ĜA�ȴA���A�ƨA�ƨA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�A�ȴA�ƨA���A���A�ĜA��jA��jA��!A���A���A�ZA�XA��A���A�XA�A�5?A�|�A�^5A�`BA���A�%A���A�t�A��A�{A��jA���A�Q�A�-A��PA��RA�  A��TA��A�A�9XA�+A��A�(�A���A���A�~�A���A�/A�
=A��wA��A�33A�~�A�9XA��mA��;A��A�VA�bA�  A��DA��mA�{A�A���A���A�A~�A{�-Az�+Axv�Aw��Aw�#Av��At�!As�TAr�Aq33Ao�wAn�!Am��Am�Al�uAj�RAhbNAcdZAbZAa�FAa|�Aa\)Aa7LA`�RA^�A\�AZ$�ASdZAPȴAP�AP�\AP�DAP~�APr�APbAOO�AL��AJ=qAH�+AH$�AG�AG��AG��AG�PAF��AE�wAE7LADĜABI�AA&�A@�`A@I�A?��A?O�A>�RA=�A<��A<��A<^5A< �A;��A;G�A:A�A9&�A9�A8�!A85?A7�A5��A4ZA3�-A3x�A2�uA1�A0ZA/��A/oA.E�A-A-7LA,�A,�9A,v�A,E�A+��A+33A*A�A)`BA(^5A'G�A%��A#�PA"�A"=qA!p�A�A��AȴA�#A�A�!A��AQ�A�mAS�A�jAbNA��A�`A�mA33A��A9XA�-A�HAr�A��A`BA�uA��A�A�yA5?A��AJA\)A"�A
1A��A �A&�AZA�#A�AȴA�jAA�A�FAK�A��AQ�A��AO�A ��@���@�ff@�@��@���@� �@�K�@�n�@���@���@�p�@��@��m@�dZ@�@�!@�v�@�-@�G�@�D@�|�@�R@�X@�\)@�E�@�h@�@�h@�Q�@��m@�C�@�~�@ܓu@�l�@��@ա�@�(�@ӝ�@�J@д9@��m@ύP@�E�@�Ĝ@�K�@��@���@�(�@��@ŉ7@���@�1'@Õ�@�
=@���@��w@���@�C�@�$�@�`B@��9@��@�"�@���@���@�&�@�%@��/@��j@�I�@��@�M�@��@�&�@���@��D@��@�(�@��y@���@���@�Ĝ@�1@���@���@��#@��h@�7L@��@�bN@� �@� �@��@��@���@��\@�v�@�J@�G�@��@�\)@���@�~�@�$�@���@�O�@��D@��@���@�%@��9@���@���@�r�@��@��;@��@��y@��@��@�O�@��/@�bN@�b@���@���@�l�@�o@��R@�n�@�J@���@��7@�7L@��`@�r�@�9X@��m@���@�l�@�@���@�5?@�{@��@���@��@��D@�bN@�9X@��m@��w@�t�@�ȴ@�ff@�-@�hs@���@���@�b@���@�C�@�
=@���@�V@��#@���@�`B@��@���@��/@��`@��9@��u@�1@��@�l�@�+@�
=@���@��+@�E�@�{@�p�@��@��9@�r�@� �@��F@�\)@��R@��@���@�`B@�%@��9@�r�@�(�@���@��F@���@���@�C�@�
=@���@���@�n�@�-@�{@�@�@���@���@��h@��@�p�@��@��@��j@���@�bN@�@~��@~��@~v�@~{@}�h@}?}@|�j@|I�@{�m@{C�@{@z�H@z�!@z�\@z^5@y��@y%@x��@x�u@x1'@w�;@w�P@w
=@v�y@vȴ@v�R@v�+@v$�@u�-@uO�@uV@tZ@t1@s��@r�H@r��@r�!@r~�@r�@qhs@p��@p�@p �@o�w@o;d@n��@n�@nV@m�@m@m�h@m�@mp�@mO�@mV@l�@k��@j�\@j^5@j=q@i�@i��@h��@h1'@g��@g�@g��@g|�@g+@f�@fȴ@f�R@f�+@fV@f{@e�@e��@e/@dI�@c��@cƨ@c�@cdZ@cC�@co@b�!@bn�@bn�@b^5@b=q@b�@a��@aG�@a�@a%@`��@`Q�@`1'@` �@_�;@_�w@_l�@_+@_
=@^��@^�y@^��@^V@^5?@]�T@]`B@\�/@\I�@[��@[��@[t�@Z�@Z�\@Z^5@Y�#@Y�7@Yhs@YX@Y7L@Y%@X��@X�9@X�u@Xr�@XQ�@W��@W��@W|�@V��@V�@Vȴ@V��@V��@Vv�@V@U�h@T��@T�@TZ@T1@Sƨ@SS�@R�!@R�@Q�^@PĜ@P�@P1'@O��@O�P@O\)@N��@Nȴ@NV@M@M?}@L�/@L�D@Lj@L�@K�
@K�F@K�@KC�@K@JM�@I��@I��@H��@HĜ@HbN@G�@G+@F�y@F��@FV@F@E@E�@EO�@D��@D�j@D��@Dj@D9X@D1@C�m@C�
@C��@C33@C"�@Co@B�\@B-@A�@A�^@A�7@A&�@@Ĝ@@r�@@ �@?�w@?K�@?�@?�@?
=@>�y@>�@>�@>ȴ@>�R@>��@>ff@>5?@>$�@=�@=��@=��@=`B@=/@<��@<�@<j@<I�@<1@;�
@;��@;t�@;S�@;33@;33@;"�@:�@:�H@:��@:��@:�!@:��@:^5@:-@:�@9x�@8�9@8r�@8A�@8 �@7�;@7��@7K�@7
=@6�y@6�@6��@6ff@6V@65?@6$�@6{@5�@5�-@5/@4��@4�@4��@4��@4�D@4z�@4I�@4�@3��@3�
@3�F@3C�@3o@2�@2�@2��@2��@2~�@2^5@2-@1�#@1��@1�@0��@0bN@0 �@0b@/�;@/�;@/�;@/��@/|�@/\)@/K�@/
=@.�R@.��@.v�@.{@-��@-��@-p�@-`B@-/@,��@,�@,9X@,(�@+ƨ@+��@+t�@+S�@+33@+"�@+@*�H@*��@*M�@*J@)x�@)G�@)%@(��@(Ĝ@(�9@(�u@(Q�@(  @'�@'��@'�P@'l�@'�@&ȴ@&5?@%��@%O�@%/@$��@$�/@$�D@$Z@$9X@$�@#�
@#dZ@"�@"��@"-@!�#@!�^@!��@!�7@!�7@!�7@!�7@!X@!7L@!�@ �`@ Q�@�@�;@�;@�P@l�@K�@
=@�@ȴ@ȴ@��@��@v�@ff@V@5?@{@@�T@��@`B@/@�@��@�D@Z@(�@�m@�m@�F@��@�@33@�!@�\@~�@n�@^5@^5@M�@�@x�@7L@%@��@��@�@A�@ �@ �@  @�@��@��@\)@�@
=@
=@��@�@��@v�@ff@E�@5?@{@�@�T@@@@��@�@O�@?}@�@��@�D@9X@�@�
@�F@��@S�@C�@o@@�@�@�@�H@��@n�@-@�@��@�^@��@x�@hs@X@G�@%@%@%@�`@Ĝ@r�@Q�@ �@  @�;@�w@��@l�@;d@
=@ȴ@��@v�@ff@{@�-@`B@/@�@I�@1@�m@ƨ@��@t�@S�@C�@"�@
�H@
~�@
n�@
^5@
=q@
�@	�@	�#@	�^@	��@	�7@	X@	&�@	%@Ĝ@��@�@�@Q�@b@�@��@�w@�@�P@|�@|�@|�@|�@l�@;d@�@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A�ĜA�ȴA���A�ƨA�ƨA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A�ȴA�ȴA�ƨA�A�ȴA�ƨA���A���A�ĜA��jA��jA��!A���A���A�ZA�XA��A���A�XA�A�5?A�|�A�^5A�`BA���A�%A���A�t�A��A�{A��jA���A�Q�A�-A��PA��RA�  A��TA��A�A�9XA�+A��A�(�A���A���A�~�A���A�/A�
=A��wA��A�33A�~�A�9XA��mA��;A��A�VA�bA�  A��DA��mA�{A�A���A���A�A~�A{�-Az�+Axv�Aw��Aw�#Av��At�!As�TAr�Aq33Ao�wAn�!Am��Am�Al�uAj�RAhbNAcdZAbZAa�FAa|�Aa\)Aa7LA`�RA^�A\�AZ$�ASdZAPȴAP�AP�\AP�DAP~�APr�APbAOO�AL��AJ=qAH�+AH$�AG�AG��AG��AG�PAF��AE�wAE7LADĜABI�AA&�A@�`A@I�A?��A?O�A>�RA=�A<��A<��A<^5A< �A;��A;G�A:A�A9&�A9�A8�!A85?A7�A5��A4ZA3�-A3x�A2�uA1�A0ZA/��A/oA.E�A-A-7LA,�A,�9A,v�A,E�A+��A+33A*A�A)`BA(^5A'G�A%��A#�PA"�A"=qA!p�A�A��AȴA�#A�A�!A��AQ�A�mAS�A�jAbNA��A�`A�mA33A��A9XA�-A�HAr�A��A`BA�uA��A�A�yA5?A��AJA\)A"�A
1A��A �A&�AZA�#A�AȴA�jAA�A�FAK�A��AQ�A��AO�A ��@���@�ff@�@��@���@� �@�K�@�n�@���@���@�p�@��@��m@�dZ@�@�!@�v�@�-@�G�@�D@�|�@�R@�X@�\)@�E�@�h@�@�h@�Q�@��m@�C�@�~�@ܓu@�l�@��@ա�@�(�@ӝ�@�J@д9@��m@ύP@�E�@�Ĝ@�K�@��@���@�(�@��@ŉ7@���@�1'@Õ�@�
=@���@��w@���@�C�@�$�@�`B@��9@��@�"�@���@���@�&�@�%@��/@��j@�I�@��@�M�@��@�&�@���@��D@��@�(�@��y@���@���@�Ĝ@�1@���@���@��#@��h@�7L@��@�bN@� �@� �@��@��@���@��\@�v�@�J@�G�@��@�\)@���@�~�@�$�@���@�O�@��D@��@���@�%@��9@���@���@�r�@��@��;@��@��y@��@��@�O�@��/@�bN@�b@���@���@�l�@�o@��R@�n�@�J@���@��7@�7L@��`@�r�@�9X@��m@���@�l�@�@���@�5?@�{@��@���@��@��D@�bN@�9X@��m@��w@�t�@�ȴ@�ff@�-@�hs@���@���@�b@���@�C�@�
=@���@�V@��#@���@�`B@��@���@��/@��`@��9@��u@�1@��@�l�@�+@�
=@���@��+@�E�@�{@�p�@��@��9@�r�@� �@��F@�\)@��R@��@���@�`B@�%@��9@�r�@�(�@���@��F@���@���@�C�@�
=@���@���@�n�@�-@�{@�@�@���@���@��h@��@�p�@��@��@��j@���@�bN@�@~��@~��@~v�@~{@}�h@}?}@|�j@|I�@{�m@{C�@{@z�H@z�!@z�\@z^5@y��@y%@x��@x�u@x1'@w�;@w�P@w
=@v�y@vȴ@v�R@v�+@v$�@u�-@uO�@uV@tZ@t1@s��@r�H@r��@r�!@r~�@r�@qhs@p��@p�@p �@o�w@o;d@n��@n�@nV@m�@m@m�h@m�@mp�@mO�@mV@l�@k��@j�\@j^5@j=q@i�@i��@h��@h1'@g��@g�@g��@g|�@g+@f�@fȴ@f�R@f�+@fV@f{@e�@e��@e/@dI�@c��@cƨ@c�@cdZ@cC�@co@b�!@bn�@bn�@b^5@b=q@b�@a��@aG�@a�@a%@`��@`Q�@`1'@` �@_�;@_�w@_l�@_+@_
=@^��@^�y@^��@^V@^5?@]�T@]`B@\�/@\I�@[��@[��@[t�@Z�@Z�\@Z^5@Y�#@Y�7@Yhs@YX@Y7L@Y%@X��@X�9@X�u@Xr�@XQ�@W��@W��@W|�@V��@V�@Vȴ@V��@V��@Vv�@V@U�h@T��@T�@TZ@T1@Sƨ@SS�@R�!@R�@Q�^@PĜ@P�@P1'@O��@O�P@O\)@N��@Nȴ@NV@M@M?}@L�/@L�D@Lj@L�@K�
@K�F@K�@KC�@K@JM�@I��@I��@H��@HĜ@HbN@G�@G+@F�y@F��@FV@F@E@E�@EO�@D��@D�j@D��@Dj@D9X@D1@C�m@C�
@C��@C33@C"�@Co@B�\@B-@A�@A�^@A�7@A&�@@Ĝ@@r�@@ �@?�w@?K�@?�@?�@?
=@>�y@>�@>�@>ȴ@>�R@>��@>ff@>5?@>$�@=�@=��@=��@=`B@=/@<��@<�@<j@<I�@<1@;�
@;��@;t�@;S�@;33@;33@;"�@:�@:�H@:��@:��@:�!@:��@:^5@:-@:�@9x�@8�9@8r�@8A�@8 �@7�;@7��@7K�@7
=@6�y@6�@6��@6ff@6V@65?@6$�@6{@5�@5�-@5/@4��@4�@4��@4��@4�D@4z�@4I�@4�@3��@3�
@3�F@3C�@3o@2�@2�@2��@2��@2~�@2^5@2-@1�#@1��@1�@0��@0bN@0 �@0b@/�;@/�;@/�;@/��@/|�@/\)@/K�@/
=@.�R@.��@.v�@.{@-��@-��@-p�@-`B@-/@,��@,�@,9X@,(�@+ƨ@+��@+t�@+S�@+33@+"�@+@*�H@*��@*M�@*J@)x�@)G�@)%@(��@(Ĝ@(�9@(�u@(Q�@(  @'�@'��@'�P@'l�@'�@&ȴ@&5?@%��@%O�@%/@$��@$�/@$�D@$Z@$9X@$�@#�
@#dZ@"�@"��@"-@!�#@!�^@!��@!�7@!�7@!�7@!�7@!X@!7L@!�@ �`@ Q�@�@�;@�;@�P@l�@K�@
=@�@ȴ@ȴ@��@��@v�@ff@V@5?@{@@�T@��@`B@/@�@��@�D@Z@(�@�m@�m@�F@��@�@33@�!@�\@~�@n�@^5@^5@M�@�@x�@7L@%@��@��@�@A�@ �@ �@  @�@��@��@\)@�@
=@
=@��@�@��@v�@ff@E�@5?@{@�@�T@@@@��@�@O�@?}@�@��@�D@9X@�@�
@�F@��@S�@C�@o@@�@�@�@�H@��@n�@-@�@��@�^@��@x�@hs@X@G�@%@%@%@�`@Ĝ@r�@Q�@ �@  @�;@�w@��@l�@;d@
=@ȴ@��@v�@ff@{@�-@`B@/@�@I�@1@�m@ƨ@��@t�@S�@C�@"�@
�H@
~�@
n�@
^5@
=q@
�@	�@	�#@	�^@	��@	�7@	X@	&�@	%@Ĝ@��@�@�@Q�@b@�@��@�w@�@�P@|�@|�@|�@|�@l�@;d@�@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�yB�yB�sB�sB�sB�sB�sB�sB�mB�mB�mB�fB�ZB�TB�5B��B��Bw�BK�B%�BPB+B�B�fB�NB�/B�B��B��BȴBB�dB�FB�3B�-B��B��B�PB�B{�Bs�B\)B@�B,B�BhB��B��B�XB�3B�B��B��B�DB�B� Bn�BQ�B7LB33B0!B)�B�B\B��B�/B��BĜB�RB��B��B�oB�PB�=B�+By�Br�Bk�BbNBYBQ�BK�BE�B@�B7LB-BbB
=B%BBBB��B��B�`B�BǮB�'B�'B�!B�!B�!B�B�B��B��B��B�uB�hB�hB�bB�\B�VB�PB�DB�+B�+B� B|�B{�By�Bw�Bu�Bt�Bq�Bo�Bn�Bm�Bm�Bk�BiyBhsBe`BdZBbNB`BB^5BW
BT�BQ�BP�BS�BO�BJ�BG�BC�B@�B>wB=qB>wB>wB=qB<jB:^B8RB5?B1'B-B&�B �B�B�B�B�BuBbBVBJB
=B	7B1BBBBBB  B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�mB
�fB
�ZB
�TB
�NB
�;B
�/B
�)B
�B
�B
�B
�B
�
B
�
B
�B
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
ɺB
ȴB
ǮB
ǮB
ƨB
ŢB
ĜB
ĜB
ĜB
ÖB
ÖB
ÖB
B
B
B
��B
��B
��B
��B
�}B
�wB
�}B
�}B
�}B
�wB
�wB
�qB
�wB
�jB
�qB
��B
��B
��B
��B
B
B
��B
ÖB
ĜB
ŢB
ƨB
ǮB
ǮB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�#B
�#B
�/B
�;B
�BB
�BB
�BB
�BB
�HB
�NB
�fB
�mB
�sB
�B
�yB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  BBBBBB1B	7B	7B
=BDBVBuB�B�B�B�B�B�B"�B$�B,B49B6FB7LB7LB8RB9XB:^B;dB?}BC�BF�BG�BI�BL�BN�BO�BQ�BR�BVBW
BYB[#B\)B^5B_;BaHBdZBffBgmBiyBiyBl�Bn�Bp�Bq�Bq�Bs�Bw�Bz�B{�B|�B~�B� B�B�%B�7B�=B�bB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�-B�FB�LB�XB�dB�jB�qB�}BBĜBɺB��B��B��B��B��B�
B�)B�NB�ZB�fB�yB�B�B�B�B��B��B��B��B��B��B  BBB%B+B
=BDBDBJBJBJBPBVB\BhBuB�B�B�B�B�B"�B#�B%�B&�B'�B,B-B/B0!B1'B2-B49B:^B<jB<jB=qB>wB@�BB�BC�BD�BD�BD�BE�BG�BI�BJ�BM�BN�BO�BS�BS�BT�BT�BW
BYBZB[#B\)B]/B^5B_;BaHBcTBdZBe`Be`BffBffBffBgmBhsBl�Bo�Bp�Bq�Br�Bs�Bu�Bx�By�Bz�Bz�B{�B|�B}�B}�B}�B~�B� B�B�B�B�B�1B�7B�=B�=B�DB�DB�JB�VB�\B�\B�\B�\B�bB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�3B�9B�?B�FB�FB�LB�LB�LB�RB�XB�dB�jB�qB�wB�}B�}B��BBÖBĜBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�)B�)B�/B�/B�5B�;B�BB�BB�HB�NB�NB�NB�TB�TB�ZB�ZB�`B�`B�fB�fB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBBBB%B%B%B%B+B+B+B+B1B	7B	7B	7B	7B
=B
=B
=B
=BDBDBJBPBPBVBVB\B\B\B\B\BbBbBbBbBhBhBoBoBuBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B!�B"�B"�B"�B#�B#�B$�B$�B%�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B'�B(�B)�B)�B)�B)�B+B+B+B,B,B,B,B,B,B-B-B-B-B-B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B1'B1'B1'B2-B33B33B33B33B33B33B33B49B5?B5?B5?B5?B6FB6FB7LB7LB7LB7LB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B?}B?}B?}B@�B@�B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BD�BD�BD�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BP�BP�BP�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�yB�yB�sB�sB�sB�sB�sB�sB�mB�mB�mB�fB�ZB�TB�5B��B��Bw�BK�B%�BPB+B�B�fB�NB�/B�B��B��BȴBB�dB�FB�3B�-B��B��B�PB�B{�Bs�B\)B@�B,B�BhB��B��B�XB�3B�B��B��B�DB�B� Bn�BQ�B7LB33B0!B)�B�B\B��B�/B��BĜB�RB��B��B�oB�PB�=B�+By�Br�Bk�BbNBYBQ�BK�BE�B@�B7LB-BbB
=B%BBBB��B��B�`B�BǮB�'B�'B�!B�!B�!B�B�B��B��B��B�uB�hB�hB�bB�\B�VB�PB�DB�+B�+B� B|�B{�By�Bw�Bu�Bt�Bq�Bo�Bn�Bm�Bm�Bk�BiyBhsBe`BdZBbNB`BB^5BW
BT�BQ�BP�BS�BO�BJ�BG�BC�B@�B>wB=qB>wB>wB=qB<jB:^B8RB5?B1'B-B&�B �B�B�B�B�BuBbBVBJB
=B	7B1BBBBBB  B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�mB
�fB
�ZB
�TB
�NB
�;B
�/B
�)B
�B
�B
�B
�B
�
B
�
B
�B
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
ɺB
ȴB
ǮB
ǮB
ƨB
ŢB
ĜB
ĜB
ĜB
ÖB
ÖB
ÖB
B
B
B
��B
��B
��B
��B
�}B
�wB
�}B
�}B
�}B
�wB
�wB
�qB
�wB
�jB
�qB
��B
��B
��B
��B
B
B
��B
ÖB
ĜB
ŢB
ƨB
ǮB
ǮB
ȴB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�#B
�#B
�/B
�;B
�BB
�BB
�BB
�BB
�HB
�NB
�fB
�mB
�sB
�B
�yB
�yB
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B  BBBBBB1B	7B	7B
=BDBVBuB�B�B�B�B�B�B"�B$�B,B49B6FB7LB7LB8RB9XB:^B;dB?}BC�BF�BG�BI�BL�BN�BO�BQ�BR�BVBW
BYB[#B\)B^5B_;BaHBdZBffBgmBiyBiyBl�Bn�Bp�Bq�Bq�Bs�Bw�Bz�B{�B|�B~�B� B�B�%B�7B�=B�bB�uB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�-B�FB�LB�XB�dB�jB�qB�}BBĜBɺB��B��B��B��B��B�
B�)B�NB�ZB�fB�yB�B�B�B�B��B��B��B��B��B��B  BBB%B+B
=BDBDBJBJBJBPBVB\BhBuB�B�B�B�B�B"�B#�B%�B&�B'�B,B-B/B0!B1'B2-B49B:^B<jB<jB=qB>wB@�BB�BC�BD�BD�BD�BE�BG�BI�BJ�BM�BN�BO�BS�BS�BT�BT�BW
BYBZB[#B\)B]/B^5B_;BaHBcTBdZBe`Be`BffBffBffBgmBhsBl�Bo�Bp�Bq�Br�Bs�Bu�Bx�By�Bz�Bz�B{�B|�B}�B}�B}�B~�B� B�B�B�B�B�1B�7B�=B�=B�DB�DB�JB�VB�\B�\B�\B�\B�bB�oB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�'B�-B�3B�9B�?B�FB�FB�LB�LB�LB�RB�XB�dB�jB�qB�wB�}B�}B��BBÖBĜBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�)B�)B�/B�/B�5B�;B�BB�BB�HB�NB�NB�NB�TB�TB�ZB�ZB�`B�`B�fB�fB�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBBBB%B%B%B%B+B+B+B+B1B	7B	7B	7B	7B
=B
=B
=B
=BDBDBJBPBPBVBVB\B\B\B\B\BbBbBbBbBhBhBoBoBuBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B!�B!�B"�B"�B"�B#�B#�B$�B$�B%�B&�B&�B&�B&�B'�B'�B'�B'�B'�B'�B'�B(�B)�B)�B)�B)�B+B+B+B,B,B,B,B,B,B-B-B-B-B-B.B.B.B.B/B/B/B0!B0!B0!B1'B1'B1'B1'B1'B2-B33B33B33B33B33B33B33B49B5?B5?B5?B5?B6FB6FB7LB7LB7LB7LB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB;dB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B?}B?}B?}B@�B@�B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BD�BD�BD�BE�BE�BE�BF�BF�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BP�BP�BP�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.13 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211205100106                              AO  ARCAADJP                                                                    20211205100106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211205100106  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211205100106  QCF$                G�O�G�O�G�O�8000            