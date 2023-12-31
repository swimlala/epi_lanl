CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-13T00:35:09Z creation;2017-10-13T00:35:13Z conversion to V3.1;2019-12-19T07:59:26Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171013003509  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_168                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�-6X 1   @�-6�}( @:о�(��d�����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @w�@��
@��
A�A=�A]�A}�A���A���A���A���A���A���A���A���Bz�Bz�Bz�Bz�B'z�B/z�B7z�B?{BGz�BOz�BWz�B_z�Bgz�Boz�Bwz�Bz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBýqBǽqB˽qBϽqBӽqB׽qB۽qB߽qB�qB�qB�qB�qB�qB��qB��qB��qC޸C޸C޸C޸C	޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C޸C!޸C#޸C%޸C'޸C)޸C+޸C-޸C/޸C1޸C3޸C5޸C7޸C9޸C;޸C=޸C?޸CA޸CC޸CE޸CG޸CI޸CK޸CM޸CO޸CQ޸CS޸CU޸CW޸CY޸C[޸C]޸C_޸Ca޸Cc޸Ce޸Cg޸Ci޸Ck޸Cm޸Co޸Cq޸Cs޸Cu޸Cw޸Cy޸C{޸C}޸C޸C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��)C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\C��\D w�D ��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��D	w�D	��D
w�D
��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D��Dw�D�Dw�D��Dw�D��D w�D ��D!w�D!��D"w�D"��D#w�D#��D$w�D$��D%w�D%��D&w�D&��D'w�D'��D(w�D(��D)w�D)��D*w�D*��D+w�D+��D,w�D,��D-w�D-��D.w�D.��D/w�D/��D0w�D0��D1w�D1��D2w�D2��D3w�D3��D4w�D4��D5w�D5��D6w�D6��D7w�D7��D8w�D8��D9w�D9��D:w�D:��D;w�D;��D<w�D<��D=w�D=��D>w�D>��D?w�D?��D@w�D@��DAw�DA��DBw�DB��DCw�DC��DDw�DD��DEw�DE��DFw�DF��DGw�DG��DHw�DH��DIw�DI��DJw�DJ��DK~DK��DLw�DL��DMw�DM��DNw�DN��DOw�DO��DPw�DP��DQw�DQ��DRw�DR��DSw�DS��DTw�DT��DUw�DU��DVw�DV��DWw�DW��DXw�DX��DYw�DY��DZw�DZ��D[w�D[��D\w�D\��D]w�D]��D^w�D^��D_w�D_��D`w�D`��Daw�Da��Dbw�Db��Dcw�Dc��Ddw�Dd��Dew�De��Dfw�Df��Dgw�Dg��Dhw�Dh��Diw�Di��Djw�Dj��Dkw�Dk��Dlw�Dl��Dmw�Dm��Dnw�Dn��Dow�Do��Dpw�Dp��Dqw�Dq��Drw�Dr��Dsw�Ds��Dtw�Dt��Duw�Du��Dvw�Dv��Dww�Dw��Dxw�Dx��Dyw�Dy��Dzw�Dz��D{w�D{��D|w�D|��D}w�D}��D~w�D~��Dw�D��D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D���D���D�;�D�{�D»�D���D�;�D�{�Dû�D���D�;�D�{�DĻ�D���D�;�D�{�DŻ�D���D�;�D�{�Dƻ�D���D�;�D�{�Dǻ�D���D�;�D�{�DȻ�D���D�;�D�{�Dɻ�D���D�;�D�{�Dʻ�D���D�;�D�{�D˻�D���D�;�D�{�D̿
D���D�;�D�{�Dͻ�D���D�;�D�{�Dλ�D���D�;�D�{�Dϻ�D���D�;�D�{�Dл�D���D�;�D�{�Dѻ�D���D�;�D�{�Dһ�D���D�;�D�{�Dӻ�D���D�;�D�{�DԻ�D���D�;�D�{�Dջ�D���D�;�D�{�Dֻ�D���D�;�D�{�D׻�D���D�;�D�{�Dػ�D���D�;�D�{�Dٻ�D���D�?
D�{�Dڻ�D���D�;�D�{�Dۻ�D���D�;�D�{�Dܻ�D���D�;�D�{�Dݻ�D���D�;�D�{�D޻�D���D�;�D�{�D߻�D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D��
D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�8�D�x�D��D���D�;�D�{�D��D���D�;�D�{�D��D���D�;�D�{�D���D���D�;�D�{�D���D���D�?
D�{�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A���A�A�A�  A���A���A�  A�
=A�VA�JA�
=A�JA�JA�JA�JA�
=A��A�t�A�z�A�VAĸRA�XA�ƨA�?}A���A�x�A��A�z�A�-A��PA��A�"�A�bA��hA�dZA��A��hA�`BA��A�bA���A���A�n�A�1'A�9XA���A�
=A��jA�1A���A�A�A���A��
A�E�A�C�A�l�A��A�1'A�ZA��HA�M�A��`A���A���A�G�A��A���A�5?A��mA�C�A��TA��mA��A�ĜA��yA��A�A�A��+A�C�A���A���A�v�A�%A�S�A�r�A��wA��;A��/A��A�=qA~E�A|��A| �A{\)Az�Ay�mAx�`AxE�Aw�
AvĜAv  Au/At9XAs33Ar�Aq��Aq;dApAn�Amx�Al�9Al�Ak�Aj�Ai��Ah�jAg��Ag�Ae�FAd$�Ab�\AaVA`z�A_ƨA^��A^�A^A�A]K�A\n�A[��AZ��AY��AY�PAY\)AX��AXJAW�-AW7LAV�+AU�TAU`BAT��AS&�ARĜAQO�AO��AN�9AN �AM�wAMG�AL��AKAJbAIXAI7LAI
=AH�RAH�jAG�
AGK�AF�AE�AC�hABZAAl�AA�A@v�A@^5A@�A?t�A>5?A=��A=p�A<�jA;ƨA:�+A9�FA9�A8�A7�A6jA6 �A5�A5��A5��A57LA4��A3G�A2�A1��A1O�A0��A0��A0^5A/C�A-�A-�FA-�7A,�\A,  A+VA*z�A)��A(jA(  A'�;A'oA&bA%G�A%%A"ĜA!�A �yA�TA�A�mA��AC�A�HA|�A�HA�!A�wA�
A?}AI�A�A��AA�A�hA^5A�A�jA�A�uA��A=qA�AĜA1A�TA&�A
�9A	�A	oAjA��At�Av�A&�A��AffA(�A��A&�A��A�
AoA I�A bA @�33@�r�@��\@���@�o@�`B@��`@��@�ȴ@�x�@��@�t�@�~�@�1'@�33@��@�J@�ƨ@�ȴ@�O�@�v�@��@߮@�+@���@�ff@ݩ�@݁@�I�@��@��#@���@�I�@ְ!@�p�@ԓu@ӥ�@�t�@�v�@��#@���@�b@��@�X@�Q�@˅@�
=@�=q@�x�@���@�33@Ɵ�@�@ēu@�
=@�@�O�@�bN@��R@���@�A�@�  @�ƨ@�l�@���@�%@�z�@��@���@���@��7@�%@�\)@���@�@��/@�Z@�ƨ@�33@���@�~�@�hs@�j@��
@�
=@�@�x�@�A�@���@�C�@��@��+@�^5@�V@�@���@��@�V@��`@��w@���@�x�@�&�@�bN@�dZ@��@���@��+@�ff@��^@�/@�Ĝ@�Z@�r�@��m@�33@���@��@��@�A�@�l�@�@��\@�=q@���@��@��@�I�@� �@��m@�+@���@�V@�J@��-@���@�J@���@�hs@��@���@�Q�@��w@��@�~�@�n�@�M�@�M�@�{@��@��T@��7@�/@���@���@�b@��@�;d@���@�=q@��@��-@�G�@�%@�r�@�  @���@��@�l�@�
=@��@��+@�-@��@�@��-@��h@�`B@�O�@�?}@�?}@��@��@�1'@�1@��@�P@\)@~v�@}`B@|�@|��@|�D@|�@{��@{dZ@{dZ@{o@z��@zJ@y�#@y�7@yX@y�@x��@xĜ@x�u@xr�@xA�@w�w@w|�@wl�@wK�@w
=@v�y@v�@v5?@t�@t1@s@r~�@r-@r�@q��@q�@q�#@q�^@q��@q7L@p�9@pr�@p �@o�P@o
=@n��@n{@mO�@mV@l��@lZ@l1@kt�@j�H@j~�@i�@i��@i�7@ihs@h��@h1'@g�;@g\)@f@e�@e�@ep�@eO�@e�@dz�@d(�@d1@c�m@c��@cC�@b�!@bn�@bJ@aX@`�u@`bN@`bN@`bN@`Q�@_K�@^ff@^E�@]�T@]�@]O�@\��@\9X@[��@[��@[o@Z�H@Z��@Z��@ZJ@Y�#@Yhs@X��@X��@Xr�@W��@W��@Wl�@W�@W�@V�y@Vv�@V$�@U��@U�-@Up�@U�@U�@UV@TI�@S��@S��@So@R��@R=q@Q�@Q%@Pr�@O�@O�P@O|�@O\)@OK�@O�@N�R@NV@M��@MV@L�@LI�@L1@K�
@K�
@K�
@K�m@K�
@K�F@K�F@K�F@K��@K33@J��@J^5@JJ@I�7@I&�@H�`@HĜ@H�@Hr�@HA�@H �@G�;@G��@GK�@G�@F��@F��@F{@E��@E`B@E/@E/@E�@EV@D��@DZ@Cƨ@CdZ@CS�@CC�@C33@C33@Co@Co@B�H@B��@B�!@B~�@BM�@B-@A��@A�#@A��@Ax�@AG�@A7L@A7L@A7L@A7L@A7L@A7L@A&�@A&�@Ahs@A�7@Ax�@Ahs@Ahs@A%@@��@?|�@=/@=�@=�T@=`B@=?}@<��@<�j@=V@=�@<�@<��@<��@<�/@<�j@<��@<��@<z�@<I�@<9X@<1@;��@;�m@;ƨ@;��@;��@;��@;�@;dZ@;dZ@;33@:�H@:�!@:~�@:M�@:-@9��@9%@8��@8�`@7��@7+@6�y@6V@4�@3ƨ@2��@2�\@2�\@2��@2�@2��@2�\@2~�@2J@1��@1�^@1��@1��@1��@1��@1X@1&�@1�@1�@1�@1%@0�`@0��@0Q�@/�w@/l�@/�@.��@.ȴ@.V@.@-�h@-�@-p�@-p�@-/@,��@,�@,�@,�D@,z�@,z�@,Z@,9X@,1@+ƨ@+��@+"�@*��@*~�@*M�@*-@*-@*�@)��@)�@)7L@(�9@(Ĝ@(�9@(�9@(�9@(�9@(�@(Q�@(b@'��@'|�@'�@&�@&V@%��@%�h@$��@$�@#��@#o@"��@!��@!X@ ��@ �`@ ��@ ��@ ��@ ��@ ��@ Ĝ@ ��@ Q�@   @��@�P@l�@��@@��@/@��@�/@�/@�@j@��@dZ@��@~�@M�@J@�#@G�@%@��@Ĝ@�9@��@�u@Q�@ �@�w@|�@;d@�@��@5?@�@�T@��@�@O�@/@�@��@�j@�j@�@��@��@�D@z�@Z@9X@1@ƨ@��@t�@�H@�\@�@��@J@�@�@�@�@J@��@��@��@&�@Ĝ@r�@bN@Q�@1'@1'@1'@1'@ �@�@�w@��@K�@��@ȴ@��@v�@ff@ff@V@5?@5?@@��@�-@O�@��@��@��@��@�D@z�@z�@9X@�
@t�@
�H@
^5@
-@
J@	��@	��@	hs@	7L@	7L@	&�@	&�@	�@	�@��@��@�9@��@��@�u@Q�@b@  @��@��@�P@\)@;d@
=@�y@ȴ@��@��@��@��@�+@�+@v�@ff@V@E�@5?@{@�@@/@�/@�@z�@z�@j@I�@I�@9X@�@1@�F@��@dZ@33@"�@@@�@��@�\@~�@^5@=q@=q@-@M�@M�@M�@=q@-@�@J@�#@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A���A�A�A�  A���A���A�  A�
=A�VA�JA�
=A�JA�JA�JA�JA�
=A��A�t�A�z�A�VAĸRA�XA�ƨA�?}A���A�x�A��A�z�A�-A��PA��A�"�A�bA��hA�dZA��A��hA�`BA��A�bA���A���A�n�A�1'A�9XA���A�
=A��jA�1A���A�A�A���A��
A�E�A�C�A�l�A��A�1'A�ZA��HA�M�A��`A���A���A�G�A��A���A�5?A��mA�C�A��TA��mA��A�ĜA��yA��A�A�A��+A�C�A���A���A�v�A�%A�S�A�r�A��wA��;A��/A��A�=qA~E�A|��A| �A{\)Az�Ay�mAx�`AxE�Aw�
AvĜAv  Au/At9XAs33Ar�Aq��Aq;dApAn�Amx�Al�9Al�Ak�Aj�Ai��Ah�jAg��Ag�Ae�FAd$�Ab�\AaVA`z�A_ƨA^��A^�A^A�A]K�A\n�A[��AZ��AY��AY�PAY\)AX��AXJAW�-AW7LAV�+AU�TAU`BAT��AS&�ARĜAQO�AO��AN�9AN �AM�wAMG�AL��AKAJbAIXAI7LAI
=AH�RAH�jAG�
AGK�AF�AE�AC�hABZAAl�AA�A@v�A@^5A@�A?t�A>5?A=��A=p�A<�jA;ƨA:�+A9�FA9�A8�A7�A6jA6 �A5�A5��A5��A57LA4��A3G�A2�A1��A1O�A0��A0��A0^5A/C�A-�A-�FA-�7A,�\A,  A+VA*z�A)��A(jA(  A'�;A'oA&bA%G�A%%A"ĜA!�A �yA�TA�A�mA��AC�A�HA|�A�HA�!A�wA�
A?}AI�A�A��AA�A�hA^5A�A�jA�A�uA��A=qA�AĜA1A�TA&�A
�9A	�A	oAjA��At�Av�A&�A��AffA(�A��A&�A��A�
AoA I�A bA @�33@�r�@��\@���@�o@�`B@��`@��@�ȴ@�x�@��@�t�@�~�@�1'@�33@��@�J@�ƨ@�ȴ@�O�@�v�@��@߮@�+@���@�ff@ݩ�@݁@�I�@��@��#@���@�I�@ְ!@�p�@ԓu@ӥ�@�t�@�v�@��#@���@�b@��@�X@�Q�@˅@�
=@�=q@�x�@���@�33@Ɵ�@�@ēu@�
=@�@�O�@�bN@��R@���@�A�@�  @�ƨ@�l�@���@�%@�z�@��@���@���@��7@�%@�\)@���@�@��/@�Z@�ƨ@�33@���@�~�@�hs@�j@��
@�
=@�@�x�@�A�@���@�C�@��@��+@�^5@�V@�@���@��@�V@��`@��w@���@�x�@�&�@�bN@�dZ@��@���@��+@�ff@��^@�/@�Ĝ@�Z@�r�@��m@�33@���@��@��@�A�@�l�@�@��\@�=q@���@��@��@�I�@� �@��m@�+@���@�V@�J@��-@���@�J@���@�hs@��@���@�Q�@��w@��@�~�@�n�@�M�@�M�@�{@��@��T@��7@�/@���@���@�b@��@�;d@���@�=q@��@��-@�G�@�%@�r�@�  @���@��@�l�@�
=@��@��+@�-@��@�@��-@��h@�`B@�O�@�?}@�?}@��@��@�1'@�1@��@�P@\)@~v�@}`B@|�@|��@|�D@|�@{��@{dZ@{dZ@{o@z��@zJ@y�#@y�7@yX@y�@x��@xĜ@x�u@xr�@xA�@w�w@w|�@wl�@wK�@w
=@v�y@v�@v5?@t�@t1@s@r~�@r-@r�@q��@q�@q�#@q�^@q��@q7L@p�9@pr�@p �@o�P@o
=@n��@n{@mO�@mV@l��@lZ@l1@kt�@j�H@j~�@i�@i��@i�7@ihs@h��@h1'@g�;@g\)@f@e�@e�@ep�@eO�@e�@dz�@d(�@d1@c�m@c��@cC�@b�!@bn�@bJ@aX@`�u@`bN@`bN@`bN@`Q�@_K�@^ff@^E�@]�T@]�@]O�@\��@\9X@[��@[��@[o@Z�H@Z��@Z��@ZJ@Y�#@Yhs@X��@X��@Xr�@W��@W��@Wl�@W�@W�@V�y@Vv�@V$�@U��@U�-@Up�@U�@U�@UV@TI�@S��@S��@So@R��@R=q@Q�@Q%@Pr�@O�@O�P@O|�@O\)@OK�@O�@N�R@NV@M��@MV@L�@LI�@L1@K�
@K�
@K�
@K�m@K�
@K�F@K�F@K�F@K��@K33@J��@J^5@JJ@I�7@I&�@H�`@HĜ@H�@Hr�@HA�@H �@G�;@G��@GK�@G�@F��@F��@F{@E��@E`B@E/@E/@E�@EV@D��@DZ@Cƨ@CdZ@CS�@CC�@C33@C33@Co@Co@B�H@B��@B�!@B~�@BM�@B-@A��@A�#@A��@Ax�@AG�@A7L@A7L@A7L@A7L@A7L@A7L@A&�@A&�@Ahs@A�7@Ax�@Ahs@Ahs@A%@@��@?|�@=/@=�@=�T@=`B@=?}@<��@<�j@=V@=�@<�@<��@<��@<�/@<�j@<��@<��@<z�@<I�@<9X@<1@;��@;�m@;ƨ@;��@;��@;��@;�@;dZ@;dZ@;33@:�H@:�!@:~�@:M�@:-@9��@9%@8��@8�`@7��@7+@6�y@6V@4�@3ƨ@2��@2�\@2�\@2��@2�@2��@2�\@2~�@2J@1��@1�^@1��@1��@1��@1��@1X@1&�@1�@1�@1�@1%@0�`@0��@0Q�@/�w@/l�@/�@.��@.ȴ@.V@.@-�h@-�@-p�@-p�@-/@,��@,�@,�@,�D@,z�@,z�@,Z@,9X@,1@+ƨ@+��@+"�@*��@*~�@*M�@*-@*-@*�@)��@)�@)7L@(�9@(Ĝ@(�9@(�9@(�9@(�9@(�@(Q�@(b@'��@'|�@'�@&�@&V@%��@%�h@$��@$�@#��@#o@"��@!��@!X@ ��@ �`@ ��@ ��@ ��@ ��@ ��@ Ĝ@ ��@ Q�@   @��@�P@l�@��@@��@/@��@�/@�/@�@j@��@dZ@��@~�@M�@J@�#@G�@%@��@Ĝ@�9@��@�u@Q�@ �@�w@|�@;d@�@��@5?@�@�T@��@�@O�@/@�@��@�j@�j@�@��@��@�D@z�@Z@9X@1@ƨ@��@t�@�H@�\@�@��@J@�@�@�@�@J@��@��@��@&�@Ĝ@r�@bN@Q�@1'@1'@1'@1'@ �@�@�w@��@K�@��@ȴ@��@v�@ff@ff@V@5?@5?@@��@�-@O�@��@��@��@��@�D@z�@z�@9X@�
@t�@
�H@
^5@
-@
J@	��@	��@	hs@	7L@	7L@	&�@	&�@	�@	�@��@��@�9@��@��@�u@Q�@b@  @��@��@�P@\)@;d@
=@�y@ȴ@��@��@��@��@�+@�+@v�@ff@V@E�@5?@{@�@@/@�/@�@z�@z�@j@I�@I�@9X@�@1@�F@��@dZ@33@"�@@@�@��@�\@~�@^5@=q@=q@-@M�@M�@M�@=q@-@�@J@�#@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BȴBȴBȴBȴBɺB��B��B��B��BɺBɺB��B��B��B��B��B��B��B��B��BĜB��B>wB��B�RB��B�DBz�BjB��B�VB~�B�DB� Bm�Bn�BhsBW
BffBffBjBffBYBW
BT�BD�B)�B��B��B��B�B�)B�qB�RB�9B�B�B�B��B�3B�LB�-B�B�B��B��B�B{�Bq�B_;BH�BA�BG�BA�B<jB0!B#�B)�B�BhB+B
��B
�B
�yB
�B
�sB
�;B
��B
ĜB
��B
�3B
��B
�uB
�+B
z�B
u�B
u�B
p�B
l�B
e`B
_;B
[#B
XB
M�B
J�B
D�B
>wB
8RB
49B
33B
.B
!�B
�B
�B
{B
uB
\B

=B	��B	��B	��B	�B	�mB	�)B	��B	��B	��B	ǮB	ÖB	B	B	�}B	�wB	�dB	�LB	�'B	�'B	�!B	�B	��B	��B	��B	��B	��B	��B	�bB	�+B	�B	y�B	jB	m�B	k�B	k�B	hsB	dZB	^5B	VB	VB	W
B	VB	R�B	Q�B	K�B	F�B	B�B	8RB	-B	'�B	'�B	'�B	$�B	%�B	"�B	�B	�B	�B	�B	oB	DB	B	B��B��B��B�B��B��B��B�B�B�yB�HB�/B�NB�;B�;B�/B�B��BǮB��B��BɺBǮBB�wB�^B�'B�?B�FB�B��B��B��B�JB�oB�bB�7B�7B�B�7B�%B�Bx�Bx�B{�Bq�BcTBm�BhsB_;BjBffB_;BXBVB`BBbNBaHBYBM�BN�BO�BL�BQ�BK�BK�BF�BB�BF�BC�BD�B=qB7LB>wB>wB>wB:^B:^B8RB49B49B5?B9XB:^B49B,B-B/B.B.B49B2-B0!B-B1'B-B-B&�B-B.B+B"�B'�B%�B�B$�B2-B1'B1'B1'B.B/B+B&�B+B)�B,B(�B)�B/B/B33B0!B0!B0!B0!B/B/B2-B5?B8RB6FB7LB8RB6FB<jB>wB<jB<jBB�B?}B@�B>wB@�BI�BL�BL�BK�BI�BH�BO�BO�BP�BR�BT�BS�BP�BW
BZB[#B`BB`BB`BB`BB`BB]/B`BBe`Be`Be`BgmBe`BiyBl�Bn�Bo�Br�Bx�Bv�Bp�Bl�Bn�Bq�Bu�Bw�Bx�B}�B{�B{�B�B�B�B�B�B�+B�1B�7B�\B�oB�{B��B�{B��B��B��B��B��B��B��B��B��B�B�B�'B�'B�?B�jB�qB�wBBǮBɺBɺB��B��B��B��B�
B�B�/B�;B�NB�ZB�mB�yB�yB�B�B�B�B��B��B��B��B��B��B��B	B	  B	B	1B	JB	\B	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	)�B	+B	,B	+B	.B	33B	5?B	5?B	5?B	8RB	:^B	<jB	<jB	>wB	?}B	B�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	I�B	I�B	L�B	N�B	P�B	P�B	Q�B	Q�B	P�B	P�B	W
B	YB	^5B	aHB	dZB	dZB	e`B	e`B	e`B	e`B	e`B	ffB	hsB	hsB	hsB	jB	l�B	l�B	n�B	r�B	s�B	s�B	t�B	t�B	t�B	u�B	w�B	{�B	}�B	}�B	|�B	~�B	�B	�B	�B	�7B	�JB	�DB	�DB	�DB	�=B	�PB	�VB	�\B	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�9B	�?B	�FB	�FB	�XB	�dB	�qB	�}B	�}B	�}B	��B	B	B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�BB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
1B

=B

=B

=B
DB
1B
B
JB
\B
\B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
%�B
&�B
&�B
&�B
&�B
&�B
)�B
(�B
&�B
(�B
)�B
'�B
#�B
$�B
%�B
)�B
,B
.B
/B
/B
.B
/B
0!B
1'B
2-B
2-B
33B
33B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
33B
49B
5?B
6FB
6FB
5?B
6FB
6FB
8RB
8RB
8RB
7LB
6FB
7LB
8RB
8RB
8RB
9XB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
9XB
:^B
=qB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
>wB
=qB
<jB
>wB
?}B
@�B
@�B
A�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
F�B
E�B
E�B
C�B
E�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
F�B
G�B
F�B
H�B
I�B
I�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
O�B
O�B
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
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
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
XB
W
B
W
B
XB
YB
ZB
[#B
[#B
[#B
[#B
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
]/B
]/B
]/B
^5B
]/B
]/B
^5B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
^5B
_;B
^5B
_;B
bNB
bNB
bNB
bNB
aHB
cTB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
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
ffB
hsB
iyB
iyB
jB
jB
jB
jB
jB
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
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BȴBȴBȴB��BɠB��B��B��B��B��BɺB��B��B��B��B��B��B��B�B��B�_B��BP.B��B��B��B�B��Bo�B�xB�B��B�dB��Bp�Bp�BkBY�BgBgmBl=BiDB[�BYeBV�BGzB/�B3B�B�RB�cB�VB��B��B�?B��B��B��B�qB�%B��B�B��B�iB��B��B��B~Bs�Ba�BK�BCBH�BCB=�B2|B&B*�B �BuB	RB
��B
�B
�B
�"B
�B
��B
��B
��B
�[B
�tB
�4B
��B
�B
}qB
w�B
v�B
q�B
m]B
f�B
`\B
\B
X�B
O(B
K�B
E�B
?�B
9�B
5tB
3�B
/B
#�B
!B

B
gB
FB
HB
DB
 �B	�B	��B	��B	�_B	�OB	��B	�pB	̳B	ȴB	ĜB	�-B	�-B	��B	��B	�6B	��B	�-B	��B	��B	�B	��B	��B	��B	��B	�qB	�YB	��B	�B	�B	{�B	lqB	n�B	l=B	l=B	iDB	eFB	_�B	W�B	V�B	WYB	VmB	SuB	R:B	MB	GzB	C�B	:xB	/OB	)�B	)B	(�B	%�B	&LB	#�B	�B	B	YB	+B	�B	�B	�B	-B��B��B�	B�UB�2B�2B�B�9B�oB�B�BޞB��B��B��BݲB��B�oB�7B�NB�bB�BȚB��B�}B��B��B��B��B�iB�fB��B��B�BB��B��B��B��B�tB��B��B�Bz�By�B|�BshBe�Bn�Bi�B`�BkBg8B`vBY�BW�B`�Bb�Ba�BZkBO�BPBP�BM�BR�BL�BL�BG�BC�BG�BD�BESB>�B8�B?.B?.B?B;0B;B9>B5�B5tB6FB9�B:�B5ZB-�B.}B0oB/�B/OB4�B2�B1'B./B1�B.B-�B(�B-�B.�B+�B$tB(�B'8B �B&fB2|B1�B1�B1�B.�B/�B+�B(
B+�B*�B,�B*B+B/�B/�B3�B0�B0�B1B0�B0;B0;B2�B5�B8�B6�B7�B8�B7�B<�B?.B=�B=qBC-B@�BAoB?�BA�BJXBM6BMBLJBJ�BI�BP}BP�BQ�BS�BU�BT�BRBW�BZ�B[�B`�B`�B`�B`�B`�B^5BaBe�BfBfLBh
Bf2BjBl�Bo BpBr�By$BwfBq�Bm�BoOBr�Bv�Bx�By�B~]B|�B|�B�oB�[B�gB��B��B��B��B��B�vB��B�2B�$B�gB�B�_B�]B�-B�TB�@B�`B��B�_B�WB�iB��B��B��B��B��B��B��BǮB�	B�#B�B�<B�\BԕB׍B�kB�IB�VB�hB�B�B��B��B��B�B�B�3B�+B�2B�XB�JB�<B�VB�wB	UB	 �B	gB	�B	dB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	;B	#:B	($B	*B	+6B	,WB	+�B	.�B	3�B	5tB	5�B	5tB	8�B	:xB	<�B	<�B	>�B	?�B	B�B	C�B	D�B	E�B	F�B	F�B	G�B	H�B	I�B	J	B	MB	OB	QB	QB	RB	RB	QNB	QhB	WYB	Y�B	^�B	abB	dtB	d�B	ezB	ezB	e�B	e�B	e�B	f�B	h�B	h�B	h�B	j�B	l�B	l�B	n�B	r�B	s�B	s�B	uB	t�B	uB	u�B	xB	|B	~(B	~B	}<B	cB	�GB	�gB	��B	��B	�dB	�^B	�xB	�xB	��B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�:B	�0B	�KB	�6B	�CB	�]B	�IB	�UB	�[B	�[B	�nB	�tB	�`B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	��B	�B	�B	�B	�@B	�@B	�TB	�,B	�SB	�KB	�KB	�QB	�1B	�KB	�KB	�QB	�kB	�xB	ބB	�vB	�B	�B	�ZB	�tB	�tB	�tB	�B	�`B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�	B	��B	�	B	��B	��B	�B	�B	�B	�B	�B	�"B	�B	�.B
 B
'B
-B
-B
-B
3B
3B
9B
?B
+B
fB

XB

rB

�B
�B
�B
�B
0B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
&B
'B
'B
'B
'8B
'B
*B
)B
'RB
)*B
*B
(XB
$tB
%`B
&LB
*0B
,"B
-�B
/5B
/OB
./B
/OB
0oB
1AB
2GB
2GB
33B
3MB
2aB
2GB
3hB
4TB
4TB
4TB
4TB
4nB
4nB
4nB
3hB
4TB
5tB
6zB
6zB
5tB
6zB
6zB
8lB
8RB
8lB
7�B
6zB
7fB
8RB
8�B
8lB
9XB
8lB
8�B
8�B
8�B
8lB
8�B
8lB
9rB
:�B
;B
;B
;B
;�B
;�B
9�B
:�B
=qB
=qB
=�B
=�B
=qB
<�B
<�B
<�B
<�B
<�B
<�B
=�B
<�B
=�B
>�B
=�B
<�B
>�B
?�B
@�B
@�B
A�B
C�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
D�B
D�B
F�B
E�B
E�B
C�B
E�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
GB
G�B
F�B
H�B
I�B
I�B
H�B
IB
I�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
J�B
K�B
MB
MB
MB
MB
M�B
N�B
N�B
OB
O�B
PB
O�B
QB
Q B
Q B
Q�B
RB
Q�B
RB
RB
R B
RB
R B
R B
S&B
RB
R B
SB
TFB
VB
W
B
W
B
W
B
W
B
W$B
W$B
W$B
XEB
XEB
W?B
W?B
X+B
Y1B
ZB
[=B
[=B
[=B
[=B
[=B
[WB
[WB
[WB
[=B
[WB
\]B
]dB
]dB
]IB
]/B
]/B
]dB
^OB
]dB
]dB
^jB
]dB
^OB
_VB
_VB
`BB
`\B
`\B
`BB
_VB
^�B
_�B
^jB
_pB
bNB
bhB
b�B
bhB
a|B
cnB
dtB
dtB
dtB
dZB
dtB
c�B
cnB
dZB
dtB
dZB
dtB
c�B
c�B
dtB
e�B
d�B
ezB
ezB
ezB
e�B
f�B
f�B
g�B
gmB
gmB
g�B
g�B
gmB
gmB
g�B
gmB
g�B
g�B
g�B
g�B
g�B
f�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
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
n�B
n�B
n�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111333111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.13(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710170033412017101700334120171017003341201806221232022018062212320220180622123202201804050427332018040504273320180405042733  JA  ARFMdecpA19c                                                                20171013093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171013003509  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171013003511  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171013003511  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171013003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171013003512  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171013003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171013003512  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171013003513  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171013003513                      G�O�G�O�G�O�                JA  ARUP                                                                        20171013005630                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171013153227  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20171016153341  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171016153341  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20171020000000  CF  PSAL_ADJUSTED_QCD�@ D�� G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192733  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033202  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                