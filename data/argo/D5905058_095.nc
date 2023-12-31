CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-10T12:35:15Z creation;2018-10-10T12:35:18Z conversion to V3.1;2019-12-23T06:13:40Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20181010123515  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA  I2_0675_095                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؇٤�O�1   @؇�[��@6�<64�cZ���)1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�i�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HA�
A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJBR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
C�
C�
C�
C�
C
�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C �
C"�
C$�
C&�
C(�
C*�
C,�
C.�
C0�
C2�
C4�
C6�
C8�
C:�
C<�
C>�
C@�
CB�
CD�
CF�
CH�
CJ�
CL�
CN�
CP�
CR�
CT�
CV�
CX�
CZ�
C\�
C^�
C`�
Cb�
Cd�
Cf�
Ch�
Cj�
Cl�
Cn�
Cp�
Cr�
Ct�
Cv�
Cx�
Cz�
C|�
C~�
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO�\DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{%�D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D�ϮD��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�O�D���D���D��D�R�D���D���D��D�R�D���D���D�D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�VD���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�VD���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�VDĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�Dҏ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�R�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�YHD�|{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$�A�(�A�(�A�&�A�+A�-A�-A�-A�+A��A���Aк^AЇ+AЁA�x�A�r�A�p�A�l�A�dZA�`BA�Q�A�5?A���A˗�A�;dAŸRA���A�oA�33A��jA���A���A��#A���A�7LA�ȴA��!A�%A��9A�jA��A�t�A�t�A���A�
=A��A�bNA�JA���A��A��A�E�A���A�9XA��hA���A��uA�A�t�A�  A��^A��TA�ĜA���A�hsA���A�`BA���A�bA��jA��
A��`A�$�A��TA��FA�oA���A�7LA�ZA�dZA�A��9A���A��A�ffA�"�A�9XA��A��A��#A���A��DA��hA�5?A�r�A�;dA��\A���A���A�E�A���A�A�hsA�^5A�A�5?A�PA}�Az�Ax��Ax^5Av��Au+ArJAoAi�hAg%AeK�AdJAc�AcVAb$�A`�RA\  AY;dAX  AVVAT�AR�AP�AM�wAK�PAJ�AJ��AI�7AGG�AEx�AC��ACVAA�A@��A?/A>�RA=�TA;+A:1A8E�A7dZA6ȴA6r�A5l�A5
=A5��A5ƨA5�7A4�/A4M�A3�A2�A1�A0��A/�PA. �A-+A,ZA,1A+t�A*v�A)&�A'�TA&�!A%�^A$�DA$$�A#�hA"�uA�AƨAz�A�
AK�A��A��A�HAr�A9XAJA��AoA�AJA�FA\)AoAjA�A?}A��AI�A�A�Az�A��Ar�A�PA
��A
(�A	l�A�\A��AE�AƨAC�A(�A�hA9XA �@���@�b@�ff@��j@��@���@���@�@�u@�\@�h@��@웦@�  @�K�@�$�@�9@�b@��@�^@�V@�o@�h@�%@���@�5?@�?}@�Z@۝�@���@�7L@�;d@�ff@��@���@��m@�
=@Ұ!@�^5@�v�@�{@�X@�j@�l�@�V@��@�~�@��@�x�@�z�@�bN@�bN@�(�@�
=@�@���@�1@�33@+@��@��h@�V@�  @�ȴ@�M�@�G�@�z�@�K�@���@��`@�$�@���@�x�@��/@��9@�bN@��
@�@��@�p�@�X@�Ĝ@��
@��@�o@��R@���@�@�p�@��`@�A�@��w@�|�@�@���@��\@���@�G�@�Ĝ@��D@�A�@�|�@��!@�~�@�V@��@��@��@��@�p�@�hs@�X@�O�@�p�@�`B@�/@���@�Ĝ@��9@� �@��@��w@���@�C�@�
=@���@�E�@���@�@�O�@�X@�O�@���@��D@�9X@��F@�;d@��!@�J@��h@�G�@�/@�V@��@��@�Q�@��F@�"�@���@�M�@�=q@�5?@�`B@�X@��@��/@�z�@�Q�@�Z@�A�@�(�@��@��w@��@�"�@�o@��y@��!@�$�@��T@��@�@���@��#@��T@�@�O�@��@�bN@�b@��w@���@��@��@�o@�;d@��@���@�ff@�ff@��^@��7@�I�@�K�@�ȴ@���@���@�ȴ@�|�@��P@��F@��
@���@�@��!@��h@�V@���@���@���@���@���@���@���@���@�I�@���@��@�
=@��y@��H@�ȴ@���@���@��!@��+@�ff@��@��@�@��7@�X@�/@���@��j@�Q�@�(�@�  @��m@���@��w@���@��@�K�@�"�@�@�ȴ@�~�@�E�@�-@�{@��@���@�O�@��/@��@��@�j@�Z@�I�@�1'@�  @��@��
@��@��P@�l�@�S�@�;d@�+@��@���@���@�ff@�@���@�x�@�p�@�p�@�hs@�X@�/@��@���@���@�Ĝ@���@�Z@�  @K�@~�@~v�@~{@}�h@}O�@|j@{�m@{C�@{"�@{"�@z�H@z�\@z�@y��@y��@yx�@x�`@x�9@w��@w\)@w;d@v�@vV@v@u�T@u�-@u�@t��@t��@t(�@s�m@sS�@r�\@r�@q��@q��@q�7@qx�@qG�@p��@pQ�@o�P@o�@n�y@n�+@n{@n@m�@m��@m�@mO�@l��@l��@l(�@k�
@k��@k��@k�@k"�@j�!@j�\@jM�@j-@jJ@i�#@i��@i&�@i�@i�@h�u@g�;@g\)@g+@f�@f��@f��@fV@e�@eO�@d��@dZ@d(�@c��@c��@cS�@b��@b~�@b^5@b�@bJ@a��@ahs@`��@`r�@`bN@`Q�@` �@_�w@_��@_+@^��@^ȴ@^5?@\�@\(�@[�m@[��@[C�@Z�!@Z^5@Z�@Y��@Yx�@X��@X�@W��@W\)@W\)@WK�@W+@V��@VE�@U@U?}@T�@T�D@Tj@Tj@T9X@T�@St�@R�H@R^5@RM�@R�@Q�#@Q��@Q�^@Q��@QG�@P��@PQ�@Pb@O��@O�P@Nȴ@NE�@N$�@N@M��@M�-@M�h@M/@Lj@LZ@L9X@L�@K�F@K�@Kt�@Kt�@KdZ@KdZ@KC�@K"�@J��@J~�@I�^@I&�@H��@H1'@Hb@G�@G�;@G��@G|�@G+@F��@Fȴ@F��@FE�@FE�@F5?@F{@E��@EO�@D�@Dz�@Dj@DZ@D1@C��@Co@Bn�@B-@AX@A%@@�`@@��@@Q�@@A�@@1'@@  @?�@>v�@=�T@=�-@=�@<�/@<�@<z�@<(�@;��@;��@;S�@:��@:J@9��@9��@9x�@9G�@97L@9&�@8��@8�@7�@7��@7��@7�w@7|�@6��@6��@6�+@6�+@6E�@5��@5�h@5p�@4��@3��@3ƨ@3��@3��@333@2�@2�H@2�!@2M�@2J@1�@1�^@1hs@1hs@1X@1G�@1�@0��@0�@0Q�@0 �@/�@/l�@.��@.ff@.E�@.{@-�@-��@-��@-�T@-��@-��@-��@-��@-�@-?}@,�/@,j@,I�@,1@+ƨ@+�F@+��@+33@+@*��@*~�@)�@)��@)��@)x�@)G�@(��@(��@(bN@(A�@(A�@(1'@(  @'��@'K�@'�@&��@&�@&ȴ@&ȴ@&ȴ@&ȴ@&�R@&��@&�+@&V@&5?@&$�@&@%�-@%�@%`B@%/@$�@$�@$z�@$I�@$�@#��@#�F@#C�@#33@#"�@#"�@#o@#@"��@"�\@"n�@"M�@"J@!X@ Ĝ@ �9@ 1'@�@;d@�y@��@��@�y@�y@�y@ȴ@�+@ff@5?@{@{@5?@ff@ff@V@�@O�@V@�@��@�j@�D@z�@j@Z@�@�F@t�@o@��@�\@M�@J@�^@hs@&�@��@�u@r�@bN@b@�w@�w@�@�@�@��@K�@�@��@�R@V@5?@{@�@�T@@�-@��@p�@O�@�@��@�@�/@�@j@1@�
@�F@�@dZ@33@��@^5@J@�@�@��@��@X@7L@�`@�9@A�@  @�w@��@�P@l�@;d@
=@��@�y@��@v�@ff@5?@$�@{@{@@@p�@O�@O�@?}@�@��@�/@�/@��@�@�D@z�@j@9X@�@��@��@�@�@�@�@�@C�@
��@
^5@
-@
�@	��@	�#@	�^@	�^@	��@	hs@	G�@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�$�A�(�A�(�A�&�A�+A�-A�-A�-A�+A��A���Aк^AЇ+AЁA�x�A�r�A�p�A�l�A�dZA�`BA�Q�A�5?A���A˗�A�;dAŸRA���A�oA�33A��jA���A���A��#A���A�7LA�ȴA��!A�%A��9A�jA��A�t�A�t�A���A�
=A��A�bNA�JA���A��A��A�E�A���A�9XA��hA���A��uA�A�t�A�  A��^A��TA�ĜA���A�hsA���A�`BA���A�bA��jA��
A��`A�$�A��TA��FA�oA���A�7LA�ZA�dZA�A��9A���A��A�ffA�"�A�9XA��A��A��#A���A��DA��hA�5?A�r�A�;dA��\A���A���A�E�A���A�A�hsA�^5A�A�5?A�PA}�Az�Ax��Ax^5Av��Au+ArJAoAi�hAg%AeK�AdJAc�AcVAb$�A`�RA\  AY;dAX  AVVAT�AR�AP�AM�wAK�PAJ�AJ��AI�7AGG�AEx�AC��ACVAA�A@��A?/A>�RA=�TA;+A:1A8E�A7dZA6ȴA6r�A5l�A5
=A5��A5ƨA5�7A4�/A4M�A3�A2�A1�A0��A/�PA. �A-+A,ZA,1A+t�A*v�A)&�A'�TA&�!A%�^A$�DA$$�A#�hA"�uA�AƨAz�A�
AK�A��A��A�HAr�A9XAJA��AoA�AJA�FA\)AoAjA�A?}A��AI�A�A�Az�A��Ar�A�PA
��A
(�A	l�A�\A��AE�AƨAC�A(�A�hA9XA �@���@�b@�ff@��j@��@���@���@�@�u@�\@�h@��@웦@�  @�K�@�$�@�9@�b@��@�^@�V@�o@�h@�%@���@�5?@�?}@�Z@۝�@���@�7L@�;d@�ff@��@���@��m@�
=@Ұ!@�^5@�v�@�{@�X@�j@�l�@�V@��@�~�@��@�x�@�z�@�bN@�bN@�(�@�
=@�@���@�1@�33@+@��@��h@�V@�  @�ȴ@�M�@�G�@�z�@�K�@���@��`@�$�@���@�x�@��/@��9@�bN@��
@�@��@�p�@�X@�Ĝ@��
@��@�o@��R@���@�@�p�@��`@�A�@��w@�|�@�@���@��\@���@�G�@�Ĝ@��D@�A�@�|�@��!@�~�@�V@��@��@��@��@�p�@�hs@�X@�O�@�p�@�`B@�/@���@�Ĝ@��9@� �@��@��w@���@�C�@�
=@���@�E�@���@�@�O�@�X@�O�@���@��D@�9X@��F@�;d@��!@�J@��h@�G�@�/@�V@��@��@�Q�@��F@�"�@���@�M�@�=q@�5?@�`B@�X@��@��/@�z�@�Q�@�Z@�A�@�(�@��@��w@��@�"�@�o@��y@��!@�$�@��T@��@�@���@��#@��T@�@�O�@��@�bN@�b@��w@���@��@��@�o@�;d@��@���@�ff@�ff@��^@��7@�I�@�K�@�ȴ@���@���@�ȴ@�|�@��P@��F@��
@���@�@��!@��h@�V@���@���@���@���@���@���@���@���@�I�@���@��@�
=@��y@��H@�ȴ@���@���@��!@��+@�ff@��@��@�@��7@�X@�/@���@��j@�Q�@�(�@�  @��m@���@��w@���@��@�K�@�"�@�@�ȴ@�~�@�E�@�-@�{@��@���@�O�@��/@��@��@�j@�Z@�I�@�1'@�  @��@��
@��@��P@�l�@�S�@�;d@�+@��@���@���@�ff@�@���@�x�@�p�@�p�@�hs@�X@�/@��@���@���@�Ĝ@���@�Z@�  @K�@~�@~v�@~{@}�h@}O�@|j@{�m@{C�@{"�@{"�@z�H@z�\@z�@y��@y��@yx�@x�`@x�9@w��@w\)@w;d@v�@vV@v@u�T@u�-@u�@t��@t��@t(�@s�m@sS�@r�\@r�@q��@q��@q�7@qx�@qG�@p��@pQ�@o�P@o�@n�y@n�+@n{@n@m�@m��@m�@mO�@l��@l��@l(�@k�
@k��@k��@k�@k"�@j�!@j�\@jM�@j-@jJ@i�#@i��@i&�@i�@i�@h�u@g�;@g\)@g+@f�@f��@f��@fV@e�@eO�@d��@dZ@d(�@c��@c��@cS�@b��@b~�@b^5@b�@bJ@a��@ahs@`��@`r�@`bN@`Q�@` �@_�w@_��@_+@^��@^ȴ@^5?@\�@\(�@[�m@[��@[C�@Z�!@Z^5@Z�@Y��@Yx�@X��@X�@W��@W\)@W\)@WK�@W+@V��@VE�@U@U?}@T�@T�D@Tj@Tj@T9X@T�@St�@R�H@R^5@RM�@R�@Q�#@Q��@Q�^@Q��@QG�@P��@PQ�@Pb@O��@O�P@Nȴ@NE�@N$�@N@M��@M�-@M�h@M/@Lj@LZ@L9X@L�@K�F@K�@Kt�@Kt�@KdZ@KdZ@KC�@K"�@J��@J~�@I�^@I&�@H��@H1'@Hb@G�@G�;@G��@G|�@G+@F��@Fȴ@F��@FE�@FE�@F5?@F{@E��@EO�@D�@Dz�@Dj@DZ@D1@C��@Co@Bn�@B-@AX@A%@@�`@@��@@Q�@@A�@@1'@@  @?�@>v�@=�T@=�-@=�@<�/@<�@<z�@<(�@;��@;��@;S�@:��@:J@9��@9��@9x�@9G�@97L@9&�@8��@8�@7�@7��@7��@7�w@7|�@6��@6��@6�+@6�+@6E�@5��@5�h@5p�@4��@3��@3ƨ@3��@3��@333@2�@2�H@2�!@2M�@2J@1�@1�^@1hs@1hs@1X@1G�@1�@0��@0�@0Q�@0 �@/�@/l�@.��@.ff@.E�@.{@-�@-��@-��@-�T@-��@-��@-��@-��@-�@-?}@,�/@,j@,I�@,1@+ƨ@+�F@+��@+33@+@*��@*~�@)�@)��@)��@)x�@)G�@(��@(��@(bN@(A�@(A�@(1'@(  @'��@'K�@'�@&��@&�@&ȴ@&ȴ@&ȴ@&ȴ@&�R@&��@&�+@&V@&5?@&$�@&@%�-@%�@%`B@%/@$�@$�@$z�@$I�@$�@#��@#�F@#C�@#33@#"�@#"�@#o@#@"��@"�\@"n�@"M�@"J@!X@ Ĝ@ �9@ 1'@�@;d@�y@��@��@�y@�y@�y@ȴ@�+@ff@5?@{@{@5?@ff@ff@V@�@O�@V@�@��@�j@�D@z�@j@Z@�@�F@t�@o@��@�\@M�@J@�^@hs@&�@��@�u@r�@bN@b@�w@�w@�@�@�@��@K�@�@��@�R@V@5?@{@�@�T@@�-@��@p�@O�@�@��@�@�/@�@j@1@�
@�F@�@dZ@33@��@^5@J@�@�@��@��@X@7L@�`@�9@A�@  @�w@��@�P@l�@;d@
=@��@�y@��@v�@ff@5?@$�@{@{@@@p�@O�@O�@?}@�@��@�/@�/@��@�@�D@z�@j@9X@�@��@��@�@�@�@�@�@C�@
��@
^5@
-@
�@	��@	�#@	�^@	�^@	��@	hs@	G�@	G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B0!B1'B2-B2-B2-B1'B1'B1'B1'B1'B0!B0!B.B.B-B,B-B,B,B+B(�B$�B�B.B(�B&�B?}BL�BW
BbNBe`BffBgmBp�Bu�By�B�%B�PB�DB�=B�=B�B��B�B��B�B�-B�3B�3B�'B�B�B��B��B��B��B��B��B�VB�=B� Bt�Br�Bp�Bm�BhsBbNBZBQ�BL�BB�B5?B+B&�B$�B�B�B\BB�5B�qB��B�1Bv�BK�BDB
��B
�B!�B<jB>wB-B�B
��B
��BB
��B
�/B
��B
B
��B
ȴB
�3B
��B
�{B
�=B
s�B
\)B
J�B
1'B
,B
�B
hB	�B	ÖB	�PB	jB	\)B	R�B	N�B	K�B	D�B	9XB	 �B	{B	hB		7B	  B��B�B�BB��B��B��BǮBɺB��B�?B�B��B��B��B��B��B��B��B�uB�VB�DB�1B�%B�DB��B��B��B��B��B��B��B��B��B��B�oB�uB�bB�VB�JB�=B�B�B|�Bz�Bu�Bt�Br�Bn�BiyBcTB^5B\)B[#BYBXBT�BS�BS�BR�BQ�BO�BM�BN�BM�BM�BL�BJ�BI�BG�BE�BE�BD�B>wB<jB:^B9XB5?B49B33B1'B1'B.B/B-B.B,B+B+B(�B'�B&�B#�B#�B"�B!�B �B�B�B"�B#�B%�B'�B(�B(�B(�B)�B(�B)�B)�B)�B,B+B+B,B+B,B+B+B+B.B0!B2-B2-B49B6FB49B49B49B9XB;dB=qB?}BA�B@�B;dB7LB8RB9XB<jB<jB=qB?}B@�B@�BA�BD�BD�BG�BK�BO�BS�BVBW
BYB^5B_;BbNBcTBgmBjBn�Bo�Bs�Bs�Bu�Bw�By�B|�B~�B~�B� B�B�B�%B�1B�1B�DB�VB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�?B�LB�RB�wBĜBƨBǮBȴB��B��B��B��B�)B�5B�BB�ZB�fB�sB�yB�yB�B�B�B��B��B��B��B	B	B	B	+B	bB	�B	�B	�B	 �B	"�B	$�B	,B	33B	7LB	=qB	B�B	D�B	J�B	L�B	M�B	O�B	Q�B	R�B	S�B	VB	XB	ZB	]/B	^5B	`BB	bNB	cTB	cTB	e`B	jB	jB	k�B	m�B	o�B	r�B	t�B	x�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�JB	�bB	�bB	�bB	�hB	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�?B	�RB	�RB	�XB	�dB	�jB	�wB	�}B	�}B	��B	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B
DB
JB
JB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
\B
bB
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
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
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
.B
.B
/B
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
49B
49B
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
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
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
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
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
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
R�B
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
YB
YB
ZB
ZB
ZB
ZB
YB
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
[#B
ZB
ZB
ZB
[#B
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
^5B
^5B
^5B
^5B
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
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
jB
jB
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
k�B
k�B
k�B
k�B
k�B
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
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B/�B0�B1�B1�B1�B0�B0�B0�B0�B0�B/�B/�B-�B-�B,�B+�B,�B+�B+�B*�B(�B$�BqB-�B(�B&�B?HBL�BV�BbBe,Bf2Bg8BpoBu�By�B��B�B�B�	B�	B��B�kB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�_B�"B�	B�Bt�Br|BpoBm]Bh>BbBY�BQ�BL�BB[B5B*�B&�B$�B�BSB(B �B�B�<B��B��BvzBK�BB
�B
�|B!�B<6B>BB,�B_B
��B
��B�B
��B
��B
ΊB
�[B
�UB
�fB
��B
�vB
�FB
�	B
s�B
[�B
J�B
0�B
+�B
�B
4B	�]B	�aB	�B	jKB	[�B	R�B	N�B	K�B	DMB	9$B	 �B	FB	B	�B��B��B�WB�BѝBΥB˒B�_BɆB�;B��B��B��B��B�jB��B��B�MB�9B�&B�"B�B��B��B��B�pB��B��B��B��B��B��B�vB�dB�_B� B�@B�.B�B�B�	B��B��B|�Bz�ButBtnBraBncBiDBcB^B[�BZ�BX�BW�BT�BS�BS�BR�BQ�BO�BM�BN�BM�BM�BL~BJrBIlBGzBESBESBDMB>BB<6B:B9	B4�B4B2�B0�B0�B-�B.�B,�B-�B+�B*�B*�B(�B'�B&�B#�B#�B"�B!|B vBpBpB"�B#�B%�B'�B(�B(�B(�B)�B(�B)�B)�B)�B+�B*�B*�B+�B*�B+�B*�B*�B*�B-�B/�B1�B1�B3�B5�B3�B3�B3�B9	B;B="B?HBA;B@4B;B6�B8B9	B<B<B="B?.B@4B@4BAUBDMBDMBG_BKxBO�BS�BU�BV�BX�B]�B_Ba�BcBgBj0BnIBoOBshBshBu�Bw�By�B|�B~�B~�B�B��B��B��B��B��B��B�B�B� B�,B�SB�QB�QB�]B�pB��B��B��B��B��B��B��B�B�BB�MB�?B�_BȀB�rB˒B�~BңB��B��B��B�B�B�$B�*B�*B�=B�iB�aB��B��B��B��B	 �B	 �B	�B	�B	B	2B	]B	jB	 vB	"�B	$�B	+�B	2�B	6�B	="B	BAB	DMB	JrB	L~B	M�B	O�B	Q�B	R�B	S�B	U�B	W�B	Y�B	\�B	]�B	`B	a�B	c B	cB	eB	j0B	j0B	k6B	m]B	oOB	raB	tnB	x�B	y�B	{�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�&B	�YB	�9B	�9B	�?B	�?B	�EB	�QB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�B	�6B	�(B	�.B	�.B	�4B	�GB	�MB	�MB	�MB	�MB	�SB	�YB	�fB	�fB	�lB	�rB	�xB	�~B	̈́B	ΊB	ѝB	ңB	ңB	өB	ԯB	յB	յB	ּB	��B	��B	��B	ٴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�B	�B	�B	�B	�B	�$B	�$B	�
B	�DB	�6B	�=B	�=B	�=B	�=B	�=B	�=B	�CB	�CB	�IB	�cB	�IB	�IB	�OB	�[B	�AB	�|B	�aB	�hB	�hB	�hB	�tB	�ZB	�zB	�zB	�`B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
�B

�B

�B

�B
�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
B
 B
B
B
 B
 B
&B
&B
,B
,B
,B
FB
2B
2B
9B
9B
9B
9B
9B
?B
?B
?B
?B
?B
EB
EB
EB
EB
KB
KB
KB
WB
WB
]B
]B
CB
dB
IB
dB
jB
jB
pB
pB
 vB
 vB
 vB
 vB
 vB
 vB
!|B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
"hB
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%zB
%�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
6B
5�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
9	B
9	B
9	B
9	B
:*B
:B
:B
;B
;B
<B
<B
<B
="B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
?B
?.B
?.B
?.B
?.B
@4B
@4B
@4B
@4B
@OB
@4B
A;B
A;B
BAB
BAB
B'B
BAB
CGB
CGB
CaB
CGB
CaB
CGB
DMB
DMB
ESB
E9B
EmB
ESB
FtB
FYB
FYB
FYB
G_B
G_B
G_B
HfB
HfB
HfB
I�B
IlB
JrB
JrB
JrB
JrB
JrB
JrB
JXB
JrB
JrB
JrB
JrB
KxB
K�B
KxB
KxB
L~B
L~B
L~B
L~B
M�B
M�B
M�B
MjB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
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
R�B
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
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Y�B
Y�B
Y�B
Z�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
]�B
]�B
^B
]�B
]�B
]�B
^B
]�B
]�B
]�B
^�B
^�B
^�B
`B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
aB
`�B
`�B
a�B
a�B
a�B
b�B
cB
cB
cB
dB
dB
dB
d&B
dB
dB
e,B
eB
eB
eB
fB
fB
e�B
fB
f2B
gB
gB
gB
g8B
gB
gB
h$B
h$B
h$B
h$B
h$B
h>B
h$B
i*B
i*B
i*B
iDB
i*B
iDB
j0B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
j0B
k6B
kQB
k6B
k6B
k6B
k6B
k6B
k6B
lWB
l=B
l=B
l=B
mCB
m]B
mCB
mCB
m]B
mCB
mCB
m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.59(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810160039572018101600395720181016003957201810170031272018101700312720181017003127JA  ARFMdecpA19c                                                                20181010213514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181010123515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181010123516  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181010123517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181010123517  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181010123517  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181010123517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181010123517  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181010123518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181010123518                      G�O�G�O�G�O�                JA  ARUP                                                                        20181010125453                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181010153402  CV  JULD            G�O�G�O�F�>�                JM  ARCAJMQC2.0                                                                 20181015153957  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181015153957  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181016153127  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                