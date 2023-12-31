CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-03T12:37:10Z creation;2019-05-03T12:37:15Z conversion to V3.1;2019-12-23T06:02:53Z update;     
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20190503123710  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_143                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ػ�Y��1   @ػ�β @7�s�PH�c<641   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�fD�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.C �
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
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{%�D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D�ϮD��D�R�D���D���D��D�R�D���D���D��D�R�D���D�ϮD��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�VD���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�R�DĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�DҒ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�R�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D���D���D��D�R�D���D���D�HD�2�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA���A��^A�K�A�ƨA��FA��!A���A���A��hA��PA��A�v�A�bNA�/A���A�bNA�1A��uA�{A���A���A��uA�S�A��A��FA�VA�hsA���A���A���A��FA��hA�hsA�$�A��A�ZA��!A�;dA��`A���A��!A��A��\A��^A�7LA�x�A��!A�oA�"�A��PA�
=A�p�A�{A���A��A��PA�33A��!A���A��A��A�jA�ĜA��PA�/A�ȴA��+A�S�A�A�A���A��A�A��9A���A��A�dZA�1A���A���A���A���A�M�A�x�A��A�l�A��A�bA���A���A�(�A�ƨA�E�A�A���A�^5A�S�A�7LA��7A�\)A�;dA���A� �A���A���A��`A�
=A��A�bA�-A��/A��yA�-A��A�-A��A��A���A��#A��!A�?}A�
=A��A|�+Az�!Az1Ay�Au�-Ar�9Ap�RAn�jAk33Ai��Ai�Ah��Agx�Ae�Ad��Ab��Aa`BA`1A^�DA]�
A]l�A\�jA[��A[oAY33AX9XAW+AU;dAShsAQ��AP�DAO�PAM��AL��AK�^AI��AG+ADr�AD  AC�^AC��AC��ACdZAB��AA
=A?
=A=|�A;K�A:v�A:9XA9��A8=qA5��A4��A4bA2��A1A0�RA/�FA.��A-�#A,��A,r�A+\)A*9XA)p�A(��A(E�A'��A'��A%��A#�A ��A�
A�A�A5?A��A�FAjA�A��A��A�TA�`A�`A��AE�A�A�/Az�A�A��A�A �A�A��A
��A	|�A	7LA	K�A�9A�wA��A�-A��A"�A`BAS�A�A �A Z@���@�X@��m@�E�@��@��F@�E�@�j@�-@��@�!@�%@��;@��@��T@�G�@�j@� �@�dZ@��@噚@�Ĝ@�r�@��m@��@�=q@�X@�r�@�C�@ް!@ݡ�@��`@܃@�t�@��H@ٙ�@ج@���@Չ7@ԛ�@ҸR@�%@�Z@ϥ�@�M�@��@�A�@���@�v�@��/@ǥ�@�~�@��/@��@�o@���@�~�@�G�@���@�-@�O�@���@�z�@��@�ƨ@���@�l�@�o@��@��@���@�l�@��!@��-@�G�@���@�j@�Q�@��P@���@�@��#@���@�?}@��9@�Z@��
@���@�dZ@�K�@�S�@�S�@��@�n�@�@���@��@�O�@�&�@��9@��D@��@� �@��@�o@�{@�{@���@��@�O�@��@���@�Ĝ@�dZ@��!@�ff@���@�Ĝ@�(�@�  @���@���@��P@�+@�v�@���@���@���@�Q�@��@��w@��@�C�@���@�5?@�V@�1@���@�\)@�K�@�;d@�@���@���@�^5@�{@��#@���@�hs@��7@��^@���@��h@��@��u@�r�@�j@�j@��j@��@��D@��D@��@�1'@�dZ@��@���@�^5@�$�@��^@�`B@�/@�/@��^@�X@�Q�@�t�@��@��y@�ȴ@��R@���@���@�v�@�^5@�{@��-@�p�@�`B@�G�@��@��/@���@���@�z�@�r�@�bN@�b@���@��@�|�@�S�@��@���@�M�@��@���@��#@��@��@���@�X@�?}@�?}@��@�bN@�9X@� �@�  @��m@��F@��@�S�@�33@���@��R@���@���@�n�@�{@�J@��@��@�-@��@�{@��@�@��7@�?}@�&�@��@�%@���@���@��`@���@�r�@�9X@���@�
=@��@�+@���@��!@�~�@�n�@�V@�$�@���@���@�G�@�/@�V@��@��@��j@�j@�r�@�9X@� �@�  @��F@�dZ@��@���@��y@���@���@��!@�ff@���@�@���@��h@��7@�X@��@���@��j@�j@�(�@�w@��@��@��@�w@+@~�@~ȴ@~�R@~v�@}?}@|�j@{�m@{��@{��@{ƨ@z�H@z=q@y��@z=q@z-@y��@xbN@w�;@w�P@w�@v��@vv�@v5?@v@up�@tz�@s��@s��@s"�@rM�@p�9@pQ�@pbN@pb@o�P@n��@n��@n5?@l�j@l�@l�D@l9X@k�
@kƨ@k�F@k�F@k��@j��@j�@jJ@j=q@j-@i�@iG�@h1'@g�;@g|�@g�@fE�@e�@d�D@c@c"�@c��@c�F@b�@b��@b~�@b^5@b�@a�^@ax�@a%@a%@aG�@ax�@ax�@`�@`  @_�;@_��@_��@_��@_\)@^v�@]p�@\��@\z�@\9X@\(�@\(�@\(�@\�@[dZ@Z�\@Z=q@Y��@Y�7@Y��@Yx�@Y%@XĜ@X1'@Xb@W�;@Wl�@W�@V��@W�@W�@W
=@V�y@V@U��@U�-@U��@U�@U/@UV@T��@Tz�@Tj@T1@S�m@S�m@S��@S��@S@R�!@R�\@Rn�@RM�@R�@Q��@QG�@Q�@P�`@P�u@PA�@P1'@Pb@O��@O��@O|�@O;d@N��@NE�@M�@M��@M`B@L�@Lz�@LI�@L�@K��@K@J��@J-@I�#@Ihs@I�@H��@H�@H1'@G�w@Gl�@GK�@F��@F�R@Fff@F@E�@E@E�@E`B@E�@D��@Dj@DI�@C��@C�m@CC�@Co@C"�@C@B~�@B=q@B�@A�@Ax�@Ahs@A�@@�9@@�@@Q�@@A�@@ �@?�;@?�P@?l�@?+@>�@>��@>��@>v�@>ff@>5?@>$�@=�T@=`B@=�@<��@<��@<��@<j@<(�@;��@;��@;�@;�@;t�@;C�@;o@;@:�@:��@:�\@:n�@:=q@9��@9�7@9hs@9&�@9%@8�`@8��@8bN@7�@7�w@7�@7|�@7+@6��@6�R@6�+@6{@5�T@5�-@5��@5p�@5p�@5O�@5V@4�/@4j@4�@3ƨ@3�@333@2��@2M�@2J@1�@1�7@1X@0�`@0��@0Q�@/�;@/;d@/
=@.�@.v�@.{@-@-p�@-/@-�@,��@,�@,��@,Z@,9X@+�m@+�@+33@+33@+33@+33@+"�@+@*�@*��@*�!@*M�@*�@)�@)x�@)7L@(�u@(bN@'�;@'l�@'K�@'�@&��@%@%@%`B@%�@%V@$��@%V@$��@$��@$j@$9X@#��@#�
@#��@#dZ@#o@"�!@"�!@"��@"=q@!�@"J@!��@!��@!��@!X@!�@ ��@ ��@ r�@ A�@   @�;@+@��@��@$�@@�T@��@p�@`B@O�@?}@V@�@I�@��@�F@�@o@��@^5@^5@=q@��@�@��@G�@�u@A�@ �@�;@�P@K�@
=@��@ȴ@��@V@5?@@@O�@�/@j@9X@�@�@1@�m@ƨ@dZ@o@�@��@��@�\@n�@^5@=q@�@J@�#@�^@��@�7@X@&�@�@%@��@��@r�@A�@ �@  @�@�w@l�@�@�y@�@ȴ@�R@v�@5?@@��@�-@`B@/@V@��@��@�D@�D@z�@j@Z@9X@��@��@�@S�@"�@o@@
��@
�\@
M�@
�@
�@
J@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��mA���A��^A�K�A�ƨA��FA��!A���A���A��hA��PA��A�v�A�bNA�/A���A�bNA�1A��uA�{A���A���A��uA�S�A��A��FA�VA�hsA���A���A���A��FA��hA�hsA�$�A��A�ZA��!A�;dA��`A���A��!A��A��\A��^A�7LA�x�A��!A�oA�"�A��PA�
=A�p�A�{A���A��A��PA�33A��!A���A��A��A�jA�ĜA��PA�/A�ȴA��+A�S�A�A�A���A��A�A��9A���A��A�dZA�1A���A���A���A���A�M�A�x�A��A�l�A��A�bA���A���A�(�A�ƨA�E�A�A���A�^5A�S�A�7LA��7A�\)A�;dA���A� �A���A���A��`A�
=A��A�bA�-A��/A��yA�-A��A�-A��A��A���A��#A��!A�?}A�
=A��A|�+Az�!Az1Ay�Au�-Ar�9Ap�RAn�jAk33Ai��Ai�Ah��Agx�Ae�Ad��Ab��Aa`BA`1A^�DA]�
A]l�A\�jA[��A[oAY33AX9XAW+AU;dAShsAQ��AP�DAO�PAM��AL��AK�^AI��AG+ADr�AD  AC�^AC��AC��ACdZAB��AA
=A?
=A=|�A;K�A:v�A:9XA9��A8=qA5��A4��A4bA2��A1A0�RA/�FA.��A-�#A,��A,r�A+\)A*9XA)p�A(��A(E�A'��A'��A%��A#�A ��A�
A�A�A5?A��A�FAjA�A��A��A�TA�`A�`A��AE�A�A�/Az�A�A��A�A �A�A��A
��A	|�A	7LA	K�A�9A�wA��A�-A��A"�A`BAS�A�A �A Z@���@�X@��m@�E�@��@��F@�E�@�j@�-@��@�!@�%@��;@��@��T@�G�@�j@� �@�dZ@��@噚@�Ĝ@�r�@��m@��@�=q@�X@�r�@�C�@ް!@ݡ�@��`@܃@�t�@��H@ٙ�@ج@���@Չ7@ԛ�@ҸR@�%@�Z@ϥ�@�M�@��@�A�@���@�v�@��/@ǥ�@�~�@��/@��@�o@���@�~�@�G�@���@�-@�O�@���@�z�@��@�ƨ@���@�l�@�o@��@��@���@�l�@��!@��-@�G�@���@�j@�Q�@��P@���@�@��#@���@�?}@��9@�Z@��
@���@�dZ@�K�@�S�@�S�@��@�n�@�@���@��@�O�@�&�@��9@��D@��@� �@��@�o@�{@�{@���@��@�O�@��@���@�Ĝ@�dZ@��!@�ff@���@�Ĝ@�(�@�  @���@���@��P@�+@�v�@���@���@���@�Q�@��@��w@��@�C�@���@�5?@�V@�1@���@�\)@�K�@�;d@�@���@���@�^5@�{@��#@���@�hs@��7@��^@���@��h@��@��u@�r�@�j@�j@��j@��@��D@��D@��@�1'@�dZ@��@���@�^5@�$�@��^@�`B@�/@�/@��^@�X@�Q�@�t�@��@��y@�ȴ@��R@���@���@�v�@�^5@�{@��-@�p�@�`B@�G�@��@��/@���@���@�z�@�r�@�bN@�b@���@��@�|�@�S�@��@���@�M�@��@���@��#@��@��@���@�X@�?}@�?}@��@�bN@�9X@� �@�  @��m@��F@��@�S�@�33@���@��R@���@���@�n�@�{@�J@��@��@�-@��@�{@��@�@��7@�?}@�&�@��@�%@���@���@��`@���@�r�@�9X@���@�
=@��@�+@���@��!@�~�@�n�@�V@�$�@���@���@�G�@�/@�V@��@��@��j@�j@�r�@�9X@� �@�  @��F@�dZ@��@���@��y@���@���@��!@�ff@���@�@���@��h@��7@�X@��@���@��j@�j@�(�@�w@��@��@��@�w@+@~�@~ȴ@~�R@~v�@}?}@|�j@{�m@{��@{��@{ƨ@z�H@z=q@y��@z=q@z-@y��@xbN@w�;@w�P@w�@v��@vv�@v5?@v@up�@tz�@s��@s��@s"�@rM�@p�9@pQ�@pbN@pb@o�P@n��@n��@n5?@l�j@l�@l�D@l9X@k�
@kƨ@k�F@k�F@k��@j��@j�@jJ@j=q@j-@i�@iG�@h1'@g�;@g|�@g�@fE�@e�@d�D@c@c"�@c��@c�F@b�@b��@b~�@b^5@b�@a�^@ax�@a%@a%@aG�@ax�@ax�@`�@`  @_�;@_��@_��@_��@_\)@^v�@]p�@\��@\z�@\9X@\(�@\(�@\(�@\�@[dZ@Z�\@Z=q@Y��@Y�7@Y��@Yx�@Y%@XĜ@X1'@Xb@W�;@Wl�@W�@V��@W�@W�@W
=@V�y@V@U��@U�-@U��@U�@U/@UV@T��@Tz�@Tj@T1@S�m@S�m@S��@S��@S@R�!@R�\@Rn�@RM�@R�@Q��@QG�@Q�@P�`@P�u@PA�@P1'@Pb@O��@O��@O|�@O;d@N��@NE�@M�@M��@M`B@L�@Lz�@LI�@L�@K��@K@J��@J-@I�#@Ihs@I�@H��@H�@H1'@G�w@Gl�@GK�@F��@F�R@Fff@F@E�@E@E�@E`B@E�@D��@Dj@DI�@C��@C�m@CC�@Co@C"�@C@B~�@B=q@B�@A�@Ax�@Ahs@A�@@�9@@�@@Q�@@A�@@ �@?�;@?�P@?l�@?+@>�@>��@>��@>v�@>ff@>5?@>$�@=�T@=`B@=�@<��@<��@<��@<j@<(�@;��@;��@;�@;�@;t�@;C�@;o@;@:�@:��@:�\@:n�@:=q@9��@9�7@9hs@9&�@9%@8�`@8��@8bN@7�@7�w@7�@7|�@7+@6��@6�R@6�+@6{@5�T@5�-@5��@5p�@5p�@5O�@5V@4�/@4j@4�@3ƨ@3�@333@2��@2M�@2J@1�@1�7@1X@0�`@0��@0Q�@/�;@/;d@/
=@.�@.v�@.{@-@-p�@-/@-�@,��@,�@,��@,Z@,9X@+�m@+�@+33@+33@+33@+33@+"�@+@*�@*��@*�!@*M�@*�@)�@)x�@)7L@(�u@(bN@'�;@'l�@'K�@'�@&��@%@%@%`B@%�@%V@$��@%V@$��@$��@$j@$9X@#��@#�
@#��@#dZ@#o@"�!@"�!@"��@"=q@!�@"J@!��@!��@!��@!X@!�@ ��@ ��@ r�@ A�@   @�;@+@��@��@$�@@�T@��@p�@`B@O�@?}@V@�@I�@��@�F@�@o@��@^5@^5@=q@��@�@��@G�@�u@A�@ �@�;@�P@K�@
=@��@ȴ@��@V@5?@@@O�@�/@j@9X@�@�@1@�m@ƨ@dZ@o@�@��@��@�\@n�@^5@=q@�@J@�#@�^@��@�7@X@&�@�@%@��@��@r�@A�@ �@  @�@�w@l�@�@�y@�@ȴ@�R@v�@5?@@��@�-@`B@/@V@��@��@�D@�D@z�@j@Z@9X@��@��@�@S�@"�@o@@
��@
�\@
M�@
�@
�@
J@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�1B
�+B
�B
�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
�7B
�=B
�DB
�VB
�PB
�uB
�{B
�FB
�}B
ɺB
�
B
�B
�)B
�NB
��B�B&�B@�BW
BhsBl�Bq�B�+B��B�B��B�fB�B�BB�B�mB��B�B�B�B��B�B��B��B��B+B2-B:^B-B-B�B�BoBbBuB\BuB7LB>wBK�BI�BA�BC�BH�BH�B>wB8RB/B$�B�B�B�B�B$�B�BVBDBJBJB��B�sB�B�dB�B�1B[#BM�BG�BG�BI�B5?B{B{BoBB
��B
�B
�B
��B
��B
�^B
�B
��B
��B
�B
r�B
[#B
M�B
=qB
0!B
(�B
"�B
VB	��B	�B	�5B	ȴB	�}B	�jB	�XB	�?B	�B	��B	��B	��B	�hB	�=B	�+B	�B	�B	z�B	v�B	l�B	gmB	aHB	[#B	N�B	C�B	:^B	5?B	)�B	!�B	�B	\B��B�/B�#B�B�B�B�
B��BŢB�XB�B��B�{B��B��B�\B�By�Bu�Bq�Bm�BiyBe`BffBiyBhsBgmBe`BffBffBjBn�Bm�Bk�Bm�Bn�BdZBbNBdZBffBdZBcTBcTBe`Be`B`BB[#B[#B[#B_;BcTB`BB`BB^5B\)BYBT�BP�BO�BM�BO�BO�BP�BXB_;B_;BZB_;BbNBjBjB`BBR�BE�BA�BA�BB�BD�BD�BA�B>wB<jB<jB:^B7LB5?B5?B7LB9XB<jB<jB<jB<jB<jB=qB=qB@�BA�BA�BA�BC�BD�BF�BG�BH�BI�BL�BM�BN�BM�BQ�BS�BS�BT�BS�BS�BS�BQ�BQ�BQ�BT�BT�BW
BXB\)B[#B\)B_;Be`BgmBk�Bl�Bp�Br�Bv�Bz�B}�B� B�B�1B�=B�DB�PB�PB�PB�PB�DB�VB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�FB�RB�jB�jB�jB��BŢBȴB��B��B��B��B�B�B�B�)B�5B�BB�NB�`B�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B	B	B	B	%B		7B	bB	oB	�B	�B	�B	�B	 �B	"�B	%�B	)�B	,B	-B	/B	1'B	33B	7LB	:^B	<jB	@�B	A�B	C�B	G�B	I�B	O�B	P�B	Q�B	Q�B	R�B	Q�B	Q�B	Q�B	S�B	T�B	XB	[#B	\)B	]/B	^5B	cTB	e`B	e`B	iyB	k�B	l�B	m�B	n�B	n�B	o�B	p�B	q�B	s�B	w�B	y�B	z�B	{�B	~�B	~�B	� B	�B	�B	�B	�B	�7B	�VB	�\B	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�3B	�?B	�FB	�RB	�XB	�XB	�dB	�jB	�qB	�wB	��B	��B	ÖB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
	7B
	7B

=B

=B

=B
	7B
	7B
1B
+B
1B
+B
+B
	7B
DB
JB
DB
JB
JB
JB
DB
DB
DB
DB
DB
JB
PB
PB
PB
PB
VB
VB
VB
\B
\B
VB
PB
PB
PB
VB
VB
\B
bB
bB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
.B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
49B
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
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
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
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
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
=qB
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
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
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
H�B
H�B
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
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
H�B
H�B
H�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
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
R�B
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
T�B
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
VB
W
B
VB
VB
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
YB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
]/B
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
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
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
iy111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�	B
�B
�"B
�B
�@B
�FB
�B
�HB
ɆB
��B
��B
��B
�B
�BYB&�B@OBV�Bh>BlWBqvB��B�kB��BʌB�2B�WB�B��B�8B��B�B�B�iB��B�B��B��B��B�B1�B:*B,�B,�B�BYB:B.B@B(B@B7B>BBK�BI�BAUBCaBH�BH�B>BB8B.�B$�BxBkBqB�B$�BkB"BBBB��B�>B��B�0B��B��BZ�BM�BGzBGzBI�B5BFBFB B�B
��B
�aB
��B
бB
ʌB
�*B
��B
��B
�EB
��B
raB
Z�B
M�B
=<B
/�B
(�B
"�B
"B	��B	�WB	�B	ȀB	�HB	�B	�	B	��B	��B	��B	��B	�YB	�B	�	B	��B	��B	��B	z�B	v�B	lWB	g8B	`�B	Z�B	N�B	CaB	:*B	4�B	)�B	!|B	~B	B��B��B��B��B��B��B��BбB�SB�$B��B�QB�,B�KB�EB�B��By�ButBq[BmCBi*Be,BfBi*Bh$Bg8Be,Bf2BfBj0BncBmCBk6Bm]BncBdBa�Bd&BfBdBc Bc Be,Be,B_�BZ�BZ�BZ�B^�Bc B_�B_�B^B[�BX�BT�BP�BO�BM�BO�BO�BP�BW�B_B^�BY�B^�Ba�Bj0Bj0B_�BR�BEmBA;BA;BB[BDMBDMBA;B>BB<B<B:*B6�B5B4�B7B9	B<6B<B<6B<B<B="B="B@4BA;BA;BA;BCGBDMBFYBGzBHfBIlBL~BM�BN�BM�BQ�BS�BS�BT�BS�BS�BS�BQ�BQ�BQ�BT�BT�BV�BW�B[�BZ�B[�B_BeBg8Bk6Bl=BpUBraBvzBz�B}�B�B��B��B��B��B�B�B�B�B��B�"B�2B�kB�QB�qB�WB�QB�]B�pB�|B�|B��B��B��B��B��B��B�B�B�B�6B�OB�SB�fB̈́BѝBөBԯB��B��B��B��B��B��B��B�,B�$B�6B�CB�IB�IB�UB�tB�tB��B��B��B��B��B��B��B��B��B��B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 B	2B	KB	dB	�B	 vB	"hB	%�B	)�B	+�B	,�B	.�B	0�B	2�B	6�B	:B	<B	@4B	A;B	CGB	G_B	IlB	O�B	P�B	Q�B	Q�B	R�B	Q�B	Q�B	Q�B	S�B	T�B	W�B	Z�B	[�B	\�B	]�B	cB	eB	eB	i*B	kQB	lWB	mCB	nIB	nIB	oOB	pUB	qvB	s�B	w�B	y�B	z�B	{�B	~�B	~�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�2B	�?B	�KB	�]B	�jB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�$B	�B	�B	�"B	�(B	�4B	�UB	�GB	�YB	�fB	�lB	�lB	�XB	�~B	̈́B	ΊB	ϑB	ϑB	ϑB	ϑB	ϑB	ϑB	ѝB	ҽB	өB	өB	ңB	өB	өB	ԯB	ԯB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�,B	�B	�B	�B	�B	�
B	�
B	�KB	�6B	�=B	�IB	�aB	�nB	�zB	�zB	�`B	�zB	�zB	�zB	�tB	�nB	�hB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
B
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
B
B
B
B
B
"B
B
B
B
B
B
(B
B
B
 B
 B
 B
 B
 B
B
,B
B
,B
,B
,B
,B
9B
?B
EB
EB
KB
QB
WB
]B
xB
dB
dB
IB
jB
jB
jB
pB
 vB
!|B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
8B
8B
9	B
8�B
9	B
:*B
:*B
:*B
:B
:B
;0B
;B
;0B
;B
;B
<B
<B
<B
<B
<B
<B
<B
<B
<B
<B
<B
="B
="B
="B
=<B
=B
="B
>(B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
?HB
@4B
@4B
A;B
A;B
A;B
B[B
BAB
B[B
BAB
BAB
CGB
CGB
DMB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
ESB
ESB
ESB
ESB
FYB
FYB
FYB
G_B
G_B
G_B
G_B
G_B
GzB
H�B
HfB
HfB
HfB
H�B
HfB
HfB
IlB
IlB
IlB
I�B
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
IlB
HfB
HfB
HfB
HfB
HfB
JrB
KxB
KxB
KxB
K�B
K�B
KxB
K�B
K�B
L~B
M�B
M�B
M�B
M�B
N�B
N�B
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
R�B
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
T�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
U�B
V�B
U�B
U�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
^B
]�B
]�B
]�B
]�B
^�B
_B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`�B
aB
_�B
`�B
`�B
`�B
`�B
bB
a�B
a�B
bB
a�B
cB
cB
cB
c B
cB
dB
d&B
c�B
dB
eB
eB
eB
eB
fB
fB
fB
fB
fB
fB
fB
fB
gB
gB
gB
h$B
h$B
h$B
h$B
h$B
h$B
iDB
iDB
iDB
i*B
i*111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.59(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905090035312019050900353120190509003531201905100026332019051000263320190510002633JA  ARFMdecpA19c                                                                20190503213642  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190503123710  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190503123712  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190503123713  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190503123714  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190503123714  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190503123714  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190503123714  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190503123715  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190503123715                      G�O�G�O�G�O�                JA  ARUP                                                                        20190503125722                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190503153418  CV  JULD            G�O�G�O�F�ا                JM  ARCAJMQC2.0                                                                 20190508153531  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190508153531  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190509152633  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                