CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-02T19:19:52Z creation;2019-02-02T19:19:55Z conversion to V3.1;2019-12-23T06:07:36Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݠ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20190202191952  20200120021524  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA  I2_0675_122                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؤ�����1   @ؤ�I���@7���u��c4��%��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
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
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)\D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{,)D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D�ϮD��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�VD���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�R�DĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�DҒ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�O�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�VD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�S�A�K�A�S�A�XA�^5A�ZA�XA�\)A�bNA�dZA�dZA�dZA�bNA�dZA�ffA�jA�jA�hsA�l�A�p�A�r�A�r�A�x�A�z�A�z�A�r�A�VA�;dA�5?A�(�A��A�VA���A��yA���A���A���A���A��
A��#A��;A���A���A�ƨA���A��wA��-A��A���A���A���A���A��hA��7A�|�A�r�A�n�A�`BA�S�A�I�A�  A�S�A��A�x�A��!A�p�A�A��-A��RA��mA�ƨA��A��mA���A�{A�%A��A��A�jA�{A��HA���A�XA��;A�=qA�1'A�A�A��^A��+A���A�Q�A�|�A�ZA�  A��A���A�\)A�O�A�|�A��`A�ĜA��A�dZA�p�A�JA�t�A�ƨA��/A��A���A���A�1'A��+A�ĜA���A��A��!A��TA�ȴA�;dA���A��A�TA��A~{A{l�Ay�-Aw�Ar�uAq33Ap-Al�Aj��Af�RAd��Aal�A_��A\5?AY��AY��AYx�AX�/AW�#AV�9ATbAR��AQ��AO|�AL�jAKp�AH�HAE�#ADM�AAG�A?�wA?�wA?��A<�A;K�A9A8ZA7�A6�jA6A57LA2��A1x�A1%A0ȴA0A�A/?}A.1A-33A,~�A+/A)ƨA($�A&�`A%p�A#G�A"bA!l�A Q�A�`AA
=A�A�A��AoA�A��AjA5?A�AVA��A(�A��A+A��A�`A-A�-A"�A��A�+AA�A5?A
�A	�
A	%A�\A��AG�A�!A��AO�Az�A|�A��A{A�;A�hA ^5@�V@���@�7L@���@�@�ff@���@��@�t�@�K�@�;d@�@���@��#@��@�@�@�l�@�^5@�^5@���@�&�@�A�@�b@�1@�1@�dZ@�5?@�&�@�P@�@�@�o@��@�b@��H@�@�V@�z�@�o@��T@��
@ղ-@�ƨ@��@�5?@���@���@���@�
=@�E�@�x�@���@̬@�9X@�=q@���@��@���@Ų-@��@��;@°!@�-@���@�/@��F@�n�@��@�7L@�A�@���@�hs@�V@�r�@��y@��h@���@�9X@��;@��P@�+@���@��7@��@�A�@��@�dZ@���@��@��-@�O�@�%@��u@��
@��R@�n�@�=q@�@���@�z�@��@�\)@��@��@�o@���@�X@���@���@���@�bN@�b@��@��F@���@���@�;d@�
=@�@���@���@���@��!@�^5@��h@�X@��@���@�V@���@��@��m@���@�"�@�@�"�@�ff@���@�X@��@���@���@�Z@�1'@�  @��
@�1@� �@�o@�^5@�@�&�@��u@�r�@��
@��w@�l�@�@��R@�v�@�@���@�`B@��`@���@��j@��9@��@��@�b@�  @�  @��F@���@���@���@���@���@���@��F@��@���@��H@�o@���@�M�@��@��+@���@�`B@��@���@��`@���@�7L@���@��-@��@��-@�O�@��@���@�Ĝ@�Ĝ@�Ĝ@��9@���@�bN@�9X@� �@�b@�b@�  @�  @��w@�
=@�ff@�E�@��T@�=q@���@���@�n�@�@�hs@���@�%@���@�z�@�bN@��@��
@��
@��P@�l�@�dZ@�K�@�C�@��@��y@���@��+@�~�@�ff@�{@�@���@�p�@�G�@�V@���@���@�j@�A�@�(�@��@�b@��;@���@�K�@��@�@���@�5?@���@��T@���@�?}@��@���@���@���@��9@�bN@��@��
@��
@���@���@���@�|�@�dZ@�C�@��H@���@�ff@�$�@��@��#@��#@�@��@�O�@�7L@���@���@��9@���@��u@�z�@�Q�@��@�  @��@|�@
=@~��@~v�@~V@~$�@}��@}�@|��@|��@|(�@{�
@{��@{C�@{"�@z�@z��@zn�@y�#@y��@yx�@y&�@x�`@x�@w�@w�w@w�P@w;d@vȴ@v��@vv�@v5?@u�h@u�@tz�@t1@sƨ@so@r~�@q��@q7L@pA�@o�@oK�@n�+@n5?@m��@m�@m?}@l��@l�j@l(�@kt�@k"�@j�\@j-@i��@i��@ihs@h�`@hQ�@hb@g�@g�w@g|�@gK�@f��@f��@f@ep�@d�@d�/@d�@d�@d�@d�@d�@c�
@cƨ@c�F@c��@b��@ax�@a7L@a�@a%@`bN@_�@_�;@_��@_�P@^��@^ȴ@^�R@^v�@^5?@^$�@]�T@]p�@]O�@]V@\�@\Z@\1@[�
@[��@Z�H@ZM�@Y�@Y��@Y�^@Yx�@XĜ@X�u@XbN@Xb@W��@W�P@WK�@Vff@U�@U�h@UV@TI�@S�
@S33@R��@R��@R��@R�!@R�\@RJ@Q��@QG�@P��@PA�@O�w@O+@N��@N��@N��@Nȴ@N��@NE�@M��@M�h@M�@L�j@L�@Lz�@L(�@K��@K�@Kt�@Kt�@KS�@J��@JM�@JJ@I��@I7L@I%@H�9@H��@HbN@Hb@G��@G��@Gl�@G�@F��@Fv�@Fff@F$�@E�T@E@E�h@Ep�@E/@EV@D��@D��@D�@D��@Dz�@D�@C�m@C�F@CC�@B��@B~�@B=q@A��@A�@A��@AX@@�9@@�@@A�@@  @?��@?�w@?��@?+@?
=@>ȴ@>��@>ff@>5?@=�@=�-@=p�@=`B@=/@=V@<�@<��@<(�@;�m@;t�@;33@;"�@;@:��@:n�@:�@9�^@9x�@9G�@9&�@8�u@8  @7�@7��@7��@7\)@7
=@6�@6ȴ@6�R@6��@6V@6$�@6{@5�T@5��@5`B@5/@4��@4�/@4z�@41@3�F@3�@3o@2��@2n�@2-@1�#@1X@1&�@1%@0Ĝ@0�u@0�@0bN@0 �@/�;@/�w@/\)@/+@.�@.��@.E�@.5?@.$�@.{@.@-@-p�@-?}@-?}@-V@,�@,�j@,��@,�D@,Z@,�@+�F@+��@+dZ@+33@+o@*�!@*��@*~�@*^5@*J@)��@)��@)X@)7L@)�@(��@(r�@(A�@( �@'�;@'�w@'|�@';d@'�@&�@&�R@&��@&�+@&{@%�h@%`B@%O�@%/@%V@$�@$��@$Z@#��@#�F@#C�@"�@"�H@"��@"�!@"^5@!�#@!�7@!X@!7L@ ��@ �@ A�@   @�@l�@�@�y@��@ff@$�@�@��@/@�@�j@��@�D@j@(�@�m@��@t�@33@"�@@��@n�@-@�^@��@�7@X@��@��@Ĝ@Ĝ@�u@�@Q�@�@��@�w@��@��@|�@l�@+@�y@�y@�y@�@�R@ff@E�@$�@{@�T@��@p�@?}@V@�@�j@�@z�@(�@�
@��@��@t�@dZ@S�@C�@33@�H@��@~�@^5@�@�^@�7@hs@7L@��@�`@��@��@�9@��@�@Q�@1'@�@�;@�;@�@�P@l�@;d@�@�y@��@��@�+@E�@{@@`B@?}@?}@/@�@��@Z@(�@�m@��@�@t�@dZ@C�@"�@
�H@
�H@
��@
�!@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�S�A�K�A�S�A�XA�^5A�ZA�XA�\)A�bNA�dZA�dZA�dZA�bNA�dZA�ffA�jA�jA�hsA�l�A�p�A�r�A�r�A�x�A�z�A�z�A�r�A�VA�;dA�5?A�(�A��A�VA���A��yA���A���A���A���A��
A��#A��;A���A���A�ƨA���A��wA��-A��A���A���A���A���A��hA��7A�|�A�r�A�n�A�`BA�S�A�I�A�  A�S�A��A�x�A��!A�p�A�A��-A��RA��mA�ƨA��A��mA���A�{A�%A��A��A�jA�{A��HA���A�XA��;A�=qA�1'A�A�A��^A��+A���A�Q�A�|�A�ZA�  A��A���A�\)A�O�A�|�A��`A�ĜA��A�dZA�p�A�JA�t�A�ƨA��/A��A���A���A�1'A��+A�ĜA���A��A��!A��TA�ȴA�;dA���A��A�TA��A~{A{l�Ay�-Aw�Ar�uAq33Ap-Al�Aj��Af�RAd��Aal�A_��A\5?AY��AY��AYx�AX�/AW�#AV�9ATbAR��AQ��AO|�AL�jAKp�AH�HAE�#ADM�AAG�A?�wA?�wA?��A<�A;K�A9A8ZA7�A6�jA6A57LA2��A1x�A1%A0ȴA0A�A/?}A.1A-33A,~�A+/A)ƨA($�A&�`A%p�A#G�A"bA!l�A Q�A�`AA
=A�A�A��AoA�A��AjA5?A�AVA��A(�A��A+A��A�`A-A�-A"�A��A�+AA�A5?A
�A	�
A	%A�\A��AG�A�!A��AO�Az�A|�A��A{A�;A�hA ^5@�V@���@�7L@���@�@�ff@���@��@�t�@�K�@�;d@�@���@��#@��@�@�@�l�@�^5@�^5@���@�&�@�A�@�b@�1@�1@�dZ@�5?@�&�@�P@�@�@�o@��@�b@��H@�@�V@�z�@�o@��T@��
@ղ-@�ƨ@��@�5?@���@���@���@�
=@�E�@�x�@���@̬@�9X@�=q@���@��@���@Ų-@��@��;@°!@�-@���@�/@��F@�n�@��@�7L@�A�@���@�hs@�V@�r�@��y@��h@���@�9X@��;@��P@�+@���@��7@��@�A�@��@�dZ@���@��@��-@�O�@�%@��u@��
@��R@�n�@�=q@�@���@�z�@��@�\)@��@��@�o@���@�X@���@���@���@�bN@�b@��@��F@���@���@�;d@�
=@�@���@���@���@��!@�^5@��h@�X@��@���@�V@���@��@��m@���@�"�@�@�"�@�ff@���@�X@��@���@���@�Z@�1'@�  @��
@�1@� �@�o@�^5@�@�&�@��u@�r�@��
@��w@�l�@�@��R@�v�@�@���@�`B@��`@���@��j@��9@��@��@�b@�  @�  @��F@���@���@���@���@���@���@��F@��@���@��H@�o@���@�M�@��@��+@���@�`B@��@���@��`@���@�7L@���@��-@��@��-@�O�@��@���@�Ĝ@�Ĝ@�Ĝ@��9@���@�bN@�9X@� �@�b@�b@�  @�  @��w@�
=@�ff@�E�@��T@�=q@���@���@�n�@�@�hs@���@�%@���@�z�@�bN@��@��
@��
@��P@�l�@�dZ@�K�@�C�@��@��y@���@��+@�~�@�ff@�{@�@���@�p�@�G�@�V@���@���@�j@�A�@�(�@��@�b@��;@���@�K�@��@�@���@�5?@���@��T@���@�?}@��@���@���@���@��9@�bN@��@��
@��
@���@���@���@�|�@�dZ@�C�@��H@���@�ff@�$�@��@��#@��#@�@��@�O�@�7L@���@���@��9@���@��u@�z�@�Q�@��@�  @��@|�@
=@~��@~v�@~V@~$�@}��@}�@|��@|��@|(�@{�
@{��@{C�@{"�@z�@z��@zn�@y�#@y��@yx�@y&�@x�`@x�@w�@w�w@w�P@w;d@vȴ@v��@vv�@v5?@u�h@u�@tz�@t1@sƨ@so@r~�@q��@q7L@pA�@o�@oK�@n�+@n5?@m��@m�@m?}@l��@l�j@l(�@kt�@k"�@j�\@j-@i��@i��@ihs@h�`@hQ�@hb@g�@g�w@g|�@gK�@f��@f��@f@ep�@d�@d�/@d�@d�@d�@d�@d�@c�
@cƨ@c�F@c��@b��@ax�@a7L@a�@a%@`bN@_�@_�;@_��@_�P@^��@^ȴ@^�R@^v�@^5?@^$�@]�T@]p�@]O�@]V@\�@\Z@\1@[�
@[��@Z�H@ZM�@Y�@Y��@Y�^@Yx�@XĜ@X�u@XbN@Xb@W��@W�P@WK�@Vff@U�@U�h@UV@TI�@S�
@S33@R��@R��@R��@R�!@R�\@RJ@Q��@QG�@P��@PA�@O�w@O+@N��@N��@N��@Nȴ@N��@NE�@M��@M�h@M�@L�j@L�@Lz�@L(�@K��@K�@Kt�@Kt�@KS�@J��@JM�@JJ@I��@I7L@I%@H�9@H��@HbN@Hb@G��@G��@Gl�@G�@F��@Fv�@Fff@F$�@E�T@E@E�h@Ep�@E/@EV@D��@D��@D�@D��@Dz�@D�@C�m@C�F@CC�@B��@B~�@B=q@A��@A�@A��@AX@@�9@@�@@A�@@  @?��@?�w@?��@?+@?
=@>ȴ@>��@>ff@>5?@=�@=�-@=p�@=`B@=/@=V@<�@<��@<(�@;�m@;t�@;33@;"�@;@:��@:n�@:�@9�^@9x�@9G�@9&�@8�u@8  @7�@7��@7��@7\)@7
=@6�@6ȴ@6�R@6��@6V@6$�@6{@5�T@5��@5`B@5/@4��@4�/@4z�@41@3�F@3�@3o@2��@2n�@2-@1�#@1X@1&�@1%@0Ĝ@0�u@0�@0bN@0 �@/�;@/�w@/\)@/+@.�@.��@.E�@.5?@.$�@.{@.@-@-p�@-?}@-?}@-V@,�@,�j@,��@,�D@,Z@,�@+�F@+��@+dZ@+33@+o@*�!@*��@*~�@*^5@*J@)��@)��@)X@)7L@)�@(��@(r�@(A�@( �@'�;@'�w@'|�@';d@'�@&�@&�R@&��@&�+@&{@%�h@%`B@%O�@%/@%V@$�@$��@$Z@#��@#�F@#C�@"�@"�H@"��@"�!@"^5@!�#@!�7@!X@!7L@ ��@ �@ A�@   @�@l�@�@�y@��@ff@$�@�@��@/@�@�j@��@�D@j@(�@�m@��@t�@33@"�@@��@n�@-@�^@��@�7@X@��@��@Ĝ@Ĝ@�u@�@Q�@�@��@�w@��@��@|�@l�@+@�y@�y@�y@�@�R@ff@E�@$�@{@�T@��@p�@?}@V@�@�j@�@z�@(�@�
@��@��@t�@dZ@S�@C�@33@�H@��@~�@^5@�@�^@�7@hs@7L@��@�`@��@��@�9@��@�@Q�@1'@�@�;@�;@�@�P@l�@;d@�@�y@��@��@�+@E�@{@@`B@?}@?}@/@�@��@Z@(�@�m@��@�@t�@dZ@C�@"�@
�H@
�H@
��@
�!@
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BuBuB{B{B�B�B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B49BaHB~�B�DB��B��B�B�!B�B�!B�-B�?B�XB�jB�qB�wB�wB�}B��BƨBȴB��B��B��B��B��B��B��B��B��B��B��B�B�
B�)B�TB�;B�5B�/B�;B�HB�TB�NB�HB�TB�B�B��BJB+BJ�BS�BF�B%�B�Bn�B0!BPB�By�B�B%�B,B�B�BB�B�B�B�`B��BɺB�dB�hB�B�B�B�B~�Bm�Be`B]/BN�BD�B9XB�BVB
��B
�5B
��B
��B
�?B
��B
�uB
y�B
]/B
>wB
5?B
9XB
.B
�B
�B
B	�ZB	��B	ɺB	�B	��B	t�B	`BB	G�B	9XB	 �B	DB	1B	1B	B��B��B	\B		7B	B��B�TB�
BȴB�-B��B��B�PB�JB�DB�B}�Bv�Bt�Bn�Bn�Bl�Bn�Bl�BjBhsBhsBgmBe`BffBcTBe`BcTB`BBZBW
BS�BQ�BJ�BI�BJ�BH�BG�BF�BD�BF�BA�B?}B>wB>wB=qB=qB<jB<jB:^B9XB8RB6FB6FB49B2-B1'B0!B/B/B1'B/B0!B0!B/B.B-B-B,B,B+B+B+B)�B+B)�B)�B)�B+B-B-B-B/B/B/B/B1'B2-B2-B2-B2-B2-B33B33B0!B/B1'B49B6FB8RB:^B;dB;dB;dB:^B;dB;dB;dB<jB9XB5?B2-B1'B1'B/B/B.B-B/B.B1'B0!B2-B33B49B49B49B7LB8RB9XB;dB;dB;dB<jBA�BD�BE�BF�BG�BH�BK�BL�BP�BT�BW
BYBZBZB[#B^5BdZBhsBiyBk�Bl�Bs�Bt�Bv�Bx�Bz�B{�B|�B�B�B�B�B�B�1B�DB�VB�bB�hB�uB��B��B��B��B��B��B��B�B�!B�'B�'B�'B�'B�jB�qB�}BBǮBɺB��B��B��B��B��B��B��B��B��B�B�B�)B�HB�`B�sB�B�B�B��B��B��B	%B	
=B	\B	hB	oB	{B	�B	�B	�B	�B	�B	�B	#�B	'�B	-B	/B	2-B	33B	6FB	6FB	:^B	?}B	B�B	D�B	G�B	I�B	J�B	N�B	P�B	Q�B	R�B	T�B	VB	W
B	XB	ZB	^5B	_;B	aHB	dZB	ffB	gmB	hsB	hsB	iyB	jB	k�B	m�B	p�B	s�B	x�B	{�B	}�B	�B	�+B	�B	�B	�B	�%B	�+B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�-B	�-B	�3B	�FB	�dB	�jB	�qB	�}B	�}B	�}B	��B	��B	��B	ÖB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�ZB	�ZB	�`B	�fB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
	7B
DB
JB
JB
PB
PB
PB
VB
VB
VB
VB
\B
\B
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
"�B
#�B
#�B
#�B
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
%�B
%�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
/B
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
33B
33B
33B
33B
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
7LB
7LB
7LB
7LB
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
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
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
F�B
F�B
F�B
F�B
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
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
M�B
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
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
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
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
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
ffB
ffB
ffB
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
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
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
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@B@BFBFBSBMBFBFBMBMBMBMBMBMBSBYBSBMBYBkBqBkBxB~B$�B4BaB~�B�B�_B��B��B��B��B��B��B�B�$B�6B�<B�BB�BB�HB�OB�tBȀBʌB˒BΥBϫBбBѷBҽBҽBҽB��B��B��B��B��B� B�B�B��B�B�B� B�B�B� B�KB�vB��BB*�BJ�BS�BFtB%�B��BncB/�BBSBy�BMB%�B+�B�BSB �B�|B�oB�QB�,BѷBɆB�0B�4B��B��B��B��B~�Bm]Be,B\�BN�BDgB9$BxB"B
��B
�B
͟B
�4B
�B
��B
�@B
y�B
\�B
>BB
5B
9$B
-�B
MB
MB
�B	�&B	ҽB	�lB	��B	�_B	tnB	`B	G_B	9$B	 �B	B	�B	�B	�B��B��B	(B	�B	�B�nB�B��BȀB��B��B�]B�B��B��B��B}�Bv�Bt�BnIBnIBl=BncBlWBj0Bh$Bh$Bg8Be,BfBcBeBcB_�BY�BV�BS�BQ�BJ�BIlBJ�BHfBG_BFtBDMBFYBA;B?HB>(B>BB=<B="B<B<B:B9$B8B5�B5�B3�B1�B0�B/�B.�B.�B0�B.�B/�B/�B.�B-�B,�B,�B+�B+�B*�B*�B*�B)�B*�B)�B)�B)�B*�B,�B,�B,�B.�B.�B.�B.�B0�B1�B1�B1�B1�B1�B2�B2�B/�B.�B0�B3�B5�B8B:B;B;0B;B:*B;B;B;B<6B9	B4�B1�B0�B0�B.�B.�B-�B,�B.�B-�B0�B/�B1�B2�B4B4B3�B6�B8B9$B;B;B;B<BA;BDgBESBFYBG_BHfBK�BL~BP�BT�BV�BX�BY�BY�BZ�B]�BdBh$Bi*Bk6Bl=BshBt�BvzBx�Bz�B{�B|�B��B��B��B��B��B��B��B�"B�B�4B�&B�9B�dB�jB�pB��B��B��B��B��B��B��B��B��B�B�<B�.B�AB�_B�lB�rB�~B�~B�~BΥBЗBЗBѝBԯB��B��B��B��B�B�$B�CB�[B�aB�tB��B��B	�B		�B	B	B	 B	,B	EB	KB	WB	]B	�B	pB	#�B	'�B	,�B	.�B	1�B	2�B	5�B	5�B	:B	?.B	BAB	DMB	G_B	IlB	JrB	N�B	P�B	Q�B	R�B	T�B	U�B	V�B	W�B	Y�B	]�B	^�B	`�B	d&B	fB	gB	h$B	h$B	i*B	j0B	k6B	mCB	pUB	s�B	x�B	{�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�@B	�2B	�KB	�WB	�dB	�dB	�jB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�.B	�.B	�.B	�4B	�4B	�;B	�GB	�YB	�YB	�lB	�~B	̈́B	̈́B	̈́B	ΊB	ϑB	ЗB	ЗB	ңB	ҽB	ҽB	��B	յB	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�*B	�*B	�*B	�*B	�0B	�0B	�0B	�6B	�=B	�CB	�CB	�IB	�cB	�OB	�OB	�OB	�vB	�aB	�aB	�aB	�|B	�|B	�aB	�aB	�hB	�hB	�hB	�nB	�nB	�nB	�nB	�nB	�nB	��B	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

�B
�B
�B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
&B
&B
&B
&B
,B
,B
,B
,B
2B
2B
2B
9B
?B
?B
_B
EB
+B
EB
EB
_B
KB
KB
KB
eB
KB
QB
WB
]B
xB
WB
dB
dB
dB
dB
dB
�B
�B
jB
jB
�B
jB
pB
�B
 vB
 vB
 �B
 vB
 vB
!|B
!|B
"�B
#�B
#�B
#�B
#�B
#�B
$tB
$tB
$tB
$�B
%zB
%�B
%�B
%�B
%�B
%zB
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
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
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4�B
4�B
5B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
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
:B
:B
:B
:*B
:B
;B
;B
;0B
<B
<B
<6B
<B
=<B
="B
=<B
="B
=<B
=<B
>(B
>(B
>BB
>(B
?.B
?.B
?.B
?.B
@OB
@4B
@4B
A;B
@B
@4B
A;B
A;B
A;B
A;B
BAB
BAB
BAB
CGB
CGB
CGB
CGB
CGB
DgB
DMB
DMB
D3B
DMB
DMB
ESB
ESB
ESB
ESB
ESB
F?B
F?B
F?B
FYB
FYB
FYB
GzB
GzB
GzB
G_B
HfB
HfB
HKB
HfB
HfB
HfB
IlB
IlB
IlB
IlB
JrB
JrB
J�B
JrB
JrB
KxB
KxB
KxB
KxB
KxB
KxB
KxB
L~B
L~B
L~B
L~B
L~B
L�B
M�B
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
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
P�B
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
S�B
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
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
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
]�B
]�B
]�B
]�B
^B
_B
_B
^�B
_�B
_�B
_�B
_�B
_�B
`B
`�B
`�B
`�B
aB
aB
`�B
a�B
bB
a�B
a�B
a�B
bB
a�B
cB
cB
cB
cB
cB
cB
c B
cB
c B
dB
dB
dB
dB
dB
c�B
e,B
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
f2B
gB
gB
gB
gB
gB
h$B
h$B
h>B
h
B
h$B
h$B
iDB
i*B
i*B
i*B
i*B
i*B
i*B
i*B
i*B
iB
jKB
j0B
j0B
j0B
j0B
j0B
k6B
k6B
k6B
kQB
kQB
k6B
l=B
lWB
l=B
l=B
l=B
mCB
mCB
mCB
mCB
nIB
mCB
ncB
nIB
nIB
nIB
nIB
nIB
ncB
nIB
oi1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.59(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902080035392019020800353920190208003539201902090025552019020900255520190209002555JA  ARFMdecpA19c                                                                20190203041723  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190202191952  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190202191953  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190202191953  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190202191954  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190202191954  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190202191954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190202191954  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190202191955  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190202191955                      G�O�G�O�G�O�                JA  ARUP                                                                        20190202215745                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190203154022  CV  JULD            G�O�G�O�F�%=                JM  ARCAJMQC2.0                                                                 20190207153539  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190207153539  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190208152555  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021524                      G�O�G�O�G�O�                