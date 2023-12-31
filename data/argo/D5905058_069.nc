CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-21T09:35:23Z creation;2018-06-21T09:35:26Z conversion to V3.1;2019-12-23T06:19:56Z update;     
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
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܘ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20180621093523  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  I2_0675_069                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�lI��1   @�l��J @8�Ov_��c(1&�y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
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
C&��C(�
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
C�K�C�K�C�K�C�K�C�>�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
�)D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D,)D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9�\D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>�)D?,)D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{%�D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�R�DĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�DҒ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�R�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D���D���D�D�R�D���D���D��D�R�D���D���D��D�R�D���D��D�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A���A�VA�ȴA���AÍPA�~�A�JA��A�K�A�
=A�ĜA��\A�(�A�+A��jA�n�A��^A�Q�A� �A��9A��A�I�A��PA��A���A���A�?}A��PA�`BA�Q�A�=qA���A��`A��A���A���A���A�r�A��A��DA�5?A�-A�A�A���A��DA��A�\)A�1A���A�bNA�l�A��A�`BA�33A�VA��A�`BA��hA�I�A���A�p�A�=qA��\A��HA��A��A���A�A��TA��jA��7A�1A��
A���A�VA��/A���A�Q�A��!A��A���A�~�A�%A�`BA��jA��#A��A�C�A��A�%A�%A���A��+A�7LA��\A��!A�A�A�XA��A�9XA��A�33A}�;A{�AyAx=qAw|�AvffAu`BAu+AtJAr�DAq|�Ao��AkAj�jAg�mAd��AbZA`ĜA_�A^�A]�A[
=AY�PAYAX �AU��AT�jAS|�AR�\AQ�
AQx�AO��AMp�ALI�AK��AIAJAI��AI/AH�+AGt�AE��AC�mABr�AAx�A?�-A>��A>jA=��A;��A:n�A8^5A7+A6bNA5�#A5&�A4$�A3|�A2��A2�A1`BA0�!A/K�A.ȴA.^5A,��A*1'A)p�A)"�A(VA'�-A&�`A%�PA$Q�A#VA"bA!|�A!�A   AhsA"�An�A?}A�A�A�AA�A$�AAt�A\)A;dAĜA1A�PA`BA&�A�A��A/A��A(�A|�Ar�A��A
v�A	x�A��Av�AZA�A+A�mAl�A��A1'AO�AĜA �A�A r�A  �@���@�v�@�O�@�9X@�l�@��!@��^@�(�@���@�=q@��@��j@��@��@�J@��@�  @�ȴ@�Q�@�|�@�33@�=q@���@�P@���@��@��@�A�@�V@��`@�(�@�o@�X@�"�@�@��@�r�@׶F@և+@�`B@�Z@�33@ҧ�@�@У�@�\)@�V@ͩ�@�?}@�bN@�^5@ə�@�-@��@�hs@�b@���@�V@�x�@���@ă@�  @��@�E�@��^@�7L@���@�  @�
=@��7@���@�9X@�33@��@�`B@��`@��@��F@�C�@�E�@���@���@��@���@���@�V@�(�@�5?@��7@��@�j@�j@��@��@�^5@��@���@��@���@�ƨ@���@�\)@��@�$�@�7L@�%@��j@�(�@��m@��@���@�ȴ@���@���@��H@��R@��+@�M�@��@�E�@�E�@�`B@��j@�Ĝ@�Ĝ@���@��@��m@��w@�"�@��H@�~�@��@���@�p�@�X@�X@�O�@��@�Z@��m@���@�l�@�K�@�+@�
=@��y@��H@��@���@���@�v�@�n�@�ff@�ff@�^5@�V@��#@��^@�$�@�-@���@���@���@��7@�O�@���@�b@��@��
@�C�@��H@�^5@�G�@��D@�I�@�b@��y@���@���@���@��@���@���@�~�@�=q@���@�G�@��`@���@��j@��`@���@���@��w@���@��@�+@�{@��@��^@���@��-@�{@�J@���@���@�x�@�X@�X@�X@���@�Ĝ@���@���@�Z@�1@���@��@�33@��y@��+@�-@�@���@���@��^@���@��h@�x�@�/@��`@���@��j@���@��@���@�9X@��D@��@�@���@���@���@��@���@��j@�z�@�j@�r�@�bN@�I�@��;@�C�@��@��y@��@�ȴ@���@���@�ȴ@�n�@��@���@�@�-@�J@���@��^@�p�@��@��`@���@���@���@���@�Ĝ@��9@���@��D@�I�@��@�1@���@��;@�|�@�@�n�@��@�%@��^@�{@�{@��@��#@���@�hs@�7L@��@�V@�V@���@�V@��/@���@�j@�(�@�  @�w@�P@K�@�@~�y@~�+@}�-@}��@}�h@}?}@|j@|�@{��@z=q@y&�@yhs@y��@xA�@w��@w\)@v�R@v�+@v{@u�h@u?}@t�/@t9X@s�
@s�F@s��@sdZ@s"�@s"�@r-@p��@p�9@pr�@p�u@o�;@o|�@o|�@o\)@o��@o�;@o�;@o�@ol�@oK�@o
=@nȴ@nv�@n{@m`B@lj@k33@j~�@j^5@i��@h�@h��@h��@i7L@i&�@h��@hA�@f��@e�h@e��@e�T@ep�@d�/@d�@d�@d�@d��@c�F@co@b�\@b�@a��@a�7@a�7@ahs@aX@a7L@`Ĝ@`  @^��@]�T@]`B@]V@\�/@\�@\9X@[��@["�@Z��@Z�!@Z~�@Z=q@ZJ@Y�#@YX@Y%@XĜ@X1'@W�;@Wl�@W;d@W�@V�R@Vv�@VV@V{@U��@UO�@U�@T��@T�j@T�D@Tj@T(�@S��@S�F@St�@SS�@S33@R�@Rn�@Q�#@QX@Q7L@P��@Pr�@P1'@O�@O\)@N�R@N��@Nff@NE�@N{@M?}@L�@L��@LI�@K�
@J�H@J��@J��@JJ@Ix�@Ihs@I7L@H�u@HA�@H1'@H �@H  @G�P@G�@F��@F@E�h@E�@E`B@E?}@E/@D�@D9X@C��@CdZ@CS�@CS�@C33@C@B^5@A�^@Ahs@A�@@�`@@r�@?�@?��@?�w@?�@?��@?\)@?�@>��@>��@>��@>��@=�@=O�@=V@<��@<�/@<�j@<�@:��@:�\@:=q@9��@9x�@9x�@9�7@9�7@9x�@9X@9&�@8��@8A�@7�;@7�;@7�;@8  @7�@7+@6��@6v�@6v�@6v�@6ff@6E�@5�@5�-@5/@4��@4�j@4�@4�@4��@4I�@3��@3t�@2�@2��@2�\@2~�@2^5@2=q@2�@1�@1��@1�7@1G�@1&�@1%@0�`@0�9@0r�@0bN@0A�@/�;@/�@.�R@.V@.E�@.@-�@-�T@-��@-�-@-`B@-/@,�@,�/@,�@,�/@,��@,Z@+�
@+ƨ@+ƨ@+ƨ@+ƨ@+ƨ@+ƨ@+�@+S�@+33@*�@*��@*~�@*M�@*�@)��@)&�@)X@)%@(�u@( �@'l�@&��@&ȴ@&��@&��@&�y@&��@&�@&��@&v�@&E�@&{@%��@%�@%`B@$��@$�@$j@$Z@$9X@$�@#��@#33@"�H@"^5@!��@!��@!�7@!7L@!�@!%@ ��@ ��@ �@  �@   @�;@�@��@�P@K�@�y@�R@�+@ff@V@5?@@�T@@p�@V@�/@�j@�@�D@Z@��@ƨ@S�@33@33@@�H@��@�!@~�@^5@=q@J@��@�@�#@�7@�@%@%@%@��@��@�`@��@�u@bN@b@�w@��@�P@|�@+@ȴ@�R@v�@ff@$�@�h@`B@O�@/@V@�/@�@j@(�@�@ƨ@��@��@�F@ƨ@��@�@��@�\@^5@-@�@=q@��@X@�@��@Ĝ@bN@1'@1'@�@��@�;@��@�w@��@\)@�y@ȴ@�R@�R@��@��@�+@5?@��@�@V@��@/@��@�@��@�@��@�D@z�@z�@j@9X@(�@�F@C�@
��@
��@
~�@
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A���A�VA�ȴA���AÍPA�~�A�JA��A�K�A�
=A�ĜA��\A�(�A�+A��jA�n�A��^A�Q�A� �A��9A��A�I�A��PA��A���A���A�?}A��PA�`BA�Q�A�=qA���A��`A��A���A���A���A�r�A��A��DA�5?A�-A�A�A���A��DA��A�\)A�1A���A�bNA�l�A��A�`BA�33A�VA��A�`BA��hA�I�A���A�p�A�=qA��\A��HA��A��A���A�A��TA��jA��7A�1A��
A���A�VA��/A���A�Q�A��!A��A���A�~�A�%A�`BA��jA��#A��A�C�A��A�%A�%A���A��+A�7LA��\A��!A�A�A�XA��A�9XA��A�33A}�;A{�AyAx=qAw|�AvffAu`BAu+AtJAr�DAq|�Ao��AkAj�jAg�mAd��AbZA`ĜA_�A^�A]�A[
=AY�PAYAX �AU��AT�jAS|�AR�\AQ�
AQx�AO��AMp�ALI�AK��AIAJAI��AI/AH�+AGt�AE��AC�mABr�AAx�A?�-A>��A>jA=��A;��A:n�A8^5A7+A6bNA5�#A5&�A4$�A3|�A2��A2�A1`BA0�!A/K�A.ȴA.^5A,��A*1'A)p�A)"�A(VA'�-A&�`A%�PA$Q�A#VA"bA!|�A!�A   AhsA"�An�A?}A�A�A�AA�A$�AAt�A\)A;dAĜA1A�PA`BA&�A�A��A/A��A(�A|�Ar�A��A
v�A	x�A��Av�AZA�A+A�mAl�A��A1'AO�AĜA �A�A r�A  �@���@�v�@�O�@�9X@�l�@��!@��^@�(�@���@�=q@��@��j@��@��@�J@��@�  @�ȴ@�Q�@�|�@�33@�=q@���@�P@���@��@��@�A�@�V@��`@�(�@�o@�X@�"�@�@��@�r�@׶F@և+@�`B@�Z@�33@ҧ�@�@У�@�\)@�V@ͩ�@�?}@�bN@�^5@ə�@�-@��@�hs@�b@���@�V@�x�@���@ă@�  @��@�E�@��^@�7L@���@�  @�
=@��7@���@�9X@�33@��@�`B@��`@��@��F@�C�@�E�@���@���@��@���@���@�V@�(�@�5?@��7@��@�j@�j@��@��@�^5@��@���@��@���@�ƨ@���@�\)@��@�$�@�7L@�%@��j@�(�@��m@��@���@�ȴ@���@���@��H@��R@��+@�M�@��@�E�@�E�@�`B@��j@�Ĝ@�Ĝ@���@��@��m@��w@�"�@��H@�~�@��@���@�p�@�X@�X@�O�@��@�Z@��m@���@�l�@�K�@�+@�
=@��y@��H@��@���@���@�v�@�n�@�ff@�ff@�^5@�V@��#@��^@�$�@�-@���@���@���@��7@�O�@���@�b@��@��
@�C�@��H@�^5@�G�@��D@�I�@�b@��y@���@���@���@��@���@���@�~�@�=q@���@�G�@��`@���@��j@��`@���@���@��w@���@��@�+@�{@��@��^@���@��-@�{@�J@���@���@�x�@�X@�X@�X@���@�Ĝ@���@���@�Z@�1@���@��@�33@��y@��+@�-@�@���@���@��^@���@��h@�x�@�/@��`@���@��j@���@��@���@�9X@��D@��@�@���@���@���@��@���@��j@�z�@�j@�r�@�bN@�I�@��;@�C�@��@��y@��@�ȴ@���@���@�ȴ@�n�@��@���@�@�-@�J@���@��^@�p�@��@��`@���@���@���@���@�Ĝ@��9@���@��D@�I�@��@�1@���@��;@�|�@�@�n�@��@�%@��^@�{@�{@��@��#@���@�hs@�7L@��@�V@�V@���@�V@��/@���@�j@�(�@�  @�w@�P@K�@�@~�y@~�+@}�-@}��@}�h@}?}@|j@|�@{��@z=q@y&�@yhs@y��@xA�@w��@w\)@v�R@v�+@v{@u�h@u?}@t�/@t9X@s�
@s�F@s��@sdZ@s"�@s"�@r-@p��@p�9@pr�@p�u@o�;@o|�@o|�@o\)@o��@o�;@o�;@o�@ol�@oK�@o
=@nȴ@nv�@n{@m`B@lj@k33@j~�@j^5@i��@h�@h��@h��@i7L@i&�@h��@hA�@f��@e�h@e��@e�T@ep�@d�/@d�@d�@d�@d��@c�F@co@b�\@b�@a��@a�7@a�7@ahs@aX@a7L@`Ĝ@`  @^��@]�T@]`B@]V@\�/@\�@\9X@[��@["�@Z��@Z�!@Z~�@Z=q@ZJ@Y�#@YX@Y%@XĜ@X1'@W�;@Wl�@W;d@W�@V�R@Vv�@VV@V{@U��@UO�@U�@T��@T�j@T�D@Tj@T(�@S��@S�F@St�@SS�@S33@R�@Rn�@Q�#@QX@Q7L@P��@Pr�@P1'@O�@O\)@N�R@N��@Nff@NE�@N{@M?}@L�@L��@LI�@K�
@J�H@J��@J��@JJ@Ix�@Ihs@I7L@H�u@HA�@H1'@H �@H  @G�P@G�@F��@F@E�h@E�@E`B@E?}@E/@D�@D9X@C��@CdZ@CS�@CS�@C33@C@B^5@A�^@Ahs@A�@@�`@@r�@?�@?��@?�w@?�@?��@?\)@?�@>��@>��@>��@>��@=�@=O�@=V@<��@<�/@<�j@<�@:��@:�\@:=q@9��@9x�@9x�@9�7@9�7@9x�@9X@9&�@8��@8A�@7�;@7�;@7�;@8  @7�@7+@6��@6v�@6v�@6v�@6ff@6E�@5�@5�-@5/@4��@4�j@4�@4�@4��@4I�@3��@3t�@2�@2��@2�\@2~�@2^5@2=q@2�@1�@1��@1�7@1G�@1&�@1%@0�`@0�9@0r�@0bN@0A�@/�;@/�@.�R@.V@.E�@.@-�@-�T@-��@-�-@-`B@-/@,�@,�/@,�@,�/@,��@,Z@+�
@+ƨ@+ƨ@+ƨ@+ƨ@+ƨ@+ƨ@+�@+S�@+33@*�@*��@*~�@*M�@*�@)��@)&�@)X@)%@(�u@( �@'l�@&��@&ȴ@&��@&��@&�y@&��@&�@&��@&v�@&E�@&{@%��@%�@%`B@$��@$�@$j@$Z@$9X@$�@#��@#33@"�H@"^5@!��@!��@!�7@!7L@!�@!%@ ��@ ��@ �@  �@   @�;@�@��@�P@K�@�y@�R@�+@ff@V@5?@@�T@@p�@V@�/@�j@�@�D@Z@��@ƨ@S�@33@33@@�H@��@�!@~�@^5@=q@J@��@�@�#@�7@�@%@%@%@��@��@�`@��@�u@bN@b@�w@��@�P@|�@+@ȴ@�R@v�@ff@$�@�h@`B@O�@/@V@�/@�@j@(�@�@ƨ@��@��@�F@ƨ@��@�@��@�\@^5@-@�@=q@��@X@�@��@Ĝ@bN@1'@1'@�@��@�;@��@�w@��@\)@�y@ȴ@�R@�R@��@��@�+@5?@��@�@V@��@/@��@�@��@�@��@�D@z�@z�@j@9X@(�@�F@C�@
��@
��@
~�@
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B1'B(�B'�B+B33BE�Bk�B� B� B|�Bx�Bw�Bv�Bu�Bt�Bw�Bw�Bv�Bw�B|�B� B�B�JB�VB��B��B��B��B��B�B�!B�!B�B�B�9B�3B�!B�9B�3B�uB� B��B�FB�!B��B�{B�uB��B��B��B��B��B�+B|�Bm�BP�BR�BM�BD�B<jB$�B��B�TB��BƨBƨBǮB�}B��BÖB�B��B�%Bx�Bx�B�=B�B}�B{�B� Bq�BM�BN�BL�BF�B:^B-B �BVB
��B
�NB
��B
��B
��B
��B
��B
��B
�9B
��B
�\B
�B
{�B
x�B
n�B
XB
J�B
7LB
,B
%�B
�B
{B
uB
PB
B	��B	�B	��B	B	�'B	��B	�1B	x�B	s�B	k�B	cTB	YB	L�B	G�B	C�B	5?B	.B	'�B	!�B	�B	�B	�B	%B	B		7B��B	  B	B	  B��B��B�B�NB�B��B��BȴBĜB�wB�?B�B��B��B��B��B�uB�bB�PB�DB�+B�B�By�Bu�Bq�BjB`BBZBYBW
BYB]/B]/B[#BYBXBW
BVBW
BVBT�BVBS�BM�BM�BH�BD�BC�B?}B=qB=qB<jB;dB9XB9XB7LB5?B5?B49B0!B/B-B-B,B+B+B)�B(�B'�B'�B'�B&�B&�B&�B$�B$�B$�B#�B#�B"�B!�B#�B!�B!�B!�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B �B!�B"�B"�B"�B#�B#�B%�B$�B$�B$�B&�B%�B'�B%�B&�B&�B+B,B+B+B+B,B-B2-B33B33B7LB=qB=qB=qBB�BF�BG�BI�BK�BL�BP�BR�BT�BYB\)B^5B_;B_;B_;B_;Be`BiyBm�Br�Bv�By�Bz�Bz�By�By�By�Bz�B|�B{�Bz�By�B{�B|�B|�B� B�B�B�B�B�=B�1B�DB�\B�oB��B��B��B��B��B��B�B�3B�3B�?B�LB�RB�^B�jB�qBBǮB��B��B��B��B��B��B�
B�B�#B�5B�BB�BB�BB�TB�ZB�`B�ZB�sB�B�B�B�B�B��B��B��B��B	B	B	+B	DB	PB	\B	oB	�B	�B	�B	�B	�B	 �B	 �B	!�B	"�B	$�B	&�B	,B	.B	1'B	49B	49B	49B	6FB	7LB	<jB	A�B	B�B	C�B	E�B	H�B	J�B	I�B	K�B	O�B	M�B	Q�B	XB	ZB	^5B	bNB	ffB	ffB	hsB	m�B	r�B	t�B	x�B	}�B	�B	�+B	�JB	�PB	�bB	�oB	�hB	�bB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�!B	�3B	�9B	�?B	�LB	�^B	�dB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�`B	�fB	�fB	�`B	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
%B
+B
+B
1B

=B
DB
JB
JB
JB
JB
JB
JB
DB
DB

=B
	7B
1B
1B
+B
B
+B
	7B
DB
JB
PB
PB
PB
JB
VB
bB
oB
oB
oB
oB
oB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
)�B
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
-B
.B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
49B
49B
5?B
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
6FB
8RB
8RB
8RB
8RB
8RB
7LB
8RB
7LB
7LB
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
:^B
:^B
:^B
;dB
;dB
<jB
<jB
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
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
H�B
H�B
I�B
I�B
I�B
I�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
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
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
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
]/B
^5B
^5B
^5B
^5B
_;B
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
aHB
aHB
bNB
bNB
bNB
cTB
cTB
dZB
e`B
e`B
ffB
e`B
ffB
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
jB
jB
jB
jB
k�B
jB
jB
jB
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
l�B
m�B
l�B
k�B
k�B
l�B
l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B0�B(�B'�B*�B2�BEmBkQB�B�B|�Bx�Bw�Bv�Bu�Bt�Bw�Bw�Bv�Bw�B|�B�B��B�B�"B�MB�kB�kB�xB��B��B��B��B��B��B�B��B��B�B��B�@B�B��B�B��B��B�FB�@B��B��B��B�kB�YB��B|�Bm]BP�BR�BM�BDgB<6B$�B��B� BΥB�tB�tB�zB�.BϫB�GB��B�xB��Bx�Bx�B��B��B}�B{�B�BqvBM�BN�BL~BFtB:*B,�B �B"B
��B
��B
ΥB
ΊB
ΥB
ΥB
˒B
�UB
��B
��B
�(B
��B
{�B
x�B
ncB
W�B
J�B
7B
+�B
%�B
�B
FB
@B
B
 �B	��B	�=B	˒B	�[B	��B	�2B	��B	x�B	s�B	kQB	c B	X�B	L�B	GzB	CaB	4�B	-�B	'�B	!�B	jB	WB	MB	�B	�B	�B��B��B	�B��B��B�zB�=B��B��BөB�~B�fB�gB�BB��B��B��B�WB�EB�SB�@B�.B�B��B��B��B��By�ButBq[BjKB`BY�BX�BV�BX�B\�B\�BZ�BX�BW�BV�BU�BV�BU�BT�BU�BS�BM�BM�BH�BDMBCGB?HB="B=<B<B;B9$B9	B6�B4�B4�B3�B/�B.�B,�B,�B+�B*�B*�B)�B(�B'�B'�B'�B&�B&�B&�B$�B$�B$�B#�B#�B"�B!�B#�B!|B!|B!|B!|B!|B �B vBpBpBjBjBdBjBjB~B]BdBdBjB vB�BpB vB!|B"�B"�B"�B#�B#�B%�B$�B$�B$�B&�B%�B'�B%�B&�B&�B*�B+�B*�B*�B*�B+�B,�B1�B2�B2�B7B="B="B="BBABFYBG_BIlBKxBL~BP�BR�BT�BX�B[�B]�B_B^�B^�B^�Be,Bi*Bm]BraBvzBy�Bz�Bz�By�By�By�Bz�B|�B{�Bz�By�B{�B|�B|�B�B��B��B��B��B��B��B��B�B� B�9B�KB�]B��B�|B��B��B��B��B��B��B�B�B�B�<B�AB�_B�xB�~BΊBϑBбBөB��B��B��B��B��B��B��B�B�B�,B��B�$B�=B�IB�UB�[B�hB�nB��B��B��B	�B	�B	�B	
�B	B	B	 B	2B	?B	]B	jB	pB	 vB	 vB	!|B	"�B	$�B	&�B	+�B	-�B	0�B	3�B	3�B	3�B	5�B	6�B	<B	A;B	BAB	CGB	ESB	HfB	JrB	IlB	KxB	O�B	M�B	Q�B	W�B	Y�B	]�B	bB	fB	fB	h$B	mCB	r|B	tnB	x�B	}�B	��B	��B	��B	�B	�B	� B	�B	�B	�B	�B	�B	�&B	�?B	�QB	�dB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�0B	�B	�B	�B	�"B	�(B	�BB	�.B	�.B	�4B	�4B	�[B	�fB	�rB	�rB	�xB	ʌB	�xB	̈́B	ϫB	ѝB	ѝB	ѷB	ңB	ңB	ңB	ѝB	ѝB	ѝB	յB	��B	֡B	ּB	��B	ٴB	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�&B	�B	�B	�&B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�B	�$B	�]B	�CB	�IB	�IB	�IB	�OB	�OB	�UB	�UB	�[B	�[B	�aB	�nB	�nB	�nB	�ZB	��B	��B	��B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B

�B
�B
B
B
B
�B
B
B
 B
 B
 B
:B
 B
B
:B
 B
 B
:B
 B
 B
:B
 B
 B
 B
:B
 B
 B
:B
B
&B
&B
&B
&B
&B
2B
9B
9B
9B
?B
EB
KB
QB
QB
QB
WB
WB
WB
]B
]B
dB
jB
jB
jB
pB
 vB
 vB
 vB
!|B
!|B
!|B
!|B
!|B
!|B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
(�B
)�B
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
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
0�B
0�B
0�B
0�B
0�B
1�B
2�B
2�B
4B
4B
4�B
5�B
5�B
6B
5�B
5�B
5�B
6�B
6�B
7B
6�B
5�B
8B
8B
8B
8B
8B
6�B
8B
6�B
6�B
8B
8B
8B
9	B
9	B
9	B
9	B
:B
:B
:B
:B
:*B
:B
:�B
;B
<B
<B
="B
>(B
>(B
>(B
>(B
>(B
>(B
>(B
?.B
?.B
?.B
?.B
?.B
?.B
?.B
?.B
@4B
A;B
A;B
A;B
A;B
A;B
A;B
AUB
AUB
A;B
AUB
A;B
AUB
A;B
A;B
A;B
A;B
A;B
AUB
B[B
BAB
B[B
CaB
CGB
CGB
CGB
CGB
CGB
DMB
DMB
DMB
DgB
DMB
DMB
DgB
ESB
FtB
FYB
FYB
FtB
FYB
FYB
G_B
G_B
G_B
HfB
HfB
IlB
IlB
I�B
I�B
KxB
KxB
KxB
L~B
L�B
L�B
KxB
KxB
KxB
KxB
KxB
K�B
L~B
M�B
N�B
O�B
O�B
O�B
P�B
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
S�B
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
X�B
X�B
X�B
X�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
`B
_�B
_�B
_�B
_�B
`B
_�B
_�B
_�B
`B
_�B
_�B
aB
`�B
`�B
aB
`�B
`�B
`�B
`�B
a�B
a�B
a�B
c B
cB
dB
e,B
eB
fB
eB
fB
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
gB
gB
gB
gB
h$B
h$B
h$B
h>B
h$B
h$B
iDB
i*B
iDB
j0B
j0B
jKB
j0B
kQB
j0B
j0B
j0B
k6B
kQB
lWB
l=B
lWB
l=B
l=B
l=B
l=B
l=B
lWB
lWB
l=B
mCB
l=B
k6B
k6B
l=B
l=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.59(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806260038292018062600382920180626003829201806270035282018062700352820180627003528JA  ARFMdecpA19c                                                                20180621183522  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180621093523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180621093524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180621093525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180621093525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180621093525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180621093526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180621093526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180621093526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180621093526                      G�O�G�O�G�O�                JA  ARUP                                                                        20180621095624                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180621154252  CV  JULD            G�O�G�O�F�`{                JM  ARCAJMQC2.0                                                                 20180625153829  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180625153829  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180626153528  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                