CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-11T06:37:34Z creation;2019-12-11T06:37:40Z conversion to V3.1;2023-06-29T05:50:30Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ip   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ML   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ̌   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191211063734  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_195                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��n�c�1   @��8� @7���`A��b������1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D�� D�� D�  D�@ D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @e@��H@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJ\)BR\)BZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
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
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�>�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2�)D3,)D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP�)DQ%�DQ��DR%�DR��DS%�DS��DT\DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_�\D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{%�D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�O�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�R�DĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�DҒ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�R�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D��D��D�R�D���D���D��D�R�D��HD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̅A�|�A�`BA�G�A�A�A�7LA�1'A�-A�-A�(�A�(�A�+A�+A�&�A�$�A� �A� �A� �A�"�A�&�A�(�A�$�A��A��A�oA��yAˏ\A�O�A�JA��TAʅA�$�A���A���AɼjAɬAɋDAɁA�~�A�~�AɁAɅAɃA�A�AȾwAǲ-A��A��hA��A�ȴA�ffA��A���A��A�n�A��A�I�A�jA��A��A���A��A�O�A�33A��RA�I�A�M�A�$�A�\)A��mA��RA�ȴA��A�(�A�$�A�bNA���A�JA�S�A��`A���A�A��HA�7LA��yA�z�A���A��9A��A�  A�=qA�  A�r�A��A�ffA�%A�ƨA�n�A��!AO�A~�A|E�AyC�Ax�Aw�FAw��Av�Au`BAr��Ap�9AooAlAh�\Af�Af=qAd�jAb1A`5?A_&�A^��A]�wA[��AYK�AW�AVE�AU%AS�AQXAP$�AO�AM+AK�^AI%AG��AF�AFbNAF�uAF�AFJAC�;ABQ�AAS�A@r�A>JA;�A9oA6��A5oA3K�A2�A2�A1��A1�A1�PA0�RA0v�A/�mA/K�A.�A-33A,�A,Q�A,$�A+K�A* �A'��A%/A$^5A#G�A!�A �DA 5?A��A��A��AA��An�A �A�^A+A��AA�A��A�HAQ�AjAI�A�wA�`A�^A"�A�uA1'A��A�yA��A�DA��AC�A
VA	�PA(�At�A�A �A�A��A1AdZA j@�@� �@�M�@���@��@�$�@�G�@�ȴ@��@�w@�^5@��`@��@��@�j@��H@�v�@�hs@��@�C�@�G�@�(�@�S�@ޏ\@ݩ�@�V@�9X@�|�@ڧ�@���@ج@�33@�-@�1@���@�ff@���@��@��m@�n�@��@͉7@��/@�ƨ@�@�7L@�z�@��;@ǶF@��H@��#@ċD@���@�V@�33@�E�@���@��h@��@��w@��\@��^@�hs@���@���@�t�@�33@��y@��+@��#@��`@�9X@���@�|�@�K�@��@��H@��\@�=q@���@�bN@���@�K�@�~�@�J@���@��P@���@��@�~�@�?}@�(�@�{@�(�@��P@��P@��@��w@�j@�Z@���@�^5@�-@�{@�{@��7@��9@�A�@��@�~�@���@�`B@�O�@�V@�{@�v�@�7L@�@��;@�1@�I�@���@�|�@�$�@��@�~�@�
=@�S�@�\)@�K�@���@��+@���@��u@��@��/@�r�@�|�@�\)@�"�@��@���@�X@�33@�M�@��u@��;@���@���@�O�@���@���@��y@�33@�
=@���@��^@�9X@���@���@���@�K�@��@��y@���@���@���@��y@���@�p�@�V@�9X@�z�@��@�Ĝ@�j@��@��`@�(�@�1'@�(�@�1'@��
@��
@���@�33@���@��!@��\@�M�@���@�/@��@���@���@��@�r�@�Z@� �@���@�C�@��@��@���@���@�ff@���@���@��@���@���@�J@�7L@�%@��@��/@���@�Q�@��w@�+@���@�b@�b@�  @��F@�ȴ@�~�@�M�@�?}@�7L@�&�@���@�Ĝ@��u@�1'@�ƨ@�S�@�@���@�n�@�@�@��^@���@�X@�7L@��@�%@�Ĝ@��@�I�@�  @���@��@�;d@�"�@�
=@�
=@���@��y@��!@�V@���@���@�hs@���@��D@�z�@�j@�Q�@� �@��@�ƨ@���@�S�@��@�@�ȴ@��R@��!@��\@�V@�=q@�5?@�-@�{@���@���@��h@��@�X@�%@��9@���@��D@�1'@��@��@\)@;d@;d@�@~�@~V@}��@}p�@}V@|�j@|Z@|�@{��@{S�@{"�@{o@z�H@z~�@z-@y��@yG�@xĜ@x��@xr�@xQ�@x  @w��@w�P@w\)@w�@v��@vȴ@v��@u�T@u/@t��@tj@t1@s��@st�@r��@r��@r=q@qhs@p��@pbN@p1'@p �@o�@o�w@o��@oK�@n�R@n{@m�h@m�@m�@m�@m�@m�@l�@lj@kt�@j�\@i��@i��@ix�@ihs@i�@h��@h��@hA�@hb@h  @g�w@g|�@gK�@f��@f�+@f@e�T@e�T@e�-@ep�@eO�@e�@d��@d��@dZ@d�@c�
@cdZ@co@b��@bM�@a��@a�7@ahs@a%@`��@`��@`Q�@`1'@_�@_�@_��@_K�@^�y@^V@^@]��@]`B@\�j@\I�@\(�@\1@[��@[dZ@["�@Z��@Z~�@Y��@Y�@X1'@W�@W|�@WK�@V�y@V��@VV@U��@U?}@T��@TI�@S�
@S��@St�@S"�@S@R�@R�!@Q��@Qhs@Q7L@Q%@P�@PbN@PA�@P �@O�@O|�@O\)@N�@N�+@NV@N@M?}@L�j@LZ@KdZ@J�@J��@J�!@J~�@J=q@I�@I�7@IG�@I�@H�`@HĜ@H�u@HA�@G�@G��@G\)@F�@F�R@Fv�@F@E�-@Ep�@EO�@E?}@D�/@D�@DI�@D1@C�m@C�@C33@B�@B��@B~�@Bn�@B-@A�#@A��@AX@A&�@A%@@�`@@��@@1'@?�w@?\)@?+@>�y@>v�@>5?@=��@=O�@<�@<(�@;��@;"�@:�H@:��@:-@9��@9�^@9x�@97L@9%@8�9@8�9@8�u@8 �@7�;@7��@7��@7\)@7;d@6ȴ@6v�@65?@6{@5�T@5?}@4�@4�/@4�/@4��@4��@4��@4�j@4��@4�D@49X@3�
@3C�@2�@2�H@2��@2��@2��@2�!@2��@2�\@2=q@1�#@1��@1�7@1hs@1G�@17L@1�@0��@01'@0b@/��@/��@/�P@/K�@.�R@.ff@.$�@-�-@-O�@,�j@,��@,j@+��@+��@+33@+@*�@*��@*M�@)�@)��@)�7@)x�@)x�@)X@)X@)&�@(Ĝ@(��@(�u@(r�@(bN@(bN@( �@(  @'�@'�@'��@'\)@';d@'�@'
=@&�@&��@&�+@&{@%�-@%��@%�@%V@$��@$I�@#�m@#��@#dZ@#"�@"�H@"n�@"=q@"J@!��@!�^@!��@!x�@!X@!G�@!�@ ��@ �u@ A�@ 1'@  �@ b@   @�;@��@K�@�@�@ȴ@�R@��@ff@5?@�T@��@O�@��@�/@�@�@�D@j@9X@9X@�@�
@�F@dZ@33@�@�H@�H@��@��@~�@M�@=q@-@�@�^@x�@G�@7L@�@��@��@�9@��@��@r�@Q�@1'@b@�@�w@�@��@\)@;d@+@+@+@
=@��@�R@�+@E�@{@{@@@�@�T@@�-@��@�h@�@?}@��@�/@�/@�@z�@I�@9X@�@��@�F@�@t�@C�@"�@@��@�!@�\@~�@-@J@�@�^@�7@G�@&�@��@��@Ĝ@Ĝ@�9@�9@r�@ �@�;@��@��@l�@\)@+@
=@��@ȴ@��@v�@E�@@�T@��@��@�@�@�@p�@?}@�@�j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̅A�|�A�`BA�G�A�A�A�7LA�1'A�-A�-A�(�A�(�A�+A�+A�&�A�$�A� �A� �A� �A�"�A�&�A�(�A�$�A��A��A�oA��yAˏ\A�O�A�JA��TAʅA�$�A���A���AɼjAɬAɋDAɁA�~�A�~�AɁAɅAɃA�A�AȾwAǲ-A��A��hA��A�ȴA�ffA��A���A��A�n�A��A�I�A�jA��A��A���A��A�O�A�33A��RA�I�A�M�A�$�A�\)A��mA��RA�ȴA��A�(�A�$�A�bNA���A�JA�S�A��`A���A�A��HA�7LA��yA�z�A���A��9A��A�  A�=qA�  A�r�A��A�ffA�%A�ƨA�n�A��!AO�A~�A|E�AyC�Ax�Aw�FAw��Av�Au`BAr��Ap�9AooAlAh�\Af�Af=qAd�jAb1A`5?A_&�A^��A]�wA[��AYK�AW�AVE�AU%AS�AQXAP$�AO�AM+AK�^AI%AG��AF�AFbNAF�uAF�AFJAC�;ABQ�AAS�A@r�A>JA;�A9oA6��A5oA3K�A2�A2�A1��A1�A1�PA0�RA0v�A/�mA/K�A.�A-33A,�A,Q�A,$�A+K�A* �A'��A%/A$^5A#G�A!�A �DA 5?A��A��A��AA��An�A �A�^A+A��AA�A��A�HAQ�AjAI�A�wA�`A�^A"�A�uA1'A��A�yA��A�DA��AC�A
VA	�PA(�At�A�A �A�A��A1AdZA j@�@� �@�M�@���@��@�$�@�G�@�ȴ@��@�w@�^5@��`@��@��@�j@��H@�v�@�hs@��@�C�@�G�@�(�@�S�@ޏ\@ݩ�@�V@�9X@�|�@ڧ�@���@ج@�33@�-@�1@���@�ff@���@��@��m@�n�@��@͉7@��/@�ƨ@�@�7L@�z�@��;@ǶF@��H@��#@ċD@���@�V@�33@�E�@���@��h@��@��w@��\@��^@�hs@���@���@�t�@�33@��y@��+@��#@��`@�9X@���@�|�@�K�@��@��H@��\@�=q@���@�bN@���@�K�@�~�@�J@���@��P@���@��@�~�@�?}@�(�@�{@�(�@��P@��P@��@��w@�j@�Z@���@�^5@�-@�{@�{@��7@��9@�A�@��@�~�@���@�`B@�O�@�V@�{@�v�@�7L@�@��;@�1@�I�@���@�|�@�$�@��@�~�@�
=@�S�@�\)@�K�@���@��+@���@��u@��@��/@�r�@�|�@�\)@�"�@��@���@�X@�33@�M�@��u@��;@���@���@�O�@���@���@��y@�33@�
=@���@��^@�9X@���@���@���@�K�@��@��y@���@���@���@��y@���@�p�@�V@�9X@�z�@��@�Ĝ@�j@��@��`@�(�@�1'@�(�@�1'@��
@��
@���@�33@���@��!@��\@�M�@���@�/@��@���@���@��@�r�@�Z@� �@���@�C�@��@��@���@���@�ff@���@���@��@���@���@�J@�7L@�%@��@��/@���@�Q�@��w@�+@���@�b@�b@�  @��F@�ȴ@�~�@�M�@�?}@�7L@�&�@���@�Ĝ@��u@�1'@�ƨ@�S�@�@���@�n�@�@�@��^@���@�X@�7L@��@�%@�Ĝ@��@�I�@�  @���@��@�;d@�"�@�
=@�
=@���@��y@��!@�V@���@���@�hs@���@��D@�z�@�j@�Q�@� �@��@�ƨ@���@�S�@��@�@�ȴ@��R@��!@��\@�V@�=q@�5?@�-@�{@���@���@��h@��@�X@�%@��9@���@��D@�1'@��@��@\)@;d@;d@�@~�@~V@}��@}p�@}V@|�j@|Z@|�@{��@{S�@{"�@{o@z�H@z~�@z-@y��@yG�@xĜ@x��@xr�@xQ�@x  @w��@w�P@w\)@w�@v��@vȴ@v��@u�T@u/@t��@tj@t1@s��@st�@r��@r��@r=q@qhs@p��@pbN@p1'@p �@o�@o�w@o��@oK�@n�R@n{@m�h@m�@m�@m�@m�@m�@l�@lj@kt�@j�\@i��@i��@ix�@ihs@i�@h��@h��@hA�@hb@h  @g�w@g|�@gK�@f��@f�+@f@e�T@e�T@e�-@ep�@eO�@e�@d��@d��@dZ@d�@c�
@cdZ@co@b��@bM�@a��@a�7@ahs@a%@`��@`��@`Q�@`1'@_�@_�@_��@_K�@^�y@^V@^@]��@]`B@\�j@\I�@\(�@\1@[��@[dZ@["�@Z��@Z~�@Y��@Y�@X1'@W�@W|�@WK�@V�y@V��@VV@U��@U?}@T��@TI�@S�
@S��@St�@S"�@S@R�@R�!@Q��@Qhs@Q7L@Q%@P�@PbN@PA�@P �@O�@O|�@O\)@N�@N�+@NV@N@M?}@L�j@LZ@KdZ@J�@J��@J�!@J~�@J=q@I�@I�7@IG�@I�@H�`@HĜ@H�u@HA�@G�@G��@G\)@F�@F�R@Fv�@F@E�-@Ep�@EO�@E?}@D�/@D�@DI�@D1@C�m@C�@C33@B�@B��@B~�@Bn�@B-@A�#@A��@AX@A&�@A%@@�`@@��@@1'@?�w@?\)@?+@>�y@>v�@>5?@=��@=O�@<�@<(�@;��@;"�@:�H@:��@:-@9��@9�^@9x�@97L@9%@8�9@8�9@8�u@8 �@7�;@7��@7��@7\)@7;d@6ȴ@6v�@65?@6{@5�T@5?}@4�@4�/@4�/@4��@4��@4��@4�j@4��@4�D@49X@3�
@3C�@2�@2�H@2��@2��@2��@2�!@2��@2�\@2=q@1�#@1��@1�7@1hs@1G�@17L@1�@0��@01'@0b@/��@/��@/�P@/K�@.�R@.ff@.$�@-�-@-O�@,�j@,��@,j@+��@+��@+33@+@*�@*��@*M�@)�@)��@)�7@)x�@)x�@)X@)X@)&�@(Ĝ@(��@(�u@(r�@(bN@(bN@( �@(  @'�@'�@'��@'\)@';d@'�@'
=@&�@&��@&�+@&{@%�-@%��@%�@%V@$��@$I�@#�m@#��@#dZ@#"�@"�H@"n�@"=q@"J@!��@!�^@!��@!x�@!X@!G�@!�@ ��@ �u@ A�@ 1'@  �@ b@   @�;@��@K�@�@�@ȴ@�R@��@ff@5?@�T@��@O�@��@�/@�@�@�D@j@9X@9X@�@�
@�F@dZ@33@�@�H@�H@��@��@~�@M�@=q@-@�@�^@x�@G�@7L@�@��@��@�9@��@��@r�@Q�@1'@b@�@�w@�@��@\)@;d@+@+@+@
=@��@�R@�+@E�@{@{@@@�@�T@@�-@��@�h@�@?}@��@�/@�/@�@z�@I�@9X@�@��@�F@�@t�@C�@"�@@��@�!@�\@~�@-@J@�@�^@�7@G�@&�@��@��@Ĝ@Ĝ@�9@�9@r�@ �@�;@��@��@l�@\)@+@
=@��@ȴ@��@v�@E�@@�T@��@��@�@�@�@p�@?}@�@�j@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��BB%B%B%B%B%B%B+B1B	7B
=B
=B
=B
=B
=B
=BDBJBPBPBJBPBDBVB�B#�B1'B:^B@�B@�BH�BM�BO�BR�B[#B^5B`BB`BB`BBaHBbNBe`BjBp�Bt�BG�BhB�VB��B+B#�B �B��B�B#�B�B(�B
��B
�B
��B
��B
��B
��BN�B�BB�HB�B@�BXB9XBE�BL�BH�B49B��BB�hBx�BiyB�?B�3B��BɺB��B�bB
�B
&�B	��B	�B	�mB	�NB	�#B	��B	��B	��B	�-B	��B	�uB	�PB	�B	s�B	k�B	m�B	u�B	x�B	z�B	p�B	^5B	L�B	<jB	"�B	�B	�B	�B	VB��B�B�B�mB�BB�)B�B��BȴBĜB�jB�FB�XB�B�9B��B��B��B��B�B��B��B�LB�B��B��B��B�B|�Bz�By�Bq�By�B�B�B�%B�B� B}�B~�B}�B~�Bv�Bp�Bv�B|�B�B|�Bv�Bn�BgmBbNB[#BXB[#BW
BQ�BQ�BL�BI�BL�BC�BC�BC�BE�BE�BD�BE�BF�BI�BJ�BJ�BG�BC�BB�BF�BE�BE�BD�B<jB49B2-B2-B49B2-B2-B/B.B+B(�B,B)�B,B-B-B(�B%�B"�B"�B!�B!�B#�B"�B#�B#�B"�B"�B!�B%�B#�B#�B%�B&�B'�B(�B)�B)�B+B+B+B,B,B,B-B/B1'B1'B49B49B49B5?B5?B:^B;dB;dB;dB<jB=qB>wBB�BB�BC�BC�BG�BH�BF�BE�BK�BN�BP�BQ�BQ�BW
BW
BZB[#B[#B^5BcTBgmBm�Bq�B~�B�B�B�+B�+B�%B�+B�+B�+B�1B�7B�PB�PB�bB�oB��B��B��B��B�3BǮB��BɺBƨBĜB�}B�wB�}B��BɺB��B�
B�B�/B�/B�/B�/B�HB�NB�NB�NB�TB�TB�fB�mB�B��B	  B	oB	�B	(�B	.B	2-B	9XB	9XB	8RB	8RB	B�B	F�B	L�B	O�B	T�B	YB	YB	XB	VB	ZB	]/B	_;B	e`B	gmB	iyB	l�B	n�B	n�B	gmB	e`B	dZB	aHB	bNB	_;B	^5B	aHB	cTB	p�B	t�B	u�B	t�B	s�B	o�B	o�B	p�B	t�B	t�B	s�B	x�B	t�B	u�B	x�B	� B	�uB	�uB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�9B	�9B	�3B	�3B	�3B	�FB	�?B	�'B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�^B	�^B	�dB	�^B	�RB	�FB	�XB	�RB	�XB	�^B	�^B	�dB	�qB	�qB	�wB	�}B	��B	B	ÖB	ƨB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
%B
+B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
%�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
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
5?B
6FB
6FB
6FB
6FB
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
;dB
;dB
;dB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
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
@�B
@�B
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
E�B
E�B
E�B
E�B
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
F�B
F�B
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
I�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
T�B
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
YB
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
]/B
]/B
]/B
]/B
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
aHB
aHB
bNB
bNB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
hsB
hsB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
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
q�B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B�B�BBB�B�B�B�B�B	B
#B
	B
	B
	B
	B
	B)BBB6B0BPB�B�B�B$@B1vB:�B@�B@�BH�BM�BO�BSBZ�B^B`B`B`'Ba|BcBf�BmwButB|�BL�B
B�B�2BB'�B%,B �B!�B*�B�NB/�B
�QB
�mB
�B
��B
��B
�zBL~B�'B�B�BA�BZ�B:xBF�BOvBLJB7LBB�B�FBz*Bh�B��B��B��B��B�,B�/B
�4B
-B	��B	�QB	��B	�tB	�/B	�oB	��B	��B	��B	��B	��B	��B	��B	t�B	k�B	m�B	v�B	z�B	}�B	r�B	`�B	PbB	?�B	$�B	�B	�B	=B	.B�B�B��B�B�BݲB��BѝB��BƎB��B��B�JB�OB��B�FB��B��B�]B�)B��BðB�	B�oB�B��B�eB�fB�B|�B{JBrBzB��B�MB��B��B�iB~�B�B~�B�OBwfBp�Bw2B~B��B�By$Bo�Bh�Bc�B\]BX�B\�BXEBS&BSBM�BJ�BN�BD3BDBC�BF?BF%BEmBE�BFtBJ	BKxBK�BH�BD3BC-BF�BF%BF�BFB=�B5B2�B33B5?B3�B3B0!B/ B,�B*�B,�B+B-]B.�B-�B*0B'B#:B#:B"�B# B$�B#�B$�B$tB#B#�B#�B&�B$B$�B&�B'�B(�B)yB*eB*KB+kB+kB+kB,WB,qB,�B-�B/�B1�B2-B4�B4TB4�B5�B5�B:�B;B;�B;�B=B=�B?cBB�BB�BC�BD3BHfBI�BG�BF�BL�BO(BP�BR BRoBWsBW�BZkB[=B[�B^�Bc:BgmBm�Bq�B.B�uB�SB�EB�B�B�B�EB�EB�fB��B�PB��B�}B��B�gB��B��B��B�B��B�VB�rB��B�mB��B�BB�.B� B�7B��B�YBچB�B�B�B�dB�B�hB�B��B�nB�TB��B�B�aB��B�BB	NB	sB	(�B	-�B	2-B	9�B	9�B	88B	7�B	A�B	F?B	L�B	O�B	UB	Y1B	YeB	X_B	U�B	ZB	]IB	_�B	e,B	gRB	i_B	l�B	oiB	o�B	h
B	fB	d�B	a|B	c B	_!B	]�B	`�B	b�B	p;B	t�B	u�B	u%B	t9B	o�B	oiB	poB	t�B	t�B	s�B	y$B	tTB	u%B	w�B	~wB	�uB	�uB	��B	�B	�,B	�$B	�B	�QB	�dB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	� B	�/B	�/B	��B	��B	��B	��B	��B	��B	�B	�B	�;B	�B	�B	�B	�B	�B	�3B	�B	�B	��B	��B	��B	�IB	��B	��B	��B	��B	�B	�"B	��B	��B	��B	�B	�DB	�dB	��B	�RB	�FB	��B	�B	�$B	�DB	�DB	�JB	�qB	�qB	�wB	�}B	��B	B	ðB	ƨB	�tB	ƎB	ǔB	ȀB	ɠB	ʌB	ʦB	ˬB	̳B	͹B	��B	��B	��B	��B	��B	ԕB	��B	��B	��B	�B	�B	��B	�QB	�CB	�!B	�B	�B	�B	�'B	�-B	�4B	�4B	�:B	�,B	�B	�LB	�8B	�8B	�8B	�XB	�DB	�*B	�DB	�DB	�KB	�eB	�kB	�QB	�qB	�B	�wB	�cB	�cB	�}B	�oB	��B	��B	�vB	�[B	�vB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
�B
�B
�B
�B
�B
B
�B
B
%B
B
B
�B
�B
�B
�B
�B
B
+B
	B
	B
�B
�B
�B
	B

#B

#B

=B

XB

=B

#B
B
B
B
B
B
B
0B
B
B
B
"B
(B
BB
BB
HB
.B
.B
.B
.B
4B
B
B
4B
4B
 B
:B
TB
@B
FB
aB
gB
MB
SB
SB
SB
SB
YB
YB
YB
yB
_B
_B
yB
B
kB
kB
�B
kB
�B
xB
xB
�B
xB
~B
~B
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
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
,B
-B
-B
-B
-�B
-�B
.�B
.�B
.�B
.�B
/ B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2B
3B
2�B
3B
4B
4B
4B
4B
4B
5B
6B
6B
6B
6+B
7B
8B
8B
88B
9$B
9$B
9	B
9$B
:*B
:*B
:*B
:DB
:*B
:DB
;JB
;0B
;B
;JB
;JB
<6B
<PB
<PB
=VB
<PB
=VB
=VB
>BB
>BB
>]B
>BB
>BB
?cB
?HB
?cB
?cB
?HB
@OB
@iB
AUB
AUB
A;B
B[B
B[B
BuB
CaB
C{B
CaB
CaB
C{B
DgB
ESB
ESB
ESB
ESB
ESB
ESB
EmB
EmB
EmB
E�B
EmB
FtB
FYB
FtB
G_B
GzB
G_B
G_B
FtB
FtB
GzB
GzB
GzB
H�B
HfB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
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
S�B
S�B
S�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
XB
X�B
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
[�B
[�B
[�B
\�B
\�B
\�B
]B
\�B
\�B
\�B
\�B
^B
^B
]�B
]�B
^B
_B
_B
^�B
_B
_B
^�B
_B
_B
_B
_B
_B
`B
`B
`B
`'B
aB
aB
`�B
aB
bB
bB
a�B
bB
b4B
bB
b4B
bB
bB
c B
cB
c B
c B
c:B
c B
cB
c:B
d&B
dB
d&B
d&B
dB
d&B
eB
d&B
eB
eB
eB
eFB
eFB
e,B
e,B
eFB
e,B
fB
f2B
f2B
fB
fB
f2B
fB
f2B
fB
f2B
g8B
g8B
gB
gB
g8B
gB
gB
gB
g8B
h$B
h$B
h
B
h>B
h>B
h>B
h>B
h$B
h>B
i_B
i_B
i*B
iDB
i*B
iDB
iDB
jB
jKB
jKB
jKB
jKB
jKB
jKB
jKB
jKB
kQB
kQB
kQB
lWB
lWB
lWB
lqB
lqB
l=B
lWB
lWB
l=B
lWB
lWB
lWB
mCB
m]B
m]B
m]B
mwB
ncB
ncB
ncB
n}B
ncB
ncB
oiB
oiB
oOB
oiB
oiB
oOB
oOB
oiB
p�B
pUB
poB
q[B
q[111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<PD<%��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.59(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912160034052019121600340520191216003405202306231719302023062317193020230623171930201912170025022019121700250220191217002502  JA  ARFMdecpA19c                                                                20191211153730  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191211063734  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191211063736  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191211063737  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191211063738  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191211063738  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191211063738  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191211063738  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191211063740  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191211063740                      G�O�G�O�G�O�                JA  ARUP                                                                        20191211065421                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191211153633  CV  JULD            G�O�G�O�FǔT                JM  ARCAJMQC2.0                                                                 20191215153405  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191215153405  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191216152502  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081930  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                