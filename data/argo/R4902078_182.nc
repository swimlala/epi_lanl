CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-04-20T11:01:31Z creation      
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
resolution        =���   axis      Z        X  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  o(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �X   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ژ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20200420110131  20200420110131  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�X�(?�1   @�[�/v@+bM���dp�hr�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@��HA	p�A)p�AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2\)B:\)BB\)BJBRBZ\)Bb\)Bj\)Br\)Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
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
C.��C0�
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
C�K�C�K�C�>�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS��DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Du%�Du��Dv%�Dv��Dw%�Dw��Dx%�Dx��Dy%�Dy��Dz%�Dz��D{%�D{��D|%�D|��D}%�D}��D~%�D~��D%�D��D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D���D���D��D�R�D�D���D��D�R�DÒ�D���D��D�R�DĒ�D���D��D�R�DŒ�D���D��D�R�Dƒ�D���D��D�R�Dǒ�D���D��D�R�DȒ�D���D��D�R�Dɒ�D���D��D�R�Dʒ�D���D��D�R�D˒�D���D��D�R�D̒�D���D��D�R�D͒�D���D��D�R�DΒ�D���D��D�R�Dϒ�D���D��D�R�DВ�D���D��D�R�Dђ�D���D��D�R�DҒ�D���D��D�R�DӒ�D���D��D�R�DԒ�D���D��D�R�DՒ�D���D��D�R�D֒�D���D��D�R�Dג�D���D��D�R�Dؒ�D���D��D�R�Dْ�D���D��D�R�Dڒ�D���D��D�R�Dے�D���D��D�R�Dܒ�D���D��D�R�Dݒ�D���D��D�R�Dޒ�D���D��D�R�Dߒ�D���D��D�R�D���D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D��D���D��D�R�D���D���D��D�R�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�-A�-A�+A�+A�/A�/A�/A�1'A�5?A�33A�33A�-A�$�A��A�oA���AԴ9AԮAԧ�Aԧ�Aԥ�Aԣ�Aԣ�Aԟ�Aԗ�AԁA�bNA�/AӶFA�O�A��A��yA��A�C�A�;dA���A���A���A�p�A�9XA�~�A�\)A� �AɶFA�~�A�ffA�G�A��A�t�A�VA���AŮA���A�&�A��wA��yA�t�A��A��A�G�A��#A��A��DA���A���A�1'A�v�A�=qA��FA���A�5?A��A��/A�M�A��`A�A�A~�+A|  Ay"�At(�Ao��AjffAc\)A^��A[��AY�AV��AR�HAN�9AK�7AIt�AG`BAD��AC�AA7LA@1'A?��A>�uA=VA<�\A;��A:�A7�A5�TA5��A5S�A4��A3��A2�\A1�A1�hA0�9A0A/A.�A.^5A.$�A-�A+�;A*�A(��A( �A(�A'��A&�RA%O�A$�RA$n�A#�A"�A"�!A"��A"jA!��A!|�A!XA ��A �\A M�A�7A`BA�yAffA�TA��A��A�7A+A�HA�RA�uA�TA?}A�jAVA$�A�A��A�A�A��A��A��A+Av�AbA��Ap�A/A�+AM�A{An�A��A�`A�Az�Av�An�AffAjAZAA�A-A{A  A�#AO�A9XA��A�uAE�A=qAE�AA�A-A�A�A�AhsA
^5A	�wA	��A	��A	XA	��A
�A
5?A
5?A	S�A	K�A	dZA	x�A	33A	�A	%A�A��Az�A$�A{A�#A%A�jA��A~�A$�A�TA�FAp�A�HAffA��AO�A%AĜA�;AhsA33A ��A (�@���@���@�b@�n�@���@�  @���@�t�@���@�V@��T@�hs@�G�@�%@�I�@�  @�P@�ȴ@��@�^@�X@��D@�
=@�$�@�X@�  @��y@�@�h@�7L@���@��@��@�@畁@��@��/@���@�
=@�J@ᙚ@�%@���@�9X@��@�|�@�\)@�K�@�+@���@ݩ�@�p�@��@��
@�K�@�33@�"�@�@ڧ�@�^5@���@�hs@�G�@�7L@�V@�9X@��
@�\)@�ȴ@�E�@�hs@Դ9@�r�@��m@�C�@�ȴ@�n�@�$�@љ�@�p�@�`B@�V@У�@�b@υ@��@��H@�M�@���@���@��@˝�@˅@�S�@���@�ff@�@ɲ-@�hs@�/@��@ț�@�j@�j@�Z@�ƨ@��@�5?@Ų-@Ł@�V@ļj@��
@�
=@+@�V@�M�@��@��h@�&�@�Ĝ@���@��w@���@�l�@��@�@��@�n�@���@��@��j@�r�@� �@�S�@��R@�ff@���@�/@�Q�@���@�t�@�S�@�K�@��y@��\@�ff@���@�/@��@���@��@�33@�ȴ@��+@���@���@���@�&�@���@�Ĝ@��@��;@�;d@�
=@��H@���@�E�@�{@���@���@�G�@���@�bN@�1'@�  @��w@��@�C�@���@��+@�n�@�M�@�5?@��#@�p�@��@��u@�9X@�(�@���@�\)@�+@�
=@���@���@�$�@���@�x�@�?}@��@��j@�1@��
@���@�dZ@�C�@�"�@��@��R@��+@�-@���@�7L@���@��9@�Z@��;@�dZ@�"�@�@�ȴ@�ff@�J@���@��7@�?}@��@��9@��u@�bN@���@��F@�33@��R@��!@���@��+@�ff@�V@�=q@�{@��h@�X@�?}@�Ĝ@�z�@�I�@�1@��@�t�@�dZ@�S�@�o@��!@���@�=q@���@���@���@�x�@�?}@��@���@���@�Q�@�  @��w@�C�@�o@��@���@�^5@�=q@��@��@���@��7@�x�@��`@�Ĝ@��u@�I�@��@��@���@��w@��@���@�dZ@�+@�+@�"�@���@�ȴ@�V@���@��@�O�@�7L@��@�V@���@��@��/@�Ĝ@��u@�Z@��
@��w@��@�K�@�"�@�ȴ@�~�@�-@���@�G�@�G�@�G�@�G�@�O�@���@��@�I�@�1'@� �@��P@�K�@�"�@���@��+@�M�@���@���@��-@�/@�j@�ƨ@�C�@�33@�o@��\@�5?@���@�`B@���@��j@�z�@��@��@�@�@~�R@~@}��@|��@|(�@{��@{ƨ@{dZ@{o@z��@z~�@y�@x�9@x�u@xQ�@xA�@x1'@x1'@x  @w\)@v�R@vv�@v@u�T@u?}@t�@tZ@t�@s�@r�H@q�#@q�^@q��@q�7@qX@qG�@q�@p�`@p��@pQ�@o�w@oK�@nff@n{@n@m�@m@m?}@l�D@l(�@kƨ@k"�@j^5@jJ@i�7@h�@h �@h  @h  @g�@g�@g�@g|�@f$�@e��@e�h@e`B@e/@d�@d�j@d��@dI�@cC�@b��@bM�@bM�@b=q@b�@a�^@ax�@`Ĝ@`bN@` �@_K�@]�@]@]�h@\�/@\z�@\Z@\1@[��@[o@Z�\@Y��@Y��@Y�@X��@X  @W�@W+@Vv�@V5?@V@U@U�h@Up�@U�@T�@T��@S�F@St�@So@Rn�@Q��@Q��@QG�@Q&�@P�9@PQ�@Pb@O�w@O|�@Nȴ@NV@M�@M@M/@L��@L�@L�D@LZ@K��@Kt�@K33@J�@J��@J=q@I�@I��@I&�@H��@H�9@HA�@Hb@G�w@G\)@G
=@F�R@Fv�@F5?@E�T@Ep�@E�@D�@DZ@C��@Ct�@Ct�@CdZ@C33@B�@B��@B��@B^5@A��@A��@A7L@A�@@�u@?�;@?|�@?l�@?+@>��@>��@>$�@=p�@=V@<�/@<z�@<9X@;�
@;t�@;S�@;C�@;33@:�@:�!@:��@:^5@9�#@9��@9&�@8��@8bN@8 �@7�w@7
=@6��@6v�@6ff@6V@6@5��@5`B@4�j@4Z@4Z@4�@3C�@3@2��@2~�@2^5@2�@1��@17L@1%@0�@0Q�@01'@/�P@/;d@.��@.��@.5?@.{@-@-�h@-�@-O�@,��@,j@+ƨ@+S�@+o@*�!@*^5@*-@*�@)�^@)G�@)%@(�`@(Ĝ@(��@(bN@( �@(b@(  @'�@'��@'|�@'K�@'
=@&��@&ff@&{@%@%��@%�@%p�@%�@%V@%V@%V@$�@$�@$��@$z�@$�@#ƨ@#�@#"�@#@"�@"�@"��@"��@"��@"��@"��@"�\@"n�@"=q@"�@!��@!X@!G�@!&�@ ��@ r�@ b@   @�P@\)@��@�R@��@ff@V@@@�@�T@��@�h@p�@�j@9X@�@1@��@dZ@C�@o@M�@M�@�@�@�#@��@��@x�@X@G�@7L@7L@��@��@�@bN@Q�@�w@l�@\)@K�@;d@;d@;d@��@�@�R@��@v�@V@E�@5?@{@�T@@�@V@�@z�@1@��@�m@�
@�F@dZ@33@o@@��@�\@~�@n�@^5@M�@=q@J@��@��@��@��@��@�@�^@x�@G�@%@Ĝ@�9@r�@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�-A�-A�+A�+A�/A�/A�/A�1'A�5?A�33A�33A�-A�$�A��A�oA���AԴ9AԮAԧ�Aԧ�Aԥ�Aԣ�Aԣ�Aԟ�Aԗ�AԁA�bNA�/AӶFA�O�A��A��yA��A�C�A�;dA���A���A���A�p�A�9XA�~�A�\)A� �AɶFA�~�A�ffA�G�A��A�t�A�VA���AŮA���A�&�A��wA��yA�t�A��A��A�G�A��#A��A��DA���A���A�1'A�v�A�=qA��FA���A�5?A��A��/A�M�A��`A�A�A~�+A|  Ay"�At(�Ao��AjffAc\)A^��A[��AY�AV��AR�HAN�9AK�7AIt�AG`BAD��AC�AA7LA@1'A?��A>�uA=VA<�\A;��A:�A7�A5�TA5��A5S�A4��A3��A2�\A1�A1�hA0�9A0A/A.�A.^5A.$�A-�A+�;A*�A(��A( �A(�A'��A&�RA%O�A$�RA$n�A#�A"�A"�!A"��A"jA!��A!|�A!XA ��A �\A M�A�7A`BA�yAffA�TA��A��A�7A+A�HA�RA�uA�TA?}A�jAVA$�A�A��A�A�A��A��A��A+Av�AbA��Ap�A/A�+AM�A{An�A��A�`A�Az�Av�An�AffAjAZAA�A-A{A  A�#AO�A9XA��A�uAE�A=qAE�AA�A-A�A�A�AhsA
^5A	�wA	��A	��A	XA	��A
�A
5?A
5?A	S�A	K�A	dZA	x�A	33A	�A	%A�A��Az�A$�A{A�#A%A�jA��A~�A$�A�TA�FAp�A�HAffA��AO�A%AĜA�;AhsA33A ��A (�@���@���@�b@�n�@���@�  @���@�t�@���@�V@��T@�hs@�G�@�%@�I�@�  @�P@�ȴ@��@�^@�X@��D@�
=@�$�@�X@�  @��y@�@�h@�7L@���@��@��@�@畁@��@��/@���@�
=@�J@ᙚ@�%@���@�9X@��@�|�@�\)@�K�@�+@���@ݩ�@�p�@��@��
@�K�@�33@�"�@�@ڧ�@�^5@���@�hs@�G�@�7L@�V@�9X@��
@�\)@�ȴ@�E�@�hs@Դ9@�r�@��m@�C�@�ȴ@�n�@�$�@љ�@�p�@�`B@�V@У�@�b@υ@��@��H@�M�@���@���@��@˝�@˅@�S�@���@�ff@�@ɲ-@�hs@�/@��@ț�@�j@�j@�Z@�ƨ@��@�5?@Ų-@Ł@�V@ļj@��
@�
=@+@�V@�M�@��@��h@�&�@�Ĝ@���@��w@���@�l�@��@�@��@�n�@���@��@��j@�r�@� �@�S�@��R@�ff@���@�/@�Q�@���@�t�@�S�@�K�@��y@��\@�ff@���@�/@��@���@��@�33@�ȴ@��+@���@���@���@�&�@���@�Ĝ@��@��;@�;d@�
=@��H@���@�E�@�{@���@���@�G�@���@�bN@�1'@�  @��w@��@�C�@���@��+@�n�@�M�@�5?@��#@�p�@��@��u@�9X@�(�@���@�\)@�+@�
=@���@���@�$�@���@�x�@�?}@��@��j@�1@��
@���@�dZ@�C�@�"�@��@��R@��+@�-@���@�7L@���@��9@�Z@��;@�dZ@�"�@�@�ȴ@�ff@�J@���@��7@�?}@��@��9@��u@�bN@���@��F@�33@��R@��!@���@��+@�ff@�V@�=q@�{@��h@�X@�?}@�Ĝ@�z�@�I�@�1@��@�t�@�dZ@�S�@�o@��!@���@�=q@���@���@���@�x�@�?}@��@���@���@�Q�@�  @��w@�C�@�o@��@���@�^5@�=q@��@��@���@��7@�x�@��`@�Ĝ@��u@�I�@��@��@���@��w@��@���@�dZ@�+@�+@�"�@���@�ȴ@�V@���@��@�O�@�7L@��@�V@���@��@��/@�Ĝ@��u@�Z@��
@��w@��@�K�@�"�@�ȴ@�~�@�-@���@�G�@�G�@�G�@�G�@�O�@���@��@�I�@�1'@� �@��P@�K�@�"�@���@��+@�M�@���@���@��-@�/@�j@�ƨ@�C�@�33@�o@��\@�5?@���@�`B@���@��j@�z�@��@��@�@�@~�R@~@}��@|��@|(�@{��@{ƨ@{dZ@{o@z��@z~�@y�@x�9@x�u@xQ�@xA�@x1'@x1'@x  @w\)@v�R@vv�@v@u�T@u?}@t�@tZ@t�@s�@r�H@q�#@q�^@q��@q�7@qX@qG�@q�@p�`@p��@pQ�@o�w@oK�@nff@n{@n@m�@m@m?}@l�D@l(�@kƨ@k"�@j^5@jJ@i�7@h�@h �@h  @h  @g�@g�@g�@g|�@f$�@e��@e�h@e`B@e/@d�@d�j@d��@dI�@cC�@b��@bM�@bM�@b=q@b�@a�^@ax�@`Ĝ@`bN@` �@_K�@]�@]@]�h@\�/@\z�@\Z@\1@[��@[o@Z�\@Y��@Y��@Y�@X��@X  @W�@W+@Vv�@V5?@V@U@U�h@Up�@U�@T�@T��@S�F@St�@So@Rn�@Q��@Q��@QG�@Q&�@P�9@PQ�@Pb@O�w@O|�@Nȴ@NV@M�@M@M/@L��@L�@L�D@LZ@K��@Kt�@K33@J�@J��@J=q@I�@I��@I&�@H��@H�9@HA�@Hb@G�w@G\)@G
=@F�R@Fv�@F5?@E�T@Ep�@E�@D�@DZ@C��@Ct�@Ct�@CdZ@C33@B�@B��@B��@B^5@A��@A��@A7L@A�@@�u@?�;@?|�@?l�@?+@>��@>��@>$�@=p�@=V@<�/@<z�@<9X@;�
@;t�@;S�@;C�@;33@:�@:�!@:��@:^5@9�#@9��@9&�@8��@8bN@8 �@7�w@7
=@6��@6v�@6ff@6V@6@5��@5`B@4�j@4Z@4Z@4�@3C�@3@2��@2~�@2^5@2�@1��@17L@1%@0�@0Q�@01'@/�P@/;d@.��@.��@.5?@.{@-@-�h@-�@-O�@,��@,j@+ƨ@+S�@+o@*�!@*^5@*-@*�@)�^@)G�@)%@(�`@(Ĝ@(��@(bN@( �@(b@(  @'�@'��@'|�@'K�@'
=@&��@&ff@&{@%@%��@%�@%p�@%�@%V@%V@%V@$�@$�@$��@$z�@$�@#ƨ@#�@#"�@#@"�@"�@"��@"��@"��@"��@"��@"�\@"n�@"=q@"�@!��@!X@!G�@!&�@ ��@ r�@ b@   @�P@\)@��@�R@��@ff@V@@@�@�T@��@�h@p�@�j@9X@�@1@��@dZ@C�@o@M�@M�@�@�@�#@��@��@x�@X@G�@7L@7L@��@��@�@bN@Q�@�w@l�@\)@K�@;d@;d@;d@��@�@�R@��@v�@V@E�@5?@{@�T@@�@V@�@z�@1@��@�m@�
@�F@dZ@33@o@@��@�\@~�@n�@^5@M�@=q@J@��@��@��@��@��@�@�^@x�@G�@%@Ĝ@�9@r�@A�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�;B�;B�;B�/B�/B�/B�/B�/B�/B�/B�/B�)B�#B�B�
B��B��B��B�NB	
=B	  B��B	�B	�B	B��B	B	uB	R�B	r�B	��B	ƨB	�B
�B
ZB
p�B
o�B
v�B
�+B
��B
�XB
��B
�B
=B?}BI�B>wB/B�B�B�BJB
�B
��B
��B
�5B
ȴB
�?B
��B
��B
�PB
[#B
�B

=B	�B	�#B	ǮB	�3B	��B	�\B	|�B	v�B	n�B	iyB	aHB	[#B	W
B	VB	R�B	P�B	VB	VB	YB	[#B	[#B	]/B	bNB	cTB	gmB	iyB	hsB	k�B	m�B	p�B	n�B	n�B	x�B	�%B	�VB	��B	��B	�3B	�FB	�RB	�XB	�}B	ǮB	��B	�;B	�`B	�ZB	�`B	�B	��B
B
B
+B

=B
DB
JB
JB
�B
�B
 �B
�B
�B
 �B
$�B
#�B
%�B
.B
49B
:^B
?}B
@�B
E�B
E�B
G�B
H�B
K�B
N�B
O�B
P�B
R�B
VB
W
B
VB
W
B
T�B
S�B
P�B
XB
VB
R�B
Q�B
Q�B
VB
S�B
W
B
VB
I�B
B�B
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
?}B
=qB
:^B
=qB
A�B
C�B
D�B
E�B
D�B
C�B
F�B
I�B
F�B
=qB
7LB
7LB
8RB
8RB
=qB
J�B
L�B
M�B
H�B
I�B
K�B
M�B
L�B
L�B
M�B
M�B
N�B
O�B
N�B
M�B
L�B
K�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
E�B
E�B
E�B
D�B
C�B
B�B
A�B
@�B
@�B
@�B
?}B
<jB
7LB
6FB
49B
33B
33B
6FB
8RB
8RB
7LB
7LB
6FB
6FB
7LB
6FB
49B
33B
2-B
2-B
2-B
1'B
0!B
0!B
/B
.B
,B
)�B
)�B
(�B
)�B
)�B
)�B
)�B
&�B
#�B
!�B
!�B
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
!�B
"�B
#�B
"�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
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
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
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
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
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
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
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
7LB
8RB
7LB
8RB
8RB
9XB
9XB
9XB
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
;dB
<jB
<jB
=qB
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
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
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
G�B
G�B
H�B
G�B
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
J�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
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
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
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
hsB
iyB
hsB
iyB
iyB
iyB
iyB
jB
iyB
jB
jB
k�B
k�B
jB
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
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
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
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƿB§B�vB�8B�B��B�B�~B	<B	[B��B�B��B	*B	B�B	beB	�BB	�]B	�_B
sB
I�B
`YB
_SB
f~B
v�B
�sB
�B
��B
�YB
��B/2B9oB.,B�BgBgB	OB
��B
�kB
�xB
�B
��B
�iB
��B
�<B
�mB
}B
J�B
aB	��B	�FB	��B	�cB	��B	��B	B	l�B	f~B	^MB	Y.B	P�B	J�B	F�B	E�B	B�B	@�B	E�B	E�B	H�B	J�B	J�B	L�B	RB	S	B	W"B	Y.B	X(B	[:B	]FB	`YB	^MB	^MB	h�B	u�B	~B	�OB	��B	��B	��B	�B	�B	�2B	�cB	��B	��B	�B	�B	�B	�MB	�B	�B	��B	��B	��B	��B	��B	��B
6B
gB
zB
sB
sB
zB
�B
�B
�B
�B
#�B
*B
/2B
08B
5WB
5WB
7cB
8iB
;|B
>�B
?�B
@�B
B�B
E�B
F�B
E�B
F�B
D�B
C�B
@�B
G�B
E�B
B�B
A�B
A�B
E�B
C�B
F�B
E�B
9oB
2DB
.,B
.,B
.,B
/2B
/2B
/2B
08B
08B
08B
08B
08B
08B
08B
/2B
-&B
*B
-&B
1>B
3KB
4QB
5WB
4QB
3KB
6]B
9oB
6]B
-&B
'B
'B
(B
(B
-&B
:vB
<�B
=�B
8iB
9oB
;|B
=�B
<�B
<�B
=�B
=�B
>�B
?�B
>�B
=�B
<�B
;|B
:vB
:vB
:vB
:vB
9oB
8iB
8iB
7cB
5WB
5WB
5WB
4QB
3KB
2DB
1>B
08B
08B
08B
/2B
,B
'B
%�B
#�B
"�B
"�B
%�B
(B
(B
'B
'B
%�B
%�B
'B
%�B
#�B
"�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
zB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
zB
sB
zB
zB
zB
zB
zB
sB
sB
sB
mB
mB
mB
mB
mB
gB
gB
aB
gB
gB
gB
gB
gB
gB
gB
gB
aB
aB
aB
aB
aB
aB
[B
[B
[B
[B
aB
aB
[B
[B
[B
[B
aB
[B
[B
[B
[B
[B
[B
[B

UB
[B
[B
[B
[B
[B

UB

UB
[B
[B

UB

UB

UB

UB

UB

UB
[B
[B
[B
[B
[B
[B
[B

UB
[B

UB

UB

UB
[B
aB
aB
[B
aB
aB
[B
[B
[B
[B
aB
mB
gB
gB
aB
[B

UB

UB
[B
aB
[B
[B

UB

UB
[B
[B
aB
aB
sB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
$�B
%�B
%�B
%�B
'B
'B
'B
'B
'B
'B
'B
'B
(B
(B
'B
(B
'B
(B
(B
)B
)B
)B
)B
)B
)B
*B
*B
*B
*B
*B
+B
+B
+B
+B
+B
+B
,B
,B
-&B
-&B
-&B
-&B
-&B
-&B
-&B
.,B
.,B
.,B
.,B
/2B
/2B
/2B
/2B
/2B
08B
08B
08B
08B
08B
1>B
2DB
2DB
2DB
2DB
3KB
3KB
4QB
5WB
6]B
6]B
6]B
6]B
6]B
6]B
6]B
6]B
6]B
6]B
7cB
7cB
7cB
7cB
7cB
7cB
8iB
7cB
8iB
9oB
9oB
9oB
9oB
9oB
9oB
9oB
9oB
9oB
9oB
:vB
9oB
:vB
:vB
:vB
:vB
:vB
;|B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
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
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
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
P�B
P�B
P�B
P�B
RB
RB
RB
RB
RB
S	B
S	B
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
UB
UB
UB
UB
UB
VB
VB
VB
W"B
W"B
W"B
W"B
W"B
X(B
X(B
X(B
X(B
X(B
X(B
X(B
Y.B
X(B
Y.B
Y.B
Y.B
Y.B
Z4B
Y.B
Z4B
Z4B
[:B
[:B
Z4B
[:B
[:B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
]FB
]FB
^MB
]FB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
a_B
beB
beB
beB
beB
ckB
ckB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
dqB
dqB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
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
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
t�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
}B
}B
}B
~B
~B
~B
~B
~B
~B
~B
~B
~B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�$B
�*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.59 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200420110131              20200420110131  AO  ARCAADJP                                                                    20200420110131    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200420110131    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200420110131  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200420110131  QCF$                G�O�G�O�G�O�0               