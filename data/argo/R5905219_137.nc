CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-01-07T06:43:54Z creation;2022-01-07T06:43:56Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ``   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �$   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �(   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �0   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �p   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220107064354  20220107065213  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @ٯ���51   @ٯŎ8�@4�G�z��dT�hr�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  A�33B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D��Dy�D��D� D  D� D��D	y�D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dxy�Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ DǼ�D�  D�@ DȀ D�� D�  D�@ D�|�D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̼�D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D���D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�3D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @XQ�@�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��A��
B�B	�B�B�B!�B)�B1�B:Q�BA�BI�BQ�BY�BbQ�Bi�Bq�By�B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B�B�B���B���B���B���B���B�(�C �{Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$�{C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4�{C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CL�{CN�{CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�J>C�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��DRD��D�D��D�D��DRD�RDRD��D�D��D	RD	�RD
�D
��D�D��D�D��D�D��D�D��D�D��D%D��D�D��D%D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D%D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<�D=�D=��D>�D>��D?�D?��D@�D@��DA%DA��DB�DB��DC�DC��DD�DD��DE�DE��DFRDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg%Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�Dn%Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx�RDy�Dy��Dz�Dz��D{�D{��D|%D|��D}�D}��D~�D~��D�D��D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�)D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��)D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD��D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��)D��\D�\D�O\D��\D��)D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D���D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD��D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��)D��\D�\D�R�D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D\D��\D�\D�O\DÏ\D��\D�\D�O\Dď\D��\D�\D�O\Dŏ\D��\D�\D�O\DƏ\D��\D�\D�O\DǏ\D��)D�\D�O\Dȏ\D��\D�\D�O\DɌ)D��\D�\D�O\Dʏ\D��\D�\D�O\Dˏ\D��\D�\D�O\D̏\D��)D�\D�O\D͏\D��\D�\D�O\DΏ\D��\D�\D�O\DϏ\D��\D�\D�O\DЏ\D��)D�\D�O\Dя\D��\D�\D�O\Dҏ\D��\D�\D�O\Dӏ\D��\D�\D�O\Dԏ\D��\D�\D�R�DՏ\D��)D�\D�O\D֏\D��\D�\D�O\D׏\D��\D�\D�O\D؏\D��\D�\D�O\Dُ\D��\D�\D�O\Dڏ\D��\D�\D�O\Dۏ\D��\D�)D�O\D܏\D��\D��D�O\Dݏ\D��\D�\D�O\Dތ)D��\D�\D�O\Dߏ\D��\D��D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�R�D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�R�D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��\D��\D�\D�R�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A�"�A��A��#A���A���A���A���A���A���A���A���A���A���A���AϸRAϥ�Aϝ�Aϟ�Aϥ�Aϣ�Aϝ�AσA�l�A�hsA�hsA�jA�hsA�ffA�ffA�ffA�ffA�bNA�^5A�XA�XA�XA�XA�ZA�\)A�^5A�bNA�bNA�bNA�l�A�|�A�r�A�&�AΉ7Aͩ�A�t�A���AɋDA�jAƋDA��A�7LA+A�33A�VA�(�A��A�t�A��A�$�A��\A�I�A���A���A�ƨA�\)A��TA��A��A�5?A���A�33A���A��PA�t�A�bA�7LA��#A��A�5?A���A��A���A��A�^5A�I�A��A�1'A���A�\)A���A���A�z�A��\A���A�JA�^5A�%A�C�A�z�A��FA�`BA��/A��jA���A� �A���A���A��^A�{A�`BAt�A~A�A}"�A|(�A{��Az�Ay��Awt�Av5?Aut�As��Ao��Al��Ah(�AcAaC�A_�A]��AZ9XAX��AW/AU��ATJAS�ARM�AQ7LAO�#AN��ANbNAMƨAKdZAI��AHAE�ADjABv�A@�A<�yA:ȴA9`BA8��A7�PA6��A5+A4�9A2E�A/�A.Q�A-O�A,jA+G�A*�A(bNA'\)A&�A$�A$E�A#�A!A bA+A�
A7LA9XA�!A�AdZA�`A�A33AffA�FA33A��A~�A1AdZA^5Ax�A-AG�A��A�AbNA$�A��A33A
�A	�A	l�Av�AO�A�AE�AVAffA��A�yAr�AE�Az�Ap�A -@�n�@��@�K�@���@�@�`B@���@�@��@�p�@���@���@���@�/@�33@��@�Ĝ@�R@��T@ᙚ@�V@�j@�Z@އ+@ݑh@�r�@�
=@ف@�Ĝ@�bN@��@�\)@��T@�?}@���@�9X@ӥ�@�@ҸR@�ff@�p�@��/@��m@�l�@�"�@�@·+@͙�@��@�A�@��@���@�\)@�^5@��@�j@�dZ@��@Ɨ�@�E�@��#@ř�@�X@�&�@�Ĝ@�r�@��m@�;d@�ff@��@��h@�X@��@�1@��;@���@�|�@�33@��R@�M�@���@�/@��@��`@��u@�  @�t�@�;d@���@�-@���@��7@�X@�/@�%@�z�@�  @���@�;d@���@���@��@���@�I�@�1'@�b@��@���@��@��R@�V@�$�@�J@��^@�O�@���@�9X@�1@��
@�l�@�
=@��y@��\@�-@���@���@���@�7L@���@�r�@��;@���@�\)@��\@�M�@��@��T@��^@��h@�%@���@��@�Z@�1@��@��@�t�@�\)@�33@�
=@��@��!@��!@��+@�M�@��-@��@�X@�/@���@��j@���@���@�  @�S�@�33@�
=@�ȴ@��R@�n�@�5?@�$�@�hs@��D@���@�S�@�S�@�ȴ@�-@�O�@��@�%@�G�@��^@���@�X@�x�@��7@�%@��@��m@��;@�Q�@�j@���@�r�@��m@���@�K�@�@���@���@��!@���@�M�@���@��@��^@�p�@�7L@���@�1'@��
@��@�\)@��H@�v�@�5?@�@�`B@�&�@�%@���@��P@�33@���@��+@��y@�^5@���@�`B@���@��j@���@�Ĝ@��@��-@��@�hs@�&�@�7L@�/@���@���@��@�K�@�\)@�l�@�l�@�l�@�\)@��@�ȴ@���@�ff@�5?@��@���@�@�@���@��h@��@��@�x�@�x�@��@�x�@�/@���@��j@��`@��`@�Ĝ@��@��@��m@�@��+@�V@�E�@�{@���@��^@��-@���@�hs@�O�@�G�@�%@��9@��D@�r�@� �@���@��F@��P@�S�@��@���@��R@��R@��!@�v�@�E�@�{@��#@���@�V@��/@�z�@� �@���@��m@��F@��P@�l�@�K�@�"�@���@��!@�v�@�^5@�E�@��@�@��@���@�X@�V@�/@��@��D@�bN@�A�@�b@��@;d@~5?@}`B@}/@|��@|�j@|9X@{�@z�@zn�@z-@y��@y�7@xQ�@x1'@x1'@w�P@w+@vȴ@v@u?}@t�@t1@sC�@q��@q��@q��@qhs@pbN@o�;@o�@o|�@ol�@o+@n��@nȴ@n�+@nff@nE�@m�T@m��@l��@l9X@l9X@lj@lI�@l(�@lI�@lZ@l9X@k�m@k��@j�!@jM�@i�@i��@i�@h��@h�9@h�@h �@g�w@g�P@g;d@f��@fE�@e�@e��@e�@d�j@d1@c��@b�H@bn�@bM�@b-@a��@a%@`bN@`b@_�;@_�w@_��@_�P@_l�@_+@^�y@\�/@\(�@[��@[�F@[33@Z�@Z^5@Y��@Y�^@Yx�@Y7L@Y�@X�u@X �@W��@W��@WK�@Vȴ@Vff@U�-@U?}@T��@T��@TI�@T1@S�F@S33@S@R��@R�!@R�\@Rn�@R^5@RM�@R�@Q��@Q�@Q�#@Q��@Q��@R-@Rn�@R^5@Q�#@Q��@QG�@Q�@P�`@Pr�@O�;@O|�@N��@Nȴ@N��@N{@M��@Mp�@L��@L9X@K�@K"�@J=q@IX@H�`@H�9@HbN@H  @G�P@G+@F��@F5?@E�T@E�-@Ep�@EO�@E�@D��@DZ@C�F@C33@B�H@B^5@B-@A�^@A��@A��@Ax�@Ahs@AG�@A�@@��@@  @?K�@?+@?
=@>ȴ@>��@>�+@>ff@>@=@=��@=?}@=�@<�@<�D@<z�@<9X@;ƨ@;�@;33@:�@:n�@:�@9�#@9��@9hs@8��@8Ĝ@8A�@7�@7��@7|�@7\)@7
=@6�@6��@6�+@6V@65?@5�@5�T@5�-@5�@5?}@4��@4�D@4j@3�F@3S�@3@2�H@2��@2��@2�\@2�@1�@1��@1��@1hs@1&�@1�@0�`@0��@0�@0r�@0Q�@0b@/�@/�@/+@.�y@.�R@.��@.��@.E�@-�@-��@-�-@-��@-�h@-�h@-�@-`B@-/@-V@-V@,��@,�/@,��@,9X@,�@,1@+�m@+��@*�@*�H@*��@*�@)�#@)��@)x�@)X@)X@)G�@)&�@(�`@(�u@(A�@(b@(b@(b@'�@';d@&ȴ@&�R@&��@&$�@%��@%p�@%V@$��@$�@$�@$��@$9X@$1@#ƨ@#C�@"�@"��@"��@"�!@"~�@"^5@"-@"-@"�@!��@!��@!7L@ ��@ �u@  �@��@��@|�@l�@+@
=@
=@�@��@�+@�+@5?@�@�@?}@�@�@I�@1@��@t�@S�@"�@�H@��@M�@��@�7@hs@&�@��@Ĝ@�u@A�@��@�P@\)@+@�y@ȴ@��@ff@{@�T@�-@`B@/@V@��@z�@Z@9X@�@��@�
@ƨ@�F@�F@�F@��@��@t�@S�@o@��@��@��@n�@^5@-@�@�^@�7@G�@%@��@�`@Ĝ@�u@A�@b@b@�@��@�P@l�@\)@;d@
=@�@��@v�@v�@v�@ff@V@$�@{@{@@�@�T@�-@�h@p�@O�@/@V@�j@z�@9X@�@��@��@�m@ƨ@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�^5A�"�A��A��#A���A���A���A���A���A���A���A���A���A���A���AϸRAϥ�Aϝ�Aϟ�Aϥ�Aϣ�Aϝ�AσA�l�A�hsA�hsA�jA�hsA�ffA�ffA�ffA�ffA�bNA�^5A�XA�XA�XA�XA�ZA�\)A�^5A�bNA�bNA�bNA�l�A�|�A�r�A�&�AΉ7Aͩ�A�t�A���AɋDA�jAƋDA��A�7LA+A�33A�VA�(�A��A�t�A��A�$�A��\A�I�A���A���A�ƨA�\)A��TA��A��A�5?A���A�33A���A��PA�t�A�bA�7LA��#A��A�5?A���A��A���A��A�^5A�I�A��A�1'A���A�\)A���A���A�z�A��\A���A�JA�^5A�%A�C�A�z�A��FA�`BA��/A��jA���A� �A���A���A��^A�{A�`BAt�A~A�A}"�A|(�A{��Az�Ay��Awt�Av5?Aut�As��Ao��Al��Ah(�AcAaC�A_�A]��AZ9XAX��AW/AU��ATJAS�ARM�AQ7LAO�#AN��ANbNAMƨAKdZAI��AHAE�ADjABv�A@�A<�yA:ȴA9`BA8��A7�PA6��A5+A4�9A2E�A/�A.Q�A-O�A,jA+G�A*�A(bNA'\)A&�A$�A$E�A#�A!A bA+A�
A7LA9XA�!A�AdZA�`A�A33AffA�FA33A��A~�A1AdZA^5Ax�A-AG�A��A�AbNA$�A��A33A
�A	�A	l�Av�AO�A�AE�AVAffA��A�yAr�AE�Az�Ap�A -@�n�@��@�K�@���@�@�`B@���@�@��@�p�@���@���@���@�/@�33@��@�Ĝ@�R@��T@ᙚ@�V@�j@�Z@އ+@ݑh@�r�@�
=@ف@�Ĝ@�bN@��@�\)@��T@�?}@���@�9X@ӥ�@�@ҸR@�ff@�p�@��/@��m@�l�@�"�@�@·+@͙�@��@�A�@��@���@�\)@�^5@��@�j@�dZ@��@Ɨ�@�E�@��#@ř�@�X@�&�@�Ĝ@�r�@��m@�;d@�ff@��@��h@�X@��@�1@��;@���@�|�@�33@��R@�M�@���@�/@��@��`@��u@�  @�t�@�;d@���@�-@���@��7@�X@�/@�%@�z�@�  @���@�;d@���@���@��@���@�I�@�1'@�b@��@���@��@��R@�V@�$�@�J@��^@�O�@���@�9X@�1@��
@�l�@�
=@��y@��\@�-@���@���@���@�7L@���@�r�@��;@���@�\)@��\@�M�@��@��T@��^@��h@�%@���@��@�Z@�1@��@��@�t�@�\)@�33@�
=@��@��!@��!@��+@�M�@��-@��@�X@�/@���@��j@���@���@�  @�S�@�33@�
=@�ȴ@��R@�n�@�5?@�$�@�hs@��D@���@�S�@�S�@�ȴ@�-@�O�@��@�%@�G�@��^@���@�X@�x�@��7@�%@��@��m@��;@�Q�@�j@���@�r�@��m@���@�K�@�@���@���@��!@���@�M�@���@��@��^@�p�@�7L@���@�1'@��
@��@�\)@��H@�v�@�5?@�@�`B@�&�@�%@���@��P@�33@���@��+@��y@�^5@���@�`B@���@��j@���@�Ĝ@��@��-@��@�hs@�&�@�7L@�/@���@���@��@�K�@�\)@�l�@�l�@�l�@�\)@��@�ȴ@���@�ff@�5?@��@���@�@�@���@��h@��@��@�x�@�x�@��@�x�@�/@���@��j@��`@��`@�Ĝ@��@��@��m@�@��+@�V@�E�@�{@���@��^@��-@���@�hs@�O�@�G�@�%@��9@��D@�r�@� �@���@��F@��P@�S�@��@���@��R@��R@��!@�v�@�E�@�{@��#@���@�V@��/@�z�@� �@���@��m@��F@��P@�l�@�K�@�"�@���@��!@�v�@�^5@�E�@��@�@��@���@�X@�V@�/@��@��D@�bN@�A�@�b@��@;d@~5?@}`B@}/@|��@|�j@|9X@{�@z�@zn�@z-@y��@y�7@xQ�@x1'@x1'@w�P@w+@vȴ@v@u?}@t�@t1@sC�@q��@q��@q��@qhs@pbN@o�;@o�@o|�@ol�@o+@n��@nȴ@n�+@nff@nE�@m�T@m��@l��@l9X@l9X@lj@lI�@l(�@lI�@lZ@l9X@k�m@k��@j�!@jM�@i�@i��@i�@h��@h�9@h�@h �@g�w@g�P@g;d@f��@fE�@e�@e��@e�@d�j@d1@c��@b�H@bn�@bM�@b-@a��@a%@`bN@`b@_�;@_�w@_��@_�P@_l�@_+@^�y@\�/@\(�@[��@[�F@[33@Z�@Z^5@Y��@Y�^@Yx�@Y7L@Y�@X�u@X �@W��@W��@WK�@Vȴ@Vff@U�-@U?}@T��@T��@TI�@T1@S�F@S33@S@R��@R�!@R�\@Rn�@R^5@RM�@R�@Q��@Q�@Q�#@Q��@Q��@R-@Rn�@R^5@Q�#@Q��@QG�@Q�@P�`@Pr�@O�;@O|�@N��@Nȴ@N��@N{@M��@Mp�@L��@L9X@K�@K"�@J=q@IX@H�`@H�9@HbN@H  @G�P@G+@F��@F5?@E�T@E�-@Ep�@EO�@E�@D��@DZ@C�F@C33@B�H@B^5@B-@A�^@A��@A��@Ax�@Ahs@AG�@A�@@��@@  @?K�@?+@?
=@>ȴ@>��@>�+@>ff@>@=@=��@=?}@=�@<�@<�D@<z�@<9X@;ƨ@;�@;33@:�@:n�@:�@9�#@9��@9hs@8��@8Ĝ@8A�@7�@7��@7|�@7\)@7
=@6�@6��@6�+@6V@65?@5�@5�T@5�-@5�@5?}@4��@4�D@4j@3�F@3S�@3@2�H@2��@2��@2�\@2�@1�@1��@1��@1hs@1&�@1�@0�`@0��@0�@0r�@0Q�@0b@/�@/�@/+@.�y@.�R@.��@.��@.E�@-�@-��@-�-@-��@-�h@-�h@-�@-`B@-/@-V@-V@,��@,�/@,��@,9X@,�@,1@+�m@+��@*�@*�H@*��@*�@)�#@)��@)x�@)X@)X@)G�@)&�@(�`@(�u@(A�@(b@(b@(b@'�@';d@&ȴ@&�R@&��@&$�@%��@%p�@%V@$��@$�@$�@$��@$9X@$1@#ƨ@#C�@"�@"��@"��@"�!@"~�@"^5@"-@"-@"�@!��@!��@!7L@ ��@ �u@  �@��@��@|�@l�@+@
=@
=@�@��@�+@�+@5?@�@�@?}@�@�@I�@1@��@t�@S�@"�@�H@��@M�@��@�7@hs@&�@��@Ĝ@�u@A�@��@�P@\)@+@�y@ȴ@��@ff@{@�T@�-@`B@/@V@��@z�@Z@9X@�@��@�
@ƨ@�F@�F@�F@��@��@t�@S�@o@��@��@��@n�@^5@-@�@�^@�7@G�@%@��@�`@Ĝ@�u@A�@b@b@�@��@�P@l�@\)@;d@
=@�@��@v�@v�@v�@ff@V@$�@{@{@@�@�T@�-@�h@p�@O�@/@V@�j@z�@9X@�@��@��@�m@ƨ@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
�B
�B
�B
�B
�/B
�BB
�NB
�`B
�mB
�sB
�yB
�mB
�`B
�ZB
�`B
�yB
�B
�yB
�fB
�NB
�NB
�NB
�NB
�TB
�TB
�ZB
�ZB
�`B
�mB
�mB
�sB
�sB
�sB
�yB
�B
�B
�B
�B
�B
��B
��B%BDB�B7LBO�Bm�B}�B�B�=B��B�RBÖB�B��BJB"�B0!B49B9XB=qBF�BL�BVB[#B^5B]/B_;BaHB^5B]/B[#BZBZBffBhsBdZBJ�B/B�B{B\BJB+B��B��B��B�B�BB�jB�LB��B�uB�7B~�Bv�By�Br�B`BBJ�B7LB�B%B
��B
�`B
ƨB
�B
��B
�+B
k�B
bNB
\)B
P�B
H�B
A�B
>wB
8RB
0!B
%�B
�B
�B

=B	��B	�;B	ǮB	��B	��B	�\B	�PB	y�B	r�B	k�B	dZB	^5B	W
B	S�B	N�B	I�B	D�B	B�B	@�B	8RB	-B	(�B	�B	�B	JB	+B��B�B�B�sB�TB�HB�#B�B��B��BĜB��B�}B�wB�XB�RB�3B�-B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�?B�FB�?B��B�}B�qB�jB��B��B�}B�wB�qB��B�}B�}B��BBĜBÖBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�B�B�)B�/B�/B�/B�5B�NB�ZB�ZB�`B�fB�mB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B	1B		7B	
=B	JB	VB	\B	oB	oB	{B	�B	�B	�B	�B	 �B	"�B	#�B	&�B	(�B	+B	,B	,B	-B	0!B	1'B	33B	5?B	6FB	7LB	8RB	<jB	?}B	@�B	C�B	G�B	I�B	J�B	K�B	L�B	M�B	P�B	R�B	T�B	VB	W
B	[#B	^5B	`BB	aHB	bNB	cTB	cTB	e`B	hsB	jB	l�B	m�B	n�B	p�B	q�B	t�B	y�B	y�B	y�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�=B	�DB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	B	ĜB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ŢB	B	B	ÖB	B	ÖB	ĜB	ĜB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�HB	�NB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
%B
+B
1B
	7B
	7B
	7B
1B
+B
	7B
PB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
'�B
(�B
(�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
(�B
,B
-B
.B
/B
/B
/B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
:^B
:^B
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
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
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
E�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
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
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
N�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
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
VB
T�B
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
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
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
cTB
cTB
cTB
dZB
dZB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
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
n�B
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
r�B
r�B
r�B
r�B
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
w�B
w�B
x�B
x�B
x�B
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
~�B
~�B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
�B
�B
�B
�B
�/B
�BB
�NB
�`B
�mB
�sB
�yB
�mB
�`B
�ZB
�`B
�yB
�B
�yB
�fB
�NB
�NB
�NB
�NB
�TB
�TB
�ZB
�ZB
�`B
�mB
�mB
�sB
�sB
�sB
�yB
�B
�B
�B
�B
�B
��B
��B%BDB�B7LBO�Bm�B}�B�B�=B��B�RBÖB�B��BJB"�B0!B49B9XB=qBF�BL�BVB[#B^5B]/B_;BaHB^5B]/B[#BZBZBffBhsBdZBJ�B/B�B{B\BJB+B��B��B��B�B�BB�jB�LB��B�uB�7B~�Bv�By�Br�B`BBJ�B7LB�B%B
��B
�`B
ƨB
�B
��B
�+B
k�B
bNB
\)B
P�B
H�B
A�B
>wB
8RB
0!B
%�B
�B
�B

=B	��B	�;B	ǮB	��B	��B	�\B	�PB	y�B	r�B	k�B	dZB	^5B	W
B	S�B	N�B	I�B	D�B	B�B	@�B	8RB	-B	(�B	�B	�B	JB	+B��B�B�B�sB�TB�HB�#B�B��B��BĜB��B�}B�wB�XB�RB�3B�-B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�?B�FB�?B��B�}B�qB�jB��B��B�}B�wB�qB��B�}B�}B��BBĜBÖBŢBƨBȴBɺB��B��B��B��B��B��B��B��B�B�B�)B�/B�/B�/B�5B�NB�ZB�ZB�`B�fB�mB�sB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B	1B		7B	
=B	JB	VB	\B	oB	oB	{B	�B	�B	�B	�B	 �B	"�B	#�B	&�B	(�B	+B	,B	,B	-B	0!B	1'B	33B	5?B	6FB	7LB	8RB	<jB	?}B	@�B	C�B	G�B	I�B	J�B	K�B	L�B	M�B	P�B	R�B	T�B	VB	W
B	[#B	^5B	`BB	aHB	bNB	cTB	cTB	e`B	hsB	jB	l�B	m�B	n�B	p�B	q�B	t�B	y�B	y�B	y�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�=B	�DB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�XB	�XB	�XB	�XB	�XB	�^B	�^B	�dB	B	ĜB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ŢB	B	B	ÖB	B	ÖB	ĜB	ĜB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�HB	�NB	�ZB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
%B
%B
+B
1B
	7B
	7B
	7B
1B
+B
	7B
PB
VB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
#�B
#�B
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
&�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
'�B
(�B
(�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
'�B
(�B
,B
-B
.B
/B
/B
/B
.B
.B
/B
0!B
0!B
0!B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
:^B
:^B
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
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
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
E�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
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
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
N�B
O�B
O�B
O�B
O�B
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
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
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
VB
T�B
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
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
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
cTB
cTB
cTB
dZB
dZB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
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
n�B
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
r�B
r�B
r�B
r�B
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
w�B
w�B
x�B
x�B
x�B
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
~�B
~�B
� B
� B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220107154207  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220107064354  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220107064355  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20220107064355  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20220107064355  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20220107064355  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20220107064355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20220107064355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20220107064356  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20220107064356                      G�O�G�O�G�O�                JA  ARUP                                                                        20220107065213                      G�O�G�O�G�O�                