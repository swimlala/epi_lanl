CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-05T21:37:13Z creation;2020-01-05T21:37:17Z conversion to V3.1;2023-06-29T05:50:18Z update;     
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
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200105213713  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_201                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @����6� 1   @����7 @7�YJ����b�=�K^1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�  B   B��B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]y�D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Q�@�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A�
=A�
=A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2aHC4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|�{C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J=C�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
�D�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D%D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\%D\��D]�D]�RD^�D^�D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D\D��\D�\D�O\DÏ\D��\D�\D�O\Dď\D��\D�\D�O\Dŏ\D��\D�\D�O\DƏ\D��\D�\D�O\DǏ\D��\D�\D�O\Dȏ\D��\D�\D�O\Dɏ\D��\D�\D�O\Dʏ\D��\D�\D�O\Dˏ\D��\D�\D�O\D̏\D��\D�\D�O\D͏\D��\D�\D�O\DΏ\D��\D�\D�O\DϏ\D��\D�\D�O\DЏ\D��\D�\D�O\Dя\D��\D�\D�O\Dҏ\D��\D�\D�O\Dӏ\D��\D�\D�O\Dԏ\D��\D�\D�O\DՏ\D��\D�\D�O\D֏\D��\D�\D�O\D׏\D��\D�\D�O\D؏\D��\D�\D�O\Dُ\D��\D�\D�R�Dڏ\D��\D�\D�O\Dۏ\D��\D�\D�O\D܏\D��\D�\D�O\Dݏ\D��\D�\D�O\Dޏ\D��\D�\D�O\Dߏ\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�X�D�x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aǲ-Aǲ-AǸRAǸRAǸRAǸRAǺ^AǶFAǶFAǶFAǸRAǸRAǸRAǼjA�A���A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��#A��/A��/A��#A��/A��/A���AǸRA�ffA�p�A��A��/A�;dA��A��jA�G�A�ĜA���A��A��HA�K�A��yA�hsA�`BA�r�A���A���A�9XA���A�l�A��A���A��A� �A��wA��A�~�A���A��-A��A��uA�1'A��FA��DA�C�A��mA���A�+A���A�I�A�A�
=A�v�A�33A��RA�x�A���A�K�A� �A�O�A�M�A�z�A��hA��A���A�z�A��DA��A��TA��A�ZA�ĜA�A33A|~�A{�mAw�At9XAo�Aj�DAf��Af(�Ad^5A_�PA[�TA[33AXVAUl�AR��AP�yAO�wAO"�AM`BAL�AK�7AJE�AH�9AE�AD�AC��AA�A@��A?XA>�A>r�A<�yA;dZA;
=A:v�A7��A6�+A6=qA5��A5K�A4��A4�DA4�A3+A1��A.��A,ffA+�A+/A*n�A)�A'?}A&E�A&bA%/A#+A!��A!G�A ��A VA��AK�A��A�A?}A�
AO�A�RA1'A;dAA7LAbNA��AO�A%AZA�A�AA�AAv�A�A�+A�yA1'A��A
�HA	��AjAM�A�wA��AXA��Ar�A`BA  �@�ff@���@�x�@�z�@��F@��@�bN@�t�@��T@�@�P@�R@���@�j@�O�@�b@�l�@ꗍ@�=q@�7@�w@�ȴ@�^5@�{@�p�@�O�@�%@�9@�@�o@�^5@�p�@�r�@�o@ݩ�@�(�@ۍP@�ȴ@�V@؋D@ץ�@ְ!@Ցh@�hs@�1'@�S�@��y@Ѻ^@ёh@д9@�+@���@�5?@� �@��@�5?@ɩ�@�bN@�t�@�33@�n�@���@őh@�G�@�r�@�@���@�K�@�@��R@��@�x�@�r�@���@��@��w@�33@�p�@��@�z�@�A�@�+@���@��@���@�%@�S�@��R@�A�@�/@�/@��@���@��;@��H@���@��@�;d@���@�{@�v�@�9X@�&�@�z�@�Ĝ@�Ĝ@��@�dZ@�n�@��u@�+@���@�~�@�=q@��T@�5?@��@��#@��h@�ff@��@�\)@���@�dZ@�C�@�o@���@�~�@�^5@��@���@�x�@���@��m@�l�@�~�@��@��9@���@���@��@��h@��@�Ĝ@�r�@���@�|�@�;d@�@��@��@��H@��@��H@��\@�ff@�$�@�`B@� �@�;d@��@�t�@���@�\)@�l�@���@�V@�J@��-@�%@�%@��@��@��@�b@���@��y@�ff@��@��y@�$�@���@��y@�
=@�o@�;d@�33@�5?@��\@��H@�~�@���@�;d@�+@�v�@��@��+@�v�@��T@��@�{@�ff@�V@�5?@��T@��-@��7@�G�@��u@�1'@�@�Ĝ@��m@���@�l�@���@�ff@�$�@�5?@�^5@���@��@�`B@���@�Q�@�  @� �@���@�;d@�33@�V@�%@���@��@�Z@�  @��w@�l�@�;d@�
=@���@��R@�
=@�o@���@�@���@�x�@�@���@�p�@���@�z�@�Q�@��@��m@�ƨ@���@�S�@��@��y@��!@�~�@�V@�=q@�-@�J@��@��7@�hs@�/@���@�z�@�bN@� �@��@�1@��m@���@��w@���@�l�@��@��@��@���@�ff@�5?@�{@���@��-@���@�p�@�&�@�%@���@��/@��j@�Z@� �@�b@�;@�@|�@K�@~�y@~@}��@}O�@|�@|��@{��@{��@{S�@{33@{@z�!@z~�@zM�@y��@yhs@x��@xQ�@xA�@w�@wl�@v�y@v�R@v��@vff@u�T@u/@u�@uV@t��@tz�@tZ@t1@s�m@sS�@s@r��@r��@r~�@rM�@q�@q��@qx�@q7L@p�`@p�9@pr�@o�w@o+@o
=@n�@nȴ@n�R@n��@n��@n�+@nV@n$�@m�@m@m?}@l�j@l9X@kt�@k"�@k@j��@j^5@i�@i�#@i�7@i&�@h��@hĜ@h�u@hA�@h �@g��@gK�@g
=@f��@fȴ@f5?@e`B@d�/@d��@d�@dz�@d(�@c�m@c�
@cS�@c@b��@b�\@b-@a�#@aX@`��@`�u@`r�@` �@_�w@_|�@_K�@_K�@_�@^�@^V@]�-@]?}@]V@\�j@\I�@[��@[��@[S�@[o@Z�H@Z�\@Z~�@Z-@Y��@X��@X��@Xr�@X1'@X  @W�P@W\)@W;d@W�@W�@V�@V5?@U�@U@U�-@U?}@T�D@TZ@T9X@T(�@T(�@S�m@S�F@S��@SS�@R�H@RM�@RJ@Q�#@QX@Q�@P�@Pb@O;d@N�R@N5?@M��@M/@L�@LZ@L9X@K�
@KdZ@K"�@J��@J~�@JM�@I�@I�@H��@HĜ@Hr�@HQ�@Hb@G��@G\)@Fȴ@F5?@F@E��@EV@D��@D�@Dz�@C��@C"�@B��@B=q@BJ@A��@Ahs@@��@@Ĝ@@r�@@A�@@1'@?�@?�w@?�P@?K�@>��@>ff@>5?@>5?@>5?@>$�@=�-@=/@<�/@<��@<�D@<9X@<1@;�m@;�m@;ƨ@;t�@;C�@:�@:��@:=q@:J@9��@9�^@9��@9�^@9��@9��@9��@9X@97L@9%@8��@8bN@8b@7�@7l�@7;d@6�@6@5��@5�h@5/@4�@4�j@4�D@49X@3�m@3�F@3S�@2��@2n�@2�\@2-@1�@1x�@1�@0��@0�`@0��@0�9@0�@0b@/��@/��@/+@.�R@.V@-�T@-��@-O�@-�@-V@,�@,��@,z�@,9X@,�@+��@+�F@+t�@+"�@*�@*�!@*�\@*n�@*M�@*-@*J@)�#@)hs@)7L@(��@(�@(1'@(  @(  @'��@'�@'�w@'�P@'+@&��@&��@&V@&@%@%?}@%�@$�/@$�@$j@$9X@$�@#�m@#ƨ@#�F@#�F@#�F@#�@#dZ@#@"��@"~�@"M�@"�@"M�@"�@!��@!��@!G�@!�@!%@ �`@ �`@ ��@ �9@ r�@ A�@  �@�;@�;@�@K�@
=@ȴ@ff@ff@5?@@�@O�@�@�@z�@I�@�@�
@t�@33@"�@"�@�H@��@��@~�@�@�#@�^@��@��@X@7L@7L@�@�`@�u@1'@b@��@��@\)@�@��@ȴ@�+@V@E�@$�@{@�T@�-@�h@p�@`B@?}@��@�/@��@�D@z�@z�@Z@(�@1@��@ƨ@��@�@dZ@33@o@@�@�!@�\@~�@^5@=q@-@-@��@x�@hs@&�@�`@Ĝ@�9@�u@�@r�@bN@Q�@  @��@��@�P@l�@\)@+@�@�@�@ȴ@ȴ@��@ff@@@��@p�@?}@p�@�@`B@?}@/@�/@��@z�@j@1@��@dZ@33@
�H@
��@
��@
�!@
��@
M�@
-@	��@	hs@	G�@	�@�`@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aǲ-Aǲ-AǸRAǸRAǸRAǸRAǺ^AǶFAǶFAǶFAǸRAǸRAǸRAǼjA�A���A�A�ĜA�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��#A��/A��/A��#A��/A��/A���AǸRA�ffA�p�A��A��/A�;dA��A��jA�G�A�ĜA���A��A��HA�K�A��yA�hsA�`BA�r�A���A���A�9XA���A�l�A��A���A��A� �A��wA��A�~�A���A��-A��A��uA�1'A��FA��DA�C�A��mA���A�+A���A�I�A�A�
=A�v�A�33A��RA�x�A���A�K�A� �A�O�A�M�A�z�A��hA��A���A�z�A��DA��A��TA��A�ZA�ĜA�A33A|~�A{�mAw�At9XAo�Aj�DAf��Af(�Ad^5A_�PA[�TA[33AXVAUl�AR��AP�yAO�wAO"�AM`BAL�AK�7AJE�AH�9AE�AD�AC��AA�A@��A?XA>�A>r�A<�yA;dZA;
=A:v�A7��A6�+A6=qA5��A5K�A4��A4�DA4�A3+A1��A.��A,ffA+�A+/A*n�A)�A'?}A&E�A&bA%/A#+A!��A!G�A ��A VA��AK�A��A�A?}A�
AO�A�RA1'A;dAA7LAbNA��AO�A%AZA�A�AA�AAv�A�A�+A�yA1'A��A
�HA	��AjAM�A�wA��AXA��Ar�A`BA  �@�ff@���@�x�@�z�@��F@��@�bN@�t�@��T@�@�P@�R@���@�j@�O�@�b@�l�@ꗍ@�=q@�7@�w@�ȴ@�^5@�{@�p�@�O�@�%@�9@�@�o@�^5@�p�@�r�@�o@ݩ�@�(�@ۍP@�ȴ@�V@؋D@ץ�@ְ!@Ցh@�hs@�1'@�S�@��y@Ѻ^@ёh@д9@�+@���@�5?@� �@��@�5?@ɩ�@�bN@�t�@�33@�n�@���@őh@�G�@�r�@�@���@�K�@�@��R@��@�x�@�r�@���@��@��w@�33@�p�@��@�z�@�A�@�+@���@��@���@�%@�S�@��R@�A�@�/@�/@��@���@��;@��H@���@��@�;d@���@�{@�v�@�9X@�&�@�z�@�Ĝ@�Ĝ@��@�dZ@�n�@��u@�+@���@�~�@�=q@��T@�5?@��@��#@��h@�ff@��@�\)@���@�dZ@�C�@�o@���@�~�@�^5@��@���@�x�@���@��m@�l�@�~�@��@��9@���@���@��@��h@��@�Ĝ@�r�@���@�|�@�;d@�@��@��@��H@��@��H@��\@�ff@�$�@�`B@� �@�;d@��@�t�@���@�\)@�l�@���@�V@�J@��-@�%@�%@��@��@��@�b@���@��y@�ff@��@��y@�$�@���@��y@�
=@�o@�;d@�33@�5?@��\@��H@�~�@���@�;d@�+@�v�@��@��+@�v�@��T@��@�{@�ff@�V@�5?@��T@��-@��7@�G�@��u@�1'@�@�Ĝ@��m@���@�l�@���@�ff@�$�@�5?@�^5@���@��@�`B@���@�Q�@�  @� �@���@�;d@�33@�V@�%@���@��@�Z@�  @��w@�l�@�;d@�
=@���@��R@�
=@�o@���@�@���@�x�@�@���@�p�@���@�z�@�Q�@��@��m@�ƨ@���@�S�@��@��y@��!@�~�@�V@�=q@�-@�J@��@��7@�hs@�/@���@�z�@�bN@� �@��@�1@��m@���@��w@���@�l�@��@��@��@���@�ff@�5?@�{@���@��-@���@�p�@�&�@�%@���@��/@��j@�Z@� �@�b@�;@�@|�@K�@~�y@~@}��@}O�@|�@|��@{��@{��@{S�@{33@{@z�!@z~�@zM�@y��@yhs@x��@xQ�@xA�@w�@wl�@v�y@v�R@v��@vff@u�T@u/@u�@uV@t��@tz�@tZ@t1@s�m@sS�@s@r��@r��@r~�@rM�@q�@q��@qx�@q7L@p�`@p�9@pr�@o�w@o+@o
=@n�@nȴ@n�R@n��@n��@n�+@nV@n$�@m�@m@m?}@l�j@l9X@kt�@k"�@k@j��@j^5@i�@i�#@i�7@i&�@h��@hĜ@h�u@hA�@h �@g��@gK�@g
=@f��@fȴ@f5?@e`B@d�/@d��@d�@dz�@d(�@c�m@c�
@cS�@c@b��@b�\@b-@a�#@aX@`��@`�u@`r�@` �@_�w@_|�@_K�@_K�@_�@^�@^V@]�-@]?}@]V@\�j@\I�@[��@[��@[S�@[o@Z�H@Z�\@Z~�@Z-@Y��@X��@X��@Xr�@X1'@X  @W�P@W\)@W;d@W�@W�@V�@V5?@U�@U@U�-@U?}@T�D@TZ@T9X@T(�@T(�@S�m@S�F@S��@SS�@R�H@RM�@RJ@Q�#@QX@Q�@P�@Pb@O;d@N�R@N5?@M��@M/@L�@LZ@L9X@K�
@KdZ@K"�@J��@J~�@JM�@I�@I�@H��@HĜ@Hr�@HQ�@Hb@G��@G\)@Fȴ@F5?@F@E��@EV@D��@D�@Dz�@C��@C"�@B��@B=q@BJ@A��@Ahs@@��@@Ĝ@@r�@@A�@@1'@?�@?�w@?�P@?K�@>��@>ff@>5?@>5?@>5?@>$�@=�-@=/@<�/@<��@<�D@<9X@<1@;�m@;�m@;ƨ@;t�@;C�@:�@:��@:=q@:J@9��@9�^@9��@9�^@9��@9��@9��@9X@97L@9%@8��@8bN@8b@7�@7l�@7;d@6�@6@5��@5�h@5/@4�@4�j@4�D@49X@3�m@3�F@3S�@2��@2n�@2�\@2-@1�@1x�@1�@0��@0�`@0��@0�9@0�@0b@/��@/��@/+@.�R@.V@-�T@-��@-O�@-�@-V@,�@,��@,z�@,9X@,�@+��@+�F@+t�@+"�@*�@*�!@*�\@*n�@*M�@*-@*J@)�#@)hs@)7L@(��@(�@(1'@(  @(  @'��@'�@'�w@'�P@'+@&��@&��@&V@&@%@%?}@%�@$�/@$�@$j@$9X@$�@#�m@#ƨ@#�F@#�F@#�F@#�@#dZ@#@"��@"~�@"M�@"�@"M�@"�@!��@!��@!G�@!�@!%@ �`@ �`@ ��@ �9@ r�@ A�@  �@�;@�;@�@K�@
=@ȴ@ff@ff@5?@@�@O�@�@�@z�@I�@�@�
@t�@33@"�@"�@�H@��@��@~�@�@�#@�^@��@��@X@7L@7L@�@�`@�u@1'@b@��@��@\)@�@��@ȴ@�+@V@E�@$�@{@�T@�-@�h@p�@`B@?}@��@�/@��@�D@z�@z�@Z@(�@1@��@ƨ@��@�@dZ@33@o@@�@�!@�\@~�@^5@=q@-@-@��@x�@hs@&�@�`@Ĝ@�9@�u@�@r�@bN@Q�@  @��@��@�P@l�@\)@+@�@�@�@ȴ@ȴ@��@ff@@@��@p�@?}@p�@�@`B@?}@/@�/@��@z�@j@1@��@dZ@33@
�H@
��@
��@
�!@
��@
M�@
-@	��@	hs@	G�@	�@�`@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B;dB<jBA�BR�BbNBffBv�B�B��B�B��B�B	7B�B/B7LB:^B:^BC�BH�BG�BQ�BQ�BP�BP�BP�BS�BL�BE�BA�B5?B"�B�BuB�B)�B!�B+B��B�B#�B%B�B��B�XB�!B�B�B�!B�B��B��B~�Bl�BXBuB
��B
��B
ÖB
��B
�PB
bNB
"�B
=qB
5?B
�B

=B	�B	��B	�?B	�uB	w�B	n�B	ffB	K�B	2-B	(�B	�B	JB��B�B�B�yB�NB�#B�
B��BɺB�}B�FB�'B�B��B��B��B��B��B��B��B��B��B�VB�JB�DB�=B�=B�7B�1B�%B�B�Bv�Bn�Bm�Bq�Bt�Bk�Bk�Bm�Bo�Bm�Be`BiyBhsBgmBe`BhsBe`BcTBe`B]/BXBYBYB\)BYBT�BS�BW
BW
BXBW
BT�BQ�BO�BJ�BG�BF�BE�BB�B>wB<jB:^B6FB1'B49B/B49B/B/B1'B33B/B0!B/B7LB5?B33B0!B,B+B,B)�B)�B)�B)�B(�B+B+B0!B1'B33B49B6FB49B49B5?B:^B:^B:^B;dB=qB=qB?}BD�BD�BE�BG�BL�BK�BK�BL�BK�BM�BM�BP�BQ�BVBW
BW
BYB[#B^5B\)Bk�Bm�BiyBl�Bm�Bs�Bx�Bx�Bz�Bz�Bx�By�B|�B}�B� B�B�B�B�B�B�1B�7B�+B�+B�+B�DB�JB�JB�PB�\B�\B�uB��B��B��B�{B�{B��B�'B�dBƨB��BȴBŢBB�}B��BŢBƨB�wBǮB�B�B�;B�TB�HB�;B�/B�
B��B�B�B�#B�;B�mB�B�B��B	B	
=B	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	/B	:^B	<jB	9XB	=qB	K�B	YB	\)B	]/B	`BB	bNB	cTB	dZB	e`B	e`B	ffB	e`B	iyB	jB	m�B	o�B	p�B	p�B	p�B	p�B	o�B	n�B	n�B	q�B	w�B	}�B	}�B	�B	�B	�B	�B	�%B	�+B	�1B	�DB	�=B	�DB	�VB	�VB	�DB	�+B	�B	y�B	v�B	x�B	�B	�B	�%B	�1B	�7B	�7B	�JB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�FB	�LB	�RB	�RB	�RB	�LB	�-B	�!B	�!B	�!B	�B	�B	�!B	�3B	�FB	�qB	�jB	�^B	�RB	�LB	�FB	�LB	�XB	�RB	�dB	�dB	�RB	�FB	�LB	�RB	�XB	�dB	�qB	�qB	�wB	��B	B	ŢB	ƨB	ǮB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
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

=B

=B

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
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
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
�B
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
�B
�B
�B
�B
�B
 �B
!�B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
)�B
)�B
+B
+B
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
8RB
8RB
8RB
9XB
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
;dB
;dB
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
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
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
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
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
K�B
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
M�B
M�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
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
XB
XB
XB
YB
XB
XB
XB
XB
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
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
\)B
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
_;B
_;B
`BB
`BB
`BB
`BB
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
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
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
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
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
m�B
n�B
n�B
n�B
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
q�B
q�B
q�B
q�B
q�B
q�B
q�B
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
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
�B
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
��B
��B
��B
��B
��B
��B
�B
� B
�IB
��B
��B
��B
�tB;�B=BB�BS�Bb�BgmBxB�+B�eB�B��B��B
rB�B0�B88B;�B=VBF%BJ#BJ�BS�BR�BQ�BR BR�BV�BN�BG�BC�B8B%�B BBOB*�B#nBKB��B�B&�B�B�B�&B�DB��B�oB��B�|B�!B�zB�1B��BqB^jB
B �B
յB
ǔB
�sB
�TB
f�B
#�B
@ B
7�B
�B
B	�TB	��B	�*B	�$B	y>B	q�B	kkB	OvB	3�B	,qB	#B	(B	B�B�B�QB�B�B��B�&B̳B��B��B�B��B�0B�zB��B��B�4B�dB��B�OB��B��B��B��B��B��B��B��B�KB�fB��Bw�Bo5Bn�BshBv�Bl�BlBo Bq�Bo Be�Bi�Bi*Bh$Be�BiDBfLBd@Bf�B]�BX�BY�BZQB]�BY�BU�BT�BWYBWsBX�BW�BU�BSBQ4BKxBH�BG�BG_BCaB?.B=�B<B8B3hB4�B0;B5�B/�B/�B2|B4�B0B0UB/iB7�B5�B49B1B,�B+�B,�B*B*eB*�B+B*�B+�B+QB0�B1vB3�B5%B6�B4nB4TB5tB:DB:xB:�B;�B=�B=�B@BEBEmBFtBH�BM6BLJBL�BMBL0BNVBNpBQBR�BVmBWYBW�BYKB[�B^�B\CBlqBn�Bi�Bl�Bm�BtTBy$Bx�B{B{Bx�Bz*B}�B~�B�UB��B�'B�3B�mB��B��B�lB�B�+B��B�B�~B�dB�jB��B�(B�uB�+B��B�$B�aB�uB�'B��B��B��BˬB�lB�tB�GB��B��BƎB��B�qB�+B�9B��B�;B�B�B��B�B��B�,B�B��B�#B��B�mB�kB�hB�B	 �B		�B	(B	sB	�B	�B	�B	�B	�B	�B	�B	pB	#�B	.}B	:xB	<jB	8�B	<PB	J�B	X�B	[�B	]IB	`BB	bhB	cnB	d�B	e`B	eFB	f2B	eFB	iDB	jeB	mCB	oiB	p�B	p�B	p�B	qB	p!B	n�B	ncB	qAB	w�B	}�B	}�B	� B	�-B	�-B	�3B	�YB	�B	��B	�^B	�rB	�B	�pB	��B	��B	��B	�B	z*B	v�B	x8B	��B	��B	��B	�1B	�lB	��B	��B	�:B	� B	�,B	��B	��B	��B	�OB	��B	��B	��B	��B	��B	��B	��B	�-B	�B	�+B	�fB	��B	��B	�$B	�RB	�|B	�;B	�!B	�;B	�B	�B	��B	��B	�B	��B	��B	��B	�RB	�2B	�B	��B	�rB	�lB	��B	��B	�lB	�+B	�LB	�RB	�XB	�dB	�qB	�VB	�]B	�4B	�AB	ňB	��B	��B	��B	�zB	�KB	˒B	͹B	�B	͹B	̳B	͹B	οB	ϫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�)B	�IB	�5B	�B	�BB	�B	�B	�B	�:B	�:B	�:B	�@B	�FB	�LB	�RB	�mB	�sB	�yB	�eB	�B	�QB	�QB	�kB	�B	�]B	�cB	�}B	�B	�B	�oB	��B	�vB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
B
B
B
B
B
�B
B
B
B
KB
	B

#B

#B

	B
	�B

	B

#B

	B

#B
B
B
B
)B
JB
PB
jB
<B
BB
(B
BB
\B
.B
HB
.B
HB
4B
4B
4B
4B
NB
hB
TB
:B
TB
�B
uB
[B
[B
[B
aB
aB
FB
gB
gB
mB
SB
SB
mB
�B
mB
sB
YB
sB
sB
_B
yB
_B
_B
B
B
B
�B
�B
�B
�B
�B
�B
�B
xB
�B
�B
~B
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
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
)�B
)�B
+B
*�B
+�B
+�B
,B
-B
,�B
,�B
-�B
-�B
.B
/B
.�B
/�B
/�B
0B
0B
0�B
1B
1B
2-B
1�B
2B
3B
4B
4B
4B
49B
5?B
6FB
6FB
72B
72B
72B
88B
8B
8B
9>B
9>B
9>B
9>B
9>B
:*B
:DB
:^B
;JB
;0B
<B
;0B
;dB
<jB
=<B
=VB
=<B
=VB
=<B
=<B
=<B
>]B
>BB
=VB
=VB
>BB
>]B
>BB
>BB
?HB
?HB
@OB
AUB
A;B
AoB
AUB
BuB
B[B
B[B
B[B
B[B
BuB
BuB
C{B
C�B
C�B
CaB
C{B
C{B
DgB
D�B
D�B
D�B
DgB
E�B
E�B
E�B
FtB
G_B
G�B
H�B
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
K�B
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
M�B
M�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
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
U�B
U�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
ZB
ZB
[	B
[	B
[	B
Z�B
Z�B
[	B
Z�B
Z�B
ZB
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
]B
\B
]B
]B
^B
^B
^B
^B
]�B
^B
^B
^B
_!B
_!B
`'B
`'B
`'B
`'B
aB
aB
aB
b4B
bB
b4B
b4B
b4B
b4B
b4B
b4B
cB
c:B
c:B
c:B
c B
c B
dB
d&B
eFB
eFB
eFB
fB
fLB
fLB
f2B
fLB
fLB
fLB
g8B
gRB
g8B
gRB
h$B
hXB
h>B
hXB
iDB
i_B
iDB
i_B
jeB
jeB
jeB
jeB
k6B
kQB
k6B
lWB
lqB
lWB
lqB
lqB
lWB
lWB
m]B
m]B
mCB
m]B
m]B
mwB
nIB
ncB
ncB
oiB
oiB
oiB
oiB
oiB
oiB
pUB
p�B
p�B
q�B
q�B
qvB
q�B
qvB
q�B
q�B
qvB
qvB
qvB
qvB
qvB
q�B
q�B
q�B
qvB
r|B
r�B
r�B
r|B
r|B
r|B
r|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�m<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.48(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001110039492020011100394920200111003949202306231719592023062317195920230623171959202001120027182020011200271820200112002718  JA  ARFMdecpA19c                                                                20200106063712  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200105213713  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200105213716  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200105213716  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200105213717  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200105213717  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200105213717  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200105213717  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200105213717  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200105213717                      G�O�G�O�G�O�                JA  ARUP                                                                        20200105215340                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200106153400  CV  JULD            G�O�G�O�F�Ǐ                JM  ARCAJMQC2.0                                                                 20200110153949  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200110153949  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200111152718  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081959  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                