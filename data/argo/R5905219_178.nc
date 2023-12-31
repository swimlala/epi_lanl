CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-02-12T21:43:02Z creation;2023-02-12T21:43:04Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230212214302  20230212215821  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�lj �.1   @�n5��@5C��$��c��t�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A^ffA�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>�C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Cp  Cq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fDfD� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D>fD>�fD?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZy�D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D��3D�  D�@ Dʃ3D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�\)A�A'�AG�AfzA��
A���A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B�B���B���B���B�B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B�(�B���B���B���B�C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C�{C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6aGC8z�C:z�C<z�C>�{C@�{CB�{CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�ClaGCnz�Cpz�CraGCtz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�0�C�=qC�=qC�=qC�=qC�J>C�=qC�=qC�J>C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�0�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�D%D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D�D%D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'%D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-�RD.�D.��D/�D/��D0�D0��D1RD1��D2�D2��D3�D3��D4�D4��D5RD5��D6�D6��D7�D7��D8�D8��D9�D9��D:%D:��D;�D;��D<�D<��D=�D=��D>%D>�D?�D?��D@�D@�RDA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DFRDF��DG�DG��DH�DH��DI%DI�DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY�DZ�DZ�RD[�D[�RD\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn�Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D���D��\D�\D�O\D��)D��\D�\D�O\D��\D��\D�\D�O\D��\D��)D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D���D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�)D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D���D��\D�\D�O\D��\D��\D�\D�O\D��\D�ҏD�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�L)D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�)D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�R�D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��)D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D\D��\D�\D�O\DÏ\D��\D�\D�O\Dď\D��\D�\D�O\Dŏ\D��\D�\D�O\DƏ\D��\D�\D�O\DǏ\D��\D�\D�O\Dȏ\D��\D�\D�O\Dɏ\D�ҏD�\D�O\Dʒ�D��\D�\D�O\Dˏ\D��\D�\D�O\D̏\D��\D�\D�O\D͏\D��\D�\D�O\DΏ\D��\D�\D�O\DϏ\D��\D�\D�O\DЏ\D��\D�\D�O\Dя\D��\D�\D�O\Dҏ\D��\D�\D�O\Dӏ\D��\D�\D�O\Dԏ\D��\D�\D�O\DՏ\D��\D�\D�O\D֏\D��\D�\D�O\D׏\D��\D�\D�O\D؏\D��\D�\D�O\Dُ\D��\D�\D�O\Dڏ\D��\D�\D�O\Dۏ\D��\D�\D�O\D܏\D��\D�\D�O\Dݏ\D��\D�\D�O\Dޏ\D��\D�\D�O\Dߏ\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��)D�)D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D�\D�ҏD��D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�;dA�=qA�;dA�;dA�;dA�9XA�9XA�1'A��AľwAĥ�Aģ�Aġ�Aģ�Aĥ�Aĥ�Aħ�Aħ�Aĩ�AĬAġ�A�~�A�`BA�&�A�"�A��A��A��Aò-A��A�r�A��A��+A�S�A�|�A�bA�O�A��A���A�7LA�/A��!A�;dA��hA�VA��A�"�A��`A��jA�bNA��!A��7A���A��PA�bA��A���A��
A��A�t�A�9XA��jA���A���A�33A�bA�?}A��HA�\)A�VA���A�K�A��A��yA��#A��7A���A�x�A��;A���A��FA�{A��\A�?}A�l�A�/A��
A�oA��uA�(�A���A�O�A���A��RA�+A��A�ffA�/A���A�+AxE�Au�-AtjAr��AoAk�FAg�hAe�A`�!A\ȴA\�9A\��A\�DA[;dAX�yAW�^AVZAT�!AS��AR�9AP�/AOAM�AM�;AMdZAL�ALz�AIoAGG�AEAB�DAA�FA@�9A>��A=��A<JA7�A5�A4�/A3�FA2�9A2{A1&�A0 �A/"�A-�-A,��A*��A)p�A(n�A'�A&bNA&-A%33A#��A!hsA �`AƨA��A|�Az�AQ�A�A�A�#A
=A��A��A��A$�AO�A
=A�;AQ�A��Ax�A
�A�7A33A��A�AjA�-AXA ��A n�@���@�@�Ĝ@�+@��@�1'@�33@�G�@�ȴ@��@��@�+@��@�A�@�hs@�;d@���@�j@�@�C�@���@ᙚ@��D@��@߅@���@�^5@�-@݁@��`@��;@�E�@�X@��`@��
@�-@ղ-@�hs@��@�j@�b@�33@�ff@�hs@���@Ѓ@�Q�@ϝ�@�K�@��@�J@�G�@��@��/@̣�@�bN@˝�@�@�Ĝ@�(�@�t�@�;d@��y@�ff@ź^@ũ�@Ł@�G�@��@�Z@��m@þw@�"�@�M�@��#@�/@� �@�C�@�
=@�@��#@��^@���@�x�@�O�@�%@�j@�Z@�(�@���@���@�=q@���@�x�@�X@�%@�1'@��m@�ƨ@��P@�C�@��@�5?@��@���@�O�@��@�Q�@� �@��m@��@�dZ@�+@��!@�E�@��@���@�?}@���@��/@���@�A�@��P@�33@��@���@���@�~�@�M�@�-@�@��@��@���@���@��@�/@���@�Ĝ@��u@�Z@�Q�@� �@�ƨ@�K�@�o@��@��@�v�@��@��T@�&�@��@�Ĝ@���@��@�r�@�bN@�(�@���@��F@��P@�dZ@�@���@�n�@�V@�J@��h@�`B@�&�@��/@��j@���@�j@�I�@�9X@��@���@���@�33@�ȴ@��@���@��@��/@��
@��F@���@��P@���@��\@�=q@���@�@��7@�`B@�?}@���@��@�r�@�Z@�Z@�A�@�1'@�(�@�(�@��@���@�K�@�\)@��@��y@���@��!@��\@�$�@�{@��T@�p�@�&�@��@��@��j@��D@�r�@�j@��
@�S�@���@���@�E�@��T@���@�hs@�O�@�7L@��@��@�Ĝ@���@��@�j@�Z@�I�@���@��@�|�@�33@�o@���@�^5@�-@�@�G�@���@�j@�(�@��@��w@���@��P@�\)@�+@���@��H@���@�~�@���@��7@��@�hs@�`B@�/@��`@��9@��u@�r�@� �@���@�K�@��H@��\@�V@�V@��@��@��`@��/@��u@�A�@� �@���@��F@�\)@��\@�{@���@��@���@�@���@���@���@��h@��@�`B@�?}@��@���@���@���@�Z@� �@��@�1@���@�o@��R@��+@�ff@��@�@�?}@��/@��D@�r�@�bN@�I�@�  @��@�P@�@~�@~��@~$�@}�h@}O�@}V@|�@|j@|9X@{��@{t�@z�@zn�@y��@yx�@y%@x��@xĜ@xr�@xA�@x �@xb@w�w@wK�@w
=@v�@v��@vV@v@up�@uV@t�D@s�m@st�@sC�@r�H@r�\@r-@qx�@p��@p�9@pr�@pbN@p  @ol�@o�@nȴ@nff@m�T@mp�@m/@l�/@l�@lZ@l9X@k�F@kt�@k33@j�\@i�#@i7L@h��@h�u@hr�@hr�@hbN@hb@g��@g|�@g+@fv�@e�T@e�@e/@d�@d��@d�j@d�j@d�@dZ@d�@b�@b~�@b-@a��@ahs@a&�@`��@`bN@_|�@_
=@^�R@^{@]�-@]p�@\��@\��@\��@\�@\z�@[ƨ@[�F@[�F@[�F@[��@[�@[C�@[@Z�\@ZM�@Y�#@Y��@YX@YX@Y7L@Y%@X�`@X��@X�u@X �@W|�@W
=@Vȴ@V��@V5?@U��@U�@U?}@U/@U�@T�@TI�@Sƨ@S�@SC�@R��@Q��@Q�7@QX@Q�@Pr�@PbN@PQ�@PA�@P1'@P1'@O�@N��@N�+@NE�@M?}@L�@LZ@K��@K�F@KdZ@K@J�H@J�@J~�@I�@I�^@Ix�@I&�@H��@H�9@HA�@H  @G;d@G
=@F�@FV@FE�@F5?@F{@E��@E�-@E�@D�D@DI�@CdZ@B��@B�\@B-@BJ@A��@Ahs@A%@@��@?�w@?��@?l�@?\)@?K�@?;d@?
=@>��@>@<�@<z�@<9X@<1@;ƨ@;dZ@;33@;"�@:��@:J@9hs@9�@8��@8��@8�9@8r�@8bN@8Q�@8A�@8 �@7�@7�;@7��@7�@7\)@7;d@6��@6��@6v�@6E�@5��@5�-@5�h@5�@5?}@5/@5�@4��@4��@4Z@49X@49X@41@3�m@3�m@3ƨ@3S�@3o@2�H@2��@2��@2~�@2^5@2^5@2-@1�#@1��@1&�@0Ĝ@0r�@0Q�@0A�@01'@/�@/�w@/�P@/;d@.��@.�@.��@.E�@.5?@.$�@.{@-�@-�-@-�h@-p�@-O�@-�@,�@,��@,�D@,9X@+�m@+ƨ@+�F@+��@+��@+�F@+�F@+��@+t�@+33@*��@*n�@*�@)��@)�^@)�7@)x�@)G�@)&�@(��@(�@(bN@(1'@'��@'�P@'\)@&��@&�@&��@&E�@%�T@%�-@%�h@%`B@%�@$�/@$��@$Z@$(�@$1@#��@#ƨ@#��@#dZ@#o@"�H@"�!@"M�@"J@!��@!��@!��@!hs@!&�@!�@!%@ ��@ �@ r�@ 1'@�@K�@�@�+@V@E�@{@@�h@`B@/@�/@z�@I�@9X@�@1@�m@ƨ@��@t�@33@�H@��@��@M�@�@�#@��@��@hs@��@�u@bN@Q�@1'@��@��@K�@;d@;d@+@�@��@V@$�@{@@�T@@@��@�@`B@�@V@V@��@�@��@z�@�@�m@�
@ƨ@"�@n�@�@�#@�^@�7@hs@7L@�@��@��@�@�@r�@Q�@Q�@A�@A�@1'@b@�@|�@l�@\)@\)@;d@�@ff@$�@@�T@��@@�-@��@p�@/@�@��@��@�j@�@��@z�@I�@I�@9X@(�@�@1@�m@��@��@��@��@t�@dZ@C�@@
��@
�!@
��@
��@
��@
��@
�!@
~�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�;dA�=qA�;dA�;dA�;dA�9XA�9XA�1'A��AľwAĥ�Aģ�Aġ�Aģ�Aĥ�Aĥ�Aħ�Aħ�Aĩ�AĬAġ�A�~�A�`BA�&�A�"�A��A��A��Aò-A��A�r�A��A��+A�S�A�|�A�bA�O�A��A���A�7LA�/A��!A�;dA��hA�VA��A�"�A��`A��jA�bNA��!A��7A���A��PA�bA��A���A��
A��A�t�A�9XA��jA���A���A�33A�bA�?}A��HA�\)A�VA���A�K�A��A��yA��#A��7A���A�x�A��;A���A��FA�{A��\A�?}A�l�A�/A��
A�oA��uA�(�A���A�O�A���A��RA�+A��A�ffA�/A���A�+AxE�Au�-AtjAr��AoAk�FAg�hAe�A`�!A\ȴA\�9A\��A\�DA[;dAX�yAW�^AVZAT�!AS��AR�9AP�/AOAM�AM�;AMdZAL�ALz�AIoAGG�AEAB�DAA�FA@�9A>��A=��A<JA7�A5�A4�/A3�FA2�9A2{A1&�A0 �A/"�A-�-A,��A*��A)p�A(n�A'�A&bNA&-A%33A#��A!hsA �`AƨA��A|�Az�AQ�A�A�A�#A
=A��A��A��A$�AO�A
=A�;AQ�A��Ax�A
�A�7A33A��A�AjA�-AXA ��A n�@���@�@�Ĝ@�+@��@�1'@�33@�G�@�ȴ@��@��@�+@��@�A�@�hs@�;d@���@�j@�@�C�@���@ᙚ@��D@��@߅@���@�^5@�-@݁@��`@��;@�E�@�X@��`@��
@�-@ղ-@�hs@��@�j@�b@�33@�ff@�hs@���@Ѓ@�Q�@ϝ�@�K�@��@�J@�G�@��@��/@̣�@�bN@˝�@�@�Ĝ@�(�@�t�@�;d@��y@�ff@ź^@ũ�@Ł@�G�@��@�Z@��m@þw@�"�@�M�@��#@�/@� �@�C�@�
=@�@��#@��^@���@�x�@�O�@�%@�j@�Z@�(�@���@���@�=q@���@�x�@�X@�%@�1'@��m@�ƨ@��P@�C�@��@�5?@��@���@�O�@��@�Q�@� �@��m@��@�dZ@�+@��!@�E�@��@���@�?}@���@��/@���@�A�@��P@�33@��@���@���@�~�@�M�@�-@�@��@��@���@���@��@�/@���@�Ĝ@��u@�Z@�Q�@� �@�ƨ@�K�@�o@��@��@�v�@��@��T@�&�@��@�Ĝ@���@��@�r�@�bN@�(�@���@��F@��P@�dZ@�@���@�n�@�V@�J@��h@�`B@�&�@��/@��j@���@�j@�I�@�9X@��@���@���@�33@�ȴ@��@���@��@��/@��
@��F@���@��P@���@��\@�=q@���@�@��7@�`B@�?}@���@��@�r�@�Z@�Z@�A�@�1'@�(�@�(�@��@���@�K�@�\)@��@��y@���@��!@��\@�$�@�{@��T@�p�@�&�@��@��@��j@��D@�r�@�j@��
@�S�@���@���@�E�@��T@���@�hs@�O�@�7L@��@��@�Ĝ@���@��@�j@�Z@�I�@���@��@�|�@�33@�o@���@�^5@�-@�@�G�@���@�j@�(�@��@��w@���@��P@�\)@�+@���@��H@���@�~�@���@��7@��@�hs@�`B@�/@��`@��9@��u@�r�@� �@���@�K�@��H@��\@�V@�V@��@��@��`@��/@��u@�A�@� �@���@��F@�\)@��\@�{@���@��@���@�@���@���@���@��h@��@�`B@�?}@��@���@���@���@�Z@� �@��@�1@���@�o@��R@��+@�ff@��@�@�?}@��/@��D@�r�@�bN@�I�@�  @��@�P@�@~�@~��@~$�@}�h@}O�@}V@|�@|j@|9X@{��@{t�@z�@zn�@y��@yx�@y%@x��@xĜ@xr�@xA�@x �@xb@w�w@wK�@w
=@v�@v��@vV@v@up�@uV@t�D@s�m@st�@sC�@r�H@r�\@r-@qx�@p��@p�9@pr�@pbN@p  @ol�@o�@nȴ@nff@m�T@mp�@m/@l�/@l�@lZ@l9X@k�F@kt�@k33@j�\@i�#@i7L@h��@h�u@hr�@hr�@hbN@hb@g��@g|�@g+@fv�@e�T@e�@e/@d�@d��@d�j@d�j@d�@dZ@d�@b�@b~�@b-@a��@ahs@a&�@`��@`bN@_|�@_
=@^�R@^{@]�-@]p�@\��@\��@\��@\�@\z�@[ƨ@[�F@[�F@[�F@[��@[�@[C�@[@Z�\@ZM�@Y�#@Y��@YX@YX@Y7L@Y%@X�`@X��@X�u@X �@W|�@W
=@Vȴ@V��@V5?@U��@U�@U?}@U/@U�@T�@TI�@Sƨ@S�@SC�@R��@Q��@Q�7@QX@Q�@Pr�@PbN@PQ�@PA�@P1'@P1'@O�@N��@N�+@NE�@M?}@L�@LZ@K��@K�F@KdZ@K@J�H@J�@J~�@I�@I�^@Ix�@I&�@H��@H�9@HA�@H  @G;d@G
=@F�@FV@FE�@F5?@F{@E��@E�-@E�@D�D@DI�@CdZ@B��@B�\@B-@BJ@A��@Ahs@A%@@��@?�w@?��@?l�@?\)@?K�@?;d@?
=@>��@>@<�@<z�@<9X@<1@;ƨ@;dZ@;33@;"�@:��@:J@9hs@9�@8��@8��@8�9@8r�@8bN@8Q�@8A�@8 �@7�@7�;@7��@7�@7\)@7;d@6��@6��@6v�@6E�@5��@5�-@5�h@5�@5?}@5/@5�@4��@4��@4Z@49X@49X@41@3�m@3�m@3ƨ@3S�@3o@2�H@2��@2��@2~�@2^5@2^5@2-@1�#@1��@1&�@0Ĝ@0r�@0Q�@0A�@01'@/�@/�w@/�P@/;d@.��@.�@.��@.E�@.5?@.$�@.{@-�@-�-@-�h@-p�@-O�@-�@,�@,��@,�D@,9X@+�m@+ƨ@+�F@+��@+��@+�F@+�F@+��@+t�@+33@*��@*n�@*�@)��@)�^@)�7@)x�@)G�@)&�@(��@(�@(bN@(1'@'��@'�P@'\)@&��@&�@&��@&E�@%�T@%�-@%�h@%`B@%�@$�/@$��@$Z@$(�@$1@#��@#ƨ@#��@#dZ@#o@"�H@"�!@"M�@"J@!��@!��@!��@!hs@!&�@!�@!%@ ��@ �@ r�@ 1'@�@K�@�@�+@V@E�@{@@�h@`B@/@�/@z�@I�@9X@�@1@�m@ƨ@��@t�@33@�H@��@��@M�@�@�#@��@��@hs@��@�u@bN@Q�@1'@��@��@K�@;d@;d@+@�@��@V@$�@{@@�T@@@��@�@`B@�@V@V@��@�@��@z�@�@�m@�
@ƨ@"�@n�@�@�#@�^@�7@hs@7L@�@��@��@�@�@r�@Q�@Q�@A�@A�@1'@b@�@|�@l�@\)@\)@;d@�@ff@$�@@�T@��@@�-@��@p�@/@�@��@��@�j@�@��@z�@I�@I�@9X@(�@�@1@�m@��@��@��@��@t�@dZ@C�@@
��@
�!@
��@
��@
��@
��@
�!@
~�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B��BB$�B,B-B.B.B.B.B.B/B0!B1'B;dBC�BM�BaHBcTBffBm�B�B�uB�B�#B�mB�yB�B��B�B��B��B��B��B�TB�#B�B��B��B�
B��B��B��BƨBŢB�XB�B��B��B��B��B��B�\B�Bp�BVBI�B9XB%�B�BDBB��B��B�B�BB�B�
B��B��BƨB�qB�9B��B�hB�+B}�BhsBYBE�B�B
��B
�B
�yB
�TB
�#B
��B
�qB
�!B
��B
�%B
aHB
T�B
>wB
bB
  B	��B	�B	�B	��B	�B	��B	�+B	iyB	hsB	gmB	e`B	_;B	T�B	J�B	E�B	=qB	5?B	0!B	(�B	 �B	�B	�B	�B	oB	\B	B��B�B�`B�HB�/B�B��B��B�jB�FB�!B�B��B��B��B��B��B��B��B�{B�VB�=B�DB�7B�=B�=B�=B�B�B�B�B�B|�B|�B|�By�Bz�B{�Bw�Bw�Bx�Bw�Bu�Bt�Bu�Bs�Bs�Br�Bq�Br�Bs�Br�Br�Bv�Bx�By�B|�B{�B}�B�B�B�B�+B�7B�7B�PB�VB�uB�uB�uB��B��B��B�B�!B�'B�'B�?B�XB�dB�wB�}B��BBĜBŢBɺB��B��B�B�B�B�/B�HB�HB�NB�TB�mB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	%B	+B	PB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	,B	-B	1'B	6FB	9XB	:^B	@�B	A�B	A�B	B�B	C�B	C�B	E�B	I�B	I�B	J�B	M�B	S�B	VB	XB	ZB	ZB	\)B	aHB	bNB	cTB	dZB	e`B	gmB	iyB	jB	l�B	n�B	p�B	r�B	s�B	t�B	u�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�B	�B	�+B	�7B	�PB	�VB	�bB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�/B	�5B	�/B	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
\B
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
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
,B
,B
,B
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
.B
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
5?B
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
=qB
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
@�B
@�B
@�B
@�B
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
C�B
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
F�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
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
L�B
M�B
M�B
M�B
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
Q�B
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
T�B
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
XB
XB
XB
XB
YB
YB
XB
YB
ZB
ZB
ZB
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
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
bNB
cTB
dZB
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
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
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
m�B
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
x�B
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
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
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
�\B
�\B
�\B
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
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
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
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 B�B�B�B�B�B�B�B�B��BB$�B,B-B.B.B.B.B.B/B0!B1'B;dBC�BM�BaHBcTBffBm�B�B�uB�B�#B�mB�yB�B��B�B��B��B��B��B�TB�#B�B��B��B�
B��B��B��BƨBŢB�XB�B��B��B��B��B��B�\B�Bp�BVBI�B9XB%�B�BDBB��B��B�B�BB�B�
B��B��BƨB�qB�9B��B�hB�+B}�BhsBYBE�B�B
��B
�B
�yB
�TB
�#B
��B
�qB
�!B
��B
�%B
aHB
T�B
>wB
bB
  B	��B	�B	�B	��B	�B	��B	�+B	iyB	hsB	gmB	e`B	_;B	T�B	J�B	E�B	=qB	5?B	0!B	(�B	 �B	�B	�B	�B	oB	\B	B��B�B�`B�HB�/B�B��B��B�jB�FB�!B�B��B��B��B��B��B��B��B�{B�VB�=B�DB�7B�=B�=B�=B�B�B�B�B�B|�B|�B|�By�Bz�B{�Bw�Bw�Bx�Bw�Bu�Bt�Bu�Bs�Bs�Br�Bq�Br�Bs�Br�Br�Bv�Bx�By�B|�B{�B}�B�B�B�B�+B�7B�7B�PB�VB�uB�uB�uB��B��B��B�B�!B�'B�'B�?B�XB�dB�wB�}B��BBĜBŢBɺB��B��B�B�B�B�/B�HB�HB�NB�TB�mB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	%B	+B	PB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	,B	-B	1'B	6FB	9XB	:^B	@�B	A�B	A�B	B�B	C�B	C�B	E�B	I�B	I�B	J�B	M�B	S�B	VB	XB	ZB	ZB	\)B	aHB	bNB	cTB	dZB	e`B	gmB	iyB	jB	l�B	n�B	p�B	r�B	s�B	t�B	u�B	v�B	x�B	{�B	}�B	~�B	� B	�B	�B	�B	�+B	�7B	�PB	�VB	�bB	�hB	�oB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	B	ÖB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�/B	�/B	�5B	�/B	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
\B
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
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
,B
,B
,B
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
.B
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
5?B
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
=qB
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
@�B
@�B
@�B
@�B
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
C�B
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
F�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
J�B
J�B
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
L�B
M�B
M�B
M�B
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
Q�B
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
T�B
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
XB
XB
XB
XB
YB
YB
XB
YB
ZB
ZB
ZB
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
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
bNB
cTB
dZB
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
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
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
m�B
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
x�B
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
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
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
�\B
�\B
�\B
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
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
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
��33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20230213064247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230212214302  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230212214303  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230212214304                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230212214305  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230212214305  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230212215821                      G�O�G�O�G�O�                