CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-06-17T12:40:59Z creation;2021-06-17T12:41:01Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `P   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܬ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߬   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20210617124059  20210617125321  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA                                  2B  A   APEX                            7906                            051216                          846 @�|<��j1   @�|=`�a@3�O�;dZ�d�C��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C�fC�fC  C  C  C  C   C"�C$  C&33C'��C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cq�fCt  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  Dy�D   D �fD!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D[��D\y�D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�3D�@ D�|�Dļ�D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�C3D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A$��AD��Ad��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBP�BY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�k�B���B���B���C O\CO\CO\CO\CO\C
O\CO\CO\Ch�Ch�C5�C5�CO\CO\CO\CO\C O\C"h�C$O\C&��C'��C*O\C,5�C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPh�CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\Cf5�ChO\CjO\ClO\CnO\CpO\Cr5�CtO\CvO\CxO\CzO\C|h�C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�4{C�4{C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�qD�D�=D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D=D��D�D��D�D�qD �D �=D!=D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2qD2��D3�D3��D4�D4�qD5�D5��D6�D6��D7�D7�=D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=qD=��D>�D>�=D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD=DD��DE�DE��DFqDF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMqDM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU=DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�qD\qD\�qD]�D]��D^�D^��D_=D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg�qDh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq=Dq�=Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�	�D�I�D���D���D�	�D�MD���D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD���D���D�D�MD��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�F�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D��D�	�D�I�D���D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�F�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D��D�D�I�DĆ�D�ƹD�	�D�I�Dŉ�D�ƹD�	�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�I�Dˉ�D���D�	�D�I�D̉�D���D�	�D�I�D͉�D���D�	�D�I�DΉ�D���D�	�D�I�Dω�D���D�	�D�I�DЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D���D�	�D�I�DӉ�D���D�	�D�I�Dԉ�D���D�	�D�I�DՉ�D���D�	�D�I�D։�D���D�	�D�I�D׉�D���D�	�D�MD؉�D���D�	�D�I�Dى�D���D�	�D�I�Dډ�D���D�	�D�I�Dۉ�D���D�	�D�I�D܉�D���D�	�D�I�D݉�D���D�	�D�I�Dމ�D���D�	�D�I�D߉�D�ƹD�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D�D���D�	�D�I�D��D���D�	�D�I�D��D��D�	�D�I�D��D���D�	�D�I�D��D��D�	�D�I�D��D���D�	�D�I�D솹D�ƹD�	�D�MD��D���D�	�D�MD��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�$�A��
A֮AփA�t�A�dZA�XA�VA�S�A�M�A�M�A�K�A�I�A�I�A�I�A�G�A�C�A�C�A�C�A�A�A�A�A�?}A�A�A�?}A�=qA�?}A�?}A�A�A�=qA�9XA�7LA�+A��A�1A�~�A��A�p�A��mAӉ7A�
=A�=qAѓuAї�A� �A�+AП�A��HA�1A�t�A�VA�v�AˍPA�z�Aə�A�ȴAƍPA��;A�ƨA�bAƗ�Aŕ�A�(�A��A�p�A��A�|�A��A�S�A��wA��`A�oA���A���A�  A��7A�ffA���A���A��
A��HA�9XA�p�A��A��A�E�A�ffA��
A��PA���A��A�l�A�9XA��FA�%A�Q�A���A���A���A�/A�S�A�33A|ȴAy7LAt{As?}ArZAq�Ap^5Am��Al=qAfȴAa�PA]�AYt�AW�AU��AUS�AT�yAS��AQ��AP��AO`BAM��AJz�AH��AE�^ADZAC�FACt�AB��AA�A?�wA=O�A<1A;t�A:�9A9\)A6��A5\)A4�9A3|�A0n�A.�A-?}A+S�A)�A)�A(bA&r�A%K�A$�HA$jA#ƨA"�\A ��A�^A?}AC�A��AƨAG�AoA�A(�A+A=qAXA1'A�RAhsA��A��AbNA|�A��Av�A��A��A�PA�A��AVAbAXA
E�A��Az�A��A��A �A�A�!A/Ar�AƨA�A~�A�A�mAp�@���@��D@��@��/@��u@�9X@�@�@�l�@�S�@�o@��@�r�@��D@�dZ@�1@���@��@�+@�&�@�9@�@��@�?}@�%@ߥ�@�{@ݙ�@�1@�"�@ڧ�@ڗ�@�E�@�$�@��@ٙ�@�1'@׍P@�C�@�G�@Ӿw@�
=@�~�@�n�@ҧ�@Ӆ@҇+@�O�@��`@�r�@��@�dZ@Η�@�?}@��
@�~�@�=q@��@�&�@�z�@��
@�C�@�$�@ļj@ă@�r�@�|�@§�@�{@���@�7L@��/@���@��u@�Q�@��;@���@�"�@���@�-@�7L@�(�@�t�@���@�V@�E�@��@��@���@�&�@���@�1'@��@��@��P@�o@��^@�/@��/@�j@�(�@���@�K�@���@���@��@��@��@��w@��F@�
=@�n�@�M�@�$�@�{@�{@��@��-@�G�@��`@�1'@���@�l�@�;d@��+@�=q@�-@��@�&�@��`@���@�%@�/@��u@�A�@�b@��@��m@��m@��m@��F@�S�@��y@��H@���@��R@���@��\@�ff@�J@���@���@���@�hs@�G�@�&�@��@�V@��`@��u@��@�Q�@�(�@��F@���@�33@�o@�"�@�"�@��y@���@�J@��@��7@��@��u@�bN@�9X@�A�@�9X@��@��;@��@�t�@�\)@�"�@���@���@�~�@�~�@�ff@�=q@�-@�J@��#@�@���@�hs@��@���@��j@�r�@��m@���@�|�@�l�@�l�@�dZ@�\)@��@�ȴ@�ff@�{@��#@�@���@�`B@�Ĝ@��9@�A�@�bN@�A�@���@��F@�33@�o@�@���@��y@���@���@��R@��!@�~�@�V@�=q@�M�@�^5@�V@��@��h@���@���@��
@�ƨ@�ƨ@��@�"�@�
=@��@��R@�v�@��@�@�p�@�/@���@��@��@��D@�b@��w@�\)@�"�@��@�
=@��H@�v�@�{@���@�hs@�7L@��`@��u@�Z@��@�ƨ@��@���@�
=@��H@���@�-@��@�@���@�x�@�X@�7L@�V@��@��9@�(�@�b@���@��;@���@�\)@�K�@�33@���@��!@�v�@�M�@�@���@���@��7@�hs@�G�@�%@��j@��u@�r�@�bN@��@��m@�ƨ@���@��@�\)@��@��@��+@�ff@�5?@���@��T@���@�hs@�X@�V@���@�z�@�A�@� �@�b@�1@~�@~$�@}��@}p�@}O�@|��@|z�@|(�@{��@{C�@z��@z^5@y�#@yG�@x��@x�@xb@v��@vV@u�T@u�h@u`B@uV@t�@t��@t(�@s�F@s"�@r~�@q�@qX@pĜ@p�@p �@o|�@n�y@nȴ@nv�@m�T@m�@m�@l�/@l��@l9X@k33@j��@j��@i�@i��@ix�@i7L@h�`@h�u@hA�@h  @g�w@gl�@g
=@f��@f��@f5?@e�-@ep�@eV@d��@c�F@cC�@c33@b��@b�@a�@a��@a&�@`��@` �@_�;@_�@_l�@_+@^�@^ȴ@]�@]p�@]O�@]/@]�@]�@\��@\��@\�@[t�@Z�@Z�\@ZM�@Z=q@ZJ@Yhs@X��@XQ�@W�;@W��@WK�@W�@V�y@V��@Vff@VV@VE�@U�@U@U�h@U`B@T��@T(�@S�m@S�F@SS�@R�@R��@R��@Rn�@R-@Q�^@Qx�@Q%@P�`@P�@O��@O|�@OK�@O
=@N�R@NV@N{@M�T@M��@M�@Mp�@M/@L��@L�@L�/@L�j@L�@LI�@K�
@K�F@K��@KdZ@Ko@J�H@J~�@JJ@I�^@I��@I�7@Ihs@I%@H��@H1'@G�@G��@GK�@F�R@F�+@F�+@FV@F{@E��@E�h@E�@D��@D�D@D(�@Cƨ@Ct�@Co@B��@B�@A�#@A�^@Ax�@A&�@A%@@��@@bN@?�w@?l�@?�@>ȴ@>�+@>$�@=�T@=��@=�@=p�@=`B@=O�@<��@<�/@<�@;��@;dZ@;"�@:�H@:-@9�#@9�^@9��@9�7@9x�@9G�@9&�@8��@8�@8  @7l�@7\)@7K�@6��@6�+@6V@6$�@6@5�@5��@5?}@4��@4�j@4�@4��@4z�@4I�@3ƨ@3�F@3t�@333@3o@3@2��@2^5@2�@1�^@1G�@0Ĝ@0r�@0Q�@01'@0  @/�;@/�w@/\)@.ȴ@.��@.E�@.{@-��@-�@-�@,�@,�/@,��@,I�@+ƨ@+��@+�@+t�@+t�@+dZ@+dZ@+dZ@+S�@+33@*��@*�@)�#@)��@)�^@)��@)X@)X@)G�@)G�@)G�@)7L@)%@(�9@( �@'�@'�;@'��@'�P@'\)@'
=@&�y@&�@&v�@&E�@&{@%�-@%�@$��@$z�@$1@#�m@#ƨ@#�@#dZ@#S�@#C�@#"�@#"�@#o@"�@"��@"��@"n�@"J@!��@!7L@ �`@ ��@ A�@  �@ b@ b@�@��@\)@+@�@�@
=@�@��@�+@V@�@�h@O�@?}@?}@�@�j@�@j@1@��@�
@t�@C�@��@~�@M�@-@J@�^@��@hs@G�@7L@&�@&�@&�@%@�`@�`@Ĝ@��@A�@b@�@��@�@\)@+@
=@ȴ@�R@��@�+@v�@E�@$�@$�@��@�@`B@/@V@�@�@j@I�@(�@��@ƨ@��@�@S�@S�@C�@"�@�H@�\@M�@�@�7@&�@%@��@�@r�@bN@1'@ �@ �@b@  @�@��@�w@��@l�@\)@K�@�@��@�y@ȴ@ȴ@��@��@�+@ff@E�@5?@5?@@�T@��@��@��@�h@`B@?}@/@V@�/@��@z�@9X@�@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�$�A��
A֮AփA�t�A�dZA�XA�VA�S�A�M�A�M�A�K�A�I�A�I�A�I�A�G�A�C�A�C�A�C�A�A�A�A�A�?}A�A�A�?}A�=qA�?}A�?}A�A�A�=qA�9XA�7LA�+A��A�1A�~�A��A�p�A��mAӉ7A�
=A�=qAѓuAї�A� �A�+AП�A��HA�1A�t�A�VA�v�AˍPA�z�Aə�A�ȴAƍPA��;A�ƨA�bAƗ�Aŕ�A�(�A��A�p�A��A�|�A��A�S�A��wA��`A�oA���A���A�  A��7A�ffA���A���A��
A��HA�9XA�p�A��A��A�E�A�ffA��
A��PA���A��A�l�A�9XA��FA�%A�Q�A���A���A���A�/A�S�A�33A|ȴAy7LAt{As?}ArZAq�Ap^5Am��Al=qAfȴAa�PA]�AYt�AW�AU��AUS�AT�yAS��AQ��AP��AO`BAM��AJz�AH��AE�^ADZAC�FACt�AB��AA�A?�wA=O�A<1A;t�A:�9A9\)A6��A5\)A4�9A3|�A0n�A.�A-?}A+S�A)�A)�A(bA&r�A%K�A$�HA$jA#ƨA"�\A ��A�^A?}AC�A��AƨAG�AoA�A(�A+A=qAXA1'A�RAhsA��A��AbNA|�A��Av�A��A��A�PA�A��AVAbAXA
E�A��Az�A��A��A �A�A�!A/Ar�AƨA�A~�A�A�mAp�@���@��D@��@��/@��u@�9X@�@�@�l�@�S�@�o@��@�r�@��D@�dZ@�1@���@��@�+@�&�@�9@�@��@�?}@�%@ߥ�@�{@ݙ�@�1@�"�@ڧ�@ڗ�@�E�@�$�@��@ٙ�@�1'@׍P@�C�@�G�@Ӿw@�
=@�~�@�n�@ҧ�@Ӆ@҇+@�O�@��`@�r�@��@�dZ@Η�@�?}@��
@�~�@�=q@��@�&�@�z�@��
@�C�@�$�@ļj@ă@�r�@�|�@§�@�{@���@�7L@��/@���@��u@�Q�@��;@���@�"�@���@�-@�7L@�(�@�t�@���@�V@�E�@��@��@���@�&�@���@�1'@��@��@��P@�o@��^@�/@��/@�j@�(�@���@�K�@���@���@��@��@��@��w@��F@�
=@�n�@�M�@�$�@�{@�{@��@��-@�G�@��`@�1'@���@�l�@�;d@��+@�=q@�-@��@�&�@��`@���@�%@�/@��u@�A�@�b@��@��m@��m@��m@��F@�S�@��y@��H@���@��R@���@��\@�ff@�J@���@���@���@�hs@�G�@�&�@��@�V@��`@��u@��@�Q�@�(�@��F@���@�33@�o@�"�@�"�@��y@���@�J@��@��7@��@��u@�bN@�9X@�A�@�9X@��@��;@��@�t�@�\)@�"�@���@���@�~�@�~�@�ff@�=q@�-@�J@��#@�@���@�hs@��@���@��j@�r�@��m@���@�|�@�l�@�l�@�dZ@�\)@��@�ȴ@�ff@�{@��#@�@���@�`B@�Ĝ@��9@�A�@�bN@�A�@���@��F@�33@�o@�@���@��y@���@���@��R@��!@�~�@�V@�=q@�M�@�^5@�V@��@��h@���@���@��
@�ƨ@�ƨ@��@�"�@�
=@��@��R@�v�@��@�@�p�@�/@���@��@��@��D@�b@��w@�\)@�"�@��@�
=@��H@�v�@�{@���@�hs@�7L@��`@��u@�Z@��@�ƨ@��@���@�
=@��H@���@�-@��@�@���@�x�@�X@�7L@�V@��@��9@�(�@�b@���@��;@���@�\)@�K�@�33@���@��!@�v�@�M�@�@���@���@��7@�hs@�G�@�%@��j@��u@�r�@�bN@��@��m@�ƨ@���@��@�\)@��@��@��+@�ff@�5?@���@��T@���@�hs@�X@�V@���@�z�@�A�@� �@�b@�1@~�@~$�@}��@}p�@}O�@|��@|z�@|(�@{��@{C�@z��@z^5@y�#@yG�@x��@x�@xb@v��@vV@u�T@u�h@u`B@uV@t�@t��@t(�@s�F@s"�@r~�@q�@qX@pĜ@p�@p �@o|�@n�y@nȴ@nv�@m�T@m�@m�@l�/@l��@l9X@k33@j��@j��@i�@i��@ix�@i7L@h�`@h�u@hA�@h  @g�w@gl�@g
=@f��@f��@f5?@e�-@ep�@eV@d��@c�F@cC�@c33@b��@b�@a�@a��@a&�@`��@` �@_�;@_�@_l�@_+@^�@^ȴ@]�@]p�@]O�@]/@]�@]�@\��@\��@\�@[t�@Z�@Z�\@ZM�@Z=q@ZJ@Yhs@X��@XQ�@W�;@W��@WK�@W�@V�y@V��@Vff@VV@VE�@U�@U@U�h@U`B@T��@T(�@S�m@S�F@SS�@R�@R��@R��@Rn�@R-@Q�^@Qx�@Q%@P�`@P�@O��@O|�@OK�@O
=@N�R@NV@N{@M�T@M��@M�@Mp�@M/@L��@L�@L�/@L�j@L�@LI�@K�
@K�F@K��@KdZ@Ko@J�H@J~�@JJ@I�^@I��@I�7@Ihs@I%@H��@H1'@G�@G��@GK�@F�R@F�+@F�+@FV@F{@E��@E�h@E�@D��@D�D@D(�@Cƨ@Ct�@Co@B��@B�@A�#@A�^@Ax�@A&�@A%@@��@@bN@?�w@?l�@?�@>ȴ@>�+@>$�@=�T@=��@=�@=p�@=`B@=O�@<��@<�/@<�@;��@;dZ@;"�@:�H@:-@9�#@9�^@9��@9�7@9x�@9G�@9&�@8��@8�@8  @7l�@7\)@7K�@6��@6�+@6V@6$�@6@5�@5��@5?}@4��@4�j@4�@4��@4z�@4I�@3ƨ@3�F@3t�@333@3o@3@2��@2^5@2�@1�^@1G�@0Ĝ@0r�@0Q�@01'@0  @/�;@/�w@/\)@.ȴ@.��@.E�@.{@-��@-�@-�@,�@,�/@,��@,I�@+ƨ@+��@+�@+t�@+t�@+dZ@+dZ@+dZ@+S�@+33@*��@*�@)�#@)��@)�^@)��@)X@)X@)G�@)G�@)G�@)7L@)%@(�9@( �@'�@'�;@'��@'�P@'\)@'
=@&�y@&�@&v�@&E�@&{@%�-@%�@$��@$z�@$1@#�m@#ƨ@#�@#dZ@#S�@#C�@#"�@#"�@#o@"�@"��@"��@"n�@"J@!��@!7L@ �`@ ��@ A�@  �@ b@ b@�@��@\)@+@�@�@
=@�@��@�+@V@�@�h@O�@?}@?}@�@�j@�@j@1@��@�
@t�@C�@��@~�@M�@-@J@�^@��@hs@G�@7L@&�@&�@&�@%@�`@�`@Ĝ@��@A�@b@�@��@�@\)@+@
=@ȴ@�R@��@�+@v�@E�@$�@$�@��@�@`B@/@V@�@�@j@I�@(�@��@ƨ@��@�@S�@S�@C�@"�@�H@�\@M�@�@�7@&�@%@��@�@r�@bN@1'@ �@ �@b@  @�@��@�w@��@l�@\)@K�@�@��@�y@ȴ@ȴ@��@��@�+@ff@E�@5?@5?@@�T@��@��@��@�h@`B@?}@/@V@�/@��@z�@9X@�@�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
	7B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	�sB	�)B	��B	ƨB	�RB	��B	�bB	��B	�\B	��B	�`B	�B	�B
�B
-B
,B
�B
�B
6FB
W
B
G�B
�DB
��BBDB�B[#B�1B�PB��B�}B��B�sB�yB��B�FB�-B�B�B�B�hB�oB��B�\B�Bu�B
��B	�B	�sB	�`B	�B	�TB	�fB	�B
B
PB
�B
!�B
!�B
P�B
ZB
J�B
!�B
+B
1'B
�B	�B	�B	ÖB	ÖB	�}B	�jB	�9B	��B	��B	�B	iyB	aHB	YB	S�B	P�B	N�B	M�B	L�B	K�B	G�B	C�B	=qB	1'B	'�B	�B	\B	DB		7B	+B	B��B��B�B�B�B�sB�NB�)B�B�B��B�qB�XB�-B�B�B�-B�B��B��B��B��B��B�oB�\B��B��B��B�B�9B�dB�FB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�B�B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B�B�3B�-B�9B�^B�qB��BBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B�NB�fB�B�B��B��B��B��B��B��B	  B	  B	  B	B	+B	1B	1B	
=B	
=B	1B	1B		7B	
=B	JB	VB	\B	bB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	%�B	)�B	/B	/B	1'B	49B	5?B	5?B	49B	6FB	8RB	9XB	:^B	;dB	=qB	A�B	B�B	C�B	F�B	G�B	M�B	M�B	O�B	S�B	YB	\)B	^5B	aHB	cTB	dZB	gmB	jB	o�B	s�B	w�B	{�B	}�B	� B	�B	�B	�B	�%B	�7B	�7B	�DB	�PB	�hB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�FB	�^B	�XB	�^B	�jB	�qB	�wB	�wB	�wB	�}B	��B	ÖB	ĜB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
DB
JB
DB
DB
JB
PB
VB
\B
\B
\B
bB
hB
hB
hB
oB
oB
oB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
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
.B
/B
0!B
1'B
1'B
2-B
2-B
2-B
33B
2-B
33B
33B
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
@�B
@�B
@�B
@�B
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
B�B
B�B
C�B
C�B
D�B
D�B
C�B
C�B
D�B
C�B
D�B
E�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
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
N�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
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
R�B
R�B
R�B
Q�B
<jB
7LB
S�B
S�B
S�B
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
\)B
\)B
\)B
\)B
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
`BB
`BB
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
bNB
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
gmB
hsB
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
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
v�B
v�B
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
y�B
y�B
y�B
y�B
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
{�B
|�B
{�B
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
~�B
~�B
~�B
~�B
� B
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
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
	7B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	�sB	�)B	��B	ƨB	�RB	��B	�bB	��B	�\B	��B	�`B	�B	�B
�B
-B
,B
�B
�B
6FB
W
B
G�B
�DB
��BBDB�B[#B�1B�PB��B�}B��B�sB�yB��B�FB�-B�B�B�B�hB�oB��B�\B�Bu�B
��B	�B	�sB	�`B	�B	�TB	�fB	�B
B
PB
�B
!�B
!�B
P�B
ZB
J�B
!�B
+B
1'B
�B	�B	�B	ÖB	ÖB	�}B	�jB	�9B	��B	��B	�B	iyB	aHB	YB	S�B	P�B	N�B	M�B	L�B	K�B	G�B	C�B	=qB	1'B	'�B	�B	\B	DB		7B	+B	B��B��B�B�B�B�sB�NB�)B�B�B��B�qB�XB�-B�B�B�-B�B��B��B��B��B��B�oB�\B��B��B��B�B�9B�dB�FB�9B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�B�B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B�B�3B�-B�9B�^B�qB��BBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B�NB�fB�B�B��B��B��B��B��B��B	  B	  B	  B	B	+B	1B	1B	
=B	
=B	1B	1B		7B	
=B	JB	VB	\B	bB	oB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	$�B	%�B	)�B	/B	/B	1'B	49B	5?B	5?B	49B	6FB	8RB	9XB	:^B	;dB	=qB	A�B	B�B	C�B	F�B	G�B	M�B	M�B	O�B	S�B	YB	\)B	^5B	aHB	cTB	dZB	gmB	jB	o�B	s�B	w�B	{�B	}�B	� B	�B	�B	�B	�%B	�7B	�7B	�DB	�PB	�hB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�FB	�^B	�XB	�^B	�jB	�qB	�wB	�wB	�wB	�}B	��B	ÖB	ĜB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B

=B
DB
DB
JB
DB
DB
JB
PB
VB
\B
\B
\B
bB
hB
hB
hB
oB
oB
oB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
)�B
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
.B
/B
0!B
1'B
1'B
2-B
2-B
2-B
33B
2-B
33B
33B
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
@�B
@�B
@�B
@�B
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
B�B
B�B
C�B
C�B
D�B
D�B
C�B
C�B
D�B
C�B
D�B
E�B
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
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
J�B
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
N�B
N�B
M�B
M�B
N�B
N�B
N�B
N�B
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
R�B
R�B
R�B
Q�B
<jB
7LB
S�B
S�B
S�B
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
\)B
\)B
\)B
\)B
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
`BB
`BB
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
bNB
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
gmB
hsB
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
k�B
k�B
k�B
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
m�B
m�B
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
v�B
v�B
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
y�B
y�B
y�B
y�B
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
{�B
|�B
{�B
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
~�B
~�B
~�B
~�B
� B
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
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210617213951  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210617124059  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210617124100  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210617124100  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210617124100  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210617124100  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210617124100  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210617124100  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210617124101  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210617124101                      G�O�G�O�G�O�                JA  ARUP                                                                        20210617125321                      G�O�G�O�G�O�                