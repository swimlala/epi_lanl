CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-11-20T03:40:03Z creation;2020-11-20T03:40:04Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20201120034003  20201120035226  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               _A   JA                                  2B  A   APEX                            7906                            051216                          846 @�H�W��1   @�H��q�@3���n��da�����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  @���A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb�Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� DfD�fDfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~y�D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�3D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D���D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�� D�� D���D�@ D��3D��3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��A\)A$��AD��Ad��A�z�A�G�A�z�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�BȞ�B̞�BО�BԞ�B؞�Bܞ�B���B䞸B螸B잸B�k�B���B���B���C O\CO\CO\CO\Ch�C
O\CO\CO\CO\CO\CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<h�C>O\C@O\CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZ5�C\O\C^O\C`O\Cbh�CdO\Cfh�Chh�CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D=D�=D�D��D=D�=D=D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�qD&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQqDQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~�qD�D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D��D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�F�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD��D���D�	�D�MD���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�MD��D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D���D�	�D�I�Dĉ�D���D�	�D�F�Dŉ�D���D�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�I�Dˉ�D���D�	�D�I�D̉�D���D�	�D�I�D͍D���D�	�D�I�DΉ�D���D�	�D�I�Dω�D���D�	�D�MDЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D�ƹD�	�D�I�DӉ�D���D�	�D�I�Dԉ�D���D�	�D�I�DՉ�D���D�	�D�I�D։�D���D�	�D�I�D׉�D���D�	�D�I�D؉�D���D�	�D�I�Dى�D���D�	�D�I�Dډ�D���D�	�D�I�Dۉ�D���D�	�D�I�D܉�D�ƹD��D�I�D݉�D���D�	�D�I�Dމ�D���D�	�D�I�D߉�D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D�D��D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�F�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�MD��D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D���D���D��D�I�D���D�ƹD�	�D�I�D���D���D��D�I�D��D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��
A���A���A�ĜA�ȴA���A�A�ƨA�ƨA�A�Aݰ!AݮAݡ�Aݟ�Aݝ�Aݟ�Aݟ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݛ�Aݛ�Aݛ�Aݙ�Aݙ�Aݛ�Aݛ�Aݛ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݟ�Aݛ�Aݙ�Aݗ�Aݗ�Aݕ�A݁A�S�AܓuA�;dA�"�AԋDA�A�1A�ȴA��TA΃A���A͙�ÁA��A���A���A�33A��A���A�v�A�?}A�33AÑhA�K�A�ƨA�;dA���A�O�A��^A�JA��^A�9XA���A��PA�(�A�K�A�=qA��A�^5A��
A�n�A��A�jA��A��PA�S�A�|�A�ƨA�n�A�bNA��A��A�hsA���A��A�\)A�A���A���A�
=A��A��;A��A��hA���A��`A� �A��A�^5A��-A�K�A��PA�VA�$�A���A�{A�`BA��+A�1A�C�A���A�jA��^A��A��A��A�7LA~��A{�TAx�DAv�!As+ApĜAn�yAkS�Ag��Ad�`Ac��Aa�A^�yA\JAZ9XAY/AXz�AWt�AU�AT�+AS
=APbNAN�AM�;AJ��AGt�AFI�AD�!ACXAA�^AA7LA@ �A=�hA=VA<��A<bA;|�A:ZA8ĜA6�jA5�7A4��A4v�A3/A1��A.�A,��A*v�A)?}A(�RA'�mA'
=A&�A$5?A#?}A"�DAG�AƨA�9AA�A�AjA��AdZA33A�Az�A��A�A��A  AA;dAA9XA�A5?Ax�A
��A
5?A	p�Av�A��A�!A�FAVA��Av�AI�A�
A/AVA�m@��m@�X@��!@���@��D@��w@���@��P@�l�@�ȴ@���@��`@��;@�v�@��@�bN@��
@�;d@��@�bN@��;@�J@��/@��@ߝ�@�v�@�5?@�=q@�E�@�%@ە�@�n�@��@�Q�@��@��@���@�`B@��@�r�@�  @ҸR@���@�hs@У�@��;@�K�@ΰ!@�V@�Z@��`@��@�Q�@�ƨ@��@�-@���@�@ɺ^@ə�@�Z@�\)@�(�@ȃ@Ǯ@�~�@Ə\@őh@ě�@�Z@�|�@�dZ@�t�@�I�@�A�@Õ�@�
=@��H@°!@�n�@�-@��-@�G�@��@�j@�b@���@�@���@��#@��/@��9@��@�bN@�1@�dZ@�"�@�
=@�|�@�t�@��y@�@��9@�j@�A�@� �@��
@�;d@��R@��R@�ff@�V@��T@�x�@���@�1@��m@�  @��;@�33@���@�v�@�E�@��R@��@���@�dZ@���@�hs@���@��9@�z�@���@�V@�Ĝ@��u@�b@�ƨ@�|�@�;d@��H@��+@��@��@���@��h@�hs@�X@���@�Ĝ@�z�@�(�@�1@��
@���@�\)@�+@��y@��!@��\@�-@�@��@���@���@���@�r�@�b@���@�t�@�C�@��@��R@�~�@�$�@���@�p�@��@���@���@�Z@��m@��;@��
@�ƨ@��F@��F@���@��P@�|�@�dZ@�K�@�"�@�
=@���@��R@�=q@�O�@��9@�Z@�(�@�ƨ@�t�@�o@��@���@���@�n�@��@�@��^@�G�@���@��`@���@��u@�1@���@�l�@�"�@��@�@��R@�E�@�-@�@���@�@���@��9@���@��u@��m@��
@��F@��P@��P@�l�@�K�@�;d@�33@��H@�V@�$�@��T@�7L@��@���@��9@�z�@�Q�@�(�@�1@��@�S�@�C�@���@���@�M�@��@���@��T@��@�G�@���@��/@��u@�Z@�1'@��@��@��@��F@���@�dZ@�;d@�"�@��!@�-@��^@�hs@�7L@���@��j@���@��D@�Q�@��@���@��F@�|�@�dZ@��@��H@���@��+@�^5@�5?@�$�@���@�Ĝ@��j@���@�I�@���@��y@���@�X@�G�@�G�@�V@��u@��@�A�@��@��
@��F@�+@��R@�~�@�$�@��T@��h@��7@�`B@�?}@���@�I�@�;@�@�@�P@~�@~{@}p�@|��@{�
@{��@{o@z�@z��@zn�@y�#@y&�@xbN@x1'@w��@v��@vE�@vE�@u@u/@t�j@t�@t��@t�D@s�m@s�F@s��@sS�@r��@r�!@q��@q�#@q��@p�`@p��@pA�@o�@o�P@o;d@o
=@n��@m�@mp�@m/@l�@l�@lI�@k��@kS�@j�!@jM�@j�@i��@i��@i��@i�7@i�@hĜ@g�@f�y@fȴ@f��@fff@f@e?}@dz�@d9X@c��@cS�@c"�@b��@b�\@a�@a��@ahs@`�`@`1'@_�@_�@_l�@^ȴ@^E�@]�T@]�-@]�h@]p�@]`B@]`B@]�@\�@\�@\j@[��@[dZ@["�@Z�H@Z�!@Z~�@Z=q@ZJ@Y��@Y��@Y��@YG�@Y&�@Y%@XĜ@X��@XA�@X  @W�;@W�w@W��@WK�@Vȴ@V��@U�@U`B@U�@T��@T�@TI�@S�@So@R��@R��@R�!@R~�@R-@Qx�@P��@P��@P�@PbN@O��@O��@O�@NV@N$�@N@M�T@M�-@M�@M/@L��@Lj@K��@Kƨ@KdZ@Ko@J�@J�!@JM�@JJ@I��@IG�@H�`@H�@HA�@G��@Gl�@G+@G�@F�R@FV@E�T@E`B@D�/@D��@D9X@C�F@Co@B~�@B=q@A�@A��@A7L@A&�@@�`@@��@@bN@?�w@?K�@>��@>�@>ȴ@>��@>E�@=�@<��@<�/@<�j@<�@<z�@<I�@<(�@;�m@;��@;S�@;"�@;o@:�@:��@:��@:��@:M�@9��@9��@9��@9hs@8��@8��@8�u@8r�@81'@7�;@7l�@7
=@6ȴ@6�R@6��@6��@6v�@6ff@6E�@5��@5p�@5V@4��@4�D@4(�@3��@3�m@3ƨ@3t�@333@2�@2�!@2M�@1��@1X@0��@0��@0�@0A�@0A�@0b@/�w@/�P@/+@.��@.v�@.V@.$�@-��@-�h@-/@,�/@,�j@,��@,j@,9X@+��@+ƨ@+t�@+S�@+C�@*�@*��@*M�@*-@)�@)��@)x�@)�@(��@(r�@(A�@( �@(  @'�;@'��@'\)@';d@'
=@&�@&�R@&��@&E�@&5?@&{@%�T@%@%�h@%V@$�/@$�@$I�@$9X@$9X@#��@#ƨ@#t�@#@"��@"�!@"^5@"�@"J@!��@!�@!�@!�^@!��@!x�@!7L@ ��@ �9@ �u@ A�@�@��@��@�P@�P@|�@l�@+@��@�R@��@5?@{@{@@@�T@�-@��@��@p�@O�@V@��@�D@Z@(�@��@�F@��@"�@o@@��@�\@=q@J@�#@��@�^@��@x�@hs@G�@&�@��@Ĝ@�9@�@bN@ �@�@�P@l�@K�@;d@+@�@�y@ȴ@��@��@V@{@�T@��@@�-@p�@V@��@��@��@��@�j@z�@Z@9X@�@��@�
@�
@ƨ@�@dZ@33@@�@�H@�!@��@n�@-@��@��@hs@G�@&�@&�@%@�`@�9@�u@r�@Q�@b@�@|�@l�@\)@K�@+@�@�y@ȴ@��@v�@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��
A���A���A�ĜA�ȴA���A�A�ƨA�ƨA�A�Aݰ!AݮAݡ�Aݟ�Aݝ�Aݟ�Aݟ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݛ�Aݛ�Aݛ�Aݙ�Aݙ�Aݛ�Aݛ�Aݛ�Aݝ�Aݝ�Aݝ�Aݝ�Aݝ�Aݟ�Aݛ�Aݙ�Aݗ�Aݗ�Aݕ�A݁A�S�AܓuA�;dA�"�AԋDA�A�1A�ȴA��TA΃A���A͙�ÁA��A���A���A�33A��A���A�v�A�?}A�33AÑhA�K�A�ƨA�;dA���A�O�A��^A�JA��^A�9XA���A��PA�(�A�K�A�=qA��A�^5A��
A�n�A��A�jA��A��PA�S�A�|�A�ƨA�n�A�bNA��A��A�hsA���A��A�\)A�A���A���A�
=A��A��;A��A��hA���A��`A� �A��A�^5A��-A�K�A��PA�VA�$�A���A�{A�`BA��+A�1A�C�A���A�jA��^A��A��A��A�7LA~��A{�TAx�DAv�!As+ApĜAn�yAkS�Ag��Ad�`Ac��Aa�A^�yA\JAZ9XAY/AXz�AWt�AU�AT�+AS
=APbNAN�AM�;AJ��AGt�AFI�AD�!ACXAA�^AA7LA@ �A=�hA=VA<��A<bA;|�A:ZA8ĜA6�jA5�7A4��A4v�A3/A1��A.�A,��A*v�A)?}A(�RA'�mA'
=A&�A$5?A#?}A"�DAG�AƨA�9AA�A�AjA��AdZA33A�Az�A��A�A��A  AA;dAA9XA�A5?Ax�A
��A
5?A	p�Av�A��A�!A�FAVA��Av�AI�A�
A/AVA�m@��m@�X@��!@���@��D@��w@���@��P@�l�@�ȴ@���@��`@��;@�v�@��@�bN@��
@�;d@��@�bN@��;@�J@��/@��@ߝ�@�v�@�5?@�=q@�E�@�%@ە�@�n�@��@�Q�@��@��@���@�`B@��@�r�@�  @ҸR@���@�hs@У�@��;@�K�@ΰ!@�V@�Z@��`@��@�Q�@�ƨ@��@�-@���@�@ɺ^@ə�@�Z@�\)@�(�@ȃ@Ǯ@�~�@Ə\@őh@ě�@�Z@�|�@�dZ@�t�@�I�@�A�@Õ�@�
=@��H@°!@�n�@�-@��-@�G�@��@�j@�b@���@�@���@��#@��/@��9@��@�bN@�1@�dZ@�"�@�
=@�|�@�t�@��y@�@��9@�j@�A�@� �@��
@�;d@��R@��R@�ff@�V@��T@�x�@���@�1@��m@�  @��;@�33@���@�v�@�E�@��R@��@���@�dZ@���@�hs@���@��9@�z�@���@�V@�Ĝ@��u@�b@�ƨ@�|�@�;d@��H@��+@��@��@���@��h@�hs@�X@���@�Ĝ@�z�@�(�@�1@��
@���@�\)@�+@��y@��!@��\@�-@�@��@���@���@���@�r�@�b@���@�t�@�C�@��@��R@�~�@�$�@���@�p�@��@���@���@�Z@��m@��;@��
@�ƨ@��F@��F@���@��P@�|�@�dZ@�K�@�"�@�
=@���@��R@�=q@�O�@��9@�Z@�(�@�ƨ@�t�@�o@��@���@���@�n�@��@�@��^@�G�@���@��`@���@��u@�1@���@�l�@�"�@��@�@��R@�E�@�-@�@���@�@���@��9@���@��u@��m@��
@��F@��P@��P@�l�@�K�@�;d@�33@��H@�V@�$�@��T@�7L@��@���@��9@�z�@�Q�@�(�@�1@��@�S�@�C�@���@���@�M�@��@���@��T@��@�G�@���@��/@��u@�Z@�1'@��@��@��@��F@���@�dZ@�;d@�"�@��!@�-@��^@�hs@�7L@���@��j@���@��D@�Q�@��@���@��F@�|�@�dZ@��@��H@���@��+@�^5@�5?@�$�@���@�Ĝ@��j@���@�I�@���@��y@���@�X@�G�@�G�@�V@��u@��@�A�@��@��
@��F@�+@��R@�~�@�$�@��T@��h@��7@�`B@�?}@���@�I�@�;@�@�@�P@~�@~{@}p�@|��@{�
@{��@{o@z�@z��@zn�@y�#@y&�@xbN@x1'@w��@v��@vE�@vE�@u@u/@t�j@t�@t��@t�D@s�m@s�F@s��@sS�@r��@r�!@q��@q�#@q��@p�`@p��@pA�@o�@o�P@o;d@o
=@n��@m�@mp�@m/@l�@l�@lI�@k��@kS�@j�!@jM�@j�@i��@i��@i��@i�7@i�@hĜ@g�@f�y@fȴ@f��@fff@f@e?}@dz�@d9X@c��@cS�@c"�@b��@b�\@a�@a��@ahs@`�`@`1'@_�@_�@_l�@^ȴ@^E�@]�T@]�-@]�h@]p�@]`B@]`B@]�@\�@\�@\j@[��@[dZ@["�@Z�H@Z�!@Z~�@Z=q@ZJ@Y��@Y��@Y��@YG�@Y&�@Y%@XĜ@X��@XA�@X  @W�;@W�w@W��@WK�@Vȴ@V��@U�@U`B@U�@T��@T�@TI�@S�@So@R��@R��@R�!@R~�@R-@Qx�@P��@P��@P�@PbN@O��@O��@O�@NV@N$�@N@M�T@M�-@M�@M/@L��@Lj@K��@Kƨ@KdZ@Ko@J�@J�!@JM�@JJ@I��@IG�@H�`@H�@HA�@G��@Gl�@G+@G�@F�R@FV@E�T@E`B@D�/@D��@D9X@C�F@Co@B~�@B=q@A�@A��@A7L@A&�@@�`@@��@@bN@?�w@?K�@>��@>�@>ȴ@>��@>E�@=�@<��@<�/@<�j@<�@<z�@<I�@<(�@;�m@;��@;S�@;"�@;o@:�@:��@:��@:��@:M�@9��@9��@9��@9hs@8��@8��@8�u@8r�@81'@7�;@7l�@7
=@6ȴ@6�R@6��@6��@6v�@6ff@6E�@5��@5p�@5V@4��@4�D@4(�@3��@3�m@3ƨ@3t�@333@2�@2�!@2M�@1��@1X@0��@0��@0�@0A�@0A�@0b@/�w@/�P@/+@.��@.v�@.V@.$�@-��@-�h@-/@,�/@,�j@,��@,j@,9X@+��@+ƨ@+t�@+S�@+C�@*�@*��@*M�@*-@)�@)��@)x�@)�@(��@(r�@(A�@( �@(  @'�;@'��@'\)@';d@'
=@&�@&�R@&��@&E�@&5?@&{@%�T@%@%�h@%V@$�/@$�@$I�@$9X@$9X@#��@#ƨ@#t�@#@"��@"�!@"^5@"�@"J@!��@!�@!�@!�^@!��@!x�@!7L@ ��@ �9@ �u@ A�@�@��@��@�P@�P@|�@l�@+@��@�R@��@5?@{@{@@@�T@�-@��@��@p�@O�@V@��@�D@Z@(�@��@�F@��@"�@o@@��@�\@=q@J@�#@��@�^@��@x�@hs@G�@&�@��@Ĝ@�9@�@bN@ �@�@�P@l�@K�@;d@+@�@�y@ȴ@��@��@V@{@�T@��@@�-@p�@V@��@��@��@��@�j@z�@Z@9X@�@��@�
@�
@ƨ@�@dZ@33@@�@�H@�!@��@n�@-@��@��@hs@G�@&�@&�@%@�`@�9@�u@r�@Q�@b@�@|�@l�@\)@K�@+@�@�y@ȴ@��@v�@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
��B
��B
��B
��B
��B
��B
�{B
�1B
W
B
2-B
(�B
)�B
F�B
dZB
�1B
��B
ŢB
�5B
�BoB�B�B{BoB\B
=B�B�B�B�B,B5?B:^BC�B_;Bx�B�=B�{B��B��B��B�BƨB�
B�#B�TB�B��B��B	7BoB\B+B  B��B�B�B�B�mB�ZB�5B�B��B��B�jB��B��B�PB�1Bl�BdZB^5BI�B/BJB  B
��B
�B
�B
�B
�fB
�;B
��B
��B
ȴB
B
�XB
�B
��B
��B
�JB
~�B
q�B
ffB
VB
>wB
1'B
�B

=B	��B	�yB	��B	�jB	�B	��B	�hB	�B	s�B	jB	dZB	\)B	P�B	H�B	A�B	2-B	$�B	�B	{B	+B	B��B��B��B�B�B�B�B�B�B�sB�mB�`B�BB�B�B�B�)B�B��B��B��B��B��B��B��B��B��B�^B�dB�B��B��B��B��B��B��B�{B�uB�hB�=B�7B��B��B�B�jB�wB�qB�XB�FB�LB�?B�?B�9B�-B�'B�-B�'B�!B�B�B�B�B�B�B�B��B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�'B�3B�3B�3B�9B�3B�FB�RB�XB��BÖBɺB��B��B��B��B�B�B�)B�HB�;B�NB�TB�TB�NB�HB�5B�fB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	
=B	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	,B	-B	.B	0!B	1'B	33B	6FB	:^B	>wB	C�B	F�B	M�B	R�B	R�B	T�B	W
B	\)B	^5B	_;B	dZB	cTB	dZB	e`B	ffB	m�B	u�B	x�B	�B	�B	�B	�B	�%B	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	�}B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B	��B	��B
  B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
JB
\B
\B
bB
bB
\B
\B
bB
bB
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
uB
uB
oB
oB
{B
uB
{B
oB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
 �B
 �B
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
"�B
!�B
!�B
!�B
"�B
#�B
%�B
&�B
&�B
&�B
%�B
%�B
&�B
&�B
'�B
(�B
+B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
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
5?B
49B
49B
49B
33B
49B
49B
49B
49B
5?B
7LB
7LB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
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
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
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
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
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
W
B
XB
YB
YB
YB
YB
YB
ZB
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
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
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
cTB
cTB
cTB
cTB
dZB
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
n�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
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
|�B
}�B
}�B
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
�B
�B
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
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
��B
��B
��B
��B
��B
��B
�{B
�1B
W
B
2-B
(�B
)�B
F�B
dZB
�1B
��B
ŢB
�5B
�BoB�B�B{BoB\B
=B�B�B�B�B,B5?B:^BC�B_;Bx�B�=B�{B��B��B��B�BƨB�
B�#B�TB�B��B��B	7BoB\B+B  B��B�B�B�B�mB�ZB�5B�B��B��B�jB��B��B�PB�1Bl�BdZB^5BI�B/BJB  B
��B
�B
�B
�B
�fB
�;B
��B
��B
ȴB
B
�XB
�B
��B
��B
�JB
~�B
q�B
ffB
VB
>wB
1'B
�B

=B	��B	�yB	��B	�jB	�B	��B	�hB	�B	s�B	jB	dZB	\)B	P�B	H�B	A�B	2-B	$�B	�B	{B	+B	B��B��B��B�B�B�B�B�B�B�sB�mB�`B�BB�B�B�B�)B�B��B��B��B��B��B��B��B��B��B�^B�dB�B��B��B��B��B��B��B�{B�uB�hB�=B�7B��B��B�B�jB�wB�qB�XB�FB�LB�?B�?B�9B�-B�'B�-B�'B�!B�B�B�B�B�B�B�B��B�B�B�B��B��B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�'B�3B�3B�3B�9B�3B�FB�RB�XB��BÖBɺB��B��B��B��B�B�B�)B�HB�;B�NB�TB�TB�NB�HB�5B�fB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	
=B	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	&�B	,B	-B	.B	0!B	1'B	33B	6FB	:^B	>wB	C�B	F�B	M�B	R�B	R�B	T�B	W
B	\)B	^5B	_;B	dZB	cTB	dZB	e`B	ffB	m�B	u�B	x�B	�B	�B	�B	�B	�%B	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�}B	�}B	��B	B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B	��B	��B
  B	��B	��B
  B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
JB
\B
\B
bB
bB
\B
\B
bB
bB
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
uB
uB
oB
oB
{B
uB
{B
oB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
 �B
 �B
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
"�B
!�B
!�B
!�B
"�B
#�B
%�B
&�B
&�B
&�B
%�B
%�B
&�B
&�B
'�B
(�B
+B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
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
5?B
49B
49B
49B
33B
49B
49B
49B
49B
5?B
7LB
7LB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
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
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
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
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
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
W
B
XB
YB
YB
YB
YB
YB
ZB
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
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
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
cTB
cTB
cTB
cTB
dZB
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
n�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
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
|�B
}�B
}�B
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
�B
�B
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
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20201120123954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201120034003  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20201120034003  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201120034003  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201120034004  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20201120034004  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201120034004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20201120034004  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201120034004  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201120034004                      G�O�G�O�G�O�                JA  ARUP                                                                        20201120035226                      G�O�G�O�G�O�                