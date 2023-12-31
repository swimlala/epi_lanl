CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:58Z creation;2022-06-04T19:12:59Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604191258  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @���͎�1   @��z�H@.Ƨ-�d/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BI��BO��BX  B`  Bh  Bp  Bx  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C� C�3C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp33Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ ��@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B�
B 
=B(
=B0
=B8
=B@
=BI��BO��BX
=B`
=Bh
=Bp
=Bx
=B�k�B�B���B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B���B�B�B�B���B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C��C��C��C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:)C<�C>�C@�CB�CD�CF�CH�CI��CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp5�Cr�Ct�Cu��Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW�
DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A٠�Aٞ�A٥FA٣�A٧�A٢hA٠�Aٞ�AٚkAٔ�AقA�\�A�M�A�I�A�9�A�6�A�3hA�2aA�1[A�/A�,=A�'�A�$�A� �A�uA֠'A�RTA�;A�E9A��oA�a�A�خA�x�A���AʋAɠ�A�5?A�($A�K�A��}A�9�A�|�A�MjA�poA�v�A�E�A��A�5�A�N<A��A�7A��A�!bA�m]A�c A�<6A�X�A�`�A�9�A��fA�@�A�]dA�LdA�7�A�'�A���A�rGA��
A��QA��\A���A�� A��A��A��fA�?�A�FA�(�A�@A�K�A�Q�A�GA��~A�IA�/�A���A�s�A��A�^5A�2-A��A�-A���A�5A}�ZA{�8AwیAr�yAlY�AhJAfOA`�fA[�AX�AU��AQ�!AO�xAM�+AJ��AI��AH4AF+�ACFA@FA>��A<�A:��A:�HA:B[A8��A8H�A6�KA7i�A6�jA6�A6N<A5�A4�A1��A.��A-g8A+�A*GA(�FA'?A&�A$��A$A!�LA��AFA� A�.A	A�@A:�A{�A��A�A�A�A{A�AffADgA�Af�AH�A8�A�&A=A҉A�[Ad�A"�A��A�"A6zA�AȴA�A��A��A�gA��AoAںA��A~A�rA�A�rA
�[A
A	�A�;A.IA$�A�
A'RA�hA��AN�A��A6�A�}Ag�A��A��AV�A\)A ��A �A<�A�AԕA`BA@A�AAIRA��AGEA7A �A L�A  �@��B@���@��.@���@��@��@�@���@�0U@��@��>@���@��P@�
�@�y�@�[W@�4�@�9@��Z@��@��Q@���@�!-@�@��@�zx@�Z�@�:�@�1�@�j@�l"@�@�o @㹌@�j@�@O@��@�d�@�rG@�^5@�_@�� @��@�|�@�~@�rG@�Q�@��c@��@�}�@�S@�i�@ݡ�@��@�_�@�h
@�4n@��A@۩*@ۧ�@۰�@�W?@ڟ�@�G@��@��@��@��	@ؽ<@�'R@׊	@��@֕�@�V@�G@��@շ@�J#@��,@���@ӆ�@�Vm@�+�@ҧ�@�@�@�@��@�@�!�@ώ�@��@�{�@�K�@�ȴ@�z�@�($@˃{@���@ʞ@�˒@��@Ȱ!@�\�@Ǯ@�\)@�6z@�ں@Ʊ�@�y>@�'R@ħ@á�@��@��f@�ȴ@���@�@�?@��[@��c@�r�@�b@��{@�@@�M�@�	@��@��4@�IR@��@�	@�;@�i�@�H@�,=@��@�X@�E9@�8@�/�@�C@���@��@�rG@�h�@�F�@��h@�#:@�u@��T@���@�p�@���@�Z@���@�N<@�ں@���@��@�N<@�8@�V@���@��e@���@�K^@�@�j�@�F@�4@�
=@�ѷ@�N�@��}@�#�@��}@�z@�C-@��@�;d@��@��@���@�h�@��{@��@��'@���@���@�M@���@�O@�(�@��@��,@��R@�Z@�e@�	@��j@��P@�*0@��@���@�r�@�!�@��W@�G�@��`@�{�@�-@��A@��q@�Dg@��@�@��8@��_@��@���@��@�c�@�A�@��@�{�@��d@�g�@���@�(�@�	@��
@�8�@��@�@��@��@�B[@�@��@���@�%F@��E@���@�kQ@�L0@�)�@���@���@�@���@�j@�8�@�5�@�+@���@�u%@��@�t�@�/@���@��m@�a|@�A�@�&�@�	@���@��j@�ƨ@�qv@���@�n�@�N�@�!@�t�@�RT@�Mj@�?}@�	l@��y@���@��u@�7@�X@�*0@��@��@��m@�oi@�/�@�{@�u@��&@��@�|�@�!�@��A@�5?@��@�1@��@�S�@��@���@�h
@�5?@���@���@���@�2a@�@��@�ѷ@�z�@�-�@�1@��#@���@��4@�8�@�%F@�C@��@�S@��@��f@���@��A@�bN@�:�@��@�1@��@��}@��X@�k�@��@��h@�oi@�!�@���@��S@���@�|@�J#@���@���@�bN@�5?@�g@]�@8@C@~�8@~�m@~xl@~.�@}��@}��@}Y�@|�/@|y>@|[�@|�@{��@{n/@{'�@zff@yf�@x�@x��@x��@x?�@w�r@w��@wo�@w,�@v��@vR�@u�N@u\�@uL�@u+@t��@t"h@s��@s�@r�m@r.�@q��@q��@q@p��@p�O@p'R@o��@o��@o�@n��@ne@m�@m��@m�X@m}�@m2a@l��@l��@k˒@j�'@jM�@j�@ix�@h��@h�O@h�u@hU2@h�@gݘ@gO@f�!@f�x@f��@f��@fe@e�X@ej@d��@dq@de�@dS�@c�;@cJ#@b�s@a�Z@aX@`�@`j@`4n@`b@_�@_��@_
=@^�,@^q�@]�.@]��@]}�@]<6@]�@\�@[�W@[+@Z��@Y�D@Y-w@X�D@XK^@W��@V��@V-@U�3@UIR@U�@T�p@Tg8@Sj�@R��@Rxl@R4@Q�@Qx�@Q�@P�v@P��@P4n@O��@OS�@O�@N��@NOv@N
�@M�S@L��@L�@K33@J��@J~�@J6�@I�3@I+�@H��@Hy>@G��@G��@Gb�@G@O@G+@G�@F�M@FV@E�o@E��@Ej@D�@D�.@C��@C��@C|�@CA�@B��@Bc @B_@A�j@A��@A�h@A5�@A@@@��@@��@@�U@?�W@?�}@?��@?J#@?&@>��@>Z�@>=q@>
�@=�@=�@=4@<��@<��@<Q�@<�@;��@;�@:�\@:c @:0U@9��@9�@9c�@8�P@8�)@8�z@8g8@8b@7�@@7e�@7;d@6�@6ں@6��@6�@5��@5^�@5;@4�@4|�@4-�@4�@3�@3��@2��@2�@2p;@21�@1�H@1��@1hs@14@0�	@0��@0�D@0m�@0,=@/��@/��@/�@/��@/)_@.��@.�h@.�6@.xl@.�@-��@-w2@--w@-�@,�@,��@,��@,`�@+�
@+n/@+P�@+$t@*��@*��@*��@*Z�@*O@)�@)zx@)`B@)IR@)(�@(�	@(��@(g8@(6@(�@'��@'�@'�0@'�@&�@&i�@&;�@&-@%�#@%Y�@%�@$�P@$�e@$Xy@#�]@#�&@#�g@#�P@#.I@"�1@"h
@")�@!��@!��@!�~@!^�@!?}@!�@ �@ K^@   @�Q@��@@��@.�@	@�@��@`B@-w@�f@֡@�$@Ft@�K@�0@��@n/@1�@�@��@��@}V@^5@J�@+k@�)@�^@�=@+�@�@�@�$@q@G@�m@�@y�@_p@/�@+@S@��@�2@�,@�R@YK@e@��@��@��@�N@�3@�t@��@�~@��@c@rG@Y�@/@�@m�@%�@�@�K@�[@��@�4@e�@4�@.I@o@�@҉@�@��@��@�A@c @W�@C�@O@��@�H@��@%F@�@��@�D@V�@$@�@�;@y�@K�@�@��@�!@�1@\�@+k@�@{@�@��@X@-w@��@�	@��@��@��@�@w�@g8@H@2�@�@�@��@�
@�K@��@t�@Mj@"�@
�"@
�@
�@
ȴ@
�A@
Ov@
1�@
)�@
O@	��@	�^@	��@	o @	5�@��@�p@��@�@:�@@�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A٠�Aٞ�A٥FA٣�A٧�A٢hA٠�Aٞ�AٚkAٔ�AقA�\�A�M�A�I�A�9�A�6�A�3hA�2aA�1[A�/A�,=A�'�A�$�A� �A�uA֠'A�RTA�;A�E9A��oA�a�A�خA�x�A���AʋAɠ�A�5?A�($A�K�A��}A�9�A�|�A�MjA�poA�v�A�E�A��A�5�A�N<A��A�7A��A�!bA�m]A�c A�<6A�X�A�`�A�9�A��fA�@�A�]dA�LdA�7�A�'�A���A�rGA��
A��QA��\A���A�� A��A��A��fA�?�A�FA�(�A�@A�K�A�Q�A�GA��~A�IA�/�A���A�s�A��A�^5A�2-A��A�-A���A�5A}�ZA{�8AwیAr�yAlY�AhJAfOA`�fA[�AX�AU��AQ�!AO�xAM�+AJ��AI��AH4AF+�ACFA@FA>��A<�A:��A:�HA:B[A8��A8H�A6�KA7i�A6�jA6�A6N<A5�A4�A1��A.��A-g8A+�A*GA(�FA'?A&�A$��A$A!�LA��AFA� A�.A	A�@A:�A{�A��A�A�A�A{A�AffADgA�Af�AH�A8�A�&A=A҉A�[Ad�A"�A��A�"A6zA�AȴA�A��A��A�gA��AoAںA��A~A�rA�A�rA
�[A
A	�A�;A.IA$�A�
A'RA�hA��AN�A��A6�A�}Ag�A��A��AV�A\)A ��A �A<�A�AԕA`BA@A�AAIRA��AGEA7A �A L�A  �@��B@���@��.@���@��@��@�@���@�0U@��@��>@���@��P@�
�@�y�@�[W@�4�@�9@��Z@��@��Q@���@�!-@�@��@�zx@�Z�@�:�@�1�@�j@�l"@�@�o @㹌@�j@�@O@��@�d�@�rG@�^5@�_@�� @��@�|�@�~@�rG@�Q�@��c@��@�}�@�S@�i�@ݡ�@��@�_�@�h
@�4n@��A@۩*@ۧ�@۰�@�W?@ڟ�@�G@��@��@��@��	@ؽ<@�'R@׊	@��@֕�@�V@�G@��@շ@�J#@��,@���@ӆ�@�Vm@�+�@ҧ�@�@�@�@��@�@�!�@ώ�@��@�{�@�K�@�ȴ@�z�@�($@˃{@���@ʞ@�˒@��@Ȱ!@�\�@Ǯ@�\)@�6z@�ں@Ʊ�@�y>@�'R@ħ@á�@��@��f@�ȴ@���@�@�?@��[@��c@�r�@�b@��{@�@@�M�@�	@��@��4@�IR@��@�	@�;@�i�@�H@�,=@��@�X@�E9@�8@�/�@�C@���@��@�rG@�h�@�F�@��h@�#:@�u@��T@���@�p�@���@�Z@���@�N<@�ں@���@��@�N<@�8@�V@���@��e@���@�K^@�@�j�@�F@�4@�
=@�ѷ@�N�@��}@�#�@��}@�z@�C-@��@�;d@��@��@���@�h�@��{@��@��'@���@���@�M@���@�O@�(�@��@��,@��R@�Z@�e@�	@��j@��P@�*0@��@���@�r�@�!�@��W@�G�@��`@�{�@�-@��A@��q@�Dg@��@�@��8@��_@��@���@��@�c�@�A�@��@�{�@��d@�g�@���@�(�@�	@��
@�8�@��@�@��@��@�B[@�@��@���@�%F@��E@���@�kQ@�L0@�)�@���@���@�@���@�j@�8�@�5�@�+@���@�u%@��@�t�@�/@���@��m@�a|@�A�@�&�@�	@���@��j@�ƨ@�qv@���@�n�@�N�@�!@�t�@�RT@�Mj@�?}@�	l@��y@���@��u@�7@�X@�*0@��@��@��m@�oi@�/�@�{@�u@��&@��@�|�@�!�@��A@�5?@��@�1@��@�S�@��@���@�h
@�5?@���@���@���@�2a@�@��@�ѷ@�z�@�-�@�1@��#@���@��4@�8�@�%F@�C@��@�S@��@��f@���@��A@�bN@�:�@��@�1@��@��}@��X@�k�@��@��h@�oi@�!�@���@��S@���@�|@�J#@���@���@�bN@�5?@�g@]�@8@C@~�8@~�m@~xl@~.�@}��@}��@}Y�@|�/@|y>@|[�@|�@{��@{n/@{'�@zff@yf�@x�@x��@x��@x?�@w�r@w��@wo�@w,�@v��@vR�@u�N@u\�@uL�@u+@t��@t"h@s��@s�@r�m@r.�@q��@q��@q@p��@p�O@p'R@o��@o��@o�@n��@ne@m�@m��@m�X@m}�@m2a@l��@l��@k˒@j�'@jM�@j�@ix�@h��@h�O@h�u@hU2@h�@gݘ@gO@f�!@f�x@f��@f��@fe@e�X@ej@d��@dq@de�@dS�@c�;@cJ#@b�s@a�Z@aX@`�@`j@`4n@`b@_�@_��@_
=@^�,@^q�@]�.@]��@]}�@]<6@]�@\�@[�W@[+@Z��@Y�D@Y-w@X�D@XK^@W��@V��@V-@U�3@UIR@U�@T�p@Tg8@Sj�@R��@Rxl@R4@Q�@Qx�@Q�@P�v@P��@P4n@O��@OS�@O�@N��@NOv@N
�@M�S@L��@L�@K33@J��@J~�@J6�@I�3@I+�@H��@Hy>@G��@G��@Gb�@G@O@G+@G�@F�M@FV@E�o@E��@Ej@D�@D�.@C��@C��@C|�@CA�@B��@Bc @B_@A�j@A��@A�h@A5�@A@@@��@@��@@�U@?�W@?�}@?��@?J#@?&@>��@>Z�@>=q@>
�@=�@=�@=4@<��@<��@<Q�@<�@;��@;�@:�\@:c @:0U@9��@9�@9c�@8�P@8�)@8�z@8g8@8b@7�@@7e�@7;d@6�@6ں@6��@6�@5��@5^�@5;@4�@4|�@4-�@4�@3�@3��@2��@2�@2p;@21�@1�H@1��@1hs@14@0�	@0��@0�D@0m�@0,=@/��@/��@/�@/��@/)_@.��@.�h@.�6@.xl@.�@-��@-w2@--w@-�@,�@,��@,��@,`�@+�
@+n/@+P�@+$t@*��@*��@*��@*Z�@*O@)�@)zx@)`B@)IR@)(�@(�	@(��@(g8@(6@(�@'��@'�@'�0@'�@&�@&i�@&;�@&-@%�#@%Y�@%�@$�P@$�e@$Xy@#�]@#�&@#�g@#�P@#.I@"�1@"h
@")�@!��@!��@!�~@!^�@!?}@!�@ �@ K^@   @�Q@��@@��@.�@	@�@��@`B@-w@�f@֡@�$@Ft@�K@�0@��@n/@1�@�@��@��@}V@^5@J�@+k@�)@�^@�=@+�@�@�@�$@q@G@�m@�@y�@_p@/�@+@S@��@�2@�,@�R@YK@e@��@��@��@�N@�3@�t@��@�~@��@c@rG@Y�@/@�@m�@%�@�@�K@�[@��@�4@e�@4�@.I@o@�@҉@�@��@��@�A@c @W�@C�@O@��@�H@��@%F@�@��@�D@V�@$@�@�;@y�@K�@�@��@�!@�1@\�@+k@�@{@�@��@X@-w@��@�	@��@��@��@�@w�@g8@H@2�@�@�@��@�
@�K@��@t�@Mj@"�@
�"@
�@
�@
ȴ@
�A@
Ov@
1�@
)�@
O@	��@	�^@	��@	o @	5�@��@�p@��@�@:�@@�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	W�B	XB	W�B	W�B	W�B	X+B	XEB	X�B	YB	Y�B	[WB	^jB	_!B	_�B	a-B	bhB	c�B	d�B	eFB	f2B	f�B	gmB	gRB	gB	i_B	��B	��B	�ZB	�$B	�3B	��B	��B	��B	�$B	�<B	�cB	˒B	�	B	�kB
B
'�B
T,B
hsB
��B
�sB
�QB
�LB
�BoBB8lBNVBS&BX�By�B� B�B}B�[B�B��B�B��B��B��B��B�
B��B�
B}�B]B-CB	B
�RB
�B
�B
��B
�-B
�:B
��B
��B
�@B
�aB
ܬB
�B
�2B
��B
��B
��B
q[B
f�B
9�B
0�B
(�B
�B
�B	�bB	�]B	��B	y�B	gmB	H1B	-)B	kB	�B�B�|B�B�B��B�!B��BյB�B�[B�LB��B�_B��B��B�hB�)BƨB��B�B��B�B��B�kBևB�sBөBרB��B��B�=B��B��B�9B�FB��B��B�qB	UB	�B	�B		RB	�B	EB	�B	"�B	#B	#�B	&2B	/�B	6�B	AoB	EB	F%B	D�B	O�B	X�B	gRB	o�B	q�B	t�B	y�B	~�B	�AB	�XB	�zB	��B	�3B	�OB	~�B	~wB	HB	� B	~wB	|6B	|�B	�B	�B	{JB	x�B	v�B	w2B	z*B	yrB	rGB	p�B	m�B	k�B	j�B	i�B	i*B	kB	kQB	kQB	jKB	l"B	m�B	sB	�SB	��B	��B	��B	�B	��B	�JB	��B	�B	�PB	�B	��B	�DB	��B	��B	��B	��B	��B	�(B	�6B	��B	�^B	��B	�$B	��B	�FB	��B	�WB	�fB	�OB	�hB	�B	�?B	�3B	��B	�wB	�B	�eB	�B	��B	�B	��B	�)B	��B	� B	�UB	�B	��B	�7B	�)B	�rB	�RB	��B	ňB	�B	��B	�xB	��B	��B	�\B	�HB	�4B	�}B	�BB	��B	��B	̈́B	��B	��B	�B	ЗB	�hB	ҽB	յB	�gB	��B	ӏB	�@B	�{B	��B	�mB	�EB	ٚB	�#B	��B	��B	��B	�WB	��B	��B	�)B	ܬB	��B	�/B	��B	��B	�B	�IB	�/B	�B	ܬB	�CB	��B	�B	�dB	�B	��B	��B	��B	�IB	ܒB	ܬB	��B	�'B	�BB	�bB	�HB	��B	��B	��B	��B	��B	�|B	�B	�TB	�B	�B	�&B	�ZB	��B	��B	�B	�B	�B	��B	�zB	�B	��B	�B	�$B	�B	�eB	��B	�qB	�B	��B	��B	��B	�/B	�5B	�OB	�B	�OB	�OB	�B	�UB	�B	�oB	�;B	�B	��B	�'B	�AB	�[B	�B	��B	�MB	�B	�B	�B	��B	�B	�TB	��B	�B	�TB	�9B	�9B	��B	��B	��B	�ZB	��B	��B	��B	�B	�+B	�B	��B	�B	��B	�>B	��B	��B	�B	�dB	��B	��B	�B	��B	��B	�BB	��B	�]B	��B	�HB	��B
 �B
 �B
 B
UB
UB
'B
�B
�B
�B
GB
MB
9B
9B
B
B
?B
YB
?B
%B
�B
?B
tB
zB
�B
	�B

#B

=B

=B

=B

	B

	B
	�B
�B
�B
�B
�B
�B
	lB
	lB
	lB
	lB
	�B

	B

=B

=B

�B
^B
�B
JB
~B
�B
B
jB
�B
�B
"B
�B
BB
B
BB
B
�B
:B
B
�B
FB
�B
�B
gB
�B
B
B
SB
SB

B
�B
�B
�B
B
�B
�B
�B
�B
)B
CB
)B
]B
�B
IB
�B
�B
OB
�B
B
pB
�B
pB
pB
�B
 BB
 vB
�B
 B
 BB
 B
�B
 'B
 BB
�B
 'B
 �B
!B
!B
!�B
"NB
"�B
"hB
"�B
#�B
#�B
$@B
$tB
$tB
%,B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
'B
'B
'B
'RB
'�B
'�B
'�B
'�B
(>B
(�B
)DB
)�B
*eB
+kB
+kB
+kB
+QB
+�B
,�B
,�B
-B
-]B
-�B
.B
.B
.B
./B
./B
.cB
.�B
.�B
.�B
.�B
/iB
/�B
/OB
/�B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2B
2B
3�B
5B
4�B
5%B
5�B
6B
6�B
6�B
7B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
8RB
8�B
8�B
8�B
8lB
8lB
8RB
88B
8�B
9	B
9rB
9�B
9XB
9XB
9�B
:*B
:�B
;B
;B
;0B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
>]B
>�B
?B
?.B
?.B
?.B
?�B
@ B
@B
@iB
@�B
@�B
@�B
@�B
@�B
AB
A�B
B'B
BuB
B�B
CaB
C�B
C�B
D3B
D�B
EB
EmB
E�B
E�B
E�B
E�B
F�B
F�B
GEB
G_B
GzB
G�B
H1B
HB
HB
H�B
I7B
IB
IlB
I�B
J	B
J	B
JrB
KB
K�B
LdB
L�B
L�B
MB
MB
M�B
NB
NVB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PB
PB
PHB
P�B
QB
Q�B
Q�B
R B
R:B
R:B
RoB
SB
S@B
S[B
S�B
S�B
T{B
TFB
TFB
T�B
UgB
U�B
U�B
U�B
U2B
UB
U�B
U�B
U�B
UgB
U�B
U�B
VSB
V�B
V�B
V�B
WYB
W�B
W�B
XEB
XEB
X�B
X�B
YB
YeB
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\B
\CB
\xB
\�B
\�B
\�B
\�B
]/B
]B
]/B
]dB
]�B
]�B
]�B
^B
^5B
^5B
^jB
^jB
^�B
_B
_B
_B
_VB
_�B
`vB
`vB
`vB
`�B
aB
aB
`�B
aHB
a�B
bB
b4B
b4B
bhB
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
ffB
f�B
gB
g8B
gB
gmB
g�B
h
B
h
B
hsB
h�B
h�B
h�B
h�B
iB
iDB
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
j�B
k6B
kkB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
nB
n�B
oB
o B
o B
oOB
oiB
o�B
o�B
o�B
p!B
p;B
pUB
poB
p�B
poB
p�B
q'B
qAB
q[B
qvB
q�B
r-B
r-B
raB
raB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tB
tB
tTB
tnB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v�B
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xB
x�B
y	B
y	B
y>B
yXB
yrB
yrB
y�B
zB
zDB
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
B
HB
.B
.B
.B
HB
�B
�B
� B
�B
��B
��B
��B
��B
�B
�;B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	W�B	XB	W�B	W�B	W�B	X+B	XEB	X�B	YB	Y�B	[WB	^jB	_!B	_�B	a-B	bhB	c�B	d�B	eFB	f2B	f�B	gmB	gRB	gB	i_B	��B	��B	�ZB	�$B	�3B	��B	��B	��B	�$B	�<B	�cB	˒B	�	B	�kB
B
'�B
T,B
hsB
��B
�sB
�QB
�LB
�BoBB8lBNVBS&BX�By�B� B�B}B�[B�B��B�B��B��B��B��B�
B��B�
B}�B]B-CB	B
�RB
�B
�B
��B
�-B
�:B
��B
��B
�@B
�aB
ܬB
�B
�2B
��B
��B
��B
q[B
f�B
9�B
0�B
(�B
�B
�B	�bB	�]B	��B	y�B	gmB	H1B	-)B	kB	�B�B�|B�B�B��B�!B��BյB�B�[B�LB��B�_B��B��B�hB�)BƨB��B�B��B�B��B�kBևB�sBөBרB��B��B�=B��B��B�9B�FB��B��B�qB	UB	�B	�B		RB	�B	EB	�B	"�B	#B	#�B	&2B	/�B	6�B	AoB	EB	F%B	D�B	O�B	X�B	gRB	o�B	q�B	t�B	y�B	~�B	�AB	�XB	�zB	��B	�3B	�OB	~�B	~wB	HB	� B	~wB	|6B	|�B	�B	�B	{JB	x�B	v�B	w2B	z*B	yrB	rGB	p�B	m�B	k�B	j�B	i�B	i*B	kB	kQB	kQB	jKB	l"B	m�B	sB	�SB	��B	��B	��B	�B	��B	�JB	��B	�B	�PB	�B	��B	�DB	��B	��B	��B	��B	��B	�(B	�6B	��B	�^B	��B	�$B	��B	�FB	��B	�WB	�fB	�OB	�hB	�B	�?B	�3B	��B	�wB	�B	�eB	�B	��B	�B	��B	�)B	��B	� B	�UB	�B	��B	�7B	�)B	�rB	�RB	��B	ňB	�B	��B	�xB	��B	��B	�\B	�HB	�4B	�}B	�BB	��B	��B	̈́B	��B	��B	�B	ЗB	�hB	ҽB	յB	�gB	��B	ӏB	�@B	�{B	��B	�mB	�EB	ٚB	�#B	��B	��B	��B	�WB	��B	��B	�)B	ܬB	��B	�/B	��B	��B	�B	�IB	�/B	�B	ܬB	�CB	��B	�B	�dB	�B	��B	��B	��B	�IB	ܒB	ܬB	��B	�'B	�BB	�bB	�HB	��B	��B	��B	��B	��B	�|B	�B	�TB	�B	�B	�&B	�ZB	��B	��B	�B	�B	�B	��B	�zB	�B	��B	�B	�$B	�B	�eB	��B	�qB	�B	��B	��B	��B	�/B	�5B	�OB	�B	�OB	�OB	�B	�UB	�B	�oB	�;B	�B	��B	�'B	�AB	�[B	�B	��B	�MB	�B	�B	�B	��B	�B	�TB	��B	�B	�TB	�9B	�9B	��B	��B	��B	�ZB	��B	��B	��B	�B	�+B	�B	��B	�B	��B	�>B	��B	��B	�B	�dB	��B	��B	�B	��B	��B	�BB	��B	�]B	��B	�HB	��B
 �B
 �B
 B
UB
UB
'B
�B
�B
�B
GB
MB
9B
9B
B
B
?B
YB
?B
%B
�B
?B
tB
zB
�B
	�B

#B

=B

=B

=B

	B

	B
	�B
�B
�B
�B
�B
�B
	lB
	lB
	lB
	lB
	�B

	B

=B

=B

�B
^B
�B
JB
~B
�B
B
jB
�B
�B
"B
�B
BB
B
BB
B
�B
:B
B
�B
FB
�B
�B
gB
�B
B
B
SB
SB

B
�B
�B
�B
B
�B
�B
�B
�B
)B
CB
)B
]B
�B
IB
�B
�B
OB
�B
B
pB
�B
pB
pB
�B
 BB
 vB
�B
 B
 BB
 B
�B
 'B
 BB
�B
 'B
 �B
!B
!B
!�B
"NB
"�B
"hB
"�B
#�B
#�B
$@B
$tB
$tB
%,B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&fB
'B
'B
'B
'RB
'�B
'�B
'�B
'�B
(>B
(�B
)DB
)�B
*eB
+kB
+kB
+kB
+QB
+�B
,�B
,�B
-B
-]B
-�B
.B
.B
.B
./B
./B
.cB
.�B
.�B
.�B
.�B
/iB
/�B
/OB
/�B
/�B
/�B
/�B
0UB
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2B
2B
3�B
5B
4�B
5%B
5�B
6B
6�B
6�B
7B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
8RB
8�B
8�B
8�B
8lB
8lB
8RB
88B
8�B
9	B
9rB
9�B
9XB
9XB
9�B
:*B
:�B
;B
;B
;0B
;�B
;�B
;�B
;�B
;�B
;�B
<B
<B
<�B
<�B
<�B
<�B
<�B
<�B
="B
=�B
>]B
>�B
?B
?.B
?.B
?.B
?�B
@ B
@B
@iB
@�B
@�B
@�B
@�B
@�B
AB
A�B
B'B
BuB
B�B
CaB
C�B
C�B
D3B
D�B
EB
EmB
E�B
E�B
E�B
E�B
F�B
F�B
GEB
G_B
GzB
G�B
H1B
HB
HB
H�B
I7B
IB
IlB
I�B
J	B
J	B
JrB
KB
K�B
LdB
L�B
L�B
MB
MB
M�B
NB
NVB
OvB
O�B
O�B
O�B
O�B
O�B
O�B
PB
PB
PHB
P�B
QB
Q�B
Q�B
R B
R:B
R:B
RoB
SB
S@B
S[B
S�B
S�B
T{B
TFB
TFB
T�B
UgB
U�B
U�B
U�B
U2B
UB
U�B
U�B
U�B
UgB
U�B
U�B
VSB
V�B
V�B
V�B
WYB
W�B
W�B
XEB
XEB
X�B
X�B
YB
YeB
Y�B
Y�B
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\B
\CB
\xB
\�B
\�B
\�B
\�B
]/B
]B
]/B
]dB
]�B
]�B
]�B
^B
^5B
^5B
^jB
^jB
^�B
_B
_B
_B
_VB
_�B
`vB
`vB
`vB
`�B
aB
aB
`�B
aHB
a�B
bB
b4B
b4B
bhB
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
ffB
f�B
gB
g8B
gB
gmB
g�B
h
B
h
B
hsB
h�B
h�B
h�B
h�B
iB
iDB
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
j�B
k6B
kkB
k�B
k�B
k�B
lWB
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
nB
n�B
oB
o B
o B
oOB
oiB
o�B
o�B
o�B
p!B
p;B
pUB
poB
p�B
poB
p�B
q'B
qAB
q[B
qvB
q�B
r-B
r-B
raB
raB
raB
r�B
r�B
r�B
r�B
sB
sMB
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tB
tB
tTB
tnB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
utB
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v�B
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
xB
x�B
y	B
y	B
y>B
yXB
yrB
yrB
y�B
zB
zDB
z�B
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
B
HB
.B
.B
.B
HB
�B
�B
� B
�B
��B
��B
��B
��B
�B
�;B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191258  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191258  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191259                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041306  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041306  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                