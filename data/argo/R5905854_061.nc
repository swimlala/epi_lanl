CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:55:32Z creation;2022-06-04T17:55:32Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175532  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               =A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�=�뵪J1   @�=�B��@02���m�c;dZ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB133B8��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�ffB�  B���B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�33B���B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C33C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ33C[�fC^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�C3Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(p�B1=pB8�
B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�k�B�B���B�B�B�B�B�k�B���B�B�B�B�B�B�B�B�B�B�B�B�k�B���B�8RB���B���B���B�B�B�B�B�B�C �C�C�C�C�C
�C)C5�C��C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4)C6)C8�C9��C;��C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ5�C[��C^�C_��Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C��{C��{C�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D 
D �
D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DXz>DX�>DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^�
D_
D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx
Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRDýD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RD�}D��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�C�Dۃ�D��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��[AܭwA܆�A�VAۚkA�~]A�x�A�sA�tTA�zDA�qA�e,A�z�Aۊ�Aۑ�Aې�AۉlA�.A�uZA�jA�e,A�`BA��A�r�A�"hAٞ�A��A�^A��A�aHA�L�A��pA��(A�8RA�A�͟A�MA�I�A���A�A A���A���A���A���A��tA�\�A��A���A���A��6A�{A��_A��eA�A�A�p�A�hA��A��A�H�A���A��{A��DA�%A�3�A���A�h�A���A�i�A��RA��A�1[A���A�OBA��mA��A�qAA�j�A�v`A�IA��dA�ݘA���A��+A���A�c�A�EA�ɺA�yrA�tTA���A��nAxXyAr�rAn0�Ah�EAe��Ad��Ab7LA[
�AV��AU�APjAN��AJ�;AHQAEخAD��ACYAA��A?��A?n�A>�gA>�MA>^5A<��A<�A;�$A< �A;�dA:�/A:�A7��A6�"A6A6��A6�A5  A3!�A2��A2��A1��A/�gA.��A-�A,�A*q�A)ѷA(�,A'�`A&�A%�zA%VA$�vA$�A#�A#@�A"N�A!��A!ƨA![�A!6A �&A =A A�A��A��AH�A˒AN<A�A�[A��A!�A��A�]A]dAI�A��AbNA��A�RA��A�'A��A}�AA�A��AC�A�UA�'AcAcA҉A��A($A��A7LA iA֡A�!A��A�mA�bA)_A�A��A��A��A�)A�A�A��A��A8�A�A8A?�A_�A|�A��A�oA@�A�~A
��A
+A}VA�eA~Ae�A�A�Aw2A!-A��A� A<�A�zA�A��A2�A �8A M�@�@�!@���@��M@�S@��@�_�@���@�%F@��j@��@���@�X@���@�=q@��C@��4@���@���@��@�@��@�{J@��@���@��8@�l"@���@�C-@��@�o @�@��@�o @���@��@�_@��@�W?@�*0@�<�@���@�^@�b�@��@�j@�Q@�خ@�~@��"@�|�@ߥ@�͟@�]d@ݓ�@�!�@��s@ܔF@�q�@�N�@���@�1�@ڵ�@�2�@�w2@��@�}V@���@׮@��@���@ւA@�n�@�ff@�n�@�Ta@��&@��@�i�@ӕ�@�t�@�(@�oi@ъ�@�@���@Ќ�@�6�@��)@��@�&�@�Ɇ@΃�@�_@�{@���@�@�4�@�n�@���@���@���@ǜ�@�Y�@�@ƃ@Ş�@�S&@� i@�n�@��A@Ã{@�@O@��@���@¯O@�h�@���@�A @�/@�(�@��@�1@�|@� \@��p@�.�@��K@�n/@�҉@��$@��=@�RT@�_p@�S�@�@@��X@���@�.�@�8�@��y@�ں@���@�)�@��[@�x�@�%F@�oi@�J@��j@���@��@��9@�Z@��D@��0@��S@�<6@��P@���@�D�@�4@��@�a@��@���@�M@��@���@� �@��*@�\)@�V@��m@�u%@�`�@�2�@���@�u�@�Q�@�-w@�C�@�w2@�
=@���@�D�@�2�@�M@���@�&@�8�@��w@���@���@�IR@�,�@�@���@��@�v�@�Ft@�	�@���@�u�@�IR@��9@��@�xl@�L0@��[@�j�@���@�/�@��D@��@��'@�;d@��c@��@���@�IR@��@���@�tT@�Z�@��@��"@�/�@��,@�2�@���@��S@�B�@��	@��e@�g8@�J�@�@��T@���@��*@�E9@�҉@��4@�R�@�s�@�>�@�&�@��@��@�s�@�R�@���@�@O@�@��X@��@�V@�($@��@��q@�:�@��y@���@�@�dZ@��|@��Y@�Q�@��.@���@�s@�@@��}@�v�@�J�@��@�@��#@���@��4@�Q�@�;@��U@��F@�R�@���@��@���@��[@���@�c�@�Dg@�$t@�֡@�bN@�M@��@��g@��@��X@�`B@�!�@�
=@��@��9@�H�@�+k@�@�_@���@��C@��4@�qv@�F�@��2@��'@��e@��@�:*@���@��d@���@��$@�hs@�9�@���@���@�$@��H@��M@�=�@���@��]@���@���@�V@�7@�l�@�8�@��P@�Ɇ@�h
@�;�@�Xy@�&�@��3@�|@�hs@�S�@�$t@�@�@�f@o@}�9@}B�@}+@|Ĝ@|~@{��@{�{@z�@z=q@z	@yhs@y/@yo @yG�@y�@x�)@x9X@w��@wdZ@w@vM�@v	@u�@t��@t��@t_@s��@s��@s��@s"�@r�@r$�@q�-@q��@qT�@qL�@q%F@p�K@pH@p�@o��@o8@o�@n�+@m�'@l��@l[�@l7@l�@k�F@kS�@k�@k�@j��@jL0@i�@iG�@h�z@h/�@g��@g4�@f�@f�y@f�B@f�@fd�@f1�@fQ@f-@e��@e��@eY�@e�@d�)@c�@cdZ@b��@bL0@ap�@`��@`:�@`"h@_�W@_��@_n/@_@O@^ߤ@^� @^GE@]�N@]^�@]%F@\��@\��@\~@[��@[~�@[=@Z�@Y��@Yk�@X��@XC-@W��@Wqv@V��@V3�@U��@U��@UN<@T��@T�[@T�I@Ty>@T  @Sݘ@S�F@SP�@Rȴ@Rv�@RC�@R@Q��@QF@P�E@P|�@P!@O�Q@O�@O��@O�@N�@N�b@NR�@M��@Mk�@MV@L��@LFt@K��@KS�@J�"@J�,@JkQ@J)�@I��@I��@I	l@HbN@H�@G��@G\)@F�H@F}V@F@�@E�@E��@EIR@D�z@D�.@D�@DD�@D!@C� @C$t@B�y@Bp;@A�)@A��@AX@AN<@A2a@@��@@�o@@*�@?�@?�;@?��@?;d@>��@>�@>�@=�@=rG@=�@<�@<,=@<  @;�}@;��@;E9@;o@:�H@:^5@:e@9�Z@9�"@9p�@9X@8��@8��@8��@8]d@7� @7�@7�@6�H@6�'@6�@6}V@6c @6GE@6J@5��@5rG@5�@4�@4�@4|�@4	�@3��@3=@2��@2��@2��@2}V@2@1��@1�h@1w2@1J�@1�@0�`@0��@0S�@0	�@/�:@/A�@.�"@.��@.H�@-�Z@-�N@-��@-x�@-e,@-`B@-Q�@,�P@,�/@,��@,`�@,Ft@,$@+�g@+O@+�@*�,@*��@*{�@*&�@*e@)��@)w2@)!�@(�/@(��@(-�@'�]@'خ@'�4@'33@'�@&ں@&��@&��@&;�@%�@%��@%��@%8�@$�@$�@$��@$l"@$~@#��@#��@#O@#�@"�L@"��@"E�@"@" �@!��@!��@!�~@!O�@!�@ ��@ K^@ 7�@ /�@�A@�:@y�@o�@E9@�@��@��@ff@.�@�o@�d@��@O�@=�@*0@%@��@�?@�I@~(@Q�@2�@�@��@�@��@�@�R@��@�+@=q@�@�@�@ԕ@�t@�S@��@|@c�@O�@&�@�@�`@�@oi@j@D�@��@��@v`@b�@P�@o@��@�h@�!@}V@C�@)�@��@�X@IR@V@��@��@�@m�@D�@x@��@�[@��@iD@P�@=@!-@ i@��@��@V@@�@��@w2@a�@:�@�@��@�?@�O@��@�Y@N�@%�@�@��@qv@X�@8@S@�<@�b@��@��@J�@e@�@��@e,@@@�	@�`@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��[AܭwA܆�A�VAۚkA�~]A�x�A�sA�tTA�zDA�qA�e,A�z�Aۊ�Aۑ�Aې�AۉlA�.A�uZA�jA�e,A�`BA��A�r�A�"hAٞ�A��A�^A��A�aHA�L�A��pA��(A�8RA�A�͟A�MA�I�A���A�A A���A���A���A���A��tA�\�A��A���A���A��6A�{A��_A��eA�A�A�p�A�hA��A��A�H�A���A��{A��DA�%A�3�A���A�h�A���A�i�A��RA��A�1[A���A�OBA��mA��A�qAA�j�A�v`A�IA��dA�ݘA���A��+A���A�c�A�EA�ɺA�yrA�tTA���A��nAxXyAr�rAn0�Ah�EAe��Ad��Ab7LA[
�AV��AU�APjAN��AJ�;AHQAEخAD��ACYAA��A?��A?n�A>�gA>�MA>^5A<��A<�A;�$A< �A;�dA:�/A:�A7��A6�"A6A6��A6�A5  A3!�A2��A2��A1��A/�gA.��A-�A,�A*q�A)ѷA(�,A'�`A&�A%�zA%VA$�vA$�A#�A#@�A"N�A!��A!ƨA![�A!6A �&A =A A�A��A��AH�A˒AN<A�A�[A��A!�A��A�]A]dAI�A��AbNA��A�RA��A�'A��A}�AA�A��AC�A�UA�'AcAcA҉A��A($A��A7LA iA֡A�!A��A�mA�bA)_A�A��A��A��A�)A�A�A��A��A8�A�A8A?�A_�A|�A��A�oA@�A�~A
��A
+A}VA�eA~Ae�A�A�Aw2A!-A��A� A<�A�zA�A��A2�A �8A M�@�@�!@���@��M@�S@��@�_�@���@�%F@��j@��@���@�X@���@�=q@��C@��4@���@���@��@�@��@�{J@��@���@��8@�l"@���@�C-@��@�o @�@��@�o @���@��@�_@��@�W?@�*0@�<�@���@�^@�b�@��@�j@�Q@�خ@�~@��"@�|�@ߥ@�͟@�]d@ݓ�@�!�@��s@ܔF@�q�@�N�@���@�1�@ڵ�@�2�@�w2@��@�}V@���@׮@��@���@ւA@�n�@�ff@�n�@�Ta@��&@��@�i�@ӕ�@�t�@�(@�oi@ъ�@�@���@Ќ�@�6�@��)@��@�&�@�Ɇ@΃�@�_@�{@���@�@�4�@�n�@���@���@���@ǜ�@�Y�@�@ƃ@Ş�@�S&@� i@�n�@��A@Ã{@�@O@��@���@¯O@�h�@���@�A @�/@�(�@��@�1@�|@� \@��p@�.�@��K@�n/@�҉@��$@��=@�RT@�_p@�S�@�@@��X@���@�.�@�8�@��y@�ں@���@�)�@��[@�x�@�%F@�oi@�J@��j@���@��@��9@�Z@��D@��0@��S@�<6@��P@���@�D�@�4@��@�a@��@���@�M@��@���@� �@��*@�\)@�V@��m@�u%@�`�@�2�@���@�u�@�Q�@�-w@�C�@�w2@�
=@���@�D�@�2�@�M@���@�&@�8�@��w@���@���@�IR@�,�@�@���@��@�v�@�Ft@�	�@���@�u�@�IR@��9@��@�xl@�L0@��[@�j�@���@�/�@��D@��@��'@�;d@��c@��@���@�IR@��@���@�tT@�Z�@��@��"@�/�@��,@�2�@���@��S@�B�@��	@��e@�g8@�J�@�@��T@���@��*@�E9@�҉@��4@�R�@�s�@�>�@�&�@��@��@�s�@�R�@���@�@O@�@��X@��@�V@�($@��@��q@�:�@��y@���@�@�dZ@��|@��Y@�Q�@��.@���@�s@�@@��}@�v�@�J�@��@�@��#@���@��4@�Q�@�;@��U@��F@�R�@���@��@���@��[@���@�c�@�Dg@�$t@�֡@�bN@�M@��@��g@��@��X@�`B@�!�@�
=@��@��9@�H�@�+k@�@�_@���@��C@��4@�qv@�F�@��2@��'@��e@��@�:*@���@��d@���@��$@�hs@�9�@���@���@�$@��H@��M@�=�@���@��]@���@���@�V@�7@�l�@�8�@��P@�Ɇ@�h
@�;�@�Xy@�&�@��3@�|@�hs@�S�@�$t@�@�@�f@o@}�9@}B�@}+@|Ĝ@|~@{��@{�{@z�@z=q@z	@yhs@y/@yo @yG�@y�@x�)@x9X@w��@wdZ@w@vM�@v	@u�@t��@t��@t_@s��@s��@s��@s"�@r�@r$�@q�-@q��@qT�@qL�@q%F@p�K@pH@p�@o��@o8@o�@n�+@m�'@l��@l[�@l7@l�@k�F@kS�@k�@k�@j��@jL0@i�@iG�@h�z@h/�@g��@g4�@f�@f�y@f�B@f�@fd�@f1�@fQ@f-@e��@e��@eY�@e�@d�)@c�@cdZ@b��@bL0@ap�@`��@`:�@`"h@_�W@_��@_n/@_@O@^ߤ@^� @^GE@]�N@]^�@]%F@\��@\��@\~@[��@[~�@[=@Z�@Y��@Yk�@X��@XC-@W��@Wqv@V��@V3�@U��@U��@UN<@T��@T�[@T�I@Ty>@T  @Sݘ@S�F@SP�@Rȴ@Rv�@RC�@R@Q��@QF@P�E@P|�@P!@O�Q@O�@O��@O�@N�@N�b@NR�@M��@Mk�@MV@L��@LFt@K��@KS�@J�"@J�,@JkQ@J)�@I��@I��@I	l@HbN@H�@G��@G\)@F�H@F}V@F@�@E�@E��@EIR@D�z@D�.@D�@DD�@D!@C� @C$t@B�y@Bp;@A�)@A��@AX@AN<@A2a@@��@@�o@@*�@?�@?�;@?��@?;d@>��@>�@>�@=�@=rG@=�@<�@<,=@<  @;�}@;��@;E9@;o@:�H@:^5@:e@9�Z@9�"@9p�@9X@8��@8��@8��@8]d@7� @7�@7�@6�H@6�'@6�@6}V@6c @6GE@6J@5��@5rG@5�@4�@4�@4|�@4	�@3��@3=@2��@2��@2��@2}V@2@1��@1�h@1w2@1J�@1�@0�`@0��@0S�@0	�@/�:@/A�@.�"@.��@.H�@-�Z@-�N@-��@-x�@-e,@-`B@-Q�@,�P@,�/@,��@,`�@,Ft@,$@+�g@+O@+�@*�,@*��@*{�@*&�@*e@)��@)w2@)!�@(�/@(��@(-�@'�]@'خ@'�4@'33@'�@&ں@&��@&��@&;�@%�@%��@%��@%8�@$�@$�@$��@$l"@$~@#��@#��@#O@#�@"�L@"��@"E�@"@" �@!��@!��@!�~@!O�@!�@ ��@ K^@ 7�@ /�@�A@�:@y�@o�@E9@�@��@��@ff@.�@�o@�d@��@O�@=�@*0@%@��@�?@�I@~(@Q�@2�@�@��@�@��@�@�R@��@�+@=q@�@�@�@ԕ@�t@�S@��@|@c�@O�@&�@�@�`@�@oi@j@D�@��@��@v`@b�@P�@o@��@�h@�!@}V@C�@)�@��@�X@IR@V@��@��@�@m�@D�@x@��@�[@��@iD@P�@=@!-@ i@��@��@V@@�@��@w2@a�@:�@�@��@�?@�O@��@�Y@N�@%�@�@��@qv@X�@8@S@�<@�b@��@��@J�@e@�@��@e,@@@�	@�`@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�xB�^B��B�B�)B��B�dB��B��B�hB��B�$B�IB�'B�|B�bB��B�;B�jB�zB�B	��B	�0B
�B
$B
)�B
1AB
.B
yB	żB	ðB	�&B	��B	�B	��B	�|B	�B	��B
	�B
$�B
,�B
�B
 BB
(�B
,�B
:*B
aHB
{B
��B
�aB
�_B
�B#B6+B>�BO(BXyBjB|�B�4B�GBshBZB[�BH�B49BaHBZQBO�BAUB-�B1'B8BOB�B{B
��B
�3B
�@B
ȚB
�sB
~�B
8lB
 �B

#B	��B	�B	�{B	�#B	�xB	��B	q�B	d�B	M�B	=qB	2|B	&�B	zB�ZB��B�;B��B�B�FB�PB�B	%B	jB	mB	B	�B	OB	�B	%B	A B	a�B	�B	�GB	żB	��B	�	B	��B	�5B
?B
 OB	�DB
�B
DB
"B
�B
<B
�B
=B
�B
2B
�B
�B
gB
�B
B
	RB
�B
?B
B
�B
	�B

�B
�B
�B
�B
�B
sB
�B
=B
jB
!B
�B
 �B
�B
!B
�B
B
xB
KB
�B
VB
�B	�HB	��B	�|B	�qB	�$B	��B	��B	�B	�KB	�B	�B	�B	�B	�B	�B	��B	�VB	��B	��B	�qB	��B	�6B	�}B
�B
4B
�B

�B
xB
JB
!-B
8�B
<�B
>wB
>�B
;�B
5�B
5B
7�B
9XB
;B
?B
@ B
AoB
@�B
:xB
5%B
0UB
#�B
]B
�B
FB
B
�B
<B
)B
	�B
fB
%B
oB	��B	�ZB	��B	�B	�B	��B	�FB	�B	�zB	�sB	�yB	��B	�B	�*B	�0B	��B	��B	�B	��B	�|B	��B	��B	��B	�QB	�B	�$B	�B	�B	�kB	�B	�B	��B	�mB	�B	��B	�4B	�HB	�'B	��B	ݲB	ܬB	�B	�B	�tB	�&B	�B	�zB	��B	�B	��B	�B	�8B	�RB	�8B	�RB	�B	�B	�B	�B	��B	�tB	�tB	�B	�ZB	�@B	�ZB	�@B	�B	��B	�FB	��B	�B	�8B	�B	�2B	��B	�B	�B	��B	�XB	��B	�B	�B	�B	�FB	�B	�
B	�XB	�8B	�B	�LB	�B	��B	�$B	�
B	�B	�LB	��B	�B	�8B	��B	�&B	�@B	�TB	�HB	��B	��B	�NB	�TB	�B	�B	�B	��B	�B	�B	��B	��B	�BB	ߤB	�VB	�B	��B	�B	�B	�vB	�\B	�bB	�B	��B	�\B	ߤB	��B	�B	ݘB	��B	ޞB	��B	�B	�B	�}B	��B	�B	�oB	��B	�B	�'B	�B	�vB	�B	�aB	�B	��B	�3B	��B	�B	�hB	�B	��B	��B	��B	�B	�fB	��B	�RB	�B	��B	��B	��B	��B	��B	��B	�B	�*B	�xB	�B	��B	��B	�<B	��B	��B	��B	��B	��B	�qB	�qB	�"B	��B	��B	��B	�VB	��B	�BB	��B	�HB	�B
 iB
 �B
;B
[B
�B
�B
�B
B
�B
�B
-B
B
�B
�B
B
B
�B
�B
�B
B
gB
B
�B
�B
�B
B
�B
�B
�B
�B
EB
�B
�B
�B
�B
KB
fB
	B
	7B

#B

#B

XB

�B

�B
xB
B
JB
�B
�B
B
�B
�B
�B
�B
pB
<B
B
"B
B
(B
�B
�B
4B
�B
B
NB
�B
B
�B
B
B
�B
@B
uB
&B
�B
�B
2B
�B
�B
B
�B
SB
mB
mB
�B
�B
�B
�B
sB
�B
+B
yB
_B
+B
B
�B
�B
+B
�B
�B
B
�B
B
=B
�B
	B
�B
�B
�B
�B
�B
�B
IB
dB
~B
B
B
B
5B
�B
;B
�B
�B
 BB
 �B
!B
 �B
!B
!bB
!|B
!�B
"B
"�B
#:B
#:B
$&B
$ZB
$�B
$�B
%�B
%�B
&LB
&�B
&�B
&�B
&fB
%�B
&B
%`B
%�B
&�B
&LB
&B
'�B
($B
(�B
(sB
(�B
(�B
(>B
&�B
%FB
$�B
%`B
&2B
'B
'�B
'�B
($B
(>B
(sB
(�B
*�B
+�B
+kB
-]B
/5B
/5B
.�B
.�B
./B
./B
-�B
-�B
.}B
.cB
-]B
-]B
-�B
-�B
-�B
-�B
.}B
.cB
.cB
.IB
.�B
.�B
/5B
/�B
0B
/�B
/iB
/�B
/B
/ B
.}B
-�B
,WB
+�B
+�B
,"B
,�B
,�B
,�B
,qB
,�B
,�B
,�B
,qB
,WB
,�B
,�B
,qB
,=B
,=B
,WB
,=B
,qB
,�B
.B
/�B
0oB
0�B
0�B
0�B
0�B
1AB
0�B
0�B
2B
3B
3�B
3�B
3�B
49B
4�B
4�B
5ZB
5ZB
5%B
5tB
5�B
6�B
6�B
6�B
7B
7�B
7�B
88B
8lB
8�B
9XB
9�B
:xB
:�B
:�B
;0B
;�B
<B
<6B
<jB
=B
=<B
=<B
=<B
=qB
=�B
=�B
=�B
>(B
?B
?HB
?}B
?�B
?�B
@B
@�B
@�B
@�B
A B
A B
A�B
A�B
A�B
B'B
B[B
B�B
CGB
CaB
C�B
C�B
DgB
D�B
D�B
E9B
E�B
FB
FtB
F�B
G_B
HfB
H�B
H�B
IB
I�B
I�B
J	B
JrB
JrB
J�B
KxB
KxB
KxB
K�B
K�B
K�B
L�B
L�B
M6B
M�B
NVB
N�B
P�B
QNB
Q�B
RoB
R�B
SB
S&B
S&B
S[B
S�B
S�B
UB
U�B
U�B
U�B
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
WsB
WYB
W�B
XEB
X_B
X_B
XyB
X�B
XEB
X�B
X�B
X�B
X�B
Y1B
Y1B
Y1B
YKB
Y�B
Y�B
ZB
ZQB
ZQB
ZkB
[	B
[WB
[�B
[�B
[�B
\B
[�B
\�B
\�B
\�B
\�B
\�B
]IB
]IB
]�B
]�B
^B
^OB
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`B
`B
_�B
_�B
`\B
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
b4B
bhB
bhB
b�B
b�B
c:B
cnB
c�B
c�B
dB
d@B
dZB
d�B
d�B
eB
e,B
eFB
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
h>B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
jB
jKB
jB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l"B
l"B
l=B
lqB
l�B
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
nB
n/B
n}B
n�B
n�B
o B
o B
o5B
oiB
o�B
oiB
pB
pUB
p;B
pUB
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q'B
qAB
q[B
q�B
q�B
q�B
rB
r-B
r-B
rGB
r�B
r�B
sB
s3B
sB
shB
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
u%B
uZB
u?B
uZB
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yXB
yXB
yrB
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
{JB
{dB
{JB
{dB
{B
{�B
|B
|B
|�B
|�B
|�B
|�B
}"B
}V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B�B�xB�^B��B�B�)B��B�dB��B��B�hB��B�$B�IB�'B�|B�bB��B�;B�jB�zB�B	��B	�0B
�B
$B
)�B
1AB
.B
yB	żB	ðB	�&B	��B	�B	��B	�|B	�B	��B
	�B
$�B
,�B
�B
 BB
(�B
,�B
:*B
aHB
{B
��B
�aB
�_B
�B#B6+B>�BO(BXyBjB|�B�4B�GBshBZB[�BH�B49BaHBZQBO�BAUB-�B1'B8BOB�B{B
��B
�3B
�@B
ȚB
�sB
~�B
8lB
 �B

#B	��B	�B	�{B	�#B	�xB	��B	q�B	d�B	M�B	=qB	2|B	&�B	zB�ZB��B�;B��B�B�FB�PB�B	%B	jB	mB	B	�B	OB	�B	%B	A B	a�B	�B	�GB	żB	��B	�	B	��B	�5B
?B
 OB	�DB
�B
DB
"B
�B
<B
�B
=B
�B
2B
�B
�B
gB
�B
B
	RB
�B
?B
B
�B
	�B

�B
�B
�B
�B
�B
sB
�B
=B
jB
!B
�B
 �B
�B
!B
�B
B
xB
KB
�B
VB
�B	�HB	��B	�|B	�qB	�$B	��B	��B	�B	�KB	�B	�B	�B	�B	�B	�B	��B	�VB	��B	��B	�qB	��B	�6B	�}B
�B
4B
�B

�B
xB
JB
!-B
8�B
<�B
>wB
>�B
;�B
5�B
5B
7�B
9XB
;B
?B
@ B
AoB
@�B
:xB
5%B
0UB
#�B
]B
�B
FB
B
�B
<B
)B
	�B
fB
%B
oB	��B	�ZB	��B	�B	�B	��B	�FB	�B	�zB	�sB	�yB	��B	�B	�*B	�0B	��B	��B	�B	��B	�|B	��B	��B	��B	�QB	�B	�$B	�B	�B	�kB	�B	�B	��B	�mB	�B	��B	�4B	�HB	�'B	��B	ݲB	ܬB	�B	�B	�tB	�&B	�B	�zB	��B	�B	��B	�B	�8B	�RB	�8B	�RB	�B	�B	�B	�B	��B	�tB	�tB	�B	�ZB	�@B	�ZB	�@B	�B	��B	�FB	��B	�B	�8B	�B	�2B	��B	�B	�B	��B	�XB	��B	�B	�B	�B	�FB	�B	�
B	�XB	�8B	�B	�LB	�B	��B	�$B	�
B	�B	�LB	��B	�B	�8B	��B	�&B	�@B	�TB	�HB	��B	��B	�NB	�TB	�B	�B	�B	��B	�B	�B	��B	��B	�BB	ߤB	�VB	�B	��B	�B	�B	�vB	�\B	�bB	�B	��B	�\B	ߤB	��B	�B	ݘB	��B	ޞB	��B	�B	�B	�}B	��B	�B	�oB	��B	�B	�'B	�B	�vB	�B	�aB	�B	��B	�3B	��B	�B	�hB	�B	��B	��B	��B	�B	�fB	��B	�RB	�B	��B	��B	��B	��B	��B	��B	�B	�*B	�xB	�B	��B	��B	�<B	��B	��B	��B	��B	��B	�qB	�qB	�"B	��B	��B	��B	�VB	��B	�BB	��B	�HB	�B
 iB
 �B
;B
[B
�B
�B
�B
B
�B
�B
-B
B
�B
�B
B
B
�B
�B
�B
B
gB
B
�B
�B
�B
B
�B
�B
�B
�B
EB
�B
�B
�B
�B
KB
fB
	B
	7B

#B

#B

XB

�B

�B
xB
B
JB
�B
�B
B
�B
�B
�B
�B
pB
<B
B
"B
B
(B
�B
�B
4B
�B
B
NB
�B
B
�B
B
B
�B
@B
uB
&B
�B
�B
2B
�B
�B
B
�B
SB
mB
mB
�B
�B
�B
�B
sB
�B
+B
yB
_B
+B
B
�B
�B
+B
�B
�B
B
�B
B
=B
�B
	B
�B
�B
�B
�B
�B
�B
IB
dB
~B
B
B
B
5B
�B
;B
�B
�B
 BB
 �B
!B
 �B
!B
!bB
!|B
!�B
"B
"�B
#:B
#:B
$&B
$ZB
$�B
$�B
%�B
%�B
&LB
&�B
&�B
&�B
&fB
%�B
&B
%`B
%�B
&�B
&LB
&B
'�B
($B
(�B
(sB
(�B
(�B
(>B
&�B
%FB
$�B
%`B
&2B
'B
'�B
'�B
($B
(>B
(sB
(�B
*�B
+�B
+kB
-]B
/5B
/5B
.�B
.�B
./B
./B
-�B
-�B
.}B
.cB
-]B
-]B
-�B
-�B
-�B
-�B
.}B
.cB
.cB
.IB
.�B
.�B
/5B
/�B
0B
/�B
/iB
/�B
/B
/ B
.}B
-�B
,WB
+�B
+�B
,"B
,�B
,�B
,�B
,qB
,�B
,�B
,�B
,qB
,WB
,�B
,�B
,qB
,=B
,=B
,WB
,=B
,qB
,�B
.B
/�B
0oB
0�B
0�B
0�B
0�B
1AB
0�B
0�B
2B
3B
3�B
3�B
3�B
49B
4�B
4�B
5ZB
5ZB
5%B
5tB
5�B
6�B
6�B
6�B
7B
7�B
7�B
88B
8lB
8�B
9XB
9�B
:xB
:�B
:�B
;0B
;�B
<B
<6B
<jB
=B
=<B
=<B
=<B
=qB
=�B
=�B
=�B
>(B
?B
?HB
?}B
?�B
?�B
@B
@�B
@�B
@�B
A B
A B
A�B
A�B
A�B
B'B
B[B
B�B
CGB
CaB
C�B
C�B
DgB
D�B
D�B
E9B
E�B
FB
FtB
F�B
G_B
HfB
H�B
H�B
IB
I�B
I�B
J	B
JrB
JrB
J�B
KxB
KxB
KxB
K�B
K�B
K�B
L�B
L�B
M6B
M�B
NVB
N�B
P�B
QNB
Q�B
RoB
R�B
SB
S&B
S&B
S[B
S�B
S�B
UB
U�B
U�B
U�B
V9B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
WsB
WYB
W�B
XEB
X_B
X_B
XyB
X�B
XEB
X�B
X�B
X�B
X�B
Y1B
Y1B
Y1B
YKB
Y�B
Y�B
ZB
ZQB
ZQB
ZkB
[	B
[WB
[�B
[�B
[�B
\B
[�B
\�B
\�B
\�B
\�B
\�B
]IB
]IB
]�B
]�B
^B
^OB
^�B
^�B
^�B
_pB
_�B
_�B
_�B
`B
`B
_�B
_�B
`\B
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
b4B
bhB
bhB
b�B
b�B
c:B
cnB
c�B
c�B
dB
d@B
dZB
d�B
d�B
eB
e,B
eFB
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
g8B
g�B
g�B
g�B
h>B
h�B
iDB
i_B
i�B
i�B
i�B
i�B
jB
jKB
jB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l"B
l"B
l=B
lqB
l�B
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
nB
n/B
n}B
n�B
n�B
o B
o B
o5B
oiB
o�B
oiB
pB
pUB
p;B
pUB
p�B
p�B
p�B
p�B
p�B
qB
q'B
qAB
q'B
qAB
q[B
q�B
q�B
q�B
rB
r-B
r-B
rGB
r�B
r�B
sB
s3B
sB
shB
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
u%B
uZB
u?B
uZB
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
yXB
yXB
yXB
yrB
y�B
y�B
z*B
z^B
z�B
z�B
z�B
z�B
{JB
{dB
{JB
{dB
{B
{�B
|B
|B
|�B
|�B
|�B
|�B
}"B
}V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105000  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175532  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175532  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175532                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025539  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025539  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                