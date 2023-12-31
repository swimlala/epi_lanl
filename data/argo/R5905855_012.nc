CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:47Z creation;2022-06-04T19:12:48Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191247  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�É�ʆB1   @�Ê��t@/-�hr�!�d/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
L�C�3C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�B 
=B
=B
=B��B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�k�B�B�B�B�B�B�B�B�B�B�8RB�B�8RB�B�B�B�B�B�B�B�8RB���B���B�B�B�B�B�B�C �C�C�C�C�C
O\C��C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<)C>�C@�CB�CC��CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�>D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DY�>DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy
Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؄�AؒA؆�A�=�A��A��Aׅ�A�H�A�2�A��A��A��A�YA� �A��A���A��A���A���A�%AՏ�A�c�A�0�A�֡A�@�A�{A�{AѢhAБ A�,�A��A�A�7A���A�[�A�OvA��?Aʞ�A��A���A��)A��A��aA�A�u�A���A�l"A���A���A���A�\�A��bA�K)A�i�A�� A�x�A�A��A�XyA���A���A�c�A��A��A��A��0A���A���A��A���A��5A�u�A���A���A��,A�^jA���A�9�A�<jA�1�A�3�A�*�A�N�A�`vA�'�A�n�A���A�W�A��A�P�A�,qA�eA�W�A��VA�'�A@�A|�A{˒Ax%AuPHAs!�Aq�4Ao�1Anl"Al��Ah��Ad��Aaf�A[�FAW/�AS��AQ-AO�AJ�7AF��AE.IAD;ABk�A?�@A;��A:��A9�A5�;A3��A2S�A0�tA/��A.�A,�+A*یA)A)IRA(��A'�A&��A%xA$�4A#�A"��A!qA�&A�"A�A��A�IA3�AzxA�A�}A�[A �A �A��A��Ae�A�A1A�AA�A�A��A��A�6A.�A%A�A�jAAJAK^A��A�AX�A��A:*AA�Al�A6A�+A
�A
8�A	��A	;A��A�)A}VAffAj�Aj�A{JA2aA*�AC-AZ�A�xAl"A��A�A�A�A&�A�TAS�A?A &�@��x@��s@�33@�-w@���@�� @��@���@���@�W?@�N�@��@�Ft@��
@봢@뫟@�`B@�  @�!�@詓@�p;@�N�@�D�@�PH@�]d@�d�@�ff@�kQ@�~(@�ں@��)@���@�8@�<6@�a�@��,@��&@��@�֡@�kQ@���@�m]@�;d@�
=@��X@�l�@�O@��@�]�@�@�W�@��@���@�'�@�Z�@�dZ@��D@�!-@���@ܯO@ܵ@ܤ�@�˒@�A @�ں@�h�@�C�@�	�@لM@ؿ�@׽�@�e,@�8�@��`@ֈ�@�i�@�&�@�_@�Z@�!�@��@՝�@�p�@ԓu@�>B@Ә�@҄�@��@���@ъ	@��@��@���@�h
@�s�@���@��p@�ȴ@Έ�@��m@�|@͌~@͎"@�u�@���@͑h@�;@�Q@���@�J#@��s@�L0@ɸ�@�33@���@�2�@��>@���@��@�Ta@�4n@ŨX@��@��)@ć+@�*�@�p�@�ȴ@�H�@�  @�ݘ@��3@���@�@O@�͟@�G@���@�8�@��g@�,�@�� @�7@�J�@��+@�D�@���@��R@�_@�`B@���@���@�/�@��X@�@���@��<@�-@��0@��'@���@��@�|�@�?}@��@���@�3�@�x@���@���@���@�Vm@��@��@��@��@���@�P�@���@��@��g@��C@���@�Vm@�>�@�C@��@��5@��h@�"h@��@��@�X@��@��'@�@�@���@���@�/�@��8@��I@�h
@�"h@��&@���@��S@�|�@�S&@�<6@���@���@�<�@���@��4@�9�@��@���@��o@�tT@�H@��@��&@��@�1�@���@�s�@�'R@���@���@�c�@�	l@��B@���@���@�h�@��)@��@�b�@�+@��K@���@��Y@�g8@�L0@�	@���@�iD@�ȴ@�ݘ@��@@��S@��7@�l�@��@��[@�z�@��@�qv@�%@���@�<�@���@��P@�T�@��B@��Y@�YK@�	@���@���@���@���@�>�@�oi@��@�خ@��6@���@�j@�,�@��@��E@�-@��@��F@�iD@�6z@���@��j@���@��x@�i�@���@�j@�S@���@��j@���@�6@��C@�W?@�Dg@�1�@�@���@��@��@���@��:@�F@��@���@�tT@��@���@�zx@�(@���@�'R@��N@���@��@��)@���@���@�H�@��@��-@�t�@�]�@�=�@��8@���@��}@�tT@�Ft@� �@���@��j@��@��K@���@���@�|�@�\)@�*0@��@��@��_@�n�@�4n@��]@��w@���@���@��7@�|@�m]@�\�@��@��@���@��_@���@��o@�m�@�L0@�;�@�e@��@~ں@~@}Y�@|�@|4n@{{J@{�@z��@zi�@y��@yhs@x�v@x�u@xQ�@x	�@w�@w@O@v�X@v��@v�A@v�@u�^@tXy@s��@s,�@r��@rl�@rM�@r4@q�o@q�@q�C@q|@q^�@qc@p�j@pM@o�r@o�	@o��@oj�@n6�@mhs@l�f@l��@lD�@kP�@j�<@j-@i�@i�X@iL�@h�f@hy>@hH@hx@g��@g�6@g�$@g�@f�@fi�@e�#@eB�@d�|@d�9@d��@c�@c�@c;d@bOv@a�z@a2a@a�@`g8@`x@_�+@_�A@_33@^4@]�-@]w2@]q@\�@\M@[�r@[��@[Z�@[�@Z~�@Z-@Y��@Y-w@X��@Xc�@X7@W��@Wv`@W/�@V�s@V�@Vl�@VQ@U��@Ue,@U%@T�@T��@T`�@S�*@SA�@R�]@Rc @R#:@Q�@Q�"@Q[W@Q�@P�u@P>B@O�@OP�@N��@N��@N{�@N;�@M��@M@L�@L6@K�@K�*@Kj�@K�@JH�@I�@I��@IA @H��@Hq@H"h@G��@GH�@F�c@F�x@FH�@E�N@ES&@E7L@E@Dی@D��@De�@D?�@D-�@C�@C�V@C.I@B��@BC�@A��@A��@Ao @AQ�@@��@@��@@M@@@?�W@?�w@?��@?e�@?_p@?K�@?1�@?@>��@>�6@>.�@=��@=��@=B�@=7L@=%F@<��@<��@<�4@<��@<?�@;�@;l�@;J#@;A�@;33@:�s@:B[@:O@:{@:@9��@9G�@8�P@8�@8V�@81'@7˒@7��@7>�@6��@6E�@6
�@5�@5B�@5�@4�P@4��@4�u@4Ft@3�K@3o�@3@O@2�6@2R�@2V@2.�@1��@1N<@1+�@0�	@0�@0��@0w�@09X@/��@/�w@/j�@.�M@.�L@.�1@.kQ@.1�@-�@-�7@-Dg@,�|@,Ɇ@,�@,Ft@+�@+��@+X�@+o@*��@*ں@*��@*\�@*#:@*�@)@)�@)zx@)T�@(�`@(�4@(Q�@("h@'��@'�*@'6z@&�B@&�6@&YK@%�@%�~@%e,@$�`@$�I@$e�@$PH@$'R@$@$G@#�m@#�f@#;d@#�@"�@"��@"�@"}V@"V@!��@!��@!f�@!F@!�@ �@ �.@ Z@ !@�]@��@��@s@33@!-@�8@��@a|@1�@��@�^@��@rG@=�@+@�@�)@�j@�j@�z@bN@:�@�@�@��@��@�@@�V@�:@o�@8@�@�s@�<@z@+k@@�@�^@�-@��@s�@*0@�)@V�@<�@M@x@�r@��@qv@�@�@�<@�@q�@{@��@��@[W@%F@@�@z�@A�@7@x@1@��@��@b�@>�@��@��@l�@)�@�o@zx@L�@�@��@��@��@�9@��@9X@��@��@�*@_p@4�@@��@�\@q�@?@_@��@�C@w2@IR@@��@|�@oi@M@6@%�@��@�[@�@�{@\)@@
�@
�s@
�B@
�m@
�x@
d�@
Ov@
;�@
�@	�3@	��@	��@	�C@	��@	�X@	��@	�'@	�S@	c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A؄�AؒA؆�A�=�A��A��Aׅ�A�H�A�2�A��A��A��A�YA� �A��A���A��A���A���A�%AՏ�A�c�A�0�A�֡A�@�A�{A�{AѢhAБ A�,�A��A�A�7A���A�[�A�OvA��?Aʞ�A��A���A��)A��A��aA�A�u�A���A�l"A���A���A���A�\�A��bA�K)A�i�A�� A�x�A�A��A�XyA���A���A�c�A��A��A��A��0A���A���A��A���A��5A�u�A���A���A��,A�^jA���A�9�A�<jA�1�A�3�A�*�A�N�A�`vA�'�A�n�A���A�W�A��A�P�A�,qA�eA�W�A��VA�'�A@�A|�A{˒Ax%AuPHAs!�Aq�4Ao�1Anl"Al��Ah��Ad��Aaf�A[�FAW/�AS��AQ-AO�AJ�7AF��AE.IAD;ABk�A?�@A;��A:��A9�A5�;A3��A2S�A0�tA/��A.�A,�+A*یA)A)IRA(��A'�A&��A%xA$�4A#�A"��A!qA�&A�"A�A��A�IA3�AzxA�A�}A�[A �A �A��A��Ae�A�A1A�AA�A�A��A��A�6A.�A%A�A�jAAJAK^A��A�AX�A��A:*AA�Al�A6A�+A
�A
8�A	��A	;A��A�)A}VAffAj�Aj�A{JA2aA*�AC-AZ�A�xAl"A��A�A�A�A&�A�TAS�A?A &�@��x@��s@�33@�-w@���@�� @��@���@���@�W?@�N�@��@�Ft@��
@봢@뫟@�`B@�  @�!�@詓@�p;@�N�@�D�@�PH@�]d@�d�@�ff@�kQ@�~(@�ں@��)@���@�8@�<6@�a�@��,@��&@��@�֡@�kQ@���@�m]@�;d@�
=@��X@�l�@�O@��@�]�@�@�W�@��@���@�'�@�Z�@�dZ@��D@�!-@���@ܯO@ܵ@ܤ�@�˒@�A @�ں@�h�@�C�@�	�@لM@ؿ�@׽�@�e,@�8�@��`@ֈ�@�i�@�&�@�_@�Z@�!�@��@՝�@�p�@ԓu@�>B@Ә�@҄�@��@���@ъ	@��@��@���@�h
@�s�@���@��p@�ȴ@Έ�@��m@�|@͌~@͎"@�u�@���@͑h@�;@�Q@���@�J#@��s@�L0@ɸ�@�33@���@�2�@��>@���@��@�Ta@�4n@ŨX@��@��)@ć+@�*�@�p�@�ȴ@�H�@�  @�ݘ@��3@���@�@O@�͟@�G@���@�8�@��g@�,�@�� @�7@�J�@��+@�D�@���@��R@�_@�`B@���@���@�/�@��X@�@���@��<@�-@��0@��'@���@��@�|�@�?}@��@���@�3�@�x@���@���@���@�Vm@��@��@��@��@���@�P�@���@��@��g@��C@���@�Vm@�>�@�C@��@��5@��h@�"h@��@��@�X@��@��'@�@�@���@���@�/�@��8@��I@�h
@�"h@��&@���@��S@�|�@�S&@�<6@���@���@�<�@���@��4@�9�@��@���@��o@�tT@�H@��@��&@��@�1�@���@�s�@�'R@���@���@�c�@�	l@��B@���@���@�h�@��)@��@�b�@�+@��K@���@��Y@�g8@�L0@�	@���@�iD@�ȴ@�ݘ@��@@��S@��7@�l�@��@��[@�z�@��@�qv@�%@���@�<�@���@��P@�T�@��B@��Y@�YK@�	@���@���@���@���@�>�@�oi@��@�خ@��6@���@�j@�,�@��@��E@�-@��@��F@�iD@�6z@���@��j@���@��x@�i�@���@�j@�S@���@��j@���@�6@��C@�W?@�Dg@�1�@�@���@��@��@���@��:@�F@��@���@�tT@��@���@�zx@�(@���@�'R@��N@���@��@��)@���@���@�H�@��@��-@�t�@�]�@�=�@��8@���@��}@�tT@�Ft@� �@���@��j@��@��K@���@���@�|�@�\)@�*0@��@��@��_@�n�@�4n@��]@��w@���@���@��7@�|@�m]@�\�@��@��@���@��_@���@��o@�m�@�L0@�;�@�e@��@~ں@~@}Y�@|�@|4n@{{J@{�@z��@zi�@y��@yhs@x�v@x�u@xQ�@x	�@w�@w@O@v�X@v��@v�A@v�@u�^@tXy@s��@s,�@r��@rl�@rM�@r4@q�o@q�@q�C@q|@q^�@qc@p�j@pM@o�r@o�	@o��@oj�@n6�@mhs@l�f@l��@lD�@kP�@j�<@j-@i�@i�X@iL�@h�f@hy>@hH@hx@g��@g�6@g�$@g�@f�@fi�@e�#@eB�@d�|@d�9@d��@c�@c�@c;d@bOv@a�z@a2a@a�@`g8@`x@_�+@_�A@_33@^4@]�-@]w2@]q@\�@\M@[�r@[��@[Z�@[�@Z~�@Z-@Y��@Y-w@X��@Xc�@X7@W��@Wv`@W/�@V�s@V�@Vl�@VQ@U��@Ue,@U%@T�@T��@T`�@S�*@SA�@R�]@Rc @R#:@Q�@Q�"@Q[W@Q�@P�u@P>B@O�@OP�@N��@N��@N{�@N;�@M��@M@L�@L6@K�@K�*@Kj�@K�@JH�@I�@I��@IA @H��@Hq@H"h@G��@GH�@F�c@F�x@FH�@E�N@ES&@E7L@E@Dی@D��@De�@D?�@D-�@C�@C�V@C.I@B��@BC�@A��@A��@Ao @AQ�@@��@@��@@M@@@?�W@?�w@?��@?e�@?_p@?K�@?1�@?@>��@>�6@>.�@=��@=��@=B�@=7L@=%F@<��@<��@<�4@<��@<?�@;�@;l�@;J#@;A�@;33@:�s@:B[@:O@:{@:@9��@9G�@8�P@8�@8V�@81'@7˒@7��@7>�@6��@6E�@6
�@5�@5B�@5�@4�P@4��@4�u@4Ft@3�K@3o�@3@O@2�6@2R�@2V@2.�@1��@1N<@1+�@0�	@0�@0��@0w�@09X@/��@/�w@/j�@.�M@.�L@.�1@.kQ@.1�@-�@-�7@-Dg@,�|@,Ɇ@,�@,Ft@+�@+��@+X�@+o@*��@*ں@*��@*\�@*#:@*�@)@)�@)zx@)T�@(�`@(�4@(Q�@("h@'��@'�*@'6z@&�B@&�6@&YK@%�@%�~@%e,@$�`@$�I@$e�@$PH@$'R@$@$G@#�m@#�f@#;d@#�@"�@"��@"�@"}V@"V@!��@!��@!f�@!F@!�@ �@ �.@ Z@ !@�]@��@��@s@33@!-@�8@��@a|@1�@��@�^@��@rG@=�@+@�@�)@�j@�j@�z@bN@:�@�@�@��@��@�@@�V@�:@o�@8@�@�s@�<@z@+k@@�@�^@�-@��@s�@*0@�)@V�@<�@M@x@�r@��@qv@�@�@�<@�@q�@{@��@��@[W@%F@@�@z�@A�@7@x@1@��@��@b�@>�@��@��@l�@)�@�o@zx@L�@�@��@��@��@�9@��@9X@��@��@�*@_p@4�@@��@�\@q�@?@_@��@�C@w2@IR@@��@|�@oi@M@6@%�@��@�[@�@�{@\)@@
�@
�s@
�B@
�m@
�x@
d�@
Ov@
;�@
�@	�3@	��@	��@	�C@	��@	�X@	��@	�'@	�S@	c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	6�B	6FB	6zB	7B	6�B	7�B	9	B	:B	:�B	;dB	;�B	<6B	<�B	<�B	<�B	=B	<�B	="B	>�B	QNB	o�B	~�B	� B	u�B	e�B	g�B	M�B	L�B	>BB	IB	r�B	�B	�B	��B	�KB	��B
�B	��B
$B
uB
�B

	B
�B
:B
b�B
p�B
}<B
��B
�BB�BB=�BDgBM�BJXBJ�B`\B�B��B��B҉BۦBߤB��B�B�B�1B�!B�B��B��B��B�bB��B{�BvFBq�Ba|BV�BL�BB[B-�BpB�B
�B
�5B
��B
ѝB
��B
��B
j0B
K�B
3�B
&�B
B	�cB	�B	�DB	�B	�B	�XB	�B	�B	�qB	��B	xB	a|B	LdB	.IB	&B	�B�VB��B��B��B�TB�}B�lB�B�B��B�B��B��B��B�SB��B�[B��B�"B��B� B�&B��B��B��B��B�gB�;Bz�B~wB|Bt�BqBxlB�nB�$B��B�aB�zB	�B	B	#B	'8B	+B	SB	+�B	<B	EB	E9B	HfB	F�B	AoB	>�B	7LB	/�B	)_B	�B	�B	aB	gB	YB	7B	�B	QB	B	*B	+�B	/�B	*�B	*KB	,�B	2�B	:�B	=�B	>�B	CB	@�B	F?B	J�B	L~B	OB	R�B	c�B	^�B	^jB	a�B	]�B	g�B	s�B	~BB	�4B	�B	}�B	z�B	u�B	p�B	k6B	o B	v�B	tTB	vB	vB	v`B	wfB	w�B	w�B	x�B	y$B	y	B	x�B	x�B	v�B	~�B	�1B	��B	��B	�BB	�vB	�B	��B	��B	�2B	�SB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�B	��B	�PB	��B	�B	�=B	ʌB	�=B	��B	ɺB	�=B	�B	˒B	�B	�0B	̈́B	�B	ɠB	ǔB	��B	�+B	��B	��B	�EB	��B	̈́B	οB	�\B	�NB	��B	�hB	��B	͹B	͹B	�VB	ΥB	��B	��B	�HB	��B	�B	�mB	�?B	�YB	��B	�B	�EB	�=B	�	B	یB	�~B	�CB	�]B	�IB	�!B	ߤB	��B	޸B	ޞB	ޞB	��B	�B	��B	��B	�vB	� B	��B	��B	�B	�&B	�B	�FB	�LB	�sB	��B	�2B	��B	�LB	�2B	�B	�B	�RB	�mB	�B	�
B	�$B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�=B	�B	�qB	�CB	�wB	�B	�CB	�WB	�QB	�B	�B	��B	��B	��B	�=B	�B	�CB	�CB	�CB	�B	�B	��B	��B	�B	��B	�B	�B	��B	�UB	�B	�B	��B	�AB	�AB	�B	��B	�aB	�B	�3B	�B	�%B	��B	��B	��B	�B	�FB	�`B	�zB	��B	�zB	��B	��B	�B	�lB	�lB	�RB	�RB	�8B	��B	��B	�8B	�	B	��B	��B	��B	��B	�VB	��B	��B	�B	�B	��B	�HB	��B
  B
 OB
 iB
 �B
UB
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
gB
MB
B
SB
�B
�B
YB
B
�B
�B
�B
_B
B
EB
�B
�B
	�B
	�B
	lB
	�B
	lB
	B
�B
�B
�B
�B
�B
�B
1B
�B
	RB
	lB

=B

�B

�B
B
0B
~B
�B
PB
�B
B
�B
�B
�B
bB
�B
�B
�B
B
uB
�B
�B
{B
SB
�B
$B
�B
�B
�B
eB
eB
�B
�B
�B
WB
�B
CB
CB
)B
xB
�B
�B
�B
�B
�B
�B
B
;B
VB
 BB
 �B
!�B
!�B
"4B
!�B
!�B
"�B
"NB
!|B
 �B
!HB
!�B
#B
#TB
#TB
#nB
#�B
$ZB
$�B
%zB
%zB
%�B
&B
&2B
&LB
&�B
'B
'�B
'�B
'�B
'�B
'�B
(>B
(>B
(>B
(�B
(�B
(�B
)*B
)�B
)�B
*B
*eB
*�B
+6B
+QB
+kB
+�B
+�B
+�B
,"B
,qB
,�B
,�B
,�B
-B
-)B
-]B
-]B
-wB
-�B
.�B
/�B
0!B
0�B
0�B
0�B
1AB
2B
1�B
2aB
2�B
3B
3MB
3�B
3�B
3�B
5B
4�B
4�B
5?B
5ZB
5%B
49B
4nB
4TB
4�B
4�B
5B
5ZB
5�B
6B
6�B
7�B
7�B
8�B
8lB
8RB
8�B
9	B
:xB
:�B
:�B
;0B
;�B
<B
<jB
<�B
<�B
="B
=VB
="B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?.B
?B
?�B
?�B
?�B
?HB
?�B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
BB
B'B
BAB
BuB
B�B
B�B
CB
C{B
C{B
C�B
C�B
C�B
D3B
DgB
DgB
D�B
DgB
D�B
E9B
E�B
E�B
E�B
E�B
F�B
F�B
G_B
G�B
G�B
HB
HKB
H�B
IB
IlB
IRB
I�B
J#B
J=B
J#B
JXB
J=B
JXB
K)B
KDB
K�B
K�B
LB
LB
K�B
L�B
L�B
MB
MjB
M�B
N"B
N"B
N"B
N�B
O\B
OvB
O�B
OvB
O�B
O�B
PB
PbB
P�B
P�B
QB
Q B
Q B
QNB
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
TB
T{B
T�B
T�B
T�B
UB
UB
T�B
T�B
UB
U2B
UgB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
W?B
W$B
W?B
W$B
W$B
WsB
W�B
XB
X+B
X+B
X�B
Y1B
Y1B
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZkB
Z�B
[=B
[=B
[�B
[�B
\�B
\�B
]/B
]dB
]�B
^5B
^OB
^OB
^jB
^�B
^�B
_B
_pB
_�B
_�B
_�B
`B
`B
`BB
`vB
`�B
`�B
`�B
aB
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
b�B
cB
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iDB
iDB
i_B
i�B
iyB
i�B
i�B
jB
jKB
jB
jB
j�B
j�B
k6B
k6B
k�B
k�B
k�B
k�B
lB
l"B
l"B
lWB
l�B
l�B
l�B
m)B
mwB
m]B
m�B
m�B
m�B
nB
nIB
nIB
n/B
nIB
n�B
n�B
n�B
n�B
o B
o B
o5B
oB
o5B
o5B
oiB
o�B
o�B
o�B
o�B
p;B
p;B
poB
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
tB
tB
tB
t�B
t�B
uB
u%B
u%B
u%B
utB
u�B
u�B
vB
v+B
vFB
v�B
vzB
v�B
wB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
y	B
y$B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|jB
|jB
|�B
}"B
}VB
}qB
}VB
}VB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	6�B	6FB	6zB	7B	6�B	7�B	9	B	:B	:�B	;dB	;�B	<6B	<�B	<�B	<�B	=B	<�B	="B	>�B	QNB	o�B	~�B	� B	u�B	e�B	g�B	M�B	L�B	>BB	IB	r�B	�B	�B	��B	�KB	��B
�B	��B
$B
uB
�B

	B
�B
:B
b�B
p�B
}<B
��B
�BB�BB=�BDgBM�BJXBJ�B`\B�B��B��B҉BۦBߤB��B�B�B�1B�!B�B��B��B��B�bB��B{�BvFBq�Ba|BV�BL�BB[B-�BpB�B
�B
�5B
��B
ѝB
��B
��B
j0B
K�B
3�B
&�B
B	�cB	�B	�DB	�B	�B	�XB	�B	�B	�qB	��B	xB	a|B	LdB	.IB	&B	�B�VB��B��B��B�TB�}B�lB�B�B��B�B��B��B��B�SB��B�[B��B�"B��B� B�&B��B��B��B��B�gB�;Bz�B~wB|Bt�BqBxlB�nB�$B��B�aB�zB	�B	B	#B	'8B	+B	SB	+�B	<B	EB	E9B	HfB	F�B	AoB	>�B	7LB	/�B	)_B	�B	�B	aB	gB	YB	7B	�B	QB	B	*B	+�B	/�B	*�B	*KB	,�B	2�B	:�B	=�B	>�B	CB	@�B	F?B	J�B	L~B	OB	R�B	c�B	^�B	^jB	a�B	]�B	g�B	s�B	~BB	�4B	�B	}�B	z�B	u�B	p�B	k6B	o B	v�B	tTB	vB	vB	v`B	wfB	w�B	w�B	x�B	y$B	y	B	x�B	x�B	v�B	~�B	�1B	��B	��B	�BB	�vB	�B	��B	��B	�2B	�SB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	�B	��B	�PB	��B	�B	�=B	ʌB	�=B	��B	ɺB	�=B	�B	˒B	�B	�0B	̈́B	�B	ɠB	ǔB	��B	�+B	��B	��B	�EB	��B	̈́B	οB	�\B	�NB	��B	�hB	��B	͹B	͹B	�VB	ΥB	��B	��B	�HB	��B	�B	�mB	�?B	�YB	��B	�B	�EB	�=B	�	B	یB	�~B	�CB	�]B	�IB	�!B	ߤB	��B	޸B	ޞB	ޞB	��B	�B	��B	��B	�vB	� B	��B	��B	�B	�&B	�B	�FB	�LB	�sB	��B	�2B	��B	�LB	�2B	�B	�B	�RB	�mB	�B	�
B	�$B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�=B	�B	�qB	�CB	�wB	�B	�CB	�WB	�QB	�B	�B	��B	��B	��B	�=B	�B	�CB	�CB	�CB	�B	�B	��B	��B	�B	��B	�B	�B	��B	�UB	�B	�B	��B	�AB	�AB	�B	��B	�aB	�B	�3B	�B	�%B	��B	��B	��B	�B	�FB	�`B	�zB	��B	�zB	��B	��B	�B	�lB	�lB	�RB	�RB	�8B	��B	��B	�8B	�	B	��B	��B	��B	��B	�VB	��B	��B	�B	�B	��B	�HB	��B
  B
 OB
 iB
 �B
UB
�B
�B
aB
�B
�B
�B
�B
�B
�B
�B
�B
gB
MB
B
SB
�B
�B
YB
B
�B
�B
�B
_B
B
EB
�B
�B
	�B
	�B
	lB
	�B
	lB
	B
�B
�B
�B
�B
�B
�B
1B
�B
	RB
	lB

=B

�B

�B
B
0B
~B
�B
PB
�B
B
�B
�B
�B
bB
�B
�B
�B
B
uB
�B
�B
{B
SB
�B
$B
�B
�B
�B
eB
eB
�B
�B
�B
WB
�B
CB
CB
)B
xB
�B
�B
�B
�B
�B
�B
B
;B
VB
 BB
 �B
!�B
!�B
"4B
!�B
!�B
"�B
"NB
!|B
 �B
!HB
!�B
#B
#TB
#TB
#nB
#�B
$ZB
$�B
%zB
%zB
%�B
&B
&2B
&LB
&�B
'B
'�B
'�B
'�B
'�B
'�B
(>B
(>B
(>B
(�B
(�B
(�B
)*B
)�B
)�B
*B
*eB
*�B
+6B
+QB
+kB
+�B
+�B
+�B
,"B
,qB
,�B
,�B
,�B
-B
-)B
-]B
-]B
-wB
-�B
.�B
/�B
0!B
0�B
0�B
0�B
1AB
2B
1�B
2aB
2�B
3B
3MB
3�B
3�B
3�B
5B
4�B
4�B
5?B
5ZB
5%B
49B
4nB
4TB
4�B
4�B
5B
5ZB
5�B
6B
6�B
7�B
7�B
8�B
8lB
8RB
8�B
9	B
:xB
:�B
:�B
;0B
;�B
<B
<jB
<�B
<�B
="B
=VB
="B
=�B
=�B
=�B
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>wB
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?.B
?.B
?B
?�B
?�B
?�B
?HB
?�B
@OB
@�B
@�B
AUB
A�B
A�B
A�B
BB
B'B
BAB
BuB
B�B
B�B
CB
C{B
C{B
C�B
C�B
C�B
D3B
DgB
DgB
D�B
DgB
D�B
E9B
E�B
E�B
E�B
E�B
F�B
F�B
G_B
G�B
G�B
HB
HKB
H�B
IB
IlB
IRB
I�B
J#B
J=B
J#B
JXB
J=B
JXB
K)B
KDB
K�B
K�B
LB
LB
K�B
L�B
L�B
MB
MjB
M�B
N"B
N"B
N"B
N�B
O\B
OvB
O�B
OvB
O�B
O�B
PB
PbB
P�B
P�B
QB
Q B
Q B
QNB
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
TB
T{B
T�B
T�B
T�B
UB
UB
T�B
T�B
UB
U2B
UgB
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
W?B
W$B
W?B
W$B
W$B
WsB
W�B
XB
X+B
X+B
X�B
Y1B
Y1B
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZkB
Z�B
[=B
[=B
[�B
[�B
\�B
\�B
]/B
]dB
]�B
^5B
^OB
^OB
^jB
^�B
^�B
_B
_pB
_�B
_�B
_�B
`B
`B
`BB
`vB
`�B
`�B
`�B
aB
a�B
a�B
a�B
bB
b4B
bhB
b�B
b�B
b�B
cB
c B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
d@B
dZB
d�B
d�B
d�B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
h�B
iDB
iDB
i_B
i�B
iyB
i�B
i�B
jB
jKB
jB
jB
j�B
j�B
k6B
k6B
k�B
k�B
k�B
k�B
lB
l"B
l"B
lWB
l�B
l�B
l�B
m)B
mwB
m]B
m�B
m�B
m�B
nB
nIB
nIB
n/B
nIB
n�B
n�B
n�B
n�B
o B
o B
o5B
oB
o5B
o5B
oiB
o�B
o�B
o�B
o�B
p;B
p;B
poB
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
r-B
r-B
r�B
r�B
r�B
r�B
r�B
sMB
shB
s�B
s�B
tB
tB
tB
t�B
t�B
uB
u%B
u%B
u%B
utB
u�B
u�B
vB
v+B
vFB
v�B
vzB
v�B
wB
wLB
wfB
wfB
w�B
w�B
w�B
w�B
xRB
xRB
x�B
x�B
y	B
y$B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
|B
|jB
|jB
|jB
|�B
}"B
}VB
}qB
}VB
}VB
}�B
}�B
}�B
~B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191247  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191247  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191248                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041255  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041255  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                